#' Contrasts Between Subpopulations
#'
#' Estimate linear combinations and nonlinear comparisons (i.e., contrasts) of domain means,
#' totals, and category proportions.
#'
#' @inheritParams cont_analysis
#' @inheritParams trend_analysis
#' @param subpops Character vector naming subpopulation variables in
#'   \code{dframe}. A contrast compares levels within one variable; this
#'   argument is required.
#' @param contrasts Named list with one element for each variable in
#'   \code{subpops}. Each element is a named list of contrasts. A contrast is
#'   either a named numeric coefficient vector, such as
#'   \code{c(East = 1, West = -1)}, or a quoted expression, such as
#'   \code{quote(East / West)}. Names in a coefficient vector and variables in
#'   an expression must identify domain levels. Omitted coefficients are assigned a value of zero.
#'   Expressions use the functions supported by \code{survey::svycontrast()}.
#' @param statistics Character vector selecting \code{"Mean"} and/or
#'   \code{"Total"} for continuous responses. Categorical responses always
#'   produce both proportions and totals.
#' @param return_joint Logical value indicating whether to return the named
#'   joint \code{svystat} objects, suitable for subsequent
#'   \code{survey::svycontrast()} calls. The default is \code{FALSE}.
#'
#' @details
#' Each contrast is applied separately to each continuous response and each
#'   category of each categorical response. Contrasts across response variables,
#'   categories, different subpopulation variables, or survey occasions are
#'   not currently supported. The \code{formula} argument specifies GREG auxiliaries,
#'   as in \code{cont_analysis()}, rather than the contrast.
#'
#' @return A list with \code{catsum}, \code{contsum_mean}, and
#'   \code{contsum_total}. Rows identify
#'   \code{Type}, \code{Indicator}, and \code{Contrast}, plus \code{Category}
#'   for categorical responses. Continuous tables provide \code{Estimate},
#'   \code{StdError}, \code{MarginofError}, and confidence bounds. Categorical
#'   tables have these quantities with \code{.P} (percentage) and \code{.U}
#'   (total) suffixes.
#'
#'   If requested, \code{joint[[type]][[indicator]][[statistic]]} contains the
#'   joint statistics. For categorical responses, \code{statistic} is
#'   \code{"Proportion"} or \code{"Total"}, with an additional category index.
#'   Each object has domain names, \code{coef()} and \code{vcov()} methods, an
#'   \code{nResp} attribute of domain response counts, and a
#'   \code{subset_independence} attribute identifying the subset approximation was used with \code{subset_local = TRUE}.
#' @examples
#' set.seed(5)
#' dat <- data.frame(y = rnorm(80), weight = 4,
#'   region = rep(c("East", "West"), 40),
#'   xc = runif(80), yc = runif(80))
#' contrast_analysis(dat, vars_cont = "y", subpops = "region",
#'   contrasts = list(region = list(difference = c(East = 1, West = -1))),
#'   weight = "weight", xcoord = "xc", ycoord = "yc", statistics = "Mean")
#' @export
contrast_analysis <- function(dframe, vars_cat = NULL, vars_cont = NULL,
                                subpops, contrasts, siteID = NULL,
                                weight = "weight", xcoord = NULL, ycoord = NULL,
                                stratumID = NULL, clusterID = NULL, weight1 = NULL,
                                xcoord1 = NULL, ycoord1 = NULL, sizeweight = FALSE,
                                sweight = NULL, sweight1 = NULL, fpc = NULL,
                                formula = NULL, popsize = NULL, subpopsize = NULL,
                                vartype = "local", jointprob = "overton", conf = 95,
                                statistics = c("Mean", "Total"),
                                subset_local = FALSE, local_nbh = 10,
                                return_joint = FALSE) {
  if (missing(subpops) || !is.character(subpops) || !length(subpops) ||
      anyNA(subpops) || any(!nzchar(subpops)) || anyDuplicated(subpops)) {
    stop("subpops must be a nonempty character vector of unique column names.", call. = FALSE)
  }
  if (is.null(vars_cat) && is.null(vars_cont)) {
    stop("Supply vars_cat, vars_cont, or both.", call. = FALSE)
  }
  if (length(intersect(vars_cat, vars_cont))) {
    stop("A response cannot appear in both vars_cat and vars_cont.", call. = FALSE)
  }
  if (!is.character(statistics) || !length(statistics) ||
      anyNA(statistics) || any(!statistics %in% c("Mean", "Total")) || anyDuplicated(statistics)) {
    stop("statistics must contain Mean, Total, or both.", call. = FALSE)
  }
  if (is.null(subset_local)) subset_local <- FALSE
  for (flag in c("subset_local", "return_joint")) {
    value <- get(flag)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(flag, " must be TRUE or FALSE.", call. = FALSE)
    }
  }
  prepared <- contrast_prepare(dframe, vars_cat, vars_cont, subpops, siteID,
    weight, xcoord, ycoord, stratumID, clusterID, weight1, xcoord1, ycoord1,
    sizeweight, sweight, sweight1, fpc, formula, popsize, subpopsize,
    vartype, jointprob, conf, local_nbh)
  data <- prepared$design$variables
  contrast_check(contrasts, subpops, data)
  if (!is.null(formula) && subset_local) {
    if (prepared$vartype == "Local") prepared$warn_df <- contrast_warning(
      prepared$warn_df, "GREG requires subset_local = FALSE.",
      "The explicitly supplied subset_local = TRUE was overridden.")
    subset_local <- FALSE
  }
  if (subset_local && prepared$vartype == "Local") {
    prepared$warn_df <- contrast_warning(prepared$warn_df,
      "Direct subset-local contrasts assume independence between domain estimates.",
      "Standard subset marginal variances were used with zero cross-domain covariances.")
  }
  local_context <- localmean_context(local_nbh)
  answer <- list(catsum = NULL, contsum_mean = NULL, contsum_total = NULL)
  joint <- list()
  for (type in subpops) {
    joint[[type]] <- list()
    for (indicator in c(prepared$vars_cont, prepared$vars_cat)) {
      categorical <- indicator %in% prepared$vars_cat
      joint[[type]][[indicator]] <- list()
      prepared$subset_categories <- NULL
      if (categorical && subset_local && prepared$vartype == "Local" && is.null(prepared$greg)) {
        # Compute the standard categorical marginals together, once. Their
        # fallback rules can depend on the whole set of category variances.
        domain_counts <- tapply(!is.na(data[[indicator]]), data[[type]], sum)
        domains <- names(domain_counts)[domain_counts > 0]
        if (length(domains)) {
          marginal_data <- data
          marginal_data[[type]] <- factor(data[[type]], levels = domains)
          marginal_design <- prepared$design
          marginal_design$variables <- marginal_data
          levels_response <- levels(data[[indicator]])
          marginal <- category_est(NULL, marginal_data, type, domains,
            length(domains), indicator, levels_response, length(levels_response),
            marginal_design, prepared$design_names, "Local", conf,
            stats::qnorm(.5 + conf / 200), !is.null(prepared$warn_df),
            prepared$warn_df, TRUE, local_nbh = local_nbh, local_context = local_context)
          prepared$subset_categories <- marginal$catsum
          prepared$warn_df <- marginal$warn_df
        }
      }
      categories <- if (categorical) levels(data[[indicator]]) else NA_character_
      for (category in categories) {
        response <- if (categorical) as.numeric(data[[indicator]] == category) else data[[indicator]]
        measures <- if (categorical) c("Proportion", "Total") else statistics
        tables <- list()
        for (statistic in measures) {
          result <- contrast_joint(prepared, response, type, indicator, statistic,
            subset_local, local_nbh, local_context, if (categorical) category else NULL)
          prepared$warn_df <- result$warn_df
          stat <- result$stat
          if (categorical) joint[[type]][[indicator]][[statistic]][[category]] <- stat else
            joint[[type]][[indicator]][[statistic]] <- stat
          rows <- list()
          for (name in names(contrasts[[type]])) {
            evaluated <- contrast_evaluate(stat, contrasts[[type]][[name]], conf,
              prepared$warn_df, c(type, name, paste(c(indicator, if (categorical) category, statistic), collapse = ": ")))
            prepared$warn_df <- evaluated$warn_df
            rows[[name]] <- data.frame(Type = type, Indicator = indicator,
              Contrast = name, as.list(evaluated$values), check.names = FALSE)
          }
          tables[[statistic]] <- do.call(rbind, rows)
        }
        if (categorical) {
          row <- tables$Proportion[, c("Type", "Indicator", "Contrast")]
          row$Category <- category
          row <- row[, c("Type", "Indicator", "Category", "Contrast")]
          for (statistic in measures) {
            values <- tables[[statistic]][, -(1:3), drop = FALSE]
            names(values) <- paste0(names(values), if (statistic == "Proportion") ".P" else ".U")
            row <- cbind(row, values)
          }
          answer$catsum <- rbind(answer$catsum, row)
        } else for (statistic in measures) {
          table <- paste0("contsum_", tolower(statistic))
          answer[[table]] <- rbind(answer[[table]], tables[[statistic]])
        }
      }
    }
  }
  for (table in names(answer)) if (!is.null(answer[[table]])) rownames(answer[[table]]) <- NULL
  if (return_joint) answer$joint <- joint
  if (!is.null(prepared$warn_df) && nrow(prepared$warn_df)) {
    assign("warn_df", prepared$warn_df, envir = .GlobalEnv)
    message("Contrast analysis produced warning messages; use warnprnt() to inspect warn_df.")
  }
  answer
}
