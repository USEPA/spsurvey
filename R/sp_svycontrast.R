#' Construct Joint Estimates for Survey Contrasts
#'
#' Construct estimates and their joint covariance for use with
#' \code{survey::svycontrast()}.
#'
#' @inheritParams contrast_analysis
#' @param subpops A single subpopulation column name (or \code{NULL} for
#'   population estimates).
#' @param statistics For continuous responses, a character vector selecting
#'   the \code{"Mean"} and/or \code{"Total"} statistics.
#'
#' @details
#' This function constructs a \code{svystat} object to be used
#' with \code{survey::svycontrast()} for custom contrasts. 
#'
#' @return A \code{svystat} vector with \code{coef()} and \code{vcov()} methods.
#'   Attributes \code{terms}, \code{unavailable}, \code{warnings},
#'   \code{covariance_method}, and \code{subset_independence} describe the terms,
#'   omissions and warnings (when relevant), covariance method, and whether to subset
#'   subpopulations to their local neighborhoods (when \code{vartype = "local"}).
#' @examples
#' set.seed(5)
#' dat <- data.frame(y1 = rnorm(80, 5), y2 = rnorm(80, 3), weight = 4,
#'   region = rep(c("East", "West"), 40), xc = runif(80), yc = runif(80))
#' joint <- sp_svycontrast(dat, vars_cont = c("y1", "y2"), subpops = "region",
#'   weight = "weight", xcoord = "xc", ycoord = "yc")
#' attr(joint, "terms")
#' survey::svycontrast(joint, quote(y1.East - y2.East))
#' survey::svycontrast(joint, quote(y1.East / y2.East))
#' @export
sp_svycontrast <- function(dframe, vars_cat = NULL, vars_cont = NULL,
                           subpops = NULL, siteID = NULL,
                           weight = "weight", xcoord = NULL, ycoord = NULL,
                           stratumID = NULL, clusterID = NULL, weight1 = NULL,
                           xcoord1 = NULL, ycoord1 = NULL, sizeweight = FALSE,
                           sweight = NULL, sweight1 = NULL, fpc = NULL,
                           formula = NULL, popsize = NULL, subpopsize = NULL,
                           vartype = "local", jointprob = "overton",
                           statistics = "Mean", subset_local = FALSE,
                           local_nbh = 10) {
  for (name in c("vars_cont", "vars_cat")) {
    value <- get(name)
    if (!is.null(value) && (!is.character(value) || !length(value) ||
        anyNA(value) || any(!nzchar(value)) || anyDuplicated(value))) {
      stop(name, " must be NULL or a nonempty character vector of unique column names.", call. = FALSE)
    }
  }
  if (is.null(vars_cat) && is.null(vars_cont)) stop("Supply vars_cat, vars_cont, or both.", call. = FALSE)
  if (length(intersect(vars_cat, vars_cont))) stop("A response cannot appear in both vars_cat and vars_cont.", call. = FALSE)
  if (!is.null(subpops) && (!is.character(subpops) || length(subpops) != 1L ||
      is.na(subpops) || !nzchar(subpops))) stop("subpops must be NULL or one column name.", call. = FALSE)
  if (!is.character(statistics) || length(statistics) != 1L || is.na(statistics) ||
      !statistics %in% c("Mean", "Total")) stop("statistics must be Mean or Total.", call. = FALSE)
  if (is.null(subset_local)) subset_local <- FALSE
  if (!is.logical(subset_local) || length(subset_local) != 1L || is.na(subset_local)) {
    stop("subset_local must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.null(subpops) && !is.null(subpopsize)) stop("subpopsize requires a subpopulation variable.", call. = FALSE)
  if (is.null(subpops) && !is.null(formula) && is.null(popsize)) stop("Population GREG estimates require popsize.", call. = FALSE)
  # conf is required by the shared validator, but no intervals are calculated here.
  prepared <- contrast_prepare(dframe, vars_cat, vars_cont, subpops, siteID,
    weight, xcoord, ycoord, stratumID, clusterID, weight1, xcoord1, ycoord1,
    sizeweight, sweight, sweight1, fpc, formula, popsize, subpopsize,
    vartype, jointprob, 95, local_nbh)
  data <- prepared$design$variables
  variables <- c(prepared$vars_cont, prepared$vars_cat)
  observed <- !is.na(data[[variables[1]]])
  for (variable in variables) {
    if (!identical(!is.na(data[[variable]]), observed)) {
      stop("Requested responses must have the same missing rows: ", variables[1],
        " and ", variable, " differ.", call. = FALSE)
    }
    if (is.numeric(data[[variable]]) && any(!is.finite(data[[variable]][observed]))) {
      stop("Requested response ", variable, " must be finite or missing.", call. = FALSE)
    }
  }
  if (!is.null(formula) && subset_local) {
    if (prepared$vartype == "Local") prepared$warn_df <- contrast_warning(
      prepared$warn_df, "GREG requires subset_local = FALSE.",
      "The explicitly supplied subset_local = TRUE was overridden.")
    subset_local <- FALSE
  }
  independent <- subset_local && prepared$vartype == "Local" &&
    is.null(prepared$greg) && !is.null(subpops)
  if (independent) prepared$warn_df <- contrast_warning(prepared$warn_df,
    "Direct subset-local estimates assume independence between domains.",
    "Within-domain covariance was retained; between-domain covariance was set to zero.")

  domains <- NA_character_
  domain_positions <- 1L
  if (!is.null(subpops)) {
    # Select output columns without changing the design or calibration factors.
    prepared_domains <- levels(data[[subpops]])
    domains <- if (is.factor(dframe[[subpops]])) levels(dframe[[subpops]]) else prepared_domains
    domains <- domains[domains %in% data[[subpops]]]
    domain_positions <- match(domains, prepared_domains)
  }
  terms <- columns <- estimates <- list()
  for (variable in variables) {
    categorical <- variable %in% prepared$vars_cat
    categories <- if (categorical) levels(data[[variable]]) else NA_character_
    for (category in categories) {
      response <- if (categorical) as.numeric(data[[variable]] == category) else data[[variable]]
      statistic <- if (categorical && statistics == "Mean") "Proportion" else statistics
      part <- contrast_contributions(prepared, response, subpops, statistic)
      scale <- if (statistic == "Proportion") 100 else 1
      terms[[length(terms) + 1L]] <- data.frame(variable = variable, category = category,
        subpop = if (is.null(subpops)) NA_character_ else subpops, level = domains,
        statistic = statistic, scale = scale, nResp = part$counts[domain_positions])
      columns[[length(columns) + 1L]] <- scale * part$contributions[, domain_positions, drop = FALSE]
      estimates[[length(estimates) + 1L]] <- scale * part$estimate[domain_positions]
    }
  }
  if (!length(terms)) stop("No requested estimates are available.", call. = FALSE)
  terms <- do.call(rbind, terms)
  terms$term <- make.unique(vapply(seq_len(nrow(terms)), function(i) {
    components <- unlist(terms[i, c("variable", "category", "level")], use.names = FALSE)
    paste(components[!is.na(components)], collapse = ".")
  }, character(1)), sep = "..")
  terms <- terms[, c("term", "variable", "category", "subpop", "level", "statistic", "scale", "nResp")]
  estimate <- unlist(estimates, use.names = FALSE)
  z <- do.call(cbind, columns)
  keep <- is.finite(estimate)
  unavailable <- terms[!keep, , drop = FALSE]
  unavailable$reason <- rep("No responding members or a nonpositive calibrated denominator.", nrow(unavailable))
  if (!any(keep)) stop("No requested estimates are available.", call. = FALSE)
  terms <- terms[keep, , drop = FALSE]
  estimate <- estimate[keep]
  z <- z[, keep, drop = FALSE]
  covariance <- matrix(0, ncol(z), ncol(z))
  blocks <- if (independent) split(seq_len(nrow(terms)),
    factor(terms$level, levels = unique(terms$level))) else list(seq_len(nrow(terms)))
  methods <- list()
  usable <- rep(TRUE, nrow(terms))
  local_context <- localmean_context(local_nbh)
  warn_df <- prepared$warn_df
  for (i in seq_along(blocks)) {
    idx <- blocks[[i]]
    domain <- if (independent) terms$level[idx[1]] else NA_character_
    rows <- if (!is.null(prepared$greg)) rep(TRUE, nrow(data)) else observed
    if (independent) rows <- rows & data[[subpops]] %in% domain
    fallback <- contrast_domain_fallback(prepared, rows, subpops, unique(terms$level[idx]))
    result <- contrast_covariance(prepared$design, z[, idx, drop = FALSE], rows,
      prepared$vartype, prepared$design_names, warn_df,
      c(if (is.null(subpops)) NA_character_ else subpops, domain, "joint estimates"),
      local_nbh, local_context, fallback, subset_local = independent)
    warn_df <- result$warn_df
    reason <- if (is.null(result$reason)) "" else result$reason
    methods[[i]] <- data.frame(domain = domain, requested = prepared$vartype,
      used = result$method, reason = reason,
      notes = paste(unique(unlist(result$notes)), collapse = "\n"))
    if (!contrast_valid_covariance(result$covariance)) {
      if (!independent) stop("The joint covariance is unavailable, including its survey fallback.", call. = FALSE)
      omitted <- terms[idx, , drop = FALSE]
      omitted$reason <- "The domain covariance is unavailable, including its survey fallback."
      unavailable <- rbind(unavailable, omitted)
      usable[idx] <- FALSE
    } else covariance[idx, idx] <- result$covariance
  }
  if (!any(usable)) stop("No requested estimates have usable covariance.", call. = FALSE)
  if (nrow(unavailable)) warn_df <- contrast_warning(warn_df,
    paste("Unavailable terms:", paste(unavailable$term, collapse = ", ")),
    "These terms were omitted from the joint object; see its unavailable attribute.")
  terms <- terms[usable, , drop = FALSE]
  rownames(terms) <- NULL
  covariance <- covariance[usable, usable, drop = FALSE]
  dimnames(covariance) <- list(terms$term, terms$term)
  answer <- greg_stat(estimate[usable], covariance,
    terms$term, statistic = tolower(statistics))
  if (!is.null(warn_df) && nrow(warn_df)) {
    warn_df$func[warn_df$func == "contrast_analysis"] <- "sp_svycontrast"
    warn_df <<- warn_df
    message("Joint estimation produced warning messages; use warnprnt() to inspect warn_df.")
  }
  attr(answer, "terms") <- terms
  attr(answer, "unavailable") <- if (nrow(unavailable)) unavailable else NULL
  attr(answer, "warnings") <- warn_df
  attr(answer, "covariance_method") <- do.call(rbind, methods)
  attr(answer, "subset_independence") <- independent
  answer
}
