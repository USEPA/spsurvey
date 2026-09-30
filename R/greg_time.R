# Prepare one occasion using the same validation and survey-design constructor
# as status estimation. Repeated occasions must specify their totals explicitly.
greg_time_args <- function(env) {
  fields <- c("dframe", "subpops", "siteID", "weight", "xcoord", "ycoord", "stratumID",
    "clusterID", "weight1", "xcoord1", "ycoord1", "sizeweight", "sweight", "sweight1",
    "fpc", "formula", "popsize", "subpopsize", "vartype", "jointprob", "conf", "All_Sites")
  args <- mget(fields, envir = env, inherits = FALSE)
  args$revisitwgt <- if (exists("revisitwgt", env, inherits = FALSE)) get("revisitwgt", env) else FALSE
  args
}

greg_build_occasion <- function(args, vars_cat, vars_cont) {
  data <- args$dframe
  if (inherits(data, "sf")) {
    xy <- sf::st_coordinates(data)
    data <- sf::st_drop_geometry(data)
    data$.greg_x <- xy[, 1]
    data$.greg_y <- xy[, 2]
    args$xcoord <- ".greg_x"
    args$ycoord <- ".greg_y"
  }
  data <- as.data.frame(data)
  if (!nrow(data)) stop("Each GREG occasion must contain observations.", call. = FALSE)
  if (is.null(args$siteID)) {
    args$siteID <- ".greg_id"
    data$.greg_id <- seq_len(nrow(data))
  }
  if (is.null(args$subpops) || isTRUE(args$All_Sites)) {
    args$subpops <- unique(c(args$subpops, "All_Sites"))
    data$All_Sites <- factor("All Sites")
  }
  fields <- c("siteID", "weight", "xcoord", "ycoord", "stratumID", "clusterID",
    "weight1", "xcoord1", "ycoord1", "sweight", "sweight1")
  design_names <- args[fields]
  # input_check() writes the validated single-stage FPC to fpcsize.
  design_names$fpcsize <- if (!is.null(args$fpc)) "fpcsize" else NULL
  design_names$Ncluster <- design_names$stage1size <- NULL
  checked <- input_check(data, design_names, vars_cat, vars_cont, NULL, NULL,
    args$subpops, args$sizeweight, args$fpc, NULL, args$vartype, args$jointprob,
    args$conf, error_ind = FALSE, error_vec = NULL, preserve_factors = TRUE)
  if (checked$error_ind) stop(paste(checked$error_vec, collapse = "\n"), call. = FALSE)
  data <- checked$dframe
  ids <- as.character(data[[args$siteID]])
  if (anyNA(ids) || anyDuplicated(ids)) stop("Site IDs must be unique and nonmissing within each GREG occasion.", call. = FALSE)
  design <- survey_design(data, args$siteID, args$weight, !is.null(args$stratumID),
    args$stratumID, FALSE, NULL, NULL, args$sizeweight, args$sweight, NULL,
    !is.null(args$fpc), design_names$fpcsize, NULL, NULL, checked$vartype, checked$jointprob)
  greg <- greg_prepare(design, args$formula, args$popsize, args$subpopsize, checked$subpops, NULL)
  list(data = data, base = design, greg = greg, ids = ids,
    design_names = design_names, vartype = checked$vartype, subpops = checked$subpops,
    revisitwgt = args$revisitwgt)
}

greg_occasions <- function(args, occasionID, occasions, vars_cat, vars_cont, check_revisits = TRUE) {
  if (length(occasionID) != 1L || !occasionID %in% names(args$dframe) ||
      length(args$siteID) != 1L || !args$siteID %in% names(args$dframe)) {
    stop("GREG change/trend requires occasion and site ID columns.", call. = FALSE)
  }
  if (!is.list(args$popsize) || is.data.frame(args$popsize) ||
      is.null(names(args$popsize)) || anyDuplicated(names(args$popsize)) ||
      !setequal(names(args$popsize), occasions)) {
    stop("For GREG change/trend, popsize must be a named list with one totals vector (or NULL) per occasion.", call. = FALSE)
  }
  if (!is.null(args$subpopsize)) greg_named_list(args$subpopsize, occasions, "subpopsize (occasions)")
  result <- stats::setNames(vector("list", length(occasions)), occasions)
  for (occasion in occasions) {
    one <- args
    one$dframe <- args$dframe[as.character(args$dframe[[occasionID]]) %in% occasion, , drop = FALSE]
    # Align panels by site ID before constructing the design and projection.
    one$dframe <- one$dframe[order(as.character(one$dframe[[args$siteID]])), , drop = FALSE]
    one$popsize <- args$popsize[[occasion]]
    one$subpopsize <- args$subpopsize[[occasion]]
    result[[occasion]] <- greg_build_occasion(one, vars_cat, vars_cont)
  }
  # Partial overlaps use the existing repeat-site covariance approximation.
  # The original design strata identify independent covariance components.
  if (check_revisits) for (i in seq_along(result)) for (j in seq_len(i - 1L)) {
    first <- result[[i]]
    second <- result[[j]]
    ids <- intersect(first$ids, second$ids)
    if (!length(ids)) next
    a <- match(ids, first$ids)
    b <- match(ids, second$ids)
    if (!identical(as.character(first$base$strata[[1]][a]), as.character(second$base$strata[[1]][b]))) {
      stop("Revisited GREG sites must retain the same sampling stratum across occasions.", call. = FALSE)
    }
    if (isTRUE(args$revisitwgt) && !isTRUE(all.equal(
        as.numeric(weights(first$base))[a], as.numeric(weights(second$base))[b]))) {
      stop("revisitwgt = TRUE requires equal original weights at matched sites; use FALSE for unequal weights.", call. = FALSE)
    }
  }
  result
}

greg_time_estimate <- function(occasions, responses, type, domain, ratio, conf, revisit_covariance = TRUE) {
  k <- length(occasions)
  estimates <- counts <- numeric(k)
  covariance <- matrix(0, k, k)
  contributions <- contexts <- vector("list", k)
  warn_df <- NULL
  for (j in seq_len(k)) {
    one <- occasions[[j]]
    q <- responses[[j]]
    keep <- one$data[[type]] %in% domain & !is.na(q)
    context <- one$greg$contexts[[type]][[domain]]
    if (is.null(context)) stop("Every requested domain must be represented in each occasion's factor levels.", call. = FALSE)
    ans <- greg_estimate(context, q, keep, ratio, one$vartype, one$design_names,
      conf, warn_df, c(type, domain, NA))
    warn_df <- ans$warn_df
    estimates[j] <- ans$estimate
    counts[j] <- sum(keep)
    covariance[j, j] <- ans$se^2
    if (revisit_covariance) {
      q[!keep] <- 0
      if (ratio) q <- q - as.numeric(keep) * estimates[j]
      contributions[[j]] <- greg_residual_contributions(context, q)
      if (ratio) contributions[[j]] <- contributions[[j]] / sum(context$calibrated_weights * keep)
    }
    contexts[[j]] <- context
  }
  if (revisit_covariance) for (j in seq_len(k)) for (i in seq_len(j - 1L)) {
    if (any(!is.finite(estimates[c(i, j)]))) next
    cross <- greg_revisit_covariance(occasions[[i]], occasions[[j]],
      contributions[[i]], contributions[[j]], contexts[[i]], warn_df)
    bound <- min(sqrt(covariance[i, i] * covariance[j, j]),
      (covariance[i, i] + covariance[j, j]) / 2)
    if (is.finite(bound) && abs(cross$covariance) > bound &&
        abs(cross$covariance) - bound < 1e-12 * max(1, bound)) {
      cross$covariance <- sign(cross$covariance) * bound
    }
    covariance[i, j] <- covariance[j, i] <- cross$covariance
    warn_df <- cross$warn_df
  }
  # Pairwise overlap estimates need not form a positive semidefinite matrix.
  # Retain all marginal variances and report an independence
  # fallback instead of silently clipping a negative contrast variance.
  if (all(is.finite(covariance)) && min(eigen(covariance, symmetric = TRUE,
      only.values = TRUE)$values) < -1e-10 * max(1, abs(covariance))) {
    covariance[row(covariance) != col(covariance)] <- 0
    warn_df <- greg_warning(warn_df, "Repeat-site covariances produced an invalid joint occasion covariance matrix.",
      "Cross-occasion covariance was omitted; all full-occasion variances were retained.", c(type, domain, NA))
  }
  list(stat = greg_stat(estimates, covariance, paste0("occasion", seq_len(k))),
    nResp = counts, warn_df = warn_df)
}

greg_interval_fields <- function(estimate, se, conf, suffix = "", estimate_name = "Estimate") {
  critical <- stats::qnorm(.5 + conf / 200)
  values <- c(estimate, se, critical * se, estimate - critical * se, estimate + critical * se)
  stats::setNames(as.list(values), paste0(c(estimate_name, "StdError", "MarginofError",
    paste0("LCB", conf, "Pct"), paste0("UCB", conf, "Pct")), suffix))
}

greg_change_row <- function(ans, occasions, type, domain, variable, conf, scale = 1, suffix = "") {
  difference <- survey::svycontrast(ans$stat, c(occasion1 = -1, occasion2 = 1))
  row <- as.data.frame(greg_interval_fields(as.numeric(stats::coef(difference)) * scale,
    as.numeric(survey::SE(difference)) * scale, conf, suffix, "DiffEst"))
  for (j in 1:2) {
    row[[paste0("nResp_", j)]] <- ans$nResp[j]
    fields <- greg_interval_fields(as.numeric(ans$stat[j]) * scale,
      sqrt(stats::vcov(ans$stat)[j, j]) * scale, conf, paste0(suffix, "_", j))
    row[names(fields)] <- fields
  }
  row
}

greg_change <- function(args, vars_cat, vars_cont, test, surveyID, survey_names) {
  available <- unique(as.character(args$dframe[[surveyID]]))
  if (is.null(survey_names)) survey_names <- available
  survey_names <- as.character(survey_names)
  if (length(survey_names) != 2L || anyNA(survey_names) || !all(survey_names %in% available)) {
    stop("GREG change requires two named survey occasions.", call. = FALSE)
  }
  if (!all(test %in% c("mean", "total", "median"))) stop("Unknown change statistic.", call. = FALSE)
  occasions <- greg_occasions(args, surveyID, survey_names, vars_cat, vars_cont)
  out <- list(catsum = NULL, contsum_mean = NULL, contsum_total = NULL, contsum_median = NULL)
  warn_df <- do.call(rbind, lapply(occasions, function(x) x$greg$warn_df))
  for (type in occasions[[1]]$subpops) for (domain in levels(occasions[[1]]$data[[type]])) {
    for (variable in c(vars_cont, vars_cat)) {
      y <- lapply(occasions, function(x) x$data[[variable]])
      ids <- data.frame(Survey_1 = survey_names[1], Survey_2 = survey_names[2],
        Type = type, Subpopulation = domain, Indicator = variable)
      if (variable %in% vars_cont) for (statistic in intersect(test, c("mean", "total"))) {
        ans <- greg_time_estimate(occasions, y, type, domain, statistic == "mean", args$conf)
        row <- cbind(ids, greg_change_row(ans, occasions, type, domain, variable, args$conf))
        target <- paste0("contsum_", statistic)
        out[[target]] <- rbind(out[[target]], row)
        warn_df <- rbind(warn_df, ans$warn_df)
      }
      if (variable %in% vars_cat || "median" %in% test) {
        if (variable %in% vars_cat) {
          categories <- unique(unlist(lapply(y, levels)))
          target <- "catsum"
          indicators <- lapply(categories, function(category) lapply(y, function(z) as.numeric(z == category)))
        } else {
          # Preserve the existing median-change estimand: change in the share
          # below the first occasion's median, not a difference of medians.
          one <- occasions[[1]]
          keep <- one$data[[type]] %in% domain & !is.na(y[[1]])
          a <- one$greg$contexts[[type]][[domain]]$calibrated_weights[keep]
          if (sum(keep) < 2 || any(a < 0)) stop("Median-change thresholds require a monotone calibrated CDF.", call. = FALSE)
          threshold <- greg_quantiles(y[[1]][keep], a, .5)
          categories <- c("<= Median", "> Median")
          indicators <- list(lapply(y, function(z) as.numeric(z <= threshold)),
            lapply(y, function(z) as.numeric(z > threshold)))
          target <- "contsum_median"
        }
        for (j in seq_along(categories)) {
          p <- greg_time_estimate(occasions, indicators[[j]], type, domain, TRUE, args$conf)
          u <- greg_time_estimate(occasions, indicators[[j]], type, domain, FALSE, args$conf)
          rp <- greg_change_row(p, occasions, type, domain, variable, args$conf, 100, ".P")
          ru <- greg_change_row(u, occasions, type, domain, variable, args$conf, 1, ".U")
          row <- cbind(ids, Category = categories[j], rp, ru[, !names(ru) %in% c("nResp_1", "nResp_2")])
          for (occasion in 1:2) {
            keep <- occasions[[occasion]]$data[[type]] %in% domain
            row[[paste0("nResp_", occasion)]] <- sum(indicators[[j]][[occasion]][keep], na.rm = TRUE)
          }
          order <- c(names(ids), "Category", names(rp)[1:5], names(ru)[1:5],
            "nResp_1", names(rp)[7:11], names(ru)[7:11],
            "nResp_2", names(rp)[13:17], names(ru)[13:17])
          out[[target]] <- rbind(out[[target]], row[, order])
          warn_df <- rbind(warn_df, p$warn_df, u$warn_df)
        }
      }
    }
  }
  greg_publish_warnings(warn_df)
  out
}

greg_trend <- function(args, vars_cat, vars_cont, yearID, model_cat, model_cont) {
  years <- sort(unique(as.numeric(as.character(args$dframe[[yearID]]))))
  if (length(years) < 3L || any(!is.finite(years))) stop("GREG trends require at least three finite numeric occasions.", call. = FALSE)
  occasions <- greg_occasions(args, yearID, as.character(years), vars_cat, vars_cont, check_revisits = FALSE)
  out <- list(catsum = NULL, contsum = NULL)
  warn_df <- do.call(rbind, lapply(occasions, function(x) x$greg$warn_df))
  elapsed <- years - min(years)
  for (type in occasions[[1]]$subpops) for (domain in levels(occasions[[1]]$data[[type]])) {
    for (variable in c(vars_cont, vars_cat)) {
      categorical <- variable %in% vars_cat
      y <- lapply(occasions, function(one) one$data[[variable]])
      categories <- if (categorical) unique(unlist(lapply(y, levels))) else NA_character_
      model <- if (categorical) model_cat else model_cont
      for (category in categories) {
        response <- if (categorical) lapply(y, function(z) as.numeric(z == category)) else y
        ans <- greg_time_estimate(occasions, response, type, domain, TRUE, args$conf, revisit_covariance = FALSE)
        warn_df <- rbind(warn_df, ans$warn_df)
        if (categorical) ans$stat <- greg_stat(as.numeric(ans$stat) * 100, stats::vcov(ans$stat) * 10000, names(ans$stat))
        variance <- diag(stats::vcov(ans$stat))
        keep <- is.finite(ans$stat)
        if (model == "WLR") keep <- keep & is.finite(variance) & variance > 0
        if (sum(keep) < 3L) {
          warn_df <- greg_warning(warn_df, "Undefined occasion estimate or nonpositive WLR variance.",
            "No GREG trend was fitted.", c(type, domain, variable))
          next
        }
        # Preserve trend_analysis's second-stage regression and t inference.
        # Annual GREG variances enter WLR weights only; SLR ignores them.
        # Neither model uses repeat-site covariance across years.
        annual <- data.frame(value = as.numeric(ans$stat)[keep], year = elapsed[keep],
          weight = if (model == "WLR") 1 / variance[keep] else 1)
        fit <- stats::lm(value ~ year, data = annual, weights = annual$weight)
        summary <- summary(fit)
        estimate <- summary$coefficients[, 1]
        se <- summary$coefficients[, 2]
        ci <- stats::confint(fit, level = args$conf / 100)
        p <- summary$coefficients[, 4]
        row <- data.frame(Type = type, Subpopulation = domain, Indicator = variable)
        if (categorical) row$Category <- category
        values <- c(estimate[2], se[2], ci[2, ], p[2], estimate[1], se[1], ci[1, ], p[1],
          summary$r.squared, summary$adj.r.squared)
        names <- c("Trend_Estimate", "Trend_Std_Error", paste0("Trend_LCB", args$conf, "Pct"),
          paste0("Trend_UCB", args$conf, "Pct"), "Trend_p_Value", "Intercept_Estimate",
          "Intercept_Std_Error", paste0("Intercept_LCB", args$conf, "Pct"),
          paste0("Intercept_UCB", args$conf, "Pct"), "Intercept_p_Value", "R_Squared", "Adj_R_Squared")
        row[names] <- as.list(values)
        target <- if (categorical) "catsum" else "contsum"
        out[[target]] <- rbind(out[[target]], row)
      }
    }
  }
  greg_publish_warnings(warn_df)
  out
}
