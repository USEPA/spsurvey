# Shared preparation and linearization for single-stage GREG analysis.
# Calibration remains owned by survey. Only the local covariance operator
# is replaced; neighborhoods use the uncalibrated analysis weights. With
# sizeweight = TRUE, survey_design() has already multiplied design and size
# weights. Calibration totals must be on that size-weighted scale; do not
# multiply either the weights or supplied totals by size again here.

greg_legacy_call <- function(call, fun, formula) {
  # Recognize legacy positional calls where popsize occupies formula, and
  # rematch them to the previous signature before evaluation. Explicitly
  # named formula/subpopsize arguments and formula objects use the current structure.
  supplied <- names(as.list(call)[-1])
  named_new <- any(vapply(supplied[nzchar(supplied) & supplied != "subpop"], function(x) {
    startsWith("formula", x) || startsWith("subpopsize", x)
  }, logical(1)))
  if (named_new || inherits(formula, "formula")) return(NULL)
  formals(fun) <- formals(fun)[!names(formals(fun)) %in% c("formula", "subpopsize", "subpop")]
  names(call)[names(call) == "subpop"] <- "subpops"
  match.call(definition = fun, call = call)
}

greg_warning <- function(warn_df, warning, action, labels = rep(NA_character_, 3)) {
  rbind(warn_df, data.frame(
    func = I("greg"), subpoptype = labels[1], subpop = labels[2],
    indicator = labels[3], stratum = NA_character_,
    warning = I(warning), action = I(action)
  ))
}

greg_check_call <- function(formula, subpopsize, clusterID) {
  if (is.null(formula)) {
    if (!is.null(subpopsize)) stop("subpopsize requires a GREG formula.", call. = FALSE)
    return(invisible(NULL))
  }
  if (!is.null(clusterID)) {
    stop("GREG estimation is not yet supported for two-stage (clustered) samples.", call. = FALSE)
  }
  if (!inherits(formula, "formula") || length(formula) != 2L) {
    stop("formula must be a one-sided GREG formula, for example ~ x1 + x2.", call. = FALSE)
  }
  if ("." %in% all.names(formula)) {
    stop("Specify auxiliary variables explicitly; '.' is not supported in a GREG formula.", call. = FALSE)
  }
}

greg_totals <- function(totals, columns, label) {
  if (!is.numeric(totals) || !is.null(dim(totals)) ||
      is.null(names(totals)) || anyDuplicated(names(totals)) ||
      !setequal(names(totals), columns) || any(!is.finite(totals))) {
    stop(label, " must be a finite named numeric vector matching the GREG model-matrix columns: ",
      paste(columns, collapse = ", "), ".", call. = FALSE)
  }
  totals[columns]
}

greg_named_list <- function(x, allowed, label) {
  if (!is.list(x) || is.data.frame(x) || is.null(names(x)) ||
      anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x)) ||
      any(!names(x) %in% allowed)) {
    stop(label, " must be a named list with names drawn from: ",
      paste(allowed, collapse = ", "), ".", call. = FALSE)
  }
}

greg_calibrate <- function(design, mm, totals, label) {
  totals <- greg_totals(totals, colnames(mm), label)
  zero <- colSums(abs(mm)) == 0
  if (any(zero & totals != 0)) {
    stop(label, " requests a nonzero total for a model column with no sample support.", call. = FALSE)
  }
  # Remove only structural zero columns with known zero totals. Keep the
  # original column specification for validation of other domains.
  mm <- mm[, !zero, drop = FALSE]
  totals <- totals[!zero]
  if (!ncol(mm)) stop(label, " has no estimable calibration columns.", call. = FALSE)
  d <- as.numeric(weights(design))
  if (any(!is.finite(d) | d <= 0)) {
    stop("GREG requires finite positive original design weights.", call. = FALSE)
  }
  qr_x <- qr(mm * sqrt(d))
  if (qr_x$rank < ncol(mm)) {
    stop(label, " has a rank-deficient GREG model matrix.", call. = FALSE)
  }
  # Sample values do not determine the population support of an auxiliary:
  # an observed positive column can legitimately have a negative total.
  # Unbounded linear calibration follows survey's signed-total convention.
  # A single matrix-valued predictor preserves evaluated contrasts and masked
  # intercepts. It also keeps the formula short for deparsing.
  column <- tail(make.unique(c(names(design$variables), ".spsurvey_greg_matrix")), 1)
  auxiliary <- mm
  colnames(auxiliary) <- paste0("x", seq_len(ncol(mm)))
  design$variables[[column]] <- I(auxiliary)
  calibration_formula <- stats::reformulate(column, intercept = FALSE)
  names(totals) <- colnames(stats::model.matrix(calibration_formula, design$variables))
  calibrated <- survey::calibrate(design,
    calibration_formula, population = totals,
    calfun = "linear"
  )
  a <- as.numeric(weights(calibrated))
  if (any(!is.finite(a)) || any(a == 0)) {
    stop(label, " produced nonfinite or zero calibration weights; revise the constraints.", call. = FALSE)
  }
  if (any(abs(colSums(mm * a) - totals) > 1e-7 * pmax(1, abs(totals)))) {
    stop(label, " could not be matched by calibration.", call. = FALSE)
  }
  list(design = calibrated, base = design, matrix = mm, qr = qr_x, original_weights = d,
    calibrated_weights = a)
}

greg_prepare <- function(design, formula, popsize, subpopsize, subpops, warn_df) {
  data <- design$variables
  terms <- stats::terms(formula, data = data)
  if (length(attr(terms, "offset"))) stop("Offsets are not supported in a GREG formula.", call. = FALSE)
  mf <- stats::model.frame(terms, data, na.action = stats::na.pass,
    drop.unused.levels = FALSE)
  mm <- stats::model.matrix(terms, mf)
  if (nrow(mm) != nrow(data) || !ncol(mm) || any(!is.finite(mm))) {
    stop("The GREG model matrix must have columns and finite values for every design row.", call. = FALSE)
  }
  if (!is.null(subpopsize)) {
    greg_named_list(subpopsize, subpops, "subpopsize")
    for (type in names(subpopsize)) {
      greg_named_list(subpopsize[[type]], levels(data[[type]]),
        paste0("subpopsize$", type))
    }
  }
  population <- if (!is.null(popsize)) greg_calibrate(design, mm, popsize, "popsize") else NULL
  contexts <- stats::setNames(vector("list", length(subpops)), subpops)
  for (type in subpops) {
    lev <- levels(data[[type]])
    contexts[[type]] <- stats::setNames(vector("list", length(lev)), lev)
    for (domain in lev) {
      delta <- data[[type]] %in% domain
      totals <- subpopsize[[type]][[domain]]
      if (is.null(totals)) {
        if (is.null(population)) {
          stop("Supply popsize for domains without known subpopsize totals (",
            type, ": ", domain, ").", call. = FALSE)
        }
        contexts[[type]][[domain]] <- population
      } else {
        contexts[[type]][[domain]] <- greg_calibrate(design, mm * delta, totals,
          paste0("subpopsize$", type, "$", domain))
      }
    }
    # Complete partition totals should agree with the supplied population.
    supplied <- subpopsize[[type]]
    if (!is.null(popsize) && all(vapply(lev, function(x) !is.null(supplied[[x]]), logical(1)))) {
      combined <- Reduce(`+`, lapply(lev, function(x) greg_totals(supplied[[x]], colnames(mm), "subpopsize")))
      target <- greg_totals(popsize, colnames(mm), "popsize")
      if (any(abs(combined - target) > 1e-7 * pmax(1, abs(target)))) {
        stop("Complete subpopsize totals for ", type, " must sum to popsize.", call. = FALSE)
      }
    }
  }
  negative <- any(vapply(unlist(contexts, recursive = FALSE),
    function(x) any(x$calibrated_weights < 0), logical(1)))
  if (negative) {
    warn_df <- greg_warning(warn_df, "Linear calibration produced negative weights.",
      "Weights were retained; GREG totals and ratios use the survey calibration convention.")
  }
  list(contexts = contexts, warn_df = warn_df)
}

greg_estimate <- function(context, q, keep, ratio, vartype, design_names, conf,
                          warn_df, labels) {
  q <- as.matrix(q)
  colnames(q) <- paste0("estimate", seq_len(ncol(q)))
  q[!keep, ] <- 0
  if (any(!is.finite(q))) stop("GREG responses must be finite or missing.", call. = FALSE)
  v <- as.numeric(keep)
  a <- context$calibrated_weights
  if (!any(keep) || (ratio && sum(a * v) <= 0)) {
    warn_df <- greg_warning(warn_df,
      "The domain has no observed responses or a nonpositive calibrated denominator.",
      "The requested estimate and uncertainty were set to NA.", labels)
    empty <- rep(NA_real_, ncol(q))
    return(list(estimate = empty, se = empty, lower = empty, upper = empty,
      warn_df = warn_df))
  }
  design <- context$design
  stat <- if (ratio) {
    survey::svyratio(q, matrix(v, ncol = 1, dimnames = list(NULL, "size")), design)
  } else {
    survey::svytotal(q, design)
  }
  estimate <- as.numeric(stats::coef(stat))
  covariance <- stats::vcov(stat)
  if (vartype == "Local") {
    u <- if (ratio) q - outer(v, estimate) else q
    d <- context$original_weights
    residual <- qr.resid(context$qr, u * sqrt(d)) / sqrt(d)
    z <- residual * a
    if (ratio) z <- z / sum(a * v)
    strata <- split(seq_len(nrow(q)), design$strata[[1]], drop = TRUE)
    local_cov <- matrix(0, ncol(q), ncol(q))
    fallback <- NULL
    for (idx in strata) {
      if (length(idx) < 4L) {
        fallback <- "A sampling stratum has fewer than four sites."
        break
      }
      if (length(idx) > 2000L) {
        warn_df <- greg_warning(warn_df,
          "Local GREG variance requires neighborhoods for a stratum with more than 2,000 sites.",
          "Proceeding with the full stratum; computation may take substantial time and memory.", labels)
      }
      nb <- localmean_weight(design$variables[[design_names$xcoord]][idx],
        design$variables[[design_names$ycoord]][idx], prb = 1 / d[idx])
      if (is.null(nb)) {
        fallback <- "Local neighborhood weights could not be calculated."
        break
      }
      local_cov <- local_cov + localmean_cov(z[idx, , drop = FALSE], nb)
    }
    if (is.null(fallback)) {
      tol <- 1e-10 * max(1, abs(local_cov))
      if (any(!is.finite(local_cov)) ||
          min(eigen(local_cov, symmetric = TRUE, only.values = TRUE)$values) < -tol) {
        fallback <- "The local covariance estimate is not finite and positive semidefinite."
      }
    }
    if (is.null(fallback)) {
      covariance <- local_cov
    } else {
      warn_df <- greg_warning(warn_df, fallback,
        "Survey's calibrated design variance was used for the entire estimate.", labels)
    }
  }
  se <- sqrt(pmax(0, diag(covariance)))
  if (vartype == "Local") {
    margin <- stats::qnorm(0.5 + conf / 200) * se
    lower <- estimate - margin
    upper <- estimate + margin
  } else {
    ci <- stats::confint(stat, level = conf / 100)
    lower <- ci[, 1]
    upper <- ci[, 2]
  }
  list(estimate = estimate, se = se, lower = lower, upper = upper,
    covariance = covariance, warn_df = warn_df)
}

greg_summary_est <- function(result, dframe, itype, lev_itype, ivar, greg,
                             design_names, vartype, conf, mult, warn_df, ratio) {
  y <- dframe[[ivar]]
  for (domain in sort(lev_itype)) {
    keep <- dframe[[itype]] %in% domain & !is.na(y)
    est <- greg_estimate(greg$contexts[[itype]][[domain]], y, keep, ratio,
      vartype, design_names, conf, warn_df, c(itype, domain, ivar))
    warn_df <- est$warn_df
    result <- rbind(result, data.frame(Type = itype, Subpopulation = domain,
      Indicator = ivar, nResp = sum(keep), Estimate = est$estimate,
      StdError = est$se, MarginofError = mult * est$se,
      LCB = est$lower, UCB = est$upper))
  }
  list(result = result, warn_ind = !is.null(warn_df), warn_df = warn_df)
}

greg_distribution_est <- function(result, dframe, itype, lev_itype, ivar, greg,
                                  design_names, vartype, conf, mult, warn_df,
                                  categorical) {
  y <- dframe[[ivar]]
  values <- if (categorical) levels(y) else sort(unique(y[!is.na(y)]))
  if (!length(values)) stop("No observed values for GREG indicator ", ivar, ".", call. = FALSE)
  if (categorical) values <- c(values, "Total")
  for (domain in sort(lev_itype)) {
    keep <- dframe[[itype]] %in% domain & !is.na(y)
    for (j in seq_along(values)) {
      q <- if (categorical) {
        if (j == length(values)) rep(1, length(y)) else as.numeric(y == values[j])
      } else as.numeric(y <= values[j])
      context <- greg$contexts[[itype]][[domain]]
      prop <- greg_estimate(context, q, keep, TRUE, vartype, design_names,
        conf, warn_df, c(itype, domain, ivar))
      total <- greg_estimate(context, q, keep, FALSE, vartype, design_names,
        conf, prop$warn_df, c(itype, domain, ivar))
      warn_df <- total$warn_df
      row <- data.frame(Type = itype, Subpopulation = domain, Indicator = ivar,
        Value = values[j], nResp = sum(q[keep]), Estimate.P = 100 * prop$estimate,
        StdError.P = 100 * prop$se, MarginofError.P = 100 * mult * prop$se,
        LCB.P = 100 * prop$lower, UCB.P = 100 * prop$upper,
        Estimate.U = total$estimate, StdError.U = total$se,
        MarginofError.U = mult * total$se, LCB.U = total$lower, UCB.U = total$upper)
      if (categorical) names(row)[4] <- "Category"
      result <- rbind(result, row)
    }
  }
  list(result = result, warn_ind = !is.null(warn_df), warn_df = warn_df)
}
