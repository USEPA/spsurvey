# Woodruff inversion with the same normal CDF interval and mathematical
# quantile rule used by survey::svyquantile(interval.type="mean", df=Inf).
greg_quantiles <- function(y, w, p) {
  ans <- rep(NA_real_, length(p))
  valid <- is.finite(p) & p >= 0 & p <= 1
  if (any(valid)) {
    design <- survey::svydesign(~1, weights = ~w, data = data.frame(y = y, w = w))
    ans[valid] <- as.numeric(stats::coef(survey::svyquantile(~y, design,
      quantiles = p[valid], ci = FALSE, qrule = "math")))
  }
  ans
}

greg_percentile_est <- function(result, dframe, itype, domains, ivar, greg,
                                design_names, vartype, conf, pctval, warn_df) {
  y <- dframe[[ivar]]
  critical <- stats::qnorm(.5 + conf / 200)
  for (domain in sort(domains)) {
    keep <- dframe[[itype]] %in% domain & !is.na(y)
    context <- greg$contexts[[itype]][[domain]]
    a <- context$calibrated_weights[keep]
    invalid <- sum(keep) < 2L || any(a < 0) || sum(a) <= 0
    estimate <- lower <- upper <- rep(NA_real_, length(pctval))
    if (invalid) {
      warn_df <- greg_warning(warn_df,
        "GREG quantiles require at least two responses and nonnegative domain weights with positive total.",
        "Quantiles and intervals were set to NA; a nonmonotone calibrated CDF was not inverted.",
        c(itype, domain, ivar))
    } else {
      estimate <- greg_quantiles(y[keep], a, pctval / 100)
      for (j in seq_along(estimate)) {
        cdf <- greg_estimate(context, as.numeric(y <= estimate[j]), keep,
          TRUE, vartype, design_names, conf, warn_df, c(itype, domain, ivar))
        warn_df <- cdf$warn_df
        bounds <- greg_quantiles(y[keep], a, c(cdf$lower, cdf$upper))
        lower[j] <- bounds[1]
        upper[j] <- bounds[2]
      }
      if (anyNA(c(lower, upper))) {
        warn_df <- greg_warning(warn_df,
          "A GREG quantile CDF confidence bound lies outside [0, 1] or is undefined.",
          "The corresponding inverse bound and interval-derived standard error are NA, as in survey's mean-interval inversion.",
          c(itype, domain, ivar))
      }
    }
    se <- (upper - lower) / (2 * critical)
    result <- rbind(result, data.frame(Type = itype, Subpopulation = domain,
      Indicator = ivar, Statistic = paste0(pctval, "Pct"),
      nResp = vapply(estimate, function(q) if (is.na(q)) NA_integer_ else sum(y[keep] <= q), integer(1)),
      Estimate = estimate, StdError = se, MarginofError = critical * se,
      LCB = lower, UCB = upper))
  }
  list(pctsum = result, warn_ind = !is.null(warn_df), warn_df = warn_df)
}
