# The same Wald/Woodruff inversion as survey::oldsvyquantile(ties="rounded"):
# retain p-centering, t critical values, rounded interpolation and endpoint
# clamping. Only the CDF variance comes from the existing local CDF machinery.
percentile_local_interval <- function(points, probabilities, design, design_names,
                                      type, domain, variable, conf, method,
                                      subset_local, warn_ind, warn_df) {
  lower <- upper <- se <- rep(NA_real_, length(points))
  valid <- is.finite(points)
  y <- design$variables[[variable]]
  keep <- design$variables[[type]] %in% domain & !is.na(y)
  w <- as.numeric(weights(design))[keep]
  if (!any(valid)) return(list(se = se, lower = lower, upper = upper, warn_ind = warn_ind, warn_df = warn_df))
  if (any(w < 0) || sum(w) <= 0) {
    warn_df <- rbind(warn_df, data.frame(func = "percentile_local_interval", subpoptype = type,
      subpop = domain, indicator = variable, stratum = NA_character_,
      warning = "Nonpositive domain size or negative weights prevent monotone percentile inversion.",
      action = "Legacy percentile point estimates were retained; local interval uncertainty is NA."))
    return(list(se = se, lower = lower, upper = upper, warn_ind = TRUE, warn_df = warn_df))
  }
  thresholds <- points[valid]
  p_cdf <- vapply(thresholds, function(q) sum(w * (y[keep] <= q)) / sum(w), numeric(1))
  local <- cdf_localmean_prop(type, domain, 1L, variable, design, design_names,
    thresholds, length(thresholds), as.data.frame(t(p_cdf)), stats::qnorm(.5 + conf / 200),
    warn_ind, warn_df, subset_local = subset_local)
  domain_design <- design[keep, ]
  critical <- stats::qt(.5 + conf / 200, df = survey::degf(domain_design))
  sd_cdf <- as.numeric(local$stderr_P[1, ])
  p <- probabilities[valid]
  bounds <- as.numeric(survey::oldsvyquantile(stats::reformulate(variable), domain_design,
    quantiles = c(p - critical * sd_cdf, p + critical * sd_cdf),
    ci = FALSE, na.rm = TRUE, ties = "rounded", method = method))
  k <- length(p)
  lower[valid] <- bounds[seq_len(k)]
  upper[valid] <- bounds[k + seq_len(k)]
  se[valid] <- (upper[valid] - lower[valid]) / (2 * critical)
  list(se = se, lower = lower, upper = upper, warn_ind = local$warn_ind, warn_df = local$warn_df)
}
