# Repeat-site extension of change_est()/changevar_mean(). Regressions are
# fitted on full occasions. z contains calibrated, weighted contributions,
# already divided by the FULL calibrated domain size for ratios. Consequently
# the legacy overlap/full-size multipliers are already incorporated: they
# should not be applied a second time. Never refit on just the repeat sites.
greg_revisit_covariance <- function(first, second, z1, z2, context, warn_df) {
  ids <- intersect(first$ids, second$ids)
  if (!length(ids)) return(list(covariance = 0, warn_df = warn_df))
  i <- match(ids, first$ids)
  j <- match(ids, second$ids)
  d1 <- as.numeric(weights(first$base))
  d2 <- as.numeric(weights(second$base))
  same <- identical(first$ids, second$ids) &&
    isTRUE(all.equal(d1, d2)) && isTRUE(all.equal(first$base$fpc, second$base$fpc))
  if (same && first$vartype == "Local") {
    for (coordinate in c("xcoord", "ycoord")) {
      same <- same && isTRUE(all.equal(first$data[[first$design_names[[coordinate]]]],
        second$data[[second$design_names[[coordinate]]]]))
    }
  }
  if (same) {
    # The complete common design identifies covariance directly, including
    # survey FPC/PPS corrections; no overlap approximation is necessary.
    ans <- greg_joint_covariance(context, cbind(z1, z2), first$vartype, first$design_names, warn_df)
    return(list(covariance = ans$covariance[1, 2], warn_df = ans$warn_df))
  }
  covariance <- 0
  strata <- as.character(first$base$strata[[1]][i])
  for (h in unique(strata)) {
    ii <- i[strata == h]
    jj <- j[strata == h]
    if (length(ii) < 2L) {
      warn_df <- greg_warning(warn_df, paste0("Fewer than two repeat sites in stratum ", h, "."),
        "This stratum contributed zero cross-occasion covariance; full-occasion variances were retained.")
      next
    }
    x <- if (!is.null(first$design_names$xcoord)) first$data[[first$design_names$xcoord]][ii] else NULL
    y <- if (!is.null(first$design_names$ycoord)) first$data[[first$design_names$ycoord]][ii] else NULL
    # v = g * residual / full-domain denominator (denominator = 1 for totals).
    # Recenter on the repeat set as in the legacy repeat-site calculation.
    v <- cbind(as.numeric(z1)[ii] / d1[ii], as.numeric(z2)[jj] / d2[jj])
    centered <- cbind(v[, 1] - stats::weighted.mean(v[, 1], d1[ii]),
      v[, 2] - stats::weighted.mean(v[, 2], d2[jj]))
    z <- centered * cbind(d1[ii], d2[jj])
    # Use a single operator choice for all parts of this stratum's covariance.
    ds <- if (isTRUE(first$revisitwgt)) list(d1[ii]) else list(rep(1, length(ii)), d1[ii], d2[jj])
    nb <- if (first$vartype == "Local" && length(ii) >= 4L)
      lapply(ds, function(d) localmean_weight(x, y, 1 / d)) else list(NULL)
    local <- first$vartype == "Local" && !any(vapply(nb, is.null, logical(1)))
    if (first$vartype == "Local" && !local) {
      warn_df <- greg_warning(warn_df, paste0("Too few repeat sites or unavailable local neighborhoods in stratum ", h, "."),
        "Survey's with-replacement repeat-site covariance was used for this stratum.")
    }
    operator <- function(values, index) {
      values <- as.matrix(values)
      if (local) return(localmean_cov(values, nb[[index]]))
      # This is n_R * cov(values), the legacy infinite-population approximation,
      # calculated by survey. Marginal FPC/PPS variances remain unchanged.
      design <- survey::svydesign(~1, weights = ~d, data = data.frame(d = ds[[index]]))
      stats::vcov(survey::svytotal(values / ds[[index]], design))
    }
    if (isTRUE(first$revisitwgt)) {
      cross <- operator(z, 1)[1, 2]
    } else {
      equal <- operator(v, 1)
      scale <- sqrt(prod(diag(equal)))
      cross <- if (is.finite(scale) && scale > 0) equal[1, 2] / scale *
        sqrt(operator(z[, 1], 2)[1, 1] * operator(z[, 2], 3)[1, 1]) else 0
    }
    if (!is.finite(cross)) {
      warn_df <- greg_warning(warn_df, "Repeat-site residual covariance was undefined.",
        "This stratum contributed zero cross-occasion covariance; full-occasion variances were retained.")
    } else covariance <- covariance + cross
  }
  list(covariance = covariance, warn_df = warn_df)
}
