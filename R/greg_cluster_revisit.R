# Apply the direct two-stage repeat-site calculation to GREG residuals.
# The full-occasion denominator is already in z for ratios. Undo only the
# design weights, then let changevar_mean() supply both stages and its
# established singleton, small-PSU and failed-neighborhood conventions.
greg_cluster_revisit <- function(first, second, z1, z2, i, j, warn_df,
                                 local_nbh = 15, local_context = NULL) {
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
    d1 <- as.numeric(weights(first$base))[ii]
    d2 <- as.numeric(weights(second$base))[jj]
    v1 <- as.numeric(z1)[ii] / d1
    v2 <- as.numeric(z2)[jj] / d2
    mean1 <- stats::weighted.mean(v1, d1)
    mean2 <- stats::weighted.mean(v2, d2)

    # For covariance/correlation use the first occasion's PSU assignment and
    # coordinates, as direct change does. For unequal revisit weights the
    # correlation uses unit weights at BOTH stages; each marginal variance
    # uses its own occasion's stage weights and geometry.
    calculate <- function(one, rows, a, b, ma, mb, correlation = FALSE) {
      data <- one$base$variables[rows, , drop = FALSE]
      names <- one$design_names
      w1 <- if (correlation) rep(1, length(rows)) else if (!is.null(data$wgt1)) data$wgt1 else 1 / data$prb1
      w2 <- if (correlation) rep(1, length(rows)) else if (!is.null(data$wgt2)) data$wgt2 else 1 / data$prb2
      ans <- changevar_mean(a, b, w2,
        data[[names$xcoord]], data[[names$ycoord]], !correlation, ma, mb,
        !is.null(names$stratumID), h, TRUE, data[[names$clusterID]],
        w1, data[[names$xcoord1]], data[[names$ycoord1]],
        if (one$vartype == "Local") "Local" else "SRS",
        !is.null(warn_df), warn_df, rep(NA_character_, 3),
        local_nbh, local_context)
      warn_df <<- ans$warn_df
      # changevar_mean() divides covariance by the repeat-set weight total
      # squared. z already has the desired full-occasion scale, so undo it.
      if (correlation) ans$rslt else ans$rslt * sum(w1 * w2)^2
    }
    if (isTRUE(first$revisitwgt)) {
      cross <- calculate(first, ii, v1, v2, mean1, mean2)
    } else {
      correlation <- calculate(first, ii, v1, v2, mean1, mean2, TRUE)
      var1 <- calculate(first, ii, v1, v1, mean1, mean1)
      var2 <- calculate(second, jj, v2, v2, mean2, mean2)
      cross <- correlation * sqrt(var1 * var2)
    }
    if (!is.finite(cross)) {
      warn_df <- greg_warning(warn_df, "Two-stage repeat-site residual covariance was undefined.",
        "This stratum contributed zero cross-occasion covariance; full-occasion variances were retained.")
    } else covariance <- covariance + cross
  }
  list(covariance = covariance, warn_df = warn_df)
}
