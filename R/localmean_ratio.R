# Local variance of a ratio across strata. Center on the overall ratio, not
# the within-stratum ratio: estimated domain/stratum sizes can be random.
localmean_ratio <- function(design, response, member, responded, estimate,
                            design_names, warn_df, labels, subset_local = FALSE,
                            local_nbh = 10, local_context = NULL,
                            type = "diagonal") {
  q <- as.matrix(response)
  v <- as.numeric(member & responded)
  q[v == 0, ] <- 0
  d <- as.numeric(weights(design))
  denominator <- sum(d * v)
  z <- (q - outer(v, estimate)) * d / denominator
  rows <- which(responded & (!subset_local | member))
  covariance <- if (type == "diagonal") rep(0, ncol(q)) else matrix(0, ncol(q), ncol(q))
  notes <- character()

  if (!is.null(design_names$clusterID)) {
    missing_psu <- !subset_local &&
      length(unique(design$cluster[[1]][rows])) != length(unique(design$cluster[[1]]))
    if (missing_psu) {
      covariance <- NULL
      notes <- "A sampled PSU has no usable responses."
    } else {
      part <- localmean_cluster_design(design, z, design_names, local_nbh,
        local_context, type, rows)
      covariance <- part$covariance
      notes <- part$notes
    }
  } else {
    for (idx in split(rows, design$strata[[1]][rows], drop = TRUE)) {
      if (!any(v[idx] != 0)) next
      nb <- NULL
      if (sum(v[idx]) >= 4L) {
        nb <- localmean_weight_internal(
          design$variables[[design_names$xcoord]][idx],
          design$variables[[design_names$ycoord]][idx], 1 / d[idx],
          local_nbh = local_nbh,
          local_context = localmean_group_context(local_context,
            paste0("stratum:", design$strata[[1]][idx[1]])))
      }
      if (is.null(nb)) {
        covariance <- NULL
        notes <- "A stratum has fewer than four domain responses or its local weights could not be calculated."
        break
      }
      contribution <- if (type == "diagonal") {
        vapply(seq_len(ncol(z)), function(j) localmean_var(z[idx, j], nb), numeric(1))
      } else localmean_cov(z[idx, , drop = FALSE], nb)
      covariance <- covariance + contribution
    }
  }

  invalid <- is.null(covariance) || any(!is.finite(covariance))
  if (!invalid) invalid <- if (type == "diagonal") any(covariance < 0) else
    min(eigen(covariance, symmetric = TRUE, only.values = TRUE)$values) <
      -1e-10 * max(1, abs(covariance))
  if (invalid) {
    # A fallback must linearize the same overall ratio. Combining variances of
    # stratum means would again omit uncertainty in the estimated size shares.
    # CDF callers need only diagonals; avoid a quadratic matrix of thresholds.
    if (type == "diagonal") {
      covariance <- vapply(seq_len(ncol(q)), function(j)
        as.numeric(stats::vcov(survey::svyratio(q[, j, drop = FALSE],
          matrix(v, ncol = 1), design))), numeric(1))
    } else {
      covariance <- stats::vcov(survey::svyratio(q, matrix(v, ncol = 1),
        design, covmat = TRUE))
    }
    notes <- c(notes, "Survey covariance of the overall ratio was used because the local covariance was unavailable or invalid.")
  }
  if (length(notes)) {
    warn_df <- rbind(warn_df, data.frame(func = I("localmean_ratio"),
      subpoptype = labels[1], subpop = labels[2], indicator = labels[3],
      stratum = NA_character_, warning = I(paste(unique(notes), collapse = "\n")),
      action = I(if (invalid) "Survey covariance was used for the entire ratio estimate." else
        "The existing two-stage variance convention was retained.")))
  }
  list(covariance = covariance, warn_df = warn_df,
    warn_ind = !is.null(warn_df) && nrow(warn_df) > 0L)
}
