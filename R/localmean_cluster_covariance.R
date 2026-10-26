# Combine the existing between-PSU and within-PSU local estimates. Keep the
# scalar/columnwise arithmetic of the legacy variance callers: squaring a
# contrast in localmean_var() need not round like multiplying two contrasts
# in localmean_cov(). No finite population correction is applied here.
localmean_cluster_combine <- function(totals, within, weight1, neighborhood,
                                      type = c("covariance", "variance", "diagonal")) {
  type <- match.arg(type)
  if (type == "variance") {
    return(localmean_var(totals * weight1, neighborhood) + sum(within * weight1))
  }
  m <- ncol(totals)
  weighted <- totals * matrix(rep(weight1, m), nrow = nrow(totals))
  if (type == "diagonal") {
    return(apply(weighted, 2, localmean_var, neighborhood) +
      apply(within * matrix(rep(weight1, m), nrow = nrow(totals)), 2, sum))
  }
  localmean_cov(weighted, neighborhood) + matrix(apply(
    within * matrix(rep(weight1, m^2), nrow = nrow(totals)), 2, sum), nrow = m)
}

# q contains SECOND-stage-weighted contributions. Keeping that scale explicit
# avoids multiplying by a first-stage weight and then dividing it out again.
# Complete GREG contributions are converted to this scale.
# This deliberately retains the existing two-stage combination, including
# zero within-PSU variance for singletons and SRS substitution for 2-3 sites.
localmean_cluster_covariance <- function(q, cluster, weight1, weight2,
                                         x, y, x1, y1, local_nbh = 15,
                                         local_context = NULL,
                                         type = c("covariance", "diagonal")) {
  type <- match.arg(type)
  q <- as.matrix(q)
  groups <- split(seq_len(nrow(q)), factor(cluster))
  m <- ncol(q)
  totals <- matrix(0, length(groups), m)
  within <- matrix(0, length(groups), if (type == "diagonal") m else m^2)
  w1 <- vapply(groups, function(i) weight1[i[1]], numeric(1))
  first <- vapply(groups, `[`, integer(1), 1L)
  notes <- character()
  for (j in seq_along(groups)) {
    i <- groups[[j]]
    values <- q[i, , drop = FALSE]
    totals[j, ] <- apply(values, 2, sum)
    n <- length(i)
    if (n == 1L) next
    nb <- if (n >= 4L) localmean_weight_internal(x[i], y[i], 1 / weight2[i],
      local_nbh, localmean_group_context(local_context, paste0("stage2:", names(groups)[j]))) else NULL
    if (is.null(nb)) {
      within[j, ] <- if (type == "diagonal") n * apply(values, 2, var) else n * var(values)
      notes <- c(notes, paste0("PSU ", names(groups)[j], ": SRS within-PSU covariance was used."))
    } else {
      within[j, ] <- if (type == "diagonal") apply(values, 2, localmean_var, nb) else localmean_cov(values, nb)
    }
  }
  nb <- if (length(groups) >= 4L) localmean_weight_internal(x1[first], y1[first],
    1 / w1, local_nbh, localmean_group_context(local_context, "stage1")) else NULL
  covariance <- if (is.null(nb)) NULL else
    localmean_cluster_combine(totals, within, w1, nb, type)
  weighted <- totals * matrix(rep(w1, m), nrow = nrow(totals))
  srs <- if (type == "diagonal") {
    length(groups) * apply(weighted, 2, var) +
      apply(within * matrix(rep(w1, m), nrow = nrow(totals)), 2, sum)
  } else {
    length(groups) * var(weighted) + matrix(apply(
      within * matrix(rep(w1, m^2), nrow = nrow(totals)), 2, sum), nrow = m)
  }
  list(covariance = covariance, srs_covariance = srs, notes = notes)
}

# Apply the cluster operator to complete contributions on a survey design.
# The original clustered design supplies the fallback and stage structure.
localmean_cluster_design <- function(design, z, design_names, local_nbh = 15,
                                     local_context = NULL, type = "covariance",
                                     rows = seq_len(nrow(design$variables))) {
  data <- design$variables
  z <- as.matrix(z)
  answer <- if (type == "diagonal") rep(0, ncol(z)) else matrix(0, ncol(z), ncol(z))
  notes <- character()
  for (i in split(rows, design$strata[[1]][rows], drop = TRUE)) {
    key <- if (is.null(design$call$strata)) "unstratified" else
      paste0("stratum:", design$strata[[1]][i[1]])
    part <- localmean_cluster_covariance(z[i, , drop = FALSE] / data$wgt1[i],
      data[[design_names$clusterID]][i], data$wgt1[i], data$wgt2[i],
      data[[design_names$xcoord]][i], data[[design_names$ycoord]][i],
      data[[design_names$xcoord1]][i], data[[design_names$ycoord1]][i],
      local_nbh, localmean_group_context(local_context, key), type)
    notes <- c(notes, part$notes)
    if (is.null(part$covariance)) return(list(covariance = NULL,
      notes = c(notes, "First-stage local covariance could not be calculated.")))
    answer <- answer + part$covariance
  }
  if (any(!is.finite(answer)) || (type == "diagonal" && any(answer < 0)) ||
      (type == "covariance" && min(eigen(answer, symmetric = TRUE, only.values = TRUE)$values) <
        -1e-10 * max(1, abs(answer)))) {
    return(list(covariance = NULL, notes = c(notes, "The local covariance is not finite and positive semidefinite.")))
  }
  list(covariance = answer, notes = notes)
}
