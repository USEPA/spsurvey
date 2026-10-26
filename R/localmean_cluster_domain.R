# Full-design domain inference for direct two-stage estimates. Membership is
# zeroed before linearization; genuine item nonresponse is handled separately.
localmean_cluster_domain <- function(design, response, member, responded, ratio,
                                     design_names, warn_df, labels,
                                     local_nbh = 15, local_context = NULL,
                                     estimate = NULL, type = "diagonal") {
  response <- as.matrix(response)
  q <- response
  v <- as.numeric(member & responded)
  q[v == 0, ] <- 0
  # Poststratified survey designs retain zero-weight rows on subsetting.
  # Give survey full-length matrices, and select responding rows explicitly
  # for the local neighborhoods rather than assuming subsetting drops them.
  survey_stat <- function(response) {
    if (ratio) survey::svyratio(response, matrix(v, ncol = 1), design) else
      survey::svytotal(response, design)
  }
  stat <- NULL
  if (is.null(estimate)) {
    stat <- survey_stat(q)
    estimate <- as.numeric(stats::coef(stat))
  }
  d <- design$variables$wgt1 * design$variables$wgt2
  u <- if (ratio) q - outer(v, estimate) else q
  z <- u * d
  if (ratio) z <- z / sum(d * v)
  missing_psu <- length(unique(design$cluster[[1]][responded])) != length(unique(design$cluster[[1]]))
  local <- if (missing_psu) list(covariance = NULL,
    notes = "A sampled PSU has no usable responses; its total cannot be treated as zero.") else
    localmean_cluster_design(design, z, design_names, local_nbh, local_context,
      type, rows = which(responded))
  covariance <- local$covariance
  if (is.null(covariance)) {
    # CDF wrappers supply their point estimates and need only marginal
    # variances. Avoid a quadratic threshold-by-threshold survey covariance
    # matrix, both on successful local paths and when a fallback is needed.
    if (!is.null(stat)) {
      covariance <- stats::vcov(stat)
      if (type == "diagonal") covariance <- diag(covariance)
    } else if (type == "diagonal") {
      covariance <- vapply(seq_len(ncol(q)), function(j)
        as.numeric(stats::vcov(survey_stat(q[, j, drop = FALSE]))), numeric(1))
    } else covariance <- stats::vcov(survey_stat(q))
    local$notes <- c(local$notes, "Survey's design covariance was used for the entire estimate.")
  }
  if (length(local$notes)) warn_df <- localmean_cluster_warning(warn_df, local$notes, labels)
  list(covariance = covariance, estimate = estimate, warn_df = warn_df,
    warn_ind = !is.null(warn_df))
}

localmean_cluster_warning <- function(warn_df, notes, labels = rep(NA_character_, 3)) {
  rbind(warn_df, data.frame(func = I("localmean_cluster_covariance"),
    subpoptype = labels[1], subpop = labels[2], indicator = labels[3],
    stratum = NA_character_, warning = I(paste(unique(notes), collapse = "\n")),
    action = I("The existing two-stage fallback convention was retained.")))
}
