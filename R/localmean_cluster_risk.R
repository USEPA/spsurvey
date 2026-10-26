# Joint cell totals on the full two-stage design. The relative-risk columns
# include stressor margins; attributable risk instead needs all four cells.
localmean_cluster_risk <- function(response, stressor, response_levels,
                                   stressor_levels, member, wgt, cluster, wgt1,
                                   x, y, x1, y1, warn_df, warn_vec,
                                   local_nbh, local_context, margins) {
  poor <- response == response_levels[1]
  first <- stressor == stressor_levels[1]
  second <- stressor == stressor_levels[2]
  q <- if (margins) cbind(poor * first, first, poor * second, second) else
    cbind(poor * first, (response == response_levels[2]) * first,
      poor * second, (response == response_levels[2]) * second)
  q <- q * member * wgt
  result <- localmean_cluster_covariance(q, cluster, wgt1, wgt,
    x, y, x1, y1, local_nbh, local_context)
  covariance <- result$covariance
  if (is.null(covariance) || any(!is.finite(covariance)) ||
      min(eigen(covariance, symmetric = TRUE, only.values = TRUE)$values) <
        -1e-10 * max(1, abs(covariance))) {
    covariance <- result$srs_covariance
    result$notes <- c(result$notes, "The SRS first-stage covariance was used.")
  }
  if (length(result$notes)) warn_df <- localmean_cluster_warning(warn_df, result$notes, warn_vec)
  list(varest = covariance, warn_df = warn_df, warn_ind = !is.null(warn_df))
}
