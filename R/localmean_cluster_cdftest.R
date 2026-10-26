# Joint cell and margin covariance for a two-stage CDF comparison. Sites
# outside the compared groups remain in both stages with zero contributions.
localmean_cluster_cdftest <- function(design, design_names, totals, warn_df,
                                      warn_vec, local_nbh, local_context) {
  make_matrix <- function(formula) model.matrix(formula,
    model.frame(formula, design$variables, na.action = na.pass))
  response <- make_matrix(~ interaction(factor(rowvar), factor(colvar)) - 1)
  if (totals) response <- cbind(response,
    make_matrix(~ factor(rowvar) - 1), make_matrix(~ factor(colvar) - 1),
    mm_total = 1)
  labels <- c(warn_vec[1], paste(warn_vec[2:3], collapse = " / "), warn_vec[4])
  result <- localmean_cluster_domain(design, response,
    !is.na(design$variables$rowvar), !is.na(design$variables$colvar),
    ratio = !totals, design_names, warn_df, labels,
    local_nbh, local_context, type = "covariance")
  colnames(result$covariance) <- colnames(response)
  list(varest = result$covariance, warn_ind = result$warn_ind,
    warn_df = result$warn_df)
}
