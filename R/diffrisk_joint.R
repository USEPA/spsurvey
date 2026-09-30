# Joint uncertainty for the legacy risk difference. No auxiliary regression is
# fitted here. The four columns are poor-response totals and stressor margins.
diffrisk_joint <- function(design, response, stressor, response_levels,
                          stressor_levels, domain, design_names, vartype,
                          subset_local, warn_ind, warn_df, labels) {
  observed <- !is.na(response) & !is.na(stressor)
  member <- domain & observed
  poor <- response %in% response_levels[1]
  a <- member & stressor %in% stressor_levels[1]
  b <- member & stressor %in% stressor_levels[2]
  q <- cbind(t1 = a * poor, t2 = a, t3 = b * poor, t4 = b) * 1
  totals <- survey::svytotal(q, design)
  values <- as.numeric(stats::coef(totals))
  warn <- function(message, action) {
    warn_ind <<- TRUE
    warn_df <<- rbind(warn_df, data.frame(func = I("diffrisk_analysis"),
      subpoptype = labels[1], subpop = labels[2], indicator = labels[3],
      stratum = NA_character_, warning = I(message), action = I(action)))
  }
  if (any(!is.finite(values)) || any(values[c(2, 4)] == 0)) {
    warn("A stressor group has no observations or has zero total weight.",
      "Risk difference inference was set to NA.")
    return(list(proportion = c(NA_real_, NA_real_), se = NA_real_,
      warn_ind = warn_ind, warn_df = warn_df))
  }
  # Preserve the legacy percentage conversion in the reported proportions.
  proportion <- (100 * values[c(1, 3)] / values[c(2, 4)]) / 100
  covariance <- stats::vcov(totals)
  if (vartype == "Local") {
    data <- design$variables
    get <- function(name, idx) {
      column <- design_names[[name]]
      if (is.null(column)) NULL else data[[column]][idx]
    }
    cluster <- !is.null(design_names$clusterID)
    stratified <- !is.null(design_names$stratumID)
    strata <- if (stratified) data[[design_names$stratumID]] else rep("All", nrow(data))
    local <- matrix(0, 4, 4)
    for (h in unique(as.character(strata))) {
      # subset_local applies to the requested subpopulation, never separately
      # to the two stressor groups: both must use a common covariance operator.
      in_stratum <- as.character(strata) == h
      if (!any(member & in_stratum)) next
      idx <- which(in_stratum & if (subset_local) member else observed)
      ans <- relrisk_var(response[idx], stressor[idx], response_levels, stressor_levels,
        wgt = if (cluster) data$wgt2[idx] else data$wgt[idx],
        x = get("xcoord", idx), y = get("ycoord", idx),
        stratum_ind = stratified, stratum_level = h, cluster_ind = cluster,
        cluster = get("clusterID", idx), wgt1 = if (cluster) data$wgt1[idx] else NULL,
        x1 = get("xcoord1", idx), y1 = get("ycoord1", idx), vartype = "Local",
        warn_ind = warn_ind, warn_df = warn_df, warn_vec = labels,
        subset_local = subset_local, subpop_ind = as.numeric(member[idx]))
      local <- local + ans$varest
      warn_ind <- ans$warn_ind
      warn_df <- ans$warn_df
    }
    valid <- all(is.finite(local)) &&
      min(eigen(local, symmetric = TRUE, only.values = TRUE)$values) >= -1e-10 * max(1, abs(local))
    if (valid) covariance <- local else {
      warn("The joint local risk covariance was undefined or not positive semidefinite.",
        "Survey's joint covariance was used for the whole risk difference.")
    }
  }
  # Use survey's delta method for the two ratios together. Keep the original
  # design information for nonlocal paths.
  attr(totals, "var") <- covariance
  difference <- risk_contrast(totals, quote(t1 / t2 - t3 / t4))
  list(proportion = proportion, se = as.numeric(survey::SE(difference)),
    warn_ind = warn_ind, warn_df = warn_df)
}
