# Warn before cdf dense allocation
localmean_cdf_warning <- function(n, m, warn_df, local_context = NULL,
                                  labels = c(NA, NA, NA), stratum = NA,
                                  greg = FALSE) {
  cells <- as.double(n) * as.double(m)
  if (cells <= 1e8) return(warn_df)
  key <- paste("CDF", greg, labels[3], n, m, sep = "/")
  message <- if (greg) {
    sprintf("Local GREG CDF estimation requires %s site-by-threshold evaluations and may take exceedingly long.", format(cells, scientific = FALSE))
  } else {
    "Local CDF variance requires large matrices and may use substantial memory."
  }
  if (!is.null(local_context) && key %in% local_context$warning_keys) return(warn_df)
  if (!is.null(warn_df) && any(warn_df$warning == message)) return(warn_df)
  warning(message, call. = FALSE)
  if (!is.null(local_context)) local_context$warning_keys <- c(local_context$warning_keys, key)
  rbind(warn_df, data.frame(func = I("localmean_cdf_warning"), subpoptype = labels[1],
    subpop = labels[2], indicator = labels[3], stratum = stratum,
    warning = I(message), action = I("Proceeding with the requested thresholds.")))
}

localmean_cdf_check_size <- function(design, design_names, variable, type, domains,
                                    m, subset_local, warn_df, local_context) {
  observed <- !is.na(design$variables[[variable]])
  strata <- if (is.null(design_names$stratumID)) rep("All", length(observed)) else
    as.character(design$strata[[1]])
  blocks <- split(seq_along(observed), strata)
  for (domain in domains) for (idx in blocks) {
    member <- design$variables[[type]][idx] %in% domain
    if (!any(observed[idx] & member)) next
    rows <- idx[observed[idx] & (if (subset_local) member else TRUE)]
    counts <- length(rows)
    if (!is.null(design_names$clusterID)) {
      sizes <- table(design$variables[[design_names$clusterID]][rows])
      counts <- c(as.numeric(sizes), sum(sizes > 0))
    }
    for (n in unique(counts)) warn_df <- localmean_cdf_warning(n, m, warn_df,
      local_context, c(type, domain, variable),
      if (is.null(design_names$stratumID)) NA else strata[idx[1]])
  }
  warn_df
}
