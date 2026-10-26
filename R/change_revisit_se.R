# Standard errors of repeat-site estimates in one stratum. Use the existing
# marginal estimators, with each occasion's own weights and geometry, to put
# that stratum's repeat-site correlation on the covariance scale.
change_revisit_se <- function(design, design_names, stratum_level, statistic,
                              itype, isubpop, ivar, catvar_levels = NULL,
                              var_nondetect = NULL, vartype, conf, mult,
                              warn_ind, warn_df, local_nbh, local_context) {
  keep <- design$variables[[design_names$stratumID]] == stratum_level
  stratum_design <- design[keep, ]
  share <- sum(weights(stratum_design)) / sum(weights(design))
  # Poststratified survey subsets retain excluded rows with zero weights.
  # Mark their responses missing so local neighborhoods contain only the
  # selected repeat sites, while retaining survey's design bookkeeping.
  stratum_design$variables[weights(stratum_design) == 0, ivar] <- NA
  args <- list(NULL, stratum_design$variables, itype, isubpop, 1, ivar)
  if (statistic == "category") {
    args <- c(args, list(catvar_levels, length(catvar_levels)))
  }
  args <- c(args, list(design = stratum_design, design_names = design_names))
  if (statistic != "category") args["var_nondetect"] <- list(var_nondetect)
  args <- c(args, list(vartype = vartype, conf = conf, mult = mult,
    warn_ind = warn_ind, warn_df = warn_df, local_nbh = local_nbh,
    local_context = local_context))
  estimator <- switch(statistic, mean = mean_est, total = total_est,
    category = category_est)
  result <- do.call(estimator, args)
  if (statistic == "category") {
    estimates <- result$catsum[match(catvar_levels, result$catsum$Category), ]
    se <- list(P = estimates$StdError.P / 100, U = estimates$StdError.U)
  } else {
    estimates <- result[[paste0(statistic, "sum")]]
    se <- estimates$StdError
  }
  list(se = se, share = share, warn_ind = result$warn_ind,
    warn_df = result$warn_df)
}
