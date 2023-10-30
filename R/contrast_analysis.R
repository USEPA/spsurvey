contrast_analysis <- function(dframe, vars, siteID = NULL, weight = "weight", xcoord, ycoord, vartype = "Local",
                              conf = 95, statistics = c("Mean", "Total")) {



  if (is.null(siteID)) {
    siteID <- "siteID"
    dframe$siteID <- paste("site", seq_len(nrow(dframe)), sep = "-")
  }

  # create design object
  stratum_ind <- FALSE
  stratumID <- NULL
  cluster_ind <- FALSE
  clusterID <- NULL
  weight1 <- NULL
  sizeweight <- FALSE
  sweight <- NULL
  sweight1 <- NULL
  fpcfactor_ind <- FALSE
  fpcsize <- NULL
  Ncluster <- NULL
  stage1size <- NULL
  jointprob <- NULL

  design <- survey_design(
    dframe, siteID, weight, stratum_ind, stratumID, cluster_ind, clusterID,
    weight1, sizeweight, sweight, sweight1, fpcfactor_ind, fpcsize, Ncluster,
    stage1size, vartype, jointprob
  )

  all_vars <- all.vars(vars)
  # incorporate form when all_vars has only length one (function of single variable)?
  # what about three variables? Function to assign matrix to localmean_cov?
  vars_form <- paste(all_vars, collapse = " + ")

  if (vartype == "Local") {
    local_weights <- localmean_weight(x = dframe[[xcoord]], y = dframe[[ycoord]], prb = 1 / dframe[[weight]])
  }

  if ("Mean" %in% statistics) {
    rslt <- svymean(make.formula(vars_form), design)
    rslt_con <- svycontrast(rslt, vars)
    if (vartype == "Local") {
      tw <- sum(dframe[[weight]])
      local_vars <- do.call(cbind, lapply(all_vars, function(x) get_local_vars(dframe, weight, x, rslt, "Mean")))
      cov_mx <- localmean_cov(local_vars, local_weights) / tw^2
      derivs <- lapply(all_vars, D, expr = vars)
      grad <- rbind(get_grad(all_vars, derivs, rslt))
      se_val <- sqrt(grad %*% cov_mx %*% t(grad))
    }

    if (vartype == "SRS") {
      se_val <- SE(rslt_con)
    }

    mean_df <- data.frame(Estimate = rslt_con[1], StdError = se_val, LCB95 = rslt_con[1] - 1.96 * se_val, UCB95 = rslt_con[1] + 1.96 * se_val)
  }


  if ("Total" %in% statistics) {
    rslt <- svytotal(make.formula(vars_form), design)
    rslt_con <- svycontrast(rslt, vars)
    if (vartype == "Local") {
      tw <- sum(dframe[[weight]])
      local_vars <- do.call(cbind, lapply(all_vars, function(x) get_local_vars(dframe, weight, x, rslt, "Total")))
      cov_mx <- localmean_cov(local_vars, local_weights)
      derivs <- lapply(all_vars, D, expr = vars)
      grad <- rbind(get_grad(all_vars, derivs, rslt))
      se_val <- sqrt(grad %*% cov_mx %*% t(grad))
    }

    if (vartype == "SRS") {
      se_val <- SE(rslt_con)
    }

    tot_df <- data.frame(Estimate = rslt_con[1], StdError = se_val, LCB95 = rslt_con[1] - 1.96 * se_val, UCB95 = rslt_con[1] + 1.96 * se_val)

  }

  contr_out <- list()

  if ("Mean" %in% statistics) {
    contr_out$Mean <- mean_df
  } else {
    contr_out$Mean <- NULL
  }

  if ("Total" %in% statistics) {
    contr_out$Total <- tot_df
  } else {
    contr_out$Total <- NULL
  }

  contr_out

}

get_grad <- function(all_vars, derivs, svyout) {

  # hoping for no name conflict with .use suffix
  grad.use.spsurvey <- rep(0, length(all_vars))
  derivs.use.spsurvey <- derivs
  svyout.use.spsurvey <- svyout

  for (x in all_vars) {
    assign(x, svyout.use.spsurvey[[x]])
  }

  for (i in seq_along(grad.use.spsurvey)) {
    grad.use.spsurvey[i] <- eval(derivs.use.spsurvey[[i]])
  }
  grad.use.spsurvey
}

get_local_vars <- function(dframe, weight, var, rslt, statistic) {
  if (statistic == "Mean") {
    val <- (dframe[[var]] - rslt[[var]]) * dframe[[weight]]
  }

  if (statistic == "Total") {
    val <- dframe[[var]] * dframe[[weight]]
  }
  val
}
