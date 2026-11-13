################################################################################
# Function: cdftest_localmean_prop (not exported)
# Programmer: Tom Kincaid
# Date: October 22, 2020
# Revised: November 2, 2020 to correctly process the column variable when it
#          includes missing (NA) values
# Revised: May 27 2021 to eliminate use of the finite population correction
#          factor with the local mean variance estimator
#
#' Local Mean Variance/Covarince Estimates of Estimated Population Proportions
#'
#' This function organizes input and output for calculation of the local mean
#' variance/covariance estimator for estimated proportions for categorical data.
#'
#' @param design Object of class \code{survey.design} that specifies a complex
#'   survey design.
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the design argument.
#'
#' @param warn_ind Logical value that indicates whether warning messages were
#'   generated.
#'
#' @param warn_df Data frame for storing warning messages.
#'
#' @param warn_vec Character vector that contains a subpopulation name, the
#'   first subpopulation level, the second subpopulation level, and an indicator
#'   name.
#'
#' @param subset_local If \code{TRUE}, construct neighborhoods using only
#'   domain members. The internal default, \code{FALSE}, retains eligible
#'   responding sites in the sampling stratum and zeros contributions outside
#'   the domain. Used only for local variance estimation.
#'
#' @return A list containing the following objects:
#'   \itemize{
#'     \item{\code{varest}}{matrix containing the variance/covariance estimates
#'       for the contingency table proportion estimates}
#'     \item{\code{warn_ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn_df}}{data frame for storing warning messages}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@@epa.gov}
#'
#' @keywords survey
#'
#' @noRd
################################################################################

cdftest_localmean_prop <- function(design, design_names, warn_ind, warn_df,
                                   warn_vec, subset_local = FALSE, local_nbh = 10, local_context = NULL) {
  # Stratified ratios use the overall estimate and denominator. Summing
  # local variances of these contributions retains uncertainty in the
  # estimated stratum sizes, including for All Sites.

  # Assign a value to the function name variable

  fname <- "cdftest_localmean_prop"

  if (!is.null(design_names$clusterID) && !subset_local) {
    return(localmean_cluster_cdftest(design, design_names, FALSE,
      warn_df, warn_vec, local_nbh, local_context))
  }

  # For variables that exist in the design$variables data frame, assign survey
  # design variables

  # colvar is NA only for a genuinely missing response (item nonresponse),
  # which is always dropped. rowvar is NA for sites outside the two groups
  # being compared; under subset_local = TRUE those are dropped too
  # (original behavior), but under subset_local = FALSE they are kept here
  # so their location can inform the local mean neighbor structure below,
  # and are zeroed out (not dropped) downstream via subpop_ind.
  dframe <- if (subset_local) {
    subset(design$variables, !(is.na(rowvar) | is.na(colvar)))
  } else {
    subset(design$variables, !is.na(colvar))
  }
  for (i in names(design_names)) {
    if (is.null(design_names[[i]])) {
      eval(parse(text = paste0(i, " <- NULL")))
    } else {
      eval(parse(text = paste0(i, " <- dframe[, \"", design_names[[i]], "\"]")))
    }
  }

  # Indicator (1/0) for domain (subpopulation) membership among the rows
  # retained above; only used when subset_local = FALSE, to zero out
  # out-of-domain rows' contribution to the contingency table while still
  # using their location in the neighbor structure

  subpop_ind_full <- as.numeric(!is.na(dframe$rowvar))

  # Assign a value to the indicator variable for a two-stage sample

  cluster_ind <- !is.null(clusterID)

  # Assign values to weight variables

  if (cluster_ind) {
    wgt1 <- dframe$wgt1
    wgt2 <- dframe$wgt2
  } else {
    wgt <- dframe$wgt
  }

  # Assign a value to the indicator variable for a stratified sample

  stratum_ind <- !is.null(stratumID)

  # For a stratified design, determine whether the subpopulation contains a
  # single stratum

  if (stratum_ind) {
    stratum <- factor(stratumID)
    stratum_levels <- levels(stratum)
    nstrata <- length(stratum_levels)
    if (nstrata == 1) {
      stratum_ind <- FALSE
    }
  }

  # Branch for a stratified sample

  if (stratum_ind) {
    form <- ~ interaction(factor(rowvar), factor(colvar)) - 1
    response <- model.matrix(form,
      model.frame(form, design$variables, na.action = na.pass))
    member <- !is.na(design$variables$rowvar)
    responded <- !is.na(design$variables$colvar)
    response[!(member & responded), ] <- 0
    d <- as.numeric(weights(design))
    estimate <- colSums(response * d) / sum(d * (member & responded))
    labels <- c(warn_vec[1], paste(warn_vec[2:3], collapse = " / "), warn_vec[4])
    full <- localmean_ratio(design, response, member, responded, estimate,
      design_names, warn_df, labels, subset_local, local_nbh, local_context,
      type = "covariance")
    colnames(full$covariance) <- colnames(response)
    results <- list(varest = full$covariance,
      warn_ind = warn_ind || full$warn_ind, warn_df = full$warn_df)

    # Branch for an unstratified sample
  } else {
    # Calculate the variance/covariance estimates

    if (cluster_ind) {
      results <- cdftestvar_prop(
        design, wgt2, xcoord, ycoord, stratum_ind, NULL, cluster_ind, clusterID,
        wgt1, xcoord1, ycoord1, warn_ind, warn_df, warn_vec,
        subset_local = subset_local, subpop_ind = subpop_ind_full,
        local_nbh = local_nbh, local_context = local_context)
    } else {
      results <- cdftestvar_prop(
        design, wgt, xcoord, ycoord, stratum_ind, NULL, cluster_ind,
        warn_ind = warn_ind, warn_df = warn_df, warn_vec = warn_vec,
        subset_local = subset_local, subpop_ind = subpop_ind_full,
        local_nbh = local_nbh, local_context = local_context)
    }
  }

  # Return results

  results
}
