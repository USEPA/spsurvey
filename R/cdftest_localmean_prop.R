###############################################################################
# Function: cdftest_localmean_prop (not exported)
# Programmer: Tom Kincaid
# Date: October 22, 2020
# Revised: November 2, 2020 to correctly process the column variable when it
#          includes missing (NA) values
#'
#' Local Mean Variance/Covarince Estimates of Estimated Population Proportions
#'
#' This function organizes input and output for calculation of the local mean
#' variance/covariance estimator for estimated proportions for categorical data.
#'
#' @param design Object of class \code{survey.design} that specifies a complex survey
#'   design.
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the design argument.
#'
#' @param popcorrect Logical value that indicates whether the finite population
#'   correction factor should be employed during variance estimation.
#'
#' @param vartype The choice of variance estimator, where \code{"Local"} = local mean
#'   estimator and \code{"SRS"} = SRS estimator.
#'
#' @param warn_ind Logical value that indicates whether warning messages were
#'   generated.
#'
#' @param warn_df Data frame for storing warning messages.
#'
#' @param warn_vec Character vector that contains a subpopulation name, the
#'   first subpopulation level, the second subpopulation level, and an
#'   indicator name.
#'
#' @return A matrix containing the variance/covariance estimates for the
#'   contingency table proportion estimates.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{cdftestvar_prop}}}{calculates variance-covariance
#'       estimates of the estimated proportion in each cell of a contingency
#'       table}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @seealso
#'   \code{\link{cdftest_localmean_total}}
#'
#' @keywords survey
#'
#' @noRd
###############################################################################

cdftest_localmean_prop <- function(design, design_names, popcorrect, vartype,
                                   warn_ind, warn_df, warn_vec) {

  # Assign a value to the function name variable

  fname <- "cdftest_localmean_prop"

  # For variables that exist in the design$variables data frame, assign survey
  # design variables

  dframe <- subset(design$variables, !(is.na(rowvar) | is.na(colvar)))
  for (i in names(design_names)) {
    if (is.null(design_names[[i]])) {
      eval(parse(text = paste0(i, " <- NULL")))
    } else {
      eval(parse(text = paste0(i, " <- dframe[, \"", design_names[[i]], "\"]")))
    }
  }

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

  # For a stratified design, determine whether the subpopulation contains a single
  # stratum

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

    # Calculate values required for weighting strata

    if (cluster_ind) {
      popsize_hat <- tapply(wgt1 * wgt2, stratum, sum)
      sum_popsize_hat <- sum(wgt1 * wgt2)
    } else {
      popsize_hat <- tapply(wgt, stratum, sum)
      sum_popsize_hat <- sum(wgt)
    }

    # Create the varest matrix

    m <- with(dframe, length(levels(rowvar)) * length(levels(colvar)))
    varest <- matrix(0, m, m)
    temp <- paste(
      "interaction(factor(rowvar), factor(colvar))Subpopulation",
      1:2
    )
    colnames_varest <- paste(rep(temp, length(levels(dframe$colvar))),
      rep(levels(dframe$colvar), each = 2),
      sep = "."
    )

    # Calculate variance estimates

    for (i in 1:nstrata) {
      temp <- design_names$stratumID
      tst <- design$variables[, temp] != stratum_levels[i]
      design_temp <- design
      design_temp$variables$rowvar[tst] <- NA
      stratum_i <- stratumID == stratum_levels[i]
      if (cluster_ind) {
        temp <- cdftestvar_prop(
          design_temp, wgt2[stratum_i], xcoord[stratum_i],
          ycoord[stratum_i], stratum_ind, stratum_levels[i], cluster_ind,
          clusterID[stratum_i], wgt1[stratum_i], xcoord1[stratum_i],
          ycoord1[stratum_i], popcorrect, NULL, Ncluster[stratum_i],
          stage1size[stratum_i], vartype, warn_ind, warn_df, warn_vec
        )
      } else {
        temp <- cdftestvar_prop(design_temp, wgt[stratum_i],
          xcoord[stratum_i], ycoord[stratum_i], stratum_ind,
          stratum_levels[i], cluster_ind,
          pcfactor_ind = popcorrect,
          fpcsize = fpcsize[stratum_i], vartype = vartype,
          warn_ind = warn_ind, warn_df = warn_df, warn_vec = warn_vec
        )
      }
      varest_st <- temp$varest
      warn_ind <- temp$warn_ind
      warn_df <- temp$warn_df

      # Add estimates to the varest matrix

      tst <- colnames_varest %in% colnames(varest_st)
      varest[tst, tst] <- varest[tst, tst] +
        ((popsize_hat[i] / sum_popsize_hat)^2) * varest_st
      colnames(varest) <- colnames_varest

      # End the loop for strata
    }

    # Create the results list

    results <- list(
      varest = varest, vartype = vartype, warn_ind = warn_ind,
      warn_df = warn_df
    )

    # Branch for an unstratified sample
  } else {

    # Calculate the variance/covariance estimates

    if (cluster_ind) {
      results <- cdftestvar_prop(
        design, wgt2, xcoord, ycoord, stratum_ind,
        NULL, cluster_ind, clusterID, wgt1, xcoord1, ycoord1, popcorrect, NULL,
        Ncluster, stage1size, vartype, warn_ind, warn_df, warn_vec
      )
    } else {
      results <- cdftestvar_prop(design, wgt, xcoord, ycoord, stratum_ind,
        NULL, cluster_ind,
        pcfactor_ind = popcorrect, fpcsize = fpcsize,
        vartype = vartype, warn_ind = warn_ind, warn_df = warn_df,
        warn_vec = warn_vec
      )
    }
  }

  # Return results

  results
}
