################################################################################
# Function: cdftest_localmean_total (not exported)
# Programmer: Tom Kincaid
# Date: October 23, 2020
# Revised: November 2, 2020 to correctly process the column variable when it
#          includes missing (NA) values
# Revised: June 3 2021 to eliminate use of the finite population correction
#          factor with the local mean variance estimator
#
#' Local Mean Variance/Covarince Estimates of Estimated Population Totals
#'
#' This function organizes input and output for calculation of the local mean
#' variance/covariance estimator for estimated totals for categorical data.
#'
#' @param design Object of class \code{survey.design} that specifies a complex
#'   survey design.
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the \code{design} argument.
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
#' @param subset_local Logical value indicating whether the local mean
#'   neighbor structure is built from the union of the two compared groups
#'   only (\code{TRUE}, the default behavior) or from the full
#'   stratum, with out-of-domain sites zeroed out rather than dropped
#'   (\code{FALSE}). The default value is \code{TRUE}.
#'
#' @return A list containing the following objects:
#'   \itemize{
#'     \item{\code{varest}}{matrix containing the variance/covariance estimates
#'       for the contingency table total estimates}
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

cdftest_localmean_total <- function(design, design_names, warn_ind, warn_df,
                                    warn_vec, subset_local = TRUE) {
  # Parallels cdftest_localmean_prop() (see that file for what rowvar/colvar
  # represent and the per-stratum masking trick), but computes the
  # variance-covariance matrix of the group-by-bin contingency table's cell
  # totals via cdftestvar_total() rather than cell proportions. Per-stratum
  # covariance matrices are combined using the same population-size-share-
  # weighted formula as the proportion version.

  # Assign a value to the function name variable

  fname <- "cdftest_localmean_total"

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
    # Calculate values required for weighting strata

    if (cluster_ind) {
      popsize_hat <- tapply(wgt1 * wgt2, stratum, sum)
      sum_popsize_hat <- sum(wgt1 * wgt2)
    } else {
      popsize_hat <- tapply(wgt, stratum, sum)
      sum_popsize_hat <- sum(wgt)
    }

    # Create the varest matrix

    lev_rowvar <- levels(design$variables$rowvar)
    nr <- length(lev_rowvar)
    lev_colvar <- levels(design$variables$colvar)
    nc <- length(lev_colvar)
    m <- nr * nc + nr + nc + 1
    varest <- matrix(0, m, m)
    temp <- paste0(
      "interaction(factor(rowvar), factor(colvar))",
      lev_rowvar
    )
    colnames_varest <- c(
      paste(rep(temp, nc), rep(lev_colvar, each = nr), sep = "."),
      paste0("factor(rowvar)", lev_rowvar),
      paste0("factor(colvar)", lev_colvar),
      "mm_total"
    )

    # Calculate variance estimates

    for (i in 1:nstrata) {
      temp <- design_names$stratumID
      tst <- design$variables[, temp] != stratum_levels[i]
      design_temp <- design
      if (subset_local) {
        design_temp$variables$rowvar[tst] <- NA
      } else {
        # Blank colvar (instead of rowvar) to signal "outside this
        # stratum" so rowvar's NA-ness continues to mean only "outside
        # the domain"; cdftestvar_total() relies on that distinction
        # to zero out (rather than drop) out-of-domain sites while
        # keeping them in the spatial neighbor structure. The union
        # is.na(rowvar) | is.na(colvar) used to drop rows is unaffected
        # by which of the two carries the "outside this stratum" flag,
        # so this leaves the subset_local = TRUE path unchanged.
        design_temp$variables$colvar[tst] <- NA
      }
      stratum_i <- stratumID == stratum_levels[i]
      if (cluster_ind) {
        temp <- cdftestvar_total(
          design_temp, wgt2[stratum_i],
          xcoord[stratum_i], ycoord[stratum_i], stratum_ind, stratum_levels[i],
          cluster_ind, clusterID[stratum_i], wgt1[stratum_i],
          xcoord1[stratum_i], ycoord1[stratum_i], warn_ind, warn_df, warn_vec,
          subset_local = subset_local, subpop_ind = subpop_ind_full[stratum_i]
        )
      } else {
        temp <- cdftestvar_total(design_temp, wgt[stratum_i], xcoord[stratum_i],
          ycoord[stratum_i], stratum_ind, stratum_levels[i], cluster_ind,
          warn_ind = warn_ind, warn_df = warn_df, warn_vec = warn_vec,
          subset_local = subset_local, subpop_ind = subpop_ind_full[stratum_i]
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
      varest = varest,
      warn_ind = warn_ind,
      warn_df = warn_df
    )

    # Branch for an unstratified sample
  } else {
    # Calculate the variance/covariance estimates

    if (cluster_ind) {
      results <- cdftestvar_total(
        design, wgt2, xcoord, ycoord, stratum_ind, NULL, cluster_ind, clusterID,
        wgt1, xcoord1, ycoord1, warn_ind, warn_df, warn_vec,
        subset_local = subset_local, subpop_ind = subpop_ind_full
      )
    } else {
      results <- cdftestvar_total(design, wgt, xcoord, ycoord, stratum_ind,
        NULL, cluster_ind,
        warn_ind = warn_ind, warn_df = warn_df,
        warn_vec = warn_vec,
        subset_local = subset_local, subpop_ind = subpop_ind_full
      )
    }
  }

  # Return results

  results
}
