################################################################################
# Function: cdf_localmean_total (not exported)
# Programmer: Tom Kincaid
# Date: July 9, 2020
# Revised: April 27, 2021 to check whether the local mean variance estimator
#          produced negative estimates and to use the SRS variance estimator
#          when that situation occurs
# Revised: June 8, 2021 to eliminate use of the finite population correction
#          factor with the local mean variance estimator
#
#' Local Mean Variance Estimates of the Estimated CDF using the Total Scale
#'
#' This function organizes input and output for calculation of the local mean
#' variance estimator for the estimated CDF using the total scale.
#'
#' @param itype Character value that identifies a factor variable in the design
#'   argument containing subpopulation (domain) values.
#'
#' @param lev_itype Character vector that provides levels of the subpopulation
#'   variable.
#'
#' @param nlev_itype Numeric value that provides the number of levels of the
#'   subpopulation variable.
#'
#' @param ivar Character value that identifies a factor variable in the design
#'   argument containing categorical response values.
#'
#' @param design Object of class \code{survey.design} that specifies a complex
#'   survey design.
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the design argument.
#'
#' @param cdfval Vector of the set of values at which the CDF is estimated.
#'
#' @param ncdfval Numeric value containing length of the set of values at which
#'   the CDF is estimated.
#'
#' @param cdfest_U Object that provides CDF estimates on the total scale for the
#'   continuous response variable.
#'
#' @param mult Numeric value that provides the Normal distribution confidence
#'   bound multiplier.
#'
#' @param warn_ind Logical value that indicates whether warning messages were
#'   generated.
#'
#' @param warn_df Data frame for storing warning messages.
#'
#' @return A list containing the following objects:
#'   \itemize{
#'     \item{\code{stderr_U}}{data frame containing standard error estimates}
#'     \item{\code{confval_U}}{data frame containing confidence bound estimates}
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

cdf_localmean_total <- function(itype, lev_itype, nlev_itype, ivar, design,
                                design_names, cdfval, ncdfval, cdfest_U,
                                mult, warn_ind, warn_df) {

  # Assign a value to the function name variable

  fname <- "cdf_localmean_total"

  # For variables that exist in the design$variables data frame, assign survey
  # design variables

  dframe <- design$variables
  for (i in names(design_names)) {
    if (is.null(design_names[[i]])) {
      eval(parse(text = paste0(i, " <- NULL")))
    } else {
      eval(parse(text = paste0(i, " <- dframe[, \"", design_names[[i]], "\"]")))
    }
  }

  # Assign values to the continuous response variable vector, contvar

  contvar <- dframe[, ivar]

  # Assign a value to the indicator variable for a two-stage sample

  cluster_ind <- !is.null(clusterID)

  # Assign values to weight variables

  if (cluster_ind) {
    wgt1 <- dframe$wgt1
    wgt2 <- dframe$wgt2
  } else {
    wgt <- dframe$wgt
  }

  # Create the output data frames for standard error estimates and confidence
  # bound estimates

  stderr_U <- data.frame(array(0, c(nlev_itype, ncdfval)))
  rownames(stderr_U) <- lev_itype
  confval_U <- data.frame(array(0, c(nlev_itype * ncdfval, 2)))
  dimnames(confval_U) <- list(
    paste(rep(lev_itype, ncdfval),
      rep(1:ncdfval, rep(nlev_itype, ncdfval)),
      sep = ":"
    ),
    c("LCB", "UCB")
  )

  # Loop through all subpopulations

  for (isubpop in 1:nlev_itype) {
    tst <- !is.na(dframe[, ivar]) &
      (dframe[, itype] %in% lev_itype[isubpop])

    # If post-stratification or calibration was applied to the design object, then
    # calculate the sum of weights for the subpopulation

    if ("postStrata" %in% names(design)) {
      totalwgt <- sum(weights(design)[tst])
    }

    # Assign values to the vector of CDF estimates, size

    size <- unlist(cdfest_U[isubpop, ])

    # Assign values to the warn_vec vector

    warn_vec <- c(itype, lev_itype[isubpop], ivar)

    # Assign a value to the indicator variable for a stratified sample

    stratum_ind <- !is.null(stratumID)

    # For a stratified design, determine whether the subpopulation contains a single
    # stratum

    if (stratum_ind) {
      stratum <- factor(stratumID[tst])
      stratum_levels <- levels(stratum)
      nstrata <- length(stratum_levels)
      if (nstrata == 1) {
        stratum_ind <- FALSE
      }
    }

    # Branch for a stratified sample

    if (stratum_ind) {

      # Begin the subsection for individual strata

      for (i in 1:nstrata) {

        # Calculate variance estimates

        stratum_i <- tst & stratumID == stratum_levels[i]
        if (cluster_ind) {
          temp <- cdfvar_total(
            contvar[stratum_i], wgt2[stratum_i],
            xcoord[stratum_i], ycoord[stratum_i], cdfval, stratum_ind,
            stratum_levels[i], cluster_ind, clusterID[stratum_i],
            wgt1[stratum_i], xcoord1[stratum_i], ycoord1[stratum_i], warn_ind,
            warn_df, warn_vec
          )
        } else {
          temp <- cdfvar_total(contvar[stratum_i], wgt[stratum_i],
            xcoord[stratum_i], ycoord[stratum_i], cdfval, stratum_ind,
            stratum_levels[i], cluster_ind,
            warn_ind = warn_ind,
            warn_df = warn_df, warn_vec = warn_vec
          )
        }
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
        if (any(temp$varest < 0)) {
          temp$vartype <- "SRS"
          warn_ind <- TRUE
          act <- "The simple random sampling variance estimator for an infinite population was used.\n"
          warn <- paste0("The local mean variance estimator produced one or more  negative variance estimates in \nstratum \"", stratum_levels[i], "\", the simple random sampling variance estimator for an infinite \npopulation was used to calculate variance of the CDF total estimates.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], I(stratum_levels[i]),
            warning = I(warn), action = I(act)
          ))
        }
        if (temp$vartype == "SRS") {
          rslt_svy <- lapply(cdfval, function(x) {
            svytotal(make.formula(paste0("I(", ivar, " <= ", x, ")")),
              design = subset(design, tst), na.rm = TRUE
            )
          })
          varest <- sapply(rslt_svy, function(x) SE(x)[2])^2
        } else {
          varest <- temp$varest
        }

        # Add estimates to the stderr_U data frame

        stderr_U[isubpop, ] <- stderr_U[isubpop, ] + varest

        # End the subsection for individual strata
      }

      # Begin the subsection for all strata combined

      # Add estimates to the data frames for results

      if ("postStrata" %in% names(design)) {
        tst <- size == totalwgt
        stderr_U[isubpop, tst] <- 0
      }
      stderr_U[isubpop, ] <- sqrt(stderr_U[isubpop, ])
      lbound <- unlist(pmax(size - mult * stderr_U[isubpop, ], 0))
      if ("postStrata" %in% names(design)) {
        ubound <- unlist(pmin(size + mult * stderr_U[isubpop, ], totalwgt))
      } else {
        ubound <- unlist(size + mult * stderr_U[isubpop, ])
      }
      temp <- paste(lev_itype[isubpop], 1:ncdfval, sep = ":")
      ind <- rownames(confval_U) %in% temp
      confval_U[ind, ] <- cbind(lbound, ubound)

      # End the subsection for all strata combined

      # Branch for an unstratified sample
    } else {

      # Calculate the standard error estimates

      if (cluster_ind) {
        temp <- cdfvar_total(
          contvar[tst], wgt2[tst], xcoord[tst], ycoord[tst],
          cdfval, stratum_ind, NULL, cluster_ind, clusterID[tst], wgt1[tst],
          xcoord1[tst], ycoord1[tst], warn_ind, warn_df, warn_vec
        )
      } else {
        temp <- cdfvar_total(contvar[tst], wgt[tst], xcoord[tst], ycoord[tst],
          cdfval, stratum_ind, NULL, cluster_ind,
          warn_ind = warn_ind,
          warn_df = warn_df, warn_vec = warn_vec
        )
      }
      warn_ind <- temp$warn_ind
      warn_df <- temp$warn_df
      if (any(temp$varest < 0)) {
        temp$vartype <- "SRS"
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator for an infinite population was used.\n"
        warn <- paste0("The local mean variance estimator produced one or more  negative variance estimates, the \nsimple random sampling variance estimator for an infinite population was used to \ncalculate variance of the CDF total estimates.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = NA,
          warning = I(warn), action = I(act)
        ))
      }
      if (temp$vartype == "SRS") {
        rslt_svy <- lapply(cdfval, function(x) {
          svytotal(make.formula(paste0("I(", ivar, " <= ", x, ")")),
            design = subset(design, tst), na.rm = TRUE
          )
        })
        sdest <- sapply(rslt_svy, function(x) SE(x)[2])
      } else {
        sdest <- sqrt(temp$varest)
      }

      # Add estimates to the data frames for results

      if ("postStrata" %in% names(design)) {
        tst <- size == totalwgt
        sdest[tst] <- 0
      }
      stderr_U[isubpop, ] <- sdest
      lbound <- pmax(size - mult * sdest, 0)
      if ("postStrata" %in% names(design)) {
        ubound <- pmin(size + mult * sdest, totalwgt)
      } else {
        ubound <- size + mult * sdest
      }
      temp <- paste(lev_itype[isubpop], 1:ncdfval, sep = ":")
      ind <- rownames(confval_U) %in% temp
      confval_U[ind, ] <- cbind(lbound, ubound)
    }
  }

  # Return results

  list(
    stderr_U = stderr_U,
    confval_U = confval_U,
    warn_ind = warn_ind,
    warn_df = warn_df
  )
}
