################################################################################
# Function: total_localmean (not exported)
# Programmer: Tom Kincaid
# Date: October 25, 2021
#
#' Local total Variance Estimates of the Estimated Total
#'
#' This function organizes input and output for calculation of the local mean
#' variance estimator for the estimated total.
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
#' @param levs Numeric vector that is used to select subpopulation levels
#'
#' @param ivar Character value that identifies a factor variable in the design
#'   argument containing categorical response values.
#'
#' @param design Object of class \code{survey.design} that specifies a complex
#'   survey design.
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the \code{design} argument.
#'
#' @param totalest Vector that provides estimates of the total.
#'
#' @param mult Numeric value that provides the Normal distribution confidence
#'   bound multiplier.
#'
#' @param warn_ind Logical value that indicates whether warning messages were
#'   generated.
#'
#' @param warn_df Data frame for storing warning messages.
#'
#' @param subset_local Logical value indicating whether the local mean
#'   neighbor structure is built from domain members only (\code{TRUE},
#'   the default behavior) or from all sites in the relevant stratum
#'   (\code{FALSE}). The default value is
#'   \code{TRUE}.
#'
#' @return A list containing the following objects:
#'   \describe{
#'     \item{\code{stderr}}{vector containing standard error estimates}
#'     \item{\code{confval}}{data frame containing confidence bound estimates}
#'     \item{\code{warn_ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn_df}}{data frame for storing warning messages}
#'   }
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{total_var}}{calculates variance estimate of the
#'       estimated total}
#'     \item{\code{\link{svytotal}}}{calculates the total for a complex survey
#'       design}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @seealso
#'   \code{\link{svytotal}}
#'
#' @keywords survey
#'
#' @noRd
################################################################################

total_localmean <- function(itype, lev_itype, nlev_itype, levs, ivar, design,
                            design_names, totalest, mult, warn_ind, warn_df,
                            subset_local = TRUE) {
  # Parallels mean_localmean(), but calls total_var() (rather than
  # mean_var()) per stratum/subpopulation to combine into an overall
  # variance estimate for a total rather than a mean. Per-stratum variances
  # are still combined using the same population-size-share-weighted
  # formula as mean_localmean() (see that file for details).

  # Assign a value to the function name variable

  fname <- "total_localmean"

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

  # Create the output objects for standard error estimates and confidence bound
  # estimates

  nlevs <- length(levs)
  stderr <- numeric(nlev_itype)
  names(stderr) <- lev_itype
  confval <- data.frame(array(0, c(nlev_itype, 2)))
  dimnames(confval) <- list(lev_itype, c("LCB", "UCB"))

  # Loop through all subpopulations

  for (isubpop in levs) {
    tst <- !is.na(dframe[, ivar]) &
      (dframe[, itype] %in% lev_itype[isubpop])

    # tst_resp/subpop_ind_full support subset_local = FALSE: tst_resp is
    # every site with a usable (non-missing) response, regardless of
    # subpopulation, and subpop_ind_full flags which of those sites are
    # actually in this subpopulation (domain). tst itself is left
    # untouched, since it still determines which strata this
    # subpopulation touches.

    tst_resp <- !is.na(dframe[, ivar])
    subpop_ind_full <- as.numeric(dframe[, itype] %in% lev_itype[isubpop])

    # Assign values to the warn_vec vector

    warn_vec <- c(itype, lev_itype[isubpop], ivar)

    # Assign a value to the indicator variable for a stratified sample

    stratum_ind <- !is.null(stratumID)

    # For a stratified design, determine whether the subpopulation contains a
    # single stratum

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
        # Calculate total estimates for the stratum

        stratum_i <- tst & stratumID == stratum_levels[i]
        totalest_st <- svytotal(make.formula(ivar),
          design = subset(design, stratum_i),
          na.rm = TRUE
        )

        # Calculate variance estimates

        if (cluster_ind) {
          # subset_local = FALSE is not yet supported for two-stage
          # designs (blocked upstream in cont_analysis()), so this branch
          # is intentionally left passing domain-only data
          temp <- total_var(
            contvar[stratum_i], wgt2[stratum_i], xcoord[stratum_i],
            ycoord[stratum_i], stratum_ind, stratum_levels[i], cluster_ind,
            clusterID[stratum_i], wgt1[stratum_i], xcoord1[stratum_i],
            ycoord1[stratum_i], warn_ind, warn_df, warn_vec
          )
        } else {
          # stratum_data_i is the row mask actually fed to total_var():
          # under subset_local = TRUE it's identical to stratum_i (domain
          # members only); under FALSE it widens to
          # every responded site in this stratum, and subpop_ind_full
          # tells total_var() which of those rows are real domain members
          # so it can zero out the rest.
          stratum_data_i <- if (subset_local) {
            stratum_i
          } else {
            tst_resp & stratumID == stratum_levels[i]
          }
          temp <- total_var(
            contvar[stratum_data_i], wgt[stratum_data_i],
            xcoord[stratum_data_i],
            ycoord[stratum_data_i], stratum_ind, stratum_levels[i], cluster_ind,
            warn_ind = warn_ind, warn_df = warn_df, warn_vec = warn_vec,
            subset_local = subset_local,
            subpop_ind = subpop_ind_full[stratum_data_i]
          )
        }
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
        if (temp$varest < 0) {
          temp$vartype <- "SRS"
          warn_ind <- TRUE
          act <- "The simple random sampling variance estimator for an infinite population was used.\n"
          warn <- paste0("The local total variance estimator produced a  negative variance estimate in stratum \n\"", stratum_levels[i], "\", the simple random sampling variance estimator for an infinite \npopulation was used to calculate variance of the total estimate.\n")
          warn_df <-
            rbind(
              warn_df,
              data.frame(
                func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
                indicator = warn_vec[3], stratum = I(stratum_levels[i]),
                warning = I(warn), action = I(act)
              )
            )
        }
        if (temp$vartype == "SRS") {
          # the fallback estimate must cover only this stratum (stratum_i),
          # not the whole subpopulation (tst); this loop contributes one
          # stratum's variance at a time
          rslt_svy <- svytotal(make.formula(ivar),
            design = subset(design, stratum_i),
            na.rm = TRUE
          )
          varest <- SE(rslt_svy)^2
        } else {
          varest <- temp$varest
        }

        # Add estimate to the stderr vector
        # The estimated total for the subpopulation is the sum of the
        # per-stratum totals, and strata are sampled independently, so the
        # stratum variances add directly: Var(sum_h T_h) = sum_h Var(T_h).
        # This differs from mean_localmean()/cat_localmean_prop(), where the
        # combined estimate is a population-size-weighted average of the
        # per-stratum estimates and the variances are therefore combined
        # with appropriate weights. stderr[isubpop] temporarily holds a variance
        # here and is converted to a standard error via sqrt() once all
        # strata are done. Perhaps confusingly, stderr[isubpop] below
        # is actually a variance until it its square root is taken a few
        # lines below (in stderr[isubpop] <- sqrt(stderr[isubpop])).
        # Consider making this more clear in the future.

        stderr[isubpop] <- stderr[isubpop] + varest

        # End the subsection for individual strata
      }

      # Begin the subsection for all strata combined

      # Add estimates to the data frames for results

      stderr[isubpop] <- sqrt(stderr[isubpop])
      lbound <- totalest[isubpop] - mult * stderr[isubpop]
      ubound <- totalest[isubpop] + mult * stderr[isubpop]
      confval[isubpop, ] <- c(lbound, ubound)

      # End the subsection for all strata combined

      # Branch for an unstratified sample
    } else {
      # Calculate the standard error estimates

      if (cluster_ind) {
        # subset_local = FALSE is not yet supported for two-stage designs
        # (blocked upstream in cont_analysis()), so this branch is
        # intentionally left passing domain-only data
        temp <- total_var(
          contvar[tst], wgt2[tst], xcoord[tst], ycoord[tst],
          stratum_ind, NULL, cluster_ind, clusterID[tst], wgt1[tst],
          xcoord1[tst], ycoord1[tst], warn_ind, warn_df, warn_vec
        )
      } else {
        # data_tst mirrors stratum_data_i above: domain-only under TRUE,
        # every responded site under FALSE, with subpop_ind_full marking
        # the real domain members.
        data_tst <- if (subset_local) tst else tst_resp
        temp <- total_var(contvar[data_tst], wgt[data_tst], xcoord[data_tst],
          ycoord[data_tst], stratum_ind, NULL, cluster_ind,
          warn_ind = warn_ind,
          warn_df = warn_df, warn_vec = warn_vec, subset_local = subset_local,
          subpop_ind = subpop_ind_full[data_tst]
        )
      }
      warn_ind <- temp$warn_ind
      warn_df <- temp$warn_df
      if (temp$varest < 0) {
        temp$vartype <- "SRS"
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator for an infinite population was used.\n"
        warn <- paste0("The local total variance estimator produced a  negative variance estimate, the simple \nrandom sampling variance estimator for an infinite population was used to calculate \nvariance of the total estimate.\n")
        warn_df <-
          rbind(
            warn_df,
            data.frame(
              func = I(fname), subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = NA, warning = I(warn),
              action = I(act)
            )
          )
      }
      if (temp$vartype == "SRS") {
        rslt_svy <- svytotal(make.formula(ivar),
          design = subset(design, tst),
          na.rm = TRUE
        )
        sdest <- SE(rslt_svy)
      } else {
        sdest <- sqrt(temp$varest)
      }

      # Calculate confidence bounds

      lbound <- totalest[isubpop] - mult * sdest
      ubound <- totalest[isubpop] + mult * sdest

      # Add estimates to the output objects

      stderr[isubpop] <- sdest
      confval[isubpop, ] <- c(lbound, ubound)
    }
  }

  # Return results

  list(
    stderr = stderr,
    confval = confval,
    warn_ind = warn_ind,
    warn_df = warn_df
  )
}
