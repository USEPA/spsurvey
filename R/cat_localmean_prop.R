###############################################################################
# Function: cat_localmean_prop (not exported)
# Programmer: Tom Kincaid
# Date: July 23, 2020
# Revised: October 23, 2020 to correct a coding error when calculating
#          proportion estimates for a stratum
# Revised: January 28, 2021 to replace "pcfactor.ind" with "pcfactor_ind"
# Revised: April 27, 2021 to check whether the local mean variance estimator
#          produced negative estimates and to use the SRS variance estimator
#          when that situation occurs
#'
#' Local Mean Variance Estimates of Estimated Proportions for Categorical Data
#'
#' This function organizes input and output for calculation of the local mean
#' variance estimator for estimated proportions for categorical data.
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
#' @param lev_ivar Character vector that provides levels of the categorical
#'   response variable.
#'
#' @param nlev_ivar Numeric value that provides the number of levels of the
#'   categorical response variable.
#'
#' @param design Object of class \code{survey.design} that specifies a complex survey
#'   design.
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the design argument.
#'
#' @param rslt_P Data frame that provides proportion estimates for the
#'   categorical response variable.
#'
#' @param popcorrect Logical value that indicates whether the finite population
#'   correction factor should be employed during variance estimation.
#'
#' @param vartype The choice of variance estimator, where \code{"Local"} = local mean
#'   estimator and \code{"SRS"} = SRS estimator.
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
#'   \describe{
#'     \item{\code{stderr_P}}{data frame containing standard error estimates}
#'     \item{\code{confval_P}}{data frame containing confidence bound estimates}
#'     \item{\code{warn_ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn_df}}{data frame for storing warning messages}
#'   }
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{catvar_prop}}}{calculates variance estimates of the
#'       estimated proportion in each of a set of categories}
#'     \item{\code{\link{svymean}}}{calculates the mean for a complex survey
#'       design}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @seealso
#'   \code{\link{cat_localmean_total}}
#'   \code{\link{svymean}}
#'
#' @keywords survey
#'
#' @noRd
###############################################################################

cat_localmean_prop <- function(itype, lev_itype, nlev_itype, ivar, lev_ivar,
                               nlev_ivar, design, design_names, rslt_P, popcorrect, vartype, mult, warn_ind,
                               warn_df) {

  # Assign a value to the function name variable

  fname <- "cat_localmean_prop"

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

  # Assign values to the categorical response variable vector, catvar

  catvar <- dframe[, ivar]

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

  stderr_P <- data.frame(array(0, c(nlev_itype, nlev_ivar + 1)))
  dimnames(stderr_P) <- list(lev_itype, c(lev_ivar, "Total"))
  temp <- nlev_ivar + 1
  confval_P <- data.frame(array(0, c(nlev_itype * temp, 2)))
  confval_P[seq(temp, by = temp, length = nlev_itype), ] <- c(1.0, 1.0)
  dimnames(confval_P) <- list(
    paste(rep(lev_itype, rep(temp, nlev_itype)),
      rep(c(lev_ivar, "Total"), nlev_itype),
      sep = ":"
    ),
    c("LCB", "UCB")
  )

  # Loop through all subpopulations

  for (isubpop in 1:nlev_itype) {
    tst <- !is.na(dframe[, ivar]) &
      (dframe[, itype] %in% lev_itype[isubpop])

    # Assign values to the vector of category proportion estimates, prop

    prop <- unlist(rslt_P[isubpop, 1:nlev_ivar])
    prop_names <- lev_ivar

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

      # Calculate values required for weighting strata

      if (cluster_ind) {
        popsize_hat <- tapply(wgt1[tst] * wgt2[tst], stratum, sum)
        sum_popsize_hat <- sum(wgt1[tst] * wgt2[tst])
      } else {
        popsize_hat <- tapply(wgt[tst], stratum, sum)
        sum_popsize_hat <- sum(wgt[tst])
      }

      # Begin the subsection for individual strata

      for (i in 1:nstrata) {

        # Calculate proportion estimates for the stratum

        stratum_i <- tst & stratumID == stratum_levels[i]
        prop_st <- t(as.data.frame(svymean(make.formula(ivar),
          design = subset(design, stratum_i), na.rm = TRUE
        ))[1])

        # Calculate variance estimates

        if (cluster_ind) {
          temp <- catvar_prop(
            factor(catvar[stratum_i]), wgt2[stratum_i],
            xcoord[stratum_i], ycoord[stratum_i], prop_st, prop_names,
            stratum_ind, stratum_levels[i], cluster_ind, clusterID[stratum_i],
            wgt1[stratum_i], xcoord1[stratum_i], ycoord1[stratum_i], popcorrect,
            NULL, Ncluster[stratum_i], stage1size[stratum_i], vartype, warn_ind,
            warn_df, warn_vec
          )
        } else {
          temp <- catvar_prop(factor(catvar[stratum_i]), wgt[stratum_i],
            xcoord[stratum_i], ycoord[stratum_i], prop_st, prop_names,
            stratum_ind, stratum_levels[i], cluster_ind,
            pcfactor_ind = popcorrect, fpcsize = fpcsize[stratum_i],
            vartype = vartype, warn_ind = warn_ind, warn_df = warn_df,
            warn_vec = warn_vec
          )
        }
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df
        if(any(temp$varest < 0)) {
          temp$vartype <- "SRS"
          warn_ind <- TRUE
          act <- "The simple random sampling variance estimator was used.\n"
          warn <- paste0("The local mean variance estimator produced one or more  negative varaince estimates in \nstratum \"", stratum_levels[i], "\", the simple random sampling variance estimator was used to calculate \nvariance of the category proportion estimates.\n")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = I(stratum_levels[i]),
            warning = I(warn), action = I(act)
          ))
        }
        if (temp$vartype == "SRS") {
          rslt_svy <- svymean(make.formula(ivar),
            design = subset(design, stratum_i), na.rm = TRUE
          )
          varest <- SE(rslt_svy)^2
          names(varest) <- prop_names
        } else {
          varest <- temp$varest
        }

        # Add estimates to the stderr_P data frame

        ind <- prop_names %in% names(varest)
        indx <- (1:nlev_ivar)[ind]
        stderr_P[isubpop, indx] <- stderr_P[isubpop, indx] +
          ((popsize_hat[i] / sum_popsize_hat)^2) * varest

        # End the subsection for individual strata
      }

      # Begin the subsection for all strata combined

      # Add estimates to the data frames for results

      names_sdest <- names(stderr_P)[stderr_P[isubpop, ] > 0]
      ind <- prop_names %in% names_sdest
      indx <- (1:nlev_ivar)[ind]
      stderr_P[isubpop, indx] <- sqrt(stderr_P[isubpop, indx])
      lbound <- unlist(pmax(prop[ind] - mult * stderr_P[isubpop, indx], 0))
      ubound <- unlist(pmin(prop[ind] + mult * stderr_P[isubpop, indx], 1))
      temp <- paste(rep(lev_itype[isubpop], length(names_sdest)), names_sdest,
        sep = ":"
      )
      ind <- rownames(confval_P) %in% temp
      confval_P[ind, ] <- cbind(lbound, ubound)

      # End the subsection for all strata combined

      # Branch for an unstratified sample
    } else {

      # Calculate the standard error estimates

      if (cluster_ind) {
        temp <- catvar_prop(
          factor(catvar[tst]), wgt2[tst], xcoord[tst],
          ycoord[tst], prop, prop_names, stratum_ind, NULL, cluster_ind,
          clusterID[tst], wgt1[tst], xcoord1[tst], ycoord1[tst], popcorrect,
          NULL, Ncluster[tst], stage1size[tst], vartype, warn_ind, warn_df,
          warn_vec
        )
      } else {
        temp <- catvar_prop(factor(catvar[tst]), wgt[tst], xcoord[tst],
          ycoord[tst], prop, prop_names, stratum_ind, NULL, cluster_ind,
          pcfactor_ind = popcorrect, fpcsize = fpcsize[tst], vartype = vartype,
          warn_ind = warn_ind, warn_df = warn_df, warn_vec = warn_vec
        )
      }
      warn_ind <- temp$warn_ind
      warn_df <- temp$warn_df
      if(any(temp$varest < 0)) {
        temp$vartype <- "SRS"
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator was used.\n"
        warn <- paste0("The local mean variance estimator produced one or more  negative varaince estimates, the \nsimple random sampling variance estimator was used to calculate variance of the category \nproportion estimates.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = NA,
          warning = I(warn), action = I(act)
        ))
      }
      if (temp$vartype == "SRS") {
        rslt_svy <- svymean(make.formula(ivar),
          design = subset(design, tst),
          na.rm = TRUE
        )
        sdest <- SE(rslt_svy)
        names(sdest) <- prop_names
      } else {
        sdest <- sqrt(temp$varest)
      }

      # Calculate confidence bounds

      names_sdest <- names(sdest)
      ind <- prop_names %in% names_sdest
      lbound <- pmax(prop[ind] - mult * sdest, 0)
      ubound <- pmin(prop[ind] + mult * sdest, 1)

      # Add estimates to the data frames for results

      indx <- (1:nlev_ivar)[ind]
      stderr_P[isubpop, indx] <- sdest
      temp <- paste(rep(lev_itype[isubpop], length(names_sdest)), names_sdest,
        sep = ":"
      )
      ind <- rownames(confval_P) %in% temp
      confval_P[ind, ] <- cbind(lbound, ubound)
    }
  }

  # Return results

  list(
    stderr_P = stderr_P,
    confval_P = confval_P,
    warn_ind = warn_ind,
    warn_df = warn_df
  )
}
