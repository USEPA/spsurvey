###############################################################################
# Function: cat_localmean_total (exported)
# Programmer: Tom Kincaid
# Date: July 23, 2020
# Revised: January 28, 2021 to replace "pcfactor.ind" with "pcfactor_ind"
#'
#' Local Mean Variance Estimates of Estimated Totals for Categorical Data
#'
#' This function organizes input and output for calculation of the local mean
#' variance estimator for estimated sizes (totals) for categorical data.
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
#' @param rslt_U Data frame that provides size estimates for the categorical
#'   response variable.
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
#'     \item{\code{stderr_U}}{data frame containing standard error estimates}
#'     \item{\code{confval_U}}{data frame containing confidence bound estimates}
#'     \item{\code{warn_ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn_df}}{data frame for storing warning messages}
#'   }
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{catvar_total}}}{calculates variance estimates of the
#'       estimated size in each of a set of categories}
#'     \item{\code{\link{svytotal}}}{calculates the total for a complex survey
#'       design}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @seealso
#'   \code{\link{cat_localmean_prop}}
#'   \code{\link{svytotal}}
#'
#' @keywords survey
#'
#' @export
###############################################################################

cat_localmean_total <- function(itype, lev_itype, nlev_itype, ivar, lev_ivar,
  nlev_ivar, design, design_names, rslt_U, popcorrect, vartype, mult, warn_ind,
  warn_df) {

# Assign a value to the function name variable

  fname <- "cat_localmean_total"

# For variables that exist in the design$variables data frame, assign survey
# design variables

  dframe <- design$variables
  for(i in names(design_names)) {
    if(is.null(design_names[[i]])) {
      eval(parse(text=paste0(i, " <- NULL")))
    } else {
      eval(parse(text=paste0(i, " <- dframe[, \"", design_names[[i]], "\"]")))
    }
  }

# Assign values to the categorical response variable vector, catvar

  catvar <- dframe[, ivar]

# Assign a value to the indicator variable for a two-stage sample

  cluster_ind <- !is.null(clusterID)

# Assign values to weight variables

  if(cluster_ind) {
    wgt1 <- dframe$wgt1
    wgt2 <- dframe$wgt2
  } else {
    wgt <- dframe$wgt
  }

# Create the output data frames for standard error estimates and confidence
# bound estimates

  stderr_U <- data.frame(array(0, c(nlev_itype, nlev_ivar + 1)))
  dimnames(stderr_U) <- list(lev_itype, c(lev_ivar, "Total"))
  temp <- nlev_ivar + 1
  confval_U <- data.frame(array(0, c(nlev_itype * temp , 2)))
  dimnames(confval_U) <- list(
    paste(rep(lev_itype, rep(temp, nlev_itype)),
          rep(c(lev_ivar, "Total"), nlev_itype), sep=":"),
    c("LCB", "UCB"))

# Loop through all subpopulations

  for(isubpop in 1:nlev_itype) {

    tst <- !is.na(dframe[, ivar]) &
           (dframe[, itype] %in% lev_itype[isubpop])

# If post-stratification or calibration was applied to the design object, then
# calculate the sum of weights for the subpopulation

   if("postStrata" %in% names(design)) {
     totalwgt <- sum(weights(design)[tst])
   }

# Assign values to the vector of category size estimates, size

    size <- unlist(rslt_U[isubpop, 1:(nlev_ivar + 1)])
    size_names <- c(lev_ivar, "Total")

# Assign values to the warn_vec vector

    warn_vec <- c(itype, lev_itype[isubpop], ivar)

# Assign a value to the indicator variable for a stratified sample

    stratum_ind <- !is.null(stratumID)

# For a stratified design, determine whether the subpopulation contains a single
# stratum

    if(stratum_ind) {
      stratum <- factor(stratumID[tst])
      stratum_levels <- levels(stratum)
      nstrata <- length(stratum_levels)
      if(nstrata == 1)
        stratum_ind <- FALSE
    }

# Branch for a stratified sample

    if(stratum_ind) {

# Begin the subsection for individual strata

      for(i in 1:nstrata) {

# Calculate variance estimates

        stratum_i <- tst & stratumID == stratum_levels[i]
        if(cluster_ind) {
          temp <- catvar_total(factor(catvar[stratum_i]), wgt2[stratum_i],
            xcoord[stratum_i], ycoord[stratum_i], size_names, stratum_ind,
            stratum_levels[i], cluster_ind, clusterID[stratum_i],
            wgt1[stratum_i], xcoord1[stratum_i], ycoord1[stratum_i], popcorrect,
            NULL, Ncluster[stratum_i], stage1size[stratum_i], vartype, warn_ind,
            warn_df, warn_vec)
        } else {
          temp <- catvar_total(factor(catvar[stratum_i]), wgt[stratum_i],
            xcoord[stratum_i], ycoord[stratum_i], size_names, stratum_ind,
            stratum_levels[i], cluster_ind, pcfactor_ind = popcorrect,
            fpcsize=fpcsize[stratum_i], vartype = vartype, warn_ind = warn_ind,
            warn_df = warn_df, warn_vec = warn_vec)
        }
        if(temp$vartype == "SRS") {
          if(nlev_ivar <= 1) {
            rslt_svy <- svytotal(~zzz, design = subset(design, stratum_i),
              na.rm = TRUE)
            varest <- c(SE(rslt_svy)^2, SE(rslt_svy)^2)
            names(varest) <- size_names
          } else {
            rslt_svy <- svytotal(make.formula(ivar),
              design = subset(design, stratum_i), na.rm = TRUE)
            rslt_T <- svytotal(~zzz, design = subset(design, stratum_i),
              na.rm = TRUE)
            varest <- c(SE(rslt_svy)^2, SE(rslt_T)^2)
            names(varest) <- size_names
          }
        } else {
          varest <- temp$varest
        }
        warn_ind <- temp$warn_ind
        warn_df <- temp$warn_df

# Add estimates to the stderr_U data frame

        ind <- size_names %in% names(varest)
        indx <- (1:(nlev_ivar + 1))[ind]
        stderr_U[isubpop, indx] <- stderr_U[isubpop, indx] + varest

# End the subsection for individual strata

      }

# Begin the subsection for all strata combined

# Add estimates to the data frames for results

      names_sdest <- names(stderr_U)[stderr_U[isubpop, ] > 0]
      ind <- size_names %in% names_sdest
      indx <- (1:(nlev_ivar + 1))[ind]
      stderr_U[isubpop, indx] <- sqrt(stderr_U[isubpop, indx])
      if("postStrata" %in% names(design)) {
        temp <- names_sdest[size[ind] == totalwgt]
        stderr_U[isubpop, temp] <- 0
      }
      lbound <- unlist(pmax(size[ind] - mult * stderr_U[isubpop, indx], 0))
      if("postStrata" %in% names(design)) {
        ubound <- unlist(pmin(size[ind] + mult * stderr_U[isubpop, indx],
          totalwgt))
      } else {
        ubound <- unlist(size[ind] + mult * stderr_U[isubpop, indx])
      }
      temp <- paste(rep(lev_itype[isubpop], length(names_sdest)), names_sdest,
        sep=":")
      ind <- rownames(confval_U) %in% temp
      confval_U[ind, ] <- cbind(lbound, ubound)

# End the subsection for all strata combined

# Branch for an unstratified sample

    } else {

# Calculate the standard error estimates

      if(cluster_ind) {
        temp <- catvar_total(factor(catvar[tst]), wgt2[tst], xcoord[tst],
          ycoord[tst], size_names, stratum_ind, NULL, cluster_ind,
          clusterID[tst], wgt1[tst], xcoord1[tst], ycoord1[tst], popcorrect,
          NULL, Ncluster[tst], stage1size[tst], vartype, warn_ind, warn_df,
          warn_vec)
      } else {
        temp <- catvar_total(factor(catvar[tst]), wgt[tst], xcoord[tst],
          ycoord[tst], size_names, stratum_ind, NULL, cluster_ind,
          pcfactor_ind = popcorrect, fpcsize=fpcsize[tst], vartype = vartype,
          warn_ind = warn_ind, warn_df = warn_df, warn_vec = warn_vec)
      }
      if(temp$vartype == "SRS") {
        if(nlev_ivar <= 1) {
          rslt_svy <- svytotal(~zzz, design = subset(design, tst), na.rm = TRUE)
          sdest <- c(SE(rslt_svy), SE(rslt_svy))
          names(sdest) <- size_names
        } else {
          rslt_svy <- svytotal(make.formula(ivar),
            design = subset(design, tst), na.rm = TRUE)
          rslt_T <- svytotal(~zzz, design = subset(design, tst), na.rm = TRUE)
          sdest <- c(SE(rslt_svy), SE(rslt_T))
          names(sdest) <- size_names
        }
      } else {
        sdest <- sqrt(temp$varest)
      }
      warn_ind <- temp$warn_ind
      warn_df <- temp$warn_df

# Add estimates to the data frames for results

      names_sdest <- names(sdest)
      ind <- size_names %in% names_sdest
      indx <- (1:(nlev_ivar + 1))[ind]
      if("postStrata" %in% names(design)) {
        tst <- size[ind] == totalwgt
        sdest[tst] <- 0
      }
      stderr_U[isubpop, indx] <- sdest
      lbound <- pmax(size - mult * sdest, 0)
      if("postStrata" %in% names(design)) {
        ubound <- pmin(size + mult * sdest, totalwgt)
      } else {
        ubound <- size + mult * sdest
      }
      temp <- paste(rep(lev_itype[isubpop], length(names_sdest)), names_sdest,
        sep=":")
      ind <- rownames(confval_U) %in% temp
      confval_U[ind, ] <- cbind(lbound, ubound)

    }

  }

# Return results

  list(stderr_U = stderr_U,
       confval_U = confval_U,
       warn_ind = warn_ind,
       warn_df = warn_df)
}
