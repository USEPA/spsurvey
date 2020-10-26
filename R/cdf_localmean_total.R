################################################################################
# Function: cdf_localmean_total
# Programmer: Tom Kincaid
# Date: July 9, 2020
#'
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
#' @param design Object of class survey.design that specifies a complex survey
#'   design.
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the design argument.
#'
#' @param cdfval Vector of the set of values at which the CDF is estimated.
#'
#' @param ncdfval Numeric value containing length of the set of values at which
#'   the CDF is estimated.
#'
#' @param cdfest.U Object that provides CDF estimates on the total scale
#'   for the continuous response variable.
#'
#' @param popcorrect Logical value that indicates whether the finite population
#'   correction factor should be employed during variance estimation.
#'
#' @param vartype The choice of variance estimator, where "Local" = local mean
#'   estimator and "SRS" = SRS estimator.
#'
#' @param mult Numeric value that provides the Normal distribution confidence
#'   bound multiplier.
#'
#' @param warn.ind Logical value that indicates whether warning messages were
#'   generated.
#'
#' @param warn.df Data frame for storing warning messages.
#'
#' @return A list containing the following objects:
#'   \describe{
#'     \item{\code{stderr.U}}{data frame containing standard error estimates}
#'     \item{\code{confval.U}}{data frame containing confidence bound estimates}
#'     \item{\code{warn.ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn.df}}{data frame for storing warning messages}
#'   }
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{cdfvar_total}}}{calculates variance estimates of the
#'       estimated CDF using the total scale}
#'     \item{\code{\link{svytotal}}}{calculates the total for a complex survey
#'       design}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @seealso
#'   \code{\link{cdf_localmean_prop}}
#'   \code{\link{svytotal}}
#'
#' @keywords survey
#'
#' @export
################################################################################

cdf_localmean_total <- function(itype, lev_itype, nlev_itype, ivar, design,
  design_names, cdfval, ncdfval, cdfest.U, popcorrect, vartype, mult, warn.ind,
  warn.df) {

# Assign a value to the function name variable

  fname <- "cdf_localmean_total"

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

# Assign values to the continuous response variable vector, contvar

  contvar <- dframe[, ivar]

# Assign a value to the indicator variable for a two-stage sample

  cluster.ind <- !is.null(clusterID)

# Assign values to weight variables

  if(cluster.ind) {
    wgt1 <- dframe$wgt1
    wgt2 <- dframe$wgt2
  } else {
    wgt <- dframe$wgt
  }

# Create the output data frames for standard error estimates and confidence
# bound estimates

  stderr.U <- data.frame(array(0, c(nlev_itype, ncdfval)))
  rownames(stderr.U) <- lev_itype
  confval.U <- data.frame(array(0, c(nlev_itype * ncdfval , 2)))
  dimnames(confval.U) <- list(
    paste(rep(lev_itype, ncdfval),
          rep(1:ncdfval, rep(nlev_itype, ncdfval)), sep=":"),
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

# Assign values to the vector of CDF estimates, size

    size <- unlist(cdfest.U[isubpop, ])

# Assign values to the warn.vec vector

    warn.vec <- c(itype, lev_itype[isubpop], ivar)

# Assign a value to the indicator variable for a stratified sample

    stratum.ind <- !is.null(stratumID)

# For a stratified design, determine whether the subpopulation contains a single
# stratum

    if(stratum.ind) {
      stratum <- factor(stratumID[tst])
      stratum.levels <- levels(stratum)
      nstrata <- length(stratum.levels)
      if(nstrata == 1)
        stratum.ind <- FALSE
    }

# Branch for a stratified sample

    if(stratum.ind) {

# Begin the subsection for individual strata

      for(i in 1:nstrata) {

# Calculate variance estimates

        stratum.i <- tst & stratumID == stratum.levels[i]
        if(cluster.ind) {
          temp <- cdfvar_total(contvar[stratum.i], wgt2[stratum.i],
            xcoord[stratum.i], ycoord[stratum.i], cdfval, stratum.ind,
            stratum.levels[i], cluster.ind, clusterID[stratum.i],
            wgt1[stratum.i], xcoord1[stratum.i], ycoord1[stratum.i], popcorrect,
            NULL, Ncluster[stratum.i], stage1size[stratum.i], vartype, warn.ind,
            warn.df, warn.vec)
        } else {
          temp <- cdfvar_total(contvar[stratum.i], wgt[stratum.i],
            xcoord[stratum.i], ycoord[stratum.i], cdfval, stratum.ind,
            stratum.levels[i], cluster.ind, pcfactor.ind = popcorrect,
            fpcsize=fpcsize[stratum.i], vartype = vartype, warn.ind = warn.ind,
            warn.df = warn.df, warn.vec = warn.vec)
        }
        if(temp$vartype == "SRS") {
          rslt.svy <- lapply(cdfval, function(x)
            svytotal(make.formula(paste0("I(", ivar, " <= ", x, ")")),
              design = subset(design, tst), na.rm = TRUE))
          varest <- sapply(rslt.svy, function(x) SE(x)[2])^2
        } else {
          varest <- temp$varest
        }
        warn.ind <- temp$warn.ind
        warn.df <- temp$warn.df

# Add estimates to the stderr.U data frame

        stderr.U[isubpop, ] <- stderr.U[isubpop, ] + varest

# End the subsection for individual strata

      }

# Begin the subsection for all strata combined

# Add estimates to the data frames for results

      if("postStrata" %in% names(design)) {
        tst <- size == totalwgt
        stderr.U[isubpop, tst] <- 0
      }
      stderr.U[isubpop, ] <- sqrt(stderr.U[isubpop, ])
      lbound <- unlist(pmax(size - mult * stderr.U[isubpop, ], 0))
      if("postStrata" %in% names(design)) {
        ubound <- unlist(pmin(size + mult * stderr.U[isubpop, ], totalwgt))
      } else {
        ubound <- unlist(size + mult * stderr.U[isubpop, ])
      }
      temp <- paste(lev_itype[isubpop], 1:ncdfval, sep=":")
      ind <- rownames(confval.U) %in% temp
      confval.U[ind, ] <- cbind(lbound, ubound)

# End the subsection for all strata combined

# Branch for an unstratified sample

    } else {

# Calculate the standard error estimates

      if(cluster.ind) {
        temp <- cdfvar_total(contvar[tst], wgt2[tst], xcoord[tst], ycoord[tst],
        cdfval, stratum.ind, NULL, cluster.ind, clusterID[tst], wgt1[tst],
          xcoord1[tst], ycoord1[tst], popcorrect, NULL, Ncluster[tst],
          stage1size[tst], vartype, warn.ind, warn.df, warn.vec)
      } else {
        temp <- cdfvar_total(contvar[tst], wgt[tst], xcoord[tst], ycoord[tst],
          cdfval, stratum.ind, NULL, cluster.ind, pcfactor.ind = popcorrect,
          fpcsize=fpcsize[tst], vartype = vartype, warn.ind = warn.ind,
          warn.df = warn.df, warn.vec = warn.vec)
      }
      if(temp$vartype == "SRS") {
          rslt.svy <- lapply(cdfval, function(x)
            svytotal(make.formula(paste0("I(", ivar, " <= ", x, ")")),
              design = subset(design, tst), na.rm = TRUE))
          sdest <- sapply(rslt.svy, function(x) SE(x)[2])
      } else {
        sdest <- sqrt(temp$varest)
      }
      warn.ind <- temp$warn.ind
      warn.df <- temp$warn.df

# Add estimates to the data frames for results

      if("postStrata" %in% names(design)) {
        tst <- size == totalwgt
        sdest[tst] <- 0
      }
      stderr.U[isubpop, ] <- sdest
      lbound <- pmax(size - mult * sdest, 0)
      if("postStrata" %in% names(design)) {
        ubound <- pmin(size + mult * sdest, totalwgt)
      } else {
        ubound <- size + mult * sdest
      }
      temp <- paste(lev_itype[isubpop], 1:ncdfval, sep=":")
      ind <- rownames(confval.U) %in% temp
      confval.U[ind, ] <- cbind(lbound, ubound)

    }

  }

# Return results

  list(stderr.U = stderr.U,
       confval.U = confval.U,
       warn.ind = warn.ind,
       warn.df = warn.df)
}
