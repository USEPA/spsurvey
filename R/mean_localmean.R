################################################################################
# Function: mean_localmean
# Programmer: Tom Kincaid
# Date: July 9, 2020
#'
#' Local Mean Variance Estimates of the Estimated Mean
#'
#' This function organizes input and output for calculation of the local mean
#' variance estimator for the estimated mean.
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
#' @param design Object of class survey.design that specifies a complex survey
#'   design.
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the design argument.
#'
#' @param meanest Vector that provides estimates of the mean.
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
#'     \item{\code{stderr}}{vector containing standard error estimates}
#'     \item{\code{confval}}{data frame containing confidence bound estimates}
#'     \item{\code{warn.ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn.df}}{data frame for storing warning messages}
#'   }
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{mean_var}}}{calculates variance estimate of the
#'       estimated mean}
#'     \item{\code{\link{svymean}}}{calculates the mean for a complex survey
#'       design}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @seealso
#'   \code{\link{cdf_localmean_total}}
#'   \code{\link{svymean}}
#'
#' @keywords survey
#'
#' @export
################################################################################

mean_localmean <- function(itype, lev_itype, nlev_itype, levs, ivar, design,
  design_names, meanest, popcorrect, vartype, mult, warn.ind, warn.df) {

# Assign a value to the function name variable

  fname <- "mean_localmean"

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

# Create the output objects for standard error estimates and confidence bound
# estimates

  nlevs <- length(levs)
  stderr <- numeric(nlev_itype)
  names(stderr) <- lev_itype
  confval <- data.frame(array(0, c(nlev_itype , 2)))
  dimnames(confval) <- list(lev_itype, c("LCB", "UCB"))

# Loop through all subpopulations

  for(isubpop in levs) {

    tst <- !is.na(dframe[, ivar]) &
           (dframe[, itype] %in% lev_itype[isubpop])

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

# Calculate values required for weighting strata

      if(cluster.ind) {
        popsize.hat <- tapply(wgt1[tst] * wgt2[tst], stratum, sum)
        sum.popsize.hat <- sum(wgt1[tst] * wgt2[tst])
      } else {
        popsize.hat <- tapply(wgt[tst], stratum, sum)
        sum.popsize.hat <- sum(wgt[tst])
      }

# Begin the subsection for individual strata

      for(i in 1:nstrata) {

# Calculate mean estimates for the stratum

        stratum.i <- tst & stratumID == stratum.levels[i]
        meanest_st <- svymean(make.formula(ivar), design = subset(design,
          stratum.i))

# Calculate variance estimates

        if(cluster.ind) {
          temp <- mean_var(contvar[stratum.i], wgt2[stratum.i],
            xcoord[stratum.i], ycoord[stratum.i],  meanest_st[1], stratum.ind,
            stratum.levels[i], cluster.ind, clusterID[stratum.i],
            wgt1[stratum.i], xcoord1[stratum.i], ycoord1[stratum.i], popcorrect,
            NULL, Ncluster[stratum.i], stage1size[stratum.i], vartype, warn.ind,
            warn.df, warn.vec)
        } else {
          temp <- mean_var(contvar[stratum.i], wgt[stratum.i],
            xcoord[stratum.i], ycoord[stratum.i], meanest_st[1], stratum.ind,
            stratum.levels[i], cluster.ind, pcfactor.ind = popcorrect,
            fpcsize=fpcsize[stratum.i], vartype = vartype, warn.ind = warn.ind,
            warn.df = warn.df, warn.vec = warn.vec)
        }
        if(temp$vartype == "SRS") {
          rslt.svy <- svymean(make.formula(ivar), design = subset(design, tst),
            na.rm = TRUE)
          varest <- SE(rslt.svy)^2
        } else {
          varest <- temp$varest
        }
        warn.ind <- temp$warn.ind
        warn.df <- temp$warn.df

# Add estimate to the stderr vector

        stderr[isubpop] <- stderr[isubpop] +
          ((popsize.hat[i]/sum.popsize.hat)^2)*varest

# End the subsection for individual strata

      }

# Begin the subsection for all strata combined

# Add estimates to the data frames for results

      stderr[isubpop] <- sqrt(stderr[isubpop])
      lbound <- meanest[isubpop] - mult*stderr[isubpop]
      ubound <- meanest[isubpop] + mult*stderr[isubpop]
      confval[isubpop, ] <- c(lbound, ubound)

# End the subsection for all strata combined

# Branch for an unstratified sample

    } else {

# Calculate the standard error estimates

      if(cluster.ind) {
        temp <- mean_var(contvar[tst], wgt2[tst], xcoord[tst], ycoord[tst],
          meanest[isubpop], stratum.ind, NULL, cluster.ind, clusterID[tst],
          wgt1[tst], xcoord1[tst], ycoord1[tst], popcorrect, NULL,
          Ncluster[tst], stage1size[tst], vartype, warn.ind, warn.df, warn.vec)
      } else {
        temp <- mean_var(contvar[tst], wgt[tst], xcoord[tst], ycoord[tst],
          meanest[isubpop], stratum.ind, NULL, cluster.ind,
          pcfactor.ind = popcorrect, fpcsize = fpcsize[tst], vartype = vartype,
          warn.ind = warn.ind, warn.df = warn.df, warn.vec = warn.vec)
      }
      if(temp$vartype == "SRS") {
        rslt.svy <- svymean(make.formula(ivar), design = subset(design, tst),
          na.rm = TRUE)
        sdest <- SE(rslt.svy)
      } else {
        sdest <- sqrt(temp$varest)
      }
      warn.ind <- temp$warn.ind
      warn.df <- temp$warn.df

# Calculate confidence bounds

      lbound <- meanest[isubpop] - mult*sdest
      ubound <- meanest[isubpop] + mult*sdest

# Add estimates to the output objects

      stderr[isubpop] <- sdest
      confval[isubpop, ] <- c(lbound, ubound)

    }

  }

# Return results

  list(stderr = stderr,
       confval = confval,
       warn.ind = warn.ind,
       warn.df = warn.df)
}
