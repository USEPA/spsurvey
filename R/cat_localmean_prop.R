################################################################################
# Function: cat_localmean_prop
# Programmer: Tom Kincaid
# Date: July 23, 2020
# Revised: October 23, 2020 to correct a coding error when calculating
#          proportion estimates for a stratum
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
#' @param design Object of class survey.design that specifies a complex survey
#'   design.
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the design argument.
#'
#' @param rslt.P Data frame that provides proportion estimates for the
#'   categorical response variable.
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
#'     \item{\code{stderr.P}}{data frame containing standard error estimates}
#'     \item{\code{confval.P}}{data frame containing confidence bound estimates}
#'     \item{\code{warn.ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn.df}}{data frame for storing warning messages}
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
#' @export
################################################################################

cat_localmean_prop <- function(itype, lev_itype, nlev_itype, ivar, lev_ivar,
  nlev_ivar, design, design_names, rslt.P, popcorrect, vartype, mult, warn.ind,
  warn.df) {

# Assign a value to the function name variable

  fname <- "cat_localmean_prop"

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

  stderr.P <- data.frame(array(0, c(nlev_itype, nlev_ivar + 1)))
  dimnames(stderr.P) <- list(lev_itype, c(lev_ivar, "Total"))
  temp <- nlev_ivar + 1
  confval.P <- data.frame(array(0, c(nlev_itype * temp , 2)))
  confval.P[seq(temp, by = temp, length = nlev_itype), ] <- c(1.0, 1.0)
  dimnames(confval.P) <- list(
    paste(rep(lev_itype, rep(temp, nlev_itype)),
          rep(c(lev_ivar, "Total"), nlev_itype), sep=":"),
    c("LCB", "UCB"))

# Loop through all subpopulations

  for(isubpop in 1:nlev_itype) {

    tst <- !is.na(dframe[, ivar]) &
           (dframe[, itype] %in% lev_itype[isubpop])

# Assign values to the vector of category proportion estimates, prop

    prop <- unlist(rslt.P[isubpop, 1:nlev_ivar])
    prop_names <- lev_ivar

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

# Calculate proportion estimates for the stratum

        stratum.i <- tst & stratumID == stratum.levels[i]
        prop_st <- t(as.data.frame(svymean(make.formula(ivar),
          design = subset(design, stratum.i), na.rm = TRUE))[1])

# Calculate variance estimates

        if(cluster.ind) {
          temp <- catvar_prop(factor(catvar[stratum.i]), wgt2[stratum.i],
            xcoord[stratum.i], ycoord[stratum.i], prop_st, prop_names,
            stratum.ind, stratum.levels[i], cluster.ind, clusterID[stratum.i],
            wgt1[stratum.i], xcoord1[stratum.i], ycoord1[stratum.i], popcorrect,
            NULL, Ncluster[stratum.i], stage1size[stratum.i], vartype, warn.ind,
            warn.df, warn.vec)
        } else {
          temp <- catvar_prop(factor(catvar[stratum.i]), wgt[stratum.i],
            xcoord[stratum.i], ycoord[stratum.i], prop_st, prop_names,
            stratum.ind, stratum.levels[i], cluster.ind,
            pcfactor.ind = popcorrect, fpcsize=fpcsize[stratum.i],
            vartype = vartype, warn.ind = warn.ind, warn.df = warn.df,
            warn.vec = warn.vec)
        }
        if(temp$vartype == "SRS") {
          rslt.svy <- svymean(make.formula(ivar),
            design = subset(design, stratum.i), na.rm = TRUE)
          varest <- SE(rslt.svy)^2
          names(varest) <- prop_names
        } else {
          varest <- temp$varest
        }
        warn.ind <- temp$warn.ind
        warn.df <- temp$warn.df

# Add estimates to the stderr.P data frame

        ind <- prop_names %in% names(varest)
        indx <- (1:nlev_ivar)[ind]
        stderr.P[isubpop, indx] <- stderr.P[isubpop, indx] +
          ((popsize.hat[i]/sum.popsize.hat)^2)*varest

# End the subsection for individual strata

      }

# Begin the subsection for all strata combined

# Add estimates to the data frames for results

      names_sdest <- names(stderr.P)[stderr.P[isubpop, ] > 0]
      ind <- prop_names %in% names_sdest
      indx <- (1:nlev_ivar)[ind]
      stderr.P[isubpop, indx] <- sqrt(stderr.P[isubpop, indx])
      lbound <- unlist(pmax(prop[ind] - mult * stderr.P[isubpop, indx], 0))
      ubound <- unlist(pmin(prop[ind] + mult * stderr.P[isubpop, indx], 1))
      temp <- paste(rep(lev_itype[isubpop], length(names_sdest)), names_sdest,
        sep=":")
      ind <- rownames(confval.P) %in% temp
      confval.P[ind, ] <- cbind(lbound, ubound)

# End the subsection for all strata combined

# Branch for an unstratified sample

    } else {

# Calculate the standard error estimates

      if(cluster.ind) {
        temp <- catvar_prop(factor(catvar[tst]), wgt2[tst], xcoord[tst],
          ycoord[tst], prop, prop_names, stratum.ind, NULL, cluster.ind,
          clusterID[tst], wgt1[tst], xcoord1[tst],ycoord1[tst], popcorrect,
          NULL, Ncluster[tst], stage1size[tst], vartype, warn.ind, warn.df,
          warn.vec)
      } else {
        temp <- catvar_prop(factor(catvar[tst]), wgt[tst], xcoord[tst],
          ycoord[tst], prop, prop_names, stratum.ind, NULL, cluster.ind,
          pcfactor.ind = popcorrect, fpcsize = fpcsize[tst], vartype = vartype,
          warn.ind = warn.ind, warn.df = warn.df, warn.vec = warn.vec)
      }

      if(temp$vartype == "SRS") {
        rslt.svy <- svymean(make.formula(ivar), design = subset(design, tst),
          na.rm = TRUE)
        sdest <- SE(rslt.svy)
        names(sdest) <- prop_names
      } else {
        sdest <- sqrt(temp$varest)
      }
      warn.ind <- temp$warn.ind
      warn.df <- temp$warn.df

# Calculate confidence bounds

      names_sdest <- names(sdest)
      ind <- prop_names %in% names_sdest
      lbound <- pmax(prop[ind] - mult*sdest, 0)
      ubound <- pmin(prop[ind] + mult*sdest, 1)

# Add estimates to the data frames for results

      indx <- (1:nlev_ivar)[ind]
      stderr.P[isubpop, indx] <- sdest
      temp <- paste(rep(lev_itype[isubpop], length(names_sdest)), names_sdest,
        sep=":")
      ind <- rownames(confval.P) %in% temp
      confval.P[ind, ] <- cbind(lbound, ubound)

    }

  }

# Return results

  list(stderr.P = stderr.P,
       confval.P = confval.P,
       warn.ind = warn.ind,
       warn.df = warn.df)
}
