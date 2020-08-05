################################################################################
# Function: cat_localmean_total
# Programmer: Tom Kincaid
# Date: July 23, 2020
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
#' @param design Object of class survey.design that specifies a complex survey
#'   design.
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the design argument.
#'
#' @param rslt.U Data frame that provides size estimates for the categorical
#'   response variable.
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
################################################################################

cat_localmean_total <- function(itype, lev_itype, nlev_itype, ivar, lev_ivar,
  nlev_ivar, design, design_names, rslt.U, popcorrect, vartype, mult, warn.ind,
  warn.df) {

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

  stderr.U <- data.frame(array(0, c(nlev_itype, nlev_ivar + 1)))
  dimnames(stderr.U) <- list(lev_itype, c(lev_ivar, "Total"))
  temp <- nlev_ivar + 1
  confval.U <- data.frame(array(0, c(nlev_itype * temp , 2)))
  dimnames(confval.U) <- list(
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

    size <- unlist(rslt.U[isubpop, 1:(nlev_ivar + 1)])
    size_names <- c(lev_ivar, "Total")

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
          temp <- catvar_total(factor(catvar[stratum.i]), wgt2[stratum.i],
            xcoord[stratum.i], ycoord[stratum.i], size_names, stratum.ind,
            stratum.levels[i], cluster.ind, clusterID[stratum.i],
            wgt1[stratum.i], xcoord1[stratum.i], ycoord1[stratum.i], popcorrect,
            NULL, Ncluster[stratum.i], stage1size[stratum.i], vartype, warn.ind,
            warn.df, warn.vec)
        } else {
          temp <- catvar_total(factor(catvar[stratum.i]), wgt[stratum.i],
            xcoord[stratum.i], ycoord[stratum.i], size_names, stratum.ind,
            stratum.levels[i], cluster.ind, pcfactor.ind = popcorrect,
            fpcsize=fpcsize[stratum.i], vartype = vartype, warn.ind = warn.ind,
            warn.df = warn.df, warn.vec = warn.vec)
        }
        if(temp$vartype == "SRS") {
          if(nlev_ivar <= 1) {
            rslt.svy <- svytotal(~zzz, design = subset(design, stratum.i),
              na.rm = TRUE)
            varest <- c(SE(rslt.svy)^2, SE(rslt.svy)^2)
            names(varest) <- size_names
          } else {
            rslt.svy <- svytotal(make.formula(ivar),
              design = subset(design, stratum.i), na.rm = TRUE)
            rslt.T <- svytotal(~zzz, design = subset(design, stratum.i),
              na.rm = TRUE)
            varest <- c(SE(rslt.svy)^2, SE(rslt.T)^2)
            names(varest) <- size_names
          }
        } else {
          varest <- temp$varest
        }
        warn.ind <- temp$warn.ind
        warn.df <- temp$warn.df

# Add estimates to the stderr.U data frame

        ind <- size_names %in% names(varest)
        indx <- (1:(nlev_ivar + 1))[ind]
        stderr.U[isubpop, indx] <- stderr.U[isubpop, indx] + varest

# End the subsection for individual strata

      }

# Begin the subsection for all strata combined

# Add estimates to the data frames for results

      names_sdest <- names(stderr.U)[stderr.U[isubpop, ] > 0]
      ind <- size_names %in% names_sdest
      indx <- (1:(nlev_ivar + 1))[ind]
      stderr.U[isubpop, indx] <- sqrt(stderr.U[isubpop, indx])
      if("postStrata" %in% names(design)) {
        temp <- names_sdest[size[ind] == totalwgt]
        stderr.U[isubpop, temp] <- 0
      }
      lbound <- unlist(pmax(size[ind] - mult * stderr.U[isubpop, indx], 0))
      if("postStrata" %in% names(design)) {
        ubound <- unlist(pmin(size[ind] + mult * stderr.U[isubpop, indx],
          totalwgt))
      } else {
        ubound <- unlist(size[ind] + mult * stderr.U[isubpop, indx])
      }
      temp <- paste(rep(lev_itype[isubpop], length(names_sdest)), names_sdest,
        sep=":")
      ind <- rownames(confval.U) %in% temp
      confval.U[ind, ] <- cbind(lbound, ubound)

# End the subsection for all strata combined

# Branch for an unstratified sample

    } else {

# Calculate the standard error estimates

      if(cluster.ind) {
        temp <- catvar_total(factor(catvar[tst]), wgt2[tst], xcoord[tst],
          ycoord[tst], size_names, stratum.ind, NULL, cluster.ind,
          clusterID[tst], wgt1[tst], xcoord1[tst], ycoord1[tst], popcorrect,
          NULL, Ncluster[tst], stage1size[tst], vartype, warn.ind, warn.df,
          warn.vec)
      } else {
        temp <- catvar_total(factor(catvar[tst]), wgt[tst], xcoord[tst],
          ycoord[tst], size_names, stratum.ind, NULL, cluster.ind,
          pcfactor.ind = popcorrect, fpcsize=fpcsize[tst], vartype = vartype,
          warn.ind = warn.ind, warn.df = warn.df, warn.vec = warn.vec)
      }
      if(temp$vartype == "SRS") {
        if(nlev_ivar <= 1) {
          rslt.svy <- svytotal(~zzz, design = subset(design, tst), na.rm = TRUE)
          sdest <- c(SE(rslt.svy), SE(rslt.svy))
          names(sdest) <- size_names
        } else {
          rslt.svy <- svytotal(make.formula(ivar),
            design = subset(design, tst), na.rm = TRUE)
          rslt.T <- svytotal(~zzz, design = subset(design, tst), na.rm = TRUE)
          sdest <- c(SE(rslt.svy), SE(rslt.T))
          names(sdest) <- size_names
        }
      } else {
        sdest <- sqrt(temp$varest)
      }
      warn.ind <- temp$warn.ind
      warn.df <- temp$warn.df

# Add estimates to the data frames for results

      names_sdest <- names(sdest)
      ind <- size_names %in% names_sdest
      indx <- (1:(nlev_ivar + 1))[ind]
      if("postStrata" %in% names(design)) {
        tst <- size[ind] == totalwgt
        sdest[tst] <- 0
      }
      stderr.U[isubpop, indx] <- sdest
      lbound <- pmax(size - mult * sdest, 0)
      if("postStrata" %in% names(design)) {
        ubound <- pmin(size + mult * sdest, totalwgt)
      } else {
        ubound <- size + mult * sdest
      }
      temp <- paste(rep(lev_itype[isubpop], length(names_sdest)), names_sdest,
        sep=":")
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