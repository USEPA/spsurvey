################################################################################
# Function: cdftest_localmean_total
# Programmer: Tom Kincaid
# Date: October 23, 2020
#'
#' Local Mean Variance/Covarince Estimates of Estimated Population Totals
#'
#' This function organizes input and output for calculation of the local mean
#' variance/covariance estimator for estimated totals for categorical data.
#'
#' @param design Object of class survey.design that specifies a complex survey
#'   design.
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the design argument.
#'
#' @param popcorrect Logical value that indicates whether the finite population
#'   correction factor should be employed during variance estimation.
#'
#' @param vartype The choice of variance estimator, where "Local" = local mean
#'   estimator and "SRS" = SRS estimator.
#'
#' @param warn.ind Logical value that indicates whether warning messages were
#'   generated.
#'
#' @param warn.df Data frame for storing warning messages.
#'
#' @param warn.vec Character vector that contains a subpopulation name, the
#'   first subpopulation level, the second subpopulation level, and an
#'   indicator name.
#'
#' @return A matrix containing the variance/covariance estimates for the
#'   contingency table proportion estimates.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{cdftestvar_total}}}{calculates variance-covariance
#'       estimates of the estimated totals (cell totals, row totals, column
#'       totals, and grand total) of a contingency table}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @seealso
#'   \code{\link{cdftest_localmean_prop}}
#'
#' @keywords survey
#'
#' @export
################################################################################

cdftest_localmean_total <- function(design, design_names,  popcorrect, vartype,
  warn.ind, warn.df, warn.vec) {

# Assign a value to the function name variable

  fname <- "cdftest_localmean_total"

# For variables that exist in the design$variables data frame, assign survey
# design variables

  dframe <- subset(design$variables, !is.na(rowvar))
  for(i in names(design_names)) {
    if(is.null(design_names[[i]])) {
      eval(parse(text=paste0(i, " <- NULL")))
    } else {
      eval(parse(text=paste0(i, " <- dframe[, \"", design_names[[i]], "\"]")))
    }
  }

# Assign a value to the indicator variable for a two-stage sample

  cluster.ind <- !is.null(clusterID)

# Assign values to weight variables

  if(cluster.ind) {
    wgt1 <- dframe$wgt1
    wgt2 <- dframe$wgt2
  } else {
    wgt <- dframe$wgt
  }

# Assign a value to the indicator variable for a stratified sample

  stratum.ind <- !is.null(stratumID)

# For a stratified design, determine whether the subpopulation contains a single
# stratum

    if(stratum.ind) {
      stratum <- factor(stratumID)
      stratum.levels <- levels(stratum)
      nstrata <- length(stratum.levels)
      if(nstrata == 1)
        stratum.ind <- FALSE
    }

# Branch for a stratified sample

    if(stratum.ind) {

# Calculate values required for weighting strata

      if(cluster.ind) {
        popsize.hat <- tapply(wgt1 * wgt2, stratum, sum)
        sum.popsize.hat <- sum(wgt1 * wgt2)
      } else {
        popsize.hat <- tapply(wgt, stratum, sum)
        sum.popsize.hat <- sum(wgt)
      }

# Create the varest matrix

      lev_rowvar <- levels(design$variables$rowvar)
      nr <- length(lev_rowvar)
      lev_colvar <- levels(design$variables$colvar)
      nc <- length(lev_colvar)
      m <- nr*nc + nr + nc + 1
      varest <- matrix(0, m, m)
      temp <- paste0("interaction(factor(rowvar), factor(colvar))",
       lev_rowvar)
      colnames_varest <- c(
        paste(rep(temp, nc), rep(lev_colvar, each=nr), sep="."),
        paste0("factor(rowvar)", lev_rowvar),
        paste0("factor(colvar)", lev_colvar),
        "mm_total"
      )

# Calculate variance estimates

      for(i in 1:nstrata) {
        temp <- design_names$stratumID
        tst <- design$variables[, temp] != stratum.levels[i]
        design.temp <- design
        design.temp$variables$rowvar[tst] <- NA
        stratum.i <- stratumID == stratum.levels[i]
        if(cluster.ind) {
          temp <- cdftestvar_total(design.temp, wgt2[stratum.i],
            xcoord[stratum.i], ycoord[stratum.i], stratum.ind,
            stratum.levels[i], cluster.ind, clusterID[stratum.i],
            wgt1[stratum.i], xcoord1[stratum.i], ycoord1[stratum.i], popcorrect,
            NULL, Ncluster[stratum.i], stage1size[stratum.i], vartype, warn.ind,
            warn.df, warn.vec)
        } else {
          temp <- cdftestvar_total(design.temp, wgt[stratum.i],
            xcoord[stratum.i], ycoord[stratum.i], stratum.ind,
            stratum.levels[i], cluster.ind, pcfactor.ind = popcorrect,
            fpcsize = fpcsize[stratum.i], vartype = vartype,
            warn.ind = warn.ind, warn.df = warn.df, warn.vec = warn.vec)
        }
        varest_st <- temp$varest
        warn.ind <- temp$warn.ind
        warn.df <- temp$warn.df

# Add estimates to the varest matrix

        tst <- colnames_varest %in% colnames(varest_st)
        varest[tst, tst] <- varest[tst, tst] +
          ((popsize.hat[i]/sum.popsize.hat)^2)*varest_st
        colnames(varest) <- colnames_varest

# End the loop for strata

      }

# Create the results list

      results <- list(varest=varest, vartype=vartype, warn.ind=warn.ind,
        warn.df=warn.df)

# Branch for an unstratified sample

    } else {

# Calculate the variance/covariance estimates

      if(cluster.ind) {
        results <- cdftestvar_total(design, wgt2, xcoord, ycoord, stratum.ind,
          NULL, cluster.ind, clusterID, wgt1, xcoord1,ycoord1, popcorrect, NULL,
          Ncluster, stage1size, vartype, warn.ind, warn.df, warn.vec)
      } else {
        results <- cdftestvar_total(design, wgt, xcoord, ycoord, stratum.ind,
          NULL, cluster.ind, pcfactor.ind = popcorrect, fpcsize = fpcsize,
          vartype = vartype, warn.ind = warn.ind, warn.df = warn.df,
          warn.vec = warn.vec)
      }

    }

# Return results

  results
}
