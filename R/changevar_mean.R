################################################################################
# Function: changevar_mean
# Programmer: Tom Kincaid
# Date: July 22, 2020
#'
#' Covariance or Correlation Matrix Estimate of Change in Means between Two
#' Surveys
#'
#' This function calculates estimates of the variance-covariance matrix of the
#' population proportions in a set of intervals (classes).  The set of values
#' defining upper bounds for the classes is supplied to the function. Either the
#' simple random sampling (SRS) variance estimator or the local mean variance
#' estimator is calculated, which is subject to user control.  The simple random
#' sampling variance estimator uses the independent random sample approximation
#' to calculate joint inclusion probabilities.  The function can accomodate
#' single-stage and two-stage samples.  Finite population and continuous
#' population correction factors can be utilized in variance estimation.
#'
#' @param z1 Vector of the response value for each site for survey one.
#'
#' @param z2 Vector of the response value for each site for survey two.
#'
#' @param wgt Vector of the final adjusted weight (inverse of the sample
#'   inclusion probability) for each site, which is either the weight for a
#'   single-stage sample or the stage two weight for a two-stage sample.
#'
#' @param x Vector of x-coordinate for location for each site, which is either
#'   the x- coordinate for a single-stage sample or the stage two x-coordinate
#'   for a two-stage sample.
#'
#' @param y Vector of y-coordinate for location for each site, which is either
#'   the y- coordinate for a single-stage sample or the stage two y-coordinate
#'   for a two-stage sample.
#'
#' @param revisitwgt Logical value that indicates whether each repeat visit site
#'   has the same survey design weight in the two surveys, where TRUE = the
#'   weight for each repeat visit site is the same and FALSE = the weight for
#'   each repeat visit site is not the same.  When this argument is FALSE, all
#'   of the repeat visit sites are assigned equal weights when calculating the
#'   covariance component of the change estimate standard error.
#'
#' @param mean1 The estimated mean for survey one.
#'
#' @param mean2 The estimated mean for survey two.
#'
#' @param stratum.ind Logical value that indicates whether the sample is
#'   stratified, where TRUE = a stratified sample and FALSE = not a stratified
#'   sample.
#'
#' @param stratum.level The stratum level.
#'
#' @param cluster.ind Logical value that indicates whether the sample is a
#'   two- stage sample, where TRUE = a two-stage sample and FALSE = not a
#'   two-stage sample.
#'
#' @param clusterID Vector of the stage one sampling unit (primary sampling unit
#'  or cluster) code for each site.
#'
#' @param wgt1 Vector of the final adjusted stage one weight for each site.
#'
#' @param x1 Vector of the stage one x-coordinate for location for each site.
#'
#' @param y1 Vector of the stage one y-coordinate for location for each site.
#'
#' @param pcfactor.ind Logical value that indicates whether the finite
#'  population correction factor is used during variance estimation, where TRUE
#'  = use the population correction factor and FALSE = do not use the factor.
#'  To employ the correction factor for a single-stage sample, a value must be
#'  supplied for argument fpcsize.  To employ the correction factor for a
#'  two-stage sample, values must be supplied for arguments Ncluster and
#'  stage1size.
#'
#' @param fpcsize Size of the resource, which is required for calculation of the
#'  finite population correction factor for a single-stage sample.
#'
#' @param Ncluster The number of stage one sampling units in the resource,
#'  which is required for calculation of the finite population correction
#'  factor for a two-stage sample.
#'
#' @param stage1size Vector of the size of the stage one sampling units of a
#'  two-stage sample, which is required for calculation of the finite
#'  population correction factor for a two-stage sample.
#'
#' @param vartype The choice of variance estimator, where "Local" = local mean
#'   estimator and "SRS" = SRS estimator.
#'
#' @param warn.ind  Logical value that indicates whether warning messages were
#'   generated, where TRUE = warning messages were generated and FALSE = warning
#'   messages were not generated.
#'
#' @param warn.df Data frame for storing warning messages.
#'
#' @param warn.vec Vector that contains names of the population type, the
#'   subpopulation, and an indicator.
#'
#' @return An object in list format composed of a vector named rslt, which
#'   contains the covariance or correlation estimate, a logical variable named
#'   warn,ind, which is the indicator for warning messges, and a data frame
#'   named warn.df, which contains warning messages.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{localmean_weight}}}{calculate the weighting matrix for
#'       the local mean variance estimator}
#'     \item{\code{\link{localmean_cov}}}{calculate the variance/covariance
#'       matrix using the local mean estimator}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

changevar_mean <- function(z1, z2, wgt, x, y, revisitwgt, mean1, mean2,
  stratum.ind, stratum.level, cluster.ind, clusterID, wgt1, x1, y1,
  pcfactor.ind, fpcsize, Ncluster, stage1size, vartype, warn.ind, warn.df,
  warn.vec) {

# Assign the function name

  fname <- "changevar_mean"

#
# Calculate covariance or correlation using the repeat visit sites
#

# Begin the section for a two-stage sample

  if(cluster.ind) {

# Calculate additional required values

    cluster <- factor(clusterID)
    cluster.levels <- levels(cluster)
    ncluster <- length(cluster.levels)
    z1.lst <- split(z1, cluster)
    z2.lst <- split(z2, cluster)
    wgt2.lst <- split(wgt, cluster)
    wgt1.u <- as.vector(tapply(wgt1, cluster, unique))
    tw2 <- (sum(wgt1*wgt))^2
    if(vartype == "Local") {
      x2.lst <- split(x, cluster)
      y2.lst <- split(y, cluster)
      x1.u <- as.vector(tapply(x1, cluster, unique))
      y1.u <- as.vector(tapply(y1, cluster, unique))
    }
    if(pcfactor.ind) {
      N.cluster <- unique(Ncluster)
      stage1size.u <- as.vector(tapply(stage1size, cluster, unique))
    }
    var.ind <- sapply(split(cluster, cluster), length) > 1

    # Determine whether the mean could be calculated for both surveys
    if(is.na(mean1) | is.na(mean2)) {
      warn.ind <- TRUE
      act <- "Covariance among the repeat visit sites was not included in calculation of \nthe standard error estimate.\n"
      if(stratum.ind) {
        warn <- paste("The repeat visit sites mean in stratum \"", stratum.level, "\" \ncould not be calculated in one of the surveys.\n", sep="")
        warn.df <- rbind(warn.df, data.frame(func=I(fname),
          subpoptype=warn.vec[1], subpop=warn.vec[2],
          indicator=warn.vec[3], stratum=I(stratum.level), warning=I(warn),
          action=I(act)))
      } else {
        warn <- paste("The repeat visit sites mean could not be calculated in one of the surveys.\n", sep="")
        warn.df <- rbind(warn.df, data.frame(func=I(fname),
          subpoptype=warn.vec[1], subpop=warn.vec[2],
          indicator=warn.vec[3], stratum=NA, warning=I(warn),
          action=I(act)))
      }
      rslt <- NA
    } else {

# Calculate estimates of the total of the stage two sampling unit residuals
# and the variance/covariance of those totals for each stage one sampling unit

      total2est <- matrix(0, ncluster, 2)
      var2est <- matrix(0, ncluster, 4)
      phat <- c(mean1, mean2)
      for(i in 1:ncluster) {

# Calculate the weighted residuals matrix

        n <- length(z1.lst[[i]])
        z1 <- z1.lst[[i]]
        z2 <- z2.lst[[i]]
        rm <- (cbind(z1, z2) - matrix(rep(phat, n), nrow=n, byrow=TRUE)) *
          matrix(rep(wgt2.lst[[i]], 2), nrow=n)

        # Calculate total estimates for the stage one sampling unit
        total2est[i,] <- apply(rm, 2, sum)

        # Adjust the variance/covariance estimator for small sample size
        SRSind <- FALSE
        if(vartype == "Local" && n < 4) {
          warn.ind <- TRUE
          act <- "The simple random sampling variance estimator was used.\n"
          if(stratum.ind) {
            warn <- paste("There are less than four response values for stage one sampling unit ", cluster.levels[i], "\nin stratum ", stratum.level, ", the simple random sampling variance estimator \nwas used to calculate covariance of the estimate.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
              subpoptype=warn.vec[1], subpop=warn.vec[2],
              indicator=warn.vec[3], stratum=I(stratum.level),
              warning=I(warn), action=I(act)))
          } else {
            warn <- paste("There are less than four response values for stage one sampling unit ", cluster.levels[i], ", \nthe simple random sampling variance estimator was used to calculate covariance of \nthe estimate.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
              subpoptype=warn.vec[1], subpop=warn.vec[2],
              indicator=warn.vec[3], stratum=NA, warning=I(warn),
              action=I(act)))
          }
          vartype <- "SRS"
          SRSind <- TRUE
        }

# Calculate the population correction factor for the stage two sample

        pcfactor <- ifelse(pcfactor.ind, (stage1size.u[i] - n)/stage1size.u[i],
          1)

# Calculate variance/covariance estimates for the stage one sampling unit

        if(var.ind[i]) {
          if(vartype == "Local") {
            weight.lst <- localmean_weight(x2.lst[[i]], y2.lst[[i]],
              1/wgt2.lst[[i]])
            var2est[i,] <- as.vector(pcfactor*localmean_cov(rm, weight.lst))
          } else {
            var2est[i,] <- as.vector(pcfactor*n*var(rm))
            if(SRSind)
              vartype <- "Local"
          }
        }
      }

# Adjust the variance estimator for small sample size

      if(vartype == "Local" && ncluster < 4) {
        warn.ind <- TRUE
        act <- "The simple random sampling variance estimator was used.\n"
        if(stratum.ind) {
          warn <- paste("There are less than four stage one sampling units in stratum ", stratum.level, ", \nthe simple random sampling variance estimator was used to calculate covariance of \nthe estimate.\n", sep="")
          warn.df <- rbind(warn.df, data.frame(func=I(fname),
            subpoptype=warn.vec[1], subpop=warn.vec[2],
            indicator=warn.vec[3], stratum=I(stratum.level), warning=I(warn),
            action=I(act)))
        } else {
          warn <- paste("There are less than four stage one sampling units, the simple random sampling \nvariance estimator was used to calculate covariance of the estimate.\n", sep="")
          warn.df <- rbind(warn.df, data.frame(func=I(fname),
            subpoptype=warn.vec[1], subpop=warn.vec[2],
            indicator=warn.vec[3], stratum=NA, warning=I(warn),
            action=I(act)))
        }
        vartype <- "SRS"
      }

# Calculate the population correction factor for the stage one sample

      pcfactor <- ifelse(pcfactor.ind, (N.cluster - ncluster)/N.cluster, 1)

# Calculate the covariance or correlation estimates

      if(vartype == "Local") {
        weight.lst <- localmean_weight(x1.u, y1.u, 1/wgt1.u)
        varest <- (pcfactor*localmean_cov(total2est * matrix(rep(wgt1.u, 2),
          nrow = ncluster), weight.lst) + matrix(apply(var2est *
              matrix(rep(wgt1.u, 4), nrow=ncluster), 2, sum), nrow=2)) / tw2
      } else {
        varest <- (pcfactor*ncluster*var(total2est * matrix(rep(wgt1.u, 2),
          nrow=ncluster)) + matrix(apply(var2est * matrix(rep(wgt1.u, 4),
            nrow=ncluster), 2, sum), nrow=2))/ tw2
      }
      if(revisitwgt) {
        rslt <- varest[1,2]
      } else {
        if(varest[1,1] == 0 | varest[2,2] == 0 | any(is.na(varest))) {
          warn.ind <- TRUE
          act <- "Covariance among the repeat visit sites was not included in calculation of \nthe standard error estimate.\n"
          if(stratum.ind) {
            warn <- paste("The variance estimate for the repeat visit sites mean in stratum \"", stratum.level, "\" \nwas equal to zero for at least one of the surveys.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
              subpoptype=warn.vec[1], subpop=warn.vec[2],
              indicator=warn.vec[3], stratum=I(stratum.level),
              warning=I(warn), action=I(act)))
          } else {
            warn <- paste("The variance estimate for the repeat visit sites mean was equal to zero \nfor at least \none of the surveys.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
              subpoptype=warn.vec[1], subpop=warn.vec[2],
              indicator=warn.vec[3], stratum=NA, warning=I(warn),
              action=I(act)))
          }
          rslt <- NA
        } else {
          rslt <- varest[1,2]/sqrt(varest[1,1]*varest[2,2])
        }
      }
    }

# End the section for a two-stage sample

  } else {

# Begin the section for a single-stage sample

# Calculate additional required values

    n <- length(z1)
    tw2 <- (sum(wgt))^2
    if(pcfactor.ind) {
      fpcsize.u <- unique(fpcsize)
    }

# Determine whether the mean could be calculated for both surveys

    if(is.na(mean1) | is.na(mean2)) {
      warn.ind <- TRUE
      act <- "Covariance among the repeat visit sites was not included in calculation of \nthe standard error estimate.\n"
      if(stratum.ind) {
        warn <- paste("The repeat visit sites mean in stratum \"", stratum.level, "\" \ncould not be calculated in one of the surveys.\n", sep="")
        warn.df <- rbind(warn.df, data.frame(func=I(fname),
          subpoptype=warn.vec[1], subpop=warn.vec[2],
          indicator=warn.vec[3], stratum=I(stratum.level), warning=I(warn),
          action=I(act)))
      } else {
        warn <- paste("The repeat visit sites mean could not be calculated in one of the surveys.\n", sep="")
        warn.df <- rbind(warn.df, data.frame(func=I(fname),
          subpoptype=warn.vec[1], subpop=warn.vec[2],
          indicator=warn.vec[3], stratum=NA, warning=I(warn),
          action=I(act)))
      }
      rslt <- NA
    } else {

# Calculate the weighted residuals matrix
      phat <- c(mean1, mean2)
      rm <- (cbind(z1, z2) - matrix(rep(phat, n), nrow=n, byrow=TRUE)) *
        matrix(rep(wgt, 2), nrow=n)

# Adjust the variance estimator for small sample size

      if(vartype == "Local" && n < 4) {
        warn.ind <- TRUE
        act <- "The simple random sampling variance estimator was used.\n"
        if(stratum.ind) {
          warn <- paste("There are less than four response values in stratum ", stratum.level, ", \nthe simple random sampling variance estimator was used to calculate covariance of \nthe estimate.\n", sep="")
          warn.df <- rbind(warn.df, data.frame(func=I(fname),
            subpoptype=warn.vec[1], subpop=warn.vec[2],
            indicator=warn.vec[3], stratum=I(stratum.level), warning=I(warn),
            action=I(act)))
        } else {
          warn <- "\nThere are less than four response values, the simple random sampling variance \nestimator was used to calculate covariance of the estimate.\n"
          warn.df <- rbind(warn.df, data.frame(func=I(fname),
            subpoptype=warn.vec[1], subpop=warn.vec[2],
            indicator=warn.vec[3], stratum=NA, warning=I(warn),
            action=I(act)))
        }
        vartype <- "SRS"
      }

# Calculate the population correction factor

      pcfactor <- ifelse(pcfactor.ind, (fpcsize.u - n)/fpcsize.u, 1)

# Calculate covariance or correlation estimates

      if(vartype == "Local") {
        weight.lst <- localmean_weight(x, y, 1/wgt)
        varest <- pcfactor*localmean_cov(rm, weight.lst) / tw2
      } else {
        varest <- pcfactor*n*var(rm) / tw2
      }
      if(revisitwgt) {
        rslt <- varest[1,2]
      } else {
        if(varest[1,1] == 0 | varest[2,2] == 0 | any(is.na(varest))) {
          warn.ind <- TRUE
          act <- "Covariance among the repeat visit sites was not included in calculation of \nthe standard error estimate.\n"
          if(stratum.ind) {
            warn <- paste("The variance estimate for the repeat visit sites mean in stratum \"", stratum.level, "\" \nwas equal to zero for at least one of the surveys.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
              subpoptype=warn.vec[1], subpop=warn.vec[2],
              indicator=warn.vec[3], stratum=I(stratum.level),
              warning=I(warn), action=I(act)))
          } else {
            warn <- paste("The variance estimate for the repeat visit sites mean was equal to zero \nfor at least \none of the surveys.\n", sep="")
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
              subpoptype=warn.vec[1], subpop=warn.vec[2],
              indicator=warn.vec[3], stratum=NA, warning=I(warn),
              action=I(act)))
          }
          rslt <- NA
        } else {
          rslt <- varest[1,2]/sqrt(varest[1,1]*varest[2,2])
        }
      }
    }

# End the section for a single-stage sample

  }

# Return the covariance or correlation estimate, the warning message indicator,
# and the warn.df data frame

  list(rslt=rslt, warn.ind=warn.ind, warn.df=warn.df)
}
