################################################################################
# Function: relrisk_var
# Programmer: Tom Kincaid
# Date: June 24, 2020
#
#' Variance-Covariance Estimate for the Relative Risk Estimator
#'
#' This function calculates the variance-covariance estimate for the cell and
#' marginal totals used to calculate the relative risk estimate.  Either the
#' simple random sampling (SRS) variance estimator or the local mean variance
#' estimator is calculated, which is subject to user control.  The SRS variance
#' estimator uses the independent random sample approximation to calculate joint
#' inclusion probabilities.  The function can  accomodate single-stage and
#' two-stage samples.
#'
#' @param response Vector of the categorical response variable.
#'
#' @param stressor  Vector of the categorical stressor variable.
#'
#' @param response_levels Vector of category values (levels) for the
#'   categorical response variable, where the first level is used for
#'   calculating the relative risk estimate.  If response_levels equals NULL,
#'   then values "Poor" and "Good" are used for the first level and second level
#'   of the response variable, respectively.  The default is NULL.
#'
#' @param stressor_levels Vector of category values (levels) for the
#'   categorical stressor variable, where the first level is used for
#'   calculating the numerator of the relative risk estimate and the second
#'   level is used for calculating the denominator of the estimate.  If
#'   stressor_levels equals NULL, then values "Poor" and "Good" are used for the
#'   first level and second level of the stressor variable, respectively.  The
#'   default is NULL.
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
#' @param stratum_ind Logical value that indicates whether the sample is
#'   stratified, where TRUE = a stratified sample and FALSE = not a stratified
#'   sample.
#'
#' @param stratum_level The stratum level.
#'
#' @param cluster_ind Logical value that indicates whether the sample is a two-
#'   stage sample, where TRUE = a two-stage sample and FALSE = not a two-stage
#'   sample.
#'
#' @param cluster Vector of the stage one sampling unit (primary sampling unit
#'   or cluster) code for each site.
#'
#' @param wgt1 Vector of the final adjusted stage one weight for each site.
#'
#' @param x1 Vector of the stage one x-coordinate for location for each site.
#'
#' @param y1 Vector of the stage one y-coordinate for location for each site.
#'
#' @param pcfactor_ind Logical value that indicates whether the finite
#'   population correction factor is used during variance estimation, where TRUE
#'   = use the population correction factor and FALSE = do not use the factor.
#'   To employ the correction factor for a single-stage sample, a value must be
#'   supplied for argument fpcsize.  To employ the correction factor for a
#'   two-stage sample, values must be supplied for arguments Ncluster and
#'   stage1size.
#'
#' @param fpcsize Size of the resource, which is required for calculation of the
#'   finite population correction factor for a single-stage sample.
#'
#' @param Ncluster The number of stage one sampling units in the resource,
#'   which is required for calculation of the finite population correction
#'   factor for a two-stage sample.
#'
#' @param stage1size Vector of the size of the stage one sampling units of a
#'   two-stage sample, which is required for calculation of the finite
#'   population correction factor for a two-stage sample.
#'
#' @param vartype The choice of variance estimator, where "Local" = local mean
#'   estimator and "SRS" = SRS estimator.
#'
#' @param warn_ind Logical value that indicates whether warning messages were
#'   generated, where TRUE = warning messages were generated and FALSE = warning
#'   messages were not generated.
#'
#' @param warn_df Data frame for storing warning messages.
#'
#' @param warn_vec Vector that contains names of the population type, the
#'   subpopulation, and an indicator.
#'
#' @return A list containing the following objects:
#'   \describe{
#'     \item{\code{varest}}{vector containing variance estimates}
#'     \item{\code{warn_ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn_df}}{data frame for storing warning messages}
#'   }
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{localmean_cov}}}{calculate the variance/covariance
#'       matrix using the local mean estimator}
#'     \item{\code{\link{localmean_weight}}}{calculate the weighting matrix for
#'       the local mean variance estimator}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

relrisk_var <- function(response, stressor, response_levels, stressor_levels,
  wgt, x, y, stratum_ind, stratum_level, cluster_ind, cluster, wgt1, x1, y1,
  pcfactor_ind, fpcsize, Ncluster, stage1size, vartype, warn_ind, warn_df,
  warn_vec) {

# Assign the function name

  fname <- "relrisk_var"

# Branch to handle two-stage and single-stage samples

  if(cluster_ind) {

# Begin the section for a two-stage sample

# Calculate additional required values

    cluster <- factor(cluster)
    cluster_levels <- levels(cluster)
    ncluster <- length(cluster_levels)
    response_1st <- split(response, cluster)
    stressor_1st <- split(stressor, cluster)
    if(vartype == "Local") {
      x2_1st <- split(x, cluster)
      y2_1st <- split(y, cluster)
      x1_u <- as.vector(tapply(x1, cluster, unique))
      y1_u <- as.vector(tapply(y1, cluster, unique))
    }
    wgt1_1st <- split(wgt, cluster)
    wgt1_u <- as.vector(tapply(wgt1, cluster, unique))
   if(pcfactor_ind) {
    N_cluster <- unique(Ncluster)
    stage1size_u <- as.vector(tapply(stage1size, cluster, unique))
   }
    var_ind <- sapply(split(cluster, cluster), length) > 1

# Calculate estimates of the total of the stage two sampling unit response
# values or residuals and the variance of those totals for each stage one
# sampling unit

    total2est <- matrix(0, ncluster, 4)
    var2est <- matrix(0, ncluster, 16)
    for(i in 1:ncluster) {

# Calculate the number of response values

      nresp <- length(response_1st[[i]])

# Create indicator variables for required cells and margins

      Ind1 <- (response_1st[[i]] == response_levels[1])*(stressor_1st[[i]] ==
        stressor_levels[1])
      Ind2 <- (stressor_1st[[i]] == stressor_levels[1])
      Ind3 <- (response_1st[[i]] == response_levels[1])*(stressor_1st[[i]] ==
        stressor_levels[2])
      Ind4 <- (stressor_1st[[i]] == stressor_levels[2])

# Calculate the matrix of weighted indicator variables

      rm <- cbind(Ind1, Ind2, Ind3, Ind4) * wgt1_1st[[i]]

# Calculate total estimates for the stage one sampling unit

      total2est[i,] <- apply(rm, 2, sum)

# Adjust the variance-covariance estimator for small sample size

      SRSind <- FALSE
      if(vartype == "Local" && nresp < 4) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator was used.\n"
        if(stratum_ind) {
          warn <- paste("There are less than four response values for stage one sampling unit ", cluster_levels[i], "\nin stratum ", stratum_level, ", the simple random sampling variance estimator \nwas used to calculate variance of the category proportion estimates.\n", sep="")
          warn_df <- rbind(warn_df, data.frame(func=I(fname),
            subpoptype=warn_vec[1], subpop=warn_vec[2],
            indicator=warn_vec[3], stratum=I(stratum_level),
            warning=I(warn), action=I(act)))
        } else {
          warn <- paste("There are less than four response values for stage one sampling unit ", cluster_levels[i], ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe category proportion estimates.\n", sep="")
          warn_df <- rbind(warn_df, data.frame(func=I(fname),
            subpoptype=warn_vec[1], subpop=warn_vec[2],
            indicator=warn_vec[3], stratum=NA, warning=I(warn),
            action=I(act)))
        }
        vartype <- "SRS"
        SRSind <- TRUE
      }

# Calculate the population correction factor for the stage two sample

      pcfactor <- ifelse(pcfactor_ind, (stage1size_u[i] - nresp) /
        stage1size_u[i], 1)

# Calculate the variance-covariance estimate for the stage one sampling unit

      if(var_ind[i]) {
        if(vartype == "Local") {
          weight_1st <- localmean_weight(x2_1st[[i]], y2_1st[[i]],
            1/wgt1_1st[[i]])
          var2est[i, ] <- as.vector(pcfactor*localmean_cov(rm, weight_1st))
        } else {
          var2est[i, ] <- as.vector(pcfactor*nresp*var(rm))
          if(SRSind)
            vartype <- "Local"
        }
      }
    }

# Adjust the variance-covariance estimator for small sample size

    if(vartype == "Local" && ncluster < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling variance estimator was used.\n"
      if(stratum_ind) {
        warn <- paste("There are less than four stage one sampling units in stratum ", stratum_level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe category proportion estimates.\n", sep="")
        warn_df <- rbind(warn_df, data.frame(func=I(fname),
          subpoptype=warn_vec[1], subpop=warn_vec[2],
          indicator=warn_vec[3], stratum=I(stratum_level), warning=I(warn),
          action=I(act)))
      } else {
        warn <- paste("There are less than four stage one sampling units, the simple random sampling \nvariance estimator was used to calculate variance of the category proportion \nestimates.\n", sep="")
        warn_df <- rbind(warn_df, data.frame(func=I(fname),
          subpoptype=warn_vec[1], subpop=warn_vec[2],
          indicator=warn_vec[3], stratum=NA, warning=I(warn),
          action=I(act)))
      }
      vartype <- "SRS"
    }

# Calculate the population correction factor for the stage one sample

    pcfactor <- ifelse(pcfactor_ind, (N_cluster - ncluster)/N_cluster, 1)

# Calculate the variance-covariance estimate

    if(vartype == "Local") {
      weight_1st <- localmean_weight(x1_u, y1_u, 1/wgt1_u)
      varest <- pcfactor*localmean_cov(total2est*matrix(rep(wgt1_u, 4),
        nrow=ncluster), weight_1st) + matrix(apply(var2est *
            matrix(rep(wgt1_u, 16), nrow = ncluster), 2, sum), nrow=4)
    } else {
      varest <- pcfactor*ncluster*var(total2est*matrix(rep(wgt1_u, 4),
        nrow=ncluster)) + matrix(apply(var2est*matrix(rep(wgt1_u, 16),
        nrow = ncluster), 2, sum), nrow=4)
    }

# End the section for a two-stage sample

  } else {

# Begin the section for a single-stage sample

# Calculate the number of response values

    nresp <- length(response)

# Create indicator variables for required cells and margins

    Ind1 <- (response == response_levels[1])*(stressor == stressor_levels[1])
    Ind2 <- (stressor == stressor_levels[1])
    Ind3 <- (response == response_levels[1])*(stressor == stressor_levels[2])
    Ind4 <- (stressor == stressor_levels[2])

# Calculate the matrix of weighted indicator variables

    rm <- cbind(Ind1, Ind2, Ind3, Ind4) * wgt

# Calculate the population correction factor

    pcfactor <- ifelse(pcfactor_ind, (fpcsize - nresp)/fpcsize, 1)

# Adjust the variance-covariance estimator for small sample size

    if(vartype == "Local" && nresp < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling variance estimator was used.\n"
      if(stratum_ind) {
        warn <- paste("There are less than four response values in stratum ", stratum_level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe category proportion estimates.\n", sep="")
        warn_df <- rbind(warn_df, data.frame(func=I(fname),
          subpoptype=warn_vec[1], subpop=warn_vec[2],
          indicator=warn_vec[3], stratum=I(stratum_level), warning=I(warn),
          action=I(act)))
      } else {
        warn <- "\nThere are less than four response values, the simple random sampling variance \nestimator was used to calculate variance of the category proportion \nestimates.\n"
        warn_df <- rbind(warn_df, data.frame(func=I(fname),
          subpoptype=warn_vec[1], subpop=warn_vec[2],
          indicator=warn_vec[3], stratum=NA, warning=I(warn),
          action=I(act)))
      }
      vartype <- "SRS"
    }

# Calculate the variance-covariance estimate for the cell and marginal totals

    if (vartype == "Local") {
      wgt_1st <- localmean_weight(x=x, y=y, prb=1/wgt)
      varest <- pcfactor*localmean_cov(rm, wgt_1st)
    } else {
      varest <- pcfactor*nresp*var(rm)
    }

# End the section for a single-stage sample

  }

# Return the variance-covariance estimate, the warning message indicator, and
# the warn_df data frame

  list(varest=varest, warn_ind=warn_ind, warn_df=warn_df)
}
