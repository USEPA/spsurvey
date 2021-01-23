################################################################################
# Function: change_est
# Programmer: Tom Kincaid
# Date: July 29, 2020
#'
#' Estimate Change between Two Surveys
#'
#' This function estimates change between two probability surveys.  The function
#' can accommodate both categorical and continuous response variables. For a
#' categorical response variable, change is estimated by the difference in
#' category estimates for the two surveys, where a category estimate is the
#' estimated proportion of values in a category.  Note that a separate change
#' estimate is calculated for each category of a categorical response variable.
#' For a continuous response variable, change can be estimated for the mean, the
#' median, or for both the mean and median. For a continuous response variable
#' using the mean, change is estimated by the difference in estimated mean
#' values for the two surveys.  For change estimates using the median, the first
#' step is to calculate an estimate of the median for the first survey. The
#' estimated median from the first survey is then used to define two categories:
#' (1) values that are less than or equal to the estimated median and (2) values
#' that are greater than the estimated median.  Once the categories are defined,
#' change analysis for the median is identical to change analysis for a
#' categorical variable, i.e., change is estimated by the difference in category
#' estimates for the two surveys. In addition to change estimates, the standard
#' error of the change estimates and confidence bounds are calculated.  Variance
#' estimates are calculated using either the local mean variance estimator or
#' the simple random sampling (SRS) variance estimator.  The choice of variance
#' estimator is subject to user control. The local mean variance estimator
#' requires the x-coordinate and y-coordinate of each site.  The SRS variance
#' estimator uses the independent random sample approximation to calculate joint
#' inclusion probabilities.  Confidence bounds are calculated using a Normal
#' distribution multiplier.  The function can accommodate a stratified sample.
#' For a stratified sample, separate estimates and standard errors are
#' calculated for each stratum, which are used to produce estimates and standard
#' errors for all strata combined. Strata that contain a single value are
#' removed.  For a stratified sample, when either the size of the resource or
#' the sum of the size-weights of the resource is provided for each stratum,
#' those values are used as stratum weights for calculating the estimates and
#' standard errors for all strata combined.  For a stratified sample when
#' neither the size of the resource nor the sum of the size-weights of the
#' resource is provided for each stratum, estimated values are used as stratum
#' weights for calculating the estimates and standard errors for all strata
#' combined.  The function can accommodate single-stage and two-stage samples
#' for both stratified and unstratified sampling designs.  It is assumed that
#' both surveys employ the same type of survey design.  Finite population and
#' continuous population correction factors can be utilized in variance
#' estimation.  The function checks for compatibility of input values and
#' removes missing values.
#'
#' @param resp_ind A character value that indicates the type of response
#'   variable, where "cat" indicates a categorical variable and "cont" indicates
#'   a continuous variable.
#'
#' @param survey_names Character vector of length two that provides the survey
#'   names contained in the survey ID variable in the dframe data frame.  The
#'   two values in the vector identify the first survey and second survey,
#'   respectively.
#'
#' @param changesum List containing estimates, which is composed of three data
#'   frames: catsum, contsum_mean, and contsum_median.
#'
#' @param dframe Data frame containing survey design variables, response
#'   variables, and subpopulation (domain) variables for both surveys.
#'
#' @param survey_1 Logical vector that identifies survey one sites in the dframe
#'   data frame.
#'
#' @param survey_2 Logical vector that identifies survey two sites in the dframe
#'   data frame.
#'
#' @param itype Character value that identifies the factor variable containing
#'   subpopulation (domain) values.
#'
#' @param isubpop Character value that identifies a level of the subpopulation
#'   variable.
#'
#' @param ivar Character value that identifies the response variable.
#'
#' @param lev_ivar Character vector that provides levels of a categorical
#'   response variable.
#'
#' @param nlev_ivar Numeric value that provides the number of levels of a
#'   categorical response variable.
#'
#' @param design_1 Object of class survey.design that specifies the complex
#'   survey design for survey one.
#'
#' @param design_2 Object of class survey.design that specifies the complex
#'   survey design for survey two
#'
#' @param design_names Character vector that provides names of survey design
#'   variables in the design argument.
#'
#' @param repeat_1 Logical vector that identifies repeat visit sites for
#'  survey one.
#'
#' @param repeat_2 Logical vector that identifies repeat visit sites for
#'  survey two.
#'
#' @param siteID Character value providing name of the site ID variable in
#'   the dframe data frame.
#'
#' @param revisitwgt Logical value that indicates whether each repeat visit
#'  site has the same survey design weight in the two surveys, where TRUE = the
#'  weight for each repeat visit site is the same and FALSE = the weight for
#'  each repeat visit site is not the same.  When this argument is FALSE, all
#'  of the repeat visit sites are assigned equal weights when calculating the
#'  covariance component of the change estimate standard error.  The default is
#'  FALSE.
#'
#' @param test Character string or character vector providing the location
#'   measure(s) to use for change estimation for continuous variables.  The
#'   choices are "mean", "median", or c("mean", "median").
#'
#' @param var_nondetect Character value that identifies the name of a logical
#'   variable in the dframe data frame specifying the presence of not detected
#'   (nondetect) values for a continuous response variable.
#'
#' @param popcorrect Logical value that indicates whether the finite population
#'   correction factor is used during variance estimation.
#'
#' @param vartype The choice of variance estimator, where "Local" = local mean
#'   estimator and "SRS" = SRS estimator.
#'
#' @param conf Numeric value for the confidence level.
#'
#' @param mult Numeric value that provides the Normal distribution confidence
#'   bound multiplier.
#'
#' @param warn_ind  Logical value that indicates whether warning messages were
#'  generated, where TRUE = warning messages were generated and FALSE = warning
#'  messages were not generated.
#'
#' @param warn_df Data frame for storing warning messages.
#'
#' @param warn_vec Vector that contains names of the population type, the
#'  subpopulation, and an indicator.
#'
#' @return A list composed of the following objects:
#'   \describe{
#'     \item{\code{results}}{if resp_ind equals "cat", a data frame named
#'       catsum containing change estimates for categories; if resp_ind equals
#'       "cont", two data frames named contsum_mean and contsum_median
#'       containing mean and median change estimates, respectively, as specified
#'       by the argument named test.}
#'     \item{\code{warn_ind}}{logical variable that indicates whether warning
#'       messages were generated}
#'     \item{\code{warn_df}}{data frame for storing warning messages}
#'   }
#'
#'
#' @section Other Functions Required:
#'  \describe{
#'     \item{\code{\link{category_est}}}{calculate category proportion and total
#'       estimates}
#'    \item{\code{\link{changevar_mean}}}{calculate the covariance or
#'      correlation estimate of the estimated change in means between two
#'      probability surveys}
#'    \item{\code{\link{changevar_prop}}}{calculate covariance or correlation
#'      estimates of the estimated change in class proportion estimates between
#'     two probability surveys}
#'    \item{\code{\link{changevar_total}}}{calculate covariance or correlation
#'      estimates of the estimated change in class total estimates between two
#'      probability surveys}
#'     \item{\code{\link{percentile_est}}}{calculates percentile estimates}
#'  }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @seealso
#'   \code{\link{category_est}}
#'   \code{\link{changevar_mean}}
#'   \code{\link{changevar_prop}}
#'   \code{\link{changevar_total}}
#'   \code{\link{confint}}
#'   \code{\link{percentile_est}}
#'   \code{\link{SE}}
#'   \code{\link{svyby}}
#'   \code{\link{svymean}}
#'   \code{\link{svytotal}}
#'
#' @keywords survey
#'
#' @export
################################################################################

change_est <- function(resp_ind, survey_names, changesum, dframe, survey_1,
  survey_2, itype, isubpop, ivar, lev_ivar, nlev_ivar, design_1, design_2,
  design_names, repeat_1, repeat_2, siteID, revisitwgt, test, var_nondetect,
  popcorrect, vartype, conf, mult, warn_ind, warn_df, warn_vec) {

# Assign a value to the function name variable

  fname <- "change_est"

# Create a subset of the dframe data frame for each survey

  dframe_1 <- subset(dframe, survey_1)
  dframe_2 <- subset(dframe, survey_2)

# Create logical vectors for subsetting the data

  subpop_1 <- dframe_1[, itype] %in% isubpop
  subpop_2 <- dframe_2[, itype] %in% isubpop

# Subset the dframe_1 and dframe_2 objects

  dframe_1 <- subset(dframe_1, subpop_1)
  dframe_2 <- subset(dframe_2, subpop_2)

# Subset the design_1 and design_2 objects

  design_1 <- subset(design_1, subpop_1)
  design_2 <- subset(design_2, subpop_2)

  # If the surveys include repeat visit sites, ensure that the subpopulation
  # inludes the same number of repeat visit sites in each survey

  repeat_1 <- repeat_1[subpop_1]
  repeat_2 <- repeat_2[subpop_2]
  if(sum(repeat_1) != sum(repeat_2)) {
    ind_1 <- !(dframe_1[repeat_1, siteID] %in%
        dframe_2[repeat_2, siteID])
    ind_2 <- !(dframe_2[repeat_2, siteID] %in%
        dframe_1[repeat_1, siteID])
    if(sum(ind_1) > 0) {
      repeat_1[ind_1] <- FALSE
      warn_ind <- TRUE
      temp_str <- vecprint(dframe[repeat_1, siteID][ind_1])
      warn <- paste("The following repeat visit site IDs for subpopulation ", isubpop, "\nof population type ", itype, " for indicator ", ivar, "\nin survey one did not have analogous site IDs present in survey two:\n", temp_str, sep="")
      act <- "The listed repeated visit sites were not used for covariance estimation.\n"
      warn_df <- rbind(warn_df, data.frame(func=I(fname), subpoptype=I(itype),
        subpop=I(isubpop), indicator=I(ivar),stratum=NA,  warning=I(warn),
        action=I(act)))
    }
    if(sum(ind_2) > 0) {
      repeat_2[ind_2] <- FALSE
      warn_ind <- TRUE
      temp_str <- vecprint(dframe[repeat_2, siteID][ind_2])
      warn <- paste("The following repeated visit site IDs for subpopulation ", isubpop[isubpop], "\nof population type ", itype, " for indicator ", ivar, "\nin survey two did not have analogous site IDs present in survey one:\n", temp_str, sep="")
      act <- "The listed repeated visit sites were not used for covariance estimation.\n"
      warn_df <- rbind(warn_df, data.frame(func=I(fname), subpoptype=I(itype),
        subpop=I(isubpop), indicator=I(ivar), stratum=NA,  warning=I(warn),
        action=I(act)))
    }
  }

#
# Begin the section for a categorical variable
#

  if(resp_ind == "cat") {

# Calculate estimates for all sites from survey one

    dframe_1[, itype] <- droplevels(dframe_1[, itype])
    temp <- category_est(NULL, dframe_1, itype, isubpop, 1, ivar, lev_ivar,
      nlev_ivar, design_1, design_names, popcorrect, vartype, conf, mult,
      warn_ind, warn_df)
    temp_1 <- droplevels(subset(temp$catsum, Category != "Total"))
    warn_ind <- temp$warn_ind
    warn_df <- temp$warn_df
    tw_1 <- sum(weights(design_1))

# Calculate estimates for all sites from survey two

    dframe_2[, itype] <- droplevels(dframe_2[, itype])
    temp <- category_est(NULL, dframe_2, itype, isubpop, 1, ivar, lev_ivar,
      nlev_ivar, design_2, design_names, popcorrect, vartype, conf, mult,
      warn_ind, warn_df)
    temp_2 <- droplevels(subset(temp$catsum, Category != "Total"))
    warn_ind <- temp$warn_ind
    warn_df <- temp$warn_df
    tw_2 <- sum(weights(design_2))

# Merge results for the two surveys

    results <- merge(temp_1, temp_2, by="Category", suffix=c("_1", "_2"),
      all=TRUE, sort=FALSE)

# Calculate the change estimates

    results$DiffEst.P <- (results$Estimate.P_2 - results$Estimate.P_1)/100
    results$DiffEst.U <- results$Estimate.U_2 - results$Estimate.U_1

# Express the standard error estimates for the two surveys on the proportion
# scale

    results$StdError.P_1 <- results$StdError.P_1 / 100
    results$StdError.P_2 <- results$StdError.P_2 / 100

# Calculate standard error of the change estimates for surveys with no repeat
# visit sites

    if(sum(repeat_1) == 0) {

      results$StdError.P <- sqrt(results$StdError.P_1^2 +
          results$StdError.P_2^2)
      results$StdError.U <- sqrt(results$StdError.U_1^2 +
          results$StdError.U_2^2)
      results$LCB.P <- 100 * pmax(results$DiffEst.P - mult*results$StdError.P,
        -1)
      results$UCB.P <- 100 * pmin(results$DiffEst.P + mult*results$StdError.P,
        1)
      results$DiffEst.P <- 100 * results$DiffEst.P
      results$MarginofError.P <- 100 * (mult * results$StdError.P)
      results$MarginofError.P_1 <- 100 * (mult * results$StdError.P_1)
      results$MarginofError.P_2 <- 100 * (mult * results$StdError.P_2)
      results$StdError.P <- 100 * results$StdError.P
      results$StdError.P_1 <- 100 * results$StdError.P_1
      results$StdError.P_2 <- 100 * results$StdError.P_2
      results$MarginofError.U <- mult * results$StdError.U
      results$MarginofError.U_1 <- mult * results$StdError.U_1
      results$MarginofError.U_2 <- mult * results$StdError.U_2
      if("postStrata" %in% names(design_1)) {
        results$LCB.U <- pmax(results$DiffEst.U - mult*results$StdError.U,
          -tw_1)
        results$UCB.U <- pmin(results$DiffEst.U + mult*results$StdError.U,
          tw_2)
      } else {
        results$LCB.U <- results$DiffEst.U - mult*results$StdError.U
        results$UCB.U <- results$DiffEst.U + mult*results$StdError.U
      }

# Calculate standard error of the change estimates for surveys with repeat
# visit sites

    } else {

# Subset the dframe_1 and dframe_2 objects to retain repeat visit sites

      dframe_1 <- subset(dframe_1, repeat_1)
      dframe_2 <- subset(dframe_2, repeat_2)

# Subset the design_1 and design_2 objects to retain repeat visit sites

      design_1 <- subset(design_1, repeat_1)
      design_2 <- subset(design_2, repeat_2)

# Assign values for the categorical variables

      catvar_1 <- dframe_1[, ivar]
      catvar_2 <- dframe_2[, ivar]

# Assign values for survey design variables using the survey one design object

      if("postStrata" %in% names(design_1)) {
      tempdf <- design_1$variables[subpop_1,][repeat_1,]
      } else {
      tempdf <- design_1$variables
      }
      for(i in names(design_names)) {
        if(is.null(design_names[[i]])) {
          eval(parse(text=paste0(i, " <- NULL")))
        } else {
          eval(parse(text=paste0(i, " <- tempdf[, \"", design_names[[i]], "\"]")))
        }
      }

# Assign a value to the indicator variable for a two-stage sample

      cluster_ind <- !is.null(clusterID)

# Assign values to weight variables

      if(revisitwgt) {
        if(cluster_ind) {
          wgt1 <- tempdf$wgt1
          wgt2 <- tempdf$wgt2
        } else {
          wgt <- tempdf$wgt
        }
      } else {
        if(cluster_ind) {
          wgt1 <- rep(1, length(catvar_1))
          wgt2 <- rep(1, length(catvar_1))
        } else {
          wgt <- rep(1, length(catvar_1))
        }
      }

# Assign a logical value to the indicator variable for a stratified sample

      stratum_ind <- !is.null(stratumID)

# If the sample is stratified, convert stratum to a factor, determine stratum
# levels, and calculate number of strata

      if(stratum_ind) {
        stratum <- factor(stratumID)
        stratum_levels <- levels(stratum)
        nstrata <- length(stratum_levels)
      }

# Remove missing values

      indx <- 1:sum(repeat_1)
      indx[is.na(catvar_1) | is.na(catvar_2)] <- 0
      catvar_1 <- catvar_1[indx]
      catvar_2 <- catvar_2[indx]
      if(stratum_ind) {
        if(cluster_ind) {
          wgt2 <- wgt2[indx]
          xcoord <- xcoord[indx]
          ycoord <- ycoord[indx]
          stratum <- stratum[indx]
          clusterID <- clusterID[indx]
          wgt1 <- wgt1[indx]
          xcoord1 <- xcoord1[indx]
          ycoord1 <- ycoord1[indx]
          if(popcorrect) {
            Ncluster <- Ncluster[indx]
            stage1size <- stage1size[indx]
          }
        } else {
          wgt <- wgt[indx]
          xcoord <- xcoord[indx]
          ycoord <- ycoord[indx]
          stratum <- stratum[indx]
          if(popcorrect) {
            fpcsize <- fpcsize[indx]
          }
        }
      } else {
        if(cluster_ind) {
          wgt2 <- wgt2[indx]
          xcoord <- xcoord[indx]
          ycoord <- ycoord[indx]
          clusterID <- clusterID[indx]
          wgt1 <- wgt1[indx]
          xcoord1 <- xcoord1[indx]
          ycoord1 <- ycoord1[indx]
          if(popcorrect) {
            Ncluster <- Ncluster[indx]
            stage1size <- stage1size[indx]
          }
        } else {
          wgt <- wgt[indx]
          xcoord <- xcoord[indx]
          ycoord <- ycoord[indx]
          if(popcorrect) {
            fpcsize <- fpcsize[indx]
          }
        }
      }

# For a stratified sample, remove strata that contain a single site

      if(stratum_ind) {
        ind <- FALSE
        for(i in 1:nstrata) {
          tst <- stratum == stratum_levels[i]
          if(sum(tst) == 1) {
            warn_ind <- TRUE
            warn <- paste0("The stratum named \"", stratum_levels[i], "\" contains a single value and was removed from the analysis.\n")
            act <- "Stratum was not used for standard error estimation.\n"
            warn_df <- rbind(warn_df, data.frame(func=I(fname), subpoptype=NA,
              subpop=NA, indicator=NA, stratum=NA, warning=I(warn),
              action=I(act)))
            dframe <- dframe[!tst,]
            ind <- TRUE
            catvar_1 <- catvar_1[!tst]
            catvar_2 <- catvar_2[!tst]
            if(vartype == "Local") {
              xcoord <- xcoord[!tst]
              ycoord <- ycoord[!tst]
            }
            stratum <- stratum[!tst]
            if(cluster_ind) {
              clusterID <- clusterID[!tst]
              wgt1 <- wgt1[!tst]
              wgt2 <- wgt2[!tst]
              if(vartype == "Local") {
                xcoord1 <- xcoord1[!tst]
                ycoord1 <- ycoord1[!tst]
              }
              if(popcorrect) {
                Ncluster <- Ncluster[!tst]
                stage1size <- stage1size[!tst]
              }
            } else {
              wgt <- wgt[!tst]
              if(popcorrect) {
                fpcsize <- fpcsize[!tst]
              }
            }
            ind <- TRUE

          }
        }
        if(ind) {
          stratum <- factor(stratum)
          stratum_levels <- levels(stratum)
          nstrata <- length(stratum_levels)
        }
      }

# For a stratified sample, check whether the number of strata is one

      if(stratum_ind) {
        if(nstrata == 1) {
          warn_ind <- TRUE
          warn <- "Only a single stratum was available for the analysis.\n"
          act <- "An unstratified data analysis was used.\n"
          warn_df <- rbind(warn_df, data.frame(func=I(fname),
            subpoptype=warn_vec[1], subpop=warn_vec[2], indicator=warn_vec[3],
            stratum=NA, warning=I(warn), action=I(act)))
          stratum_ind <- FALSE
        }
      }

# Assign levels of the categorical variable

      catvar_levels <- levels(results$Category)
      nlevels <- length(catvar_levels)

# Calculate population size values

      if(stratum_ind) {
        if(cluster_ind) {
          popsize_hat <- tapply(wgt1 * wgt2, stratum, sum)
          sum_popsize_hat <- sum(wgt1 * wgt2)
        } else {
          popsize_hat <- tapply(wgt, stratum, sum)
          sum_popsize_hat <- sum(wgt)
        }
      } else {
        if(cluster_ind) {
          popsize_hat <- sum(wgt1 * wgt2)
        } else {
          popsize_hat <- sum(wgt)
        }
      }

# Branch to handle stratified and unstratified data

      if(stratum_ind) {

# Begin the section for stratified data

# Create the vectors of covariance or correlation estimates for all strata
# combined

        rslt_P <- rep(NA, nlevels)
        rslt_U <- rep(NA, nlevels)

# Check whether the vectors of categorical variable values for revisit sites
# are empty or contain a single value

        if(length(catvar_1) <= 1) {
          warn_ind <- TRUE
          act <- "Covariance among the revisited sites was not included in calculation of \nthe standard error estimate.\n"
          warn <- paste("The number of nonmissing repeat visit sites was less than two in one of the \nsurveys.\n", sep="")
          warn_df <- rbind(warn_df, data.frame(func=I(fname),
            subpoptype=warn_vec[1], subpop=warn_vec[2], indicator=warn_vec[3],
            stratum=NA, warning=I(warn), action=I(act)))

# Begin section for nonempty vectors of categorical variable values for revisit
# sites

        } else {

# Begin the loop for individual strata

          for(i in 1:nstrata) {

# Check whether the vectors of categorical variable values for revisit sites
# are empty or contain a single value for a stratum

            stratum_i <- stratum == stratum_levels[i]
            if((sum(!is.na(catvar_1[stratum_i])) <= 1) |
                (sum(!is.na(catvar_2[stratum_i])) <= 1)) {
              warn_ind <- TRUE
              act <- "Due to insufficient number of sites, the stratum was not included in \ncalculation of covariance among the revisited sites.\n"
              warn <- paste("The number of nonmissing repeat visit sites  in one of the surveys was less \nthan two for stratum \"", stratum_levels[i], "\".\n", sep="")
              warn_df <- rbind(warn_df, data.frame(func=I(fname),
                subpoptype=warn_vec[1], subpop=warn_vec[2],
                indicator=warn_vec[3], stratum=I(stratum_levels[i]),
                warning=I(warn), action=I(act)))

# Begin section for nonempty vectors of categorical variable values for revisit
# sites for a stratum
#
            } else {

# Calculate proportion estimates

              z1 <- factor(catvar_1[stratum_i], levels=catvar_levels)
              z2 <- factor(catvar_2[stratum_i], levels=catvar_levels)
              m <- length(catvar_levels)
              prop1 <- as.vector(svymean(make.formula(ivar),
                design = subset(design_1, stratum_i), na.rm = TRUE))
              prop2 <- as.vector(svymean(make.formula(ivar),
                design = subset(design_2, stratum_i), na.rm = TRUE))

# Calculate covariance or correlation estimates

              if(cluster_ind) {
                temp <- changevar_prop(catvar_levels, z1, z2, wgt2[stratum_i],
                  xcoord[stratum_i], ycoord[stratum_i], revisitwgt, prop1,
                  prop2, stratum_ind, stratum_levels[i], cluster_ind,
                  clusterID[stratum_i], wgt1[stratum_i], xcoord1[stratum_i],
                  ycoord1[stratum_i], popcorrect, NULL, Ncluster[stratum_i],
                  stage1size[stratum_i], vartype, warn_ind, warn_df, warn_vec)
              } else {
                temp <- changevar_prop(catvar_levels, z1, z2, wgt[stratum_i],
                  xcoord[stratum_i], ycoord[stratum_i], revisitwgt, prop1,
                  prop2, stratum_ind, stratum_levels[i], cluster_ind,
                  pcfactor_ind = popcorrect, fpcsize = fpcsize[stratum_i],
                  vartype = vartype, warn_ind = warn_ind, warn_df = warn_df,
                  warn_vec = warn_vec)
              }
              correst <- temp$rslt
              warn_ind <- temp$warn_ind
              warn_df <- temp$warn_df

# Add estimates to the vector for all strata combined

              rslt_P[!is.na(correst)] <- rslt_P[!is.na(correst)] +
                (popsize_hat[i]/sum_popsize_hat)*correst[!is.na(correst)]

# Estimate the size of each category

              size1 <- popsize_hat[i] * prop1
              size2 <- popsize_hat[i] * prop2

# Calculate covariance or correlation estimates

              if(cluster_ind) {
                temp <- changevar_total(catvar_levels, z1, z2, wgt2[stratum_i],
                  xcoord[stratum_i], ycoord[stratum_i], revisitwgt, size1,
                  size2, stratum_ind, stratum_levels[i], cluster_ind,
                  clusterID[stratum_i], wgt1[stratum_i], xcoord1[stratum_i],
                  ycoord1[stratum_i], popcorrect, NULL, Ncluster[stratum_i],
                  stage1size[stratum_i], vartype, warn_ind, warn_df, warn_vec)
              } else {
                temp <- changevar_total(catvar_levels, z1, z2, wgt[stratum_i],
                  xcoord[stratum_i], ycoord[stratum_i], revisitwgt, size1,
                  size2, stratum_ind, stratum_levels[i], cluster_ind,
                  pcfactor_ind = popcorrect, fpcsize = fpcsize[stratum_i],
                  vartype = vartype, warn_ind = warn_ind, warn_df = warn_df,
                  warn_vec = warn_vec)
              }
              correst <- temp$rslt
              warn_ind <- temp$warn_ind
              warn_df <- temp$warn_df

# Add estimates to the vector for all strata combined

              rslt_U[!is.na(correst)] <- rslt_U[!is.na(correst)] +
                correst[!is.na(correst)]

# End the section for nonempty vectors of categorical variable values for
# revisit sites for a stratum

            }

# End the loop for individual strata

          }

# End the section for nonempty vectors of categorical variable values for
# revisit sites

        }

# End the section for stratified data

      } else {

# Begin the section for unstratified data

# Check whether the vectors of categorical variable values for revisit sites
# are empty or contain a single value

        if(length(catvar_1) <= 1) {
          rslt_P <- rep(NA, nlevels)
          rslt_U <- rep(NA, nlevels)
          warn_ind <- TRUE
          act <- "Covariance among the revisited sites was not included in calculation of \nthe standard error estimate.\n"
          warn <- paste("The number of nonmissing repeat visit sites was less than two in one of the \nsurveys.\n", sep="")
          warn_df <- rbind(warn_df, data.frame(func=I(fname),
            subpoptype=warn_vec[1], subpop=warn_vec[2], indicator=warn_vec[3],
            stratum=NA, warning=I(warn), action=I(act)))

# Begin section for nonempty vectors of categorical variable values for revisit
# sites

        } else {

# Calculate proportion estimates

          z1 <- factor(catvar_1, levels=catvar_levels)
          z2 <- factor(catvar_2, levels=catvar_levels)
          prop1 <- as.vector(svymean(make.formula(ivar),
            design = design_1, na.rm = TRUE))
          prop2 <- as.vector(svymean(make.formula(ivar),
            design = design_2, na.rm = TRUE))

# Calculate covariance or correlation estimates

          if(cluster_ind) {
            temp <- changevar_prop(catvar_levels, z1, z2, wgt2, xcoord, ycoord,
              revisitwgt, prop1, prop2, stratum_ind, NULL, cluster_ind,
              clusterID, wgt1, xcoord1, ycoord1, popcorrect, NULL, Ncluster,
              stage1size, vartype, warn_ind, warn_df, warn_vec)
          } else {
            temp <- changevar_prop(catvar_levels, z1, z2, wgt, xcoord, ycoord,
              revisitwgt, prop1, prop2, stratum_ind, NULL, cluster_ind,
              pcfactor_ind = popcorrect, fpcsize = fpcsize, vartype = vartype,
              warn_ind = warn_ind, warn_df = warn_df, warn_vec = warn_vec)
          }
          rslt_P <- temp$rslt
          warn_ind <- temp$warn_ind
          warn_df <- temp$warn_df

# Estimate the size of each category
#
          size1 <- popsize_hat*prop1
          size2 <- popsize_hat*prop2

# Calculate covariance or correlation estimates

          if(cluster_ind) {
            temp <- changevar_total(catvar_levels, z1, z2, wgt2, xcoord, ycoord,
              revisitwgt, size1, size2, stratum_ind, NULL, cluster_ind,
              clusterID, wgt1, xcoord1, ycoord1, popcorrect, NULL, Ncluster,
              stage1size, vartype, warn_ind, warn_df, warn_vec)
          } else {
            temp <- changevar_total(catvar_levels, z1, z2, wgt, xcoord, ycoord,
              revisitwgt, size1, size2, stratum_ind, NULL, cluster_ind,
              pcfactor_ind = popcorrect, fpcsize = fpcsize,  vartype = vartype,
              warn_ind = warn_ind, warn_df = warn_df, warn_vec = warn_vec)
          }
          rslt_U <- temp$rslt
          warn_ind <- temp$warn_ind
          warn_df <- temp$warn_df

# End the section for nonempty vectors of categorical variable values for
# revisit sites

        }

# End the section for unstratified data

      }

# Calculate standard errors

      results$StdError.P <- rep(NA, nlevels)
      results$StdError.U <- rep(NA, nlevels)
      ind <- is.na(rslt_P)
      results$StdError.P[ind] <- sqrt(results$StdError.P_1[ind]^2 +
          results$StdError.P_2[ind]^2)
      results$StdError.U[ind] <- sqrt(results$StdError.U_1[ind]^2 +
          results$StdError.U_2[ind]^2)
      if(any(!ind)) {
        tw_1r <- sum(weights(design_1))
        tw_2r <- sum(weights(design_2))
        if(revisitwgt) {
          temp <- results$StdError.P_1^2 + results$StdError.P_2^2 -
            ((2*tw_1r*tw_2r)/(tw_1*tw_2))*rslt_P
          ind <- !is.na(rslt_P) & temp <= 0
          results$StdError.P[ind] <- sqrt(results$StdError.P_1[ind]^2 +
              results$StdError.P_2[ind]^2)
          ind <- !is.na(rslt_P) & temp > 0
          results$StdError.P[ind] <- sqrt(temp[ind])
          temp <- results$StdError.U_1^2 + results$StdError.U_2^2 - 2*rslt_U
          ind <- !is.na(rslt_U) & temp <= 0
          results$StdError.U[ind] <- sqrt(results$StdError.U_1[ind]^2 +
              results$StdError.U_2[ind]^2)
          ind <- !is.na(rslt_U) & temp > 0
          results$StdError.U[ind] <- sqrt(temp[ind])
        } else {
          se_1_p <- rep(NA, nlevels)
          se_1_u <- rep(NA, nlevels)
          temp <- category_est(NULL, dframe_1, itype, isubpop, 1, ivar,
            lev_ivar, nlev_ivar, design_1, design_names, popcorrect, vartype,
            conf, mult, warn_ind, warn_df)
          temp$results <- droplevels(subset(temp$catsum, Category != "Total"))
          ind <- match(temp$results$Category, catvar_levels, nomatch=0)
          se_1_p[ind] <- temp$results$StdError.P / 100
          se_1_u[ind] <- temp$results$StdError.U
          se_2_p <- rep(NA, nlevels)
          se_2_u <- rep(NA, nlevels)
          temp <- category_est(NULL, dframe_2, itype, isubpop, 1, ivar,
            lev_ivar, nlev_ivar, design_2, design_names, popcorrect, vartype,
            conf, mult, warn_ind, warn_df)
          temp$results <- droplevels(subset(temp$catsum, Category != "Total"))
          ind <- match(temp$results$Category, catvar_levels, nomatch=0)
          se_2_p[ind] <- temp$results$StdError.P / 100
          se_2_u[ind] <- temp$results$StdError.U
          covest <- rslt_P * se_1_p * se_2_p
          temp <- results$StdError.P_1^2 + results$StdError.P_2^2 -
            ((2*tw_1r*tw_2r)/(tw_1*tw_2))*covest
          ind <- !is.na(rslt_P) & temp <= 0
          results$StdError.P[ind] <- sqrt(results$StdError.P_1[ind]^2 +
              results$StdError.P_2[ind]^2)
          ind <- !is.na(rslt_P) & temp > 0
          results$StdError.P[ind] <- sqrt(temp[ind])
          covest <- rslt_U * se_1_u * se_2_u
          temp <- results$StdError.U_1^2 + results$StdError.U_2^2 -2*covest
          ind <- !is.na(rslt_U) & temp <= 0
          results$StdError.U[ind] <- sqrt(results$StdError.U_1[ind]^2 +
              results$StdError.U_2[ind]^2)
          ind <- !is.na(rslt_U) & temp > 0
          results$StdError.U[ind] <- sqrt(temp[ind])
        }
      }

# Calculate margins of error and confidence bounds

      results$LCB.P <- 100 * pmax(results$DiffEst.P -
          mult*results$StdError.P, -1)
      results$UCB.P <- 100 * pmin(results$DiffEst.P +
          mult*results$StdError.P, 1)
      results$DiffEst.P <- 100 * results$DiffEst.P
      results$MarginofError.P <- 100 * (mult * results$StdError.P)
      results$MarginofError.P_1 <- 100 * (mult * results$StdError.P_1)
      results$MarginofError.P_2 <- 100 * (mult * results$StdError.P_2)
      results$StdError.P <- 100 * results$StdError.P
      results$StdError.P_1 <- 100 * results$StdError.P_1
      results$StdError.P_2 <- 100 * results$StdError.P_2
      results$MarginofError.U <- mult * results$StdError.U
      results$MarginofError.U_1 <- mult * results$StdError.U_1
      results$MarginofError.U_2 <- mult * results$StdError.U_2
      if("postStrata" %in% names(design_1)) {
        results$LCB.U <- pmax(results$DiffEst.U - mult*results$StdError.U,
          -tw_1)
        results$UCB.U <- pmin(results$DiffEst.U + mult*results$StdError.U,
          tw_2)
      } else {
        results$LCB.U <- results$DiffEst.U - mult*results$StdError.U
        results$UCB.U <- results$DiffEst.U + mult*results$StdError.U
      }

# End the section for surveys with repeat visit sites

    }

# Add estimates to the catsum data frame

  changesum$catsum <- rbind(changesum$catsum, cbind(
    Survey_1 = survey_names[1],
    Survey_2 = survey_names[2],
    subset(results, select = c(2:4, 1, 30, 32, 36, 34:35, 31, 33, 37:39, 5:15,
                               19:29))))

#
# End the section for a categorical variable
#

  } else if(resp_ind == "cont") {

#
# Begin the section for a continuous variable
#

# Calculate estimate for all sites from survey one

    pctest_1 <- percentile_est(NULL, dframe_1, itype, isubpop, 1, ivar,
      design_1, design_names, var_nondetect, popcorrect, vartype, conf, mult,
      pctval = c(50), warn_ind, warn_df)

# Calculate estimate for all sites from survey two

    pctest_2 <- percentile_est(NULL, dframe_2, itype, isubpop, 1, ivar,
      design_2, design_names, var_nondetect, popcorrect, vartype, conf, mult,
      pctval = c(50), warn_ind, warn_df)

#
# Begin the section for a continuous variable using the mean
#

    if("mean" %in% test) {

# As necessary, store original values for the dframe_1 and dframe_2 data
# frames and the design_1 and design_2 objects

      if("median" %in% test) {
        dframe_1_org <- dframe_1
        dframe_2_org <- dframe_2
        design_1_org <- design_1
        design_2_org <- design_2
      }

# Using the pctest_1 data frame, extract the mean estimate for survey one

      temp_cont_1 <- subset(pctest_1$pctsum, Statistic == "Mean")
      warn_ind <- pctest_1$warn_ind
      warn_df <- pctest_1$warn_df
      tw_1 <- sum(weights(design_1))

# Using the pctest_2 data frame, extract the mean estimate for survey two

      temp_cont_2 <- subset(pctest_2$pctsum, Statistic == "Mean")
      warn_ind <- pctest_2$warn_ind
      warn_df <- pctest_2$warn_df
      tw_2 <- sum(weights(design_2))

# Merge results for the two surveys

      results <- merge(temp_cont_1, temp_cont_2, by = "Statistic",
        suffix=c("_1", "_2"))

# Calculate the change estimate

      results$DiffEst <- results$Estimate_2 - results$Estimate_1

# Calculate the confidence bound multiplier

      mult <- qnorm(0.5 + (conf/100)/2)

#
# Calculate standard error of the change estimate
#

# Section for surveys with no repeat visit sites

      if(sum(repeat_1) == 0) {
        results$StdError <- sqrt(results$StdError_1^2 + results$StdError_2^2)
        results$MarginofError <- mult * results$StdError
        results$LCB <- results$DiffEst - mult*results$StdError
        results$UCB <- results$DiffEst + mult*results$StdError


# Section for surveys with repeat visit sites

      } else {

# Subset the dframe_1 and dframe_2 objects to retain repeat visit sites

        dframe_1 <- subset(dframe_1, repeat_1)
        dframe_2 <- subset(dframe_2, repeat_2)

# Subset the design_1 and design_2 objects to retain repeat visit sites

        design_1 <- subset(design_1, repeat_1)
        design_2 <- subset(design_2, repeat_2)

# Assign values for the continuous variables

        contvar_1 <- dframe_1[repeat_1, ivar]
        contvar_2 <- dframe_2[repeat_2, ivar]

# Assign values for survey design variables using the survey one design object

        tempdf <- design_1$variables
        for(i in names(design_names)) {
          if(is.null(design_names[[i]])) {
            eval(parse(text=paste0(i, " <- NULL")))
          } else {
            eval(parse(text=paste0(i, " <- tempdf[, \"", design_names[[i]], "\"]")))
          }
        }

# Assign a value to the indicator variable for a two-stage sample

        cluster_ind <- !is.null(clusterID)

# Assign values to weight variables

        if(revisitwgt) {
          if(cluster_ind) {
            wgt1 <- tempdf$wgt1
            wgt2 <- tempdf$wgt2
          } else {
            wgt <- tempdf$wgt
          }
        } else {
          if(cluster_ind) {
            wgt1 <- rep(1, length(contvar_1))
            wgt2 <- rep(1, length(contvar_1))
          } else {
            wgt <- rep(1, length(contvar_1))
          }
        }

# Assign a logical value to the indicator variable for a stratified sample

        stratum_ind <- !is.null(stratumID)

# If the sample is stratified, convert stratum to a factor, determine stratum
# levels, and calculate number of strata

        if(stratum_ind) {
          stratum <- factor(stratumID)
          stratum_levels <- levels(stratum)
          nstrata <- length(stratum_levels)
        }

# Remove missing values

        indx <- 1:sum(repeat_1)
        indx[is.na(contvar_1) | is.na(contvar_2)] <- 0
        contvar_1 <- contvar_1[indx]
        contvar_2 <- contvar_2[indx]
        if(stratum_ind) {
          if(cluster_ind) {
            wgt2 <- wgt2[indx]
            xcoord <- xcoord[indx]
            ycoord <- ycoord[indx]
            stratum <- stratum[indx]
            clusterID <- clusterID[indx]
            wgt1 <- wgt1[indx]
            xcoord1 <- xcoord1[indx]
            ycoord1 <- ycoord1[indx]
            if(popcorrect) {
              Ncluster <- Ncluster[indx]
              stage1size <- stage1size[indx]
            }
          } else {
            wgt <- wgt[indx]
            xcoord <- xcoord[indx]
            ycoord <- ycoord[indx]
            stratum <- stratum[indx]
            if(popcorrect) {
              fpcsize <- fpcsize[indx]
            }
          }
        } else {
          if(cluster_ind) {
            wgt2 <- wgt2[indx]
            xcoord <- xcoord[indx]
            ycoord <- ycoord[indx]
            clusterID <- clusterID[indx]
            wgt1 <- wgt1[indx]
            xcoord1 <- xcoord1[indx]
            ycoord1 <- ycoord1[indx]
            if(popcorrect) {
              Ncluster <- Ncluster[indx]
              stage1size <- stage1size[indx]
            }
          } else {
            wgt <- wgt[indx]
            xcoord <- xcoord[indx]
            ycoord <- ycoord[indx]
            if(popcorrect) {
              fpcsize <- fpcsize[indx]
            }
          }
        }

# For a stratified sample, remove strata that contain a single site

        if(stratum_ind) {
          ind <- FALSE
          for(i in 1:nstrata) {
            tst <- stratum == stratum_levels[i]
            if(sum(tst) == 1) {
              warn_ind <- TRUE
              warn <- paste0("The stratum named \"", stratum_levels[i], "\" contains a single value and was removed from the analysis.\n")
              act <- "Stratum was not used for standard error estimation.\n"
              warn_df <- rbind(warn_df, data.frame(func=I(fname), subpoptype=NA,
                subpop=NA, indicator=NA, stratum=NA, warning=I(warn),
                action=I(act)))
              dframe <- dframe[!tst,]
              ind <- TRUE
              contvar_1 <- contvar_1[!tst]
              contvar_2 <- contvar_2[!tst]
              if(vartype == "Local") {
                xcoord <- xcoord[!tst]
                ycoord <- ycoord[!tst]
              }
              stratum <- stratum[!tst]
              if(cluster_ind) {
                clusterID <- clusterID[!tst]
                wgt1 <- wgt1[!tst]
                wgt2 <- wgt2[!tst]
                if(vartype == "Local") {
                  xcoord1 <- xcoord1[!tst]
                  ycoord1 <- ycoord1[!tst]
                }
                if(popcorrect) {
                  Ncluster <- Ncluster[!tst]
                  stage1size <- stage1size[!tst]
                }
              } else {
                wgt <- wgt[!tst]
                if(popcorrect) {
                  fpcsize <- fpcsize[!tst]
                }
              }
              ind <- TRUE

            }
          }
          if(ind) {
            stratum <- factor(stratum)
            stratum_levels <- levels(stratum)
            nstrata <- length(stratum_levels)
          }
        }

# For a stratified sample, check whether the number of strata is one

        if(stratum_ind) {
          if(nstrata == 1) {
            warn_ind <- TRUE
            warn <- "Only a single stratum was available for the analysis.\n"
            act <- "An unstratified data analysis was used.\n"
            warn_df <- rbind(warn_df, data.frame(func=I(fname),
              subpoptype=warn_vec[1], subpop=warn_vec[2], indicator=warn_vec[3],
              stratum=NA, warning=I(warn), action=I(act)))
            stratum_ind <- FALSE
          }
        }

# Calculate population size values

        if(stratum_ind) {
          if(cluster_ind) {
            popsize_hat <- tapply(wgt1 * wgt2, stratum, sum)
            sum_popsize_hat <- sum(wgt1 * wgt2)
          } else {
            popsize_hat <- tapply(wgt, stratum, sum)
            sum_popsize_hat <- sum(wgt)
          }
        } else {
          if(cluster_ind) {
            popsize_hat <- sum(wgt1 * wgt2)
          } else {
            popsize_hat <- sum(wgt)
          }
        }

# Branch to handle stratified and unstratified data

        if(stratum_ind) {

# Begin the section for stratified data

# Create the object for covariance or correlation estimates for all strata
# combined

          rslt <- NA

# Check whether the vectors of continuous variable values for revisit sites
# are empty or contain a single value

          if(length(contvar_1) <= 1) {
            warn_ind <- TRUE
            act <- "Covariance among the revisited sites was not included in calculation of \nthe standard error estimate.\n"
            warn <- paste("The number of nonmissing repeat visit sites was less than two in one of the \nsurveys.\n", sep="")
            warn_df <- rbind(warn_df, data.frame(func=I(fname),
              subpoptype=warn_vec[1], subpop=warn_vec[2], indicator=warn_vec[3],
              stratum=NA, warning=I(warn), action=I(act)))

# Begin section for nonempty vectors of continuous variable values for revisit
# sites
          } else {

# Begin the loop for individual strata

          for(i in 1:nstrata) {

# Check whether the vectors of continuous variable values for revisit sites
# are empty or contain a single value for a stratum

            stratum_i <- stratum == stratum_levels[i]
            if(length(contvar_1[stratum_i]) <= 1) {
              warn_ind <- TRUE
              act <- "Due to insufficient number of sites, the stratum was not included in \ncalculation of covariance among the revisited sites.\n"
              warn <- paste("The number of nonmissing repeat visit sites  in one of the surveys was less \nthan two for stratum \"", stratum_levels[i], "\".\n", sep="")
              warn_df <- rbind(warn_df, data.frame(func=I(fname),
                subpoptype=warn_vec[1], subpop=warn_vec[2],
                indicator=warn_vec[3], stratum=I(stratum_levels[i]),
                warning=I(warn), action=I(act)))

# Begin section for nonempty vectors of continuous variables values for revisit
# sites for a stratum

            } else {

# Calculate mean estimates

              stratum_i <- stratum == stratum_levels[i]
              z1 <- contvar_1[stratum_i]
              z2 <- contvar_2[stratum_i]
              mean1 <- as.vector(svymean(make.formula(ivar),
                design = subset(design_1, stratum_i), na.rm = TRUE))
              mean2 <- as.vector(svymean(make.formula(ivar),
                design = subset(design_2, stratum_i), na.rm = TRUE))

# Calculate covariance or correlation estimates
              if(cluster_ind) {
                temp <- changevar_mean(z1, z2, wgt2[stratum_i],
                  xcoord[stratum_i], ycoord[stratum_i], revisitwgt, mean1,
                  mean2, stratum_ind, stratum_levels[i], cluster_ind,
                  clusterID[stratum_i], wgt1[stratum_i], xcoord1[stratum_i],
                  ycoord1[stratum_i], popcorrect, NULL, Ncluster[stratum_i],
                  stage1size[stratum_i], vartype, warn_ind, warn_df, warn_vec)
              } else {
                temp <- changevar_mean(z1, z2, wgt[stratum_i],
                  xcoord[stratum_i], ycoord[stratum_i], revisitwgt, mean1,
                  mean2, stratum_ind, stratum_levels[i], cluster_ind,
                  pcfactor_ind = popcorrect, fpcsize = fpcsize[stratum_i],
                  vartype = vartype, warn_ind = warn_ind, warn_df = warn_df,
                  warn_vec = warn_vec)
              }
              correst <- temp$rslt
              warn_ind <- temp$warn_ind
              warn_df <- temp$warn_df

# Add estimates to the object for all strata combined
              rslt[!is.na(correst)] <- rslt[!is.na(correst)] +
                  (popsize_hat[i]/sum_popsize_hat)*correst[!is.na(correst)]

# End the section for nonempty vectors of continuous variable values for
# revisit sites for a stratum
            }

# End the loop for individual strata

          }

# End the section for nonempty vectors of continuous variable values for
# revisit sites
        }

# End the section for stratified data

      } else {

# Begin the section for unstratified data

# Check whether the vectors of continuous variable values for revisit sites
# are empty or contain a single value

        if(length(contvar_1) <= 1) {
          rslt <- NA
          warn_ind <- TRUE
          act <- "Covariance among the revisited sites was not included in calculation of \nthe standard error estimate.\n"
          warn <- paste("The number of nonmissing repeat visit sites was less than two in one of the \nsurveys.\n", sep="")
          warn_df <- rbind(warn_df, data.frame(func=I(fname),
            subpoptype=warn_vec[1], subpop=warn_vec[2], indicator=warn_vec[3],
            stratum=NA, warning=I(warn), action=I(act)))

# Begin section for nonempty vectors of continuous variable values for revisit
# sites

        } else {

# Calculate mean estimates

          z1 <- contvar_1
          z2 <- contvar_2
          mean1 <- as.vector(svymean(make.formula(ivar), design = design_1,
            na.rm = TRUE))
          mean2 <- as.vector(svymean(make.formula(ivar), design = design_2,
            na.rm = TRUE))

# Calculate covariance or correlation estimates
          if(cluster_ind) {
            temp <- changevar_mean(z1, z2, wgt2, xcoord, ycoord, revisitwgt,
              mean1, mean2, stratum_ind, NULL, cluster_ind, clusterID, wgt1,
              xcoord1, ycoord1, popcorrect, NULL, Ncluster, stage1size, vartype,
              warn_ind, warn_df, warn_vec)
          } else {
            temp <- changevar_mean(z1, z2, wgt, xcoord, ycoord, revisitwgt,
              mean1, mean2, stratum_ind, NULL, cluster_ind,
              pcfactor_ind = popcorrect, fpcsize = fpcsize,  vartype = vartype,
              warn_ind = warn_ind, warn_df = warn_df, warn_vec = warn_vec)
          }
          rslt <- temp$rslt
          warn_ind <- temp$warn_ind
          warn_df <- temp$warn_df

# End the section for nonempty vectors of continuous variable values for
# revisit sites

        }

# End the section for unstratified data
      }

# Calculate standard errors

      if(is.na(rslt)) {
        results$StdError <- sqrt(results$StdError_1^2 + results$StdError_2^2)
      } else {
        tw_1r <- sum(weights(design_1))
        tw_2r <- sum(weights(design_2))
        if(revisitwgt) {
          temp <- results$StdError_1^2 + results$StdError_2^2 -
            ((2*tw_1r*tw_2r)/(tw_1*tw_2))*rslt
          if(is.na(temp)) {
            temp <- 0
          }
          if(temp <= 0) {
            results$StdError <- sqrt(results$StdError_1^2 +
                results$StdError_2^2)
          } else {
            results$StdError <- sqrt(temp)
          }
        } else {
          temp <- percentile_est(NULL, dframe_1, itype, isubpop, 1, ivar,
            design_1, design_names, var_nondetect, popcorrect, vartype, conf,
            mult, pctval = c(50), warn_ind, warn_df)
          se_1 <- subset(temp$pctsum, Statistic == "Mean")$StdError
          temp <- percentile_est(NULL, dframe_2, itype, isubpop, 1, ivar,
            design_2, design_names, var_nondetect, popcorrect, vartype, conf,
            mult, pctval = c(50), warn_ind, warn_df)
          se_2 <- subset(temp$pctsum, Statistic == "Mean")$StdError
          covest <- rslt * se_1 * se_2
          temp <- results$StdError_1^2 + results$StdError_2^2 -
            ((2*tw_1r*tw_2r)/(tw_1*tw_2))*covest
          if(is.na(temp)) {
            temp <- 0
          }
          if(temp <= 0) {
            results$StdError <- sqrt(results$StdError_1^2 +
                results$StdError_2^2)
          } else {
            results$StdError <- sqrt(temp)
          }
        }
      }

# Calculate margins of error and confidence bounds for the change estimate

      results$MarginofError <- mult * results$StdError
      results$LCB <- results$DiffEst - mult*results$StdError
      results$UCB <- results$DiffEst + mult*results$StdError

# End the section for surveys with repeat visit sites

    }

# Add estimates to the contsum_mean data frame

    changesum$contsum_mean <- rbind(changesum$contsum_mean, cbind(
      Survey_1 = survey_names[1],
      Survey_2 = survey_names[2],
      subset(results, select = c(2:4, 1, 20:24, 5:10, 14:19))))

#
# End the section for a continuous variable using the mean
#

  }

    if("median" %in% test) {

#
# Begin the section for a continuous variable using the median
#

# As necessary, restore original values for the dframe_1 and dframe_2 data
# frames and the design_1 and design_2 objects

      if("mean" %in% test) {
        dframe_1 <- dframe_1_org
        dframe_2 <- dframe_2_org
        design_1 <- design_1_org
        design_2 <- design_2_org
      }

# Using the pctest_1 data frame, extract the median estimate for survey one

      temp_cont_1 <- pctest_1$pctsum[pctest_1$pctsum$Statistic == "50Pct",]
      warn_ind <- pctest_1$warn_ind
      warn_df <- pctest_1$warn_df

# Create the categorical response variables

      p_1 <- factor(ifelse(dframe_1[, ivar] <= temp_cont_1$Estimate,
        "Less_Than_Median", "Greater_Than_Median"))
      dframe_1$medcat <- p_1
      if("postStrata" %in% names(design_1)) {
        temp <- factor(ifelse(design_1$variables[, ivar] <= temp_cont_1$Estimate,
          "Less_Than_Median", "Greater_Than_Median"))
        design_1 <- update(design_1, medcat = temp)
      } else {
        design_1 <- update(design_1, medcat = p_1)
      }
      p_2 <- factor(ifelse(dframe_2[, ivar] <= temp_cont_1$Estimate,
        "Less_Than_Median", "Greater_Than_Median"))
      dframe_2$medcat <- p_2
      if("postStrata" %in% names(design_1)) {
        temp <- factor(ifelse(design_2$variables[, ivar] <= temp_cont_1$Estimate,
          "Less_Than_Median", "Greater_Than_Median"))
        design_2 <- update(design_2, medcat = temp)
      } else {
        design_2 <- update(design_2, medcat = p_2)
      }
      ivar <- "medcat"
      lev_ivar<- levels(p_1)
      nlev_ivar <- 2

# Calculate estimates for all sites from survey one

      dframe_1[, itype] <- droplevels(dframe_1[, itype])
      temp <- category_est(NULL, dframe_1, itype, isubpop, 1, ivar, lev_ivar,
        nlev_ivar, design_1, design_names, popcorrect,  vartype, conf, mult,
        warn_ind, warn_df)
      temp_1 <- droplevels(subset(temp$catsum, Category != "Total"))
      warn_ind <- temp$warn_ind
      warn_df <- temp$warn_df
      tw_1 <- sum(weights(design_1))

# Calculate estimates for all sites from survey two

      dframe_2[, itype] <- droplevels(dframe_2[, itype])
      temp <- category_est(NULL, dframe_2, itype, isubpop, 1, ivar, lev_ivar,
        nlev_ivar, design_2, design_names, popcorrect,  vartype, conf, mult,
        warn_ind, warn_df)
      temp_2 <- droplevels(subset(temp$catsum, Category != "Total"))
      warn_ind <- temp$warn_ind
      warn_df <- temp$warn_df
      tw_2 <- sum(weights(design_2))

# Merge results for the two surveys

      results <- merge(temp_1, temp_2, by="Category", suffix=c("_1", "_2"),
        all=TRUE, sort=FALSE)

# Calculate the change estimates

      results$DiffEst.P <- (results$Estimate.P_2 - results$Estimate.P_1)/100
      results$DiffEst.U <- results$Estimate.U_2 - results$Estimate.U_1

# Express the standard error estimates for the two surveys on the proportion
# scale

      results$StdError.P_1 <- results$StdError.P_1/100
      results$StdError.P_2 <- results$StdError.P_2/100

# Calculate confidence bound multiplier

      mult <- qnorm(0.5 + (conf/100)/2)

# Calculate standard error of the change estimates for surveys with no repeat
# visit sites

      if(sum(repeat_1) == 0) {

        results$StdError.P <- sqrt(results$StdError.P_1^2 +
            results$StdError.P_2^2)
        results$StdError.U <- sqrt(results$StdError.U_1^2 +
            results$StdError.U_2^2)
        results$LCB.P <- 100 * pmax(results$DiffEst.P - mult*results$StdError.P,
          -1)
        results$UCB.P <- 100 * pmin(results$DiffEst.P + mult*results$StdError.P,
          1)
        results$DiffEst.P <- 100 * results$DiffEst.P
        results$StdError.P <- 100 * results$StdError.P
        results$StdError.P_1 <- 100 * results$StdError.P_1
        results$StdError.P_2 <- 100 * results$StdError.P_2
        if("postStrata" %in% names(design_1)) {
          results$LCB.U <- pmax(results$DiffEst.U - mult*results$StdError.U,
            -tw_1)
          results$UCB.U <- pmin(results$DiffEst.U + mult*results$StdError.U,
            tw_2)
        } else {
          results$LCB.U <- results$DiffEst.U - mult*results$StdError.U
          results$UCB.U <- results$DiffEst.U + mult*results$StdError.U
        }

# Calculate standard error of the change estimates for surveys with repeat
# visit sites

      } else {

# Subset the dframe_1 and design_2 objects to retain repeat visit sites

        dframe_1 <- subset(dframe_1, repeat_1)
        dframe_2 <- subset(dframe_2, repeat_2)

# Subset the design_1 and design_2 objects to retain repeat visit sites

        design_1 <- subset(design_1, repeat_1)
        design_2 <- subset(design_2, repeat_2)

# Assign values for the categorical variables

        catvar_1 <- dframe_1[, ivar]
        catvar_2 <- dframe_2[, ivar]

# Assign values for survey design variables using the survey one design object

        tempdf <- design_1$variables
        for(i in names(design_names)) {
          if(is.null(design_names[[i]])) {
            eval(parse(text=paste0(i, " <- NULL")))
          } else {
            eval(parse(text=paste0(i, " <- tempdf[, \"", design_names[[i]], "\"]")))
          }
        }

# Assign a value to the indicator variable for a two-stage sample

        cluster_ind <- !is.null(clusterID)

# Assign values to weight variables

        if(revisitwgt) {
          if(cluster_ind) {
            wgt1 <- tempdf$wgt1
            wgt2 <- tempdf$wgt2
          } else {
            wgt <- tempdf$wgt
          }
        } else {
          if(cluster_ind) {
            wgt1 <- rep(1, length(catvar_1))
            wgt2 <- rep(1, length(catvar_1))
          } else {
            wgt <- rep(1, length(catvar_1))
          }
        }

# Assign a logical value to the indicator variable for a stratified sample

        stratum_ind <- !is.null(stratumID)

# If the sample is stratified, convert stratum to a factor, determine stratum
# levels, and calculate number of strata

        if(stratum_ind) {
          stratum <- factor(stratumID)
          stratum_levels <- levels(stratum)
          nstrata <- length(stratum_levels)
        }

# Remove missing values

        indx <- 1:sum(repeat_1)
        indx[is.na(catvar_1) | is.na(catvar_2)] <- 0
        catvar_1 <- catvar_1[indx]
        catvar_2 <- catvar_2[indx]
        if(stratum_ind) {
          if(cluster_ind) {
            wgt2 <- wgt2[indx]
            xcoord <- xcoord[indx]
            ycoord <- ycoord[indx]
            stratum <- stratum[indx]
            clusterID <- clusterID[indx]
            wgt1 <- wgt1[indx]
            xcoord1 <- xcoord1[indx]
            ycoord1 <- ycoord1[indx]
            if(popcorrect) {
              Ncluster <- Ncluster[indx]
              stage1size <- stage1size[indx]
            }
          } else {
            wgt <- wgt[indx]
            xcoord <- xcoord[indx]
            ycoord <- ycoord[indx]
            stratum <- stratum[indx]
            if(popcorrect) {
              fpcsize <- fpcsize[indx]
            }
          }
        } else {
          if(cluster_ind) {
            wgt2 <- wgt2[indx]
            xcoord <- xcoord[indx]
            ycoord <- ycoord[indx]
            clusterID <- clusterID[indx]
            wgt1 <- wgt1[indx]
            xcoord1 <- xcoord1[indx]
            ycoord1 <- ycoord1[indx]
            if(popcorrect) {
              Ncluster <- Ncluster[indx]
              stage1size <- stage1size[indx]
            }
           } else {
            wgt <- wgt[indx]
            xcoord <- xcoord[indx]
            ycoord <- ycoord[indx]
            if(popcorrect) {
              fpcsize <- fpcsize[indx]
            }
          }
        }

# For a stratified sample, remove strata that contain a single site

        if(stratum_ind) {
          ind <- FALSE
          for(i in 1:nstrata) {
            tst <- stratum == stratum_levels[i]
            if(sum(tst) == 1) {
              warn_ind <- TRUE
              warn <- paste0("The stratum named \"", stratum_levels[i], "\" contains a single value and was removed from the analysis.\n")
              act <- "Stratum was not used for standard error estimation.\n"
              warn_df <- rbind(warn_df, data.frame(func=I(fname), subpoptype=NA,
                subpop=NA, indicator=NA, stratum=NA, warning=I(warn),
                action=I(act)))
              dframe <- dframe[!tst,]
              ind <- TRUE
              catvar_1 <- catvar_1[!tst]
              catvar_2 <- catvar_2[!tst]
              if(vartype == "Local") {
                xcoord <- xcoord[!tst]
                ycoord <- ycoord[!tst]
              }
              stratum <- stratum[!tst]
              if(cluster_ind) {
                clusterID <- clusterID[!tst]
                wgt1 <- wgt1[!tst]
                wgt2 <- wgt2[!tst]
                if(vartype == "Local") {
                  xcoord1 <- xcoord1[!tst]
                  ycoord1 <- ycoord1[!tst]
                }
                if(popcorrect) {
                  Ncluster <- Ncluster[!tst]
                  stage1size <- stage1size[!tst]
                }
              } else {
                wgt <- wgt[!tst]
                if(popcorrect) {
                  fpcsize <- fpcsize[!tst]
                }
              }
              ind <- TRUE

            }
          }
          if(ind) {
            stratum <- factor(stratum)
            stratum_levels <- levels(stratum)
            nstrata <- length(stratum_levels)
          }
        }

# For a stratified sample, check whether the number of strata is one

        if(stratum_ind) {
          if(nstrata == 1) {
            warn_ind <- TRUE
            warn <- "Only a single stratum was available for the analysis.\n"
            act <- "An unstratified data analysis was used.\n"
            warn_df <- rbind(warn_df, data.frame(func=I(fname),
              subpoptype=warn_vec[1], subpop=warn_vec[2], indicator=warn_vec[3],
              stratum=NA, warning=I(warn), action=I(act)))
            stratum_ind <- FALSE
          }
        }

# Assign levels of the categorical variables

        catvar_levels <- results$Category
        nlevels <- length(catvar_levels)

# Calculate population size values

        if(stratum_ind) {
          if(cluster_ind) {
            popsize_hat <- tapply(wgt1 * wgt2, stratum, sum)
            sum_popsize_hat <- sum(wgt1 * wgt2)
          } else {
            popsize_hat <- tapply(wgt, stratum, sum)
            sum_popsize_hat <- sum(wgt)
          }
        } else {
          if(cluster_ind) {
            popsize_hat <- sum(wgt1 * wgt2)
          } else {
            popsize_hat <- sum(wgt)
          }
        }

# Branch to handle stratified and unstratified data

        if(stratum_ind) {

# Begin the section for stratified data

# Create the vector of covariance or correlation estimates for all strata
# combined

          rslt_P <- rep(NA, nlevels)
          rslt_U <- rep(NA, nlevels)

# Check whether the vectors of categorical variable values for revisit sites
# are empty or contain a single value

          if(length(catvar_1) <= 1) {
            warn_ind <- TRUE
            act <- "Covariance among the revisited sites was not included in calculation of \nthe standard error estimate.\n"
            warn <- paste("The number of nonmissing repeat visit sites was less than two in one of the \nsurveys.\n", sep="")
            warn_df <- rbind(warn_df, data.frame(func=I(fname),
              subpoptype=warn_vec[1], subpop=warn_vec[2], indicator=warn_vec[3],
              stratum=NA, warning=I(warn), action=I(act)))

# Begin section for nonempty vectors of categorical variable values for revisit
# sites

          } else {

            # Begin the loop for individual strata

            for(i in 1:nstrata) {

# Check whether the vectors of categorical variable values for revisit sites
# are empty or contain a single value for a stratum

              stratum_i <- stratum == stratum_levels[i]
              if((sum(!is.na(catvar_1[stratum_i])) <= 1) |
                  (sum(!is.na(catvar_2[stratum_i])) <= 1)) {
                warn_ind <- TRUE
                act <- "Due to insufficient number of sites, the stratum was not included in \ncalculation of covariance among the revisited sites.\n"
                warn <- paste("The number of nonmissing repeat visit sites  in one of the surveys was less \nthan two for stratum \"", stratum_levels[i], "\".\n", sep="")
                warn_df <- rbind(warn_df, data.frame(func=I(fname),
                  subpoptype=warn_vec[1], subpop=warn_vec[2],
                  indicator=warn_vec[3], stratum=I(stratum_levels[i]),
                  warning=I(warn), action=I(act)))

# Begin section for nonempty vectors of categorical variable values for revisit
# sites for a stratum
#
              } else {

                # Calculate proportion estimates

                z1 <- factor(catvar_1[stratum_i], levels=catvar_levels)
                z2 <- factor(catvar_2[stratum_i], levels=catvar_levels)
                m <- length(catvar_levels)
                prop1 <- as.vector(svymean(make.formula(ivar),
                  design = subset(design_1, stratum_i), na.rm = TRUE))
                prop2 <- as.vector(svymean(make.formula(ivar),
                  design = subset(design_2, stratum_i), na.rm = TRUE))

# Calculate covariance or correlation estimates

                if(cluster_ind) {
                  temp <- changevar_prop(catvar_levels, z1, z2, wgt2[stratum_i],
                    xcoord[stratum_i], ycoord[stratum_i], revisitwgt, prop1,
                    prop2, stratum_ind, stratum_levels[i], cluster_ind,
                    clusterID[stratum_i], wgt1[stratum_i], xcoord1[stratum_i],
                    ycoord1[stratum_i], popcorrect, NULL, Ncluster[stratum_i],
                    stage1size[stratum_i], vartype, warn_ind, warn_df, warn_vec)
                } else {
                  temp <- changevar_prop(catvar_levels, z1, z2, wgt[stratum_i],
                    xcoord[stratum_i], ycoord[stratum_i], revisitwgt, prop1,
                    prop2, stratum_ind, stratum_levels[i], cluster_ind,
                    pcfactor_ind = popcorrect, fpcsize = fpcsize[stratum_i],
                    vartype = vartype, warn_ind = warn_ind, warn_df = warn_df,
                    warn_vec = warn_vec)
                }
                correst <- temp$rslt
                warn_ind <- temp$warn_ind
                warn_df <- temp$warn_df

# Add estimates to the vector for all strata combined

                rslt_P[!is.na(correst)] <- rslt_P[!is.na(correst)] +
                  (popsize_hat[i]/sum_popsize_hat)*correst[!is.na(correst)]

# Estimate the size of each category

                size1 <- popsize_hat[i] * prop1
                size2 <- popsize_hat[i] * prop2

# Calculate covariance or correlation estimates

                if(cluster_ind) {
                  temp <- changevar_total(catvar_levels, z1, z2, wgt2[stratum_i],
                    xcoord[stratum_i], ycoord[stratum_i], revisitwgt, size1,
                    size2, stratum_ind, stratum_levels[i], cluster_ind,
                    clusterID[stratum_i], wgt1[stratum_i], xcoord1[stratum_i],
                    ycoord1[stratum_i], popcorrect, NULL, Ncluster[stratum_i],
                    stage1size[stratum_i], vartype, warn_ind, warn_df, warn_vec)
                } else {
                  temp <- changevar_total(catvar_levels, z1, z2, wgt[stratum_i],
                    xcoord[stratum_i], ycoord[stratum_i], revisitwgt, size1,
                    size2, stratum_ind, stratum_levels[i], cluster_ind,
                    pcfactor_ind = popcorrect, fpcsize = fpcsize[stratum_i],
                    vartype = vartype, warn_ind = warn_ind, warn_df = warn_df,
                    warn_vec = warn_vec)
                }
                correst <- temp$rslt
                warn_ind <- temp$warn_ind
                warn_df <- temp$warn_df

# Add estimates to the vector for all strata combined

                rslt_U[!is.na(correst)] <- rslt_U[!is.na(correst)] +
                  correst[!is.na(correst)]

# End the section for nonempty vectors of categorical variable values for
# revisit sites for a stratum

              }

# End the loop for individual strata

            }

# End the section for nonempty vectors of categorical variable values for
# revisit sites

          }

# End the section for stratified data

        } else {

          # Begin the section for unstratified data

          # Check whether the vectors of categorical variable values for revisit sites
          # are empty or contain a single value

          if(length(catvar_1) <= 1) {
            rslt_P <- rep(NA, nlevels)
            rslt_U <- rep(NA, nlevels)
            warn_ind <- TRUE
            act <- "Covariance among the revisited sites was not included in calculation of \nthe standard error estimate.\n"
            warn <- paste("The number of nonmissing repeat visit sites was less than two in one of the \nsurveys.\n", sep="")
            warn_df <- rbind(warn_df, data.frame(func=I(fname),
              subpoptype=warn_vec[1], subpop=warn_vec[2], indicator=warn_vec[3],
              stratum=NA, warning=I(warn), action=I(act)))

# Begin section for nonempty vectors of categorical variable values for revisit
# sites

          } else {

            # Calculate proportion estimates

            z1 <- factor(catvar_1, levels=catvar_levels)
            z2 <- factor(catvar_2, levels=catvar_levels)
            prop1 <- as.vector(svymean(make.formula(ivar),
              design = design_1, na.rm = TRUE))
            prop2 <- as.vector(svymean(make.formula(ivar),
              design = design_2, na.rm = TRUE))

# Calculate covariance or correlation estimates

            if(cluster_ind) {
              temp <- changevar_prop(catvar_levels, z1, z2, wgt2, xcoord, ycoord,
                revisitwgt, prop1, prop2, stratum_ind, NULL, cluster_ind,
                clusterID, wgt1, xcoord1, ycoord1, popcorrect, NULL, Ncluster,
                stage1size, vartype, warn_ind, warn_df, warn_vec)
            } else {
              temp <- changevar_prop(catvar_levels, z1, z2, wgt, xcoord, ycoord,
                revisitwgt, prop1, prop2, stratum_ind, NULL, cluster_ind,
                pcfactor_ind = popcorrect, fpcsize = fpcsize, vartype = vartype,
                warn_ind = warn_ind, warn_df = warn_df, warn_vec = warn_vec)
            }
            rslt_P <- temp$rslt
            warn_ind <- temp$warn_ind
            warn_df <- temp$warn_df

# Estimate the size of each category

            size1 <- popsize_hat*prop1
            size2 <- popsize_hat*prop2

# Calculate covariance or correlation estimates

            if(cluster_ind) {
              temp <- changevar_total(catvar_levels, z1, z2, wgt2, xcoord, ycoord,
                revisitwgt, size1, size2, stratum_ind, NULL, cluster_ind,
                clusterID, wgt1, xcoord1, ycoord1, popcorrect, NULL, Ncluster,
                stage1size, vartype, warn_ind, warn_df, warn_vec)
            } else {
              temp <- changevar_total(catvar_levels, z1, z2, wgt, xcoord, ycoord,
                revisitwgt, size1, size2, stratum_ind, NULL, cluster_ind,
                pcfactor_ind = popcorrect, fpcsize = fpcsize,  vartype = vartype,
                warn_ind = warn_ind, warn_df = warn_df, warn_vec = warn_vec)
            }
            rslt_U <- temp$rslt
            warn_ind <- temp$warn_ind
            warn_df <- temp$warn_df

# End the section for nonempty vectors of categorical variable values for
# revisit sites

          }

# End the section for unstratified data

        }

# Calculate standard errors

        results$StdError.P <- rep(NA, nlevels)
        results$StdError.U <- rep(NA, nlevels)
        ind <- is.na(rslt_P)
        results$StdError.P[ind] <- sqrt(results$StdError.P_1[ind]^2 +
            results$StdError.P_2[ind]^2)
        results$StdError.U[ind] <- sqrt(results$StdError.U_1[ind]^2 +
            results$StdError.U_2[ind]^2)
        if(any(!ind)) {
          tw_1r <- sum(weights(design_1))
          tw_2r <- sum(weights(design_2))
          if(revisitwgt) {
            temp <- results$StdError.P_1^2 + results$StdError.P_2^2 -
              ((2*tw_1r*tw_2r)/(tw_1*tw_2))*rslt_P
            ind <- !is.na(rslt_P) & temp <= 0
            results$StdError.P[ind] <- sqrt(results$StdError.P_1[ind]^2 +
                results$StdError.P_2[ind]^2)
            ind <- !is.na(rslt_P) & temp > 0
            results$StdError.P[ind] <- sqrt(temp[ind])
            temp <- results$StdError.U_1^2 + results$StdError.U_2^2 - 2*rslt_U
            ind <- !is.na(rslt_U) & temp <= 0
            results$StdError.U[ind] <- sqrt(results$StdError.U_1[ind]^2 +
                results$StdError.U_2[ind]^2)
            ind <- !is.na(rslt_U) & temp > 0
            results$StdError.U[ind] <- sqrt(temp[ind])
          } else {
            se_1_p <- rep(NA, nlevels)
            se_1_u <- rep(NA, nlevels)
            temp <- category_est(NULL, dframe_1, itype, isubpop, 1, ivar,
              lev_ivar, nlev_ivar, design_1, design_names, popcorrect, vartype,
              conf, mult, warn_ind, warn_df)
            temp$results <- droplevels(subset(temp$catsum, Category != "Total"))
            ind <- match(temp$results$Category, catvar_levels, nomatch=0)
            se_1_p[ind] <- temp$results$StdError.P / 100
            se_1_u[ind] <- temp$results$StdError.U
            se_2_p <- rep(NA, nlevels)
            se_2_u <- rep(NA, nlevels)
            temp <- category_est(NULL, dframe_2, itype, isubpop, 1, ivar,
              lev_ivar, nlev_ivar, design_2, design_names, popcorrect, vartype,
              conf, mult, warn_ind, warn_df)
            temp$results <- droplevels(subset(temp$catsum, Category != "Total"))
            ind <- match(temp$results$Category, catvar_levels, nomatch=0)
            se_2_p[ind] <- temp$results$StdError.P / 100
            se_2_u[ind] <- temp$results$StdError.U
            covest <- rslt_P * se_1_p * se_2_p
            temp <- results$StdError.P_1^2 + results$StdError.P_2^2 -
              ((2*tw_1r*tw_2r)/(tw_1*tw_2))*covest
            ind <- !is.na(rslt_P) & temp <= 0
            results$StdError.P[ind] <- sqrt(results$StdError.P_1[ind]^2 +
                results$StdError.P_2[ind]^2)
            ind <- !is.na(rslt_P) & temp > 0
            results$StdError.P[ind] <- sqrt(temp[ind])
            covest <- rslt_U * se_1_u * se_2_u
            temp <- results$StdError.U_1^2 + results$StdError.U_2^2 -2*covest
            ind <- !is.na(rslt_U) & temp <= 0
            results$StdError.U[ind] <- sqrt(results$StdError.U_1[ind]^2 +
                results$StdError.U_2[ind]^2)
            ind <- !is.na(rslt_U) & temp > 0
            results$StdError.U[ind] <- sqrt(temp[ind])
          }
        }

# Calculate margins of error and confidence bounds

        results$LCB.P <- 100 * pmax(results$DiffEst.P -
            mult*results$StdError.P, -1)
        results$UCB.P <- 100 * pmin(results$DiffEst.P +
            mult*results$StdError.P, 1)
        results$DiffEst.P <- 100 * results$DiffEst.P
        results$MarginofError.P <- 100 * (mult * results$StdError.P)
        results$MarginofError.P_1 <- 100 * (mult * results$StdError.P_1)
        results$MarginofError.P_2 <- 100 * (mult * results$StdError.P_2)
        results$StdError.P <- 100 * results$StdError.P
        results$StdError.P_1 <- 100 * results$StdError.P_1
        results$StdError.P_2 <- 100 * results$StdError.P_2
        results$MarginofError.U <- mult * results$StdError.U
        results$MarginofError.U_1 <- mult * results$StdError.U_1
        results$MarginofError.U_2 <- mult * results$StdError.U_2
        if("postStrata" %in% names(design_1)) {
          results$LCB.U <- pmax(results$DiffEst.U - mult*results$StdError.U,
            -tw_1)
          results$UCB.U <- pmin(results$DiffEst.U + mult*results$StdError.U,
            tw_2)
        } else {
          results$LCB.U <- results$DiffEst.U - mult*results$StdError.U
          results$UCB.U <- results$DiffEst.U + mult*results$StdError.U
        }

# End the section for surveys with repeat visit sites

      }

# Add estimates to the contsum_median data frame

      changesum$contsum_median <- rbind(changesum$contsum_median, cbind(
        Survey_1 = survey_names[1],
        Survey_2 = survey_names[2],
        subset(results, select = c(2:4, 1, 30, 32, 36, 34:35, 31, 33, 37:39,
          5:15, 19:29))))

#
# End the section for a continuous variable using the median
#

    }

# Print an error message for an unrecognized type of test

    if(!any(c("mean", "median") %in% test))
      stop(paste0("\nThe value provided for argument test, \"", test, "\", is not a valid value", sep=""))

  } else {

# Print an error message for an unrecognized type of response variable

    stop(paste("\nThe value provided for argument resp_ind, ", resp_ind, ", is not a valid value", sep=""))

  }

# Return the changesum object, the warn_ind logical value, and the warn_df
# data frame

  list(changesum = changesum, warn_ind = warn_ind, warn_df = warn_df)

}
