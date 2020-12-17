################################################################################
# Function: survey_design
# Programmer: Tom Kincaid
# Date: December 16, 2020
#
#' Create a Survey Design Object
#'
#' This function creates a survey design object.  The function organizes input
#' and then calls the svydesign function in the survey package to create an
#' object of class survey.design2.
#'
#' @param dframe Data frame containing survey design variables, response
#'   variables, and subpopulation (domain) variables.
#'
#' @param siteID Character value providing name of the site ID variable in
#'   the dframe data frame.  For a two-stage sample, the site ID variable
#'   identifies stage two site IDs.
#'
#' @param weight Character value providing name of the survey design weight
#'   variable in the dframe data frame.  For a two-stage sample, the weight
#'   variable identifies stage two weights.
#'
#' @param stratum.ind Logical value that indicates whether the sample is
#'   stratified, where TRUE = a stratified sample and FALSE = an unstratified
#'   sample.
#'
#' @param stratumID Character value providing name of the stratum ID variable in
#'   the dframe data frame.
#'
#' @param cluster.ind Logical value that indicates whether the sample contains
#'   two stages, where TRUE = a two-stage sample and FALSE = a single stage
#'   sample.
#'
#' @param clusterID Character value providing name of the cluster (stage one) ID
#'   variable in the dframe data frame.  Note that cluster IDs are required for
#'   a two-stage sample.
#'
#' @param weight1 Character value providing name of the stage one weight
#'   variable in the dframe data frame.
#'
#' @param sizeweight Logical value that indicates whether size weights should be
#'   used during estimation, where TRUE = use size weights and FALSE = do not
#'   use size weights.
#'
#' @param sweight Character value providing name of the size weight variable in
#'   the dframe data frame.  For a two-stage sample, the size weight variable
#'   identifies stage two size weights.
#'
#' @param sweight1 Character value providing name of the stage one size weight
#'   variable in the dframe data frame.
#'
#' @param popcorrect Logical value that indicates whether the finite population
#'   correction factor is used during variance estimation.
#'
#' @param fpcsize Character value providing name of the variable in the dframe
#'   data frame that identifies size of the resource, which is required for
#'   calculation of the finite population correction factor for a single-stage
#'   sample.
#'
#' @param Ncluster Character value providing name of the variable in the dframe
#'   data frame that identifies the number of clusters (stage one sampling
#'   units) in the resource, which is required for calculation of the finite
#'   population correction factor for a two-stage sample.  This argument is also
#'   required for a two-stage sample when the popsize argument is not equal to
#'   NULL and the vartype argument equals "Local".
#'
#' @param stage1size Character value providing name of the variable in the
#'   dframe data frame that identifies cluster size, i.e. the number of the
#'   stage two sampling units in the resource for a cluster.  Note that cluster
#'   size is required for calculation of the finite population correction factor
#'   for a two-stage sample.
#'
#' @param vartype Character value providing choice of the variance estimator,
#'   where "Local" = the local mean estimator, "SRS" = the simple random
#'   sampling estimator, "HT" = the Horvitz-Thompson estimator, and "YG" = the
#'   Yates-Grundy estimator.
#'
#' @param jointprob Character value providing choice of joint inclusion
#'   probability approximation for use with Horvitz-Thompson and Yates-Grundy
#'   variance estimators, where "overton" indicates the Overton approximation,
#'   "hr" indicates the Hartley_Rao approximation, and "brewer" equals the
#'   Brewer approximation.
#'
#' @return An object of class survey.design2.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{svydesign}}}{specifies a complex survey design}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @seealso
#'   \code{\link{svydesign}}
#'
#' @keywords survey
#'
#' @export
################################################################################

survey_design <- function(dframe, siteID, weight, stratum.ind, stratumID,
  cluster.ind, clusterID, weight1, sizeweight, sweight, sweight1, popcorrect,
  fpcsize, Ncluster, stage1size, vartype, jointprob) {

  if(vartype %in% c("Local", "SRS")) {
    if(stratum.ind) {
      if(cluster.ind) {
        if(sizeweight) {
          dframe$wgt1 <- dframe[, weight1] * dframe[, sweight1]
          dframe$wgt2 <- dframe[, weight] * dframe[, sweight]
          if(popcorrect) {
            design <- svydesign(
              ids = make.formula(paste(clusterID, siteID, sep=" + ")),
              weights = ~wgt1 + wgt2,
              strata = make.formula(stratumID),
              nest = TRUE,
              fpc = make.formula(paste(Ncluster, stage1size, sep=" + ")),
              data = dframe)
          } else {
            design <- svydesign(
              ids = make.formula(paste(clusterID, siteID, sep=" + ")),
              weights = ~wgt1 + wgt2,
              strata = make.formula(stratumID),
              nest = TRUE,
              data = dframe)
          }
        } else {
          dframe$wgt1 <- dframe[, weight1]
          dframe$wgt2 <- dframe[, weight]
          if(popcorrect) {
            design <- svydesign(
              ids = make.formula(paste(clusterID, siteID, sep=" + ")),
              weights = ~wgt1 + wgt2,
              strata = make.formula(stratumID),
              nest = TRUE,
              fpc = make.formula(paste(Ncluster, stage1size, sep=" + ")),
              data = dframe)
          } else {
            design <- svydesign(
              ids = make.formula(paste(clusterID, siteID, sep=" + ")),
              weights = ~wgt1 + wgt2,
              strata = make.formula(stratumID),
              nest = TRUE,
              data = dframe)
          }
        }
      } else {
        if(sizeweight) {
          dframe$wgt <- dframe[, weight] * dframe[, sweight]
          if(popcorrect) {
            design <- svydesign(
              ids = make.formula(siteID),
              weights = ~wgt,
              strata = make.formula(stratumID),
              fpc = make.formula(fpcsize),
              data = dframe)
          } else {
            design <- svydesign(
              ids = make.formula(siteID),
              weights = ~wgt,
              strata = make.formula(stratumID),
              data = dframe)
          }
        } else {
          dframe$wgt <- dframe[, weight]
          if(popcorrect) {
            design <- svydesign(
              ids = make.formula(siteID),
              weights = ~wgt,
              strata = make.formula(stratumID),
              fpc = make.formula(fpcsize),
              data = dframe)
          } else {
            design <- svydesign(
              ids = make.formula(siteID),
              weights = ~wgt,
              strata = make.formula(stratumID),
              data = dframe)
          }
        }
      }
    } else {
      if(cluster.ind) {
        if(sizeweight) {
          dframe$wgt1 <- dframe[, weight1] * dframe[, sweight1]
          dframe$wgt2 <- dframe[, weight] * dframe[, sweight]
          if(popcorrect) {
            design <- svydesign(
              ids = make.formula(paste(clusterID, siteID, sep=" + ")),
              weights = ~wgt1 + wgt2,
              fpc = make.formula(paste(Ncluster, stage1size, sep=" + ")),
              data = dframe)
          } else {
            design <- svydesign(
              ids = make.formula(paste(clusterID, siteID, sep=" + ")),
              weights = ~wgt1 + wgt2,
              data = dframe)
          }
        } else {
          dframe$wgt1 <- dframe[, weight1]
          dframe$wgt2 <- dframe[, weight]
          if(popcorrect) {
            design <- svydesign(
              ids = make.formula(paste(clusterID, siteID, sep=" + ")),
              weights = ~wgt1 + wgt2,
              fpc = make.formula(paste(Ncluster, stage1size, sep=" + ")),
              data = dframe)
          } else {
            design <- svydesign(
              ids = make.formula(paste(clusterID, siteID, sep=" + ")),
              weights = ~wgt1 + wgt2,
              data = dframe)
          }
        }
      } else {
        if(sizeweight) {
          dframe$wgt <- dframe[, weight] * dframe[, sweight]
          if(popcorrect) {
            design <- svydesign(
              ids = make.formula(siteID),
              weights = ~wgt,
              fpc = make.formula(fpcsize),
              data = dframe)
          } else {
            design <- svydesign(
              ids = make.formula(siteID),
              weights = ~wgt,
              data = dframe)
          }
        } else {
          dframe$wgt <- dframe[, weight]
          if(popcorrect) {
            design <- svydesign(
              ids = make.formula(siteID),
              weights = ~wgt,
              fpc = make.formula(fpcsize),
              data = dframe)
          } else {
            design <- svydesign(
              ids = make.formula(siteID),
              weights = ~wgt,
              data = dframe)
          }
        }
      }
    }
  } else {
    if(jointprob %in% c("overton", "hr")) {
      if(stratum.ind) {
        if(cluster.ind) {
          if(sizeweight) {
            dframe$prb1 <- 1 / (dframe[, weight1] * dframe[, sweight1])
            dframe$prb2 <- 1 / (dframe[, weight] * dframe[, sweight])
            if(jointprob == "overton") {
              design <- svydesign(
                ids = make.formula(paste(clusterID, siteID, sep=" + ")),
                probs = ~prb1 + prb2,
                strata = make.formula(stratumID),
                nest = TRUE,
                variance = vartype,
                pps = jointprob,
                data = dframe)
            } else {
              design <- svydesign(
                ids = make.formula(paste(clusterID, siteID, sep=" + ")),
                probs = ~prb1 + prb2,
                strata = make.formula(stratumID),
                nest = TRUE,
                variance = vartype,
                pps = HR(),
                data = dframe)
            }
          } else {
            dframe$prb1 <- 1 / dframe[, weight1]
            dframe$prb2 <- 1 / dframe[, weight]
            if(jointprob == "overton") {
              design <- svydesign(
                ids = make.formula(paste(clusterID, siteID, sep=" + ")),
                probs = ~prb1 + prb2,
                strata = make.formula(stratumID),
                nest = TRUE,
                variance = vartype,
                pps = jointprob,
                data = dframe)
            } else {
              design <- svydesign(
                ids = make.formula(paste(clusterID, siteID, sep=" + ")),
                probs = ~prb1 + prb2,
                strata = make.formula(stratumID),
                nest = TRUE,
                variance = vartype,
                pps = HR(),
                data = dframe)
            }
          }
        } else {
          if(sizeweight) {
            dframe$prb <- 1 / (dframe[, weight] * dframe[, sweight])
            if(jointprob == "overton") {
              design <- svydesign(
                ids = make.formula(siteID),
                probs = ~prb,
                strata = make.formula(stratumID),
                variance = vartype,
                pps = jointprob,
                data = dframe)
            } else {
              design <- svydesign(
                ids = make.formula(siteID),
                probs = ~prb,
                strata = make.formula(stratumID),
                variance = vartype,
                pps = HR(),
                data = dframe)
            }
          } else {
            dframe$prb <- 1 / dframe[, weight]
            if(jointprob == "overton") {
              design <- svydesign(
                ids = make.formula(siteID),
                probs = ~prb,
                strata = make.formula(stratumID),
                variance = vartype,
                pps = jointprob,
                data = dframe)
            } else {
              design <- svydesign(
                ids = make.formula(siteID),
                probs = ~prb,
                strata = make.formula(stratumID),
                variance = vartype,
                pps = HR(),
                data = dframe)
            }
          }
        }
      } else {
        if(cluster.ind) {
          if(sizeweight) {
            dframe$prb1 <- 1 / (dframe[, weight1] * dframe[, sweight1])
            dframe$prb2 <- 1 / (dframe[, weight] * dframe[, sweight])
            if(jointprob == "overton") {
              design <- svydesign(
                ids = make.formula(paste(clusterID, siteID, sep=" + ")),
                probs = ~prb1 + prb2,
                variance = vartype,
                pps = jointprob,
                data = dframe)
            } else {
              design <- svydesign(
                ids = make.formula(paste(clusterID, siteID, sep=" + ")),
                probs = ~prb1 + prb2,
                variance = vartype,
                pps = HR(),
                data = dframe)
            }
          } else {
            dframe$prb1 <- 1 / dframe[, weight1]
            dframe$prb2 <- 1 / dframe[, weight]
            if(jointprob == "overton") {
              design <- svydesign(
                ids = make.formula(paste(clusterID, siteID, sep=" + ")),
                probs = ~prb1 + prb2,
                variance = vartype,
                pps = jointprob,
                data = dframe)
            } else {
              design <- svydesign(
                ids = make.formula(paste(clusterID, siteID, sep=" + ")),
                probs = ~prb1 + prb2,
                variance = vartype,
                pps = HR(),
                data = dframe)
            }
          }
        } else {
          if(sizeweight) {
            dframe$prb <- 1 / (dframe[, weight] * dframe[, sweight])
            if(jointprob == "overton") {
              design <- svydesign(
                ids = make.formula(siteID),
                probs = ~prb,
                variance = vartype,
                pps = jointprob,
                data = dframe)
            } else {
              design <- svydesign(
                ids = make.formula(siteID),
                probs = ~prb,
                variance = vartype,
                pps = HR(),
                data = dframe)
            }
          } else {
            dframe$prb <- 1 / dframe[, weight]
            if(jointprob == "overton") {
              design <- svydesign(
                ids = make.formula(siteID),
                probs = ~prb,
                variance = vartype,
                pps = jointprob,
                data = dframe)
            } else {
              design <- svydesign(
                ids = make.formula(siteID),
                probs = ~prb,
                variance = vartype,
                pps = HR(),
                data = dframe)
            }
          }
        }
      }
    } else {
      if(stratum.ind) {
        if(cluster.ind) {
          if(sizeweight) {
            dframe$prb1 <- 1 / (dframe[, weight1] * dframe[, sweight1])
            dframe$prb2 <- 1 / (dframe[, weight] * dframe[, sweight])
            design <- svydesign(
              ids = make.formula(paste(clusterID, siteID, sep=" + ")),
              probs = ~prb1 + prb2,
              strata = make.formula(stratumID),
              nest = TRUE,
              variance = vartype,
              pps = jointprob,
              data = dframe)
          } else {
            dframe$prb1 <- 1 / dframe[, weight1]
            dframe$prb2 <- 1 / dframe[, weight]
            design <- svydesign(
              ids = make.formula(paste(clusterID, siteID, sep=" + ")),
              probs = ~prb1 + prb2,
              strata = make.formula(stratumID),
              nest = TRUE,
              variance = vartype,
              pps = jointprob,
              data = dframe)
          }
        } else {
          if(sizeweight) {
            dframe$prb <- 1 / (dframe[, weight] * dframe[, sweight])
            design <- svydesign(
              ids = make.formula(siteID),
              probs = ~prb,
              strata = make.formula(stratumID),
              variance = vartype,
              pps = jointprob,
              data = dframe)
          } else {
            dframe$prb <- 1 / dframe[, weight]
            design <- svydesign(
              ids = make.formula(siteID),
              probs = ~prb,
              strata = make.formula(stratumID),
              variance = vartype,
              pps = jointprob,
              data = dframe)
          }
        }
      } else {
        if(cluster.ind) {
          if(sizeweight) {
            dframe$prb1 <- 1 / (dframe[, weight1] * dframe[, sweight1])
            dframe$prb2 <- 1 / (dframe[, weight] * dframe[, sweight])
            design <- svydesign(
              ids = make.formula(paste(clusterID, siteID, sep=" + ")),
              probs = ~prb1 + prb2,
              variance = vartype,
              pps = jointprob,
              data = dframe)
          } else {
            dframe$prb1 <- 1 / dframe[, weight1]
            dframe$prb2 <- 1 / dframe[, weight]
            design <- svydesign(
              ids = make.formula(paste(clusterID, siteID, sep=" + ")),
              probs = ~prb1 + prb2,
              variance = vartype,
              pps = jointprob,
              data = dframe)
          }
        } else {
          if(sizeweight) {
            dframe$prb <- 1 / (dframe[, weight] * dframe[, sweight])
            design <- svydesign(
              ids = make.formula(siteID),
              probs = ~prb,
              variance = vartype,
              pps = jointprob,
              data = dframe)
          } else {
            dframe$prb <- 1 / dframe[, weight]
            design <- svydesign(
              ids = make.formula(siteID),
              probs = ~prb,
              variance = vartype,
              pps = jointprob,
              data = dframe)
          }
        }
      }
    }
  }

  design
}
