###############################################################################
# Function: cdftestvar_prop (not exported)
# Programmer: Tom Kincaid
# Date: October 23, 2020
# Revised: November 2, 2020 to correctly process the column variable when it
#          includes missing (NA) values
# Revised: April 28, 2021 to use the SRS estimator when the local mean estimator
#          fails to produce a valid estimate
#'
#' Local Mean Variance/Covariance Estimates of Estimated Population Proportions
#'
#' This function calculates the local mean estimate of the variance/covariance
#' matrix of the population proportions for a contingency table.  The function
#' can accomodate single-stage and two-stage samples.  The finite population
#' correction factor can be utilized in variance estimation.
#'
#' @param design Object of class \code{survey.design} that specifies a complex
#'   survey design.
#'
#' @param wgt Vector of the final adjusted weight (reciprocal of the sample
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
#'   stratified, where \code{TRUE} = a stratified sample and \code{FALSE} = not
#'   a stratified sample.
#'
#' @param stratum_level Vector indicating the stratum level.
#'
#' @param cluster_ind Logical value that indicates whether the sample is a
#'   two- stage sample, where \code{TRUE} = a two-stage sample and
#'   \code{FALSE} = not a two-stage sample.
#'
#' @param clusterID Vector of the stage one sampling unit (primary sampling unit
#'   or cluster) code for each site.
#'
#' @param wgt1 Vector of the final adjusted stage one weight for each site.
#'
#' @param x1 Vector of the stage one x-coordinate for location for each site.
#'
#' @param y1 Vector of the stage one y-coordinate for location for each site.
#'
#' @param pcfactor_ind Logical value that indicates whether the finite
#'   population correction factor is used during variance estimation, where
#'   \code{TRUE} = use the population correction factor and \code{FALSE} = do
#'   not use the factor.  To employ the correction factor for a single-stage
#'   sample, a value must be supplied for argument \code{fpcsize}.  To employ
#'   the correction factor for a two-stage sample, values must be supplied for
#'   arguments \code{Ncluster} and \code{stage1size}.
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
#' @param vartype The choice of variance estimator, where \code{"Local"} = local
#'   mean estimator and \code{"SRS"} = SRS estimator.
#'
#' @param warn_ind Logical value that indicates whether warning messages were
#'   generated, where \code{TRUE} = warning messages were generated and
#'   \code{FALSE} = warning messages were not generated.
#'
#' @param warn_df A data frame for storing warning messages.
#'
#' @param warn_vec Character vector that contains a subpopulation name, the
#'   first subpopulation level, the second subpopulation level, and an
#'   indicator name.
#'
#' @return Object in list format composed of a matrix named varest, which
#'   contains variance/covariance estimates, a logical variable named warn,ind,
#'   which is the indicator for warning messges, and a data frame named
#'   \code{warn_df}, which contains warning messages.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{localmean_weight}}}{calculate the weighting matrix for
#'       the local mean variance estimator}
#'     \item{\code{\link{localmean_cov}}}{calculate the variance/covariance
#'       matrix using the local mean estimator}
#'     \item{\code{\link{svymean}}}{calculates means for a complex survey
#'       design}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @noRd
###############################################################################

cdftestvar_prop <- function(design, wgt, x, y, stratum_ind,
                            stratum_level, cluster_ind, clusterID, wgt1, x1, y1, pcfactor_ind,
                            fpcsize, Ncluster, stage1size, vartype, warn_ind, warn_df, warn_vec) {

  # Assign the function name

  fname <- "cdftestvar_prop"

  # Create the model matrix for the contingency table using all of the data

  frm_cells <- eval(bquote(~ interaction(
    factor(.(as.name("rowvar"))),
    factor(.(as.name("colvar")))
  ) - 1))
  mm_cells <- model.matrix(frm_cells, model.frame(frm_cells,
    design$variables,
    na.action = na.pass
  ))

  # Calculate contingency table means

  means <- svymean(mm_cells, design, na.rm = TRUE)
  names_means <- names(means)
  m <- length(names_means)

  # Branch to handle two-stage and single-stage samples

  if (cluster_ind) {

    # Begin the section for a two-stage sample

    # Calculate additional required values

    cluster <- factor(clusterID)
    cluster_levels <- levels(cluster)
    ncluster <- length(cluster_levels)
    z_lst <- split(design$variables$colvar, cluster)
    wgt2_lst <- split(wgt, cluster)
    wgt1_u <- as.vector(tapply(wgt1, cluster, unique))
    tw2 <- (sum(wgt1 * wgt))^2
    x2_lst <- split(x, cluster)
    y2_lst <- split(y, cluster)
    x1_u <- as.vector(tapply(x1, cluster, unique))
    y1_u <- as.vector(tapply(y1, cluster, unique))
    if (pcfactor_ind) {
      N_cluster <- unique(Ncluster)
      stage1size_u <- as.vector(tapply(stage1size, cluster, unique))
    }
    var_ind <- sapply(split(cluster, cluster), length) > 1

    # Calculate estimates of the total of the stage two sampling unit residuals
    # and the variance/covariance of those totals for each stage one sampling unit

    total2est <- matrix(0, ncluster, m)
    var2est <- matrix(0, ncluster, m^2)
    for (i in 1:ncluster) {

      # Create the model matrix for the contingency table using using a single cluster

      design_var <- subset(design$variables, cluster == cluster_levels[i] &
        !(is.na(rowvar) | is.na(colvar)))
      mm_cluster <- model.matrix(frm_cells, model.frame(frm_cells, design_var,
        na.action = na.pass
      ))

      # Calculate the weighted residuals matrix

      n <- length(z_lst[[i]])
      m_cl <- ncol(mm_cluster)
      tst <- names_means %in% colnames(mm_cluster)
      rm <- (mm_cluster - matrix(rep(means[tst], n), nrow = n, byrow = TRUE)) *
        matrix(rep(wgt2_lst[[i]], m_cl), nrow = n)

      # Calculate total estimates for the stage one sampling unit

      total2est[i, tst] <- apply(rm, 2, sum)

      # Adjust the variance/covariance estimator for small sample size

      SRSind <- FALSE
      if (vartype == "Local" && n < 4) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator was used.\n"
        if (stratum_ind) {
          warn <- paste("There are less than four response values for stage one sampling unit ", cluster_levels[i], "\nin stratum ", stratum_level, ", the simple random sampling variance estimator \nwas used to calculate variance of the estimate.\n", sep = "")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = I(stratum_level),
            warning = I(warn), action = I(act)
          ))
        } else {
          warn <- paste("There are less than four response values for stage one sampling unit ", cluster_levels[i], ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe estimate.\n", sep = "")
          warn_df <- rbind(warn_df, data.frame(
            func = I(fname),
            subpoptype = warn_vec[1], subpop = warn_vec[2],
            indicator = warn_vec[3], stratum = NA, warning = I(warn),
            action = I(act)
          ))
        }
        vartype <- "SRS"
        SRSind <- TRUE
      }

      # Calculate the population correction factor for the stage two sample

      pcfactor <- ifelse(pcfactor_ind, (stage1size_u[i] - n) / stage1size_u[i], 1)

      # Calculate variance/covariance estimates for the stage one sampling unit

      if (var_ind[i]) {
        tst <- rep(tst, m)
        if (vartype == "Local") {
          weight_lst <- localmean_weight(
            x2_lst[[i]], y2_lst[[i]],
            1 / wgt2_lst[[i]]
          )
          if(is.null(weight_lst)) {
            warn_ind <- TRUE
            act <- "The simple random sampling variance estimator was used.\n"
            warn <- paste0("The local mean variance estimator cannot calculate valid estimates for stage one \nsampling unit \"", cluster_levels[i], "\", the simple random sampling variance estimator was used to \ncalculate variance of the category proportion estimates.\n")
            warn_df <- rbind(warn_df, data.frame(
              func = I(fname),
              subpoptype = warn_vec[1], subpop = warn_vec[2],
              indicator = warn_vec[3], stratum = stratum_level,
              warning = I(warn), action = I(act)
            ))
            var2est[i, tst] <- as.vector(pcfactor * n * var(rm))
          } else {
            var2est[i, tst] <- as.vector(pcfactor * localmean_cov(rm, weight_lst))
          }
        } else {
          var2est[i, tst] <- as.vector(pcfactor * n * var(rm))
          if (SRSind) {
            vartype <- "Local"
          }
        }
      }
    }

    # Adjust the variance estimator for small sample size

    if (vartype == "Local" && ncluster < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling variance estimator was used.\n"
      if (stratum_ind) {
        warn <- paste("There are less than four stage one sampling units in stratum ", stratum_level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe estimate.\n", sep = "")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop_1 = warn_vec[2],
          subpop_2 = warn_vec[3], indicator = warn_vec[4],
          stratum = I(stratum_level), warning = I(warn), action = I(act)
        ))
      } else {
        warn <- paste("There are less than four stage one sampling units, the simple random sampling \nvariance estimator was used to calculate variance of the estimate.\n", sep = "")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop_1 = warn_vec[2],
          subpop_2 = warn_vec[3], indicator = warn_vec[4], stratum = NA,
          warning = I(warn),
          action = I(act)
        ))
      }
      vartype <- "SRS"
    }

    # Calculate the population correction factor for the stage one sample

    pcfactor <- ifelse(pcfactor_ind, (N_cluster - ncluster) / N_cluster, 1)

    # Calculate the variance estimate

    if (vartype == "Local") {
      weight_lst <- localmean_weight(x1_u, y1_u, 1 / wgt1_u)
      if(is.null(weight_lst)) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator was used.\n"
        warn <- paste0("The local mean variance estimator cannot calculate valid estimates, the simple random \nsampling variance estimator was used to calculate variance of the category proportion \nestimates.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = stratum_level,
          warning = I(warn), action = I(act)
        ))
        varest <- (pcfactor * ncluster * var(total2est * matrix(rep(wgt1_u, m_cl),
          nrow = ncluster
        )) + matrix(apply(var2est * matrix(rep(wgt1_u, m_cl^2),
          nrow = ncluster
        ), 2, sum), nrow = m_cl)) / tw2
      } else {
        varest <- (pcfactor * localmean_cov(total2est * matrix(rep(wgt1_u, m_cl),
          nrow = ncluster
        ), weight_lst) + matrix(apply(var2est *
            matrix(rep(wgt1_u, m_cl^2), nrow = ncluster), 2, sum), nrow = m_cl)) / tw2
      }
    } else {
      varest <- (pcfactor * ncluster * var(total2est * matrix(rep(wgt1_u, m_cl),
        nrow = ncluster
      )) + matrix(apply(var2est * matrix(rep(wgt1_u, m_cl^2),
        nrow = ncluster
      ), 2, sum), nrow = m_cl)) / tw2
    }
    colnames(varest) <- names_means

    # End of section for a two-stage sample

  } else {

    # Begin the section for a single-stage sample

    # Calculate additional required values

    tw2 <- (sum(wgt))^2
    if (pcfactor_ind) {
      fpcsize_u <- unique(fpcsize)
    }

    # Calculate the weighted residuals matrix

    mm_cells <- subset(mm_cells, !(is.na(design$variables$rowvar) |
      is.na(design$variables$colvar)))
    n <- nrow(mm_cells)
    rm <- (mm_cells - matrix(rep(means, n), nrow = n, byrow = TRUE)) *
      matrix(rep(wgt, m), nrow = n)

    # Adjust the variance estimator for small sample size

    if (vartype == "Local" && n < 4) {
      warn_ind <- TRUE
      act <- "The simple random sampling variance estimator was used.\n"
      if (stratum_ind) {
        warn <- paste("There are less than four response values in stratum ", stratum_level, ", \nthe simple random sampling variance estimator was used to calculate variance of \nthe estimate.\n", sep = "")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop_1 = warn_vec[2],
          subpop_2 = warn_vec[3], indicator = warn_vec[4],
          stratum = I(stratum_level), warning = I(warn), action = I(act)
        ))
      } else {
        warn <- "\nThere are less than four response values, the simple random sampling variance \nestimator was used to calculate variance of the estimate.\n"
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop_1 = warn_vec[2],
          subpop_2 = warn_vec[3], indicator = warn_vec[4], stratum = NA,
          warning = I(warn), action = I(act)
        ))
      }
      vartype <- "SRS"
    }

    # Calculate the population correction factor

    pcfactor <- ifelse(pcfactor_ind, (fpcsize_u - n) / fpcsize_u, 1)

    # Calculate the variance estimate

    if (vartype == "Local") {
      weight_lst <- localmean_weight(x, y, 1 / wgt)
      if(is.null(weight_lst)) {
        warn_ind <- TRUE
        act <- "The simple random sampling variance estimator was used.\n"
        warn <- paste0("The local mean variance estimator cannot calculate valid estimates, the simple random \nsampling variance estimator was used to calculate variance of the category proportion \nestimates.\n")
        warn_df <- rbind(warn_df, data.frame(
          func = I(fname),
          subpoptype = warn_vec[1], subpop = warn_vec[2],
          indicator = warn_vec[3], stratum = stratum_level,
          warning = I(warn), action = I(act)
        ))
        varest <- pcfactor * n * var(rm) / tw2
      } else {
        varest <- pcfactor * localmean_cov(rm, weight_lst) / tw2
      }
    } else {
      varest <- pcfactor * n * var(rm) / tw2
    }
    colnames(varest) <- names_means

    # End section for a single-stage sample
  }

  # Return the number of values in each class, the variance estimate, degrees of
  # freedom for the variance estimate, the warning message indicator, and the
  # warn_df data frame

  list(varest = varest, vartype = vartype, warn_ind = warn_ind, warn_df = warn_df)
}
