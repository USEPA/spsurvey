################################################################################
# Function: grtspts_mindis
# Programmer:  Tony Olsen
# Date: May 11, 2020
#
#' Select a grts sample with minimum distance between sites.
#'
#' @param mindis Minimum distance required between sites in sample.
#'
#' @param sites sf object with initial sites selected for survey design when
#'   mindis is ignored.
#'
#' @param sframe The sf object containing variables: id and ip.
#'
#' @param grts_grid The hierarchical grid as a list required for GRTS design
#'
#' @param SiteBegin Number to use for first site selected.
#'
#' @param maxtry Number of maximum attempts to ensure minimum distance between sites.
#'   Default is 10.
#'
#' @param stratum Character value for name of stratum.
#'   Used for internal collection of messages only.
#'
#' @param legacy_var Character value for name of column for legacy site variable.
#'   Default is NULL.
#'
#' @param warn.ind  A logical value where TRUE indicates a warning message.
#'   Used for internal collection of messages only.
#'
#' @param warn.df A data frame containing messages warning of potential issues.
#'   Used for internal collection of messages only.
#'
#' @return sf object of sample points.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{constructAddr}}{constructs the hierarchical address for
#'       sample points}
#'     \item{\code{ranho}}{constructs the randomized hierarchical address for
#'       sample points}
#'     \item{\code{pickGridCells}}{selects grid cells that get a sample point}
#'     \item{\code{\link{pickFiniteSamplePoints}}}{pick sample point(s) from
#'       selected cells}
#'   }
#'
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

grtspts_mindis <- function(mindis, sframe, grts_grid, samplesize, SiteBegin, maxtry = 10,
                           stratum = NULL, legacy_var = NULL, warn.ind = NULL, warn.df = NULL) {

  # save current ip as it will not change due to mindis site selection.
  # sframe$ip_init <- sframe$ip

  # select initial set of sites
  sites <- grtspts_select(sframe, grts_grid, samplesize = samplesize,
                          SiteBegin = SiteBegin,  warn.ind = warn.ind,
                          warn.df = warn.df)
  if(sites$warn.ind) {
    warn.ind <- sites$warn.ind
    warn.df <- sites$warn.df
    warn.df$stratum <- ifelse(is.na(warn.df$stratum), s, warn.df$stratum)
  }
  sites <- sites$rho

  # check distance between sites
  # below_min is TRUE if less than mindis
  site_dist <- st_distance(sites)
  class(site_dist) <- "numeric"
  below_min <- site_dist < mindis
  diag(below_min) <- FALSE

  # drop sites less than mindis in reverse hierarchical order
  # keep is TRUE if no site is within mindis and FALSE means drop site
  keep <- rep(TRUE, NROW(sites))
  nr <- NROW(sites)
  for (i in 1:nr) {
    for (j in nr:i) {
      if(below_min[i, j] == TRUE) {
        keep[j] <- FALSE
      }
    }
  }

  # if any legacy sites make sure they are included
  if(!is.null(legacy_var)) {
    keep[sites$legacy == TRUE] <- TRUE
  }

  # see if any sites are less than mindis and check until none or max tries
  ntry <- 1
  while(any(!keep)) {
    # identify sites that will be treated as legacy probability sites in sample frame
    sframe$probdis <- FALSE
    sframe$probdis[sframe$LAKE_ID %in% sites$LAKE_ID[keep]] <- TRUE

    # if any true legacy sites add them to sites to be kept
    if(!is.null(legacy_var)){
      sframe$probdis[sframe$legacy == TRUE] <- TRUE
    }

    # Adjust initial inclusion probabilities to account for current mindis sites
    # and any legacy sites
    sframe$ip <- grtspts_ipleg(sframe$ip_init, sframe$probdis)

    # adjust cell weights to use new inclusion probabilities
    grts_grid$cel.wt <- cellWeight(grts_grid$xc, grts_grid$yc,
                                   grts_grid$dx, grts_grid$dy, sframe)

    # select new sites that include legacy sites
    sites <- grtspts_select(sframe, grts_grid, samplesize = samplesize,
                            SiteBegin = SiteBegin,  warn.ind = warn.ind,
                            warn.df = warn.df)
    # check for warnings
    if(sites$warn.ind) {
      warn.ind <- sites$warn.ind
      warn.df <- sites$warn.df
      warn.df$stratum <- ifelse(is.na(warn.df$stratum), stratum, warn.df$stratum)
    }
    sites <- sites$rho

    # check distance between sites
    site_dist <- st_distance(sites)
    class(site_dist) <- "numeric"
    below_min <- site_dist < mindis
    diag(below_min) <- FALSE

    # identify sites less than mindis in reverse hierarchical order
    keep <- rep(TRUE, NROW(sites))
    nr <- NROW(sites)
    for (i in 1:nr) {
      for (j in nr:i) {
        if(below_min[i, j] == TRUE) {keep[j] <- FALSE}
      }
    }
    # if any legacy sites make sure they are included
    if(!is.null(legacy_var)) {
      keep[sites$legacy == TRUE] <- TRUE
    }

    if(ntry < maxtry){
      ntry <- ntry + 1
    } else {
      keep <- rep(TRUE, nr)
    }

    # check if maxtry reached. If so write out warning message
    if(ntry > maxtry) {
      warn <- paste0("Minimum distance between sites not attained after ", maxtry, " attempts.")
      if(warn.ind){
        warn.df <- rbind(warn.df, data.frame(stratum = stratum, func = I("grtspts_mindis"),
                                             warning = warn))
      } else {
        warn.ind <- TRUE
        warn.df <- data.frame(stratum = stratum, func = I("grtspts_mindis"), warning = warn)
      }
    }
  } # end of ntry loop

  # drop internal variables and replace ip with ip_init
  sites$ip <- sites$ip_init
  tmp <- names(sites)
  sites <- subset(sites, select = tmp[!(tmp %in% c("ip_init", "probdis", "geometry"))])

  sites <- list(sites = sites,  warn.ind = warn.ind, warn.df = warn.df)

  invisible(sites)

}



