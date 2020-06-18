################################################################################
# Function: grtspts_select
# Programmer:  Marc Weber, Tony Olsen
# Date: June 18, 2020
#
#' Select a grts sample of size "samplesize" and with option to select "over.near" sites
#'   within each cell.
#'
#' @param sframe The sf object containing variables: id and ip.
#'
#' @param grts_grid The hierarchical grid as a list required for GRTS design
#'
#' @param samplesize Sample size required.
#' 
#' @param over.near number of nearby sites to be used as potential replacement(s) 
#'       if a site cannot be sampled for any reason. If specified, must 1, 2 or 3.
#'       Default is NULL.
#'
#' @param warn.ind  A logical value where TRUE indicates a warning message.
#'   Used for internal collection of messages only.
#'
#' @param warn.df A data frame containing messages warning of potential issues.
#'   Used for internal collection of messages only.
#'
#' @return sites A list of sf object of sample points, over sample points if any,
#'   warning indicator and warning messages
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
#' @author Marc Weber \email{Weber.Marc@epa.gov}
#'
#' @keywords survey
#' 
#' @return sites A list with components site.base for base sites, site.near for 
#'   replacement sites within each cell (NULL if none), warn.ind and warn.df 
#'
#' @export
################################################################################

grtspts_select <- function(sframe, grts_grid, samplesize, over.near = NULL, 
                           warn.ind = NULL, warn.df = NULL) {

  # simplify variables
  nlev <- grts_grid$nlev
  dx <- grts_grid$dx
  dy <- grts_grid$dy
  xc <- grts_grid$xc
  yc <- grts_grid$yc
  cel.wt <- grts_grid$cel.wt
  sint <- grts_grid$sint

  # Remove cells with zero weight

  indx <- cel.wt > 0
  xc <- xc[indx]
  yc <- yc[indx]
  cel.wt <- cel.wt[indx]

  # Construct the hierarchical address for all cells

  hadr <- constructAddr(xc, yc, dx, dy, nlev)

  # Construct randomized hierarchical addresses

  ranhadr <- ranho(hadr)

  # Determine order of the randomized hierarchical addresses

  rord <- order(ranhadr)

  # Select grid cells that get a sample point

  rstrt <- runif(1, 0, sint)
  ttl.wt <- c(0, cumsum(cel.wt[rord]))
  idx <- ceiling((ttl.wt - rstrt)/sint)
  smpdx <- pickGridCells(samplesize, idx)
  rdx <- rord[smpdx]
  n.cells <- length(unique(rdx))
  if(length(rdx) > n.cells) {
    temp <- sum(sapply(split(rdx, rdx), length) > 1)
    warn <- paste0("Of the ", n.cells, " grid cells from which sample points were selected, ",
                   temp, " (", round(100*temp/n.cells, 1),
                   "%) of the cells contained more than one sample point.")
    if(warn.ind) {
      warn.df <- rbind(warn.df, data.frame(stratum = NA, func = "grtspts_select",
                                           warning = warn))
    } else {
      warn.ind <- TRUE
      warn.df <- data.frame(stratum = NA, func = "grtspts_select", warning = warn)
    }
  }

  # Pick sample point(s) in selected cells

  id.samp <- pickFiniteSamplePoints(rdx, xc, yc, dx, dy, sframe, over.near = over.near,
                                warn.ind = warn.ind, warn.df = warn.df)
  idmatch <- match(id.samp[["id"]]$id, sframe$id)
  rho <- sframe[idmatch, ]
  rho$siteuse <- id.samp[["id"]]$siteuse
  rho$replsite <- id.samp[["id"]]$replsite
  sites.near <- NULL
  if(!is.null(over.near)) {
    idmatch <- match(id.samp[["id.near"]]$id.near, sframe$id)
    sites.near <- sframe[idmatch, ]
    sites.near$siteuse <- id.samp[["id.near"]]$siteuse.near
    sites.near$replsite <- id.samp[["id.near"]]$replsite.near
  }

  # Construct sample hierarchical address

  np <- nrow(rho)
  nlev <- max(1, trunc(logb(np,4)))
  ifelse(np == 4^nlev, nlev, nlev <- nlev + 1)
  ad <- matrix(0, 4^nlev, nlev)
  rv4 <- 0:3
  pwr4 <- 4.^(0.:(nlev - 1.))
  for(i in 1:nlev)
    ad[, i] <- rep(rep(rv4, rep(pwr4[i], 4.)),pwr4[nlev]/pwr4[i])
  rho4 <- as.vector(ad%*%matrix(rev(pwr4), nlev, 1))

  # Place sample in reverse hierarchical order

  sites.base <- rho[unique(floor(rho4 * np/4^nlev)) + 1.,]

  # Return as a list the sample, plus over sample and warning messages
  sites <- list(sites.base = sites.base, sites.near = sites.near, 
              warn.ind = warn.ind, warn.df = warn.df)

  invisible(sites)
}
