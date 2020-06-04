################################################################################
# Function: grtspts_select
# Programmer:  Marc Weber, Tony Olsen
# Date: April 27, 2020
#
#' Select a grts sample.
#'
#' @param sframe The sf object containing variables: id and ip.
#'
#' @param grts_grid The hierarchical grid as a list required for GRTS design
#'
#' @param samplesize Sample size required.
#' 
#' @param over number of nearby sites to be used as potential replacement(s) 
#'       if a site cannot be sampled for any reason. If specified, typically 1 to 3.
#'       Default is NULL. In this function treated as either NULL or not NULL.
#'
#' @param SiteBegin Number to use for first site selected.
#' 
#' @param OverBegin Number to use for first over sample site selected.
#'
#' @param warn.ind  A logical value where TRUE indicates a warning message.
#'   Used for internal collection of messages only.
#'
#' @param warn.df A data frame containing messages warning of potential issues.
#'   Used for internal collection of messages only.
#'
#' @return A list of sf object of sample points, over sample points if any,
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
#' @export
################################################################################

grtspts_select2 <- function(sframe, grts_grid, samplesize, over = NULL, SiteBegin,
                           OverBegin = NULL, warn.ind = NULL, warn.df = NULL) {

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

  id <- pickFiniteSamplePoints2(rdx, xc, yc, dx, dy, sframe, over = over)
  rho <- sframe[match(id$id, sframe$id), ]
  over.samp <- NULL
  if(!is.null(over)) {
    over.samp <- sframe[match(id$id.over, sframe$id), ]
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

  rho <- rho[unique(floor(rho4 * np/4^nlev)) + 1.,]

  # Create siteID

  rho$siteID <- SiteBegin - 1 + 1:nrow(rho)
  if(!is.null(over)) {
    over.samp$siteID <- OverBegin - 1 + 1:nrow(over.samp)
  }

  # Return as a list the sample, plus over sample and warning messages
  rho <- list(rho = rho, over.samp = over.samp, warn.ind = warn.ind, warn.df = warn.df)

  invisible(rho)
}
