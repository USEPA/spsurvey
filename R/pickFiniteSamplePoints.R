################################################################################
# Function: pickFiniteSamplePoints
# Programmer: Tom Kincad and Tony Olsen
# Date: June 18, 2020
#
#' Select Sample Points from a Simple Features Point Object.This function selects
#'  sample points from an sf object of geometry type point.
#'
#' @param rdx Vector of cell IDs.
#'
#' @param xc Vector of x-coordinates for the grid cells.
#'
#' @param yc Vector of y-coordinates for the grid cells.
#'
#' @param dx The x-axis grid cell dimension.
#'
#' @param dy The y-axis grid cell dimension.
#'
#' @param sfobject The sf point object.
#' 
#' @param over.near number of nearby sites to be used as potential replacement(s) 
#'       if a site cannot be sampled for any reason. If specified, must be 1, 2 or 3.
#'       Default is NULL.
#'
#' @param warn.ind  A logical value where TRUE indicates a warning message.
#'   Used for internal collection of messages only.
#'
#' @param warn.df A data frame containing messages warning of potential issues.
#'   Used for internal collection of messages only.
#'
#' @return id.samp A list with potentially four items. First item is a list of three items:
#'   id - vector of feature IDs of the selected sample points, siteuse - character vector 
#'   with same length as id with value of "Base", replsite - vector with same length as id with
#'   all values equal to NA. Second item is a list of three vectors: id.near - vector of feature
#'   IDs for replacement sites within each cell if any requested (NULL otherwise), 
#'   siteuse.near - character vector designating order replacement sites will be used (potential
#'   values are First, Second, Third, Fourth, Fifth), replsite.near - vector same length as id.near
#'   with feature IDs of id that replacement sites may be used to replace sites that cannot be
#'   sampled. Last two items in the list are warn.ind that is logical value indicating whether
#'   warning messages were created during execution of function and warn.df as a data.frame 
#'   with the warning messages.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

pickFiniteSamplePoints <- function(rdx, xc, yc, dx, dy, sfobject, over.near = NULL,
                                   warn.ind = warn.ind, warn.df = warn.df) {

  # Determine feature IDs for selected sample points

  id <- NULL
  siteuse <- NULL
  replsite <- NULL
  id.near <- NULL
  siteuse.near <- NULL
  replsite.near <- NULL
  npt.over = over.near
  if(is.null(over.near))  npt.over = 0
  
  coords <- st_coordinates(sfobject)
  
  # select sites for each cell
  for(i in unique(rdx)) {
    # find the points in cell and subset
    xr <- c( xc[i] - dx, xc[i])
    yr <- c( yc[i] - dy, yc[i])
    tstcell <- (xr[1] < coords[,1]) & (coords[,1] <= xr[2]) &
      (yr[1] < coords[,2]) & (coords[,2] <= yr[2])
    sfcell <- sfobject[tstcell, ]

    # number of points in cell
    npt.cell <- NROW(sfcell)
    # number of sites to select in base sample within cell
    npt.sample <- sum(rdx == i)
    # number of points that can be selected
    n.select <- min(npt.sample + npt.over, npt.cell)
    # number of points with ip = 1
    nip1 <- sum(sfcell$ip == 1)
    id.ip1 <- sfcell$id[sfcell$ip == 1]
    
    # select sites required from available points in cell
    if(length(sfcell$id) > 1) {
      tmp <- sample(sfcell$id, n.select, prob = sfcell$ip)
      if(all(!(tmp %in% id.ip1))) tmp <- sample(sfcell$id, n.select, prob = sfcell$ip)
    } else { tmp <- sfcell$id}
    
    # if n.select == npt.sample + over.near - Great!
    # allocate sites to base and over ensuring that points with ip = 1 in base
    if(n.select > npt.sample) {
      tmp.nsamp <- tmp[tmp %in% id.ip1]
      if(npt.sample - nip1 > 0) {
        tmp.nsamp <- c(tmp.nsamp, tmp[!(tmp %in% id.ip1)][1:(npt.sample - nip1)])
      }
      id <- c(id, tmp.nsamp)
      siteuse <- c(siteuse, rep("Base", npt.sample))
      replsite <- c(replsite, rep(NA, npt.sample))
      n.over <- n.select - npt.sample
      if(n.over > 0) {
        id.near <- c(id.near, rep(tmp[!(tmp %in% tmp.nsamp)][1:n.over], npt.sample))
        siteuse.near <- c(siteuse.near, 
                          rep(c("First", "Second", "Third", "Fourth", "Fifth")[1:n.over], 
                              npt.sample))
        replsite.near <- c(replsite.near, rep(tmp.nsamp, each = n.over))
      }
    }
    
    if(n.select == npt.sample) {
      id <- c(id, tmp)
      siteuse <- c(siteuse, rep("Base", n.select))
      replsite <- c(replsite, rep(NA, n.select))
    }
    
    if(n.select < npt.sample) {
      id <- c(id, tmp)
      siteuse <- c(siteuse, rep("Base", n.select))
      replsite <- c(replsite, rep(NA, n.select))
      warn <- paste("The number of points to be selected from the cell with index value", i,
                    "exceeded the number of points in the cell.")
      if(warn.ind){
        warn.df <- rbind(warn.df, data.frame(func = I("pickFiniteSamplePoints.R"),
                                             warning = warn))
      } else {
        warn.ind <- TRUE
        warn.df <- data.frame(func = I("pickFiniteSamplePoints.R"), warning = warn)
      }
    }
  }

  # Return the vector of feature IDs
  id.samp <- list(id = list(id=id, siteuse = siteuse, replsite = replsite),
                  id.near = list(id.near=id.near, siteuse.near = siteuse.near,
                                 replsite.near = replsite.near),
                  warn.ind = warn.ind, warn.df = warn.df)
  
  return(id.samp)
}


