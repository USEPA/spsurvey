################################################################################
# Function: spbalance
# Programmer: Tom Kincaid
# Date: February 17, 2012
# Last Revised: May 31, 2019
#
#' Calculate Spatial Balance Metrics for a Survey Design
#'
#' This function calculates spatial balance metrics for a survey design.    Two
#' options for calculation of spatial balance metrics are available: (1) use
#' proportions obtained from the intersection of Dirichlet tesselation polygons
#' for the sample points with the frame object and (2) use proportions obtained
#' from a rectangular grid superimposed on the sample points and the frame
#' object.  In both cases the proportions are used to calculate the spatial
#' balance metrics.  Two metrics are calculated: (1) the Pielou evenness measure
#' and (2) the chi-square statistic.
#'
#' @param spsample Object of class SpatialDesign produced by either the grts or
#'   irs functions that contains survey design information and additional
#'   attribute (auxiliary) variables.
#'
#' @param sfframe An object of class sf that contains the survey frame.
#'
#' @param tess_ind Logical variable indicating whether spatial balance metrics
#'   are calculated using proportions obtained from the intersection of
#'   Dirichlet tesselation polygons for the sample points with the frame object.
#'   TRUE means calculate the metrics.  FALSE means do not calculate the
#'   metrics. The default is TRUE.
#'
#' @param sbc_ind Logical variable indicating whether spatial balance metrics
#'   are calculated using proportions obtained from a rectangular grid
#'   superimposed on the sample points and the frame.  TRUE means calculate the
#'   metrics.  FALSE means do not calculate the metrics.  The default is FALSE.
#'
#' @param nrows Number of rows (and columns) for the grid of cells. The
#'   default is 5.
#'
#' @param dxdy Indicator for equal x-coordinate and y-coordinate grid cell
#'   increments, where TRUE means the increments are equal and FALSE means the
#'   increments are not equal.  The default is TRUE.
#'
#' @return  List containing the following components:
#'   \describe{
#'     \item{\code{tess}}{results for spatial balance metrics using
#'       tesselation polygons}
#'     \item{\code{sbc}}{results for spatial balance metrics using a
#'       rectangular grid}
#'   }
#'   If either the tess_ind or sbc_ind arguments are set to FALSE, the
#'   corresponding component in the list is set to NULL.  Otherwise, each
#'   components of the list is a lists that contains the following components:
#'   \describe{
#'     \item{\code{J_subp}}{Pielou evenness measure}
#'     \item{\code{chi_sq}}{Chi-square statistic}
#'     \item{\code{extent}}{frame extent for each  Dirichlet tesselation
#'       polygon or rectangular grid cell}
#'     \item{\code{prop}}{frame proportion for each Dirichlet tesselation
#'       polygon or rectangular grid cell}
#'   }
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{deldir}}}{deldir package function that computes the
#'       Delaunay triangulation and Dirichlet tesselation of a set of points}
#'     \item{\code{\link{tile.list}}}{deldir package function that extracts
#'       coordinates of the Dirichlet tesselation polygons from the object
#'       produced by the deldir function}
#'     \item{\code{\link{gIntersection}}}{rgeos package function that determines
#'       the intersection between two sp package objects}
#'     \item{\code{\link{LinesLength}}}{sp package function that determines
#'       length of the line segemnts in a class Lines object}
#'     \item{\code{\link{sbcframe}}}{function to calculate spatial balance grid
#'       cell extent and  proportions for a sample frame}
#'     \item{\code{\link{sbcsamp}}}{function to calculate spatial balance grid
#'       cell extent and proportions for a survey design}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @examples
#' \dontrun{
#' design <- list(
#'   Stratum1=list(panel=c(PanelOne=50), seltype="Equal", over=10),
#'   Stratum2=list(panel=c(PanelOne=50, PanelTwo=50), seltype="Unequal",
#'     caty.n=c(CatyOne=25, CatyTwo=25, CatyThree=25, CatyFour=25), over=75))
#' samp <- grts(design=design, DesignID="Test.Site", type.frame="area",
#'   src.frame="shapefile", in.shape="shapefile.shp", stratum="stratum",
#'   mdcaty="mdcaty", shapefile=TRUE, out.shape="sample.shp")
#' sframe <- read.shp("shapefile.shp")
#' spbalance(samp, sframe, sbc_ind = TRUE)
#' }
#'
#' @export
################################################################################

spbalance <- function(spsample, sfframe, tess_ind = TRUE, sbc_ind = FALSE,
   nrows = 5, dxdy = TRUE) {

# Obtain the sample x-coordinates, y-coordinates, and survey design weights,
# from the spsample object
xcoord <- spsample@data$xcoord
ycoord <- spsample@data$ycoord
wgt <- spsample@data$wgt
n <- nrow(spsample@data)

#
# Section for metrics calculted using Dirichlet tesselation polygons
#

if(tess_ind) {

# Determine whether an appropriate frame object was supplied

   if(is.null(sfframe))
      stop("\nAn object containing the survey design frame must be supplied as the sfframe \nargument.")
   if(!("sf" %in% class(sfframe)))
      stop("\nThe sfframe argument must be a member of class sf.")

# Obtain the bounding box from the sfframe object
   bbox <- st_bbox(sfframe)

# Create an sf object containing the Dirichlet tesselation polygons for the
# sample points
   tiles <- tile.list(deldir(xcoord, ycoord, rw=as.vector(bbox[c(1,3,2,4)])))
   sftess <- vector("list", n)
   for(i in 1:n) {
      nv <- length(tiles[[i]]$x)
      sftess[[i]] <- st_polygon(list(cbind(
         c(tiles[[i]]$x[1], tiles[[i]]$x[nv:1]),
         c(tiles[[i]]$y[1], tiles[[i]]$y[nv:1]))))
   }
   sftess <- st_sfc(sftess, crs = st_crs(sfframe))
   sftess <- st_sf(poly = 1:n, geometry = sftess)

# Join the sftess object with the sfframe object
  sftess <- st_join(sftess, sfframe)

# Calculate extent for each Dirichlet tesselation polygon

  if(all(st_geometry_type(sfframe) %in% c("POINT", "MULTIPOINT"))) {
    sftess$point_mdm <- 1
    extent <- with(sftess, tapply(point_mdm, poly, sum))
    extent[is.na(extent)] <- 0
  } else if(all(st_geometry_type(sfframe) %in% c("LINESTRING", "MULTILINESTRING"))) {
    sftess$length_mdm <- as.numeric(st_length(sftess))
    extent <- with(sftess, tapply(length_mdm, poly, sum))
    extent[is.na(extent)] <- 0
  } else {
    sftess$area_mdm <- as.numeric(st_area(sftess))
    extent <- with(sftess, tapply(area_mdm, poly, sum))
    extent[is.na(extent)] <- 0
  }

# Calculate proportion for Dirichlet tesselation polygon
  prop <- extent/sum(extent)

# Calculate the spatial balance metrics
   prob <- wgt/sum(wgt)
   J_subp <- sum(prop * log(prop))/sum(prob * log(prob))
   chi_sq <- sum(((prop - prob)^2)/prob)

# Print the spatial balance metrics
   cat("\nSpatial Balance Metric using Dirichlet Tesselation Polygons:\n")
   cat(paste("   Pielou evenness measure:", J_subp, "\n"))
   cat(paste("   Chi-square statistic:", chi_sq, "\n"))

# Create the output list
   tess <- list(J_subp=J_subp, chi_sq=chi_sq, extent=extent, prop=prop)

# Metrics calculated using Dirichlet tesselation polygons were not requested
} else {
   tess <- NULL
}

#
# Section for metrics calculted using a rectangular grid
#

if(sbc_ind) {

# Calculate grid cell extent and proportion for the frame
   sbc.frame <- sbcframe(sfframe, nrows, dxdy)

# Calculate grid cell extent and proportion for the sample
   sbc.sample <- sbcsamp(spsample, sbc.frame)

# Calculate the spatial balance metrics
   prop_f <- sbc.frame$prop[sbc.frame$prop != 0]
   prop_s <- sbc.sample$prop[sbc.sample$prop != 0]
   J_subp <- sum(prop_s * log(prop_s))/sum(prop_f * log(prop_f))
   ind <- sbc.frame$prop != 0
   prop_f <- sbc.frame$prop[ind]
   prop_s <- sbc.sample$prop[ind]
   chi_sq <- sum(((prop_s - prop_f)^2)/prop_f)

# Print the spatial balance metrics
   cat("\n\nSpatial Balance Metric using a Rectangular Grid:\n")
   cat(paste("   Pielou evenness measure:", J_subp, "\n"))
   cat(paste("   Chi-square statistic:", chi_sq, "\n"))

# Create the output list
   sbc <- list(J_subp=J_subp, chi_sq=chi_sq, extent_f=sbc.frame$extent,
               prop_f=sbc.frame$prop, extent_s=sbc.sample$extent,
               prop_s=sbc.sample$prop)

# Metrics calculated using a rectangular grid were not requested
} else {
   sbc <- NULL
}

# Return results
list(tess=tess, sbc=sbc)
}
