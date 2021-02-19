#' New England Lakes
#'
#' A simple features (sf) point data.frame of 6121 lakes that was created
#' from a shapefile of lakes in the southern New England region of the U.S.
#' containing the area category in hectares and coordinates. The variables are as follows:
#'
#' @format A data frame with 6121 rows and 5 variables:
#' \describe{
#'   \item{xcoord}{x coordinate in meters in Albers (1832073 - 2136098)}
#'   \item{ycoord}{y coordinate in meters in Albers (2218907 - 2474343)}
#'   \item{State}{state lake point is in (CT, MA, RI)}
#'   \item{Area_Cat}{lake surface area category in hectares ((0,1],(1,5],
#'   (10,50],(5,10],(50,500], (500,1e+04])}
#'   \item{geometry}{simple features geometry list-column}
#' }
"NE_Lakes"
