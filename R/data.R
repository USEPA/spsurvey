#' New England Lakes data
#'
#' An \code{sf} point object of 200 lakes in the Northeastern
#' United States.
#'
#' @format An \code{sf} object with 200 rows and 6 variables:
#' \describe{
#'   \item{\code{AREA}}{ Lake area in hectares.}
#'   \item{\code{AREA_CAT}}{ Lake area categories based on a hectare cutoff.}
#'   \item{\code{ELEV}}{ Elevation in meters.}
#'   \item{\code{ELEV_CAT}}{ Elevation categories based on a meter cutoff.}
#'   \item{\code{LEGACY}}{ Legacy site indicator.}
#'   \item{\code{geometry}}{ Point geometry using a NAD83 / Conus Albers
#'     projection (EPSG: 5070).}
#' }
"NE_Lakes"

#' New England Lakes data (as a data frame)
#'
#' An data frame of 200 lakes in the Northeastern
#' United States.
#'
#' @format An data frame with 200 rows and 7 variables:
#' \describe{
#'   \item{\code{AREA}}{ Lake area in hectares.}
#'   \item{\code{AREA_CAT}}{ Lake area categories based on a hectare cutoff.}
#'   \item{\code{ELEV}}{ Elevation in meters.}
#'   \item{\code{ELEV_CAT}}{ Elevation categories based on a meter cutoff.}
#'   \item{\code{LEGACY}}{ Legacy site indicator.}
#'   \item{\code{XCOORD}}{ x-coordinate using WGS 84
#'     (EPSG: 5070)}
#'   \item{\code{YCOORD}}{ y-coordinate using WGS 84
#'     (EPSG: 5070)}
#' }
"NE_Lakes_df"

#' Illinois River data
#'
#' An (\code{sf}) multilinestring object of 244 segments of the
#' Illinois River in Arkansas and Oklahoma.
#'
#' @format An \code{sf} multilinestring object object with 244 rows and 2
#' variables:
#' \describe{
#'   \item{\code{STATE_NAME}}{ State name.}
#'   \item{\code{geometry}}{ Multilinestring geometry using a NAD83 / Conus Albers
#'     projection (EPSG: 5070).}
#' }
"Illinois_River"

#' Illinois River legacy data
#'
#' An (\code{sf}) point object of legacy sites for  the Illinois
#' River data.
#'
#' @format An \code{sf} point object object with 5 rows and 2
#' variables:
#' \describe{
#'   \item{\code{STATE_NAME}}{ State name.}
#'   \item{\code{geometry}}{ Point geometry using a NAD83 / Conus Albers
#'     projection (EPSG: 5070).}
#' }
"Illinois_River_Legacy"

#' Lake Ontario data
#'
#' An \code{sf} multipolygon object of 187 polygons consisting
#' of shoreline segments in Lake Ontario.
#'
#' @format An \code{sf} multipolygon object with 187 rows and 5 variables:
#' \describe{
#'   \item{\code{COUNTRY}}{ Country.}
#'   \item{\code{RSRC_CLASS}}{ Bay class.}
#'   \item{\code{PSTL_CODE}}{ Postal code.}
#'   \item{\code{AREA_SQKM}}{ Area in square kilometers}
#'   \item{\code{geometry}}{ Multipolygon geometry using a NAD83 / Conus Albers
#'     projection (EPSG: 5070).}
#' }
"Lake_Ontario"

#' NLA PNW data
#'
#' An \code{sf} point object of 96 lakes in the Pacific Northwest Region of the United
#' States during the year 2017, from a subset of the Environmental
#' Protection Agency's "National Lakes Assessment."
#'
#' @format An \code{sf} point object with 96 rows and 9 variables:
#' \describe{
#'   \item{\code{SITE_ID}}{ A unique lake identifier.}
#'   \item{\code{WEIGHT}}{ The sampling design weight.}
#'   \item{\code{URBAN}}{ Urban category.}
#'   \item{\code{STATE}}{ State name.}
#'   \item{\code{BMMI}}{ Benthic MMI value.}
#'   \item{\code{BMMI_COND}}{ Benthic MMI condition categories.}
#'   \item{\code{PHOS_COND}}{ Phosphorus condition categories.}
#'   \item{\code{NITR_COND}}{ Nitrogen condition categories.}
#'   \item{\code{geometry}}{ Point geometry using a NAD83 / Conus Albers
#'     projection (EPSG: 5070).}
#' }
"NLA_PNW"

#' NRSA EPA7 data
#'
#' An \code{sf} point object of 353 stream segments in the Central
#' United States during the years 2008 and 2013, from a subset of the Environmental
#' Protection Agency's "National Rivers and Streams Assessment."
#'
#' @format A data frame with 353 rows and 10 variables:
#' \describe{
#'   \item{\code{SITE_ID}}{ A unique site identifier.}
#'   \item{\code{YEAR}}{ Year of design cycle.}
#'   \item{\code{WEIGHT}}{ Sampling design weights.}
#'   \item{\code{ECOREGION}}{ Ecoregion.}
#'   \item{\code{STATE}}{ State name.}
#'   \item{\code{BMMI}}{ Benthic MMI value.}
#'   \item{\code{BMMI_COND}}{ Benthic MMI categories.}
#'   \item{\code{PHOS_COND}}{ Phosphorus condition categories.}
#'   \item{\code{NITR_COND}}{ Nitrogen condition categories.}
#'   \item{\code{geometry}}{ Point geometry using a NAD83 / Conus Albers
#'     projection (EPSG: 5070).}
#' }
"NRSA_EPA7"
