#' New England Lakes data
#'
#' An \code{sf} point object of 5328 lakes in the Northeastern
#' United States.
#'
#' @format An \code{sf} object with 5328 rows and 6 variables:
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

#' Illinois River data
#'
#' A simple features (\code{sf}) multilinestring object of 9087 segments of the
#' Illinois River in Arkansas and Oklahoma.
#'
#' @format An \code{sf} multilinestring object object with 9087 rows and 3
#' variables:
#' \describe{
#'   \item{\code{STATE_NAME}}{ State name.}
#'   \item{\code{geometry}}{ Multilinestring geometry using a NAD83 / Conus Albers
#'     projection (EPSG: 5070).}
#' }
"Illinois_River"

#' Illinois River legacy data
#'
#' A simple features (\code{sf}) point object of legacy sites for  the Illinois
#' River data.
#'
#' @format An \code{sf} point object object with 5 rows and 1
#' variable:
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
#' @format An \code{sf} multipolygon object with 187 rows and 4 variables:
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
#' A data frame consisting lake observations from a subset of the Environmental
#' Protection Agency's "National Lakes Assessment" in the Pacific Northwest Region of the United
#' States during the year 2017.
#'
#' @format An \code{sf} point object with 851 rows and 9 variables:
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
#' An sf object consisting of stream observations from a subset of the Environmental
#' Protection Agency's "National Rivers and Streams Assessment" in the Central
#' United States duing the years 2008 and 2013.
#'
#' @format A data frame with 682 rows and 10 variables:
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
