#' New England Lakes data
#'
#' An \code{sf} POINT object of 195 lakes in the Northeastern
#' United States.
#'
#' @format 195 rows and 5 variables:
#' \describe{
#'   \item{\code{AREA}}{ Lake area in hectares.}
#'   \item{\code{AREA_CAT}}{ Lake area categories based on a hectare cutoff.}
#'   \item{\code{ELEV}}{ Elevation in meters.}
#'   \item{\code{ELEV_CAT}}{ Elevation categories based on a meter cutoff.}
#'   \item{\code{geometry}}{ POINT geometry using the NAD83 / Conus Albers
#'     coordinate reference system (EPSG: 5070).}
#' }
"NE_Lakes"

#' New England Lakes legacy data
#'
#' An \code{sf} POINT object of 5 legacy sites for the NE Lakes data
#'
#' @format 5 rows and 5 variables:
#' \describe{
#'   \item{\code{AREA}}{ Lake area in hectares.}
#'   \item{\code{AREA_CAT}}{ Lake area categories based on a hectare cutoff.}
#'   \item{\code{ELEV}}{ Elevation in meters.}
#'   \item{\code{ELEV_CAT}}{ Elevation categories based on a meter cutoff.}
#'   \item{\code{geometry}}{ POINT geometry using the NAD83 / Conus Albers
#'     coordinate reference system (EPSG: 5070).}
#' }
"NE_Lakes_Legacy"

#' New England Lakes data (as a data frame)
#'
#' An data frame of 195 lakes in the Northeastern
#' United States.
#'
#' @format 195 rows and 6 variables:
#' \describe{
#'   \item{\code{AREA}}{ Lake area in hectares.}
#'   \item{\code{AREA_CAT}}{ Lake area categories based on a hectare cutoff.}
#'   \item{\code{ELEV}}{ Elevation in meters.}
#'   \item{\code{ELEV_CAT}}{ Elevation categories based on a meter cutoff.}
#'   \item{\code{XCOORD}}{ x-coordinate using the WGS 84 coordinate reference system
#'     (EPSG: 4326)}
#'   \item{\code{YCOORD}}{ y-coordinate using WGS 84 coordinate reference system
#'     (EPSG: 4326)}
#' }
"NE_Lakes_df"

#' Illinois River data
#'
#' An (\code{sf}) MULTILINESTRING object of 244 segments of the
#' Illinois River in Arkansas and Oklahoma.
#'
#' @format 244 rows and 2 variables:
#' \describe{
#'   \item{\code{STATE_NAME}}{ State name.}
#'   \item{\code{geometry}}{ MULTILINESTRING geometry using the NAD83 / Conus Albers
#'     coordinate reference system (EPSG: 5070).}
#' }
"Illinois_River"

#' Illinois River legacy data
#'
#' An (\code{sf}) POINT object of legacy sites for the Illinois
#' River data.
#'
#' @format 5 rows and 2 variables:
#' \describe{
#'   \item{\code{STATE_NAME}}{ State name.}
#'   \item{\code{geometry}}{ POINT geometry using the NAD83 / Conus Albers
#'     coordinate reference system (EPSG: 5070).}
#' }
"Illinois_River_Legacy"

#' Lake Ontario data
#'
#' An \code{sf} MULTIPOLYGON object of 187 polygons consisting
#' of shoreline segments in Lake Ontario.
#'
#' @format 187 rows and 5 variables:
#' \describe{
#'   \item{\code{COUNTRY}}{ Country.}
#'   \item{\code{RSRC_CLASS}}{ Bay class.}
#'   \item{\code{PSTL_CODE}}{ Postal code.}
#'   \item{\code{AREA_SQKM}}{ Area in square kilometers}
#'   \item{\code{geometry}}{ MULTIPOLYGON geometry using the NAD83 / Conus Albers
#'     coordinate reference system (EPSG: 5070).}
#' }
"Lake_Ontario"

#' NLA PNW data
#'
#' An \code{sf} POINT object of 96 lakes in the Pacific Northwest Region of the United
#' States during the year 2017, from a subset of the Environmental
#' Protection Agency's "National Lakes Assessment."
#'
#' @format 96 rows and 9 variables:
#' \describe{
#'   \item{\code{SITE_ID}}{ A unique lake identifier.}
#'   \item{\code{WEIGHT}}{ The sampling design weight.}
#'   \item{\code{URBAN}}{ Urban category.}
#'   \item{\code{STATE}}{ State name.}
#'   \item{\code{BMMI}}{ Benthic MMI value.}
#'   \item{\code{BMMI_COND}}{ Benthic MMI condition categories.}
#'   \item{\code{PHOS_COND}}{ Phosphorus condition categories.}
#'   \item{\code{NITR_COND}}{ Nitrogen condition categories.}
#'   \item{\code{geometry}}{ POINT geometry using the NAD83 / Conus Albers
#'     coordinate reference system (EPSG: 5070).}
#' }
"NLA_PNW"

#' NRSA EPA7 data
#'
#' An \code{sf} POINT object of 353 stream segments in the Central
#' United States during the years 2008 and 2013, from a subset of the Environmental
#' Protection Agency's "National Rivers and Streams Assessment."
#'
#' @format 353 rows and 10 variables:
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
#'   \item{\code{geometry}}{ POINT geometry using the NAD83 / Conus Albers
#'     coordinate reference system (EPSG: 5070).}
#' }
"NRSA_EPA7"
