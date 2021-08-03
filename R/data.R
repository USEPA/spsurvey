#' New England Lakes
#'
#' An \code{sf} point object of 5328 lakes in the Northeastern
#' United States.
#'
#' @format An \code{sf} object with 5328 rows and 6 variables:
#' \itemize{
#'   \item{AREA}{Lake area in hectares.}
#'   \item{AREA_CAT}{Lake area categories based on a hectare cutoff.}
#'   \item{ELEV}{Elevation in meters.}
#'   \item{ELEV_CAT}{Elevation categories based on a meter cutoff.}
#'   \item{LEGACY}{Legacy site indicator.}
#'   \item{geometry}{Point geometry using a USA Contiguous Albers Equal Area
#'     Conic projection.}
#' }
"NE_Lakes"

#' Illinois River
#'
#' A simple features (\code{sf}) multilinestring object of 9087 segments of the
#' Illinois River in Arkansas and Oklahoma.
#'
#' @format An \code{sf} multilinestring object object with 9087 rows and 3
#' variables:
#' \itemize{
#'   \item{STATE_NAME}{State name.}
#'   \item{geometry}{Multilinestring geometry using a USA Contiguous Albers
#'     Equal Area Conic projection.}
#' }
"Illinois_River"

#' Illinois River Legacy
#'
#' A simple features (\code{sf}) point object of legacy sites for  the Illinois
#' River data.
#'
#' @format An \code{sf} point object object with 5 rows and 1
#' variable:
#' \itemize{
#'   \item{STATE_NAME}{State name.}
#'   \item{geometry}{Point geometry using a USA Contiguous Albers
#'     Equal Area Conic projection.}
#' }
"Illinois_River_Legacy"

#' Lake Ontario
#'
#' An \code{sf} multipolygon object of 187 polygons consisting
#' of shoreline segments in Lake Ontario.
#'
#' @format An \code{sf} multipolygon object with 187 rows and 4 variables:
#' \itemize{
#'   \item{COUNTRY}{Country.}
#'   \item{RSRC_CLASS}{Bay class.}
#'   \item{PSTL_CODE}{Postal code.}
#'   \item{AREA_SQKM}{Area in square kilometers}
#'   \item{geometry}{simple features geometry list-column}
#' }
"Lake_Ontario"

#' NLA PNW
#'
#' A data frame consisting lake observations from a subset of the Environmental
#' Protection Agency's "National Lakes Assessment" in the Western United
#' States during the year 2017.
#'
#' @format An \code{sf} point object with 851 rows and 9 variables:
#' \itemize{
#'   \item{SITE_ID}{A unique lake identifier.}
#'   \item{WEIGHT}{The sampling design weight.}
#'   \item{URBAN}{Urban category.}
#'   \item{STATE}{State name.}
#'   \item{BMMI}{Benthic MMI value.}
#'   \item{BMMI_COND}{Benthic MMI condition categories.}
#'   \item{PHOS_COND}{Phosphorus condition categories.}
#'   \item{NITR_COND}{Nitrogen condition categories.}
#'   \item{geometry}{Point geometry using a USA Contiguous Albers Equal Area
#'     Conic projection.}
#' }
"NLA_PNW"

#' NRSA EPA7
#'
#' An sf object consisting of stream observations from a subset of the Environmental
#' Protection Agency's "National Rivers and Streams Assessment" in the Central
#' United States duing the years 2008 and 2013.
#'
#' @format A data frame with 682 rows and 10 variables:
#' \itemize{
#'   \item{SITE_ID}{A unique site identifier.}
#'   \item{YEAR}{Year of design cycle.}
#'   \item{WEIGHT}{Sampling design weights.}
#'   \item{ECOREGION}{Ecoregion.}
#'   \item{STATE}{State name.}
#'   \item{BMMI}{Benthic MMI value.}
#'   \item{BMMI_COND}{Benthic MMI categories.}
#'   \item{PHOS_COND}{Phosphorus condition categories.}
#'   \item{NITR_COND}{Nitrogen condition categories.}
#'   \item{geometry}{Point geometry using a USA Contiguous Albers Equal Area
#'     Conic projection.}
#' }
"NRSA_EPA7"
