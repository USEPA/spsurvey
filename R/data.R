#' New England Lakes
#'
#' A simple features (sf) point object of 5328 lakes created from document.....
#' The variables are:
#'
#' @format A data frame with 5328 rows and 7 variables:
#' \describe{
#'   \item{AREA_HA}{.....}
#'   \item{AREA_HA_CAT}{.....}
#'   \item{ELEVATION}{.....}
#'   \item{ELEVATION_CAT}{.....}
#'   \item{LAT_DD_N83}{.....}
#'   \item{LON_DD_N83}{.....}
#'   \item{geometry}{.....}
#' }
"NE_Lakes"

#' Illinois River
#'
#' A simple features (sf) multilinestring object of 9087 river segments created
#' from document..... The variables are:
#'
#' @format A data frame with 9087 rows and 3 variables:
#' \describe{
#'   \item{STATE_NAME}{.....}
#'   \item{SO_ID}{.....}
#'   \item{geometry}{.....}
#' }
"Illinois_River"

#' Great Lakes
#'
#' A simple features (sf) multipolygon object of 4113 polygons created
#' from document.....  The variables are:
#'
#' @format A data frame with 4113 rows and 5 variables:
#' \describe{
#'   \item{COUNTRY}{.....}
#'   \item{WTBDY_NM}{.....}
#'   \item{RSRC_CLASS}{.....}
#'   \item{PSTL_CODE}{.....}
#'   \item{AREA_SQKM}{.....}
#'   \item{geometry}{simple features geometry list-column}
#' }
"Great_Lakes"

#' NLA West
#'
#' A data frame of lake information from a "National Lakes Assessment" in the 
#' Western United States.
#'
#' @format A data frame with 851 rows and 18 variables:
#' \describe{
#'   \item{UNIQUE_ID}{.....}
#'   \item{SITE_ID}{.....}
#'   \item{DSGN_CYCLE}{.....}
#'   \item{LAT_DD83}{latitude NAD83.....} consider reloading
#'   \item{LON_DD83}{.....}
#'   \item{XCOORD}{transformed latitude coordinate}
#'   \item{YCOORD}{transformed longitude coordinate}
#'   \item{WGT_TP_EXTENT}{weight target pop extent}
#'   \item{WGT_SP_CORE}{weight sampled population CORE means sampled}
#'   \item{WGT_TP_CORE}{.....}
#'   \item{EVAL_CAT}{.....}
#'   \item{TNT_CAT}{.....}
#'   \item{URBN_NLA17}{.....}
#'   \item{STATE_NM}{.....}
#'   \item{BENTHIC_MMI}{.....}
#'   \item{BENTHIC_MMI_COND}{.....}
#'   \item{PHOSPHORUS_COND}{.....}
#'   \item{NITROGEN_COND}{.....}
#' }
"NLA_West"

#' NRSA EPA7
#'
#' A data frame of stream information from a "National Rivers and Streams Assessment"..... The variables:
#'
#' @format A data frame with 682 rows and 18 variables:
#' \describe{
#'   \item{UNIQUE_ID}{.....}
#'   \item{SITE_ID}{.....}
#'   \item{DSGN_CYCLE}{.....}
#'   \item{LAT_DD83}{.....}
#'   \item{LON_DD83}{.....}
#'   \item{XCOORD}{.....}
#'   \item{YCOORD}{.....}
#'   \item{WGT_TP_EXTENT}{.....}
#'   \item{WGT_SP_CORE}{.....}
#'   \item{WGT_TP_CORE}{.....}
#'   \item{EVAL_CAT}{.....}
#'   \item{TNT_CAT}{.....}
#'   \item{NA_LEVEL1_ECO_NM}{.....}
#'   \item{STATE_NM}{.....}
#'   \item{BENTHIC_MMI}{.....}
#'   \item{BENTHIC_MMI_COND}{.....}
#'   \item{PHOSPHORUS_COND}{.....}
#'   \item{NITROGEN_COND}{.....}
#' }
"NRSA_EPA7"

#' NCCA Gulf
#'
#' A data frame of gulf information from the "National Coastal Condition Assessment" ..... The variables:
#'
#' @format A data frame with 851 rows and 18 variables:
#' \describe{
#'   \item{UNIQUE_ID}{.....}
#'   \item{SITE_ID}{.....}
#'   \item{REVISIT}{.....}
#'   \item{DSGN_CYCLE}{.....}
#'   \item{LAT_DD83}{.....}
#'   \item{LON_DD83}{.....}
#'   \item{XCOORD}{.....}
#'   \item{YCOORD}{.....}
#'   \item{WGT_TP_EXTENT}{.....}
#'   \item{WGT_SP_CORE}{.....}
#'   \item{WGT_TP_CORE}{.....}
#'   \item{EVAL_CAT}{.....}
#'   \item{TNT_CAT}{.....}
#'   \item{SMALL_EST}{.....}
#'   \item{STATE_NM}{.....}
#'   \item{BENTHIC_MMI}{.....}
#'   \item{BENTHIC_MMI_COND}{.....}
#'   \item{FISH_TISSUE_COND}{.....}
#'   \item{PHOSPHORUS_COND}{.....}
#' }
"NCCA_Gulf"