#' Western Mountains Ecoregion Rivers and Streams
#'
#' A dataset containing attributes for Western Mountains Ecoregion rivers and 
#' streams sampled by the U.S. Environmental Protection Agency (EPA) during the
#' National Rivers and Streams Assessment (NRSA) surveys for 2004-2006 and
#' 2008-2009.
#'
#' @docType data
#'
#' @name NRSA_2009
#'
#' @usage NRSA_2009
#'
#' @format A data frame with 668 rows and 13 attributes:
#' \describe{
#'   \item{siteID}{site ID value.}
#'   \item{xcoord}{Albers projection x-coordinate.}
#'   \item{ycoord}{Albers projection y-coordinate.}
#'   \item{wgt}{survey design weight.}
#'   \item{Survey}{survey identifier, which is either WSA or NRSA.}
#'   \item{Revisit_Site}{identifier of revisit sites for the two surveys, where
#'     Y = a revisit site and N = not a revisit site.}
#'   \item{Stream_Size}{stream size category, which is either large or small.}
#'   \item{NTL}{total nitrogen concentration.}
#'   \item{PTL}{total phosphorus concentration.}
#'   \item{Benthic_MMI}{value of the benthic macroinvertebrate multimetric index
#'     (MMI).}
#'   \item{NTL_Cond}{condition class category of the total nitrogen value.}
#'   \item{PTL_Cond}{condition class category of the total phosphorus value.}
#'   \item{Benthic_MMI_Cond}{condition class category of the benthic MMI value.}
#' }
NULL
