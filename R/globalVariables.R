###############################################################################
# Function: globalVariables (exported)
# Programmers: Tom Kincaid
# Date: January 22, 2021
#' Define Global Variables
#'
#' @noRd
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "siteID", "weight", "xcoord", "ycoord", "stratumID",
    "clusterID", "weight1", "xcoord1", "ycoord1", "sweight", "sweight1",
    "fpcsize", "Ncluster", "stage1size", "Category", "Statistic", "rowvar",
    "colvar", "regest", "df.residual", "pt", "qt"
  ))
}
