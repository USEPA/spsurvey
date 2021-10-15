###############################################################################
# Function: startup (not exported)
# Programmer: Tom Kincaid
# Date: July 17, 2002
# Last Revised: February 25, 2004
#
#' Successful startup message
#'
#' This is a function that displays a startup message.
#'
#' @param libname Data library name.
#' @param pkgname Package name.
#' @return A startup message
#' @noRd
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "spsurvey version 5.0.0 introduced significant changes to the inputs and outputs
of many functions. Please review the updated materials, vignettes, and documentation
by visiting https://cran.r-project.org/web/packages/spsurvey/index.html"
  )
}
