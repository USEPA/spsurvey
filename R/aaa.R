###############################################################################
# Function: aaa (not exported)
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
    "\nVersion 5.0 of the spsurvey package was loaded successfully.\n"
  )
}
