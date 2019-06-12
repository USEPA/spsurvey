################################################################################
# Function: ranho
# Programmer: Tom Kincaid using original code by Denis White
# Date: May 13, 2019
#
#' Construct Randomized Hierarchical Addresses for a Generalized
#' Random-Tesselation Stratified (GRTS) Survey Design
#'
#' This function constructs randomized hierarchical addresses for a GRTS survey
#' design.
#'
#' @param hadr Vector hierarchical addresses.
#'
#' @return Vector of randomized hierarchical addresses.
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################

ranho <- function(hadr) {

# Construct the randomized hierarchical addresses

  perm <- function (level, indx, fin, hadr) {
    if (level > fin) return
    perms <- as.character (sample (1:4))
    a <- substr (hadr[indx], level, level)
    b <- indx[which (a == "1")]
    if (length (b) > 0) {
      perm (level+1, b, fin, hadr)
      substr (hadr[b], level, level) <<- perms[1]
    }
    b <- indx[which (a == "2")]
    if (length (b) > 0) {
      perm (level+1, b, fin, hadr)
      substr (hadr[b], level, level) <<- perms[2]
    }
    b <- indx[which (a == "3")]
    if (length (b) > 0) {
      perm (level+1, b, fin, hadr)
      substr (hadr[b], level, level) <<- perms[3]
    }
    b <- indx[which (a == "4")]
    if (length (b) > 0) {
      perm (level+1, b, fin, hadr)
      substr (hadr[b], level, level) <<- perms[4]
    }
    invisible ()
  }
  fin <- nchar (hadr[1])
  perm (1, 1:length(hadr), fin, hadr)

# Return the addresses
 
  return(hadr)
}
