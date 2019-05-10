################################################################################
# Function: ranho
# Programmer: Tom Kincaid using original code by Denis White
# Date: April 25, 2019
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

  perm <- function (level, ind) {
    if (level > fin) return
    perms <- as.character (sample (1:4))
    a <- substr (hadr[ind], level, level)
    b <- ind[which (a == "1")]
    if (length (b) > 0) {
      perm (level+1, b)
      substr (hadr[b], level, level) <<- perms[1]
    }
    b <- ind[which (a == "2")]
    if (length (b) > 0) {
      perm (level+1, b)
      substr (hadr[b], level, level) <<- perms[2]
    }
    b <- ind[which (a == "3")]
    if (length (b) > 0) {
      perm (level+1, b)
      substr (hadr[b], level, level) <<- perms[3]
    }
    b <- ind[which (a == "4")]
    if (length (b) > 0) {
      perm (level+1, b)
      substr (hadr[b], level, level) <<- perms[4]
    }
    invisible ()
  }
  fin <- nchar (hadr[1])
  perm (1, 1:length(hadr))

# Return the addresses
 
  return(hadr)
}
