###############################################################################
# Function: get.address (exported)
# Programmer: Guillaume Chauvet
# Adapted by: Marc Weber
# Date: September 2, 2020
#
#' Get Heirarchical Address
#'
#' This function creates a tessellation of the two dimensional space, attributes
#' an address to each unit in the population and sorts the sampling frame in the 
#' order given by the addresses. This function was used directly from Guillaume Chauvet and Ronan Le Gleut 
#' as they describe in their paper Inference under pivotal sampling: Properties, 
#' variance estimation, and application to tesselation for spatial sampling,
#' Scand J Statist. 2020;1–24. 
#' 
#' GET ADDRESS: function to attriubute an address to each unit and to sort the 
#' sampling frame. The parameter rand allows a randomization of the tessellation. 
#' The output is the order in which the sampling frame should be sorted.
#' 
#' BETWEEN: function to project the coordinates between two bounds low and up.
#' 
#' @param x The x-coordinate value in the sample frame of points.
#'
#' @param y The y-coordinate value in the sample frame of points.
#'
#' @param rand Whether or not to randomize the addressing.  Default is \code{TRUE}
#'
#' @return A sorted sample frame.
#'
#' @author Marc Weber \email{Weber.Marc@epa.gov}
#'
#' @keywords survey
#'
#' @export
###############################################################################


get_address <- function(x, y, rand = TRUE){
  x <- trunc(between(x, 0, 2^31 - 1))
  y <- trunc(between(y, 0, 2^31 - 1))
  if(!rand){
    a <- 0L; b <- 1L; c <- 2L; d <- 3L
  }else{
    t <- sapply(1:32, function(i) sample(0:3))
    a <- rep(t[1, ], length(x)); 
    b <- rep(t[2, ], length(x)); 
    c <- rep(t[3, ], length(x)); 
    d <- rep(t[4, ], length(x)); 
  }
  x <- as.logical(rev(intToBits(x)))
  y <- as.logical(rev(intToBits(y)))
  t <- a * (!x & !y) + b * (!x & y) + c * (x & !y) + d * (x & y)
  l <- lapply(2:32L, function(i){rev(t[seq(i, length(t), by = 32L)])})
  do.call(order, l)
}

between <- function(x, low = 0, up = 1){
  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  low + (up - low) * (x - minx) / (maxx - minx)
} 

