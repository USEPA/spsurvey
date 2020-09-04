################################################################################
# Function: get.address
# Programmer: Guillaume Chauvet
# Adapted by: Marc Weber
# Date: September 2, 2020
#
#' This function was used directly from Guillaume Chauvet and Ronan Le Gleut 
#' as they describe in their paper Inference under pivotal sampling: Properties, 
#' variance estimation, and application to tesselation for spatial sampling,
#' Scand J Statist. 2020;1–24. 
#'
#' This function uses the pivotal tesselation method to perform a tessellation 
#' of the two dimensional space, attributes an address to each unit in the population 
#' and sorts the sampling frame in the order given by the addresses.
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
#' @param rand Whether or not to randomize the addressing.  Default is FALSE
#'
#' @return A sorted sample frame.
#'
#' @author Marc Weber \email{Weber.Marc@epa.gov}
#'
#' @keywords survey
#'
#' @export
################################################################################


get_address <- function(x, y, rand = FALSE){
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



###################################################################
# FUNCTION TO SORT A SAMPLING FRAME WITH MORE THAN TWO DIMENSIONS
###################################################################

# Simulated data
dim4 <- data.frame(a = rnorm(164), b = rnorm(164), c = rnorm(164), d = rnorm(164))


library(dplyr)
get_address2 <- function(df){
  df <- apply(df, 2, function(i) between(i,0, 2^31 - 1))
  df <- apply(df, 2, function(i) as.logical(rev(intToBits(i))))
  # List of all possible cases:
  cas0 <- c(TRUE,FALSE)
  cas <- merge(cas0, cas0)
  if(dim(df)[2] > 2){
    for(i in 3:dim(df)[2]){
      cas <- rbind(cbind(cas, cas0), cbind(cas, cas0 = rev(cas0)))
    }
  }
  names(cas) <- colnames(df)
  cas <- cas[do.call(order, cas),]
  cas$ord <- 0:(2^dim(df)[2] - 1)
  if(dim(df)[2] > 3) cas$ord[1:10] <- paste0('0', cas$ord[1:10])
  t <- as.data.frame(df) %>% full_join(cas, by = colnames(df)) %>% select(ord)
  l <- lapply(2:32L, function(i){rev(t[[1]][seq(i, length(t[[1]]), by = 32L)])})
  do.call(order, l)
}

# Order for the dim4 data set
ord <- get_address2(dim4)
