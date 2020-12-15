################################################################################
# Function: spbalance
# Programmer: Michael Dumelle
# Date: December 03, 2020
# Last Revised: December 03, 2020
# 
#' Spatial Balance
#'
#' This function computes the spatial balance of a design with respect 
#' to the population using Dirichlet Tesselations, measuring the
#' extent to which a sample is a miniature of the population.
#' 
#' @param sample 
#' @param population 
#' @param stratum_sample 
#' @param stratum_population 
#' @param metrics 
#' @param return_extent 
#'
#' @return
#' @export
#'
#' @examples
spbalance <- function(sample, population, stratum_sample = "stratum", stratum_population, metrics = "pielou", return_extent = FALSE) {
   
   stratum <- sort(as.character(unique(sample[[stratum_sample]])))
   
   if (any(stratum != "None")) {
      if (!all.equal(stratum, sort(as.character(unique(population[[stratum_population]]))))) {
         stop("stratums in sample and population must be equal")
      }      
   }

   output <- lapply(stratum, calculate_spbalance, sample, population, stratum_sample, stratum_population, metrics, return_extent)
   
   names(output) <- stratum
   
   return(output)
}


calculate_spbalance <- function(stratum, sample, population, stratum_sample, stratum_population, metrics, return_extent) {
   
   if (any(stratum != "None")) {
      sample <- sample[sample[[stratum_sample]] == stratum, ]
      population <- population[population[[stratum_population]] == stratum, ]
   }
   
   # finding the population bounding box, this needs to be reordered to 
   # use in the Dirichlet Tesselation
   pop_bbox <- st_bbox(population)[c("xmin", "xmax", "ymin", "ymax")]
   
   # working with the sampled sites
   
   ## take the sampled sites coordinates
   samp_coords <- st_coordinates(sample)
   ## name them X and Y
   colnames(samp_coords) <- c("X", "Y")
   ## isolate X
   samp_xcoord <- samp_coords[, "X"]
   ## isolate Y
   samp_ycoord <- samp_coords[, "Y"]
   ## recover the sample size
   n <- nrow(sample)
   
   # and the population sites
   
   
   ## recover the population size
   N <- nrow(population)
   
   # spatial balance with respect to the population
   

      
   ## making the dirichlet polygon
   tiles <- deldir(x = samp_xcoord, 
                   y = samp_ycoord, 
                   rw = pop_bbox) %>% 
      ## storing it as a tile.list()
      tile.list(.)
   ## using lapply instead of a loop
   sftess <- lapply(tiles, function(t) {
      ## finding the number of points in the bounding polygon
      npol <- length(t$x)
      ## binding the coordinates
      return(cbind(c(t$x[1], t$x[npol:1]), 
                   c(t$y[1], t$y[npol:1])) %>% 
                ## storing as a list
                list(.) %>%
                ## storing as a st polygon
                st_polygon(.))
   }) %>% 
      ## adding in the popoulation level crs
      st_sfc(crs = st_crs(population)) %>%
      ## adding in poly as a variable and storing as an sf object
      st_sf(poly = 1:n, geometry = .) %>%
      ## joining the polygon bounds with the population data
      st_join(population, .)
   
   # calculate counts 
   
   ## for points, population geometry must be point or multipoint
   if(all(st_geometry_type(population) %in% c("POINT", "MULTIPOINT"))) {
      ## storing a dummy variable to index counts by
      sftess$point_mdm <- 1
      ## summing over each polygon
      extent <- with(sftess, tapply(point_mdm, poly, sum))
      ## setting NA's equal to zero
      extent[is.na(extent)] <- 0
   } else if(all(st_geometry_type(population) %in% c("LINESTRING", "MULTILINESTRING"))) {
     sftess$length_mdm <- as.numeric(st_length(sftess))
     extent <- with(sftess, tapply(length_mdm, poly, sum))
     extent[is.na(extent)] <- 0
   } else if(all(st_geometry_type(population) %in% c("POLYGON", "MULTIPOLYGON"))) {
     sftess$area_mdm <- as.numeric(st_area(sftess))
     extent <- with(sftess, tapply(area_mdm, poly, sum))
     extent[is.na(extent)] <- 0
   }

   # making proportions and expected quantities
   proportions <- extent / sum(extent)
   expected_proportions <- 1 / n
   
   
   # metrics
   values <- vapply(metrics, calculate_metric, double(1), proportions, expected_proportions)
   
    # returning the results when stored by the user
   output <- list(values = values)
   if (return_extent) {
      output$extent <- extent
   }
   
   return(output)
}


calculate_metric <- function(metric, proportions, expected_proportions) {
   switch(metric,
          pielou = calculate_pielou(proportions, expected_proportions),
          chisq = calculate_chisq(proportions, expected_proportions),
          abserr = calculate_abserr(proportions, expected_proportions),
          simpsons = calculate_simpsons(proportions, expected_proportions),
          stop("an invalid metric was provided")
          )
}

calculate_pielou <- function(proportions, expected_proportions) {
   pielou <- 1 + sum(proportions * log(proportions)) / log(1/expected_proportions) #1/E(p) = n$
   names(pielou) <- "pielou"
   return(pielou)
}

calculate_chisq <- function(proportions, expected_proportions) {
   chisq <- sum((proportions - expected_proportions)^2 / expected_proportions)
   names(chisq) <- "chisq"
   return(chisq)
}

calculate_abserr <- function(proportions, expected_proportions) {
   abserr <- sum(abs(proportions - expected_proportions) / expected_proportions)
   names(abserr) <- "abserr"
   return(abserr)
}

calculate_simpsons <- function(proportions, expected_proportions) {
   simpsons <- sum(proportions^2) - expected_proportions
   names(simpsons) <- "simpsons"
   return(simpsons)
}