###############################################################################
# Function: spbalance (exported)
# Programmer: Michael Dumelle
# Date: December 03, 2020
# Last Revised: December 03, 2020
# 
#' Calculate spatial balance metrics
#'
#' This function computes the spatial balance of a design with respect 
#' to the sample frame (\code{sframe}) using Dirichlet Tesselations, measuring the
#' extent to which a object is a miniature of \code{sframe}. This function is
#' applicable for unstratified or stratified designs with equal selection
#' probabilities.
#' 
#' @param object A design object output from \code{grts()} or \code{irs()}
#' having class \code{"design"}.
#' @param sframe An \code{sframe} or \code{sf} object.
#' @param sites The sites in object for which spatial balance metrics
#' are desired. This argument must be \code{"sites_base"}, \code{"sites_over"}, or 
#' \code{"sites_near"}. Defaults to \code{"sites_base"}. 
#' @param metrics A character vector of spatial balance metrics:
#' \itemize{
#'   \item{pielou - }{pielou evenness index (the default)}
#'   \item{simpsons - }{simpsons evenness index}
#'   \item{chisq - }{chi-squared loss}
#'   \item{pielou - }{absolute error loss}
#'  }
#'
#' @param extents Should the total extent within each dirichlet
#' tesselation be returned? Defaults to FALSE.
#'
#' @return A list having names equal to each strata.
#' @export
#'
#' @examples
#' \dontrun{
#' sample <- grts(NE_Lakes, 30)
#' spbalance(sample, NE_Lakes)
#' }
###############################################################################
spbalance <- function(object, sframe, sites = "sites_base", metrics = "pielou", extents = FALSE) {
   
   object_split <- split(object[[sites]], object[[sites]][["stratum"]])
   if (is.null(object$dsgn$Call$stratum_var)) {
      output <- lapply("None", function(x) {
         calculate_spbalance(object_split[[x]], sframe, sites, metrics, extents)
      })
      names(output) <- "None"
   } else {
      sframe_split <- split(sframe, sframe[[object$dsgn$stratum_var]])
      output <- lapply(object$dsgn$stratum, function(x) {
         calculate_spbalance(object_split[[x]], sframe_split[[x]], sites, metrics, extents)
      })
      names(output) <- object$dsgn$stratum
   }
   metrics_dfs <- lapply(names(output), function(x) cbind(stratum = x, data.frame(metric = metrics, value = output[[x]]$values)))
   metrics <- do.call("rbind", metrics_dfs)
   metrics <- metrics[order(metrics$metric), , drop = FALSE]
   row.names(metrics) <- NULL
   if (extents) {
      extent_sf_split <- lapply(names(output), function(x) {
         object_split[[x]]$extent <- output[[x]]$extent
         object_split[[x]][c("stratum", "extent")]
      })
      extents <- do.call("rbind", extent_sf_split)
      return(list(metrics = metrics, extents = extents))
   } else {
      return(metrics)
   }
   output
}


calculate_spbalance <- function(object_split, sframe_split, sites, metrics, extents) {
   
   # finding the sframe bounding box, this needs to be reordered to 
   # use in the Dirichlet Tesselation
   pop_bbox <- st_bbox(sframe_split)[c("xmin", "xmax", "ymin", "ymax")]
   
   # working with the objectd sites
   
   ## take the objectd sites coordinates
   samp_coords <- st_coordinates(object_split)
   ## name them X and Y
   colnames(samp_coords) <- c("X", "Y")
   ## isolate X
   samp_xcoord <- samp_coords[, "X"]
   ## isolate Y
   samp_ycoord <- samp_coords[, "Y"]
   ## recover the object size
   n <- nrow(object_split)
   
   # and the sframe sites
   
   
   ## recover the sframe size
   N <- nrow(sframe_split)
   
   # spatial balance with respect to the sframe
   
   tiles <- tile.list(deldir(x = samp_xcoord, y = samp_ycoord, rw = pop_bbox))
   
   ## using lapply instead of a loop
   sftess <- lapply(tiles, get_sftess) 
   sftess <- st_sfc(sftess, crs = st_crs(sframe_split))
   sftess <- st_sf(poly = 1:n, geometry = sftess)
   sftess <- st_join(sframe_split, sftess)
   
   # calculate counts 
   
   ## for points, sframe geometry must be point or multipoint
   if(all(st_geometry_type(sframe_split) %in% c("POINT", "MULTIPOINT"))) {
      ## storing a dummy variable to index counts by
      sftess$point_mdm <- 1
      ## summing over each polygon
      extent <- with(sftess, tapply(point_mdm, poly, sum))
      ## setting NA's equal to zero
      extent[is.na(extent)] <- 0
   } else if(all(st_geometry_type(sframe_split) %in% c("LINESTRING", "MULTILINESTRING"))) {
      sftess$length_mdm <- as.numeric(st_length(sftess))
      extent <- with(sftess, tapply(length_mdm, poly, sum))
      extent[is.na(extent)] <- 0
   } else if(all(st_geometry_type(sframe_split) %in% c("POLYGON", "MULTIPOLYGON"))) {
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
   if (extents) {
      output$extent <- extent
   }
   output
}

get_sftess <- function(tile) {
   ## finding the number of points in the bounding polygon
   npol <- length(tile$x)
   
   ## creating and returning the appropriate polygon after binding coords
   st_polygon(list(cbind(c(tile$x[1], tile$x[npol:1]), 
                         c(tile$y[1], tile$y[npol:1]))))
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