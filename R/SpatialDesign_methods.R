################################################################################
# File: SpatialDesign_methods
# Programmer: Tom Kincaid
# Date: November 5, 2018
# Revised: July 3, 2019
#'
#' Class SpatialDesign
#'
#' Define S3 and S4 methods for summary and plot for class \code{SpatialDesign}.
#'
#' @name SpatialDesign-method
#'
#' @rdname SpatialDesign-class
#'
#' @param object \code{SpatialDesign} object.
#'
#' @param x \code{SpatialDesign} object.
#'
#' @param y Missing - this argument is not used.
#'
#' @param ... Arguments passed through.
#'
#' @param auxvar Vector containing the names of variables in the data slot of
#'   the \code{SpatialDesign} object that identify auxiliary variables to be
#'   used to summarize the survey design or create plots of the survey design.
#'    The default is NULL.
#'
#' @param sfframe Object of class \code{sf} that contains the survey design
#'   frame.  The default is NULL.
#'
#' @param tess_ind a logical variable indicating whether spatial balance
#'   metrics are calculated using proportions obtained from the intersection of
#'   Dirichlet tesselation polygons for the sample points with the frame object.
#'   TRUE means calculate the metrics.  FALSE means do not calculate the
#'   metrics.  The default is TRUE.
#'
#' @param sbc_ind a logical variable indicating whether spatial balance metrics
#'   are calculated using proportions obtained from a rectangular grid
#'   superimposed on the sample points and the frame.  TRUE means calculate the
#'   metrics. FALSE means do not calculate the metrics. The default is FALSE.
#'
#' @param nrows number of rows (and columns) for the grid of cells.  The default
#'   is 5.
#'
#' @param dxdy indicator for equal x-coordinate and y-coordinate grid cell
#'   increments, where TRUE means the increments are equal and FALSE means the
#'   increments are not equal.  The default is TRUE.
#'
#' @param stratum name of the attribute from the sfframe object that identifies
#'   stratum membership for each feature in the frame.  If stratum equals NULL,
#'   the design is unstratified, and an attribute named "stratum" (with all its
#'   elements equal to the stratum name specified in design) is added to the
#'   sfframe object.  The default is NULL.
#'
#' @param mdcaty name of the attribute from the sfframe object that identifies
#'   the unequal probability category for each feature in the survey frame.  The
#'   default is NULL.
#'
#' @param pdffile a character variable containing the name of the pdf file to
#'   which output is written. If a value is not provided, output is written to
#'   the graphics window.  The default is NULL.
#'
#' @param width width of the graphic region in inches.  The default is 8.
#'
#' @param height height of the graphic region in inches.  The default is 10.
#'
#' @section Extends:
#' Class \code{"SpatialPointsDataFrame"}, directly.\cr\cr
#' Class \code{"SpatialPoints"}, by class \code{"SpatialPointsDataFrame"}.\cr\cr
#' Class \code{"Spatial"}, by class \code{"SpatialPoints"}.
#'
#' @return A summary or plot depending on the method called.
################################################################################


################################################################################
# Create S3 Method for summary for Class SpatialDesign
#
#' @rdname SpatialDesign-class
#'
#' @aliases summary, SpatialDesign-method
#'
#' @method summary SpatialDesign
#'
#' @export
################################################################################

summary.SpatialDesign <- function(object, ..., auxvar = NULL, sfframe = NULL,
	tess_ind = TRUE, sbc_ind = FALSE, nrows = 5, dxdy = TRUE) {

# Ensure that object belongs to class SpatialDesign
	if(class(object) != "SpatialDesign")
			stop("\nThe object argument must be a member of class SpatialDesign.")

# Create the survey design summary
	dsum <- dsgnsum(object, auxvar = auxvar)

# Calculate spatial balance metrics for the survey design
	if(is.null(sfframe)) {
		spbal <- NULL
	} else {
		if(!("sf" %in% class(sfframe)))
			stop("\nThe sfframe argument must be a member of class sf.")
		spbal <- spbalance(object, sfframe, tess_ind, sbc_ind, nrows, dxdy)
	}

# Return the results list
	invisible(list("design summary" = dsum, "spatial balance statistics" = spbal))
}

################################################################################
# Create S4 Method for summary for Class SpatialDesign
#
#' @rdname SpatialDesign-class
#'
#' @aliases summary, SpatialDesign-method
#'
#' @export
################################################################################

setMethod("summary", signature(object = "SpatialDesign"),
	summary.SpatialDesign)

################################################################################
# Create S3 Method for plot for Class SpatialDesign
#
#' @rdname SpatialDesign-class
#'
#' @aliases plot, SpatialDesign-method
#'
#' @method plot SpatialDesign
#'
#' @export
################################################################################

plot.SpatialDesign <- function(x, y, ..., sfframe = NULL, stratum = NULL,
	mdcaty = NULL, auxvar = NULL, pdffile = NULL, width = 8, height = 10) {

# Ensure that x belongs to class SpatialDesign
	if(class(x) != "SpatialDesign")
		stop("\nThe x argument must be a member of class SpatialDesign.")

# Assign the sites (attributes) data frame and the design list from the
# SpatialDesign object

	sites <- x@data
	design <- x@design

# Determine whether an appropriate survey design frame object was supplied

	if(is.null(sfframe))
		stop("\nAn object containing the survey design frame must be supplied as the sfframe \nargument.")
	if(!("sf" %in% class(sfframe)))
		stop("\nThe sfframe argument must be a member of class sf.")

# Assign strata names and number of strata from the design list

	snames <- names(design)
	nstrata <- length(snames)

# If stratum equals NULL, ensure that the design list specifies a single
# stratum.  Otherwise, ensure that the name provided for stratum identifies an
# attribute in the sfframe object.

	if(is.null(stratum)) {
		if(length(snames) > 1)
			stop("\nThe attribute in the sfframe object that identifies stratum membership was not \nprovided and the design list specifies more than one stratum.")
	} else {
		temp <- match(stratum, names(sfframe), nomatch=0)
		if(temp == 0)
			stop(paste("\nThe value provided for the attribute in the sfframe object that identifies \nstratum membership for each feature in the survey frame, \"", stratum, "\", \ndoes not occur among the attributes in the sfframe object.", sep=""))
}

# Ensure that the stratum variable in the sites data frame is a factor

	if(!is.factor(sites$stratum))
		sites$stratum <- as.factor(sites$stratum)


# Ensure that unequal probability category is a factor, and assign category
# names and number of categories from the attributes data frame

	sites$mdcaty <- factor(sites$mdcaty)
	cnames <- levels(sites$mdcaty)
	ncaty <- length(cnames)

# Determine the selection type for each strata from the design list

	seltype <- character(nstrata)
	for(i in 1:nstrata) seltype[i] <- design[[i]]$seltype

# If mdcaty equals NULL, ensure that the sfframe object does not include unequal
# probability categories.  Otherwise, ensure that the name provided for mdcaty
# identifies an attribute in the sfframe object.

	if(is.null(mdcaty)) {
		if(length(cnames) > 1)
			stop("\nThe attribute in the sfframe object that identifies unequal probability \ncategories was not provided, and the sf object includes a column containing \nunequal probability categories.")
	} else {
		temp <- match(mdcaty, names(sfframe), nomatch=0)
		if(temp == 0)
			stop(paste("\nThe value provided for the attribute in the sfframe object that identifies \nunequal probability categories for each feature in the survey frame,\n\"", mdcaty, "\", does not occur among the attributes in the sfframe object.", sep=""))
	}

# Ensure that names provided by the auxvar argument occur in the attributes
# table of the SpatialDesign object

	if(!is.null(auxvar)) {
		temp <- match(auxvar, names(sites), nomatch=0)
		if(any(temp == 0)) {
			temp.str <- vecprint(auxvar[temp == 0])
			stop(paste("\nThe following values in the vector of auxiliary variable names do not occur \namong the columns in the data slot of the SpatialDesign object:\n", temp.str, sep=""))
		}
	}

# If requested, open the PDF file

	if(!is.null(pdffile))
		pdf(file = pdffile, width = width, height = height)

#
# This section handles finite survey designs
#

	if(all(st_geometry_type(sfframe) %in% c("POINT", "MULTIPOINT"))) {

# Plot the complete set of sample sites

   coords <- st_coordinates(sfframe)
		plot(coords, pch=20, xlab="x-coordinate", ylab="y-coordinate",
			main="Sample Sites", ...)
		points(sites$xcoord, sites$ycoord, pch=20, col="red")

# Plot sample sites by stratum

		if(nstrata > 1) {
			for(i in 1:nstrata) {
				ind <- sfframe[, stratum, drop = TRUE] == snames[i]
				plot(coords[ind,], pch=20, xlab="x-coordinate", ylab="y-coordinate",
				  main=paste("Sample Sites for Stratum:", snames[i]), ...)
				ind <- sites$stratum == snames[i]
				points(sites$xcoord[ind], sites$ycoord[ind], pch=20, col="red")
			}
		}

# Plot sample sites by design category

		if(all(seltype != "Continuous") & ncaty > 1) {
			for(i in 1:ncaty) {
				ind <- sfframe[, mdcaty, drop = TRUE] == cnames[i]
				plot(coords[ind,], pch=20, xlab="x-coordinate",
					ylab="y-coordinate", main=paste("Sample Sites for Design Category:", cnames[i]), ...)
				ind <- sites$mdcaty == cnames[i]
				points(sites$xcoord[ind], sites$ycoord[ind], pch=20, col="red")
			}
		}

# Plot sample sites color-coded by design category for each stratum

		if(nstrata > 1 & all(seltype != "Continuous") & ncaty > 1) {
			cols <- rainbow(ncaty, s=0.75)
			for(i in 1:nstrata) {
				ind <- sfframe[, stratum, drop = TRUE] == snames[i]
				plot(coords[ind,], pch=20, xlab="x-coordinate", ylab="y-coordinate",
				  main=paste("Sample Sites Color-Coded by Design Category \nStratum:",
				  snames[i]), ...)
				legend(x="bottomright", inset=0.05, legend=cnames, pch=20, col=cols)
				for(j in 1:ncaty) {
					ind <- sites$stratum == snames[i] & sites$mdcaty == cnames[j]
					points(sites$xcoord[ind], sites$ycoord[ind], pch=20, col=cols[j])
				}
			}
		}

# For each auxiliary variable, plot sample sites color-coded by category

		nvar <- length(auxvar)
		if(nvar > 0) {
			for(i in 1:nvar) {
				sites[,auxvar[i]] <- factor(sites[,auxvar[i]])
				cnames <- levels(sites[,auxvar[i]])
				ncaty <- length(cnames)
				cols <- rainbow(ncaty, s=0.75)
				plot(coords, pch=20, xlab="x-coordinate", ylab="y-coordinate",
				  main=paste("Sample Sites Color-Coded by", auxvar[i], "Category"), ...)
				legend(x="bottomright", inset=0.05, legend=cnames, pch=20, col=cols)
				for(j in 1:ncaty) {
					ind <- sites[,auxvar[i]] == cnames[j]
					points(sites$xcoord[ind], sites$ycoord[ind], pch=20, col=cols[j])
				}
			}
		}

#
# This section handles linear and area survey designs
#

	} else {

# Plot the complete set of sample sites

		plot(st_geometry(sfframe), axes=TRUE, pch=20, xlab="x-coordinate",
		  ylab="y-coordinate", main="Sample Sites", ...)
		points(sites$xcoord, sites$ycoord, pch=20, col="red")

# Plot sample sites by stratum

		if(nstrata > 1) {
			for(i in 1:nstrata) {
				ind <- sfframe[, stratum, drop = TRUE] == snames[i]
				plot(st_geometry(sfframe[ind,]), axes=TRUE, pch=20, xlab="x-coordinate",
					ylab="y-coordinate", main=paste("Sample Sites for Stratum:",
					snames[i]), ...)
				ind <- sites$stratum == snames[i]
				points(sites$xcoord[ind], sites$ycoord[ind], pch=20, col="red")
			}
		}

# Plot sample sites by design category

		if(all(seltype != "Continuous") & ncaty > 1) {
			for(i in 1:ncaty) {
				ind <- sfframe[, mdcaty, drop = TRUE] == cnames[i]
				plot(st_geometry(sfframe[ind,]), axes=TRUE, pch=20, xlab="x-coordinate",
					ylab="y-coordinate", main=paste("Sample Sites for Design Category:",
					cnames[i]), ...)
				ind <- sites$mdcaty == cnames[i]
				points(sites$xcoord[ind], sites$ycoord[ind], pch=20, col="red")
			}
		}

# Plot sample sites color-coded by design category for each stratum

		if(nstrata > 1 & all(seltype != "Continuous") & ncaty > 1) {
			cols <- rainbow(ncaty, s=0.75)
			for(i in 1:nstrata) {
				ind <- sfframe[, stratum, drop = TRUE] == snames[i]
				plot(st_geometry(sfframe[ind,]), axes=TRUE, pch=20, xlab="x-coordinate",
					ylab="y-coordinate", main=paste("Sample Sites Color-Coded by Design Category \nStratum:",
					snames[i]), ...)
				legend(x="bottomright", inset=0.05, legend=cnames, pch=20, col=cols)
				for(j in 1:ncaty) {
					ind <- sites$stratum == snames[i] & sites$mdcaty == cnames[j]
					points(sites$xcoord[ind], sites$ycoord[ind], pch=20, col=cols[j])
				}
			}
		}

# For each auxiliary variable, plot sample sites color-coded by category

		nvar <- length(auxvar)
		if(nvar > 0) {
			for(i in 1:nvar) {
				sites[,auxvar[i]] <- factor(sites[,auxvar[i]])
				cnames <- levels(sites[,auxvar[i]])
				ncaty <- length(cnames)
				cols <- rainbow(ncaty, s=0.75)
				plot(st_geometry(sfframe), axes=TRUE, pch=20, xlab="x-coordinate",
					ylab="y-coordinate", main=paste("Sample Sites Color-Coded by",
					auxvar[i], "Category"), ...)
				legend(x="bottomright", inset=0.05, legend=cnames, pch=20, col=cols)
				for(j in 1:ncaty) {
					ind <- sites[,auxvar[i]] == cnames[j]
					points(sites$xcoord[ind], sites$ycoord[ind], pch=20, col=cols[j])
				}
			}
		}
	}

# If necesssary, close the PDF file

	if(!is.null(pdffile))
		graphics.off()

}

################################################################################
# Create S4 Method for plot for Class SpatialDesign
#
#' @rdname SpatialDesign-class
#'
#' @aliases plot, SpatialDesign-method
#'
#' @export
################################################################################

setMethod("plot", signature(x = "SpatialDesign", y = "missing"),
	plot.SpatialDesign)
