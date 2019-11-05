################################################################################
# Function: grts
# Programmers: Tony Olsen, Tom Kincaid, Don Stevens, Christian Platt,
#              Denis White, Richard Remington
# Date: October 8, 2002
# Last Revised: October 30, 2019
#'
#' Select a Generalized Random-Tesselation Stratified (GRTS) Sample
#'
#' This function select a GRTS sample of a finite, linear, or area resource.
#' Frame elements must be located in 1- or 2-dimensional coordinate system.
#' Sample may be equal probability or unequal probability (either categorical or
#' proportional to auxiliary variable).  May designate panels of sites for
#' surveys over time.
#'
#' @param design Named list of stratum design specifications which are also
#'   lists.  Stratum names must be subset of values in stratum argument.  Each
#'   stratum list has four components:
#'   \describe{
#'     \item{panel}{named vector of sample sizes for each panel in stratum}
#'     \item{seltype}{the type of random selection, which must be one of
#'       following: "Equal" - equal probability selection, "Unequal" - unequal
#'       probability selection by the categories specified in caty.n and mdcaty,
#'       or "Continuous" - unequal probability selection proportional to
#'       auxiliary variable mdcaty}
#'     \item{caty.n}{if seltype equals "Unequal", a named vector of sample sizes
#'       for each category specified by mdcaty, where sum of the sample sizes
#'       must equal sum of the panel sample sizes, and names must be a subset of
#'       values in mdcaty}
#'     \item{over}{number of replacement sites ("oversample" sites) for the
#'       entire design, which is set equal to 0 if none are required)}
#'   }
#'   Example design for a stratified sample:\cr
#'     design=list(
#'       Stratum1=list(panel=c(PanelOne=50), seltype="Equal", over=10),
#'       Stratum2=list(panel=c(PanelOne=50, PanelTwo=50), seltype="Unequal",
#'         caty.n=c(CatyOne=25, CatyTwo=25, CatyThree=25, CatyFour=25),
#'         over=75))
#'   Example design for an unstratified sample:\cr
#'     design <- list(
#'       None=list(panel=c(Panel1=50, Panel2=100, Panel3=50), seltype="Unequal",
#'         caty.n=c("Caty 1"=50, "Caty 2"=25, "Caty 3"=25, "Caty 4"=25,
#'         "Caty 5"=75), over=100))
#'
#' @param DesignID Name for the design, which is used to create a site ID for
#'   each site.  The default is "Site".
#'
#' @param SiteBegin Number to use for first site in the design.  The default
#'   is 1.
#'
#' @param type.frame The type of frame, which must be one of following:
#'   "finite", "linear", or "area".  The default is NULL.
#'
#' @param src.frame Source of the frame, which equals "sf.object" if the frame
#'   is contained in an sf package object, "shapefile" if the frame is to be
#'   read from a shapefile, "sp.object" if the frame is obtained from an sp
#'   package object, or "att.frame" if type.frame equals "finite" and the
#'   frame is included in att.frame.  The default is "shapefile".
#'
#' @param in.shape Name of a shapefile containing the frame, which is required
#'   when src.frame equals "shapefile".  The shapefile name should include the
#'   ".shp" extension.  If the name does not include that extension, it will be
#'   added.  The default is NULL.
#'
#' @param sf.object An sf package object containing the frame, which is required
#'   when src.frame equals "sf.object".  The default is NULL.
#'
#' @param sp.object Name of the sp package object when src.frame equals
#'   "sp.object". The default is NULL.
#'
#' @param att.frame Data frame composed of attributes associated with elements
#'   in the frame.  If src.frame equals "att.frame", then att.frame must include
#'   columns that contain x-coordinates and y-coordinates for each element in
#'   the frame.  If src.frame does not equal "att.frame" and att.frame is not
#'   equal to NULL, then an sf object is created from att.frame and the geometry
#'   column from the object named "sf.object" that is created by the function.
#'   The default is NULL.
#'
#' @param id This argument is depricated.
#'
#' @param xcoord Character string containing the name of the column from
#'   att.frame that identifies x-coordinates when src.frame equals "att.frame".
#'   If xcoord equals NULL, then xcoord is given the value "x".  The default is
#'   NULL.
#'
#' @param ycoord Character string containing the name of the column from
#'   att.frame that identifies y-coordinates when src.frame equals "att.frame".
#'   If ycoord equals NULL, then ycoord is given the value "y".  The default is
#'   NULL.
#'
#' @param stratum Character string containing the name of the column from
#'   att.frame that identifies stratum membership for each element in the frame.
#'   If stratum equals NULL, the design is unstratified, and a column named
#'   "stratum" (with all of its elements equal to the stratum name specified in
#'   design) is added to att.frame.  The default is NULL.
#'
#' @param mdcaty Character string containing the name of the column from
#'   att.frame that identifies the unequal probability category for each element
#'   in the frame.  The default is NULL.
#'
#' @param startlev Initial number of hierarchical levels to use for the GRTS
#'   grid, which must be less than or equal to maxlev (if maxlev is specified)
#'   and cannot be greater than 11.  The default is NULL.
#'
#' @param maxlev Maxmum number of hierarchical levels to use for the GRTS
#'   grid, which cannot be greater than 11.  The default is 11.
#'
#' @param maxtry This argument is depricated.
#'
#' @param shift.grid Option to randomly shift the hierarchical grid, where
#'   TRUE means shift the grid and FALSE means do not shift the grid, which is
#'   useful if one desires strict spatial stratification by hierarchical grid
#'   cells.  The default is TRUE.
#'
#' @param do.sample Named vector that provides the option controlling sample
#'   selection for each stratum, where TRUE means select a sample from a stratum
#'   and FALSE means return the sample frame for a stratum in reverse
#'   hierarchical order.  Note that FALSE can only be used when type.frame
#'   equals "points" and seltype equals "Equal".  Names for the vector must
#'   match the names in design.  If the vector is not named, then the names in
#'   design are used.  The default is TRUE for each stratum.
#'
#' @param shapefile Option to create a shapefile containing the survey design
#'   information,  where TRUE equals create a shapefile and FALSE equals do not
#'   create a shapefile.  The default is TRUE.
#'
#' @param prjfilename This argument is depricated.
#'
#' @param out.shape  Name of the output shapefile.  The default is "sample.shp".
#'
#' @return  An object of class SpatialDesign containing the survey design
#'   information and any additional attribute variables that were provided.
#'   Optionally, a shapefile can be created that contains the survey design
#'   information.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{grtsarea}}}{select a GRTS sample of an area resource}
#'     \item{\code{\link{grtslin}}}{select a GRTS sample of a linear resource}
#'     \item{\code{\link{grtspts}}}{select a GRTS sample of a finite resource}
#'     \item{\code{\link{SpatialPoints}}}{sp package function to create an
#'       object of class SpatialPoints}
#'     \item{\code{\link{SpatialPointsDataFrame}}}{sp package function to create
#'       an object of class SpatialPointsDataFrame}
#'   }
#'
#' @author Tom Kincaid  email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @examples
#' \dontrun{
#'   test_design <- list(
#'     Stratum1=list(panel=c(PanelOne=50), seltype="Equal", over=10),
#'     Stratum2=list(panel=c(PanelOne=50, PanelTwo=50), seltype="Unequal",
#'       caty.n=c(CatyOne=25, CatyTwo=25, CatyThree=25, CatyFour=25), over=75))
#'   test.sample <- grts(design=test_design, DesignID="TestSite",
#'     type.frame="area", src.frame="shapefile", in.shape="test_shapefile.shp",
#'     stratum="test_stratum", mdcaty="test_mdcaty", shapefile=TRUE,
#'     out.shape="test_sample.shp")
#' }
#'
#' @export
################################################################################

grts <- function(design, DesignID = "Site", SiteBegin = 1, type.frame = NULL,
   src.frame = "shapefile", in.shape = NULL, sf.object = NULL, sp.object = NULL,
   att.frame = NULL, id = NULL, xcoord = NULL, ycoord = NULL, stratum = NULL,
   mdcaty = NULL, startlev = NULL, maxlev = 11, maxtry = NULL, shift.grid =
   TRUE, do.sample = rep(TRUE, length(design)), shapefile = TRUE, prjfilename =
   NULL, out.shape = "sample.shp") {

# Ensure that a design list is provided

if(is.null(design))
   stop("\nA design list must be provided.")

# Ensure that the design list is named and determine strata names from the
# design list

strata.names <- names(design)
if(is.null(strata.names)) {
   if(length(design) > 1) {
      stop("\nThe design list must be named.")
   } else {
      warning("\nSince the single stratum specified in the design list was not named, \n\"None\" will be used for the stratum name.\n")
      strata.names <- "None"
      names(design) <- strata.names
   }
}

# Ensure that type.frame contains a valid value

if(is.null(type.frame))
   stop("\nA value must be provided for argument type.frame.")
temp <- match(type.frame, c("finite", "linear", "area"), nomatch=0)
if(temp == 0)
   stop(paste("\nThe value provided for argument type.frame, \"", type.frame, "\", is not a valid value.", sep=""))

# Ensure that src.frame contains a valid value

temp <- match(src.frame, c("sf.object", "shapefile", "sp.object", "att.frame"),
   nomatch=0)
if(temp == 0)
   stop(paste("\nThe value provided for argument src.frame, \"", src.frame, "\", is not a valid value.", sep=""))

# If src.frame equals "shapefile", then create an sf object from the shapefile

if(src.frame == "shapefile") {
   if(is.null(shapefile))
      stop("\nA shapefile name is required when the value provided for argument src.frame \nequals \"shapefile\".")
   nc <- nchar(in.shape)
   if(substr(in.shape, nc-3, nc) != ".shp") {
      if(substr(in.shape, nc-3, nc-3) == ".") {
         in.shape <- paste(substr(in.shape, 1, nc-4), ".shp", sep="")
      } else {
         in.shape <- paste(in.shape, ".shp", sep="")
      }
   }
   sf.object <- st_read(in.shape, quiet = TRUE)
}

# If src,frame equals "sf.object", ensure that an sf object was provided

if(src.frame == "sf.object") {
   if(is.null(sf.object))
      stop("\nAn sf package object is required when the value provided for argument src.frame \nequals \"sf.object\".")
}

# If src.frame equals "sp.object", then create an sf object from the sp object

if(src.frame == "sp.object") {
   if(is.null(sp.object))
      stop("\nAn sp package object is required when the value provided for argument src.frame \nequals \"sp.object\".")
   sf.object <- st_as_sf(sp.object)
}

# If src.frame equals "att.frame", ensure that type.frame equals "finite",
# ensure that a data frame object is assigned to argument att.frame, and create
# an sf object from att.frame

if(src.frame == "att.frame") {
   if(type.frame != "finite") {
      stop(paste("\nThe value provided for argument type.frame must equal \"finite\" when argument \nsrc.frame equals \"att.frame\"  The value provided for argument type.frame was \n\"", type.frame, "\".", sep=""))
   }
   if(is.null(att.frame)) {
      stop(paste("\nA data frame object must be assigned to argument att.frame when argument\nsrc.frame equals \"att.frame\"."))
   }
   if(is.null(xcoord) | is.null(ycoord)) {
      stop(paste("\nValues must be provided for arguments xcoord and ycoord when argument src.frame \nequals \"att.frame\"."))
   }
   if(!(all(c(xcoord, ycoord) %in% names(att.frame)))) {
      stop(paste("\nThe values provided for arguments xcoord and ycoord do not occur among the \nnames for att.frame."))
   }
   sf.object <- st_as_sf(att.frame, coords = c(xcoord, ycoord))
}

# If src.frame does not equal "att.frame" and att.frame is not NULL, create an
# sf object composed of att.frame and the geometry column from sf.object

if(src.frame != "att.frame" & !is.null(att.frame)) {
   geom <- st_geometry(sf.object)
   sf.object <- st_set_geometry(att.frame, geom)
}

# Ensure that the class attribute for sf.object contains only the values "sf"
# and "data.frame"

class(sf.object) <- c("sf", "data.frame")

# Ensure that the geometry types for sf.object are consistent

temp <- st_geometry_type(sf.object)
tst <- all(temp %in% c("POINT", "MULTIPOINT")) |
       all(temp %in% c("LINESTRING", "MULTILINESTRING")) |
       all(temp %in% c("POLYGON", "MULTIPOLYGON"))
if(!tst) {
   stop(paste("\nThe geometry types for the survey frame object passed to function grts: \n\"", unique(st_geometry_type(sf.object)), "\" are not consistent.", sep=""))
}

# Create ID values

id <- "id"
sf.object$id <- 1:nrow(sf.object)

# If stratum equals NULL, ensure that the design list specifies a single stratum
# and add an attribute named "stratum" to sf.object.  Otherwise, ensure that the
# name provided for stratum identifies an attribute in sf.object.

if(is.null(stratum)) {
   if(length(strata.names) > 1)
      stop("\nThe attribute in sf.object that identifies stratum membership was not provided \nand the design list specifies more than one stratum.")
   stratum <- "stratum"
   sf.object$stratum <- factor(rep(strata.names, nrow(sf.object)))
} else {
   temp <- match(stratum, names(sf.object), nomatch=0)
   if(temp == 0)
      stop(paste("\nThe value provided for the attribute in sf.object that identifies stratum \nmembership for each feature, \"", stratum, "\", does not occur among the \nattributes in sf.object.", sep=""))
}

# Ensure that the stratum attribute in sf.object is a factor

if(!is.factor(sf.object$stratum))
   sf.object[, stratum] <- as.factor(sf.object[, stratum, drop = TRUE])

# Check whether strata names from the design list occur among the values for the
# stratum attribute in sf.object

temp <- match(strata.names, levels(sf.object[, stratum, drop = TRUE]),
   nomatch=0)
if(any(temp == 0)) {
   temp.str <- vecprint(strata.names[temp == 0])
   stop(paste("\nThe following strata names in the design list do not occur among the strata \nnames in the stratum attribute in sf.object:\n", temp.str, sep=""))
}

# If seltype is not "Equal" for every stratum, then do the following: (1) ensure
# that mdcaty is not NULL and (2) ensure that the name provided for mdcaty
# identifies an attribute in sf.object

seltype.ind <- FALSE
for(s in strata.names) {
   if(design[[s]]$seltype != "Equal") {
      seltype.ind <- TRUE
   }
}
if(seltype.ind) {
   if(is.null(mdcaty))
      stop(paste("\nThe name of the attribute in sf.object that identifies the unequal probability \ncategory for each feature must be provided.", sep=""))
   temp <- match(mdcaty, names(sf.object), nomatch=0)
   if(temp == 0)
      stop(paste("\nThe value provided for the attribute in sf.object that identifies the unequal \nprobability category for each feature, \"", mdcaty, "\", does not occur among \nthe attributes in sf.object.", sep=""))
}

# Ensure that startlev and maxlev are valid and compatible values

if(!is.null(startlev)) {
   if(startlev < 1)
      stop("\nThe value for startlev cannot be less than 1")
   if(startlev > 11)
      stop("\nThe value for startlev cannot be greater than 11")
   if(maxlev < 1)
      stop("\nThe value for maxlev cannot be less than 1")
   if(maxlev > 11)
      stop("\nThe value for maxlev cannot be greater than 11")
   if(startlev > maxlev)
      stop("\nThe value for startlev cannot be greater than the value for maxlev")
} else {
   if(maxlev < 1)
      stop("\nThe value for maxlev cannot be less than 1")
   if(maxlev > 11)
      stop("\nThe value for maxlev cannot be greater than 11")
}

# As necessary, initialize parallel processing

if(type.frame != "finite") {
   ncore <- detectCores()
   tst <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
   if(nzchar(tst)) {
      ncore <- ifelse(ncore == 1L, 1L, 2L)
   } else {
      temp <- as.integer(floor(0.1 * ncore))
      ncore <- ifelse(ncore == 1L, 1L, ifelse(temp == 0, ncore - 1L, ncore - temp))
   }
   cl <- makeCluster(ncore, methods=FALSE)
   invisible(clusterEvalQ(cl, library(sf)))
   setDefaultCluster(cl)
}

# Begin the section for a finite population (discrete points)

if(type.frame == "finite") {

   first <- TRUE
   SiteBegin <- SiteBegin

# Ensure that do.sample is the correct length and is named

   if(length(do.sample) > 1) {
      if(length(do.sample) != length(design))
         stop("\nArgument do.sample must be the same length as the design list.")
      if(is.null(names(do.sample))) {
         names(do.sample) <- strata.names
      } else {
         temp <- match(names(do.sample), strata.names, nomatch=0)
         if(any(temp) == 0)
            temp.str <- vecprint(names(do.sample)[temp == 0])
            stop(paste("\nThe following names in do.sample do not occur among the names in design:\n", temp.str, sep=""))
      }
   } else if(is.null(names(do.sample))) {
      names(do.sample) <- strata.names
   }

# Begin the loop for strata

   for(s in strata.names) {

      cat(paste("\nStratum:", s, "\n"))

# Create the sample frame

      temp <- sf.object[, stratum, drop = TRUE] == s
      grtspts.ind <- TRUE
      if(sum(temp) == 0) {
         warning(paste("\nThe stratum column in the attributes data frame contains no values that match \nthe stratum named \"", s, "\" in the design list.\n", sep=""))
         next
      } else if(sum(temp) == 1) {
         warning(paste("\nThe stratum column in the attributes data frame contains a single value that \nmatches the stratum named \"", s, "\" in the design list. \nThe sample for this stratum will be composed of a single point.\n", sep=""))
         grtspts.ind <- FALSE
      }

      sframe <- subset(sf.object, temp)
      if(design[[s]]$seltype == "Equal") {
         sframe$mdcaty <- "Equal"
      } else if(design[[s]]$seltype == "Unequal") {
         sframe$mdcaty <- factor(sframe[, mdcaty, drop = TRUE])
      } else if(design[[s]]$seltype == "Continuous") {
         sframe$mdcaty <- sframe[, mdcaty, drop = TRUE]
      } else {
         stop(paste("\nThe value provided for the type of random selection, \"", design[[s]]$seltype, "\", \nfor stratum \"", s, "\" is not valid.", sep=""))
      }

# If seltype is not "Equal", ensure that mdcaty contains valid values

      if(design[[s]]$seltype == "Unequal") {
         if(any(is.na(sframe$mdcaty)))
            stop(paste("\nMissing values were detected among the unequal probability category values for \nstratum \"", s, "\".", sep=""))
      } else if(design[[s]]$seltype == "Continuous") {
         if(any(is.na(sframe$mdcaty)))
            stop(paste("\nMissing values were detected among the unequal probability category values for \nstratum \"", s, "\".", sep=""))
         if(!is.numeric(sframe$mdcaty))
            stop(paste("\nThe type of random selection for stratum \"", s, "\" is \"Continuous\", \nbut the unequal probability category values are not numeric.", sep=""))
         if(any(sframe$mdcaty < 0))
            stop(paste("\nNonpositive values were detected among the unequal probability category values \nfor stratum \"", s, "\".", sep=""))
      }

# If seltype is "Unequal", ensure that caty.n is provided and that the names
# in caty.n are included amont the levels of mdcaty

      if(design[[s]]$seltype == "Unequal") {
         if(is.null(design[[s]]$caty.n))
            stop(paste("The type of random selection was set to \"Unequal\", but caty.n was not \nprovided for stratum \"", s, "\".", sep=""))
         temp <- match(names(design[[s]]$caty.n),
            levels(as.factor(sframe$mdcaty)), nomatch=0)
         if(any(temp == 0)) {
            temp.str <- vecprint(names(design[[s]]$caty.n)[temp == 0])
            stop(paste("\nThe following names in caty.n for stratum \"", s, "\" do not occur \namong the levels of the mdcaty variable in att.frame:\n", temp.str, sep=""))
         }
      }

# Ensure that panel and caty.n contain valid values

      if(!is.numeric(design[[s]]$panel))
         stop(paste(" The design list must contain numeric values in the panel argument for \nstratum \"", s, "\".\n", sep=""))
      design[[s]]$panel <- round(design[[s]]$panel)
      design[[s]]$panel <- design[[s]]$panel[design[[s]]$panel > 0]
      if(length(design[[s]]$panel) == 0)
         stop(paste(" The design list does not not contain any valid values of the panel \nargument for stratum \"", s, "\".\n", sep=""))

      if(design[[s]]$seltype == "Unequal") {
         if(!is.numeric(design[[s]]$caty.n))
            stop(paste(" The design list must contain numeric values in the caty.n argument for \nstratum \"", s, "\".\n", sep=""))
         design[[s]]$caty.n <- round(design[[s]]$caty.n)
         design[[s]]$caty.n <- design[[s]]$caty.n[design[[s]]$caty.n > 0]
         if(length(design[[s]]$caty.n) == 0)
            stop(paste(" The design list does not not contain any valid values of the caty.n \nargument for stratum \"", s, "\".\n", sep=""))
      }

# As necessary, remove rows from sframe that have values of mdcaty which are not
# included among the names in caty.n

      if(design[[s]]$seltype == "Unequal") {
         temp <- sframe$mdcaty %in% names(design[[s]]$caty.n)
         if(any(!temp)) {
            sframe <- sframe[temp,]
         }
      }

# Determine overall sample size for the stratum

      if(is.null(design[[s]]$over))
         design[[s]]$over <- 0
      if(design[[s]]$seltype != "Unequal") {
         samplesize <- sum(design[[s]]$panel)
         n.desired <- sum(samplesize, design[[s]]$over)
      } else {
         if(sum(design[[s]]$panel) != sum(design[[s]]$caty.n))
            stop("\nThe sum of panel sample sizes does not equal sum of caty.n sample sizes")
         samplesize <- sum(design[[s]]$caty.n)
         if(design[[s]]$over == 0) {
            n.desired <- design[[s]]$caty.n
         } else {
            over.n <- design[[s]]$over * design[[s]]$caty.n /
               sum(design[[s]]$caty.n)
            if(any(over.n != floor(over.n)))
               warning(paste("\nOversample size is not proportional to category sample sizes for stratum\n\"", s, "\".\n", sep=""))
            n.desired <- design[[s]]$caty.n + ceiling(over.n)
         }
      }

# Calculate mdm - inclusion probabilities

      if(design[[s]]$seltype == "Equal")
         	sframe$mdm <- mdmpts(sframe$mdcaty, c(Equal=n.desired))
      else if(design[[s]]$seltype == "Unequal")
         sframe$mdm <- mdmpts(sframe$mdcaty, n.desired)
      else
         sframe$mdm <- n.desired * sframe$mdcaty / sum(sframe$mdcaty)

# Select the sample

      st_agr(sframe) <- "constant"
      if(grtspts.ind) {
         stmp <- grtspts(sframe, sum(n.desired), SiteBegin,
            shift.grid, do.sample[s], startlev, maxlev)
      } else {
         stmp <- sframe
         stmp$siteID <- SiteBegin
         stmp$wgt <- 1/sframe$mdm
         stmp <- subset(stmp, select = c("siteID", "id", "mdcaty", "wgt"))
         row.names(stmp) <- 1
         attr(stmp, "nlev") <- NA
      }

# Determine whether the realized sample size is less than the desired size

      if(nrow(stmp) < sum(n.desired))
         warning(paste("\nThe size of the selected sample was less than the desired size for stratum\n\"", s, "\".\n", sep=""))

# Add the stratum variable

      stmp$stratum <- as.factor(rep(s,nrow(stmp)))

# Add panel and oversample structure

      stmp$panel <- as.character(rep("OverSamp",nrow(stmp)))
      n.panel <- length(design[[s]]$panel)
      if(nrow(stmp) < samplesize) {
         n.short <- samplesize - nrow(stmp)
         n.temp <- n.short / n.panel
         if(n.temp != floor(n.temp)) {
            n.temp <- c(ceiling(n.temp), rep(floor(n.temp), n.panel-1))
            i <- 1
            while(sum(n.temp) != n.short) {
               i <- i+1
               n.temp[i] <- n.temp[i] + 1
            }
         }
         np <- c(0, cumsum(design[[s]]$panel - n.temp))
      } else {
         np <- c(0, cumsum(design[[s]]$panel))
      }
      for(i in 1:n.panel)
         stmp$panel[(np[i]+1):np[i+1]] <- names(design[[s]]$panel[i])

# If an oversample is present or the realized sample size is less than the
# desired size, then adjust the weights

      if(design[[s]]$over > 0 || nrow(stmp) < samplesize) {
         if(design[[s]]$seltype != "Unequal") {
            if(nrow(stmp) < samplesize) {
               stmp$wgt <- n.desired * stmp$wgt / nrow(stmp)
            } else {
               stmp$wgt <- n.desired * stmp$wgt / samplesize
            }
         } else {
            if(nrow(stmp) < samplesize) {
               n.caty <- length(design[[s]]$caty.n)
               n.temp <- n.short / n.caty
               nc <- design[[s]]$caty.n - n.temp
            } else {
               nc <- design[[s]]$caty.n
            }
            for(i in names(n.desired)) {
               stmp$wgt[stmp$mdcaty == i] <- n.desired[i] *
                  stmp$wgt[stmp$mdcaty == i] / nc[i]
            }
         }
      }

# Add stratum sample to the output object

      if(first) {
         sites <- stmp
         levels(sites$stratum) <- strata.names
         first <- FALSE
      } else {
         sites <- rbind(sites, stmp)
      }
      SiteBegin <- SiteBegin + nrow(stmp)

# End the loop for strata

   }

# End the section for a finite population (discrete points)

} else if(type.frame == "linear") {

# Begin the section for a linear network

   first <- TRUE
   SiteBegin <- SiteBegin
   sf.object$length_mdm <- as.numeric(st_length(sf.object))

# Begin the loop for strata

   for(s in strata.names) {

      cat(paste("\nStratum:", s, "\n"))

# Create the sample frame

      temp <- sf.object[, stratum, drop = TRUE] == s
      if(sum(temp) == 0) {
         warning(paste("\nThe stratum column in the attributes data frame contains no values that match \nthe stratum named \"", s, "\" in the design list.\n", sep=""))
         next
      }

      sframe <- subset(sf.object, temp)
      if(design[[s]]$seltype == "Equal") {
         sframe$mdcaty <- "Equal"
      } else if(design[[s]]$seltype == "Unequal") {
         sframe$mdcaty <- factor(sframe[, mdcaty, drop = TRUE])
      } else if(design[[s]]$seltype == "Continuous") {
         sframe$mdcaty <- sframe[, mdcaty, drop = TRUE]
      } else {
         stop(paste("\nThe value provided for the type of random selection, \"", design[[s]]$seltype, "\", \nfor stratum \"", s, "\" is not valid.", sep=""))
      }

# If seltype is not "Equal", ensure that mdcaty contains valid values

      if(design[[s]]$seltype == "Unequal") {
         if(any(is.na(sframe$mdcaty)))
            stop(paste("\nMissing values were detected among the unequal probability category values for \nstratum \"", s, "\".", sep=""))
      } else if(design[[s]]$seltype == "Continuous") {
         if(any(is.na(sframe$mdcaty)))
            stop(paste("\nMissing values were detected among the unequal probability category values for \nstratum \"", s, "\".", sep=""))
         if(!is.numeric(sframe$mdcaty))
            stop(paste("\nThe type of random selection for stratum \"", s, "\" is \"Continuous\", \nbut the unequal probability category values are not numeric.", sep=""))
         if(any(sframe$mdcaty < 0))
            stop(paste("\nNonpositive values were detected among the unequal probability category values \nfor stratum \"", s, "\".", sep=""))
      }

# If seltype is "Unequal", ensure that caty.n is provided and that the names
# in caty.n and the levels of mdcaty are equivalent

      if(design[[s]]$seltype == "Unequal") {
         if(is.null(design[[s]]$caty.n))
            stop(paste("The type of random selection was set to \"Unequal\", but caty.n was not \nprovided for stratum \"", s, "\".", sep=""))
         temp <- match(names(design[[s]]$caty.n),
            levels(as.factor(sframe$mdcaty)), nomatch=0)
         if(any(temp == 0)) {
            temp.str <- vecprint(names(design[[s]]$caty.n)[temp == 0])
            stop(paste("\nThe following names in caty.n for stratum \"", s, "\" do not occur \namong the levels of the mdcaty variable in att.frame:\n", temp.str, sep=""))
         }
      }

# Ensure that panel and caty.n contain valid values

      if(!is.numeric(design[[s]]$panel))
         stop(paste(" The design list must contain numeric values in the panel argument for \nstratum \"", s, "\".\n", sep=""))
      design[[s]]$panel <- round(design[[s]]$panel)
      design[[s]]$panel <- design[[s]]$panel[design[[s]]$panel > 0]
      if(length(design[[s]]$panel) == 0)
         stop(paste(" The design list does not not contain any valid values of the panel \nargument for stratum \"", s, "\".\n", sep=""))

      if(design[[s]]$seltype == "Unequal") {
         if(!is.numeric(design[[s]]$caty.n))
            stop(paste(" The design list must contain numeric values in the caty.n argument for \nstratum \"", s, "\".\n", sep=""))
         design[[s]]$caty.n <- round(design[[s]]$caty.n)
         design[[s]]$caty.n <- design[[s]]$caty.n[design[[s]]$caty.n > 0]
         if(length(design[[s]]$caty.n) == 0)
            stop(paste(" The design list does not not contain any valid values of the caty.n \nargument for stratum \"", s, "\".\n", sep=""))
      }

# As necessary, remove rows from sframe that have values of mdcaty which are not
# included among the names in caty.n

      if(design[[s]]$seltype == "Unequal") {
         temp <- sframe$mdcaty %in% names(design[[s]]$caty.n)
         if(any(!temp)) {
            sframe <- sframe[temp,]
         }
      }

# Determine overall sample size for the stratum

      if(is.null(design[[s]]$over))
         design[[s]]$over <- 0
      if(design[[s]]$seltype != "Unequal") {
         samplesize <- sum(design[[s]]$panel)
         n.desired <- sum(samplesize, design[[s]]$over)
      } else {
         if(sum(design[[s]]$panel) != sum(design[[s]]$caty.n))
            stop("\nThe sum of panel sample sizes does not equal sum of caty.n sample sizes")
         samplesize <- sum(design[[s]]$caty.n)
         if(design[[s]]$over == 0) {
            n.desired <- design[[s]]$caty.n
         } else {
            over.n <- design[[s]]$over * design[[s]]$caty.n /
               sum(design[[s]]$caty.n)
            if(any(over.n != floor(over.n)))
               warning(paste("\nOversample size is not proportional to category sample sizes for stratum\n\"", s, "\".\n", sep=""))
            n.desired <- design[[s]]$caty.n + ceiling(over.n)
         }
      }

# Calculate mdm - inclusion probabilities

      if(design[[s]]$seltype == "Equal")
         sframe$mdm <- mdmlin(sframe$len, sframe$mdcaty, c(Equal=n.desired))
      else if(design[[s]]$seltype == "Unequal")
         sframe$mdm <- mdmlin(sframe$len, sframe$mdcaty, n.desired)
      else
         sframe$mdm <- n.desired * sframe$mdcaty /
                       sum(sframe$len * sframe$mdcaty)

# Select the sample

      st_agr(sframe) <- "constant"
      stmp <- grtslin(sframe, sum(n.desired), SiteBegin, shift.grid, startlev,
         maxlev)

# Add the stratum variable

      stmp$stratum <- as.factor(rep(s,nrow(stmp)))

# Add panel and oversample structure

      stmp$panel <- rep("OverSamp",nrow(stmp))
      np <- c(0,cumsum(design[[s]]$panel))
      for(i in 1:length(design[[s]]$panel))
         stmp$panel[(np[i]+1):np[i+1]] <- names(design[[s]]$panel[i])

# If an oversample is present, then adjust the weights

      if(design[[s]]$over > 0) {
         if(design[[s]]$seltype != "Unequal") {
            stmp$wgt <- n.desired * stmp$wgt / samplesize
         } else {
            nc <- design[[s]]$caty.n
            for(i in names(n.desired)) {
               stmp$wgt[stmp$mdcaty == i] <- n.desired[i] *
                  stmp$wgt[stmp$mdcaty == i] / nc[i]
            }
         }
      }

# Add stratum sample to the output data frame

      if(first) {
         sites <- stmp
         levels(sites$stratum) <- strata.names
         first <- FALSE
      } else {
         sites <- rbind(sites, stmp)
      }
      SiteBegin <- SiteBegin + nrow(stmp)

# End the loop for strata

   }

# End the section for a linear network

} else if(type.frame == "area") {

# Begin the section for a polygonal area

   first <- TRUE
   SiteBegin <- SiteBegin
   sf.object$area_mdm <- as.numeric(st_area(sf.object))

# Begin the loop for strata

   for(s in strata.names) {

      cat(paste("\nStratum:", s, "\n"))

# Create the sample frame

      temp <- sf.object[, stratum, drop = TRUE] == s
      if(sum(temp) == 0) {
         warning(paste("\nThe stratum column in the attributes data frame contains no values that match \nthe stratum named \"", s, "\" in the design list.\n", sep=""))
         next
      }

      sframe <- subset(sf.object, temp)
      if(design[[s]]$seltype == "Equal") {
         sframe$mdcaty <- "Equal"
      } else if(design[[s]]$seltype == "Unequal") {
         sframe$mdcaty <- factor(sframe[, mdcaty, drop = TRUE])
      } else if(design[[s]]$seltype == "Continuous") {
         sframe$mdcaty <- sframe[, mdcaty, drop = TRUE]
      } else {
         stop(paste("\nThe value provided for the type of random selection, \"", design[[s]]$seltype, "\", \nfor stratum \"", s, "\" is not valid.", sep=""))
      }

# If seltype is not "Equal", ensure that mdcaty contains valid values

      if(design[[s]]$seltype == "Unequal") {
         if(any(is.na(sframe$mdcaty)))
            stop(paste("\nMissing values were detected among the unequal probability category values for \nstratum \"", s, "\".", sep=""))
      } else if(design[[s]]$seltype == "Continuous") {
         if(any(is.na(sframe$mdcaty)))
            stop(paste("\nMissing values were detected among the unequal probability category values for \nstratum \"", s, "\".", sep=""))
         if(!is.numeric(sframe$mdcaty))
            stop(paste("\nThe type of random selection for stratum \"", s, "\" is \"Continuous\", \nbut the unequal probability category values are not numeric.", sep=""))
         if(any(sframe$mdcaty < 0))
            stop(paste("\nNonpositive values were detected among the unequal probability category values \nfor stratum \"", s, "\".", sep=""))
      }

# If seltype is "Unequal", ensure that caty.n is provided and that the names
# in caty.n and the levels of mdcaty are equivalent

      if(design[[s]]$seltype == "Unequal") {
         if(is.null(design[[s]]$caty.n))
            stop(paste("The type of random selection was set to \"Unequal\", but caty.n was not \nprovided for stratum \"", s, "\".", sep=""))
         temp <- match(names(design[[s]]$caty.n),
            levels(as.factor(sframe$mdcaty)), nomatch=0)
         if(any(temp == 0)) {
            temp.str <- vecprint(names(design[[s]]$caty.n)[temp == 0])
            stop(paste("\nThe following names in caty.n for stratum \"", s, "\" do not occur \namong the levels of the mdcaty variable in att.frame:\n", temp.str, sep=""))
         }
      }

# Ensure that panel and caty.n contain valid values

      if(!is.numeric(design[[s]]$panel))
         stop(paste(" The design list must contain numeric values in the panel argument for \nstratum \"", s, "\".\n", sep=""))
      design[[s]]$panel <- round(design[[s]]$panel)
      design[[s]]$panel <- design[[s]]$panel[design[[s]]$panel > 0]
      if(length(design[[s]]$panel) == 0)
         stop(paste(" The design list does not not contain any valid values of the panel \nargument for stratum \"", s, "\".\n", sep=""))

      if(design[[s]]$seltype == "Unequal") {
         if(!is.numeric(design[[s]]$caty.n))
            stop(paste(" The design list must contain numeric values in the caty.n argument for \nstratum \"", s, "\".\n", sep=""))
         design[[s]]$caty.n <- round(design[[s]]$caty.n)
         design[[s]]$caty.n <- design[[s]]$caty.n[design[[s]]$caty.n > 0]
         if(length(design[[s]]$caty.n) == 0)
            stop(paste(" The design list does not not contain any valid values of the caty.n \nargument for stratum \"", s, "\".\n", sep=""))
      }

# As necessary, remove rows from sframe that have values of mdcaty which are not
# included among the names in caty.n

      if(design[[s]]$seltype == "Unequal") {
         temp <- sframe$mdcaty %in% names(design[[s]]$caty.n)
         if(any(!temp)) {
            sframe <- sframe[temp,]
         }
      }

# Determine overall sample size for the stratum

      if(is.null(design[[s]]$over))
         design[[s]]$over <- 0
      if(design[[s]]$seltype != "Unequal") {
         samplesize <- sum(design[[s]]$panel)
         n.desired <- sum(samplesize, design[[s]]$over)
      } else {
         if(sum(design[[s]]$panel) != sum(design[[s]]$caty.n))
            stop("\nThe sum of panel sample sizes does not equal sum of caty.n sample sizes")
         samplesize <- sum(design[[s]]$caty.n)
         if(design[[s]]$over == 0) {
            n.desired <- design[[s]]$caty.n
         } else {
            over.n <- design[[s]]$over * design[[s]]$caty.n /
               sum(design[[s]]$caty.n)
            if(any(over.n != floor(over.n)))
               warning(paste("\nOversample size is not proportional to category sample sizes for stratum\n\"", s, "\".\n", sep=""))
            n.desired <- design[[s]]$caty.n + ceiling(over.n)
         }
      }

# Calculate mdm - inclusion probabilities

      if(design[[s]]$seltype == "Equal")
         sframe$mdm <- mdmarea(sframe$area_mdm, sframe$mdcaty, c(Equal=n.desired))
      else if(design[[s]]$seltype == "Unequal")
         sframe$mdm <- mdmarea(sframe$area, sframe$mdcaty, n.desired)
      else
         sframe$mdm <- n.desired * sframe$mdcaty /
                       sum(sframe$area * sframe$mdcaty)

# Select the sample

      st_agr(sframe) <- "constant"
      stmp <- grtsarea(sframe, sum(n.desired), SiteBegin, shift.grid,
         startlev, maxlev, maxtry)

# Determine whether the realized sample size is less than the desired size

      if(nrow(stmp) < sum(n.desired))
         warning(paste("\nThe size of the selected sample was less than the desired size for stratum \n\"", s, "\".\n", sep=""))

# Add the stratum variable

      stmp$stratum <- as.factor(rep(s, nrow(stmp)))

# Add panel and oversample structure

      stmp$panel <- as.character(rep("OverSamp",nrow(stmp)))
      n.panel <- length(design[[s]]$panel)
      if(nrow(stmp) < samplesize) {
         n.short <- samplesize - nrow(stmp)
         n.temp <- n.short / n.panel
         if(n.temp != floor(n.temp)) {
            n.temp <- c(ceiling(n.temp), rep(floor(n.temp), n.panel-1))
            i <- 1
            while(sum(n.temp) != n.short) {
               i <- i+1
               ntemp[i] <- n.temp[i] + 1
            }
         }
         np <- c(0, cumsum(design[[s]]$panel - n.temp))
      } else {
         np <- c(0, cumsum(design[[s]]$panel))
      }
      for(i in 1:n.panel)
         stmp$panel[(np[i]+1):np[i+1]] <- names(design[[s]]$panel[i])

# If an oversample is present or the realized sample size is less than the
# desired size, then adjust the weights

      if(design[[s]]$over > 0 || nrow(stmp) < samplesize) {
         if(design[[s]]$seltype != "Unequal") {
            if(nrow(stmp) < samplesize) {
               stmp$wgt <- n.desired * stmp$wgt / nrow(stmp)
            } else {
               stmp$wgt <- n.desired * stmp$wgt / samplesize
            }
         } else {
            if(nrow(stmp) < samplesize) {
               n.caty <- length(design[[s]]$caty.n)
               n.temp <- n.short / n.caty
               nc <- design[[s]]$caty.n - n.temp
            } else {
               nc <- design[[s]]$caty.n
            }
            for(i in names(n.desired)) {
               stmp$wgt[stmp$mdcaty == i] <- n.desired[i] *
                  stmp$wgt[stmp$mdcaty == i] / nc[i]
            }
         }
      }

# Add stratum sample to the output data frame

      if(first) {
         sites <- stmp
         levels(sites$stratum) <- strata.names
         first <- FALSE
      } else {
         sites <- rbind(sites, stmp)
      }
      SiteBegin <- SiteBegin + nrow(stmp)

# End the loop for strata

   }

# End the section for a polygonal area

}

# As necessary, terminate parallel processing

if(type.frame != "finite") {
   stopCluster(cl)
}

# Add DesignID name to the numeric siteID value to create a new siteID

sites$siteID <- as.character(gsub(" ","0", paste(DesignID,"-",
   format(sites$siteID), sep="")))

# Add Evaluation Status and Evaluation Reason variables to the output data frame

sites$EvalStatus <- rep("NotEval", nrow(sites))
sites$EvalReason <- rep(" ", nrow(sites))

# Add attributes from sf.object that are not included in sites

tm <- match(sites$id, sf.object$id)
geom_name <- attr(sf.object, "sf_column")
if(design[[s]]$seltype == "Equal") {
   td <- match(c(id, stratum, "length_mdm", "area_mdm", geom_name),
      names(sf.object), nomatch=0)
} else {
   td <- match(c(id, stratum, mdcaty, "length_mdm", "area_mdm", geom_name),
      names(sf.object), nomatch=0)
}
temp <- names(sf.object)[-td]
if(length(temp) > 0) {
   for(i in temp) {
      sites[, i] <- sf.object[tm, i, drop = TRUE]
   }
}

# Remove the id attribute from sites

temp <- names(sites)
temp <- temp[!(temp %in% c("id", geom_name))]
sites <- subset(sites, select=temp)

# Add row names to sites

n <- nrow(sites)
IDs <- as.character(1:n)
row.names(sites) <- IDs

# Assign attributes to sites

ifelse(is.null(startlev),
   attr(sites, "startlev") <- "Not specified",
   attr(sites, "startlev") <- startlev)
ifelse(is.null(maxlev),
   attr(sites, "maxlev") <- "Not specified",
   attr(sites, "maxlev") <- maxlev)
attr(sites, "endlev") <- attributes(stmp)$nlev
attr(sites, "shift.grid") <- shift.grid
attr(sites, "do.sample") <- do.sample

# If requested, create a shapefile containing the sample information

if(shapefile == TRUE) {
   nc <- nchar(out.shape)
   if(substr(out.shape, nc-3, nc) != ".shp") {
      if(substr(out.shape, nc-3, nc-3) == ".") {
         out.shape <- paste(substr(out.shape, 1, nc-4), ".shp", sep="")
      } else {
         out.shape <- paste(out.shape, ".shp", sep="")
      }
   }
   if(out.shape %in% list.files()) {
      warning(paste("\nThe output shapefile named \"", out.shape, "\" already exists and was \noverwritten.\n", sep=""))
      st_write(sites, out.shape, quiet = TRUE, delete_dsn = TRUE)
   } else {
      st_write(sites, out.shape, quiet = TRUE)
   }
}

# Create an object of class SpatialDesign

SpointsMat <- st_coordinates(sites)
rownames(SpointsMat) <- IDs
sp_obj <- SpatialPointsDataFrame(SpatialPoints(SpointsMat),
   data = sites[, 1:(ncol(sites)-1), drop = TRUE])
rslt <- SpatialDesign(design = design, sp_obj = sp_obj)

# Return the SpatialDesign object

invisible(rslt)
}
