################################################################################
# Function: change.analysis
# Programmer: Tom Kincaid
# Date: January 27, 2012
# Last Revised: January 11, 2018
#
#' Estimation of Change between Two Time Periods in a Probability Survey
#'
#' This function organizes input and output for estimation of change between two
#' probability surveys.
#'
#' @param sites Data frame consisting of three variables: the first variable
#'   is site IDs, and the other variables are logical vectors indicating which
#'   sites to use in the analysis.  The first logical vector indicates the
#'   complete set of sites for the first survey.  The second logical vector
#'   indicates the complete set of sites for the second survey.
#'
#' @param repeats Data frame that identifies site IDs for repeat visit sites
#'   from the two surveys.   The first variable is site IDs for survey one. The
#'   second variable is site IDs for survey two.  For each row of the data
#'   frame, the two site IDs must correspond to the same site. This argument
#'   should equal NULL when repeat visit sites are not present.  The default is
#'   NULL.
#'
#' @param subpop Data frame describing sets of populations and subpopulations
#'   for which estimates will be calculated.  The first variable is site IDs.
#'   Each subsequent variable identifies a Type of population, where the
#'   variable name is used to identify Type.  A Type variable identifies each
#'   site with one of the subpopulations of that Type.  The default is NULL.
#'
#' @param design Data frame consisting of design variables.  If spsurvey.obj
#'   is not provided, then this argument is required.  The default is NULL.
#'   Variables should be named as follows:
#'     \describe{
#'       \item{siteID}{Vector of site IDs}
#'       \item{wgt}{Vector of weights, which are either the weights for a
#'         single-stage sample or the stage two weights for a two-stage sample}
#'       \item{xcoord}{Vector of x-coordinates for location, which are either
#'         the x-coordinates for a single-stage sample or the stage two
#'         x-coordinates for a two-stage sample}
#'       \item{ycoord}{Vector of y-coordinates for location, which are either
#'         the y-coordinates for a single-stage sample or the stage two
#'         y-coordinates for a two-stage sample}
#'       \item{stratum}{Vector of the stratum codes for each site}
#'       \item{cluster}{Vector of the stage one sampling unit (primary sampling
#'         unit or cluster) codes for each site}
#'       \item{wgt1}{Vector of stage one weights in a two-stage design}
#'       \item{xcoord1}{Vector of the stage one x-coordinates for location in a
#'         two-stage design}
#'       \item{ycoord1}{Vector of the stage one y-coordinates for location in a
#'         two-stage design}
#'       \item{support}{Vector of support values - for a finite resource, the
#'         value one (1) for a for site.  For an extensive resource, the measure
#'         of the sampling unit associated with a site.  Required for
#'         calculation of finite and continuous population 
#'         correction factors.}
#'       \item{swgt}{Vector of size-weights, which is the stage two size-weight
#'         for a two-stage design.}
#'       \item{swgt1}{Vector of stage one size-weights for a two-stage design.}
#'     } 
#'
#' @param data.cat Data frame of categorical response variables.  The first
#'   variable is site IDs.  Subsequent variables are response variables. Missing
#'   data (NA) is allowed.  The default is NULL.
#'
#' @param data.cont Data frame of continuous response variables.  The first
#'   variable is site IDs.  Subsequent variables are response variables. Missing
#'   data (NA) is allowed.  The default is NULL.
#'
#' @param revisitwgt Logical value that indicates whether each repeat visit
#'   site has the same survey design weight in the two surveys, where TRUE = the
#'   weight for each repeat visit site is the same and FALSE = the weight for
#'   each repeat visit site is not the same.  When this argument is FALSE, all
#'   of the repeat visit sites are assigned equal weights when calculating the
#'   covariance component of the change estimate standard error.  The default is
#'   FALSE.
#'
#' @param test Character string or character vector providing the location
#'   measure(s) to use for change estimation for continuous variables.  The
#'   choices are "mean", "median", or c("mean", "median").  The default is
#'   "mean".
#'
#' @param popsize_1 Known size of the resource for survey one, which is used
#'   to perform ratio adjustment to estimators expressed using measurement units
#'   for the resource and to calculate strata proportions for calculating
#'   estimates for a stratified sample.  For a finite resource, this argument is
#'   either the total number of sampling units or the known sum of size-
#'   weights.  For an extensive resource, this argument is the measure of the
#'   resource, i.e., either known total length for a linear resource or known
#'   total area for an areal resource.  The argument must be in the form of a
#'   list containing an element for each population Type in the subpop data
#'   frame, where NULL is a valid choice for a population Type.  The list must
#'   be named using the column names for the population Types in subpop. If a
#'   population Type doesn't contain subpopulations, then each element of the
#'   list is either a single value for an unstratified sample or a vector
#'   containing a value for each stratum for a stratified sample, where elements
#'   of the vector are named using the stratum codes.  If a population Type
#'   contains subpopulations, then each element of the list is a list containing
#'   an element for each subpopulation, where the list is named using the
#'   subpopulation names.  The element for each subpopulation will be either a
#'   single value for an unstratified sample or a named vector of values for a
#'   stratified sample.  The default is NULL.\cr\cr
#'     Example popsize for a stratified sample:\cr
#'       popsize = list("Pop 1"=c("Stratum 1"=750,\cr
#'                                "Stratum 2"=500,\cr
#'                                "Stratum 3"=250),\cr
#'                      "Pop 2"=list("SubPop 1"=c("Stratum 1"=350,\cr
#'                                                "Stratum 2"=250,\cr
#'                                                "Stratum 3"=150),\cr
#'                                   "SubPop 2"=c("Stratum 1"=250,\cr
#'                                                "Stratum 2"=150,\cr
#'                                                "Stratum 3"=100),\cr
#'                                   "SubPop 3"=c("Stratum 1"=150,\cr
#'                                                "Stratum 2"=150,\cr
#'                                                "Stratum 3"=75)),\cr
#'                      "Pop 3"=NULL)\cr\cr
#'     Example popsize for an unstratified sample:\cr
#'       popsize = list("Pop 1"=1500,\cr
#'                      "Pop 2"=list("SubPop 1"=750,\cr
#'                                   "SubPop 2"=500,\cr
#'                                   "SubPop 3"=375),\cr
#'                      "Pop 3"=NULL)\cr
#'
#' @param popsize_2 Known size of the resource for survey two.  The default is
#'   NULL.
#'
#' @param popcorrect_1 Logical value that indicates whether finite or
#'   continuous population correction factors should be employed during variance
#'   estimation for survey one, where TRUE = use the correction factor and FALSE
#'   = do not use the correction factor.  The default is FALSE.  To employ the
#'   correction factor for a single-stage sample, values must be supplied for
#'   argument pcfsize_1 and for the support variable of the design argument.  To
#'   employ the correction factor for a two-stage sample, values must be
#'   supplied for arguments N.cluster_1 and  stage1size_1 and for the support
#'   variable of the design argument.
#'
#' @param popcorrect_2 Logical value that indicates whether finite or
#'   continuous population correction factors should be employed during variance
#'   estimation for survey two, where TRUE = use the correction factor and FALSE
#'   = do not use the correction factor.  The default is FALSE.  To employ the
#'   correction factor for a single-stage sample, values must be supplied for
#'   argument pcfsize_2 and for the support variable of the design argument.  To
#'   employ the correction factor for a two-stage sample, values must be
#'   supplied for arguments N.cluster_2 and  stage1size_2 and for the support
#'   variable of the design argument.
#'
#' @param pcfsize_1 Size of the resource for survey one, which is required for
#'   calculation of finite and continuous population correction factors for a
#'   single-stage sample.  For a stratified sample this argument must be a
#'   vector containing a value for each stratum and must have the names
#'   attribute set to identify the stratum codes.  The default is NULL.
#'
#' @param pcfsize_2 Size of the resource for survey two.  The default is NULL.
#'
#' @param N.cluster_1 Number of stage one sampling units in the resource
#'   for survey one, which is required for calculation of finite and continuous
#'   population correction factors for a two-stage sample.  For a stratified
#'   sample this argument must be a vector containing a value for each stratum
#'   and must have the names attribute set to identify the stratum codes.  The
#'   default is NULL.
#'
#' @param N.cluster_2 Number of stage one sampling units in the resource
#'   for survey two.  The default is NULL.
#'
#' @param stage1size_1 Size of the stage one sampling units of a two-stage
#'   sample for survey one, which is required for calculation of finite and
#'   continuous population correction factors for a two-stage sample and must
#'   have the names attribute set to identify the stage one sampling unit codes.
#'   For a stratified sample, the names attribute must be set to identify both
#'   stratum codes and stage one sampling unit codes using a convention where
#'   the two codes are separated by the & symbol, e.g., "Stratum 1&Cluster 1".
#'   The default is NULL.
#'
#' @param stage1size_2 Size of the stage one sampling units of a two-stage
#'   sample for survey two.  The default is NULL.
#'
#' @param sizeweight_1 Logical value that indicates whether size-weights should
#'   be used in the analysis of survey one, where TRUE = use the size-weights
#'   and FALSE = do not use the size-weights.  The default is FALSE.
#'
#' @param sizeweight_2 Logical value that indicates whether size-weights
#'   should be used in the analysis of survey two.  The default is FALSE.
#'
#' @param vartype_1 The choice of variance estimator for survey one, where
#'   "Local" = local mean estimator and "SRS" = SRS estimator.  The default is
#'   "Local".
#'
#' @param vartype_2 The choice of variance estimator for survey two.  The
#'   default is "Local".
#'
#' @param conf Numeric value for the confidence level.  The default is 95.
#'
#' @return List of change estimates composed of three items: (1) catsum
#'   contains change estimates for categorical variables, (2) contsum_mean
#'   contains estimates for continuous variables using the mean, and (3)
#'   contsum_median contains estimates for continuous variables using the
#'   median.  The items in the list will contain NULL for estimates that were
#'   not calculated.  Each data frame includes estimates for all combinations of
#'   population Types, subpopulations within Types, response variables, and
#'   categories within each response variable (for categorical variables and
#'   continuous variables using the median).  Change estimates are provided plus
#'   standard error estimates and confidence interval estimates.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{dframe.check}}}{check site IDs, the sites data frame,
#'       the subpop data frame, and the data.cat data frame to assure valid
#'       contents and, as necessary, create the sites data frame and the subpop
#'       data frame}
#'     \item{\code{\link{vecprint}}}{takes an input vector and outputs a
#'       character string with line breaks inserted}
#'     \item{\code{\link{uniqueID}}}{creates unique site IDs by appending a
#'       unique number to each occurrence of a site ID}
#'     \item{\code{\link{input.check}}}{check input values for errors,
#'       consistency, and compatibility with analytical functions}
#'     \item{\code{\link{change.est}}}{estimate change between two surveys}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @keywords survey
#'
#' @examples
#' # Categorical variable example for three resource classes
#' mysiteID <- paste("Site", 1:200, sep="")
#' mysites <- data.frame(
#'   siteID=mysiteID,
#'   Survey1=rep(c(TRUE, FALSE), c(100,100)),
#'   Survey2=rep(c(FALSE, TRUE), c(100,100)))
#' myrepeats <- data.frame(
#'   siteID_1=paste("Site", 1:40, sep=""),
#'   siteID_2=paste("Site", 101:140, sep=""))
#' mysubpop <- data.frame(
#'   siteID=mysiteID,
#'   All_Sites=rep("All Sites", 200),
#'   Region=rep(c("North","South"), 100))
#' mydesign <- data.frame(
#'   siteID=mysiteID,
#'   wgt=runif(200, 10, 100),
#'   xcoord=runif(200),
#'   ycoord=runif(200),
#'   stratum=rep(rep(c("Stratum1", "Stratum2"), c(2,2)), 50))
#' mydata.cat <- data.frame(
#'   siteID=mysiteID,
#'   Resource_Class=sample(c("Good","Fair","Poor"), 200, replace=TRUE))
#' change.analysis(sites=mysites, repeats=myrepeats, subpop=mysubpop,
#'   design=mydesign, data.cat=mydata.cat, data.cont=NULL)
#'
#' @export
################################################################################

change.analysis <- function(sites, repeats = NULL, subpop = NULL, design,
   data.cat = NULL, data.cont = NULL, revisitwgt = FALSE, test = "mean",
   popsize_1 = NULL, popsize_2 = NULL, popcorrect_1 = FALSE,
   popcorrect_2 = FALSE, pcfsize_1 = NULL, pcfsize_2 = NULL, N.cluster_1 = NULL,
   N.cluster_2 = NULL, stage1size_1 = NULL, stage1size_2 = NULL,
   sizeweight_1 = FALSE, sizeweight_2 = FALSE, vartype_1 = "Local",
   vartype_2 = "Local", conf = 95) {

# Create a data frame for warning messages

   warn.ind <- FALSE
   warn.df <- NULL
   fname <- "change.analysis"

# Check that the required data frames have been provided

   if(is.null(sites))
      stop("\nThe sites data frame must be provided.")
   if(is.null(design))
      stop("\nThe design data frame must be provided.")
   if(!is.data.frame(design))
      stop("\nThe design argument must be a data frame.")
   if(is.null(data.cat) & is.null(data.cont))
      stop("\nAt least one of the data.cat or data.cont data frames must be provided.")

# Check the sites data frame for missing values in the logical vectors

   temp <- apply(sites[,2:3], 1, function(x) any(is.na(x)))
   if(any(temp)) {
      temp.str <- vecprint(seq(nrow(sites))[temp])
      stop(paste("\nThe following rows in the sites data frame contain missing logical variable \nvalues:\n", temp.str, sep=""))
   }

# Check the repeats data frame for missing values

   if(!is.null(repeats)) {
      temp <- any(is.na(repeats[,1]))
      if(any(temp)) {
         temp.str <- vecprint(seq(nrow(sites))[temp])
         stop(paste("\nThe following rows in the repeats data frame contain missing for site IDs \nin survey one:\n", temp.str, sep=""))
      }
      temp <- any(is.na(repeats[,2]))
      if(any(temp)) {
         temp.str <- vecprint(seq(nrow(sites))[temp])
         stop(paste("\nThe following rows in the repeats data frame contain missing for site IDs \nin survey two:\n", temp.str, sep=""))
      }
   }

# Assign some required values from the subpop data frame

   ntypes <- ncol(subpop)
   typenames <- names(subpop)

# Check the design data frame for required names

   design.names <- names(design)
   temp <- match(design.names, c("siteID", "wgt", "xcoord", "ycoord", "stratum",
      "cluster", "wgt1", "xcoord1", "ycoord1", "support", "swgt", "swgt1"),
      nomatch=0)
   if(any(temp == 0)) {
      temp.str <- vecprint(design.names[temp == 0])
      stop(paste("\nThe following names used in the design data frame do not match the required names:\n", temp.str))
   }

# Create data frames for the two surveys

   sites_1 <- sites[sites[,2],c(1,2)]
   sites_2 <- sites[sites[,3],c(1,3)]
   subpop_1 <- subpop[sites[,2],]
   subpop_2 <- subpop[sites[,3],]
   design_1 <- design[sites[,2],]
   design_2 <- design[sites[,3],]
   data.cat_1 <- data.cat[sites[,2],]
   data.cat_2 <- data.cat[sites[,3],]
   data.cont_1 <- data.cont[sites[,2],]
   data.cont_2 <- data.cont[sites[,3],]

# Assign the repeat visit sites logical vectors

   if(is.null(repeats)) {
      repeat_1 <- logical(nrow(sites_1))
      repeat_2 <- logical(nrow(sites_2))
   } else {
      temp <- match(sites_1[,1], repeats[,1], nomatch=0)
      repeat_1 <- temp > 0
      temp <- match(repeats[,1], sites_1[,1], nomatch=0)
      if(any(temp == 0)) {
         temp.str <- vecprint(repeats[,1][temp == 0])
          stop(paste("\nThe following site IDs for survey one in the repeats data frame do not have \nmatching site IDs for survey one in the sites data frame:\n", temp.str, sep=""))
      }
      sites_1[repeat_1,] <- sites_1[temp,]

      temp <- match(sites_2[,1], repeats[,2], nomatch=0)
      repeat_2 <- temp > 0
      temp <- match(repeats[,2], sites_2[,1], nomatch=0)
      if(any(temp == 0)) {
         temp.str <- vecprint(repeats[,1][temp == 0])
         stop(paste("\nThe following site IDs for survey two in the repeats data frame do not have \nmatching site IDs for survey two in the sites data frame:\n", temp.str, sep=""))
      }
      sites_2[repeat_2,] <- sites_2[temp,]
   }

# Check the repeat visit logical variables for size
   n1 <- sum(repeat_1)
   n2 <- sum(repeat_2)
   if(n1 != n2)
      stop(paste("\nThe number of repeat visit sites for survey one, ", n1, ", does not equal the number \nof repeat visit sites for survey two, ", n2, ".", sep=""))

#
# Check arguments for survey one
#

# Check the sites, design, subpop, data.cat, and data.cont data frames to assure
# valid contents

   temp <- dframe.check(sites_1, design_1, subpop_1, data.cat_1, data.cont_1,
      NULL, design.names)
   sites_1 <- temp$sites
   design_1 <- temp$design
   subpop_1 <- temp$subpop
   data.cat_1 <- temp$data.cat
   data.cont_1 <- temp$data.cont

# Assign variables from the design data frame

   siteID <- design_1$siteID
   wgt <- design_1$wgt
   xcoord <- design_1$xcoord
   ycoord <- design_1$ycoord
   stratum <- design_1$stratum
   cluster <- design_1$cluster
   wgt1 <- design_1$wgt1
   xcoord1 <- design_1$xcoord1
   ycoord1 <- design_1$ycoord1
   support <- design_1$support
   swgt <- design_1$swgt
   swgt1 <- design_1$swgt1

# Check site IDs for repeat values and, as necessary, create unique site IDs and
# output a warning message

   temp <- sapply(split(siteID, siteID), length)
   if(any(temp > 1)) {
      warn.ind <- TRUE
      temp.str <- vecprint(names(temp)[temp > 1])
      warn <- paste("The following site ID values occur more than once among the values that were \ninput to the function:\n", temp.str, sep="")
      act <- "Unique site ID values were created.\n"
      warn.df <- rbind(warn.df, data.frame(func=I(fname), subpoptype=NA,
         subpop=NA, indicator=NA, stratum=NA, warning=I(warn), action=I(act)))
      siteID <- uniqueID(siteID)
      sites_1[,1] <- siteID
      subpop_1[,1] <- siteID
      data.cat_1[,1] <- siteID
      data.cont_1[,1] <- siteID
   }

# Assign a logical value to the indicator variables for a stratified sample

   stratum.ind_1 <- length(unique(stratum)) > 1

# If the sample is stratified, convert stratum to a factor, determine stratum 
# levels, and calculate number of strata

   if(stratum.ind_1) {
      stratum <- factor(stratum)
      stratum.levels <- levels(stratum)
      nstrata <- length(stratum.levels)
   } else {
      stratum.levels <- NULL
      nstrata <- NULL
   }

# Assign a logical value to the indicator variable for a two stage sample

   cluster.ind_1 <- length(unique(cluster)) > 1

# If the sample has two stages, convert cluster to a factor, determine cluster 
# levels, and calculate number of clusters

   if(cluster.ind_1) {
      if(stratum.ind_1) {
         cluster.in <- cluster
         cluster <- tapply(cluster, stratum, factor)
         cluster.levels <- sapply(cluster, levels, simplify=FALSE)
         ncluster <- sapply(cluster.levels, length)
      } else {
         cluster <- factor(cluster)
         cluster.levels <- levels(cluster)
         ncluster <- length(cluster.levels)
      }
   }

# Ensure that popsize is a list

   if(!is.null(popsize_1) && !is.list(popsize_1))
      stop("\nThe popsize argument must be a list")

# If the population correction factor is to be used, ensure that support values
# are provided

   if(popcorrect_1 && is.null(support))
      stop("\nThe logical value that indicates whether finite or continuous population \ncorrection factors should be employed during variance estimation was set to \nTRUE, but support values were not provided in the design data frame.")

# Assign the value of popcorrect to the indicator variable for use of the
# population correction factor

   pcfactor.ind_1 <- popcorrect_1

# If the sample uses size-weights, ensure that size weights are provided

   if(sizeweight_1) {
      if(is.null(swgt))
         stop("\nThe logical value that indicates whether size-weights should be employed in the analysis was set to \nTRUE, but size-weights were not provided in the design data frame.")
      if(cluster.ind_1 && is.null(swgt1))
         stop("\nThe sample has two stages and the logical value that indicates whether size- \nweights should be employed in the analysis was set to TRUE, but stage one \nsize-weights were not provided in the design data frame.")
   }

# Assign the value of sizeweight to the indicator variable for use of size
# weights

   swgt.ind_1 <- sizeweight_1

# Determine the number of response values

   nresp <- nrow(design_1)

# Check for compatibility of input values

   temp <- input.check(nresp, wgt, NULL, NULL, xcoord, ycoord, stratum.ind_1,
      stratum, stratum.levels, nstrata, cluster.ind_1, cluster, cluster.levels,
      ncluster, wgt1, xcoord1, ycoord1, popsize_1, pcfactor.ind_1, pcfsize_1,
      N.cluster_1, stage1size_1, support, swgt.ind_1, swgt, swgt1, vartype_1,
      conf, subpop=subpop)
   popsize_1 <- temp$popsize
   pcfsize_1 <- temp$pcfsize
   N.cluster_1 <- temp$N.cluster
   stage1size_1 <- temp$stage1size

# If the sample was stratified and had two stages, then reset cluster to its 
# input value

   if(stratum.ind_1 && cluster.ind_1)
      cluster <- cluster.in

# If the sample has two stages, determine whether there are a sufficient number
# of sites in each stage one sampling unit to allow variance calculation

   if(cluster.ind_1) {
      for(itype in 2:ntypes) {
         temp <- apply(table(cluster, subpop_1[,itype]) == 1, 2, sum)
         ind <- tapply(cluster, subpop_1[,itype], function(x) length(unique(x)))
         if(any(temp == ind)) {
            temp.str <- vecprint(names(temp)[temp == ind])
            warn.df <<- warn.df
            stop(paste("\nA variance estimate cannot be calculated since all of the stage one sampling \nunits contain a single stage two sampling unit for the following \nsubpopulation(s) of population ", typenames[itype], ":\n", temp.str, "\nEnter the following command to view the warning messages that were generated: \nwarnprnt() \n", sep=""))
         }
         if(any(temp > 0)) {
            temp.str <- vecprint(names(temp)[temp > 0])
            warn <- paste("Since they include one or more stage one sampling units with a single site, \nthe mean of the variance estimates for stage one sampling units with two or \nmore sites will be used as the variance estimate for stage one sampling units \nwith one site for the following subpopulation(s) of population\n", typenames[itype], ":\n", temp.str, sep="")
            act <- "The mean of the variance estimates will be used.\n"
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=NA, subpop=NA, indicator=NA, stratum=NA,
               warning=I(warn), action=I(act)))
         }
      }
   }

# As necessary, assign missing values to the design variables

   if(is.null(xcoord))
      xcoord <- rep(NA, nresp)
   if(is.null(ycoord))
      ycoord <- rep(NA, nresp)
   if(is.null(stratum))
      stratum <- rep(NA, nresp)
   if(is.null(cluster))
      cluster <- rep(NA, nresp)
   if(is.null(wgt1))
      wgt1 <- rep(NA, nresp)
   if(is.null(xcoord1))
      xcoord1 <- rep(NA, nresp)
   if(is.null(ycoord1))
      ycoord1 <- rep(NA, nresp)
   if(is.null(support))
      support <- rep(NA, nresp)
   if(is.null(swgt))
      swgt <- rep(NA, nresp)
   if(is.null(swgt1))
      swgt1 <- rep(NA, nresp)

# Recreate the design data frame

   design_1 <- data.frame(siteID=siteID, wgt=wgt, xcoord=xcoord,
      ycoord=ycoord, stratum=stratum, cluster=cluster, wgt1=wgt1,
      xcoord1=xcoord1, ycoord1=ycoord1, support=support, swgt=swgt,
      swgt1=swgt1)

#
# Check arguments for survey two
#

# Check the sites, design, subpop, data.cat, and data.cont data frames to assure
# valid contents

   temp <- dframe.check(sites_2, design_2, subpop_2, data.cat_2, data.cont_2,
      NULL, design.names)
   sites_2 <- temp$sites
   design_2 <- temp$design
   subpop_2 <- temp$subpop
   data.cat_2 <- temp$data.cat
   data.cont_2 <- temp$data.cont

# Assign variables from the design data frame

   siteID <- design_2$siteID
   wgt <- design_2$wgt
   xcoord <- design_2$xcoord
   ycoord <- design_2$ycoord
   stratum <- design_2$stratum
   cluster <- design_2$cluster
   wgt1 <- design_2$wgt1
   xcoord1 <- design_2$xcoord1
   ycoord1 <- design_2$ycoord1
   support <- design_2$support
   swgt <- design_2$swgt
   swgt1 <- design_2$swgt1

# Check site IDs for repeat values and, as necessary, create unique site IDs and
# output a warning message

   temp <- sapply(split(siteID, siteID), length)
   if(any(temp > 1)) {
      warn.ind <- TRUE
      temp.str <- vecprint(names(temp)[temp > 1])
      warn <- paste("The following site ID values occur more than once among the values that were \ninput to the function:\n", temp.str, sep="")
      act <- "Unique site ID values were created.\n"
      warn.df <- rbind(warn.df, data.frame(func=I(fname), subpoptype=NA,
         subpop=NA, indicator=NA, stratum=NA, warning=I(warn), action=I(act)))
      siteID <- uniqueID(siteID)
      sites_2[,1] <- siteID
      subpop_2[,1] <- siteID
      data.cat_2[,1] <- siteID
      data.cont_2[,1] <- siteID
   }

# Assign a logical value to the indicator variables for a stratified sample

   stratum.ind_2 <- length(unique(stratum)) > 1

# If the sample is stratified, convert stratum to a factor, determine stratum 
# levels, and calculate number of strata

   if(stratum.ind_2) {
      stratum <- factor(stratum)
      stratum.levels <- levels(stratum)
      nstrata <- length(stratum.levels)
   } else {
      stratum.levels <- NULL
      nstrata <- NULL
   }

# Assign a logical value to the indicator variable for a two stage sample

   cluster.ind_2 <- length(unique(cluster)) > 1

# If the sample has two stages, convert cluster to a factor, determine cluster 
# levels, and calculate number of clusters

   if(cluster.ind_2) {
      if(stratum.ind_2) {
         cluster.in <- cluster
         cluster <- tapply(cluster, stratum, factor)
         cluster.levels <- sapply(cluster, levels, simplify=FALSE)
         ncluster <- sapply(cluster.levels, length)
      } else {
         cluster <- factor(cluster)
         cluster.levels <- levels(cluster)
         ncluster <- length(cluster.levels)
      }
   }

# Ensure that popsize is a list

   if(!is.null(popsize_2) && !is.list(popsize_2))
      stop("\nThe popsize argument must be a list")

# If the population correction factor is to be used, ensure that support values
# are provided

   if(popcorrect_2 && is.null(support))
      stop("\nThe logical value that indicates whether finite or continuous population \ncorrection factors should be employed during variance estimation was set to \nTRUE, but support values were not provided in the design data frame.")

# Assign the value of popcorrect to the indicator variable for use of the
# population correction factor

   pcfactor.ind_2 <- popcorrect_2

# If the sample uses size-weights, ensure that size weights are provided

   if(sizeweight_2) {
      if(is.null(swgt))
         stop("\nThe logical value that indicates whether size-weights should be employed in the analysis was set to \nTRUE, but size-weights were not provided in the design data frame.")
      if(cluster.ind_2 && is.null(swgt1))
         stop("\nThe sample has two stages and the logical value that indicates whether size- \nweights should be employed in the analysis was set to TRUE, but stage one \nsize-weights were not provided in the design data frame.")
   }

# Assign the value of sizeweight to the indicator variable for use of size
# weights

   swgt.ind_2 <- sizeweight_2

# Determine the number of response values

   nresp <- nrow(design_2)

# Check for compatibility of input values

   temp <- input.check(nresp, wgt, NULL, NULL, xcoord, ycoord, stratum.ind_2,
      stratum, stratum.levels, nstrata, cluster.ind_2, cluster, cluster.levels,
      ncluster, wgt1, xcoord1, ycoord1, popsize_2, pcfactor.ind_2, pcfsize_2,
      N.cluster_2, stage1size_2, support, swgt.ind_2, swgt, swgt1, vartype_2,
      conf, subpop=subpop)
   popsize_2 <- temp$popsize
   pcfsize_2 <- temp$pcfsize
   N.cluster_2 <- temp$N.cluster
   stage1size_2 <- temp$stage1size

# If the sample was stratified and had two stages, then reset cluster to its 
# input value

   if(stratum.ind_2 && cluster.ind_2)
      cluster <- cluster.in

# If the sample has two stages, determine whether there are a sufficient number
# of sites in each stage one sampling unit to allow variance calculation

   if(cluster.ind_2) {
      for(itype in 2:ntypes) {
         temp <- apply(table(cluster, subpop_2[,itype]) == 1, 2, sum)
         ind <- tapply(cluster, subpop_2[,itype], function(x) length(unique(x)))
         if(any(temp == ind)) {
            temp.str <- vecprint(names(temp)[temp == ind])
            warn.df <<- warn.df
            stop(paste("\nA variance estimate cannot be calculated since all of the stage one sampling \nunits contain a single stage two sampling unit for the following \nsubpopulation(s) of population ", typenames[itype], ":\n", temp.str, "\nEnter the following command to view the warning messages that were generated: \nwarnprnt() \n", sep=""))
         }
         if(any(temp > 0)) {
            temp.str <- vecprint(names(temp)[temp > 0])
            warn <- paste("Since they include one or more stage one sampling units with a single site, \nthe mean of the variance estimates for stage one sampling units with two or \nmore sites will be used as the variance estimate for stage one sampling units \nwith one site for the following subpopulation(s) of population\n", typenames[itype], ":\n", temp.str, sep="")
            act <- "The mean of the variance estimates will be used.\n"
            warn.df <- rbind(warn.df, data.frame(func=I(fname),
               subpoptype=NA, subpop=NA, indicator=NA, stratum=NA,
               warning=I(warn), action=I(act)))
         }
      }
   }

# As necessary, assign missing values to the design variables

   if(is.null(xcoord))
      xcoord <- rep(NA, nresp)
   if(is.null(ycoord))
      ycoord <- rep(NA, nresp)
   if(is.null(stratum))
      stratum <- rep(NA, nresp)
   if(is.null(cluster))
      cluster <- rep(NA, nresp)
   if(is.null(wgt1))
      wgt1 <- rep(NA, nresp)
   if(is.null(xcoord1))
      xcoord1 <- rep(NA, nresp)
   if(is.null(ycoord1))
      ycoord1 <- rep(NA, nresp)
   if(is.null(support))
      support <- rep(NA, nresp)
   if(is.null(swgt))
      swgt <- rep(NA, nresp)
   if(is.null(swgt1))
      swgt1 <- rep(NA, nresp)

# Recreate the design data frame

   design_2 <- data.frame(siteID=siteID, wgt=wgt, xcoord=xcoord,
      ycoord=ycoord, stratum=stratum, cluster=cluster, wgt1=wgt1,
      xcoord1=xcoord1, ycoord1=ycoord1, support=support, swgt=swgt,
      swgt1=swgt1)

#
# Begin the section for categorical response variables
#

catsum <- NULL
if(!is.null(data.cat)) {
   nvar <- ncol(data.cat)
   varnames <- names(data.cat)
   nrow <- 0 

# Loop through all categorical response variables

   for(ivar in 2:nvar) {

# Loop through all types of populations

      for(itype in 2:ntypes) {

# Find unique subpopulations of this type of population

         subpopnames <- levels(factor(subpop[,itype]))	

# Loop through all subpopulations of this type

         for(isubpop in 1:length(subpopnames)) {

#
# Select sites in a subpopulation for survey one
#

            subpop.ind_1 <- subpop_1[,itype] == subpopnames[isubpop]
            subpop.ind_1[is.na(subpop.ind_1)] <- FALSE

# Determine whether the subpopulation is empty

            if(all(is.na(data.cat_1[subpop.ind_1,ivar]))) {
               next
            }

# Determine whether the subpopulation contains a single value

            if(sum(!is.na(data.cat_1[subpop.ind_1,ivar])) == 1) {
               warn.ind <- TRUE
               warn <- paste("Subpopulation", subpopnames[isubpop], "of population type", typenames[itype], "for indicator", varnames[ivar], "\nin survey one contains a single value.  No analysis was performed.\n")
               act <- "None.\n"
               warn.df <- rbind(warn.df, data.frame(func=I(fname),
                  subpoptype=I(typenames[itype]),
                  subpop=I(subpopnames[isubpop]), indicator=I(varnames[ivar]),
                  stratum=NA,  warning=I(warn), action=I(act)))
               next
            }

#
# Select sites in a subpopulation for survey two
#

            subpop.ind_2 <- subpop_2[,itype] == subpopnames[isubpop]
            subpop.ind_2[is.na(subpop.ind_2)] <- FALSE

# Determine whether the subpopulation is empty

            if(all(is.na(data.cat_2[subpop.ind_2,ivar]))) {
               next
            }

# Determine whether the subpopulation contains a single value

            if(sum(!is.na(data.cat_2[subpop.ind_2,ivar])) == 1) {
               warn.ind <- TRUE
               warn <- paste("Subpopulation", subpopnames[isubpop], "of population type", typenames[itype], "for indicator", varnames[ivar], "\nin survey two contains a single value.  No analysis was performed.\n")
               act <- "None.\n"
               warn.df <- rbind(warn.df, data.frame(func=I(fname),
                  subpoptype=I(typenames[itype]),
                  subpop=I(subpopnames[isubpop]), indicator=I(varnames[ivar]),
                  stratum=NA,  warning=I(warn), action=I(act)))
               next
            }

# Create logical vectors for subsetting the repeat_1 and repeat_2 objects that
# ensure both visits belong to the subpopulation.

            indx_1 <- match(sites_1[subpop.ind_1, 1], repeats[, 1], nomatch=0)
            indx_1 <- indx_1[indx_1 > 0]
            indx_2 <- match(sites_2[subpop.ind_2, 1], repeats[, 2], nomatch=0)
            indx_2 <- indx_2[indx_2 > 0]
            repeat.ind_1 <- repeat_1[subpop.ind_1]
            ind <- indx_1 %in% indx_2
            if(any(!ind)) {
               repeat.ind_1[repeat.ind_1 == TRUE] <- ind
               warn.ind <- TRUE
               temp.str <- vecprint(repeats[indx_1[!ind], 1])
               warn <- paste("The following repeated visit site IDs for subpopulation ", subpopnames[isubpop], "\nof population type ", typenames[itype], " for indicator ", varnames[ivar], "\nin survey one did not have analogous site IDs present in survey two:\n", temp.str, sep="")
               act <- "The listed repeated visit sites were not used for covariance estimation.\n"
               warn.df <- rbind(warn.df, data.frame(func=I(fname),
                  subpoptype=I(typenames[itype]),
                  subpop=I(subpopnames[isubpop]), indicator=I(varnames[ivar]),
                  stratum=NA,  warning=I(warn), action=I(act)))
            }
            repeat.ind_2 <- repeat_2[subpop.ind_2]
            ind <- indx_2 %in% indx_1
            if(any(!ind)) {
               repeat.ind_2[repeat.ind_2 == TRUE] <- ind
               warn.ind <- TRUE
               temp.str <- vecprint(repeats[indx_2[!ind], 2])
               warn <- paste("The following repeated visit site IDs for subpopulation ", subpopnames[isubpop], "\nof population type ", typenames[itype], " for indicator ", varnames[ivar], "\nin survey two did not have analogous site IDs present in survey one:\n", temp.str, sep="")
               act <- "The listed repeated visit sites were not used for covariance estimation.\n"
               warn.df <- rbind(warn.df, data.frame(func=I(fname),
                  subpoptype=I(typenames[itype]),
                  subpop=I(subpopnames[isubpop]), indicator=I(varnames[ivar]),
                  stratum=NA,  warning=I(warn), action=I(act)))
            }

# For a stratified sample, remove values from pcfsize, N.cluster, and stage1size
# for strata that do not occur in the subpopulation for survey one

            if(stratum.ind_1) {
               temp.pcfsize_1 <- pcfsize_1[!is.na(match(names(pcfsize_1),
                  unique(design_1[subpop.ind_1, 5])))]
               temp.N.cluster_1 <- N.cluster_1[!is.na(match(names(N.cluster_1),
                  unique(design_1[subpop.ind_1, 5])))]
               temp.stage1size_1 <- stage1size_1[!is.na(match(names(stage1size_1),
                  unique(design_1[subpop.ind_1, 5])))]
            } else {
               temp.pcfsize_1 <- pcfsize_1
               temp.N.cluster_1 <- N.cluster_1
               temp.stage1size_1 <- stage1size_1
           }

# For a stratified sample, remove values from pcfsize, N.cluster, and stage1size
# for strata that do not occur in the subpopulation for survey two

            if(stratum.ind_2) {
               temp.pcfsize_2 <- pcfsize_2[!is.na(match(names(pcfsize_2),
                  unique(design_2[subpop.ind_2, 5])))]
               temp.N.cluster_2 <- N.cluster_2[!is.na(match(names(N.cluster_2),
                  unique(design_2[subpop.ind_2, 5])))]
               temp.stage1size_2 <- stage1size_2[!is.na(match(names(stage1size_2),
                  unique(design_2[subpop.ind_2, 5])))]
            } else {
               temp.pcfsize_2 <- pcfsize_2
               temp.N.cluster_2 <- N.cluster_2
               temp.stage1size_2 <- stage1size_2
           }

# Select values from popsize for survey one

            if(is.list(popsize_1[[itype-1]]))
               temp.popsize_1 <- popsize_1[[itype-1]][[isubpop]]
            else
               temp.popsize_1 <- popsize_1[[itype-1]]

# Select values from popsize for survey two

            if(is.list(popsize_2[[itype-1]]))
               temp.popsize_2 <- popsize_2[[itype-1]][[isubpop]]
            else
               temp.popsize_2 <- popsize_2[[itype-1]]

# Calculate change estimates for each category of the response variable

            temp <- change.est(resp.ind="cat",
                               z_1=data.cat_1[subpop.ind_1,ivar],
                               wgt_1=design_1[subpop.ind_1,2],
                               x_1=design_1[subpop.ind_1,3],
                               y_1=design_1[subpop.ind_1,4],
                               repeat_1=repeat.ind_1,
                               z_2=data.cat_2[subpop.ind_2,ivar],
                               wgt_2=design_2[subpop.ind_2,2],
                               x_2=design_2[subpop.ind_2,3],
                               y_2=design_2[subpop.ind_2,4],
                               repeat_2=repeat.ind_2,
                               revisitwgt=revisitwgt,
                               stratum_1=design_1[subpop.ind_1,5],
                               stratum_2=design_2[subpop.ind_2,5],
                               cluster_1=design_1[subpop.ind_1,6],
                               cluster_2=design_2[subpop.ind_2,6],
                               wgt1_1=design_1[subpop.ind_1,7],
                               x1_1=design_1[subpop.ind_1,8],
                               y1_1=design_1[subpop.ind_1,9],
                               wgt1_2=design_2[subpop.ind_2,7],
                               x1_2=design_2[subpop.ind_2,8],
                               y1_2=design_2[subpop.ind_2,9],
                               popsize_1=temp.popsize_1,
                               popsize_2=temp.popsize_2,
                               popcorrect_1=pcfactor.ind_1,
                               pcfsize_1=temp.pcfsize_1,
                               N.cluster_1=temp.N.cluster_1,
                               stage1size_1=temp.stage1size_1,
                               support_1=design_1[subpop.ind_1,10],
                               popcorrect_2=pcfactor.ind_2,
                               pcfsize_2=temp.pcfsize_2,
                               N.cluster_2=temp.N.cluster_2,
                               stage1size_2=temp.stage1size_2,
                               support_2=design_2[subpop.ind_2,10],
                               sizeweight_1=swgt.ind_1,
                               swgt_1=design_1[subpop.ind_1,11],
                               swgt1_1=design_1[subpop.ind_1,12],
                               sizeweight_2=swgt.ind_2,
                               swgt_2=design_2[subpop.ind_1,11],
                               swgt1_2=design_2[subpop.ind_1,12],
                               vartype_1=vartype_1,
                               vartype_2=vartype_2,
                               conf=conf,
                               check.ind=FALSE,
                               warn.ind=warn.ind,
                               warn.df=warn.df,
                               warn.vec=c(typenames[itype],
                                          subpopnames[isubpop],
                                          varnames[ivar]))
            temp.cat <- temp$Results
            warn.ind <- temp$warn.ind
            warn.df <- temp$warn.df

# Assign change estimates for the response variable to a data frame

            if(nrow == 0) {
               nn <- nrow(temp.cat)
               catsum <- data.frame(Type=rep(typenames[itype],nn), 
                  Subpopulation=rep(subpopnames[isubpop],nn), 
                  Indicator=rep(varnames[ivar],nn), temp.cat)
               nrow <- nn
            } else {
               nn <- nrow(temp.cat)
               catsum <- rbind(catsum, data.frame(Type=rep(typenames[itype],nn), 
                  Subpopulation=rep(subpopnames[isubpop],nn), 
                  Indicator=rep(varnames[ivar],nn), temp.cat, 
                  row.names=(nrow+1):(nrow+nn)))
               nrow <- nrow + nn
            }

# End of the loop for subpopulations

         }

# End of the loop for type of population

      }

# End of the loop for response variables

   }

# End of the section for categorical response variables

}

#
# Begin the section for continuous response variables
#

contsum_mean <- NULL
contsum_median <- NULL
if(!is.null(data.cont)) {
   nvar <- ncol(data.cont)
   varnames <- names(data.cont)
   nrow1 <- 0 
   nrow2 <- 0 

# Loop through all continuous response variables

   for(ivar in 2:nvar) {

# Loop through all types of populations

      for(itype in 2:ntypes) {

# Find unique subpopulations of this type of population

         subpopnames <- levels(factor(subpop[,itype]))	

# Loop through all subpopulations of this type

         for(isubpop in 1:length(subpopnames)) {

#
# Select sites in a subpopulation for survey one
#

            subpop.ind_1 <- subpop_1[,itype] == subpopnames[isubpop]
            subpop.ind_1[is.na(subpop.ind_1)] <- FALSE

# Determine whether the subpopulation is empty

            if(all(is.na(data.cont_1[subpop.ind_1,ivar]))) {
               next
            }

# Determine whether the subpopulation contains a single value

            if(sum(!is.na(data.cont_1[subpop.ind_1,ivar])) == 1) {
               warn.ind <- TRUE
               warn <- paste("Subpopulation", subpopnames[isubpop], "of population type", typenames[itype], "for indicator", varnames[ivar], "\nin survey one contains a single value.  No analysis was performed.\n")
               act <- "None.\n"
               warn.df <- rbind(warn.df, data.frame(func=I(fname),
                  subpoptype=I(typenames[itype]),
                  subpop=I(subpopnames[isubpop]), indicator=I(varnames[ivar]),
                  stratum=NA,  warning=I(warn), action=I(act)))
               next
            }

#
# Select sites in a subpopulation for survey two
#

            subpop.ind_2 <- subpop_2[,itype] == subpopnames[isubpop]
            subpop.ind_2[is.na(subpop.ind_2)] <- FALSE

# Determine whether the subpopulation is empty

            if(all(is.na(data.cont_2[subpop.ind_2,ivar]))) {
               next
            }

# Determine whether the subpopulation contains a single value

            if(sum(!is.na(data.cont_2[subpop.ind_2,ivar])) == 1) {
               warn.ind <- TRUE
               warn <- paste("Subpopulation", subpopnames[isubpop], "of population type", typenames[itype], "for indicator", varnames[ivar], "\nin survey two contains a single value.  No analysis was performed.\n")
               act <- "None.\n"
               warn.df <- rbind(warn.df, data.frame(func=I(fname),
                  subpoptype=I(typenames[itype]),
                  subpop=I(subpopnames[isubpop]), indicator=I(varnames[ivar]),
                  stratum=NA,  warning=I(warn), action=I(act)))
               next
            }

# Create logical vectors for subsetting the repeat_1 and repeat_2 objects that
# ensure both visits belong to the subpopulation.

            indx_1 <- match(sites_1[subpop.ind_1, 1], repeats[, 1], nomatch=0)
            indx_1 <- indx_1[indx_1 > 0]
            indx_2 <- match(sites_2[subpop.ind_2, 1], repeats[, 2], nomatch=0)
            indx_2 <- indx_2[indx_2 > 0]
            repeat.ind_1 <- repeat_1[subpop.ind_1]
            ind <- indx_1 %in% indx_2
            if(any(!ind)) {
               repeat.ind_1[repeat.ind_1 == TRUE] <- ind
               warn.ind <- TRUE
               temp.str <- vecprint(repeats[indx_1[!ind], 1])
               warn <- paste("The following repeated visit site IDs for subpopulation ", subpopnames[isubpop], "\nof population type ", typenames[itype], " for indicator ", varnames[ivar], "\nin survey one did not have analogous site IDs present in survey two:\n", temp.str, sep="")
               act <- "The listed repeated visit sites were not used for covariance estimation.\n"
               warn.df <- rbind(warn.df, data.frame(func=I(fname),
                  subpoptype=I(typenames[itype]),
                  subpop=I(subpopnames[isubpop]), indicator=I(varnames[ivar]),
                  stratum=NA,  warning=I(warn), action=I(act)))
            }
            repeat.ind_2 <- repeat_2[subpop.ind_2]
            ind <- indx_2 %in% indx_1
            if(any(!ind)) {
               repeat.ind_2[repeat.ind_2 == TRUE] <- ind
               warn.ind <- TRUE
               temp.str <- vecprint(repeats[indx_2[!ind], 2])
               warn <- paste("The following repeated visit site IDs for subpopulation ", subpopnames[isubpop], "\nof population type ", typenames[itype], " for indicator ", varnames[ivar], "\nin survey two did not have analogous site IDs present in survey one:\n", temp.str, sep="")
               act <- "The listed repeated visit sites were not used for covariance estimation.\n"
               warn.df <- rbind(warn.df, data.frame(func=I(fname),
                  subpoptype=I(typenames[itype]),
                  subpop=I(subpopnames[isubpop]), indicator=I(varnames[ivar]),
                  stratum=NA,  warning=I(warn), action=I(act)))
            }

# For a stratified sample, remove values from pcfsize, N.cluster, and stage1size
# for strata that do not occur in the subpopulation for survey one

            if(stratum.ind_1) {
               temp.pcfsize_1 <- pcfsize_1[!is.na(match(names(pcfsize_1),
                  unique(design_1[subpop.ind_1, 5])))]
               temp.N.cluster_1 <- N.cluster_1[!is.na(match(names(N.cluster_1),
                  unique(design_1[subpop.ind_1, 5])))]
               temp.stage1size_1 <- stage1size_1[!is.na(match(names(stage1size_1),
                  unique(design_1[subpop.ind_1, 5])))]
            } else {
               temp.pcfsize_1 <- pcfsize_1
               temp.N.cluster_1 <- N.cluster_1
               temp.stage1size_1 <- stage1size_1
           }

# For a stratified sample, remove values from pcfsize, N.cluster, and stage1size
# for strata that do not occur in the subpopulation for survey two

            if(stratum.ind_2) {
               temp.pcfsize_2 <- pcfsize_2[!is.na(match(names(pcfsize_2),
                  unique(design_2[subpop.ind_2, 5])))]
               temp.N.cluster_2 <- N.cluster_2[!is.na(match(names(N.cluster_2),
                  unique(design_2[subpop.ind_2, 5])))]
               temp.stage1size_2 <- stage1size_2[!is.na(match(names(stage1size_2),
                  unique(design_2[subpop.ind_2, 5])))]
            } else {
               temp.pcfsize_2 <- pcfsize_2
               temp.N.cluster_2 <- N.cluster_2
               temp.stage1size_2 <- stage1size_2
           }

# Select values from popsize for survey one

            if(is.list(popsize_1[[itype-1]]))
               temp.popsize_1 <- popsize_1[[itype-1]][[isubpop]]
            else
               temp.popsize_1 <- popsize_1[[itype-1]]

# Select values from popsize for survey two

            if(is.list(popsize_2[[itype-1]]))
               temp.popsize_2 <- popsize_2[[itype-1]][[isubpop]]
            else
               temp.popsize_2 <- popsize_2[[itype-1]]

# Calculate change estimates for each response variable

            temp <- change.est(resp.ind="cont",
                               z_1=data.cont_1[subpop.ind_1,ivar],
                               wgt_1=design_1[subpop.ind_1,2],
                               x_1=design_1[subpop.ind_1,3],
                               y_1=design_1[subpop.ind_1,4],
                               repeat_1=repeat.ind_1,
                               z_2=data.cont_2[subpop.ind_2,ivar],
                               wgt_2=design_2[subpop.ind_2,2],
                               x_2=design_2[subpop.ind_2,3],
                               y_2=design_2[subpop.ind_2,4],
                               repeat_2=repeat.ind_2,
                               revisitwgt=revisitwgt,
                               test=test,
                               stratum_1=design_1[subpop.ind_1,5],
                               stratum_2=design_2[subpop.ind_2,5],
                               cluster_1=design_1[subpop.ind_1,6],
                               cluster_2=design_2[subpop.ind_2,6],
                               wgt1_1=design_1[subpop.ind_1,7],
                               x1_1=design_1[subpop.ind_1,8],
                               y1_1=design_1[subpop.ind_1,9],
                               wgt1_2=design_2[subpop.ind_2,7],
                               x1_2=design_2[subpop.ind_2,8],
                               y1_2=design_2[subpop.ind_2,9],
                               popsize_1=temp.popsize_1,
                               popcorrect_1=pcfactor.ind_1,
                               pcfsize_1=temp.pcfsize_1,
                               N.cluster_1=temp.N.cluster_1,
                               stage1size_1=temp.stage1size_1,
                               support_1=design_1[subpop.ind_1,10],
                               popcorrect_2=pcfactor.ind_2,
                               pcfsize_2=temp.pcfsize_2,
                               N.cluster_2=temp.N.cluster_2,
                               stage1size_2=temp.stage1size_2,
                               support_2=design_2[subpop.ind_2,10],
                               sizeweight_1=swgt.ind_1,
                               swgt_1=design_1[subpop.ind_1,11],
                               swgt1_1=design_1[subpop.ind_1,12],
                               sizeweight_2=swgt.ind_2,
                               swgt_2=design_2[subpop.ind_1,11],
                               swgt1_2=design_2[subpop.ind_1,12],
                               vartype_1=vartype_1,
                               vartype_2=vartype_2,
                               conf=conf,
                               check.ind=FALSE,
                               warn.ind=warn.ind,
                               warn.df=warn.df,
                               warn.vec=c(typenames[itype],
                                          subpopnames[isubpop],
                                          varnames[ivar]))
            temp.cont1 <- temp$Results$Mean
            temp.cont2 <- temp$Results$Median
            warn.ind <- temp$warn.ind
            warn.df <- temp$warn.df

# Assign change estimates for the response variable to a data frame

            if(nrow1 == 0 & nrow2 == 0) {
               if(!is.null(temp.cont1)) {
                  nn <- nrow(temp.cont1)
                  contsum_mean <- data.frame(Type=rep(typenames[itype],nn), 
                     Subpopulation=rep(subpopnames[isubpop],nn), 
                     Indicator=rep(varnames[ivar],nn), temp.cont1)
                  nrow1 <- nn
               }
               if(!is.null(temp.cont2)) {
                  nn <- nrow(temp.cont2)
                  contsum_median <- data.frame(Type=rep(typenames[itype],nn), 
                     Subpopulation=rep(subpopnames[isubpop],nn), 
                     Indicator=rep(varnames[ivar],nn), temp.cont2)
                  nrow2 <- nn
               }
            } else {
               if(!is.null(temp.cont1)) {
                  nn <- nrow(temp.cont1)
                  contsum_mean <- rbind(contsum_mean,
                     data.frame(Type=rep(typenames[itype],nn), 
                     Subpopulation=rep(subpopnames[isubpop],nn), 
                     Indicator=rep(varnames[ivar],nn), temp.cont1, 
                     row.names=(nrow1+1):(nrow1+nn)))
                  nrow1 <- nrow1 + nn
               }
               if(!is.null(temp.cont2)) {
                  nn <- nrow(temp.cont2)
                  contsum_median <- rbind(contsum_median,
                     data.frame(Type=rep(typenames[itype],nn), 
                     Subpopulation=rep(subpopnames[isubpop],nn), 
                     Indicator=rep(varnames[ivar],nn), temp.cont2, 
                     row.names=(nrow2+1):(nrow2+nn)))
                  nrow2 <- nrow2 + nn
               }
            }

# End of the loop for subpopulations

         }

# End of the loop for type of population

      }

# End of the loop for response variables

   }

# End of the section for continuous response variables

}

# As necessary, output a message indicating that warning messages were generated
# during execution of the program

   if(warn.ind) {
      warn.df <<- warn.df
      if(nrow(warn.df) == 1)
         cat("During execution of the program, a warning message was generated.  The warning \nmessage is stored in a data frame named 'warn.df'.  Enter the following command \nto view the warning message: warnprnt()\n")
      else
         cat(paste("During execution of the program,", nrow(warn.df), "warning messages were generated.  The warning \nmessages are stored in a data frame named 'warn.df'.  Enter the following \ncommand to view the warning messages: warnprnt() \nTo view a subset of the warning messages (say, messages number 1, 3, and 5), \nenter the following command: warnprnt(m=c(1,3,5))\n"))
   }

# Assign consecutive numbers to the row names of the output data frames

   if(!is.null(catsum))
      row.names(catsum) <- 1:nrow(catsum)
   if(!is.null(contsum_mean))
      row.names(contsum_mean) <- 1:nrow(contsum_mean)
   if(!is.null(contsum_median))
      row.names(contsum_median) <- 1:nrow(contsum_median)

# Return the data frames as a list

   list(catsum=catsum, contsum_mean=contsum_mean, contsum_median=contsum_median)
}
