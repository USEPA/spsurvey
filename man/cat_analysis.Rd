% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cat_analysis.R
\name{cat_analysis}
\alias{cat_analysis}
\title{Categorical data analysis for probability survey data}
\usage{
cat_analysis(
  dframe,
  vars,
  subpops = NULL,
  siteID = "siteID",
  weight = "weight",
  xcoord = NULL,
  ycoord = NULL,
  stratumID = NULL,
  clusterID = NULL,
  weight1 = NULL,
  xcoord1 = NULL,
  ycoord1 = NULL,
  sizeweight = FALSE,
  sweight = NULL,
  sweight1 = NULL,
  fpc = NULL,
  popsize = NULL,
  vartype = "Local",
  jointprob = "overton",
  conf = 95
)
}
\arguments{
\item{dframe}{Data frame or \code{sf} object containing survey design variables, response
variables, and subpopulation (domain) variables.}

\item{vars}{Vector composed of character values that identify the
names of response variables in the \code{dframe} data frame.}

\item{subpops}{Vector composed of character values that identify the
names of subpopulation (domain) variables in the \code{dframe} data frame.
If a value is not provided, the value \code{"All_Sites"} is assigned to the
subpops argument and a factor variable named \code{"All_Sites"} that takes
the value \code{"All Sites"} is added to the \code{dframe} data frame.  The
default value is \code{NULL}.}

\item{siteID}{Character value providing name of the site ID variable in
the \code{dframe} data frame.  For a two-stage sample, the site ID variable
identifies stage two site IDs.  The default value is \code{"siteID"}.}

\item{weight}{Character value providing name of the survey design weight
variable in the \code{dframe} data frame.  For a two-stage sample, the
weight variable identifies stage two weights.  The default value is
\code{"weight"}.}

\item{xcoord}{Character value providing name of the x-coordinate variable in
the \code{dframe} data frame.  For a two-stage sample, the x-coordinate
variable identifies stage two x-coordinates.  Note that x-coordinates are
required for calculation of the local mean variance estimator.  The default
value is \code{NULL}.}

\item{ycoord}{Character value providing name of the y-coordinate variable in
the \code{dframe} data frame.  For a two-stage sample, the y-coordinate
variable identifies stage two y-coordinates.  Note that y-coordinates are
required for calculation of the local mean variance estimator.  The default
value is \code{NULL}.}

\item{stratumID}{Character value providing name of the stratum ID variable in
the \code{dframe} data frame.  The default value is \code{NULL}.}

\item{clusterID}{Character value providing the name of the cluster
(stage one) ID variable in the \code{dframe} data frame.  Note that cluster
IDs are required for a two-stage sample.  The default value is \code{NULL}.}

\item{weight1}{Character value providing name of the stage one weight
variable in the \code{dframe} data frame.  The default value is \code{NULL}.}

\item{xcoord1}{Character value providing the name of the stage one
x-coordinate variable in the \code{dframe} data frame.  Note that x
coordinates are required for calculation of the local mean variance
estimator.  The default value is \code{NULL}.}

\item{ycoord1}{Character value providing the name of the stage one
y-coordinate variable in the \code{dframe} data frame.  Note that
y-coordinates are required for calculation of the local mean variance
estimator.  The default value is \code{NULL}.}

\item{sizeweight}{Logical value that indicates whether size weights should be
used during estimation, where \code{TRUE} uses size weights and
\code{FALSE} does not use size weights. To employ size weights for a
single-stage sample, a value must be supplied for argument weight.  To
employ size weights for a two-stage sample, values must be supplied for
arguments \code{weight} and \code{weight1}. The default value is \code{FALSE}.}

\item{sweight}{Character value providing the name of the size weight variable
in the \code{dframe} data frame.  For a two-stage sample, the size weight
variable identifies stage two size weights.  The default value is
\code{NULL}.}

\item{sweight1}{Character value providing name of the stage one size weight
variable in the \code{dframe} data frame.  The default value is \code{NULL}.}

\item{fpc}{Object that specifies values required for calculation of the
  finite population correction factor used during variance estimation. The
  object must match the survey design in terms of stratification and whether
  the design is single-stage or two-stage.  For an unstratified design, the
  object is a vector.  The vector is composed of a single numeric value for a
  single-stage design.  For a two-stage unstratified design, the object is a
  named vector containing one more than the number of clusters in the sample,
  where the first item in the vector specifies the number of clusters in the
  population and each subsequent item specifies the number of stage two units
  for the cluster.  The name for the first item in the vector is arbitrary.
  Subsequent names in the vector identify clusters and must match the cluster
  IDs.  For a stratified design, the object is a named list of vectors, where
  names must match the strata IDs.  For each stratum, the format of the
  vector is identical to the format described for unstratified single-stage
  and two-stage designs.  Note that the finite population correction factor
  is not used with the local mean variance estimator.

  Example fpc for a single-stage unstratified survey design:

  \verb{fpc <- 15000}

  Example fpc for a single-stage stratified survey design:

  \verb{fpc <- list(
    Stratum_1 = 9000,
    Stratum_2 = 6000)
   }

  Example fpc for a two-stage unstratified survey design:

  \verb{fpc <- c(
    Ncluster = 150,
    Cluster_1 = 150,
    Cluster_2 = 75,
    Cluster_3 = 75,
    Cluster_4 = 125,
    Cluster_5 = 75)
  }

  Example fpc for a two-stage stratified survey design:

  \verb{fpc <- list(
    Stratum_1 = c(
      Ncluster_1 = 100,
      Cluster_1 = 125,
      Cluster_2 = 100,
      Cluster_3 = 100,
      Cluster_4 = 125,
      Cluster_5 = 50),
    Stratum_2 = c(
      Ncluster_2 = 50,
      Cluster_1 = 75,
      Cluster_2 = 150,
      Cluster_3 = 75,
      Cluster_4 = 75,
      Cluster_5 = 125))
  }}

\item{popsize}{Object that provides values for the population argument of
  the \code{calibrate} or \code{postStratify} functions in the survey package.  For the
  \code{calibrate} function, the object is a named list, where the names
  identify factor variables in the \code{dframe} data frame.  Each element
  of the list is a named vector containing the population total for each
  level of the associated factor variable.  For the \code{postStratify}
  function, the object is either a data frame, table, or xtabs
  object that provides the population total for all combinations of selected
  factor varaibles in the \code{dframe} data frame.  If a data frame is used
  for \code{popsize}, the variable containing population totals must be the
  last variable in the data frame.  If a table is used for \code{popsize},
  the table must have named \code{dimnames} where the names identify factor
  variables in the \code{dframe} data frame.  If the popsize argument is
  equal to \code{NULL}, then neither calibration nor post-stratification is
  performed.  The default value is
  \code{NULL}.

  Example popsize for calibration:
  \verb{popsize <- list(
    Ecoregion = c(
      East = 750,
      Central = 500,
      West = 250),
    Type = c(
      Streams = 1150,
      Rivers = 350))
  }
  Example popsize for post-stratification using a data frame:
  \verb{popsize <- data.frame(
    Ecoregion = rep(c("East", "Central", "West"),
      rep(2, 3)),
    Type = rep(c("Streams", "Rivers"), 3),
    Total = c(575, 175, 400, 100, 175, 75))
  }

  Example popsize for post-stratification using a table:

  \verb{popsize <- with(MySurveyFrame,
    table(Ecoregion, Type))}

  Example popsize for post-stratification using an xtabs object:

  \verb{popsize <- xtabs(~Ecoregion + Type,
    data = MySurveyFrame)}}

\item{vartype}{Character value providing the choice of the variance
estimator, where \code{"Local"} indicates the local mean estimator, \code{"SRS"} indicates the
simple random sampling estimator, \code{"HT"} indicates the Horvitz-Thompson
estimator, and \code{"YG"} indicates the Yates-Grundy estimator.  The default value
is \code{"Local"}.}

\item{jointprob}{Character value providing the choice of joint inclusion
probability approximation for use with Horvitz-Thompson and Yates-Grundy
variance estimators, where \code{"overton"} indicates the Overton
approximation, \code{"hr"} indicates the Hartley-Rao approximation, and
\code{"brewer"} equals the Brewer approximation.  The default value is
\code{"overton"}.}

\item{conf}{Numeric value providing the confidence level.  The default value
is \code{95}.}
}
\value{
A data frame of population estimates for all combinations of
  subpopulations, categories within each subpopulation, response variables,
  and categories within each response variable.  Estimates are provided for
  proportion and total of the population plus standard error, margin of
  error, and confidence interval estimates.
}
\description{
This function organizes input and output for analysis of categorical data
generated by a probability survey.  The input data argument can be either a
data frame or a simple features (\code{sf}) object.  If an \code{sf} object is used,
coordinates are extracted from the geometry column in the object, arguments
\code{xcoord} and \code{ycoord} are assigned values \code{"xcoord"} and
\code{"ycoord"}, respectively, and the geometry column is dropped from the
object.
}
\examples{
dframe <- data.frame(
  siteID = paste0("Site", 1:100),
  wgt = runif(100, 10, 100),
  xcoord = runif(100),
  ycoord = runif(100),
  stratum = rep(c("Stratum1", "Stratum2"), 50),
  CatVar = rep(c("north", "south", "east", "west"), 25),
  All_Sites = rep("All Sites", 100),
  Resource_Class = rep(c("Good", "Poor"), c(55, 45))
)
myvars <- c("CatVar")
mysubpops <- c("All_Sites", "Resource_Class")
mypopsize <- data.frame(
  Resource_Class = c("Good", "Poor"),
  Total = c(4000, 1500)
)
cat_analysis(dframe,
  vars = myvars, subpops = mysubpops, siteID = "siteID",
  weight = "wgt", xcoord = "xcoord", ycoord = "ycoord",
  stratumID = "stratum", popsize = mypopsize
)
}
\seealso{
\describe{
  \item{\code{\link{cont_analysis}}}{ for continuous variable analysis}
  }
}
\author{
Tom Kincaid \email{Kincaid.Tom@epa.gov}
}
\keyword{survey}
\keyword{univar}