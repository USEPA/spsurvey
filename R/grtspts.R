###################################################################################
# Function: grtspts
# Programmers: Tony Olsen, Tom Kincaid
# Date: "`r format(Sys.time(),  '%B %d, %Y')`"
#'
#' Select a finite population spatially balanced sample using generalized random tessalation
#' stratified algorithm from a point sample frame based on a survey design specification.
#'
#' @param dsgn Named list of stratum design specifications which are also
#'   lists.  Stratum names must be subset of values in stratum argument.  Each
#'   stratum list has four components:
#'   \describe{
#'     \item{seltype}{the type of random selection, which must be one of
#'       following: "equal" - equal probability selection, "unequal" - unequal
#'       probability selection by the categories specified in caty.n and caty,
#'       or "proportional" - unequal probability selection proportional to
#'       auxiliary variable aux}
#'
#'     \item{panel}{named vector of sample sizes for each panel in stratum}
#'
#'     \item{caty.n}{if seltype equals "unequal", a named vector of sample sizes
#'       for each category specified by caty, where sum of the sample sizes
#'       must equal sum of the panel sample sizes, and names must be a subset of
#'       values in caty}
#'
#'     \item{over}{number of replacement sites ("over sample" sites) for the
#'       design, which is set equal to 0 if none are required. if seltype is unequal, then
#'       must be a named vector of over sample sizes for each category specified by caty
#'       and names must be the same as caty.n names)}
#'   }
#'   Example design for a stratified sample:\cr
#'     design=list(
#'       Stratum1=list(seltype="equal", panel=c(PanelOne=50), over=10),
#'       Stratum2=list(seltype="unequal", panel=c(PanelOne=50, PanelTwo=50),
#'         caty.n=c(CatyOne=25, CatyTwo=25, CatyThree=25, CatyFour=25),
#'         over=c(CatyOne=10, CatyTwo=10, CatyThree=5, CatyFour=5))
#'   Example design for an unstratified sample:\cr
#'     design <- list(
#'       None=list(seltype="unequal", panel=c(Panel1=50, Panel2=100, Panel3=50),
#'         caty.n=c("Caty 1"=50, "Caty 2"=25, "Caty 3"=25, "Caty 4"=25,"Caty 5"=75),
#'         over=c("Caty 1"=10, "Caty 2"=10, "Caty 3"=20, "Caty 4"=20,"Caty 5"=20)))
#'
#' @param sframe Sample frame for points as an sf object. If the design is stratified,
#'   unequal probability or has legacy sites, then sample frame must include variables
#'   that identify the stratum; category or auxillary variables for unequal
#'   selection; or which elements are legacy sites. The coordinate system for sframe
#'   must be one where distance for coordinates are meaningful.
#'
#' @param DesignID Name for the design, which is used to create a site ID for
#'   each site.  The default is "Site".
#'
#' @param SiteBegin Number to use for first site in the design.  The default
#'   is 1.
#'
#' @param strata_var Character string containing the name of the column from
#'   sframe that identifies stratum membership for each element in the frame.
#'   If stratum equals NULL, the design is unstratified.  The default is NULL.
#'
#' @param caty_var Character string containing the name of the column from
#'   sframe that identifies the unequal probability category for each element
#'   in the frame.  The default is NULL.
#'
#' @param aux_var Character string that is the name of the column from sframe that
#'   identifies the auxiliary variable value for each element in the sample frame
#'   that will be used to calculate inclusion probabilities when the survey design
#'   specifies that the selection type (seltype) is "proportional". Default is NULL.
#'
#' @param legacy_var Character string that is the name of the logical variable in sframe that
#'   identifies which elements in the sample frame are legacy elements. The logical
#'   variable equals TRUE if element is a legacy site and FALSE otherwise. Default is NULL.
#'
#' @param mindis Numeric value for the minimum distance required between elements
#'   in the sample.  Units must be the same units as in sf geometry. Default is NULL.
#'
#'@param maxtry Number of maximum attempts to ensure minimum distance between sites.
#'   Default is 10.
#'
#' @param startlev Initial number of hierarchical levels to use for the GRTS
#'   grid, which must be less than or equal to maxlev (if maxlev is specified)
#'   and cannot be greater than 11.  The default is NULL.
#'
#' @param maxlev Maxmum number of hierarchical levels to use for the GRTS
#'   grid, which cannot be greater than 11.  The default is 11.
#'
#' @param shift.grid Logical value. If TRUE, then hierarchical grid is shifted. If
#'   FALSE, then hierarchical grid not shifted.
#'
#' @return Return A class sf object containing the sites selected to meet the
#'   survey design requirements.
#'
#' @section Other functions required:
#'   \describe{
#'     \item{\code{{\link{junk}}}{What it does}
#'     }
#'
#' @author Tony Olsen email{olsen.tony@epa.gov}
#'
#' @keywords survey
#'
#' @examples
#' \dontrun{
#'   test_design <- list(
#'     Stratum1=list(panel=c(PanelOne=50), seltype="Equal", over=10),
#'     Stratum2=list(panel=c(PanelOne=50, PanelTwo=50), seltype="Unequal",
#'       caty.n=c(CatyOne=25, CatyTwo=25, CatyThree=25, CatyFour=25),
#'       over=c(CatyOne=10, CatyTwo=10, CatyThree=5, CatyFour=5)))
#'   test.sample <- grts(dsgn=test_design, sframe = "test_sf" DesignID="TestSite",
#'     stratum="test_stratum", mdcaty="test_mdcaty")
#' }
#'
#' @export
#################################################################################

grtspts <- function(dsgn, sframe, DesignID = "Site", SiteBegin = 1, stratum_var = NULL ,
                    caty_var = NULL, aux_var = NULL, legacy_var = NULL, mindis = NULL,
                    maxtry = 10, startlev = NULL, maxlev = 11, shift.grid = TRUE) {


  # check input. If errors, dsgn_check will stop grtspts and report errors.
  dsgn_check(dsgn, sframe, DesignID, SiteBegin, stratum_var, caty_var, aux_var,
             legacy_var, mindis, startlev, maxlev)

  # Create warning indicator and data frame to collect all potential issues during
  # sample selection
  warn.ind <- FALSE
  warn.df <- data.frame(stratum = "Stratum", func = "Calling Function", warn = "Message")

  # Create unique ID values
  sframe$id <- 1:nrow(sframe)

  # Assign stratum variable or create it if design not stratified and variable not provided.
  if(is.null(stratum_var)) {
    stratum_var <- "stratum_var"
    sframe$stratum <- "None"
  } else {
    sframe$stratum <- sframe[[stratum_var]]
  }

  # set caty, aux and legacy variables if needed
  if(!is.null(caty_var)) {
    sframe$caty <- sframe[[caty_var]]
  }
  if(!is.null(aux_var)) {
    sframe$aux <- sframe[[aux_var]]
  }
  if(!is.null(legacy_var)) {
    sframe$legacy <- sframe[[legacy_var]]
  }


  # Begin the loop for strata
  strata.names <- names(dsgn)
  # initialize first stratum
  first <- TRUE

  for(s in strata.names) {

    # detemine overall sample size required from dsgn for s stratum
    # if over sample is NULL set to zero
    if(is.null(dsgn[[s]]$over.n)) dsgn[[s]]$over.n <- 0
    if(dsgn[[s]]$seltype == "equal" | dsgn[[s]]$seltype == "proportional"){
      nsamp <- sum(dsgn[[s]]$panel) + dsgn[[s]]$over.n
      } else {
      nsamp <- dsgn[[s]]$caty.n + dsgn[[s]]$over.n
      }

    # subset sframe to s stratum
    sftmp <- sframe[sframe$stratum == s, ]

    # Determine number of elements in stratum
    Nstratum <- NROW(sftmp)

    # Basic GRTS sample: no legacy sites or minimum distance
    if(is.null(legacy_var) & is.null(mindis)) {
      # compute inclusion probabilities
      ip <- grtspts_ip(type = dsgn[[s]]$seltype, nsamp = nsamp,
                             Nstratum = Nstratum, caty = sftmp$caty, aux = sftmp$aux,
                            warn.ind = warn.ind,  warn.df = warn.df)
      sftmp$ip <- ip$ip
      if(ip$warn.ind) {
        warn.ind <- ip$warn.ind
        warn.df <- ip$warn.df
        warn.df$stratum <- ifelse(is.na(warn.df$stratum), s, warn.df$stratum)
      }

      # Create hierarchical grid based on number of levels required
      grts_grid <- numLevels(sum(nsamp), sftmp, shift.grid, startlev, maxlev,
                            warn.ind = warn.ind,  warn.df = warn.df)
      if(grts_grid$warn.ind) {
        warn.ind <- grts_grid$warn.ind
        warn.df <- grts_grid$warn.df
        warn.df$stratum <- ifelse(is.na(warn.df$stratum), s, warn.df$stratum)
      }

      # select sites
      sites <- grtspts_select(sftmp, grts_grid, samplesize = sum(nsamp),
                              SiteBegin = SiteBegin, warn.ind = warn.ind, warn.df = warn.df)
      warn.ind <- sites$warn.ind
      warn.df <- sites$warn.df
      if(warn.ind) {
        warn.df$stratum <- ifelse(is.na(warn.df$stratum), s, warn.df$stratum)
      }
      sites <- sites$rho
    }
    # End of Basic GRTS sample

    # GRTS sample with legacy sites and no minimum distance
    if(!is.null(legacy_var) & is.null(mindis)) {

     # compute inclusion probabilities ignoring legacy sites
     ip <- grtspts_ip(type = dsgn[[s]]$seltype, nsamp = nsamp,
                            Nstratum = Nstratum, caty = sftmp$caty, aux = sftmp$aux,
                            warn.ind = warn.ind, warn.df = warn.df)
     sftmp$ip <- ip$ip
     if(ip$warn.ind) {
       warn.ind <- ip$warn.ind
       warn.df <- ip$warn.df
       warn.df$stratum <- ifelse(is.na(warn.df$stratum), s, warn.df$stratum)
     }

      # Create hierarchical grid based on number of levels required
      grts_grid <- numLevels(nsamp, sftmp, shift.grid, startlev, maxlev,
                             warn.ind = warn.ind,  warn.df = warn.df)
      if(grts_grid$warn.ind) {
        warn.ind <- grts_grid$warn.ind
        warn.df <- grts_grid$warn.df
        warn.df$stratum <- ifelse(is.na(warn.df$stratum), s, warn.df$stratum)
      }

      # Adjust inclusion probabilities to account for legacy sites
      # save ip
      sftmp$ip_init <- sftmp$ip
      sftmp$ip <- grtspts_ipleg(sftmp$ip, sftmp$legacy)

      # adjust cell weights to use legacy inclusion probabilities
      grts_grid$cel.wt <- cellWeight(grts_grid$xc, grts_grid$yc,
                                     grts_grid$dx, grts_grid$dy, sftmp)

      # Select sites using adjusted inclusion probabilities
      sites <- grtspts_select(sftmp, grts_grid, samplesize = sum(nsamp),
                              SiteBegin = SiteBegin,  warn.ind = warn.ind,
                              warn.df = warn.df)
      if(sites$warn.ind) {
        warn.ind <- sites$warn.ind
        warn.df <- sites$warn.df
        warn.df$stratum <- ifelse(is.na(warn.df$stratum), s, warn.df$stratum)
      }
      sites <- sites$rho

      # Assign original inclusion probabilites to sites and drop legacy ip variable
      sites$ip <- sites$ip_init
      tmp <- names(sites)
      sites <- subset(sites, select = tmp[!(tmp %in% c("ip_init", "geometry"))])
    }


    # GRTS sample with minimum distance and with or without legacy sites
    if(!is.null(mindis)) {

      # compute inclusion probabilities assuming no legacy sites
      ip <- grtspts_ip(type = dsgn[[s]]$seltype, nsamp = nsamp,
                       Nstratum = Nstratum, caty = sftmp$caty, aux = sftmp$aux,
                       warn.ind = warn.ind, warn.df = warn.df)
      # save initial ip and set ip based on no legacy sites
      sftmp$ip_init <- ip$ip
      sftmp$ip <- sftmp$ip_init
      # check for warning messages
      if(ip$warn.ind) {
        warn.ind <- ip$warn.ind
        warn.df <- ip$warn.df
        warn.df$stratum <- ifelse(is.na(warn.df$stratum), s, warn.df$stratum)
      }

      # Create hierarchical grid based on number of levels required
      grts_grid <- numLevels(nsamp, sftmp, shift.grid, startlev, maxlev,
                             warn.ind = warn.ind,  warn.df = warn.df)
      if(grts_grid$warn.ind) {
        warn.ind <- grts_grid$warn.ind
        warn.df <- grts_grid$warn.df
        warn.df$stratum <- ifelse(is.na(warn.df$stratum), s, warn.df$stratum)
      }

      # compute inclusion probabilies when have legacy sites
      if(!is.null(legacy_var)) {
        # Adjust inclusion probabilities to account for legacy sites
        sftmp$ip <- grtspts_ipleg(sftmp$ip, sftmp$legacy)
      }

      if(ip$warn.ind) {
        warn.ind <- ip$warn.ind
        warn.df <- ip$warn.df
        warn.df$stratum <- ifelse(is.na(warn.df$stratum), s, warn.df$stratum)
      }

      # adjust cell weights to use legacy inclusion probabilities
      grts_grid$cel.wt <- cellWeight(grts_grid$xc, grts_grid$yc,
                                     grts_grid$dx, grts_grid$dy, sftmp)

      # Given hierarchical grid, select sites then check sites for mindis, delete
      # and add sites as necessary
      sites <- grtspts_mindis(mindis, sftmp, grts_grid, samplesize = sum(nsamp),
                              SiteBegin = SiteBegin, stratum = s, legacy_var = legacy_var,
                              maxtry = maxtry, warn.ind = warn.ind, warn.df = warn.df)

      # check for warning messages
      if(sites$warn.ind) {
        warn.ind <- sites$warn.ind
        warn.df <- sites$warn.df
        warn.df$stratum <- ifelse(is.na(warn.df$stratum), s, warn.df$stratum)
      }
      sites <- sites$sites

    } # end section on mindis and legacy site option

  # Add sample for s stratum to the output object

  if(first) {
    rslts <- sites
    first <- FALSE
  } else {
    rslts <- rbind(rslts, sites)
  }
  SiteBegin <- SiteBegin + nrow(sites)
} # End the loop for strata

  # Remove the id attribute from rslts
  tmp <- names(rslts)
  rslts <- subset(rslts, select = tmp[!(tmp %in% c("id", "geometry"))])

  # Add DesignID name to the numeric siteID value to create a new siteID
  rslts$siteID <- as.character(gsub(" ","0", paste(DesignID,"-",
                                                   format(rslts$siteID), sep="")))
  
  # Calculate weights, asssign panels and over samples


  # As necessary, output a message indicating that warning messages were generated
  # during execution of the program

  if(warn.ind) {
    warn.df <<- warn.df
    if(nrow(warn.df) == 1){
      cat("During execution of the program, a warning message was generated.  The warning \nmessage is stored in a data frame named 'warn.df'.  Enter the following command \nto view the warning message: warnprnt()\n")
    } else {
      cat(paste("During execution of the program,", nrow(warn.df), "warning messages were generated.  The warning \nmessages are stored in a data frame named 'warn.df'.  Enter the following \ncommand to view the warning messages: warnprnt() \nTo view a subset of the warning messages (say, messages number 1, 3, and 5), \nenter the following command: warnprnt(m=c(1,3,5))\n"))
    }
  }

  # return the survey design sf object
  invisible(rslts)
}


