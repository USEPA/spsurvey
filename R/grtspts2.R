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
#'     \item{over}{number of nearby sites to be used as potential replacement(s) 
#'       if a site cannot be sampled for any reason. If specified, possible 
#'       values 1, 2, or 3. Default is NULL)}
#'   }
#'   Example design for a stratified sample:\cr
#'     design=list(
#'       Stratum1=list(seltype="equal", panel=c(PanelOne=50)),
#'       Stratum2=list(seltype="unequal", panel=c(PanelOne=50, PanelTwo=50),
#'         caty.n=c(CatyOne=25, CatyTwo=25, CatyThree=25, CatyFour=25),
#'         over=1)
#'   Example design for an unstratified sample:\cr
#'     design <- list(
#'       None=list(seltype="unequal", panel=c(Panel1=50, Panel2=100, Panel3=50),
#'         caty.n=c("Caty 1"=50, "Caty 2"=25, "Caty 3"=25, "Caty 4"=25,"Caty 5"=75),
#'         over=1))
#'
#' @param sframe Sample frame for points as an sf object. If the design is stratified,
#'   unequal probability or has legacy sites, then sample frame must include variables
#'   that identify the stratum; category or auxillary variables for unequal
#'   selection; or which elements are legacy sites. The coordinate system for sframe
#'   must be one where distance for coordinates is meaningful.
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
#'
#' @return An sf object containing the sites selected that meet the
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
#'       Stratum1=list(seltype="equal", panel=c(PanelOne=50), over=2),
#'       Stratum2=list(seltype="unequal", panel=c(PanelOne=50, PanelTwo=50),
#'         caty.n=c(CatyOne=25, CatyTwo=25, CatyThree=25, CatyFour=25),
#'         over=1)
#'   
#'   test.sample <- grts(dsgn=test_design, sframe = "test_sf" DesignID="TestSite",
#'     stratum="test_stratum", mdcaty="test_mdcaty")
#' }
#'
#' @export
#################################################################################

grtspts2 <- function(dsgn, sframe, DesignID = "Site", SiteBegin = 1, stratum_var = NULL ,
                    caty_var = NULL, aux_var = NULL, legacy_var = NULL, mindis = NULL,
                    maxtry = 10, startlev = NULL, maxlev = 11) {


  # check input. If errors, dsgn_check will stop grtspts and report errors.
  dsgn_check(dsgn, sframe, DesignID, SiteBegin, stratum_var, caty_var, aux_var,
             legacy_var, mindis, startlev, maxlev)

  # Create warning indicator and data frame to collect all potential issues during
  # sample selection
  warn.ind <- FALSE
  warn.df <- data.frame(stratum = "Stratum", func = "Calling Function", warn = "Message")

  # Create unique ID values and save variable names in sample frame provided on input
  geom_name <- attr(sframe, "sf_column")
  names.sframe <- names(sframe)[names(sframe) != geom_name]
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

  # Determine maximum number of over samples per site across strata
  strata.names <- names(dsgn)
  over.max <- NULL
  for(s in strata.names) {
    over.max <- max(over.max, dsgn[[s]]$over, na.rm = TRUE)
  }
    
  # Begin the loop for strata by initializing indicator for first stratum 
  # and set beginning site number for first over sample site.
  OverBegin <- 1
  first <- TRUE

  for(s in strata.names) {

    # detemine overall sample size required from dsgn for s stratum
    if(dsgn[[s]]$seltype == "equal" | dsgn[[s]]$seltype == "proportional"){
      nsamp <- sum(dsgn[[s]]$panel)
      } else {
        nsamp <- dsgn[[s]]$caty.n
      }
    
    # subset sframe to s stratum
    sftmp <- sframe[sframe$stratum == s, ]

    # Determine number of elements in stratum
    Nstratum <- NROW(sftmp)

    # If sel.type is "equal" or "proportional", set caty to same as stratum
    if(dsgn[[s]]$seltype == "equal" | dsgn[[s]]$seltype == "proportional") {
      sftmp$caty <- sftmp$stratum
    }

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
      grts_grid <- numLevels(sum(nsamp), sftmp, startlev, maxlev,
                            warn.ind = warn.ind,  warn.df = warn.df)
      if(grts_grid$warn.ind) {
        warn.ind <- grts_grid$warn.ind
        warn.df <- grts_grid$warn.df
        warn.df$stratum <- ifelse(is.na(warn.df$stratum), s, warn.df$stratum)
      }

      # select sites
      sites <- grtspts_select2(sftmp, grts_grid, samplesize = sum(nsamp), 
                               over = dsgn[[s]]$over,
                              SiteBegin = SiteBegin, OverBegin = OverBegin,
                              warn.ind = warn.ind, warn.df = warn.df)
      warn.ind <- sites$warn.ind
      warn.df <- sites$warn.df
      if(warn.ind) {
        warn.df$stratum <- ifelse(is.na(warn.df$stratum), s, warn.df$stratum)
      }
      if(!is.null(dsgn[[s]]$over)) {
        sites.over <- sites$over.samp
      }
      sites <- sites$rho
      
    } # End of Basic GRTS sample

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
      grts_grid <- numLevels(nsamp, sftmp, startlev, maxlev,
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
      sites <- grtspts_select2(sftmp, grts_grid, samplesize = sum(nsamp), 
                               over = dsgn[[s]]$over,
                              SiteBegin = SiteBegin,  OverBegin = OverBegin,
                              warn.ind = warn.ind, warn.df = warn.df)
      if(sites$warn.ind) {
        warn.ind <- sites$warn.ind
        warn.df <- sites$warn.df
        warn.df$stratum <- ifelse(is.na(warn.df$stratum), s, warn.df$stratum)
      }
      
      if(!is.null(dsgn[[s]]$over)) {
        sites.over <- sites$over.samp
      }
      sites <- sites$rho

      # Assign original inclusion probabilites to sites and drop legacy ip variable
      sites$ip <- sites$ip_init
      tmp <- names(sites)
      sites <- subset(sites, select = tmp[!(tmp %in% c("ip_init", "geometry"))])
      
      
    } # end basic loop


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
      grts_grid <- numLevels(nsamp, sftmp, startlev, maxlev,
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
                              over = dsgn[[s]]$over, SiteBegin = SiteBegin,  
                              OverBegin = OverBegin, 
                              stratum = s, legacy_var = legacy_var,
                              maxtry = maxtry, warn.ind = warn.ind, warn.df = warn.df)

      # check for warning messages
      if(sites$warn.ind) {
        warn.ind <- sites$warn.ind
        warn.df <- sites$warn.df
        warn.df$stratum <- ifelse(is.na(warn.df$stratum), s, warn.df$stratum)
      }
      
      # keep over sample and sample
      if(!is.null(dsgn[[s]]$over)) {
        sites.over <- sites$over.samp
      }
      sites <- sites$sites

    } # end section on mindis and legacy site option
    
    # Add panel structure
    sites$panel <- NA
    n.panel <- length(dsgn[[s]]$panel)
    samplesize <- sum(nsamp)
    if(nrow(sites) < samplesize) {
      n.short <- samplesize - nrow(sites)
      n.temp <- n.short / n.panel
      if(n.temp != floor(n.temp)) {
        n.temp <- c(ceiling(n.temp), rep(floor(n.temp), n.panel-1))
        i <- 1
        while(sum(n.temp) != n.short) {
          i <- i+1
          n.temp[i] <- n.temp[i] + 1
        }
      }
      np <- c(0, cumsum(dsgn[[s]]$panel - n.temp))
    } else {
      np <- c(0, cumsum(dsgn[[s]]$panel))
    }
    for(i in 1:n.panel) {
      sites$panel[(np[i]+1):np[i+1]] <- names(dsgn[[s]]$panel[i])
    }
    
    # create weights based on inclusion probability
    sites$wgt <- 1/sites$ip
    
    # If an over sample required select replacement sites for each site
    # add panel and weight variables first
    if(!is.null(dsgn[[s]]$over)) {
      sites.over$panel <- "OverSamp"
      sites.over$wgt <- 1/sites.over$ip
      sites.over <- replace_sites(dsgn[[s]]$over, over.max = over.max, sites = sites,
                                  sites.over = sites.over)
      # add new variables to sites
      sites$replsite <- ""
      sites$oversite <- "Base"
    }
    
    # Add sample for s stratum to the rslts and rslts.over objects
    if(first) {
      rslts.base <- sites
      rslts.over <- sites.over
      first <- FALSE
    } else {
      rslts.base <- rbind(rslts.base, sites)
      rslts.over <- rbind(rslts.over, sites.over)
    }
    
    # update site number for use in next stratum for both base and over sample.
    SiteBegin <- SiteBegin + nrow(sites)
    if(!is.null(dsgn[[s]]$over)) {
      OverBegin <- OverBegin + nrow(sites.over)
    }
    
    } # End the loop for strata
  

  # Combine base and over sample sites into one data frame
  rslts <- rbind(rslts.base, rslts.over)
  
  # Add DesignID name to the numeric siteID value to create a new siteID
  rslts$siteID <- as.character(gsub(" ","0", paste(DesignID,"-",
                                                   format(rslts$siteID), sep="")))
  
  # if over sample sites, assign base siteIDs to the replacement sites
  if(!is.null(over.max)) {
    tst <- match(rslts$replsite, rslts$id, nomatch = 0)
    rslts$replsite[rslts$replsite != ""] <- rslts$siteID[tst]
  }
  
  # sort by stratum, panel, oversite, then siteID before reassigning siteIDs
  rslts <- rslts[order(rslts$stratum, rslts$oversite, rslts$siteID),]
  rslts$siteID <- as.character(gsub(" ","0", paste(DesignID,"-", 
                                                   SiteBegin + 0:(nrow(rslts) - 1), sep="")))
  
  
  # Remove temporary variables and reorder variables in rslts
  if(is.null(legacy_var) & is.null(aux_var)) {
    rslts <- subset(rslts, select = c("siteID", "panel", "stratum", "caty", "wgt",
                                      "oversite", "replsite", names.sframe ))
  }
  if(is.null(legacy_var) & !is.null(aux_var)) {
    rslts <- subset(rslts, select = c("siteID", "panel", "stratum", "caty", "aux", 
                                      "wgt", names.sframe ))
  }
  if(!is.null(legacy_var) & is.null(aux_var)) {
    rslts <- subset(rslts, select = c("siteID", "panel", "stratum", "caty", "legacy", 
                                      "wgt", names.sframe ))
  }
  if(!is.null(legacy_var) & !is.null(aux_var)) {
    rslts <- subset(rslts, select = c("siteID", "panel", "stratum", "caty", "aux", 
                                      "legacy", "wgt", names.sframe ))
  }

  
  
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


