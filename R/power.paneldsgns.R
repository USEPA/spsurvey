# File: power.paneldsgns.R
#
# Contents: Functions to create panel designs,  compute and plot power
#           associated with panel designs given a linear trend. Linear
#           trend and change between two time period power options.
#

###############################################################################
# Function: revisit_dsgn
# Purpose: Create panel design using structure defined by McDonald (2003)
# Programmer: Tony Olsen
# Date: February, 2017
#
#' Create panel revisit design
#'
#' Create a revisit design for panels in a survey that specifies the time
#' periods that members of each panel will be sampled. Three basic panel design
#' structures may be created: always revisit panel, serially alternating panels, or
#' rotating panels.
#'
#' @param n.period  Number of time periods for the panel design. For example,
#'   number of periods if sampling occurs once per period or number of months if
#'   sampling occurs once per month.
#'
#' @param panels List of lists where each list specifies a revisit panel
#'   structure. Each sublist consists of four components: n - sample size for
#'   each panel in the sublist, pnl_dsgn - a vector with an even number of
#'   elements specifying the panel revisit schedule in terms of the number of
#'   consecutive time periods sample units will be sampled, followed by number
#'   of consecutive time periods skipped, and then repeated as necessary. pnl_n
#'   - number of panels in the sublist, and start_option - option for starting
#'   the pnl_dsgn (None, Partial_Begin, or Partial_End). Three basic panel
#'   structures are possible: a) if pnl_dsgn ends in 0, then the sample units
#'   are visited on all subsequent time periods, b) if pnl_dsgn ends in NA, then
#'   panel follows a rotating panel structure, and c) if pnl_dsgn ends in any
#'   number > 0, then panel follows a serially alternating panel structure. See
#'   details for further information.
#'
#' @param begin  Numeric name of first sampling occasion, e.g. a specific period.
#'
#' @param skip  Number of time periods to skip between planned sampling
#'   periods, e.g., sampling will occur only every 5 periods if skip = 5.
#'
#' @details  The function creates revisit designs using the concepts in McDonald
#'   (2003) to specify the revisit pattern across time periods for each panel.
#'   The panel revisit schedule is specificed by a vector. Odd positions in
#'   vector specify the number of consecutive time periods when panel units are
#'   sampled. Even positions in vector specify the number of consecutive time
#'   periods when panel units are not sampled.
#'
#'   If last even position is a "0", then a single panel follows an always
#'   revisit panel structure.  After satisfying the initial revisit schedule
#'   specified prior to the "0", units in a panel are always visited for rest of
#'   the time periods. The simplest always revisit panel design is to revisit
#'   every sample unit on every time period, specified as pnl_dsgn = c(1,0) or
#'   using McDonald's notation [1-0].
#'
#'   If the last even position is NA, the panels follow a rotating panel
#'   structure. For example, pnl_dsgn = c(1, NA) designates that sample units in
#'   a panel will be visited once and then never again, [1-n] in McDonald's
#'   notation. pnl_dsgn = c(1, 4, 1, NA) designates that sample units in a panel
#'   will be visited once, then not sampled on next four time periods, then
#'   sampled again once at the next time period and then never sampled again,
#'   [1-4-1-n] in McDonald/s notation.
#'
#'   If the last even position is > 0, the panels follow a serially alternating
#'   panel structure. For example, pnl_dsgn = c(1, 4) designates that sample
#'   units in a panel will be visited once, then not sampled during the next
#'   four time periods, then sampled once and not sampled for next four time
#'   periods, and that cycle repeated until end of the number of time periods,
#'   [1-4] in McDonald's notation. pnl_dsgn = c(2, 3, 1, 4) designates that the
#'   cycle has sample units in a panel being visited during two consecutive time
#'   periods and then not sampled on next four time periods, and the cycle is
#'   repeated until end of the number of time periods, [2-3-1-4] in McDonald's
#'   notation.
#'
#'   The number of panels in a single panel design is specified by pnl_n.  For
#'   an always revisit panel structure, a single panel is created and pnl_n is
#'   ignored. For a rotating panel structure, when pnl_n = NA, the number of
#'   panels is equal to n.period. Note that this should only be used when the
#'   rotating panel structure is the only panel design, i.e., no split panel
#'   design (see below for split panel details). If pnl_n = m is specified for a
#'   rotating panel design, then then number of panels will be m.  For example,
#'   pnl_dsgn = c( 1, 4, 1, NA) and and pnl_n = 5 means that only 5 panels will
#'   be constructed and the last time period to be sampled will be time period
#'   10. In McDonald's notation the panel design structure is [(1-4-1-n)^5].  If
#'   the number of time periods, n.period, is 20 and no other panel design
#'   structure is specified, then the last 10 time periods will not be sampled.
#'   For serially alternating panels, when pnl_n = NA, the number of panels will
#'   be the sum of the elements in pan_dsgn (ignoring NA). If pnl_n is specified
#'   as m, then m panels will be created.  For example, pnl_dsgn = c(1, 4, 1, 4)
#'   and pnl_n =3, [(1-4-1-4)^3] in McDonald's notation, will create first three
#'   panels of the 510 serially alternating panels specified by pnl_dsgn.
#'
#'   A serially alternating or rotating panel revisit design may not result in
#'   the same number of units being sampled during each time period,
#'   particularly during the initial start up period.  The default is to not
#'   specify a startup option ("None").  Start up option "Partial_Begin"
#'   initiates the revisit design at the last time period scheduled for sampling
#'   in the first panel. For example, a [2-3-1-4] design starts at time period 6
#'   instead of time period 1 under the Partial_Begin option. For a serially
#'   alternating panel structure, start up option "Partial_End" initiates the
#'   revisit design at the time period that begins the second serially
#'   alternating pattern. For example, a [2-3-1-4] design starts at time period
#'   11 instead of time period 1. For a rotating panel structure design, use of
#'   Partial_End makes the assumption that the number of panels equals the
#'   number of time periods and adds units to the last "m" panels for time
#'   periods 1 to "m" as if number of time periods was extended by "m" where "m"
#'   is one less than then the sum of the panel design.  For example, a
#'   [1-4-1-4-1-n] design would result in m = 10.  Note that some designs with
#'   pnl_n not equal to the number of sample occasions can produce unespected
#'   panel designs.  See examples.
#'
#'   Different types of panel structures can be combined, these are termed split
#'   panels by many authors, by specifying more than one list for the panels
#'   parameter.  The total number of panels is the sum of the number of panels
#'   in each of the panel structures specified by the split panel design.
#'
#' @return  A two-dimensional array of sample sizes to be sampled at each
#'   combination of panel and time period.
#'
#' @references
#' McDonald, T. (2003). "Review of environmental monitoring methods: Survey
#' Designs." Environmental Monitoring and Assessment 85: 277-292.
#'
#' @examples
#'  ## One panel of  60 sample units sampled at every time period: [1-0]
#'  revisit_dsgn(20, panels = list(Annual=list(n = 60, pnl_dsgn = c(1, 0),
#'                                   pnl.n = NA, start_option = "None")), begin=1)
#'
#'  ## rotating panels of 60 units sampled once and never again: [1-n].  Number
#'  ## of panels equal n.period.
#'  revisit_dsgn(20, panels=list(R60N=list(n=60, pnl_dsgn = c(1, NA), pnl_n=NA,
#'                               start_option="None")), begin=1 )
#'
#'  ##serially alternating panel with three visits to sample unit then skip
#'  ## next two time periods: [3-2]
#'  revisit_dsgn(20, panels=list(SA60PE=list(n=20, pnl_dsgn = c(3, 2), pnl_n=NA,
#'                                 start_option="Partial_End")), begin=1 )
#'
#'  ##split panel of sample units combining above two panel designs: [1-0, 1-n]
#'  revisit_dsgn(n.period=20, begin = 2017,
#'         panels = list(Annual=list(n = 60, pnl_dsgn = c(1, 0), pnl.n = NA,
#'                                 start_option = "None"),
#'                       R60N=list(n=60, pnl_dsgn = c(1, NA), pnl_n=NA,
#'                               start_option="None") ))
#'
#' @export
#'
#######################################################
revisit_dsgn <- function(n.period, panels, begin = 1, skip = 1 )  {
  if (!is.list(panels)) {
    stop ('\npanels must be a list of lists')
  }

  # initialize final panel design array
  panels.dsgn <- NULL

  ## cycle through the number of panel structures
  for (nsplit in 1:length(names(panels)) ) {
    tmp.n <- panels[[nsplit]]$n
    tmp.dsgn <- panels[[nsplit]]$pnl_dsgn
    tmp.pnl_n <- panels[[nsplit]]$pnl_n
    tmp.start_option <- panels[[nsplit]]$start_option
    # determine if start up option required. If num.vis > 1 start up options okay
    num.vis <- sum ( tmp.dsgn[seq (1, by = 2, length.out = length(tmp.dsgn) / 2)])

    # check that pnl_dsgn has even number of elements as required
    if (length(tmp.dsgn) %% 2 == 1) {
      stop ('\nVector specifying pnl_dsgn must have even number of elements')
    }

    # check start options
    if (!(tmp.start_option %in% c("None", "Partial_Begin", "Partial_End"))) {
      stop ('\nStart option for a panel design is not one of None, Partial_Begin, Partial_End')
    }

    # check that pnl_dsgn ends in NA, 0 or > 0 as required
    pnl.type <- tmp.dsgn[length(tmp.dsgn)]
    if ( !( pnl.type == 0 | is.na(pnl.type) | pnl.type > 0 )) {
      stop ('\nVector specifying pnl_dsgn must end in NA, 0, or > 0')
    }

    # set up panel core visit, skip schedule that may or may not be repeated
    # v.len is number of occasions in a panel cycle
    v.len <- sum(tmp.dsgn, na.rm = TRUE)
    visit <- rep(tmp.n, tmp.dsgn[1])
    if(length(tmp.dsgn) > 1) {
      for (j in 2:length(tmp.dsgn)) {
        if(j %% 2 == 1) visit <- c(visit, rep(tmp.n, tmp.dsgn[j]))
        if(j %% 2 == 0 & !is.na(tmp.dsgn[j]) ) visit <- c(visit, rep(0, tmp.dsgn[j]))
      }
    }

    ####  Always revisit panels
    if (!is.na (pnl.type) &  pnl.type == 0) {
      pan.dsgn <- array (c(visit, rep (tmp.n, n.period - length (visit))), c(1,n.period))
      # assign dimnames
      dimnames (pan.dsgn) <- list(names(panels)[nsplit], seq(begin, by = skip, length.out=n.period) )
      # combine with other panels if any
      panels.dsgn <- rbind(panels.dsgn, pan.dsgn)
    }

    #### serially alternating panel type designS
    if (!is.na (pnl.type) & pnl.type > 0) {

      # determine number of panels
      n.panels <- sum(tmp.dsgn)
      n.cycle <- sum(tmp.dsgn)

      pan.dsgn <- rep(visit, ceiling (1 + n.period / v.len))
      for (i in 2:n.panels){
        tmp <-  c(rep(0, i-1), rep(visit, ceiling (1 + n.period / v.len)))
        pan.dsgn <- rbind(pan.dsgn, tmp[1:(length(tmp) - i + 1)])
      }

      ## set up start options: None, Partial_Begin, Partial_End

      # start.option = None
      if (tmp.start_option == "None") {
        pan.dsgn <- pan.dsgn[, 1:n.period]
      }

      # set up start.option = Partial_Begin
      if (tmp.start_option == "Partial_Begin") {
        if (num.vis == 1) {
          pan.dsgn <- pan.dsgn[ , 1:n.period]
        }
        if (num.vis > 1) {
          itmp <- sum ( tmp.dsgn[1:(length(tmp.dsgn) - 1)] )
          pan.dsgn <- pan.dsgn[ , itmp:ncol(pan.dsgn)]
          pan.dsgn <- pan.dsgn[ , 1:n.period]
        }
      }

      # set up start.option = Partial_End
      if (tmp.start_option == "Partial_End") {
        if (num.vis == 1) {
          pan.dsgn <- pan.dsgn[ , 1:n.period]
        }
        if (num.vis > 1) {
          itmp <- n.cycle + 1
          pan.dsgn <- pan.dsgn[ , itmp:ncol(pan.dsgn)]
          pan.dsgn <- pan.dsgn[ , 1:n.period]
        }
      }

      # check to see if want to use all panels
      if (!is.na (tmp.pnl_n) ) {
        pan.dsgn <- pan.dsgn[1:tmp.pnl_n, ]
      }

      # assign dimnames
      if (nrow(pan.dsgn) < 10) {
        dimnames (pan.dsgn) <- list(c(paste(names(panels)[nsplit], 1:nrow(pan.dsgn), sep="_") ),
                                    seq(begin, by = skip, length.out=ncol(pan.dsgn)) )
      }
      else {
        dimnames (pan.dsgn) <- list(c(paste(names(panels)[nsplit], 1:9, sep="_0"),
                                      paste(names(panels)[nsplit], 10:nrow(pan.dsgn), sep="_") ),
                                    seq(begin, by = skip, length.out=ncol(pan.dsgn)) )
      }
      # combine with other panels if any
      panels.dsgn <- rbind(panels.dsgn, pan.dsgn)
    }

    #### rotating panel type designs
    if (is.na (pnl.type)) {

      # determine cycle length based on number of sampling occasions
      # from first through last sample visit
      n.cycle <- sum (tmp.dsgn, na.rm = TRUE)
      n.panels <- n.period

      for (i in 1:(n.panels + n.cycle)) {
        if ( i == 1) {
          pan.dsgn <- c(visit, rep(0, n.period + n.cycle))
        }
        else {
          pan.dsgn <- rbind(pan.dsgn, c(rep(0, i-1), visit, rep(0, n.period + n.cycle - (i - 1))) )
        }
      }

      ## set up start options: None, Partial_Begin, Partial_End

      # if num.vis equal to 1 then no start option required
      if ( num.vis == 1) {
        # shorten columns to n.period and keep only panels with at least one sample occasion
        pan.dsgn <- pan.dsgn[, 1:n.period]
        keep <- ifelse (apply (pan.dsgn, 1, sum) > 0, TRUE, FALSE)
        pan.dsgn <- pan.dsgn[keep, ]
      }

      # start.option = None
      if (tmp.start_option == "None") {
        # shorten columns to n.period and keep only panels with at least one sample occasion
        pan.dsgn <- pan.dsgn[, 1:n.period]
        keep <- ifelse (apply (pan.dsgn, 1, sum) > 0, TRUE, FALSE)
        pan.dsgn <- pan.dsgn[keep, ]
      }
      # set up start.option = Partial_Begin
      if (tmp.start_option == "Partial_Begin" & num.vis > 1) {
        itmp <- sum ( tmp.dsgn[1:(length(tmp.dsgn) - 1)] )
        pan.dsgn <- pan.dsgn[ , itmp:ncol(pan.dsgn)]

        # shorten columns to n.period and keep only panels with at least one sample occasion
        pan.dsgn <- pan.dsgn[, 1:n.period]
        keep <- ifelse (apply (pan.dsgn, 1, sum) > 0, TRUE, FALSE)
        pan.dsgn <- pan.dsgn[keep, ]
      }
      # set up start.option = Partial_End:
      if (tmp.start_option == "Partial_End" & num.vis > 1) {
       if(v.len > 1) {
          for (i in 1:(v.len - 1) ) {
            pan.dsgn[n.panels - v.len + 1 + i, 1:i] <- visit[(v.len + 1 - i):v.len]
          }
        }
        # shorten columns to n.period and keep only panels with at least one sample occasion
        pan.dsgn <- pan.dsgn[, 1:n.period]
        keep <- ifelse (apply (pan.dsgn, 1, sum) > 0, TRUE, FALSE)
        pan.dsgn <- pan.dsgn[keep, ]
      }

      # check to see if want to use all panels
      if (!is.na (tmp.pnl_n) ) {
        pan.dsgn <- pan.dsgn[1:tmp.pnl_n, ]
      }

      # assign dimnames
      if (nrow(pan.dsgn) < 10) {
        dimnames (pan.dsgn) <- list(c(paste(names(panels)[nsplit], 1:nrow(pan.dsgn), sep="_") ),
                                    seq(begin, by = skip, length.out=ncol(pan.dsgn)) )
      }
      else {
        dimnames (pan.dsgn) <- list(c(paste(names(panels)[nsplit], 1:9, sep="_0"),
                                      paste(names(panels)[nsplit], 10:nrow(pan.dsgn), sep="_") ),
                                    seq(begin, by = skip, length.out=ncol(pan.dsgn)) )
      }
      # combine with All Revisit and Serially Alternating panel designs if any
      panels.dsgn <- rbind (panels.dsgn, pan.dsgn)
    }
    # end of rotating panel structure
  }

  # return final revisit panel design structure
  class (pan.dsgn) <- "paneldesign"
  return (panels.dsgn)
}
#########################################################################


###############################################################################
# Function: revisit_bibd
# Purpose: Create revisit panel design based on Balanced Incomplete Block
#          Designs (Generalized Youden Designs (GYD))
# Programmer: Tony Olsen
# Date: February, 2017
#
#' Create Balanced Incomplete Block Panel Revisit Design
#'
#' Create a revisit design for panels in a survey that specifies the time
#' periods for the units of each panel to be sampled based on searching for a D-optimal
#' block design that is a member of the class of generalized Youden designs.
#' The resulting design need not be a balanced incomplete block design.  Based on
#' algorithmic idea by Cook and Nachtsheim (1989) and implemented by Robert Wheeler.
#'
#' @param n.period  Number of time periods for the survey design. Typically,
#'   number of periods if sampling occurs once per period or number of months if
#'   sampling occurs once per month. (v, number of varieties/treatments in BIBD terms)
#'
#' @param n.pnl Number of panels (b, number of blocks in BIBD terms)
#'
#' @param n.visit  Number of time periods to be visited in a panel (k, block
#'   size in BIBD terms)
#'
#' @param nsamp Number of samples in each panel.
#'
#' @param panel_name  Prefix for name of each panel
#'
#' @param begin  Numeric name of first sampling occasion, e.g. a specific period.
#'
#' @param skip  Number of sampling occasions to skip between planned sampling
#'   periods, e.g., sampling will occur only every 5 periods if skip = 5.
#'
#' @param iter  Maximum number of iterations in search for D-optimal Generallized
#'   Youden Design.
#'
#' @details  The function uses find.BIB function from crossdes package to
#' search for a D-optimal block design.  crossdes uses package AlgDesign
#' to search balanced incomplete block designs.
#'
#' @return  A two-dimensional array of sample sizes to be sampled for each panel
#'   and each sampling occasion.
#'
#' @references
#'  R.D. Cook and C. Nachtsheim. Computer-aided blocking of factorial and
#'  response-surface designs. Technometrics, 31(3):339-346, 1989.
#'
#' @examples
#'  ## Balanced incomplete block design with 20 sample occasions, 20 panels,
#'  ## 3 visits to each unit, and 20 units in each panel.
#'  revisit_bibd(n.period = 20, n.pnl = 20, n.visit = 3, nsamp = 20)
#'
#' @export
#'
revisit_bibd <- function (n.period, n.pnl, n.visit, nsamp, panel_name = "BIB",
                          begin = 1, skip = 1, iter = 30 ) {

  # check input parameters for meeting requirements
  if ( !(n.period > n.visit & n.pnl >= n.period & n.visit > 1)) {
    stop ("\nInput parameters do not satisfy minimal conditions")
  }

  # use crossdes package functions to generate BIB design
  bib <- find.BIB(n.period, n.pnl, n.visit, iter = iter)
  pan.dsgn <- table (c(rep(1:n.pnl, rep(n.visit, n.pnl))), as.vector(t(bib)) )
  pan.dsgn[pan.dsgn ==1] <- nsamp

  # assign dimnames
  if (nrow(pan.dsgn) < 10) {
    dimnames (pan.dsgn) <- list(c(paste(panel_name, 1:nrow(pan.dsgn), sep="_") ),
                                seq(begin, by = skip, length.out=ncol(pan.dsgn)) )
  }
  else {
    dimnames (pan.dsgn) <- list(c(paste(panel_name, 1:9, sep="_0"),
                                  paste(panel_name, 10:nrow(pan.dsgn), sep="_") ),
                                seq(begin, by = skip, length.out=ncol(pan.dsgn)) )
  }
  class (pan.dsgn) <- "paneldesign"
  return (pan.dsgn)
}
#########################################################################

###############################################################################
# Function: revisit_rand
# Purpose: Create revisit panel design with random assignment of sample units
#          to panels
# Programmer: Tony Olsen
# Date: February, 2017
#
#' Create Revisit Design With Random Assignment to Panels and Time Periods
#'
#' Create a revisit design for a survey that specifies the panels and time
#' periods that will be sampled by random selection of panels and time periods.
#' Three options for random assignments are "period" where the number of time
#' periods to be sampled in a panel is fixed, "panel" where the number panels to
#' be sampled in a time period is fixed, and "none" where the number of
#' panel-period combinations is fixed.
#'
#' @param n.period  Number of time periods for the survey design. Typically,
#'   number of periods if sampling occurs once per period or number of months if
#'   sampling occurs once per month. (v, number of varieties (or treatments) in
#'   BIBD terms)
#'
#' @param n.pnl Number of panels
#'
#' @param rand.control  Character value must be "none", "panel", or "period".
#'   Specifies whether the number of sample events will be fixed for each panel
#'   ("panel"), for each sample occasion ("occasion"), or for total panel-period
#'   combinations ("none").  Default is "panel".
#'
#' @param n.visit If rand_control is "panel", this is the number of panels that
#'   will be sampled in each time period. If rand_control is "period", this is
#'   the number of time periods to be sampled in each panel. If rand_control is
#'   "none", this is the total number of panel-period combinations that will
#'   have units sampled in the revisit design.
#'
#' @param nsamp Number of samples in each panel.
#'
#' @param panel_name  Prefix for name of each panel
#'
#' @param begin  Numeric name of first sampling occasion, e.g. a specific period.
#'
#' @param skip  Number of sampling occasions to skip between planned sampling
#'   periods, e.g., sampling will occur only every 5 periods if skip = 5.
#'
#' @details  The revisit design for a survey is created by
#'  random selection of panels and time periods that will have sample events.  The number of sample
#'   occasions that will be visited by a panel is random.
#'
#' @return  A two-dimensional array of sample sizes to be sampled for each panel
#'   and each time period.
#'
#' @examples
#'  revisit_rand(n.period = 20, n.pnl = 10, rand.control = "none", n.visit = 50, nsamp = 20)
#'  revisit_rand(n.period = 20, n.pnl = 10, rand.control = "panel", n.visit = 5, nsamp = 10)
#'  revisit_rand(n.period = 20, n.pnl = 10, rand.control = "period", n.visit = 5, nsamp = 10)
#'
#' @export
#'
revisit_rand <- function (n.period, n.pnl, rand.control = "period",  n.visit,
                          nsamp, panel_name = "Random", begin = 1, skip = 1) {

  if ( !(rand.control %in% c("none", "panel", "period")) ) {
    stop ("\nRandom control not equal to none, panel or period")
  }

  if (rand.control == "panel" ) {
    pan.dsgn <- matrix(rep (c(rep(nsamp, n.visit), rep (0, n.period - n.visit)), n.pnl),
                       nc=n.period, byrow = TRUE)
    pan.dsgn <- t (apply (pan.dsgn, 1, permute) )
  }

  if (rand.control == "period" ) {
    pan.dsgn <- matrix(rep (c(rep(nsamp, n.visit), rep (0, n.pnl - n.visit)), n.period),
                       nc=n.period)
    pan.dsgn <- apply (pan.dsgn, 2, permute)
  }

  if (rand.control == "none") {
    pan.dsgn <- c( rep(nsamp, n.visit), rep (0, (n.pnl * n.period) - n.visit) )
    pan.dsgn <- permute (pan.dsgn)
    pan.dsgn <- matrix (pan.dsgn, nr = n.pnl)
  }

  # drop panels if no visits
  keep <- ifelse (apply (pan.dsgn, 1, sum) > 0, TRUE, FALSE)
  pan.dsgn <- pan.dsgn[keep, ]

  # assign dimnames
  if (nrow(pan.dsgn) < 10) {
    dimnames (pan.dsgn) <- list(c(paste(panel_name, 1:nrow(pan.dsgn), sep="_") ),
                               seq(begin, by = skip, length.out=ncol(pan.dsgn)) )
  }
  else {
    dimnames (pan.dsgn) <- list(c(paste(panel_name, 1:9, sep="_0"),
                                 paste(panel_name, 10:nrow(pan.dsgn), sep="_") ),
                               seq(begin, by = skip, length.out=ncol(pan.dsgn)) )
  }
  class (pan.dsgn) <- "paneldesign"
  return (pan.dsgn)
}
#########################################################################


#########################################################################
# Function: panel_summary
# Purpose: summarize characteristics of a revisit panel design.
# Programmer: Tony Olsen
# Date: March 2017
#
#' Summary Characteristics of a Revisit Panel Design
#'
#' Revisit panel design characteristics are summarized: number of panels, number
#' of time periods, total number of sample events for the revisit design, total
#' number of sample events for each panel, total number of sample events for
#' each time period and cumulative number of unique units sampled by time
#' periods.
#'
#' @param  paneldsgn Two-dimensional array with dimnames specifying revisit
#'   panel design. Typically, array is output from revisit_dsgn, revisit_bibd or
#'   revisit_rand functions.
#'
#' @param  visitdsgn Two-dimensional array with same dimensions as paneldsgn
#'   specifying the number of times a sample unit is sampled at each time
#'   period. Default is visitdsgn=NULL, where default assumes that a sample unit
#'   will be sampled only once at each time period.
#'
#' @details The revisit panel design and the visit design (if present) are
#'   summarized. Summaries can be useful to know the effort required to complete
#'   the survey design. See the values returned for the summaries that are
#'   produced.
#'
#' @return List of six elements.
#'   \itemize{ \item n.panel - number of panels in revisit design
#'
#'   \item n.period - number of time periods in revisit design
#'
#'   \item n.total - total number of sample events across all panels and all
#'   time periods, accounting for visitdsgn, that will be sampled in the revisit
#'   design
#'
#'   \item n.periodunit - Vector of the number of time periods a unit will be
#'   sampled in each panel
#'
#'   \item n.unitpnl - Vector of the number of sample units, accounting for
#'   visitdsgn, that will be sampled in each panel
#'
#'   \item n.unitperiod - Vector of the number of sample units, accounting for
#'   visitdsgn, that will be sampled during each time period
#'
#'   \item ncum.unit - Vector of the cumulative number of unique units that will
#'   be sampled in time periods up to and including the current time period.
#'   }
#'
#' @examples
#' ## serially alternating panel revisit design summary
#' sa.dsgn <- revisit_dsgn(20, panels=list(SA60N=list(n=60, pnl_dsgn = c(1, 4),
#'                                     pnl_n=NA, start_option="None")), begin=1 )
#' panel_summary(sa.dsgn)
#'
#' ## add visit design where first panel is sampled twice at every time period
#' sa.visit <- sa.dsgn
#' sa.visit [sa.visit > 0] <- 1
#' sa.visit [1, sa.visit[1,] > 0] <- 2
#' panel_summary(sa.dsgn, sa.visit)
#'
#' @export
#'
############################################
panel_summary <- function (paneldsgn, visitdsgn = NULL) {
  n.pan <- dim (paneldsgn)[1]
  n.period <- dim (paneldsgn)[2]

  # determine the cumulative number of unique sample units by sampling occasion
  used <- rep(FALSE, n.pan)
  tot <- vector("numeric", length=n.period)
  for (i in 1:n.period) {
    units <- paneldsgn[,i] > 0
    new <- used + units
    tot[i] <- sum(paneldsgn[new == 1,i])
    used <- new
  }
  n.unique_cum <- cumsum (tot)
  names(n.unique_cum) <- dimnames(paneldsgn)[[2]]

  # summarize number of sample results by panel and by time period
  # incorporate multiple times unit is sampled if sample units for a time period are
  # sampled more than once.
  ifelse (!is.null (visitdsgn), vis <- visitdsgn * paneldsgn, vis <- paneldsgn)
  n.unitpnl <- apply(vis, 1, sum)
  n.unitperiod <- apply(vis, 2, sum)
  n.total <- sum(n.unitperiod)
  # number of times a sample unit is visited in each panel
  tmp <- array(0, c(n.pan, n.period))
  tmp[paneldsgn > 0] <- 1
  ifelse (!is.null (visitdsgn), vis <- visitdsgn * tmp, vis <- tmp)
  n.periodunit <- apply(vis, 1, sum)
  names (n.periodunit) <- dimnames(paneldsgn)[[1]]

  # create list of results
  rslt <- list(n.panel = n.pan,
               n.period = n.period,
               n.total = n.total,
               n.periodunit = n.periodunit,
               n.unitpnl = n.unitpnl,
               n.unitperiod = n.unitperiod,
               ncum.unit = n.unique_cum)
  return (rslt)
}
######################################################################


######################################################################
# Function:   power.dsgn
# Programmer: Tony Olsen
# Date: July, 2016
#
#' Power calculation for multiple panel designs
#'
#' Calculates the power for trend detection for one or more variables, for one
#' or more panel designs, for one or more linear trends, and for one or more
#' signficance levels.  The panel designs create a covarance model where the
#' model includes variance components for units, periods, the interaction of units
#' and periods, and the residual (or index) variance.
#'
#' @param ind.names  Vector of indicator names
#'
#' @param ind.values  Vector of indicator mean values
#'
#' @param unit.var  Vector of variance component estimates for unit variability
#'   for the indicators
#'
#' @param period.var  Vector of variance component estimates for period variability
#'   for the indicators
#'
#' @param unitperiod.var  Vector of variance component estimates for unit by period
#'   interaction variability for the indicators
#'
#' @param index.var  Vector of variance component estimates for index (residual)
#'   error for the indicators
#'
#' @param unit.rho  Correlation across units. Default is 1
#'
#' @param period.rho  Correlation across periods. Default is 0
#'
#' @param paneldsgn  A list of panel designs each as a matrix.  Each element of
#'   the list is a matrix with dimnames (dimensions: number of panels (rows) by
#'   number of periods (columns)) containing the number of units visited for each
#'   combination of panel and period.  Dimnames for columns must be convertable to
#'   an integer (e.g., 2016).  All designs must span the same number of periods.
#'   Typically, the panel designs are the output of the function revisit_dsgn.
#'
#' @param nrepeats  Either NULL or a list of matrices the same length as
#'   paneldsgn specifying the number of revisits made to units in a panel in the
#'   same period for each design.  Specifying NULL indicates that number of
#'   revisits to units is the same for all panels and for all periods and for all
#'   panel designs. The default is NULL, a single visit. Names must match list
#'   names in paneldsgn.
#'
#' @param trend.type  Trend type is either "mean" where trend is applied as
#'   percent trend in the indicator mean or "percent" where the trend is applied
#'   as percent trend in the proportion (percent) of the distribution that is
#'   below or above a fixed value. Default is trend.type="mean"
#'
#' @param ind.pct  When trend.type is equal to "percent", a vector of the
#'   values of the indicator fixed value that defines the percent.  Default is
#'   NULL
#'
#' @param ind.tail  When trend.type is equal to "percent", a character vector
#'   with values of either "lower" or "upper" for each indicator.  "lower"
#'   states that the percent is associated with the lower tail of the
#'   distribution and "upper" states that the percent is associated with the
#'   upper tail of the distribution. Default is NULL.
#'
#' @param trend  Single value or vector of assumed percent change from
#'   initial value in the indicator for each period. Assumes the trend is
#'   expressed as percent per period. Note that the trend may be either positive
#'   or negative. The default is trend=2.
#'
#' @param alpha  Single value or vector of significance level for linear
#'   trend test, alpha, Type I error, level.  The defualt is 0.05.
#'
#' @details Calculates the power for detecting a change in the mean for
#'   different panel design structures. The model incorporates unit, period, unit
#'   by period, and index variance components as well as correlation across units
#'   and across periods.  See references for methods.
#
#' @references
#'     Urquhart, N. S., W. S. Overton, et al. (1993) "Comparing sampling designs for
#'          monitoring ecological status and trends: impact of temporal patterns."
#'          Statistics for the Environment.  V. Barnett and K. F. Turkman.
#'          New York, John Wiley & Sons: 71-86.
#'
#'     Urquhart, N. S. and T. M. Kincaid (1999). "Designs for detecting trends from
#'          repeated surveys of ecological resources." Journal of Agricultural,
#'          Biological, and Environmental Statistics 4(4): 404-414.
#'
#'     Urquhart, N. S. (2012). The role of monitoring design in detecting trend in
#'          long-term ecological monitoring studies. Design and Analysis of Long-term
#'          Ecological Monitoring Studies. R. A. Gitzen, J. J. Millspaugh, A. B. Cooper
#'          and D. S. Licht. New York, Cambridge University Press: 151-173.
#
#' @return  A list with components trend.type, ind.pct, ind.tail, trend values
#'   across periods, periods (all periods included in one or more panel designs),
#'   significance levels, a five-dimensional array of power calculations
#'   (dimensions: panel design names, periods, indicator names, trend names,
#'   alpha.names), an array of indicator mean values for each trend and the
#'   function call.
#'
#' @examples
#' ## power for rotating panel with sample size 60
#' power.dsgn("Variable_Name", ind.values = 43, unit.var = 280, period.var = 4,
#'            unitperiod.var = 40, index.var = 90, unit.rho = 1, period.rho = 0,
#'             paneldsgn = list(NoR60=revisit_dsgn(20,
#'                          panels=list(NoR60=list(n=60, pnl_dsgn = 1, pnl_n=20,
#'                          start_option="None")), begin=1 )),
#'             nrepeats = NULL, trend.type = "mean", trend= 1.0, alpha=0.05 )
#'
#' @export
#'
###################################
power.dsgn <- function(ind.names, ind.values, unit.var, period.var, unitperiod.var,
                       index.var, unit.rho = 1, period.rho = 0,
                       paneldsgn, nrepeats = NULL, trend.type="mean", ind.pct=NULL,
                       ind.tail = NULL, trend = 2 , alpha = 0.05) {

  # number of indicators for power comparison
  n.ind <- length(ind.names)

  # number of trend change values for power comparison
  n.trend <- length (trend)
  trend.names <- paste ("Trend_", round (trend, 1), "%", sep="")

  # check that trend.type has correct possible values
  if (!(trend.type %in% c("mean", "percent"))) stop ("\ntrend.type value not 'mean' or 'percent' ")

  # number of significance levels for power comparison
  n.alpha <- length (alpha)
  alpha.names <- paste ("alpha_", alpha, sep="")

  # number of designs for power comparison
  n.dsgn <- length (paneldsgn)

  # Create nrepeats revisit design if necessary
  if(is.null(nrepeats)) {
    nrepeats <- vector("list", n.dsgn)
    for (i in names (paneldsgn)) {
      nrepeats[[i]] <- paneldsgn[[i]]
      nrepeats[[i]][nrepeats[[i]] > 0] <- 1
    }
  }

  # number of periods for power comparison
  # First set up periods to calculate trend for each panel design based on periods when units are sampled
  period.power <- vector("list", n.dsgn)
  names (period.power) <- names (paneldsgn)
  period.values <- numeric(length=0 )
  for (i in names (paneldsgn)) {
    period.power[[i]] <- ifelse ( apply (paneldsgn[[i]], 2, max) > 0, TRUE, FALSE)
    period.power[[i]] <- as.numeric(dimnames(paneldsgn[[i]])[[2]][period.power[[i]]])
    period.values <- c(period.values, period.power[[i]])
  }
  # find minimum and maximum period from designs that are monitored
  period.min <- min (period.values)
  period.max <- max (period.values)
  periods <- seq(period.min, by = 1, to=period.max)
  n.periods <- length (periods)

  # When trend.type = "percent", check if for lower or upper tail
  if (!is.null(ind.tail)) {
    if (any( !(ind.tail %in% c("lower", "upper")) ) ) stop ("\nValues must be lower or upper")
    lower.tail <- ind.tail == "lower"
  }

  # set up output array for calculated power for each indicator, panel design, periods, trend, and alpha
  pout <- array(NA, c(n.dsgn, n.periods, n.ind, n.trend, n.alpha))
  dimnames (pout) <- list(names (paneldsgn), periods, ind.names, trend.names, alpha.names)

  # set up change in mean for output and use in power calculations
  trend.value <- array(NA, c(n.trend, n.periods, n.ind))
  dimnames (trend.value) <- list(trend.names, as.character(periods), ind.names)

  # set up trend for power calculation based on percent per period and indicator value so
  # that trend is in units indicator units of change per period.

  for (ind in 1:length(ind.names)) {

    for (k in 1:length(trend.names)) {
      # calculate trend value
      if (trend.type == "mean") {
        trend.delta <- ind.values[ind] * trend[k] / 100
        trend.value[k, ,ind] <- ind.values[ind] + trend.delta * seq(0, to=n.periods-1, by=1)
        }
      if (trend.type == "percent") {
        # find cut point value for "pct" which is assumed to have a normal distribution
        ind.sd <- sqrt(unit.var[ind] + residual.var[ind])
        ind.cut <- qnorm(ind.pct[ind]/100, ind.values[ind], ind.sd, lower.tail = lower.tail)
        # function to search for shift in mean required to change the %Good by trend % change
        mean.change <- function(x, ind.pct, ind.cut, ind.sd, lower.tail = lower.tail) {
          abs (ind.pct/100 - pnorm(ind.cut, x, ind.sd, lower.tail = lower.tail) )
        }
        # find change in mean required to achieve change in percent
        for ( i in 1:n.periods) {
          tmp <- optimize(mean.change, lower=ind.values[ind] - 3.1 * ind.sd,
                          upper=ind.values[ind] + 3.1 * ind.sd,
                          ind.pct = ind.pct[ind] + trend[k] * (i - 1),
                          ind.cut = ind.cut, ind.sd= ind.sd,
                          lower.tail = lower.tail)
          trend.value[k, i, ind] <- tmp$minimum
        }
      }

      # power for individual panal designs
      for (j in names(paneldsgn)) {
          nperiod <- ncol(paneldsgn[[j]])
          npanel <- nrow(paneldsgn[[j]])



          # Assume increasing number of periods monitored to compute power
          for (i.period in 1:length(period.power[[j]]) ) {
            if (i.period == 1) {
              # set up initial period variables for x matrix
              period.xmat <- rep(1, npanel)
              if (trend.type == "percent") {
                period.y <- rep (trend.value[k, 1, ind], npanel)
              }
            }
            if (i.period != 1) {
              # create the covariance matrix for jth design and ith end period
              tmp <- matrix (paneldsgn[[j]][, c(as.character(period.power[[j]][1:i.period])) ] , nrow=npanel)
              tmp2 <- matrix (nrepeats[[j]][,  c(as.character(period.power[[j]][1:i.period])) ], nrow=npanel)
              panel.cov <- cov.panel.dsgn (tmp, tmp2,
                                         unit.var = unit.var[ind], period.var = period.var[ind],
                                         unitperiod.var = unitperiod.var[ind], index.var = index.var[ind],
                                         unit.rho = unit.rho, period.rho = period.rho)

              # compute the power at ith monitoring time
              # define linear trend xmat for the panel
              period.diff <- period.power[[j]][i.period] - period.power[[j]][1]
              period.xmat <- c(period.xmat,  rep(period.diff + 1, npanel) )
              if (trend.type == "percent") {
                period.y <- c(period.y, rep (trend.value[k, i.period, ind], npanel) )
              }
              xmat <- cbind(rep(1, npanel * i.period), period.xmat )

              # remove time periods with no sample units
              indx <- as.vector(tmp) > 0
              xmat <- xmat[indx,]
              if (trend.type == "percent" ) {y <- period.y[indx] }

              # calculate inverse of covariance matrix and calculate trend covariance matrix
              phi.inv <- solve(panel.cov$cov[indx,indx])
              bhat.cov <- solve(t(xmat) %*% phi.inv %*% xmat)
              if (trend.type == "percent") {
                bhat <- bhat.cov %*% t(xmat) %*% phi.inv %*% y
              }

              # calculate trend slope standard error and power
              indexse <- sqrt(bhat.cov[2,2])
              ifelse(trend.type == "mean", bhat.std <- trend.delta/ indexse,
                     bhat.std <- bhat[2] / indexse)
              }
            else {
              bhat.std <- 0
            }

            # calculate power for all alpha
            for (m in 1:length(alpha.names)) {
             pout[j, as.character(period.power[[j]][i.period]), ind.names[ind],
                  trend.names[k], alpha.names[m]]  <-
                (pnorm (qnorm (alpha[m] / 2) - bhat.std )) +
                (1 - pnorm ( qnorm(1-(alpha[m] / 2)) - bhat.std ) )
            }
          }
        }
      }
  }

  # Fill in NAs for time poriods when no sampling occurs by repeating power from
  # last time period sampled, that is, has power calculated
  pwr.fill <- function (jnk) {
    keep <- jnk[1]
    for ( i in 2:length (jnk)) {
      ifelse ( is.na (jnk[i]), jnk[i] <- keep, keep <- jnk[i])
    }
    return (jnk)
  }
  for (i in names(paneldsgn)) {
    for (j in 1:length(ind.names)) {
      for (k in 1:length(alpha.names)) {
        pout[i, ,j , ,k] <- apply (pout[i, ,j , , k], 2,  pwr.fill)
      }
    }
  }


  # output list
  dsgn.pwr <- vector("list", 11)
  names(dsgn.pwr) <- c("design", "period", "indicator", "trend", "alpha",
                       "trend.type", "ind.pct", "ind.tail", "trend.change", "dsgn.power", "call")
  dsgn.pwr$design <- names (paneldsgn)
  dsgn.pwr$period <- periods
  dsgn.pwr$indicator <- ind.names
  dsgn.pwr$trend <- trend
  dsgn.pwr$alpha <- alpha
  dsgn.pwr$trend.type <- trend.type
  dsgn.pwr$ind.pct <- ind.pct
  dsgn.pwr$ind.tail <- ind.tail
  dsgn.pwr$trend.change <- trend.value
  dsgn.pwr$dsgn.power <- pout
  dsgn.pwr$call <- sys.call()

  class(dsgn.pwr) <- "powerpaneldesign"

  return (dsgn.pwr)
}
######################################################################


######################################################################
# Function: cov.panel.dsgn
# Purpose: Construct covariance matrix for panel design
# Programmers: Tom Kincaid & Tony Olsen
# Date: July, 2016
#
#'
#' Covariance matrix for panel design
#'
#' Covariance structure accounts for the panel design and the four variance
#' components: unit variation, period variation, unit by period interaction
#' variation and index (or residual) variation. The model incorporates unit,
#' period, unit by period, and index variance components. It also includes a
#' provision for unit correlation and period autocorrelation.
#'
#'
#' @param paneldsgn  A matrix (dimensions: number of panels (rows) by number of
#'   periods (columns)) containing the number of units visited for each
#'   combination of panel and period. Default is matrix(50, 1, 10) which is a
#'   single panel of 50 units visited 10 times, typical time is a period.
#'
#' @param nrepeats   Either NULL or a list of matrices the same length as
#'   paneldsgn specifying the number of revisits made to units in a panel in the
#'   same period for each design.  Specifying NULL indicates that number of
#'   revisits to units is the same for all panels and for all periods and for all
#'   panel designs. The default is NULL, a single visit. Names must match list
#'   names in paneldsgn.
#'
#' @param unit.var   The variance component estimate for unit (the default is
#'   Null)
#'
#' @param period.var   The variance component estimate for period (the default is
#'   Null)
#'
#' @param unitperiod.var The variance component estimate for unit by period
#'   interaction (the default is Null)
#'
#' @param index.var  The variance component estimate for index error (the
#'   def0ult is Null)
#'
#' @param unit.rho   unit correlation across periods (the default is 1)
#'
#' @param period.rho   period autocorrelation (the default is 0)
#'
#' @details Covariance structure accounts for the panel design and the four
#'   variance components: unit variation, period variation, unit by period
#'   interaction variation and index (or residual) variation. Uses the model
#'   structure defined by Urquhart.
#'
#'   If nrepeats is NULL, then no units sampled more than once in a specific
#'   panel, period combination) aned then unit by period and index variances are
#'   added together or user may have only estimated unit, period and unit by period
#'   variance components so that index component is zero It calculates the
#'   covariance matrix for the simple linear regression. The standard error for
#'   a linear trend coeficient is the square root of the variance.
#'
#' @references
#'     Urquhart, N. S., W. S. Overton, et al. (1993) "Comparing sampling designs for
#'          monitoring ecological status and trends: impact of temporal patterns."
#'          Statistics for the Environment.  V. Barnett and K. F. Turkman.
#'          New York, John Wiley & Sons: 71-86.
#'
#'     Urquhart, N. S. and T. M. Kincaid (1999). "Designs for detecting trends from
#'          repeated surveys of ecological resources." Journal of Agricultural,
#'          Biological, and Environmental Statistics 4(4): 404-414.
#'
#'     Urquhart, N. S. (2012). The role of monitoring design in detecting trend in
#'          long-term ecological monitoring studies. Design and Analysis of Long-term
#'          Ecological Monitoring Studies. R. A. Gitzen, J. J. Millspaugh, A. B. Cooper
#'          and D. S. Licht. New York, Cambridge University Press: 151-173.
#
#' @return A list containing the covariance matrix (cov) for the panel design,
#'   the input paneldsgn design (paneldsgn.dsgn), the input nrepeats design
#'   (nrepeats.dsgn) and the function call.
#'
#' @export
#'
######################################
cov.panel.dsgn <- function(paneldsgn = matrix(50,1,10), nrepeats = 1,
                           unit.var = NULL, period.var = NULL, unitperiod.var = NULL,
                           index.var = NULL, unit.rho = 1, period.rho = 0) {

  paneldsgn <- as.matrix (paneldsgn)
  nperiod <- ncol (paneldsgn)
  npanel <- nrow (paneldsgn)

  if (any(is.null(unit.var), is.null(period.var), is.null(unitperiod.var), is.null(index.var)) ) {
    stop("Must provide four variance components. index.var may be zero if nrepeats=1")
  }

  # Create covariance matrix for period correlation
  rhomat <- matrix(0, nrow = nperiod, ncol = nperiod)
  if (nperiod > 1) {
    rhomat[1, ] <- 0:(nperiod-1)
    for(i in 2:nperiod) {
      rhomat[i, ] <- c(rev (1:(i-1)), 0:(nperiod-i))
    }
  }

  period.cov <- period.var * (period.rho ^ rhomat)

  # Create covariance matrix for unit correlation
  unit.cov <- unit.var * (unit.rho ^ rhomat)

  # Construct panel covariance term
  # nunits is number of units in a panel
  nunits <- apply (paneldsgn, 1, max)
  if(npanel > 1) pan.cov <- kronecker (unit.cov, diag (ifelse (nunits >0, 1 / nunits, 0)))
  else pan.cov <- unit.cov / nunits

  # construct period covariance term
  yr.cov <- kronecker(period.cov, matrix (1, nrow = npanel, ncol = npanel))

  # Construct unit by period covariance term
  if (length (nunits) > 1)
    sy.cov <- unitperiod.var * kronecker (diag(nperiod), diag(ifelse(nunits > 0, 1/nunits, 0)))
  else sy.cov <- unitperiod.var * diag(nperiod) / nunits

  # Construct index covariance term
  # Create nrepeats revisit design if necessary
  if(is.null(nrepeats)) {
    nrepeats <- vector("list", n.dsgn)
    for (i in names (paneldsgn)) {
      nrepeats[[i]] <- paneldsgn[[i]]
      nrepeats[[i]][nrepeats[[i]] > 0] <- 1
    }
  }
  if(npanel > 1) {
    index.cov <- index.var * kronecker(diag (nperiod),
                                       diag (ifelse(nunits > 0, 1/(nunits * apply (nrepeats, 1, max)), 0)) )
  }
  else index.cov <- index.var * diag (nperiod) / nunits

  # overall covariance matrix
  phi <- pan.cov + yr.cov + sy.cov + index.cov

  cov <- list(cov = phi, paneldsgn = paneldsgn, nrepeat.dsgn = nrepeats, call = sys.call() )
  return(cov)
}
######################################################################




