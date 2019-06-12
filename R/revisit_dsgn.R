################################################################################
# Function: revisit_dsgn
# Programmer: Tony Olsen
# Date: March 14, 2019
#'
#' Create a Panel Revisit Design
#'
#' Create a revisit design for panels in a survey that specifies the time
#' periods that members of each panel will be sampled. Three basic panel design
#' structures may be created: always revisit panel, serially alternating panels,
#' or rotating panels.
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
#' @param begin  Numeric name of first sampling occasion, e.g. a specific
#'   period.
#'
#' @param skip  Number of time periods to skip between planned sampling
#'   periods, e.g., sampling will occur only every 5 periods if skip = 5.
#'
#' @details The function creates revisit designs using the concepts in McDonald
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
#' @return A two-dimensional array of sample sizes to be sampled at each
#'   combination of panel and time period.
#'
#' @references
#'   McDonald, T. (2003). Review of environmental monitoring methods: survey
#'   designs. \emph{Environmental Monitoring and Assessment} \bold{85},
#'   277-292.
#'
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @seealso 
#'   \describe{
#'     \item{\code{\link{revisit_bibd}}}{create a balanced incomplete block
#'       panel revisit design}
#'     \item{\code{\link{revisit_rand}}}{create a revisit design with random
#'       assignment to panels and time periods}
#'     \item{\code{\link{panel_summary}}}{summarize characteristics of a revisit
#'       panel design}
#'     \item{\code{\link{power.dsgn}}}{power calculation for multiple panel
#'       designs}
#'     \item{\code{\link{cov.panel.dsgn}}}{covariance matrix for a panel design}
#'     \item{\code{\link{plot_powerpaneldesign}}}{plot power curves for panel
#'       designs}
#'   }
#'
#' @keywords survey
#'
#' @examples
#' # One panel of  60 sample units sampled at every time period: [1-0]
#' revisit_dsgn(20, panels = list(
#'   Annual=list(n = 60, pnl_dsgn = c(1, 0), pnl.n = NA,
#'               start_option = "None")), begin=1)
#'
#' # Rotating panels of 60 units sampled once and never again: [1-n].  Number
#' # of panels equal n.period.
#' revisit_dsgn(20, panels=list(
#'   R60N=list(n=60, pnl_dsgn = c(1, NA), pnl_n=NA, start_option="None")),
#'             begin=1 )
#'
#' # Serially alternating panel with three visits to sample unit then skip
#' # next two time periods: [3-2]
#' revisit_dsgn(20, panels=list(
#'   SA60PE=list(n=20, pnl_dsgn = c(3, 2), pnl_n=NA,
#'              start_option="Partial_End")), begin=1 )
#'
#' # Split panel of sample units combining above two panel designs: [1-0, 1-n]
#' revisit_dsgn(n.period=20, begin=2017,  panels = list(
#'   Annual=list(n = 60, pnl_dsgn = c(1, 0), pnl.n = NA,
#'               start_option = "None"),
#'   R60N=list(n=60, pnl_dsgn = c(1, NA), pnl_n=NA, start_option="None") ))
#'
#' @export
################################################################################

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
