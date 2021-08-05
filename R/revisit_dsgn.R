###############################################################################
# Function: revisit_dsgn (exported)
# Programmer: Tony Olsen
# Date: March 14, 2019
#'
#' Create a panel revisit design
#'
#' Create a revisit design for panels in a survey that specifies the time
#' periods that members of each panel will be sampled. Three basic panel design
#' structures may be created: always revisit panel, serially alternating panels,
#' or rotating panels.
#'
#' @param n_period  Number of time periods for the panel design. For example,
#'   number of periods if sampling occurs once per period or number of months if
#'   sampling occurs once per month.
#'
#' @param panels List of lists where each list specifies a revisit panel
#'   structure. Each sublist consists of four components: \code{n} - sample size for
#'   each panel in the sublist, \code{pnl_dsgn} - a vector with an even number of
#'   elements specifying the panel revisit schedule in terms of the number of
#'   consecutive time periods sample units will be sampled, followed by number
#'   of consecutive time periods skipped, and then repeated as necessary. \code{pnl_n}
#'   - number of panels in the sublist, and \code{start_option} - option for starting
#'   the \code{pnl_dsgn} (\code{None}, \code{Partial_Begin}, or \code{Partial_End}). Three basic panel
#'   structures are possible: a) if \code{pnl_dsgn} ends in \code{0}, then the sample units
#'   are visited on all subsequent time periods, b) if \code{pnl_dsgn} ends in \code{NA}, then
#'   panel follows a rotating panel structure, and c) if \code{pnl_dsgn} ends in any
#'   number > \code{0}, then panel follows a serially alternating panel structure. See
#'   details for further information.
#'
#' @param begin  Numeric name of first sampling occasion, e.g. a specific
#'   period.
#'
#' @param skip  Number of time periods to skip between planned sampling
#'   periods, e.g., sampling will occur only every 5 periods if \code{skip = 5}.
#'
#' @details The function creates revisit designs using the concepts in McDonald
#'   (2003) to specify the revisit pattern across time periods for each panel.
#'   The panel revisit schedule is specificed by a vector. Odd positions in
#'   vector specify the number of consecutive time periods when panel units are
#'   sampled. Even positions in vector specify the number of consecutive time
#'   periods when panel units are not sampled.
#'
#'   If last even position is a \code{"0"}, then a single panel follows an always
#'   revisit panel structure.  After satisfying the initial revisit schedule
#'   specified prior to the \code{"0"}, units in a panel are always visited for rest of
#'   the time periods. The simplest always revisit panel design is to revisit
#'   every sample unit on every time period, specified as \code{pnl_dsgn = c(1,0)} or
#'   using McDonald's notation [1-0].
#'
#'   If the last even position is \code{NA}, the panels follow a rotating panel
#'   structure. For example, \code{pnl_dsgn = c(1, NA)} designates that sample units in
#'   a panel will be visited once and then never again, [1-n] in McDonald's
#'   notation. pnl_dsgn = \code{c(1, 4, 1, NA)} designates that sample units in a panel
#'   will be visited once, then not sampled on next four time periods, then
#'   sampled again once at the next time period and then never sampled again,
#'   [1-4-1-n] in McDonald/s notation.
#'
#'   If the last even position is > \code{0}, the panels follow a serially alternating
#'   panel structure. For example, \code{pnl_dsgn = c(1, 4)} designates that sample
#'   units in a panel will be visited once, then not sampled during the next
#'   four time periods, then sampled once and not sampled for next four time
#'   periods, and that cycle repeated until end of the number of time periods,
#'   [1-4] in McDonald's notation. \code{pnl_dsgn = c(2, 3, 1, 4)} designates that the
#'   cycle has sample units in a panel being visited during two consecutive time
#'   periods and then not sampled on next four time periods, and the cycle is
#'   repeated until end of the number of time periods, [2-3-1-4] in McDonald's
#'   notation.
#'
#'   The number of panels in a single panel design is specified by \code{pnl_n}.  For
#'   an always revisit panel structure, a single panel is created and \code{pnl_n} is
#'   ignored. For a rotating panel structure, when \code{pnl_n = NA}, the number of
#'   panels is equal to n_period. Note that this should only be used when the
#'   rotating panel structure is the only panel design, i.e., no split panel
#'   design (see below for split panel details). If \code{pnl_n = m} is specified for a
#'   rotating panel design, then then number of panels will be \code{m}.  For example,
#'   \code{pnl_dsgn = c( 1, 4, 1, NA)} and and \code{pnl_n = 5} means that only 5 panels will
#'   be constructed and the last time period to be sampled will be time period
#'   10. In McDonald's notation the panel design structure is [(1-4-1-n)^5].  If
#'   the number of time periods, \code{n_period}, is 20 and no other panel design
#'   structure is specified, then the last 10 time periods will not be sampled.
#'   For serially alternating panels, when \code{pnl_n = NA}, the number of panels will
#'   be the sum of the elements in pan_dsgn (ignoring \code{NA}). If \code{pnl_n} is specified
#'   as \code{m}, then \code{m} panels will be created.  For example, \code{pnl_dsgn = c(1, 4, 1, 4)}
#'   and \code{pnl_n = 3}, [(1-4-1-4)^3] in McDonald's notation, will create first three
#'   panels of the 510 serially alternating panels specified by \code{pnl_dsgn}.
#'
#'   A serially alternating or rotating panel revisit design may not result in
#'   the same number of units being sampled during each time period,
#'   particularly during the initial start up period.  The default is to not
#'   specify a startup option (\code{"None"}).  Start up option \code{"Partial_Begin"}
#'   initiates the revisit design at the last time period scheduled for sampling
#'   in the first panel. For example, a [2-3-1-4] design starts at time period 6
#'   instead of time period 1 under the Partial_Begin option. For a serially
#'   alternating panel structure, start up option \code{"Partial_End"} initiates the
#'   revisit design at the time period that begins the second serially
#'   alternating pattern. For example, a [2-3-1-4] design starts at time period
#'   11 instead of time period 1. For a rotating panel structure design, use of
#'   Partial_End makes the assumption that the number of panels equals the
#'   number of time periods and adds units to the last "m" panels for time
#'   periods \code{1} to \code{"m"} as if number of time periods was extended by \code{"m"} where \code{"m"}
#'   is one less than then the sum of the panel design.  For example, a
#'   [1-4-1-4-1-n] design would result in \code{m = 10}.  Note that some designs with
#'   \code{pnl_n} not equal to the number of sample occasions can produce unespected
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
#' @author Tony Olsen \email{Olsen.Tony@@epa.gov}
#'
#' @seealso
#'   \describe{
#'     \item{\code{\link{revisit_bibd}}}{to create a balanced incomplete block
#'       panel revisit design}
#'     \item{\code{\link{revisit_rand}}}{to create a revisit design with random
#'       assignment to panels and time periods}
#'     \item{\code{\link{summary.paneldesign}}}{ to summarize characteristics of a
#'       panel revisit design}
#'   }
#'
#' @keywords survey
#'
#' @examples
#' # One panel of  60 sample units sampled at every time period: [1-0]
#' revisit_dsgn(20, panels = list(
#'   Annual = list(
#'     n = 60, pnl_dsgn = c(1, 0), pnl.n = NA,
#'     start_option = "None"
#'   )
#' ), begin = 1)
#'
#' # Rotating panels of 60 units sampled once and never again: [1-n].  Number
#' # of panels equal n_period.
#' revisit_dsgn(20,
#'   panels = list(
#'     R60N = list(n = 60, pnl_dsgn = c(1, NA), pnl_n = NA, start_option = "None")
#'   ),
#'   begin = 1
#' )
#'
#' # Serially alternating panel with three visits to sample unit then skip
#' # next two time periods: [3-2]
#' revisit_dsgn(20, panels = list(
#'   SA60PE = list(
#'     n = 20, pnl_dsgn = c(3, 2), pnl_n = NA,
#'     start_option = "Partial_End"
#'   )
#' ), begin = 1)
#'
#' # Split panel of sample units combining above two panel designs: [1-0, 1-n]
#' revisit_dsgn(n_period = 20, begin = 2017, panels = list(
#'   Annual = list(
#'     n = 60, pnl_dsgn = c(1, 0), pnl.n = NA,
#'     start_option = "None"
#'   ),
#'   R60N = list(n = 60, pnl_dsgn = c(1, NA), pnl_n = NA, start_option = "None")
#' ))
#' @export
###############################################################################

revisit_dsgn <- function(n_period, panels, begin = 1, skip = 1) {
  if (!is.list(panels)) {
    stop("\npanels must be a list of lists")
  }

  # initialize final panel design array
  panels_dsgn <- NULL

  ## cycle through the number of panel structures
  for (nsplit in 1:length(names(panels))) {
    tmp_n <- panels[[nsplit]]$n
    tmp_dsgn <- panels[[nsplit]]$pnl_dsgn
    tmp_pnl_n <- panels[[nsplit]]$pnl_n
    tmp_start_option <- panels[[nsplit]]$start_option
    # determine if start up option required. If num_vis > 1 start up options okay
    num_vis <- sum(tmp_dsgn[seq(1, by = 2, length.out = length(tmp_dsgn) / 2)])

    # check that pnl_dsgn has even number of elements as required
    if (length(tmp_dsgn) %% 2 == 1) {
      stop("\nVector specifying pnl_dsgn must have even number of elements")
    }

    # check start options
    if (!(tmp_start_option %in% c("None", "Partial_Begin", "Partial_End"))) {
      stop("\nStart option for a panel design is not one of None, Partial_Begin, Partial_End")
    }

    # check that pnl_dsgn ends in NA, 0 or > 0 as required
    pnl_type <- tmp_dsgn[length(tmp_dsgn)]
    if (!(pnl_type == 0 | is.na(pnl_type) | pnl_type > 0)) {
      stop("\nVector specifying pnl_dsgn must end in NA, 0, or > 0")
    }

    # set up panel core visit, skip schedule that may or may not be repeated
    # v_len is number of occasions in a panel cycle
    v_len <- sum(tmp_dsgn, na.rm = TRUE)
    visit <- rep(tmp_n, tmp_dsgn[1])
    if (length(tmp_dsgn) > 1) {
      for (j in 2:length(tmp_dsgn)) {
        if (j %% 2 == 1) visit <- c(visit, rep(tmp_n, tmp_dsgn[j]))
        if (j %% 2 == 0 & !is.na(tmp_dsgn[j])) visit <- c(visit, rep(0, tmp_dsgn[j]))
      }
    }

    ####  Always revisit panels
    if (!is.na(pnl_type) & pnl_type == 0) {
      pan_dsgn <- array(c(visit, rep(tmp_n, n_period - length(visit))), c(1, n_period))
      # assign dimnames
      dimnames(pan_dsgn) <- list(names(panels)[nsplit], seq(begin, by = skip, length.out = n_period))
      # combine with other panels if any
      panels_dsgn <- rbind(panels_dsgn, pan_dsgn)
    }

    #### serially alternating panel type designS
    if (!is.na(pnl_type) & pnl_type > 0) {

      # determine number of panels
      n_panels <- sum(tmp_dsgn)
      n_cycle <- sum(tmp_dsgn)

      pan_dsgn <- rep(visit, ceiling(1 + n_period / v_len))
      for (i in 2:n_panels) {
        tmp <- c(rep(0, i - 1), rep(visit, ceiling(1 + n_period / v_len)))
        pan_dsgn <- rbind(pan_dsgn, tmp[1:(length(tmp) - i + 1)])
      }

      ## set up start options: None, Partial_Begin, Partial_End

      # start.option = None
      if (tmp_start_option == "None") {
        pan_dsgn <- pan_dsgn[, 1:n_period]
      }

      # set up start.option = Partial_Begin
      if (tmp_start_option == "Partial_Begin") {
        if (num_vis == 1) {
          pan_dsgn <- pan_dsgn[, 1:n_period]
        }
        if (num_vis > 1) {
          itmp <- sum(tmp_dsgn[1:(length(tmp_dsgn) - 1)])
          pan_dsgn <- pan_dsgn[, itmp:ncol(pan_dsgn)]
          pan_dsgn <- pan_dsgn[, 1:n_period]
        }
      }

      # set up start.option = Partial_End
      if (tmp_start_option == "Partial_End") {
        if (num_vis == 1) {
          pan_dsgn <- pan_dsgn[, 1:n_period]
        }
        if (num_vis > 1) {
          itmp <- n_cycle + 1
          pan_dsgn <- pan_dsgn[, itmp:ncol(pan_dsgn)]
          pan_dsgn <- pan_dsgn[, 1:n_period]
        }
      }

      # check to see if want to use all panels
      if (!is.na(tmp_pnl_n)) {
        pan_dsgn <- pan_dsgn[1:tmp_pnl_n, ]
      }

      # assign dimnames
      if (nrow(pan_dsgn) < 10) {
        dimnames(pan_dsgn) <- list(
          c(paste(names(panels)[nsplit], 1:nrow(pan_dsgn), sep = "_")),
          seq(begin, by = skip, length.out = ncol(pan_dsgn))
        )
      } else {
        dimnames(pan_dsgn) <- list(
          c(
            paste(names(panels)[nsplit], 1:9, sep = "_0"),
            paste(names(panels)[nsplit], 10:nrow(pan_dsgn), sep = "_")
          ),
          seq(begin, by = skip, length.out = ncol(pan_dsgn))
        )
      }
      # combine with other panels if any
      panels_dsgn <- rbind(panels_dsgn, pan_dsgn)
    }

    #### rotating panel type designs
    if (is.na(pnl_type)) {

      # determine cycle length based on number of sampling occasions
      # from first through last sample visit
      n_cycle <- sum(tmp_dsgn, na.rm = TRUE)
      n_panels <- n_period

      for (i in 1:(n_panels + n_cycle)) {
        if (i == 1) {
          pan_dsgn <- c(visit, rep(0, n_period + n_cycle))
        } else {
          pan_dsgn <- rbind(pan_dsgn, c(rep(0, i - 1), visit, rep(0, n_period + n_cycle - (i - 1))))
        }
      }

      ## set up start options: None, Partial_Begin, Partial_End

      # if num_vis equal to 1 then no start option required
      if (num_vis == 1) {
        # shorten columns to n_period and keep only panels with at least one sample occasion
        pan_dsgn <- pan_dsgn[, 1:n_period]
        keep <- ifelse(apply(pan_dsgn, 1, sum) > 0, TRUE, FALSE)
        pan_dsgn <- pan_dsgn[keep, ]
      }

      # start.option = None
      if (tmp_start_option == "None") {
        # shorten columns to n_period and keep only panels with at least one sample occasion
        pan_dsgn <- pan_dsgn[, 1:n_period]
        keep <- ifelse(apply(pan_dsgn, 1, sum) > 0, TRUE, FALSE)
        pan_dsgn <- pan_dsgn[keep, ]
      }
      # set up start.option = Partial_Begin
      if (tmp_start_option == "Partial_Begin" & num_vis > 1) {
        itmp <- sum(tmp_dsgn[1:(length(tmp_dsgn) - 1)])
        pan_dsgn <- pan_dsgn[, itmp:ncol(pan_dsgn)]

        # shorten columns to n_period and keep only panels with at least one sample occasion
        pan_dsgn <- pan_dsgn[, 1:n_period]
        keep <- ifelse(apply(pan_dsgn, 1, sum) > 0, TRUE, FALSE)
        pan_dsgn <- pan_dsgn[keep, ]
      }
      # set up start.option = Partial_End:
      if (tmp_start_option == "Partial_End" & num_vis > 1) {
        if (v_len > 1) {
          for (i in 1:(v_len - 1)) {
            pan_dsgn[n_panels - v_len + 1 + i, 1:i] <- visit[(v_len + 1 - i):v_len]
          }
        }
        # shorten columns to n_period and keep only panels with at least one sample occasion
        pan_dsgn <- pan_dsgn[, 1:n_period]
        keep <- ifelse(apply(pan_dsgn, 1, sum) > 0, TRUE, FALSE)
        pan_dsgn <- pan_dsgn[keep, ]
      }

      # check to see if want to use all panels
      if (!is.na(tmp_pnl_n)) {
        pan_dsgn <- pan_dsgn[1:tmp_pnl_n, ]
      }

      # assign dimnames
      if (nrow(pan_dsgn) < 10) {
        dimnames(pan_dsgn) <- list(
          c(paste(names(panels)[nsplit], 1:nrow(pan_dsgn), sep = "_")),
          seq(begin, by = skip, length.out = ncol(pan_dsgn))
        )
      } else {
        dimnames(pan_dsgn) <- list(
          c(
            paste(names(panels)[nsplit], 1:9, sep = "_0"),
            paste(names(panels)[nsplit], 10:nrow(pan_dsgn), sep = "_")
          ),
          seq(begin, by = skip, length.out = ncol(pan_dsgn))
        )
      }
      # combine with All Revisit and Serially Alternating panel designs if any
      panels_dsgn <- rbind(panels_dsgn, pan_dsgn)
    }
    # end of rotating panel structure
  }

  # return final revisit panel design structure
  class(pan_dsgn) <- "paneldesign"
  return(pan_dsgn)
}
