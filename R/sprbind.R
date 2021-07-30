#' Combine rows from a GRTS or IRS design
#'
#' @param object A design object from \code{grts()} or \code{irs()}
#'
#' @param siteuse A character vector of site types to return. Can contain
#' \code{"Legacy"} (for legacy sites), \code{"Base"} (for base sites),
#' \code{"Over"} (for \code{n_over} replacement sites), and \code{"Near"}
#' (for \code{n_near} replacement sites). The default is \code{NULL}, which
#' returns all non-\code{NULL} output from \code{object$sites_legacy},
#' \code{object$sites_base}, \code{object$sites_over}, and \code{object$sites_near}.
#'
#' @return A single \code{sf} object containing rows for the requested sites.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' grts1 <- grts(NE_Lakes, 50, n_over = 10)
#' sprbind(grts1)
#' }
sprbind <- function(object, siteuse = NULL) {
  if (!inherits(object, "spdesign")) {
    stop("object must be output from grts() or irs()")
  } else {
    if (is.null(siteuse)) {
      siteuse <- c("Legacy", "Base", "Over", "Near")
    }

    # legacy
    if ("Legacy" %in% siteuse) {
      new_legacy <- object$sites_legacy
    } else {
      new_legacy <- NULL
    }

    if ("Base" %in% siteuse) {
      new_base <- object$sites_base
    } else {
      new_base <- NULL
    }

    if ("Over" %in% siteuse) {
      new_over <- object$sites_over
    } else {
      new_over <- NULL
    }

    if ("Near" %in% siteuse) {
      new_near <- object$sites_near
    } else {
      new_near <- NULL
    }

    new_non_legacy <- rbind(new_base, new_over, new_near)

    if (is.null(new_non_legacy) & is.null(new_legacy)) {
      stop("siteuse cannot be empty")
    }

    if (is.null(new_legacy)) {
      return(new_non_legacy)
    } else {
      if (is.null(new_non_legacy)) {
        return(new_legacy)
      } else {
        new_legacy[setdiff(names(new_non_legacy), names(new_legacy))] <- NA
        new_non_legacy[setdiff(names(new_legacy), names(new_non_legacy))] <- NA
        return(rbind(new_legacy, new_non_legacy))
      }
    }
  }
}
