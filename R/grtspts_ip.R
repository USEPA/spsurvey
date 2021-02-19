###############################################################################
# Function: grtspts_ip (exported)
# Programmers: Tony Olsen, Tom Kincaid
# Date: January 22, 2021
#'
#' Calculate inclusion probabities for point sample frame for equal, unequal based
#' on categories or proportional to a variable. The inclusion probabilities are
#' calculated assuming a single stratum.
#'
#' @param type Character string with values of \code{"equal"}, \code{"unequal"} or \code{"proportional"}. Default
#'   is \code{"equal"}.
#'
#' @param n_base Sample size for the stratum. If type is \code{"equal"} or \code{"proportional"}, then \code{n_base}
#'   is numeric value for the total sample size for the stratum. If type is \code{"unequal"},
#'   then \code{n_base} is a named vector where names are the unequal probability categories with the
#'   expected sample size for each category. Names in \code{n_base} must be a subset of the values in \code{caty}.
#'   The sum of the category sample sizes is the total sample size for the stratum.
#'
#' @param Nstratum Number of elements in the stratum. Each element will be assigned an
#'   inclusion probabibility. Only required when type is \code{"equal"}. Default is \code{NULL}.
#'
#' @param caty If type is \code{"unequal"}, a character variable that identifies the category
#'   for each element of the sample frame that may be used for an unequal probability
#'   design. All categories in \code{n_base} must be present in \code{pt_caty} but additional categories
#'   are allowed but will not be included in the survey design for the statum. Default is \code{NULL}.
#'
#' @param aux If type is proportional, a numeric vector that will be used to select
#'   points proportional to the variable. Default is NULL.
#'
#' @param warn_ind  A logical value where TRUE indicates a warning message.
#'   Used for internal collection of messages only.
#'
#' @param warn_df A data frame containing messages warning of potential issues.
#'   Used for internal collection of messages only.
#'
#' @return A numeric vector of inclusion probabilities with all values being greater
#'   than 0 and less than or equal 1. Values equal to 1 will be selected with certainty.
#'   Values equal to 0 will not be selected.
#'
#' @author Tony Olsen \email{olsen.tony@@epa.gov}
#'
#' @keywords survey, inclusion probability
#'
#' @examples
#' tmp <- grtspts_ip(type = "equal", n_base = 10, Nstratum = 25)
#' tmp
#' tmp <- grtspts_ip(type = "unequal", n_base = c(small = 5, large = 5),
#'           caty = c(rep("small", 15), rep("large", 10)))
#' tmp
#' tmp <- grtspts_ip(type = "proportional", n_base = 10,
#'                   aux = c(rnorm(20, 3), -2, 30, 40, 0, 0))
#' tmp
#'
#' @export
###############################################################################

grtspts_ip <- function(type = "equal", n_base, Nstratum = NULL, caty = NULL,
                       aux = NULL, warn_ind = NULL, warn_df = NULL) {

  # equal inclusion probabilities
  if(type == "equal") {
    ip <- rep(sum(n_base, na.rm = TRUE)/Nstratum, Nstratum)
  }
  # unequal inclusion probabilities
  if(type == "unequal") {
    gsum <- table(caty)
    catmatch <- match(names(n_base),names(gsum),nomatch=0)
    piden <- n_base/gsum[catmatch]
    ip <- rep(NA,length(caty))
    for(i in names(n_base))
      ip[caty == i] <- piden[i]
  }
  # proportional inclusion probabilities
  if(type == "proportional") {
    ip <- aux
    # check for "0" and negative values. if negative set to "0".
    nnull <- sum(aux == 0)
    nneg = sum(aux < 0)
    if (nnull > 0) {
      warn <-  "Proportional vector has zero values and their inclusion probabilities are set to 0."
      if(warn_ind){
        warn_df <- rbind(warn_df, data.frame(stratum = NA, func = I("grtspts_ip"),
                                             warning = warn))
      } else {
        warn_df <- data.frame(stratum = NA, func = I("grtspts_ip"), warning = warn)
        warn_ind <- TRUE
      }
    }

    if (nneg > 0) {
      ip[ip < 0] <- 0
      warn <- paste0("Proportional vector has ", nneg, " negative value(s) and
              their inclusion probabilities are set to 0.")
      if(warn_ind){
        warn_df <- data.frame(stratum = NA, func = I("grtspts_ip"), warning = warn)
      } else {
        warn_df <- rbind(warn_df, data.frame(stratum = NA, func = I("grtspts_ip"),
                                             warning = warn))
        warn_ind <- TRUE
      }
    }
    # initialize inclusion probabilities
    ip <- n_base * ip / sum(ip)

    # identify and subset elements that have ip > 0
    element_gt0 <- ip > 0
    ip_gt0 <- ip[element_gt0]
    # identify elements greater than or equal to 1 and count them.
    element_gt1 <- ip_gt0 >= 1
    ngt1 <- sum(element_gt1)
    # if ngt1 is greater than 0, then set them to 1 and adjust remaining elements
    # to have sample size n - ngt1. Adjustment may cause other elements to become
    # greater than 1 so repeat until all are less than or equal to 1.
    if(ngt1 > 0) {
      tst <- 0
      while(ngt1 != tst) {
        tmp <- ip_gt0[!element_gt1]
        ip_gt0[!element_gt1] <- (n_base - ngt1) * tmp/sum(tmp)
        ip_gt0[element_gt1] <- 1
        tst <- ngt1
        element_gt1 <- ip_gt0 >= 1
        ngt1 <- sum(element_gt1)
      }
      # replace non zero inclusion probabilities with new ones
      ip[element_gt0] <- ip_gt0
    }
    ip
  }

  # return list with vector of inclusion probabilities and warning indicator and messages
  ip <- list(ip = ip, warn_ind = warn_ind, warn_df = warn_df)

  invisible (ip)
}


