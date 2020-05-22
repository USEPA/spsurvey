###################################################################################
# Function: grtspts_ip
# Programmers: Tony Olsen, Tom Kincaid
# Date: April 23, 2020
#'
#' Calculate inclusion probabities for point sample frame for equal, unequal based
#' on categories or proportional to a variable. The inclusion probabilities are
#' calculated assuming a single stratum.
#'
#' @param type Character string with values of equal, unequal or proportional. Default
#'   is equal.
#'
#' @param nsamp Sample size for the stratum. If type is equal or proportional, then nsamp
#'   is numeric value for the total sample size for the stratum. If type is unequal,
#'   then nsamp is a named vector where names are the unequal probability categories with the
#'   expected sample size for each category. Names in nsamp must be a subset of the values in caty.
#'   The sum of the category sample sizes is the total sample size for the stratum.
#'
#' @param Nstratum Number of elements in the stratum. Each element will be assigned an
#'   inclusion probabibility. Only required when type is "equal". Default is NULL
#'
#' @param caty If type is unequal, a character variable that identifies the category
#'   for each element of the sample frame that may be used for an unequal probability
#'   design. All categories in nsamp must be present in pt_caty but additional categories
#'   are allowed but will not be included in the survey design for the statum. Default is NULL.
#'
#' @param aux If type is proportional, a numeric vector that will be used to select
#'   points proportional to the variable. Default is NULL.
#'
#' @param warn.ind  A logical value where TRUE indicates a warning message.
#'   Used for internal collection of messages only.
#'
#' @param warn.df A data frame containing messages warning of potential issues.
#'   Used for internal collection of messages only.
#'
#' @return A numeric vector of inclusion probabilities with all values being greater
#'   than 0 and less than or equal 1. Values equal to 1 will be selected with certainty.
#'   Values equal to 0 will not be selected.
#'
#' @author Tony Olsen email{olsen.tony@epa.gov}
#'
#' @keywords survey, inclusion probability
#'
#' @example
#' tmp <- grtspts_ip(type = "equal", nsamp = 10, Nstratum = 25)
#' tmp
#' tmp <- grtspts_ip(type = "unequal", nsamp = c(small = 5, large = 5),
#'           caty = c(rep("small", 15), rep("large", 10)))
#' tmp
#' tmp <- grtspts_ip(type = "proportional", nsamp = 10,
#'                   aux = c(rnorm(20, 3), -2, 30, 40, 0, 0))
#' tmp
#'
#' @export
#################################################################################

grtspts_ip <- function(type = "equal", nsamp, Nstratum = NULL, caty = NULL,
                       aux = NULL, warn.ind = NULL, warn.df = NULL) {

  # equal inclusion probabilities
  if(type == "equal") {
    ip <- rep(nsamp/Nstratum, Nstratum)
  }
  # unequal inclusion probabilities
  if(type == "unequal") {
    gsum <- table(caty)
    catmatch <- match(names(nsamp),names(gsum),nomatch=0)
    piden <- nsamp/gsum[catmatch]
    ip <- rep(NA,length(caty))
    for(i in names(nsamp))
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
      if(warn.ind){
        warn.df <- rbind(warn.df, data.frame(stratum = NA, func = I("grtspts_ip"),
                                             warning = warn))
      } else {
        warn.df <- data.frame(stratum = NA, func = I("grtspts_ip"), warning = warn)
        warn.ind <- TRUE
      }
    }

    if (nneg > 0) {
      ip[ip < 0] <- 0
      warn <- paste0("Proportional vector has ", nneg, " negative value(s) and
              their inclusion probabilities are set to 0.")
      if(warn.ind){
        warn.df <- data.frame(stratum = NA, func = I("grtspts_ip"), warning = warn)
      } else {
        warn.df <- rbind(warn.df, data.frame(stratum = NA, func = I("grtspts_ip"),
                                             warning = warn))
        warn.ind <- TRUE
      }
    }
    # initialize inclusion probabilities
    ip <- nsamp * ip / sum(ip)

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
        ip_gt0[!element_gt1] <- (nsamp - ngt1) * tmp/sum(tmp)
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
  ip <- list(ip = ip, warn.ind = warn.ind, warn.df = warn.df)

  invisible (ip)
}


