
###################################################################################
# Function: grtspts_ipleg
# Programmers: Tony Olsen
# Date: January 22, 2021
#'
#' Adjust the initial inclusion probabities for point sample to ensure inclusion of
#' legacy elements that must be in the sample.
#'
#' @param ip Initial inclusion probability for the sample ignoring which elements in the
#'   sample frame must be included in the sample as legacy sites
#'
#' @param legacy A logical vector that identifies whether an element of the sample
#'   frame is a legacy site (TRUE) or not (FALSE) that must be included in the sample.
#'
#' @return A numeric vector of inclusion probabilities with all values being greater
#'   than 0 and less than or equal 1. Values equal to 1 will be selected with certainty.
#'   Values equal to 0 will not be selected. Legacy sites will have an inclusion
#'   probability equal to 1.
#'
#' @author Tony Olsen email{olsen.tony@epa.gov}
#'
#' @keywords survey, inclusion probability
#'
#' @examples
#' a <- runif(30)
#' tmp <- grtspts_ipleg(ip = 10 * a/sum(a), legacy = c(TRUE, rep(FALSE, 5), TRUE, TRUE,
#'                  rep(FALSE, 22)) )
#' tmp
#'
#' @export
###################################################################################
grtspts_ipleg <- function(ip, legacy) {
  # determine sample size
  n <- sum(ip)
  # set legacy site inclusion probabilities equal to 1
  ip[legacy == TRUE] <- 1

  # determine number of elements with inclusion probability >= 1
  elem_nge1 <- ip >= 1
  nge1 <- sum(elem_nge1)

  # adjust non legacy ip to sum to n - nleg
  tst <- 0
  while(nge1 != tst) {
    tmp <- ip[!elem_nge1]
    ip[!elem_nge1] <- (n - nge1) * tmp/sum(tmp)
    tst <- nge1
    element_ge1 <- ip >= 1
    nge1 <- sum(element_ge1)
  }
  invisible (ip)
}
