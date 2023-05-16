############################################################################
# Function: adjwgtNR (exported)
# Programmer: Tony Olsen
# Date: April 5, 2022
#
#' Adjust survey design weights for non-response by categories
#'
#' @description Adjust weights for target sample units that do not respond 
#' and are missing at random within categories. The missing at random
#' assumption implies that their sample weight may be assigned to
#' specific categories of units that have responded (i.e., have been
#' sampled). This is a class-based method for non-response adjustment.
#'
#' @param wgt vector of weights for each sample unit that will be adjusted
#'  for non-response. Weights must be weights for the design as implemented.
#'  All weights must be greater than zero.
#'           
#' @param MARClass vector that identifies for each sample unit the category
#'  that will be used in non-response weight adjustment for sample units 
#'  that are known to be target. Within each missing at random (MAR) 
#'  category, the missing sample units that are not sampled are assumed to
#'  be missing at random.
#'            
#' @param EvalStatus vector of the evaluation status for each sample unit.
#'  Values must include the values given in TNRclass and TRClass. May 
#'  include other values not required for the non-response adjustment.
#'                 
#' @param TNRClass subset of values in EvalStatus that identify sample units
#'  whose target status is known and that do not respond (i.e., are not 
#'  sampled).
#'                  
#' @param TRClass Subset of values in EvalStatus that identify sample units 
#'  whose target status is known and that respond (i.e., are target and
#'  sampled).
#'
#' @return Vector of sample unit weights that are adjusted for non-response
#'  and that is the same length of input weights. Weights for sample 
#'  units that did not response but were known to be eligible are set
#'  to zero. Weights for all other sample units are also set to zero.
#' 
#' @export
#'
#' @author Tony Olsen \email{olsen.tony@epa.gov}
#' 
#' @keywords survey non-response weight adjustment
#' 
#' @examples
#' set.seed(5)
#' wgt <- runif(40)
#' MARClass <- rep(c("A", "B"), rep(20, 2))
#' EvalStatus <- sample(c("Not_Target", "Target_Sampled", "Target_Not_Sampled"), 40, replace = TRUE)
#' TNRClass <- "Target_Not_Sampled"
#' TRClass <- "Target_Sampled"
#' adjwgtNR(wgt, MARClass, EvalStatus, TNRClass, TRClass)
#' # function that has an error check
adjwgtNR  <- function(wgt, MARClass, EvalStatus, TNRClass, TRClass){
  tstTNRClass <- EvalStatus %in% c(TNRClass)
  tstTRClass <- EvalStatus %in% c(TRClass)
  num <- tapply(wgt[tstTNRClass | tstTRClass],
                MARClass[tstTNRClass | tstTRClass], sum)
  den <- tapply(wgt[tstTRClass], MARClass[tstTRClass], sum)
  # error check
  # could use any(! unique(MARClass[tstTNRClass]) %in% unique(MARClass[tstTRClass]))
  if (length(num) > length(den)) {
    stop("At least one level of MARClass does not have any EvalStatus values in
         TRClass, so no non-response weight adjustment can be performed.
         Consider aggregating categories so that all levels of MARClass are
         instead in TRClass.", call. = FALSE)
  }
  ar <- num/den[match(names(num), names(den))]
  wgt[tstTRClass] <- wgt[tstTRClass] *
    ar[match(MARClass, names(ar))][tstTRClass]
  wgt[!tstTRClass] <- 0
  as.vector(wgt)
}