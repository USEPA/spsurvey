# File: Adjust.Weights.R
# Contents: Functions to do weight adjustments for Over Sample, Unknown eligibility,
#           and Non Response.
# Programmer: Tony Olsen
# Date: July 15, 2016


#####################################################
# Function:  wgtadjOS
# Purpose: Adjust initial survey design weights using a class-based method when design
#          implementation requires use of over sample sites or when it is desired
#          to have final weights sum to a known frame size.
#'
#' Weight adjustment of initial design weights
#'
#' @description  Adjusts initial survey design weights using a class-based
#'   method when implementation of the design does not use the planned sample
#'   size for the design or when it is desired to have final weights sum to a
#'   known frame size.
#'
#' @param wgt  Vector of initial design weights for each sample unit that will
#'   be adjusted to account for use of sample units greater than (or less than)
#'   the base number of sample units.
#'
#' @param EvalStatus Vector giving the evaluation status for each sample unit.
#'
#' @param OSclass  Vector that identifies for each sample unit the class that
#'   will be used in weight adjustment
#'
#' @param framesize  Named vector containing the frame size (extent) for each
#'   OSclass.  Names must match names in OSclass
#'
#' @param NN  A single value or vector of values that identify sample units that
#'   were not evaluated. That is, sites not needed or not used. Values must be a
#'   subset of the unique values in EvalStatus
#'
#' @details Adjusted weights are equal to initial weight times the frame size
#'   divided by the sum of the initial weights. The adjustment is done
#'   separately for each class specified in OSclass.  Weights for sample units
#'   that are not needed (NN) are set to zero.
#'
#' @return Vector of sample unit weights that are adjusted based on site
#'   evaluation. Length is same as length of wgt.
#'
#' @export
#'
##########################################################
wgtadjOS  <- function (wgt, EvalStatus, OSclass, framesize, NN = "NN") {
  use <- !EvalStatus %in% NN
  wgtsum <- tapply (wgt[use], OSclass[use], sum)
  adjfac <- framesize / wgtsum[match (names (framesize), names (wgtsum))]
  wgtadj <- wgt * adjfac [match (OSclass, names (adjfac))]
  wgtadj[!use] <- 0
  as.vector (wgtadj)
}

#######################################################
# Function: wgtadjU
# Purpose: Adjust weights for sample units that have unknown eligibility
#          using class-based method.
#
#' Weight adjustment for unknown eligibility
#'
#' @description Adjust weights for sample units that have unknown eligibility
#'   using class-based method.  Assumes weights are either initial design
#'   weights when design is implemented as planned or design weights that are
#'   adjusted when sample size of evaluated sites does not match design sample
#'   size.
#'
#' @param wgt  Vector of design weights for each sample unit that will be
#'   adjusted for unknown eligibility
#'
#' @param EvalStatus  Vector of evaluation status for each sample unit. Values
#'   must be one of the values given in ER, ENR, IN, UNK, or NN.
#'
#' @param UNKclass  vector of character vector that identifies for each sample
#'   unit the class that will be used in class-based weight adjustment for
#'   unknown eligibility
#'
#' @param ER  vector of character values in EvalStatus that identify sample
#'   units whose eligibility status is known and that responded
#'
#' @param ENR  vector of character values in EvalStatus that identify sample
#'   units whose eligibility status is known and that do not response
#'
#' @param IN  vector of character values in EvalStatus that identify sample
#'   units that are known to be ineligible
#'
#' @param UNK  vector of character values in EvalStatus that identify sample
#'   units whose eligibility is unknown
#'
#' @param NN  vector of character values in EvalStatus that identify sample
#'   units that are not needed or used in the weight adjustment.
#'
#' @details Class-based weight adjustment is used to adjust the weights for
#'   sites that have unknown eligibility as an unit in the target population.  A
#'   ratio adjustment factor is used to proportionally allocate the weights
#'   associated with sample units that have an unknown class to units that have
#'   known eligibility and known ineligibility. Sample units that are not needed
#'   (NN) or have unknown eligibility (UNK) have weights set to zero.
#'
#' @return Vector of sample unit weights that are adjusted for uknown
#'   eligibility.
#'
#' @export
wgtadjU  <- function(wgt, EvalStatus, UNKclass, ER="ER", ENR="ENR",
                     IN="IN", UNK="UNK", NN="NN"){
  use <- !EvalStatus %in% NN
  num <- tapply (wgt[use], UNKclass[use], sum)
  tst <- use & EvalStatus %in% c(ER, ENR, IN)
  den <- tapply (wgt[tst], UNKclass[tst], sum)
  au <- num / den[match (names (num), names (den))]
  wgtadj <- wgt * au[match (UNKclass, names (au))]
  wgtadj[use & EvalStatus %in% UNK] <- 0
  wgtadj[use & EvalStatus %in% NN] <- 0
  as.vector (wgtadj)
}


#####################################################################
# Function: wgtadjNR
# Purpose: Adjust weights for eligible sample units that do not respond and
#          are missing at random.
#
#' Weight adjustment for Unit non response
#'
#' @description Class-based adjustment of weights for eligible sample units that
#'   do not respond and are missing at random. The missing at random assumption
#'   implies that their sample weight may be assigned to specific classes of
#'   units that are have responded (i.e., have been sampled).
#'
#' @param wgt  Vector of weights for each sample unit that will be adjusted for
#'   unknown eligibility. Weights may be design weights if weights not adjusted
#'   for unknown eligibility. Otherwise they are the design weights.
#'
#' @param EvalStatus Vector of evaluation status for each sample unit. Values
#'   must include the values given in ENRclass or ERClass.  May include other
#'   values not required for unit non-response adjustment.
#'
#' @param MARClass  Vector that identifies for each sample unit the class that
#'   will be used in nonresponse weight adjustment for sample units that are
#'   known to be eligible
#'
#' @param ENRClass  Values in EvalStatus that identify sample units whose
#'   eligibility status is known and that do not respond (i.e., are not sampled)
#'
#' @param ERClass  Values in EvalStatus that identify sample units whose
#'   eligibility status is known and that respond (i.e., are sampled)
#'
#' @details Class-based weight adjustment is used to adjust the weights for
#'   sites that did not respond but are known to be eligible unit in the target
#'   population.  A ratio adjustment factor is used to proportionally allocate
#'   the weights associated with sample units that did not respond to units that
#'   did respond. Weights for sample units that did not response but were known
#'   to be eligible are set to zero. Weights for all other sample units are
#'   unchanged.
#'
#' @return Vector of sample unit weights that are adjusted for nonresponse.
#'
#' @export
#'
##########################################################################
wgtadjNR  <- function(wgt, EvalStatus, MARClass, ENRclass, ERClass ){
  tstENRClass <- EvalStatus %in% c(ENRClass)
  tstERClass <- EvalStatus %in% c(ERClass)
  num <- tapply (wgt[tstENRClass | tstERClass], MARClass[tstENRClass | tstERClass], sum)
  den <- tapply (wgt[tstERClass], MARClass[tstERClass], sum)
  ar <- num / den[match (names (num), names (den))]
  wgt[tstERClass] <- wgt[tstERClass] * ar[match (MARClass, names (ar))][tstERClass]
  wgt[tstENRClass] <- 0
  as.vector(wgt)
}




