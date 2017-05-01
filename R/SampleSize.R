# File: SampleSize.R
# Purpose: Compute sample sizes for estimating means and proportions
#					 for a simple random sample survey design
#	Programmer: Tony Olsen
# Date: July 20, 2016

########################################################################
# Function: n.mean
# Purpose: Sample size for estimating a mean
#########
#'@title Sample size for estimating a mean
#'
#'@description Calculates sample size for estimating mean with 1-beta
#'  probability that the 100*conf confidence interval will have length equal to
#'  halfwidth. May specify multiple halfwidths for sample size calculation.
#'
#'@param halfwidth  A single value or vector of one-half the width of the
#'  confidence interval desired.
#'
#'@param	s  Estimate of standard deviation for the population.
#'
#'@param	v  Degrees of freedom for associated with the estimated standard
#'  deviation. Only required for Harris method. Default is NULL.
#'
#'@param	conf  A single value or vector for confidence level desired. Default is
#'  0.95.
#'
#'@param tolprb Tolerance probability for confidence interval coverage. Default
#'  is 0.90.
#'
#'@param N  Finite population size.  If NULL (default), then infinite population
#'  assumed.
#'
#'@param method  Which sample size approach to be used.  If 'Harris', then
#'  Harris, Horvitz, & Moody (1948). If 'Guenther', then Guenther(1965). If
#'  'Cochran', then Cochran(1977) simple formulation using t-distribution.
#'  Default is 'Harris'.
#'
#' @details  Sample size for confidence interval for a mean is calculated used
#'   three alternative methods.  Cochran method is the standard approach given
#'   in most books and assumes a known variance.  Guenther and Harris methods
#'   are a tolerance interval approach that require meeting a tolerance
#'   probability for the confidence interval.  Guenther method assumes a known
#'   variance while Harris method assumes an estimated variance.
#'
#' @return A single value or an array of the sample size required to meet
#'   desired precision.
#'
#' @references
#'    Kupper, L. L. and K. B. Hafner (1989). "How appropriate are popular sample size formulas?"
#'         The American Statistician 43(2): 101-105.
#'
#'    Harris, Horvitz, & Moody (1948) On the Determination of Sample Sizes
#'        in Designing Experiments. American Statistical Association Journal 43:391-402.
#'
#'    Cochran, W. G. (1987). Sampling Techniques. New York, John Wiley & Sons.
#
#' @seealso PracticalTools package has several functions for sample size
#'   calculations. nCont uses coefficient of variance as input for confidence
#'   interval for mean.  strAlloc allocates sample size to strata.
#
#' @examples
#' Example from Zar(1999)
#' n.mean (1.5, s = sqrt(18.0388), method = 'Cochran')
#' n.mean (1.5, s = sqrt(18.0388), method = 'Guenther')
#' n.mean (1.5, s = sqrt(18.0388), v = 24, method = 'Harris')
#
#' # n.mean test when given vector of halfwidth
#' n.mean (c(1.0, 1.5, 2.0, 2.5), s = sqrt(18.0388),
#'         conf=c(0.90, 0.95), method = 'Cochran')
#' n.mean(c(1.0, 1.5, 2.0, 2.5), s = sqrt(18.0388), method = 'Guenther')
#' n.mean(c(1.0, 1.5, 2.0, 2.5), s = sqrt(18.0388), v =24, method = 'Harris')
#'
#' @export
#'
##########################
n.mean <- function (halfwidth, s, v = NULL, conf = 0.95, tolprb = 0.90,
                    N = NULL, method = 'Harris') {
  # Cochran method for sample size ====================================
	if (method == 'Cochran') {
		alpha <- (1 - conf) / 2
		n <- array (NA, c(length (halfwidth), length (alpha)))
		for(j in 1:length (alpha)) {
		  n.est <- as.vector ((s * ((1 / halfwidth) %o% qt (1 - alpha[j], 100)) )^2) # initial n estimate
		  n[,j] <- (s * diag (1 / halfwidth, nrow=length(1 / halfwidth)) %*%
		              qt (1 - alpha[j], n.est - 1) )^2 # update estimate based on new df
		  while (any (abs (n[,j] - n.est) > 1 )) {
		    n.est <- n[,j]
		    n[,j] <- (s * diag (1 / halfwidth, nrow=length(1 / halfwidth)) %*%
		                qt (1 - alpha[j], n.est - 1) )^2
		  }
		}
		ifelse (is.null (N), n <- ceiling(n), n <- ceiling (n / (1 + (n-1)/N)) )
		return (n)
	}
  # Guether method for sample size =================================================
  if (method == 'Guenther') {
    f <- function (n.est, s, halfwidth, conf = conf, tolprb = tolprb) {
      tmp <- abs (n.est * (n.est - 1) - qchisq (tolprb, n.est - 1) *
                    qf (conf, 1, n.est - 1) * s^2 / halfwidth^2  )
    }
    ff <- function (halfwidth, f,  s = s, conf = conf, tolprb = tolprb) {
      rslt <- optimize (f, lower = 2, upper = 100000, maximum = FALSE,
                        s = s, halfwidth = halfwidth, conf = conf, tolprb = tolprb)
      }
    n <- array (NA, c(length (halfwidth), length (conf)))
    for(j in 1:length (conf)) {
    rslt <- sapply (halfwidth, ff, f, s = s, conf[j], tolprb = tolprb)
    n[,j] <- unlist(rslt[1,])
    }
    ifelse (is.null (N), n <- ceiling(n), n <- ceiling (n / (1 + (n-1)/N)) )
    return (n)
  }
  # Harris method for sample size ================================
	if(method == 'Harris') {
		alpha <- (1 - conf) / 2
		beta <- 1 - tolprb
		n <- array (NA, c(length (halfwidth), length (alpha)))
		for (j in 1:length(alpha)) {
		  n.est <- 25
		  n[,j] <- s^2 * qt (alpha[j], n.est)^2 *
		           qf (beta, n.est, v, lower.tail = FALSE) / halfwidth^2
		  while (any(abs(n[,j] - n.est) > 1 )) {
		    n.est <- n[,j]
			  n[,j] <- s^2 * qt (alpha[j], ceiling (n.est) - 1)^2 *
			    qf (beta, ceiling (n.est) - 1, v, lower.tail = FALSE) / halfwidth^2
			  }
		  }
		ifelse (is.null (N),  n <- ceiling (n), n <- ceiling (n / (1 + (n - 1) / N)) )
		return (n)
	}
}
#######################################################

########################################################
# Function: n.prop
# Purpose: Sample size for estimating proportions
#######################################################
#'
#' @title Sample size for estimating proportions
#'
#' @description Calculates sample size for estimating proportion with 1-beta
#'   probability that the 100*conf confidence interval will have length equal to
#'   halfwidth.
#'
#'@param halfwidth Single value or vector of one-half the width of the
#'  confidence interval desired for the true proportion.
#'
#'@param	P  Single value or vector of true proportion for the population desired
#'
#'@param	v  Degrees of freedom for associated with the estimated standard
#'  deviation of the assumed true proportion. Only required for Harris method
#'
#'@param	conf  Confidence level desired
#'
#'@param tolprb  Tolerance probability for confidence interval coverage
#'
#'@param N  Finite population size.  If NULL, then infinite population assumed
#'  Default is NULL.
#'
#'@param method   Which sample size approach to be used.  If 'Wilson', then the
#'  score confidence interval is used as first discussed by Wilson (1927). If
#'  'Wald', then Cochran(1977) simple formulation using t-distribution. Default
#'  is 'Wilson'.
#'
#'@details Three methods for computing the sample size required for a confidence
#'  interval for a proportion are given. Wald method is the standard method
#'  given by Cochran (1977) with enhancement to use t-distribution with correct
#'  degrees of freedom which requires iteration to achieve. It assumes that true
#'  proportion is known.  Wilson method is based on the score confidence
#'  interval for a proportion and assumes true proportion is known. Harris
#'  method is a tolerance interval approach that requires meeting a tolerance
#'  probability for the confidence interval and assumes variance of p is
#'  estimated with v degrees of freedom.
#'
#'@return The estimated sample size required to meet desired precision as a
#'  single value or as an array that is number of halfwidths by number of P
#'  values specified.
#'
#' @references
#' Agresti, A. and B. A. Coull (1998). "Approximate is better than `exact' for interval
#'     estimation of binomial proportions." The American Statistician 52: 119-126.
#'
#' Cochran, W. G. (1977). Sampling Techniques. New York, John Wiley & Sons.
#'
#' Wilson, E. B. (1927). "Probable inference, the law of succession, and statistical inference."
#'   Journal of the American Statistical Association 22: 209-212.
#
#' @seealso PracticalTools package has several functions for determining sample sizes for proportions
#'      including nProp, nPropMoe, nWilson.
#'
#' @examples
#' Example from Cochran for proportions
#' n.prop(0.05, P=.5, method = 'Wald')
#' n.prop(0.05, P=.5, method = 'Wilson')
#' n.prop(0.05, P=.5, v=25, method = 'Harris')
#' n.prop(0.05, P=.5, N = 3200, method = 'Wald')
#' n.prop(0.05, P=.5, N = 3200, v = 25, method = 'Harris')
#' Examples when given vector of halfwidth and of P
#' n.prop(halfwidth = c(0.025, 0.05, 0.075, 0.10, 0.20), P = c(0.3, 0.5), N=3200, method='Wald')
#' n.prop(c(0.025, 0.05, 0.075, 0.10, 0.20), P = c(0.3, 0.5), N=3200, method='Wilson')
#' n.prop(c(0.025, 0.05, 0.075, 0.10, 0.20), P = c(0.3, 0.5), v=25,  N=3200, method='Harris')
#
#' @export
#'
############################################
n.prop <- function (halfwidth, P = 0.5, v=NULL, conf = 0.95, tolprb = .90,
                    N = NULL, method = 'Wilson'){
	if( method == 'Wald' ) {
		alpha <- (1 - conf) / 2
		n.est <- 25 # initial df for t-distribution
		n <- array (NA, c(length (halfwidth), length (P)))
		for (j in 1:length (P) ) {
		  n[,j] <- P[j] * (1 - P[j]) * qt (alpha, n.est)^2 / halfwidth^2
		  }
		ifelse (is.null (N),  n <- ceiling (n),  n <- ceiling (n / (1 + (n - 1) / N)) )
		while ( any (abs (n - n.est) > 1 )) {
			n.est <- n
			for (j in 1:length (P) ) {
			  n[,j] <- P[j] * (1 - P[j]) * qt (alpha, n.est[,j] - 1 )^2 / halfwidth^2
			}
			if (is.null (N) )  n <- ceiling (n)
			else n <- ceiling (n / (1 + (n - 1) / N))
		}
	}
	if(method == 'Wilson') {
	  alpha <- (1 - conf) / 2
	  ifelse (!is.null (N), maxn <- N, maxn <- 100000)
    hwdif <- function (n, p, hw, z) {
         lcl <- (p + z^2 / (2 * n) - z * (p * (1-p) / n + z^2 / (4 * n^2))^0.5) / (1 + z^2 / n)
         ucl <- (p + z^2 / (2 * n) + z * (p * (1-p) / n + z^2 / (4  *n^2))^0.5) / (1 + z^2 / n)
         abs (hw - (ucl - lcl) / 2)
         }
    n <- array(NA, c(length (halfwidth), length (P)))
    for (i in 1:length (halfwidth)) {
      for (j in 1:length (P)) {
        n[i, j] <- optimize (hwdif, c(0, maxn), p = P[j], hw = halfwidth[i], z = qnorm (1 - alpha))$minimum
      }
    }
    n <- ceiling (n)
  }
	if(method == 'Harris' ) {
		alpha <- (1 - conf)/2
		beta <- 1 - tolprb
		n.est <- 25 # Initial estimate of sample size for use in t-distribution
		n <- array(NA, c(length (halfwidth), length (P)))
		for (j in 1:length(P) ) {
		n[,j] <- P[j] * (1-P[j]) * qt (alpha, n.est)^2 * qf (beta, n.est, v, lower.tail = FALSE) / halfwidth^2
		}
		ifelse (is.null(N),  n <- ceiling(n), n <- ceiling (n/(1 + (n-1)/N)) )
		while ( any (abs (n - n.est) > 1 )) {
			n.est <- n
			for (j in 1:length(P) ) {
			n[,j] <- P[j] * (1-P[j]) * qt (alpha, n.est[,j])^2 *
			                         qf (beta, n.est[,j], v, lower.tail = FALSE) / halfwidth^2
			}
			ifelse (is.null(N), n <- ceiling(n), n <- ceiling (n / (1 + (n-1) / N)))
		}
	}
	return(n)
}
#######################################################


########################################################
# Function: n.mean.test
# Purpose: Sample size for testing if mean is equal to a hypothesized value
########################################################
#' @title Sample size for testing a mean
#'
#' @description Sample size for test of mean being equal to, less than/equal to
#'   or greater than/equal to a hypothesized value with alpha as significance
#'   probability (Type I error) and beta probability of type II error to detect
#'   a difference of delta.
#'
#' @param delta  single value or vector of difference(s) to be detected
#'
#' @param	s  Estimate of standard deviation for the population
#'
#' @param alpha  Significance level desired for test. Default is 0.05.
#'
#' @param power  Single value or vector of power (1 - beta) desired for test.
#'   Default is 0.90.
#'
#' @param alternative	 Equal to 'twoway' for two-sided test or 'oneway' for
#'   one-sided test. Default is 'twoway'.
#'
#' @param N  Finite population size.  If NULL (default), then infinite
#'   population assumed
#'
#' @details Standard z-test or t-test based sample size formulas are used to
#'   compute the required sample size.
#'
#' @return  The estimated sample size required to meet desired power as a single
#'   value or as an array that is number of deltas by number of P values
#'   specified
#'
#' @references
#'  Zar, J. H. (1999). Biostatistical Analysis. Upper Saddle River,
#'     New Jersey, Prentice-Hall, Inc.
#'
#'  Guenther, W. C. (1973). "Determination of sample size for tests concerning means
#'    and variances of normal distributions." Statistical Neerlandica 27: 103-113.
#'
#' @seealso PracticalTools package has functions for determining sample sizes
#'   for testing means. They include nDep2sam and nProp2sam
#'
#' @examples
#' n.mean.test(1, s=2, power=0.99, alpha=0.05, alternative='oneway', method='Ztest')
#' n.mean.test(1, s=2, power=0.99, alpha=0.05, alternative='oneway', method='Ttest')
#' n.mean.test( delta = c( 1.0, 1.5, 2.0, 2.5), s=sqrt(1.5682),
#'             power=c(0.90, 0.95), method='Ttest')
#' n.mean.test( delta = c( 1.0, 1.5, 2.0, 2.5), s=sqrt(1.5682),
#'              power=c(0.90, 0.95), method='Ztest')
#' @export
#'
################################################
n.mean.test <- function(delta, s, alpha = 0.05, power = .90,
                        alternative = 'twoway',   N = NULL, method = 'Ttest') {
#
	alpha <- ifelse (alternative == 'twoway', alpha/2, alpha)
	beta <- 1 - power
#  Method for sample size when variance is an estimate  ========================
		if ( method == 'Ttest' ) {
		  nsamp <- (s *(1 / delta) %o% (qnorm (1-alpha) + qnorm (power)) )^2
		  for (j in 1:length(power)) {
		    for ( i in 1:length(delta)) {
		      n.est <- nsamp[i, j]
		      nsamp[i,j] <- s^2 * (qt (alpha, n.est - 1) + qt (beta[j], n.est - 1) )^2  / delta[i]^2
		      ifelse (is.null(N),  nsamp[i,j] <- ceiling (nsamp[i,j]),
		              nsamp[i,j] <- ceiling (nsamp[i,j]/(1 + (nsamp[i,j] - 1) / N)) )
		      while ( abs(nsamp[i,j] - n.est) > 0.01)  {
		        n.est <- nsamp[i,j]
		        nsamp[i,j] <- s^2 * (qt (alpha, n.est - 1) + qt (beta[j], n.est - 1) )^2  / delta[i]^2
		      }
		      ifelse (is.null(N),  nsamp[i,j] <- ceiling(nsamp[i,j]),
		              nsamp[i,j] <- ceiling(nsamp[i,j]/(1 + (nsamp[i,j] - 1) / N)) )
		    }
		  }
		}

	# Method for sample size when variance is known  ========================
	if ( method == 'Ztest' ) {
	  nsamp <- (s *(1 / delta) %o% (qnorm (1-alpha) + qnorm (power)) )^2
	  ifelse (is.null(N),  nsamp <- ceiling (nsamp),
	          nsamp <- ceiling (nsamp/(1 + (nsamp - 1) / N)) )
	}
	return (nsamp)
}
#######################################################



