###############################################################################
# Function: svychisq_localmean (exported)
# Programmers: Thomas Lumley
#              Tom Kincaid
# Date: January 7, 2014
# Revised: October 23, 2020 to accomodate use of the local mean variance
#          estimator
# Revised: October 30, 2020 to correctly process the column variable  when it
#          includes missing (NA) values
#'
#' Design-Based Contingency Table Tests
#'
#' This function implements design-based contingency table tests.  It is a
#' modified version of the \code{svychisq} function in the survey package.  The
#' modification allows use of the local mean variance algorithms for calculating
#' variance of contingency table means and totals.
#'
#' @param formula Two-sided model formula specifying margins for the contingency
#'   table.
#'
#' @param design Survey design object created by the \code{svydesign} function.
#'
#' @param statistic Character value specifying the test statistic.  See Details.
#'
#' @param vartype  Character value identifying the choice for variance
#'   estimator, where \code{"Local"} = local mean estimator and \code{"SRS"} = simple random
#'   sampling estimator.  The default is \code{"Local"}.
#'
#' @param var_totals Vector containing variance of totals for the contingency
#'   table, which is required for calculating the Wald statistics when vartype
#'   equals \code{"Local"}.  The default is \code{NULL}.
#'
#' @param var_means Vector containing variance of means for contingency table
#'   cells, which is required for calculating the Rao-Scott statistics when
#'   vartype equals \code{"Local"}.  The default is \code{NULL}.
#'
#' @section Details:
#' \code{svychisq_localmean} computes first and second-order Rao-Scott corrections to
#' the Pearson chi-squared test and two Wald-type tests.  For calculating the
#' variance of contingency table means and totals, the function can use either
#' the local mean variance estimator or the simple random sampling variance
#' estimator.
#'
#' The default test (statistic equals \code{"F"}) is the Rao-Scott second-order
#' correction. The p-value is computed using a Satterthwaite approximation to
#' the distribution with denominator degrees of freedom as recommended by Thomas
#' and Rao (1990).
#'
#' The test specified by statistic equals \code{"Chisq"} adjusts the Pearson
#' chi-squared statistic by a design effect estimate and then compares that
#' value to the chi-squared distribution it would have under simple random
#' sampling.
#'
#' The test specified by statistic equals \code{"Wald"} is the procedure proposed by
#' Koch et al (1975) and used by the SUDAAN software package. It is a Wald test
#' based on the differences between the observed cells counts and those expected
#' under independence.
#'
#' The test specified by statistic equals \code{"adjWald"} reduces the Wald statistic
#' when the number of sites is small compared to the number of degrees of
#' freedom of the test. Thomas and Rao (1990) compared The two Wald tests and
#' found the adjustment benefical.
#'
#' The test specified by statistic equals \code{"lincom"} replaces the numerator of the
#' Rao-Scott F with the exact asymptotic distribution, which is a linear
#' combination of chi-squared variables (see \code{pchisqsum}).  The CompQuadForm
#' package is required when statistic equals \code{"lincom"} is specified.
#'
#' The test specified by statistic equals \code{"saddlepoint"} uses a saddlepoint
#' approximation to the  exact asymptotic distribution mentioned previously.
#' The CompQuadForm package is not required when statistic equals \code{"lincom"} is
#' specified. The saddlepoint approximation is especially useful when the
#' p-value is very small (e.g., large-scale multiple testing problems).
#'
#' @return List belonging to class \code{htest}.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{cat_localmean_prop}}}{organizes input and output for
#'       calculation of the local mean variance estimator for estimated
#'       proportions for categorical data}
#'     \item{\code{\link{cat_localmean_total}}}{organizes input and output for
#'       calculation of the local mean variance estimator for estimated sizes
#'       (totals) for categorical data}
#'     \item{\code{\link{chisq.test}}}{performs chi-squared contingency table
#'       tests and goodness-of-fit tests}
#'     \item{\code{\link{svymean}}}{calculates the mean for a complex survey
#'       design}
#'     \item{\code{\link{svytotal}}}{calculates the total for a complex survey
#'       design}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @seealso
#'   \code{\link{cat_localmean_prop}}
#'   \code{\link{cat_localmean_total}}
#'   \code{\link{chisq.test}}
#'   \code{\link{svydesign}}
#'   \code{\link{svymean}}
#'   \code{\link{svytotal}}
#'
#' @keywords survey
#'
#' @examples
#' n <- 100
#' resp <- rnorm(n, 10, 1)
#' wgt <- runif(n, 10, 100)
#' xcoord = runif(n)
#' ycoord = runif(n)
#' sample1 <- data.frame(samp = "sample1", z = resp, wgt = wgt, x = xcoord,
#'   y = ycoord)
#' sample2 <- data.frame(samp = "sample2", z = resp+0.5, wgt = wgt, x = xcoord,
#'   y = ycoord)
#' bounds <- sort(c(sample1$z, sample2$z))[floor(seq((2*n)/3, (2*n),
#'   length=3))]
#' dframe <- rbind(sample1, sample2)
#' dframe$id <- paste0("Site", 1:(2*n))
#' dframe$catvar <- cut(dframe$z, c(-Inf,  bounds))
#' design <- survey::svydesign(id = ~id, weights = ~wgt, data = dframe)
#' svychisq_localmean(~samp+catvar, design, statistic = "adjWald",
#'   vartype = "SRS")
#'
#' @export
###############################################################################

svychisq_localmean <- function(formula, design, statistic = c("F", "Chisq",
  "Wald","adjWald","lincom", "saddlepoint"), vartype = "Local",
  var_totals = NULL, var_means = NULL) {

  statistic <- match.arg(statistic)

  rows <- formula[[2]][[2]]
  cols <- formula[[2]][[3]]
  rowvar <- unique(design$variables[, as.character(rows)])
  rowvar <- rowvar[!is.na(rowvar)]
  colvar <- unique(design$variables[, as.character(cols)])
  colvar <- colvar[!is.na(colvar)]
  nr <- length(rowvar)
  nc <- length(colvar)
  mf1 <- expand.grid(rows = 1:nr, cols = 1:nc)

  fsat <- eval(bquote(~interaction(factor(.(rows)), factor(.(cols))) - 1))
  mm <- model.matrix(fsat, model.frame(fsat, design$variables,
    na.action = na.pass))
  N <- nrow(mm)
  nu <- length(unique(design$cluster[,1])) - length(unique(design$strata[,1]))

  pearson <-  suppressWarnings(chisq.test(svytable(formula, design ,Ntotal = N),
    correct = FALSE))

  if(statistic %in% c("Wald", "adjWald")) {

    frow <- eval(bquote(~factor(.(rows)) - 1))
    fcol <- eval(bquote(~factor(.(cols)) - 1))
    mr <- model.matrix(frow, model.frame(frow, design$variables,
      na.action = na.pass))
    mc <- model.matrix(fcol, model.frame(fcol, design$variables,
      na.action = na.pass))
    one <- rep(1, NROW(mc))
    cells <- svytotal(~mm+mr+mc+one, design, na.rm = TRUE)

    Jcb <- cbind(
      diag(nr * nc),
      -outer(mf1$rows, 1:nr, "==") * rep(cells[(nr*nc) + nr + 1:nc] /
        cells[(nr*nc) + nr + nc + 1], each = nr),
      -outer(mf1$cols, 1:nc,"==") * cells[(nr*nc) + 1:nr] /
        cells[(nr*nc) + nr + nc + 1],
      as.vector(outer(cells[(nr*nc) + 1:nr], cells[(nr*nc +nr) + 1:nc]) /
        cells[(nr*nc) + nr + nc + 1]^2))

    Y <- cells[1:(nc*nr)] - as.vector(outer(cells[(nr*nc) + 1:nr],
      cells[(nr*nc + nr) + 1:nc])) / cells[(nr*nc) + nr + nc + 1]
    if(vartype == "Local") {
      V <- Jcb %*% var_totals %*% t(Jcb)
    } else {
      V <- Jcb %*% attr(cells,"var") %*% t(Jcb)
    }
    use <- as.vector(matrix(1:(nr*nc), nrow = nr, ncol = nc)[-1, -1])
    waldstat <- Y[use] %*% solve(V[use,use], Y[use])

    if (statistic=="Wald") {
      waldstat <- waldstat/((nc-1)*(nr-1))
      numdf <- (nc-1)*(nr-1)
      denomdf <- nu
    } else {
      numdf <- (nr-1)*(nc-1)
      denomdf <- (nu - numdf + 1)
      waldstat <- waldstat * denomdf / (numdf * nu)
    }

    pearson$statistic <- waldstat
    pearson$parameter <- c(ndf = numdf, ddf = denomdf)
    pearson$p.value <- pf(pearson$statistic, numdf, denomdf, lower.tail = FALSE)
    attr(pearson$statistic,"names") <- "F"
    pearson$data.name <- deparse(sys.call(-1))
    pearson$method <- "Design-based Wald test of association"

  } else {

    X1 <- model.matrix(~factor(rows)+factor(cols), mf1)
    X12 <- model.matrix(~factor(rows)*factor(cols), mf1)
    Cmat <- qr.resid(qr(X1),X12[,-(1:(nr+nc-1)),drop = FALSE])
    mean2 <- svymean(mm, design, na.rm = TRUE)
    Dmat <- diag(mean2)
    iDmat <-  diag(ifelse(mean2 == 0, 0, 1/mean2))
    Vsrs <- (Dmat - outer(mean2, mean2))/N
    if(vartype == "Local") {
      V <- var_means
    } else {
      V <- attr(mean2, "var")
    }
    denom <-  t(Cmat) %*% (iDmat/N) %*% Cmat
    numr <- t(Cmat)%*% iDmat %*% V %*% iDmat %*% Cmat
    Delta <- solve(denom,numr)
    d0 <-  sum(diag(Delta))^2/(sum(diag(Delta %*% Delta)))

    if (match.arg(statistic)=="F"){
      pearson$statistic <- pearson$statistic/sum(diag(Delta))
      pearson$p.value <- pf(pearson$statistic, d0, d0*nu, lower.tail = FALSE)
      attr(pearson$statistic,"names") <- "F"
      pearson$parameter <- c(ndf = d0,ddf = d0*nu)
      pearson$method <- "Pearson's X^2: Rao & Scott adjustment"
    }  else if (match.arg(statistic)=="lincom") {
      pearson$p.value <- pFsum(pearson$statistic, rep(1, ncol(Delta)),
        eigen(Delta,only.values = TRUE)$values, lower.tail = FALSE,
        method = "integration",ddf = d0*nu)
      pearson$parameter <- NULL
      pearson$method <- "Pearson's X^2: asymptotic exact distribution"
    } else if  (match.arg(statistic)=="saddlepoint") {
      pearson$p.value <- pFsum(pearson$statistic, rep(1, ncol(Delta)),
        eigen(Delta,only.values = TRUE)$values, lower.tail = FALSE,
        method = "saddlepoint",ddf = d0*nu)
      pearson$parameter <- NULL
      pearson$method <- "Pearson's X^2: saddlepoint approximation"
    } else{
      pearson$p.value <- pchisq(pearson$statistic/mean(diag(Delta)),
        df=NCOL(Delta),lower.tail = FALSE)
      pearson$parameter <- c(df=NCOL(Delta))
      pearson$method <- "Pearson's X^2: Rao & Scott adjustment"
    }
    pearson$data.name <- deparse(sys.call(-1))

  }

  pearson
}

