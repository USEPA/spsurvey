summary.spsurvey <- function(object, type = "counts", vars, ...) {
  switch(type, 
         "counts" = summary.spsurvey_counts(object, vars, ...),
         "numsum" = summary.spsurvey_numsum(object, vars, ...),
         stop(cat("type must equal \"counts\" or \"numsum\""))
  )
}

summary.spsurvey_counts <- function(object, vars, maxsum = 1e10, ...) {
  # keep the sites sf objects from class spsurvey
  sites <- object[names(object) %in% c("sites.base", "sites.over", "sites.near")]
  # compute the counts for each sites set
  counts <- lapply(sites, return_counts, vars, maxsum, ...)
  # remove NULL counts
  counts <- counts[!vapply(counts, is.null, logical(1))]
}

return_counts <- function(df, vars, maxsum, ...) {
  
  # convert a*b to a + b + a:b
  varterms <- terms(vars)
  varnames <- attr(varterms, "term.labels")
  if (length(varnames) == 0) {
    stop("right hand side of vars formula must contain at least one variable")
  }
  # split variables on : to subset 
  varnames_split <- strsplit(varnames, ":")

  # keep NULL value if there
  if (is.null(df)) {
    counts_sites <- NULL
    return(counts_sites)
  } else {
    # remove sticky geometry and convert characters to factors to print in summary.data.frame
    sites <- as.data.frame(unclass(df))
    # dealing with intercept
    if (attr(varterms, "intercept") == 1) {
      varnames_split <- c("total", varnames_split)
      sites <- cbind(data.frame(total = "total"), sites)
      varnames <- c("total", varnames)
    }
    # take the interaction of each set of factors
    list_sites <- lapply(varnames_split, function(x) interaction(sites[, x], sep = ":")) # makes numerics a factor
    # giving columns appropriate names
    names(list_sites) <- varnames
    # coverting from list to data frame
    df_sites <- as.data.frame(list_sites)
    # returning the summary
    counts_sites <- summary.data.frame(df_sites, maxsum, ...)
    return(counts_sites)
  }
}

summary.spsurvey_numsum <- function(object, vars, ...) {
  # keep the sites sf objects from class spsurvey
  sites <- object[names(object) %in% c("sites.base", "sites.over", "sites.near")]
  # compute the numsum for each sites set
  numsum <- lapply(sites, return_numsum, vars, maxsum, ...)
  # remove NULL numsum
  numsum <- numsum[!vapply(numsum, is.null, logical(1))]
}

return_numsum <- function(df, vars, ...) {
  # convert a*b to a + b + a:b
  varterms <- terms(vars)
  varnames <- attr(varterms, "term.labels")
  # get variable name of response
  response <- all.vars(varterms)[1]
  if (length(varnames) == 0 && attr(varterms, "intercept") == 0) {
    stop("right hand side of vars formula must contain at least one variable or an intercept")
  }
  # split variables on : to subset 
  varnames_split <- strsplit(varnames, ":")
  
  # keep NULL value if there
  if (is.null(df)) {
    numsum_sites <- NULL
    return(numsum_sites)
  } else {
    # remove sticky geometry and convert characters to factors to print in summary.data.frame
    sites <- as.data.frame(unclass(df))
    if (attr(varterms, "intercept") == 1) {
      varnames_split <- c("total", varnames_split)
      sites <- cbind(data.frame(total = "total"), sites)
      varnames <- c("total", varnames)
    }
    numsum_sites <- lapply(varnames_split, function(x) do.call(rbind, tapply(sites[, response], interaction(sites[, x], sep = ":"), summary, ...)))
    # giving list entries appropriate names
    names(numsum_sites) <- varnames
    return(numsum_sites)
  }
}





