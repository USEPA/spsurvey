make_varlist <- function(vars, varsub) {
  # find all terms from the formula
  varterms <- terms(vars)
  # find all variable names
  allvars <- all.vars(varterms)
  # find all right hand side names
  varlabels <- attr(varterms, "term.labels")
  # find if intercept exists in the formula
  if (attr(varterms, "intercept") == 1) {
    intercept <- TRUE
  } else {
    intercept <- FALSE
  }
  # find if response exists in the formula
  if (attr(varterms, "response") == 1) {
    response <- allvars[1]
  } else {
    response <- NULL
  }
  # make a vector of names
  varnames <- c(response, varlabels)
  # make a list where names are split if they interact
  varnames_split <- strsplit(varnames, ":")
  # giving the list names
  names(varnames_split) <- varnames
  # this will be used if varsub is removed from summary
  if (missing(varsub)) {
    varsub <- NULL
  }
  # storing the output list
  varlist <- list(
    varterms = varterms,
    allvars = allvars,
    varlabels = varlabels,
    intercept = intercept,
    response = response,
    varnames = varnames,
    varnames_split = varnames_split,
    varsub = varsub
  )
}

make_varsf <- function(object, varlist) {
  # can possibly deprecate this in the future by making use of
  # model.frame and extracting the main effects and using them to make the interactions
  # only real advantage will be creating variables for use mid formula with numeric variables
  
  # store geometry 
  object_geometry <- st_geometry(object)
  
  if (varlist$intercept && is.null(varlist$response) && length(varlist$varlabels) == 0) {
    return(object_geometry)
  } else {
    
    # remove geometry to make a regular data frame
    object_df <- st_drop_geometry(object)
    varlist <- lapply(
      varlist$varnames_split,
      function(x) {
        if (length(x) == 1 && is.numeric(object_df[[x]])) {
          return(object_df[, x, drop = FALSE]) # return numeric if provided
        } else {
          return(interaction(object_df[, x, drop = FALSE], sep = ":")) # return factors
        }
      }
    )
    varsf <- as.data.frame(varlist, optional = TRUE) # without optional the : in name gets
    # converted to synctactic name with .
    varsf <- st_as_sf(varsf, geometry = object_geometry)
    return(varsf)
  }
}

get_varlevels <- function(varlist, varsf) {
  varsf_nogeom <- st_drop_geometry(varsf)
  levels <- lapply(varlist$varnames, function(x) {
    if (is.numeric(varsf[[x]]) || (x == varlist$response & !(x %in% varlist$varlabels))) {
      levels <- 0
    } else {
      levels <- length(na.omit(unique(x)))
    }
  })
  levels <- sum(unlist(levels))
}

