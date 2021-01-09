make_formlist <- function(vars, showonly) {
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
  # this will be used if showonly is removed from summary
  if (missing(showonly)) {
    showonly <- NULL
  }
  # storing the output list
  formlist <- list(
    varterms = varterms,
    allvars = allvars,
    varlabels = varlabels,
    intercept = intercept,
    response = response,
    varnames = varnames,
    varnames_split = varnames_split,
    showonly = showonly
  )
}

make_varsf <- function(object, formlist) {
  # can possibly deprecate this in the future by making use of
  # model.frame and extracting the main effects and using them to make the interactions
  # only real advantage will be creating variables for use mid formula with numeric variables
  
  # store geometry 
  object_geometry <- st_geometry(object)
  
  if (formlist$intercept && is.null(formlist$response) && length(formlist$varlabels) == 0) {
    return(object_geometry)
  } else {
    
    # remove geometry to make a regular data frame
    object_df <- st_drop_geometry(object)
    formlist <- lapply(
      formlist$varnames_split,
      function(x) {
        if (length(x) == 1 && is.numeric(object_df[[x]])) {
          return(object_df[, x, drop = FALSE]) # return numeric if provided
        } else {
          return(interaction(object_df[, x, drop = FALSE], sep = ":")) # return factors
        }
      }
    )
    varsf <- as.data.frame(formlist, optional = TRUE) # without optional the : in name gets
    # converted to synctactic name with .
    varsf <- st_as_sf(varsf, geometry = object_geometry)
    return(varsf)
  }
}

get_varlevels <- function(formlist, varsf) {
  varsf_nogeom <- st_drop_geometry(varsf)
  levels <- lapply(formlist$varnames, function(x) {
    if (is.numeric(varsf[[x]]) || (x == formlist$response)) { # & !(x %in% formlist$varlabels))) {
      levels <- 0
    } else {
      levels <- length(na.omit(unique(varsf[[x]])))
    }
  })
  levels <- sum(unlist(levels))
}

make_levelargs_list <- function(varsf, levelargs) {
  levelargs_list <- lapply(names(levelargs), function(x) {
    vardf <- st_drop_geometry(varsf[x])
    vardf[[x]] <- as.character(vardf[[x]])
    colnames(vardf) <- "levels"
    vardf$index <- 1:nrow(vardf)
    levelargs_df <- as.data.frame(levelargs[[x]], stringsAsFactors = FALSE)
    levelargs_df <- merge(vardf, levelargs_df)
    levelargs_df <- levelargs_df[order(levelargs_df$index), , drop = FALSE]
    badcol <- which(colnames(levelargs_df) %in% c("levels", "index"))
    levelargs_df <- levelargs_df[, -badcol, drop = FALSE]
    levelargs_list <- as.list(levelargs_df)
  })
  names(levelargs_list) <- names(levelargs)
  levelargs_list
}

check_rhs_cat <- function(varsf, formlist) {
  any_numeric <- vapply(varsf[[formlist$varlabels]], is.numeric(), logical(1))
  if (any_numeric) {
    stop("Right hand side of formula must only contain categorical variables")
  }
}
