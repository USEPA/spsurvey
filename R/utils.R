make_formlist <- function(vars, onlyshow) {
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
  # this will be used if onlyshow is removed from summary
  if (missing(onlyshow)) {
    onlyshow <- NULL
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
    onlyshow = onlyshow
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
    if (is.numeric(varsf[[x]]) || (x %in% formlist$response)) { # & !(x %in% formlist$varlabels))) {
      levels <- 0
    } else {
      levels <- length(na.omit(unique(varsf[[x]])))
    }
  })
  levels <- sum(unlist(levels))
}

make_level_args_list <- function(varsf, level_args) {
  level_args_list <- lapply(names(level_args), function(x) {
    vardf <- st_drop_geometry(varsf[x])
    vardf[[x]] <- as.character(vardf[[x]])
    colnames(vardf) <- "levels"
    vardf$index <- 1:nrow(vardf)
    level_args_df <- as.data.frame(level_args[[x]], stringsAsFactors = FALSE)
    level_args_df <- merge(vardf, level_args_df)
    level_args_df <- level_args_df[order(level_args_df$index), , drop = FALSE]
    badcol <- which(colnames(level_args_df) %in% c("levels", "index"))
    level_args_df <- level_args_df[, -badcol, drop = FALSE]
    level_args_listval <- as.list(level_args_df)
  })
  names(level_args_list) <- names(level_args)
  level_args_list
}

make_variable_args_list <- function(varsf, variable_args) {
  variable_args_list <- lapply(names(variable_args), function(x) {
    variable_args_listsub <- lapply(names(variable_args[[x]]), function(y) {
      vardf <- st_drop_geometry(varsf[y])
      vardf[[y]] <- as.character(vardf[[y]])
      colnames(vardf) <- "levels"
      vardf$index <- 1:nrow(vardf)
      variable_args_df <- as.data.frame(variable_args[[x]][[y]], stringsAsFactors = FALSE)
      variable_args_df <- merge(vardf, variable_args_df)
      variable_args_df <- variable_args_df[order(variable_args_df$index), , drop = FALSE]
      badcol <- which(colnames(variable_args_df) %in% c("levels", "index"))
      variable_args_df <- variable_args_df[, -badcol, drop = FALSE]
      variable_args_listsubval <- as.list(variable_args_df)
    })
    names(variable_args_listsub) <- names(variable_args[[x]])
    variable_args_listsub
  })
  names(variable_args_list) <- names(variable_args)
  variable_args_list
}

check_rhs_cat <- function(varsf, formlist) {
  any_numeric <- vapply(varsf[[formlist$varlabels]], is.numeric(), logical(1))
  if (any_numeric) {
    stop("Right hand side of formula must only contain categorical variables")
  }
}

match_sf_defaults <- function(varsf, list_args) {
  if (all(st_geometry_type(varsf, by_geometry = FALSE) %in% c("POINT", "MULTIPOINT"))) {
    sf_defaults <- list(pch = 1, cex = 1, col = 1, bg = 0, lwd = 1, lty = 1, type = "p")
  }
  
  if (all(st_geometry_type(varsf, by_geometry = FALSE) %in% c("LINESTRING", "MULTILINESTRING"))) {
    sf_defaults <- list(lty = 1, lwd = 1, col = 1, pch = 1, type = "l")
  }

  if (all(st_geometry_type(varsf, by_geometry = FALSE) %in% c("POLYGON", "MULTIPOLYGON"))) {
    sf_defaults <- list(lty = 1, lwd = 1, col = NA, cex = 1, pch = NA, border = 1, rule = "evenodd")
  }

  names_list_args <- names(list_args)
  list_args <- lapply(names_list_args, function(x) {
      if (x %in% names(sf_defaults) && any(is.na(list_args[[x]]))) {
        list_args[[x]][is.na(list_args[[x]])] <- sf_defaults[[x]]
      } else {
        list_args[[x]] <- list_args[[x]]
      }
    }
  )
  names(list_args) <- names_list_args
  list_args
}
