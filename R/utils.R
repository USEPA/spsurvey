# Helpers -----------------------------------------------------------------

make_formlist <- function(formula, onlyshow, object) {
  # find all terms from the formula
  varterms <- terms(formula, data = object)
  # find all variable names
  allvars <- all.vars(varterms)
  # find all right hand side names
  varlabels <- attr(varterms, "term.labels")
  # remove geometry if present
  varlabels <- varlabels[varlabels != "geometry"]
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



  if (formlist$intercept && is.null(formlist$response) && length(formlist$varlabels) == 0) {
    return(object)
  } else {
    # store geometry
    if ("sf" %in% class(object)) {
      object_geometry <- st_geometry(object)
      object_df <- st_drop_geometry(object)
    } else {
      object_df <- object
    }
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
    if ("sf" %in% class(object)) {
      varsf <- st_as_sf(varsf, geometry = object_geometry)
    }
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

make_varlevel_args_list <- function(varsf, varlevel_args) {
  varlevel_args_list <- lapply(names(varlevel_args), function(x) {
    vardf <- st_drop_geometry(varsf[x])
    vardf[[x]] <- as.character(vardf[[x]])
    colnames(vardf) <- "levels"
    vardf$index <- 1:nrow(vardf)
    varlevel_args_df <- as.data.frame(varlevel_args[[x]], stringsAsFactors = FALSE)
    varlevel_args_df <- merge(vardf, varlevel_args_df)
    varlevel_args_df <- varlevel_args_df[order(varlevel_args_df$index), , drop = FALSE]
    badcol <- which(colnames(varlevel_args_df) %in% c("levels", "index"))
    varlevel_args_df <- varlevel_args_df[, -badcol, drop = FALSE]
    varlevel_args_listval <- as.list(varlevel_args_df)
  })
  names(varlevel_args_list) <- names(varlevel_args)
  varlevel_args_list
}

make_var_args_list <- function(varsf, var_args) {
  var_args_list <- lapply(names(var_args), function(x) {
    var_args_listsub <- lapply(names(var_args[[x]]), function(y) {
      vardf <- st_drop_geometry(varsf[y])
      vardf[[y]] <- as.character(vardf[[y]])
      colnames(vardf) <- "levels"
      vardf$index <- 1:nrow(vardf)
      var_args_df <- as.data.frame(var_args[[x]][[y]], stringsAsFactors = FALSE)
      var_args_df <- merge(vardf, var_args_df)
      var_args_df <- var_args_df[order(var_args_df$index), , drop = FALSE]
      badcol <- which(colnames(var_args_df) %in% c("levels", "index"))
      var_args_df <- var_args_df[, -badcol, drop = FALSE]
      var_args_listsubval <- as.list(var_args_df)
    })
    names(var_args_listsub) <- names(var_args[[x]])
    var_args_listsub
  })
  names(var_args_list) <- names(var_args)
  var_args_list
}

check_rhs_cat <- function(varsf, formlist) {
  any_numeric <- vapply(varsf[[formlist$varlabels]], is.numeric(), logical(1))
  if (any_numeric) {
    stop("Right hand side of formula must only contain categorical variables")
  }
}

match_sf_defaults <- function(varsf, list_args) {
  sf_default_df <- data.frame(
    geometry = c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON"),
    pch = c(1, 1, 1, 1, NA, NA),
    cex = c(1, 1, NA, NA, 1, 1),
    col = c(1, 1, 1, 1, NA, NA),
    bg = c(0, 0, NA, NA, NA, NA),
    lwd = c(1, 1, 1, 1, 1, 1),
    lty = c(1, 1, 1, 1, 1, 1),
    type = c("p", "p", "l", "l", NA, NA),
    border = c(NA, NA, NA, NA, 1, 1),
    rule = c(NA, NA, NA, NA, "evenodd", "evenodd"),
    stringsAsFactors = FALSE
  )
  sf_defaults <- merge(as.character(st_geometry_type(varsf)), sf_default_df, sort = FALSE)
  # sf_defaults <- lapply(st_geometry_type(varsf), get_sf_defaults)
  # sf_defaults <- do.call("rbind", sf_defaults)


  names_list_args <- names(list_args)
  list_args <- lapply(names_list_args, function(x) {
    if (x %in% names(sf_defaults) && any(is.na(list_args[[x]]))) {
      list_args[[x]] <- ifelse(is.na(list_args[[x]]), sf_defaults[[x]], list_args[[x]])
    } else {
      list_args[[x]] <- list_args[[x]]
    }
  })
  names(list_args) <- names_list_args
  list_args
}

# get_sf_defaults <-   function(geometry) {
#   if (geometry %in% c("POINT", "MULTIPOINT")) {
#     sf_defaults <- data.frame(pch = 1, cex = 1, col = 1, bg = 0, lwd = 1, lty = 1, type = "p", border = NA, rule = NA, stringsAsFactors = FALSE)
#   } else if (geometry %in% c("LINESTRING", "MULTILINESTRING")){
#     sf_defaults <- data.frame(pch = 1, cex = NA, col = 1, bg = 0, lwd = 1, lty = 1, type = "l", border = NA, rule = NA, stringsAsFactors = FALSE)
#   } else if (geometry %in% c("POLYGON", "MULTIPOLYGON")){
#     sf_defaults <- data.frame(pch = NA, cex = 1, col = NA, bg = NA, lwd = 1, lty = 1, type = NA, border = 1, rule = "evenodd", stringsAsFactors = FALSE)
#   } else {
#     stop("All x geometries must be POINT, MULTIPOINT, LINESTRING, MULTILINESTRING, POLYGON, or MULTIPOLYGON")
#   }
#   sf_defaults
# }
