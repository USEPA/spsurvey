plot.spsurvey <- function(object, args_sites.base, args_sites.over, args_sites.near) {
  
}

plot.sframe <- function(object, vars = ~ 1, ...) {
  oldpar <- par()
  varterms <- terms(vars)
  if (identical(vars, ~ 1)) {
    plot(st_geometry(object), ...)
  } else if (attr(varterms, "response") == 0) {
    vars_sf <- subset_vars(object, vars)
    par(ask = TRUE)
    vars_sf_names <- colnames(vars_sf)[!(colnames(vars_sf) %in% c("geometry"))]
    invisible(lapply(vars_sf_names, function(x) plot(vars_sf[x], main = paste(" ", expression("~"), " ", x, sep = ""), ...)))
  } else {
    xlim <- st_bbox(object)[c(1, 3)]
    ylim <- st_bbox(object)[c(2, 4)]
    response <- all.vars(terms(vars))[1]
    vars_sf <- subset_vars(object, vars)
    if (attr(varterms, "intercept") == 1) {
      vars_sf$intercept <- "intercept"
      vars_sf <- vars_sf[c("intercept", setdiff(colnames(vars_sf), "intercept"))]
    }
    par(ask = TRUE)
    vars_sf_names <- colnames(vars_sf)[!(colnames(vars_sf) %in% c(response, "geometry"))]
    vars_split <- lapply(vars_sf_names, function(x) split(vars_sf[, c(response, x)], vars_sf[[x]]))
    names(vars_split) <- vars_sf_names
    if (attr(varterms, "intercept") == 1) {
      vars_sf_names <- vars_sf_names[vars_sf_names != "intercept"]
      invisible(plot(vars_split[["intercept"]][["intercept"]][response], ...))
      invisible(lapply(vars_sf_names, function(x) lapply(names(vars_split[[x]]),
                                                         function(y) plot(vars_split[[x]][[y]][response],
                                                                          main = paste(response, " ", expression("~"), " ", x, " (", y, ")", sep = ""),
                                                                          xlim = xlim, ylim = ylim, ...))))
    } else {
      invisible(lapply(vars_sf_names, function(x) lapply(names(vars_split[[x]]),
                                                         function(y) plot(vars_split[[x]][[y]][response],
                                                                          main = paste(response, " ", expression("~"), " ", x, " (", y, ")", sep = ""),
                                                                          xlim = xlim, ylim = ylim, ...))))
    }
  }
  on.exit(par(ask = oldpar$ask))
}

subset_vars <- function(object, vars) {
  # convert a*b to a + b + a:b
  varterms <- terms(vars)
  if (attr(varterms, "response") == 1) {
    response <- all.vars(varterms)[1]
  } else {
    response <- NULL
  }
  varnames <- c(response, attr(varterms, "term.labels"))
  # split variables on : to subset 
  varnames_split <- strsplit(varnames, ":")
  object_df <- as.data.frame(unclass(object))
  object_geometry <- st_geometry(object)
  list_subset <- lapply(varnames_split, function(x) {
      if (all(unlist(lapply(object_df[, x], is.numeric)))) {
        object_df[, x]
      } else {
        interaction(object_df[, x], sep = ":") 
      }
    }
  )
  # giving columns appropriate names
  names(list_subset) <- varnames
  # coverting from list to data frame
  df_subset <- as.data.frame(list_subset, optional = TRUE) # without optional the : in name gets
  # converted to synctactic name with .
  df_sf <- st_as_sf(df_subset, geometry = object_geometry)
}
