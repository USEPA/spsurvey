plot.spsurvey <- function(object, args_sites.base, args_sites.over, args_sites.near) {
  
}

plot.sframe <- function(object, vars = ~ 1, ...) {
  if (terms(vars) == terms(~ 1)) {
    plot(st_geometry(object), ...)
  } else {
    vars_sf <- subset_vars(object, vars)
    plot(vars_sf, ...)
  }
  
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
  list_subset <- lapply(varnames_split, function(x) interaction(object_df[, x], sep = ":"))
  # giving columns appropriate names
  names(list_subset) <- varnames
  # coverting from list to data frame
  df_subset <- as.data.frame(list_subset, optional = TRUE) # without optional the : in name gets
  # converted to synctactic name with .
  df_sf <- st_as_sf(df_subset, geometry = object_geometry)
}
