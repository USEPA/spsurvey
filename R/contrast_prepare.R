# Prepare the same survey design used by the other analysis entry points.
# Keep validation, FPC/PPS construction and calibration in their existing helpers.
contrast_prepare <- function(dframe, vars_cat, vars_cont, subpops, siteID,
                              weight, xcoord, ycoord, stratumID, clusterID,
                              weight1, xcoord1, ycoord1, sizeweight, sweight,
                              sweight1, fpc, formula, popsize, subpopsize,
                              vartype, jointprob, conf, local_nbh, reject_missing = FALSE) {
  greg_check_call(formula, subpopsize)
  categories <- declared_categories(dframe, vars_cat, reject_missing = reject_missing)
  if (inherits(dframe, "sf")) {
    xy <- sf::st_coordinates(dframe)
    xcoord <- "xcoord"
    ycoord <- "ycoord"
    dframe$xcoord <- xy[, "X"]
    dframe$ycoord <- xy[, "Y"]
    dframe <- sf::st_drop_geometry(dframe)
  }
  dframe <- as.data.frame(dframe)
  if (!nrow(dframe)) stop("dframe must contain rows.", call. = FALSE)
  if (is.null(formula)) dframe <- droplevels(dframe)
  if (is.null(siteID)) {
    siteID <- "siteID"
    dframe[[siteID]] <- paste0("site-", seq_len(nrow(dframe)))
  }
  if (!siteID %in% names(dframe)) stop("siteID is not a column in dframe.", call. = FALSE)
  warn_df <- NULL
  if (anyDuplicated(dframe[[siteID]])) {
    dframe[[siteID]] <- uniqueID(dframe[[siteID]])
    warn_df <- contrast_warning(warn_df, "Repeated site IDs were supplied.",
      "Unique site IDs were created.")
  }
  clustered <- !is.null(clusterID)
  stratified <- !is.null(stratumID)
  dn <- list(siteID = siteID, weight = weight, xcoord = xcoord, ycoord = ycoord,
    stratumID = stratumID, clusterID = clusterID, weight1 = weight1,
    xcoord1 = xcoord1, ycoord1 = ycoord1, sweight = sweight, sweight1 = sweight1,
    fpcsize = if (!is.null(fpc) && !clustered) "fpcsize" else NULL,
    Ncluster = if (!is.null(fpc) && clustered) "Ncluster" else NULL,
    stage1size = if (!is.null(fpc) && clustered) "stage1size" else NULL)
  checked <- input_check(dframe, dn, vars_cat, vars_cont, NULL, NULL,
    subpops, sizeweight, fpc, if (is.null(formula)) popsize else NULL,
    vartype, jointprob, conf, error_ind = FALSE, error_vec = NULL,
    preserve_factors = !is.null(formula), local_nbh = local_nbh)
  if (checked$error_ind) {
    error_vec <- NULL
    error_vec <- checked$error_vec
    error_vec <<- error_vec
    stop(paste(checked$error_vec, collapse = "\n"), call. = FALSE)
  }
  dframe <- checked$dframe
  vartype <- checked$vartype
  if (is.null(formula)) popsize <- checked$popsize
  if (stratified && is.null(formula)) {
    counts <- table(dframe[[stratumID]])
    singleton <- names(counts)[counts == 1L]
    if (length(singleton)) {
      dframe <- droplevels(dframe[!dframe[[stratumID]] %in% singleton, , drop = FALSE])
      warn_df <- contrast_warning(warn_df,
        paste("Strata with a single site:", paste(singleton, collapse = ", ")),
        "These strata were removed from the analysis.")
    }
  }
  design <- survey_design(dframe, siteID, weight, stratified, stratumID,
    clustered, clusterID, weight1, sizeweight, sweight, sweight1,
    !is.null(fpc), dn$fpcsize, dn$Ncluster, dn$stage1size,
    vartype, checked$jointprob)
  greg <- NULL
  if (!is.null(formula)) {
    greg <- greg_prepare(design, formula, popsize, subpopsize, checked$subpops, warn_df)
    warn_df <- greg$warn_df
  } else if (!is.null(popsize)) {
    if (all(class(popsize) %in% c("data.frame", "table", "xtabs"))) {
      columns <- if (is.data.frame(popsize)) names(popsize)[-ncol(popsize)] else names(dimnames(popsize))
      design <- survey::postStratify(design, make.formula(columns), popsize)
    } else {
      form <- make.formula(names(popsize))
      totals <- c(sum(popsize[[1]]), unlist(lapply(popsize, function(x) x[-1]), use.names = FALSE))
      names(totals) <- survey::cal_names(form, design)
      design <- survey::calibrate(design, form, totals)
    }
    if (vartype == "Local") {
      if (clustered) design$variables$wgt2 <- weights(design) / design$variables$wgt1 else
        design$variables$wgt <- weights(design)
    }
  }
  responses <- category_columns(design$variables, categories,
    c(unlist(dn), checked$subpops, checked$vars_cont, all.vars(formula),
      category_popsize_columns(popsize), "wgt", "wgt1", "wgt2"))
  design$variables <- responses$data
  list(design = design, design_names = dn, greg = greg, vartype = vartype,
    category_columns = responses$columns,
    vars_cat = checked$vars_cat, vars_cont = checked$vars_cont,
    subpops = checked$subpops, warn_df = warn_df)
}

contrast_warning <- function(warn_df, warning, action,
                              labels = rep(NA_character_, 3)) {
  rbind(warn_df, data.frame(func = I("contrast_analysis"),
    subpoptype = labels[1], subpop = labels[2], indicator = labels[3],
    stratum = NA_character_, warning = I(warning), action = I(action)))
}
