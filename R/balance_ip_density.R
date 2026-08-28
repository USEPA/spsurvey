###############################################################################
# Function: balance_ip_density, balance_ip_density_orig (not exported)
# Programmer: Michael Dumelle
# Date: August 28, 2026
#'
#' Compute, for one stratum, each frame unit's spatial-balance inclusion
#' probability density: probability per unit length/area for linear/areal
#' frames, or a straight probability for point frames and point-geometry
#' legacy sites. This is the quantity \code{sp_balance()} needs from the
#' frame, computed consistently
#' with the \code{ip}/\code{wgt} values \code{grts_stratum()}/
#' \code{irs_stratum()} attach to selected sites.
#'
#' \code{balframe} is the original frame for this stratum
#' (original \code{sframe} features, plus any appended legacy sites); for
#' linear/areal frames these are lines/polygons, while \code{sftmp} is the
#' dense-point approximation actually used to draw the sample. Every dense
#' point inherits its parent feature's \code{caty}/\code{aux} attributes
#' (unchanged by the join used to build \code{sftmp}), so per-dense-point
#' \code{ip_init} is constant within an original feature for every
#' \code{seltype}, and legacy sites
#' are rows within \code{sftmp} when \code{ip_init} was computed.
#'
#' @param sftmp The dense-point-level (or, for point frames,
#'   the frame) candidate frame for this stratum
#'
#' @param balframe The original frame for this stratum.
#'
#' @param sf_type The sample frame geometry type: \code{"sf_point"},
#'   \code{"sf_linear"}, or \code{"sf_area"}.
#'
#' @param seltype This stratum's selection type: \code{"equal"},
#'   \code{"unequal"}, or \code{"proportional"}.
#'
#' @param ip_step1 The step-1 (dense-point) inclusion probability for this
#'   stratum (\code{1} for point frames).
#'
#' @param n_base,n_total This stratum's base and total (base + over) sample
#'   sizes.
#'
#' @return A numeric vector the length of \code{balframe}: each row's
#'   inclusion-probability density, already scaled by \code{ip_step1} and
#'   the \code{n_base / n_total} oversample adjustment applied to selected
#'   sites, so it is directly comparable to a selected site's own \code{ip}.
#'
#' @noRd
###############################################################################
balance_ip_density <- function(sftmp, balframe, sf_type, seltype, ip_step1,
                                n_base, n_total) {
  mult <- (n_base / n_total) * ip_step1
  dens <- rep(NA_real_, nrow(balframe))

  if (sf_type == "sf_point") {
    # sftmp is already at original frame (legacy and non-legacy rows
    # alike), so every balframe row has an exact match by idpts.
    m <- match(balframe$idpts, sftmp$idpts)
    dens <- sftmp$ip_init[m]
  } else {
    # sftmp$legacy does not exist at all when this design has no legacy
    # sites anywhere (legacy_option == FALSE); treat every dense point as
    # non-legacy
    if ("legacy" %in% names(sftmp)) {
      sftmp_leg_ind <- sftmp$legacy
      sftmp_leg_ind[is.na(sftmp_leg_ind)] <- FALSE
    } else {
      sftmp_leg_ind <- rep(FALSE, nrow(sftmp))
    }

    is_leg <- balframe$legacy
    if (any(is_leg)) {
      sftmp_leg <- sftmp[sftmp_leg_ind, , drop = FALSE]
      m <- match(balframe$idpts[is_leg], sftmp_leg$idpts)
      dens[is_leg] <- sftmp_leg$ip_init[m]
    }
    if (any(!is_leg)) {
      sftmp_nonleg <- sftmp[!sftmp_leg_ind, , drop = FALSE]
      dens[!is_leg] <- balance_ip_density_orig(
        sftmp_nonleg,
        balframe$id[!is_leg], balframe$caty[!is_leg], balframe$aux[!is_leg],
        seltype, n_total
      )
    }
  }

  dens * mult
}

#' Map dense-point inclusion probabilities back to original (non-legacy)
#' frame features
#'
#' For \code{seltype = "equal"} this is a single stratum-wide constant. For
#' \code{"unequal"} and \code{"proportional"}, features that received at
#' least one dense point get that dense point's exact \code{ip_init}; \code{st_sample()}
#' can leave small features with zero dense points, and these fall back to
#' the \code{seltype} rule evaluated directly on the feature's own
#' \code{caty}/\code{aux}, using the same aggregates \code{grtspts_ip()}
#' already computed. For \code{"unequal"} this fallback is exact; for
#' \code{"proportional"} it is exact unless capping occurred in this
#' stratum
#'
#' @param sftmp_nonleg The dense-point-level frame for this stratum, with
#'   legacy rows already excluded.
#'
#' @param orig_id,orig_caty,orig_aux The \code{id}/\code{caty}/\code{aux}
#'   values of the original (non-legacy) frame features to compute a
#'   density for.
#'
#' @param seltype This stratum's selection type.
#'
#' @param n_total This stratum's total (base + over) sample size.
#'
#' @return A numeric vector the length of \code{orig_id}.
#'
#' @noRd
balance_ip_density_orig <- function(sftmp_nonleg, orig_id, orig_caty, orig_aux,
                                     seltype, n_total) {
  n <- length(orig_id)
  if (seltype == "equal") {
    return(rep(sftmp_nonleg$ip_init[1], n))
  }

  by_id <- tapply(sftmp_nonleg$ip_init, sftmp_nonleg$id, `[`, 1)
  dens <- unname(by_id[as.character(orig_id)])
  missing <- is.na(dens)
  if (!any(missing)) {
    return(dens)
  }

  if (seltype == "unequal") {
    by_caty <- tapply(sftmp_nonleg$ip_init, sftmp_nonleg$caty, `[`, 1)
    dens[missing] <- unname(by_caty[as.character(orig_caty[missing])])
  } else if (seltype == "proportional") {
    capped <- sftmp_nonleg$ip_init >= 1
    ngt1 <- sum(capped)
    if (ngt1 == 0) {
      dens[missing] <- n_total * orig_aux[missing] / sum(sftmp_nonleg$aux, na.rm = TRUE)
    } else {
      s_noncapped <- sum(sftmp_nonleg$aux[!capped], na.rm = TRUE)
      dens[missing] <- pmin(1, (n_total - ngt1) * orig_aux[missing] / s_noncapped)
    }
  }
  dens
}
