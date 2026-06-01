#' Resolve per-loan annual CPR from the configured prepayment model
#'
#' Internal helper. Returns a numeric vector of annual CPRs, one per row of
#' `data`, according to `cfg$prepay_model`. Decoupling CPR resolution from the
#' cash-flow engine lets the engine treat CPR as a plain per-loan input,
#' independent of how it was derived.
#'
#' @param data Validated loan data frame.
#' @param cfg Merged configuration list.
#' @param default_tier Fallback tier name (first name of cpr_vec).
#' @return Numeric vector of annual CPRs, length nrow(data).
#' @keywords internal
#' @noRd
resolve_prepay_cpr <- function(data, cfg, default_tier) {
  n <- nrow(data)

  # Per-loan tier (or default when no tier column)
  if (!is.null(cfg$col_tier) && cfg$col_tier %in% names(data)) {
    tier_raw <- as.character(data[[cfg$col_tier]])
  } else {
    tier_raw <- rep(default_tier, n)
  }

  if (identical(cfg$prepay_model, "tier_static")) {
    keys <- names(cfg$cpr_vec)
    tier_eff <- ifelse(tier_raw %in% keys, tier_raw, default_tier)
    return(unname(cfg$cpr_vec[tier_eff]))

  } else if (identical(cfg$prepay_model, "linear_incentive")) {
    stop(
      "prepay_model = 'linear_incentive' is not implemented yet ",
      "(arrives in v0.2.4 Step 2). Use prepay_model = 'tier_static' for now."
    )

  } else {
    stop(
      "Unknown prepay_model: '", cfg$prepay_model, "'. ",
      "Must be one of: 'tier_static', 'linear_incentive'."
    )
  }
}
