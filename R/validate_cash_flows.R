# Validation is separate from the projection engine: no rows are silently dropped.
validate_cash_flow_inputs <- function(data, config, allowed_keys) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("data must be a non-empty data frame.")
  }
  if (anyNA(names(data)) || any(!nzchar(trimws(names(data)))) ||
      anyDuplicated(names(data))) {
    stop("data column names must be non-empty and unique.")
  }
  if (!is.list(config) || is.data.frame(config)) {
    stop("config must be a named list.")
  }
  if (length(config) > 0L &&
      (is.null(names(config)) || anyNA(names(config)) ||
       any(!nzchar(trimws(names(config)))))) {
    stop("config contains unnamed element(s). Use name = value, not '<-'.")
  }
  if (anyDuplicated(names(config))) {
    stop("config contains duplicate keys: ",
         paste(unique(names(config)[duplicated(names(config))]), collapse = ", "))
  }
  unknown <- setdiff(names(config), allowed_keys)
  if (length(unknown) > 0L) {
    stop("Unknown config keys: ", paste(unknown, collapse = ", "))
  }
}

validate_cash_flow_vector <- function(x, label, lower = 0, upper = 1) {
  if (!is.numeric(x) || !is.null(dim(x)) || length(x) == 0L ||
      any(!is.finite(x))) {
    stop(label, " must be a non-empty finite numeric vector.")
  }
  if (is.null(names(x)) || anyNA(names(x)) ||
      any(!nzchar(trimws(names(x)))) || anyDuplicated(names(x))) {
    stop(label, " must have non-empty, unique tier names.")
  }
  if (any(x < lower | x > upper)) {
    stop(label, " values must be between ", lower, " and ", upper, ".")
  }
}

validate_cash_flow_config <- function(cfg) {
  required_maps <- c("col_balance", "col_rate", "col_term", "col_start_date")
  optional_maps <- c("col_loanid", "col_tier", "col_monthly_payment", "col_orig_balance")
  for (nm in c(required_maps, optional_maps)) {
    x <- cfg[[nm]]
    if (is.null(x) && nm %in% optional_maps) next
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
      stop(nm, " must be a single non-empty column name",
           if (nm %in% optional_maps) " or NULL" else "", ".")
    }
  }
  for (nm in c("interest_on_starting_balance", "credit_loss_reduces_interest",
               "reamortize_survivors", "return_monthly_totals", "show_progress")) {
    x <- cfg[[nm]]
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      stop(nm, " must be TRUE or FALSE.")
    }
  }
  for (nm in c("servicing_fee", "annual_reporting_fee", "investor_share",
               "origination_fee", "de_minimis_balance")) {
    x <- cfg[[nm]]
    if (!is.numeric(x) || length(x) != 1L || !is.null(dim(x)) || !is.finite(x)) {
      stop(nm, " must be a single finite numeric value.")
    }
    if (x < 0 || (nm != "de_minimis_balance" && x > 1)) {
      stop(nm, if (nm == "de_minimis_balance") " must be non-negative." else
        " must be between 0 and 1.")
    }
  }
  groups <- cfg$monthly_totals_group_vars
  if (!is.null(groups) && (!is.character(groups) || anyNA(groups) ||
      any(!nzchar(trimws(groups))) || anyDuplicated(groups))) {
    stop("monthly_totals_group_vars must be a character vector of unique, non-empty column names.")
  }
  if (!is.character(cfg$prepay_model) || length(cfg$prepay_model) != 1L ||
      is.na(cfg$prepay_model) || !cfg$prepay_model %in% c("tier_static", "linear_incentive")) {
    stop("prepay_model must be 'tier_static' or 'linear_incentive'.")
  }

  validate_cash_flow_vector(cfg$cpr_vec, "cpr_vec")
  validate_cash_flow_vector(cfg$credit_cost_vec, "credit_cost_vec")
  if (xor(is.null(cfg$pd_vec), is.null(cfg$lgd_vec))) {
    stop("pd_vec and lgd_vec must be supplied together.")
  }
  if (!is.null(cfg$pd_vec)) {
    validate_cash_flow_vector(cfg$pd_vec, "pd_vec")
    validate_cash_flow_vector(cfg$lgd_vec, "lgd_vec")
    if (!setequal(names(cfg$pd_vec), names(cfg$lgd_vec))) {
      stop("pd_vec and lgd_vec must cover the same set of tier names.")
    }
  }

  li_names <- c("base_cpr_vec", "beta_vec", "cpr_min_vec", "cpr_max_vec")
  for (nm in li_names) {
    if (is.null(cfg[[nm]])) {
      if (cfg$prepay_model == "linear_incentive") {
        stop("prepay_model = 'linear_incentive' requires ", nm, ".")
      }
    } else {
      validate_cash_flow_vector(cfg[[nm]], nm,
                                lower = if (nm == "beta_vec") -Inf else 0,
                                upper = if (nm == "beta_vec") Inf else 1)
    }
  }
  market <- cfg$current_market_rate
  if (!is.null(market) || cfg$prepay_model == "linear_incentive") {
    if (!is.numeric(market) || length(market) != 1L ||
        !is.null(dim(market)) || !is.finite(market)) {
      stop("current_market_rate must be a single finite numeric value.")
    }
  }
  if (cfg$prepay_model == "linear_incentive") {
    tiers <- names(cfg$base_cpr_vec)
    if (!all(vapply(cfg[li_names], function(x) setequal(names(x), tiers), logical(1)))) {
      stop("Linear-incentive vectors must cover the same set of tier names.")
    }
    if (any(cfg$cpr_min_vec[tiers] > cfg$cpr_max_vec[tiers])) {
      stop("cpr_min_vec must be <= cpr_max_vec for every tier.")
    }
    if (any(cfg$beta_vec < 0)) {
      warning("Negative beta_vec: prepayment falls as the rate incentive rises. Proceeding as specified.")
    }
  }
  if (cfg$reamortize_survivors && !cfg$interest_on_starting_balance) {
    warning("interest_on_starting_balance = FALSE is ignored when reamortize_survivors = TRUE.")
  }
  cfg
}

validate_cash_flow_data <- function(data, cfg) {
  ids <- data[[cfg$col_loanid]]
  if (!(is.character(ids) || is.factor(ids) || is.numeric(ids)) ||
      !is.null(dim(ids)) || anyNA(ids) ||
      any(!nzchar(trimws(as.character(ids)))) ||
      (is.numeric(ids) && any(!is.finite(ids)))) {
    stop("Loan IDs in '", cfg$col_loanid, "' must be non-missing, non-empty character or finite numeric values.")
  }
  if (anyDuplicated(ids)) {
    stop("Duplicate loan IDs in '", cfg$col_loanid,
         "'. Supply one row per loan; duplicates include: ",
         paste(utils::head(unique(ids[duplicated(ids)]), 5), collapse = ", "))
  }
  for (nm in c("col_balance", "col_rate", "col_term", "col_monthly_payment", "col_orig_balance")) {
    col <- cfg[[nm]]
    if (is.null(col)) next
    x <- data[[col]]
    optional <- nm %in% c("col_monthly_payment", "col_orig_balance")
    if (!is.numeric(x) || !is.null(dim(x))) {
      stop("Column '", col, "' must be numeric.")
    }
    bad <- !is.finite(x)
    # NA optional values request the established per-loan calculated fallback.
    if (optional) bad <- bad & !(is.na(x) & !is.nan(x))
    if (any(bad)) {
      stop("Column '", col, "' contains missing or non-finite values at row(s): ",
           paste(utils::head(which(bad), 5), collapse = ", "))
    }
    if (nm == "col_rate") {
      bad <- x < 0
    } else {
      bad <- !is.na(x) & x <= 0
    }
    if (nm == "col_term") bad <- bad | x != floor(x) | x > .Machine$integer.max
    if (any(bad)) {
      stop("Column '", col, "' must contain ",
           if (nm == "col_rate") "non-negative rates" else
             if (nm == "col_term") "positive integer terms" else "positive values",
           "; invalid row(s): ", paste(utils::head(which(bad), 5), collapse = ", "))
    }
  }
  col <- cfg$col_start_date
  dates <- tryCatch(as.Date(data[[col]]), error = function(e) {
    stop("Column '", col, "' cannot be converted to Date format.")
  })
  if (length(dates) != nrow(data) || any(!is.finite(as.numeric(dates)))) {
    stop("Column '", col, "' must contain valid, non-missing dates.")
  }
  data[[col]] <- dates
  if (!is.null(cfg$col_tier)) {
    tiers <- data[[cfg$col_tier]]
    if (!(is.character(tiers) || is.factor(tiers) || is.numeric(tiers)) || !is.null(dim(tiers))) {
      stop("Column '", cfg$col_tier, "' must contain character, factor, or numeric tiers.")
    }
  }
  rates <- data[[cfg$col_rate]]
  if (any(rates > 1)) {
    warning("Some interest rates are greater than 1. Rates must be decimals ",
            "(e.g., 0.0599 for 5.99%); verify your inputs.")
  }
  if (cfg$servicing_fee > stats::median(rates)) {
    warning("servicing_fee is greater than median portfolio rate. Please verify.")
  }
  data
}
