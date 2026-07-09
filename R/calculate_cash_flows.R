# Global variables for NSE in dplyr
utils::globalVariables(c(
  "starting_balance", "adjusted_balance", "scheduled_payment",
  "gross_interest", "servicing_fee_amt", "reporting_fee_amt", "total_fees",
  "scheduled_principal", "prepayment", "total_principal", "credit_loss",
  "remaining_balance", "net_interest", "total_payment", "investor_principal",
  "investor_interest", "investor_total", "orig_fee", "net_interest_raw",
  "date", "."
))

#' Calculate Loan Portfolio Cash Flows
#'
#' Generates monthly cash flow projections for a loan portfolio, incorporating
#' user-defined prepayment speeds (CPR), credit costs, and various fee structures.
#'
#' @param data A data frame containing loan portfolio snapshot data
#' @param config A list of configuration parameters. See details for structure.
#'
#' @return Depending on return_monthly_totals setting:
#'   \itemize{
#'     \item If FALSE: A data frame with loan-level cash flows containing columns:
#'       LOAN_ID, eff_date, rate, tier, month, date (payment date; first payment
#'       falls one month after eff_date, which is the t=0 valuation anchor), starting_balance, adjusted_balance, accrual_balance,
#'       scheduled_payment, gross_interest, servicing_fee_amt, reporting_fee_amt,
#'       total_fees, scheduled_principal, prepayment, total_principal, credit_loss,
#'       remaining_balance, orig_fee, net_interest, total_payment, investor_principal,
#'       investor_interest, investor_total
#'     \item If TRUE: A list with two elements:
#'       \itemize{
#'         \item loan_cash_flows: Detailed loan-level cash flows (data frame)
#'         \item monthly_totals: Aggregated monthly totals across all loans (data frame)
#'       }
#'   }
#'
#' @details
#' Configuration list structure:
#' \itemize{
#'   \item col_loanid: Column name for loan identifier (default: "LOAN_ID")
#'   \item col_balance: Column name for current balance (default: "balance")
#'   \item col_rate: Column name for interest rate as decimal (default: "current_interest_rate")
#'   \item col_term: Column name for months to maturity (default: "months_to_maturity")
#'   \item col_start_date: Column name for effective date (default: "eff_date")
#'   \item col_tier: Column name for loan tier (default: NULL, optional)
#'   \item col_monthly_payment: Column name for monthly payment (default: NULL, optional)
#'   \item col_orig_balance: Column name for original balance (default: NULL, optional)
#'   \item servicing_fee: Annual servicing fee rate as decimal (default: 0.0025)
#'   \item annual_reporting_fee: Annual reporting fee rate as decimal (default: 0.00)
#'   \item investor_share: Investor share percentage (default: 1.0 for 100%)
#'   \item origination_fee: Origination fee rate (default: 0.0000)
#'   \item interest_on_starting_balance: Calculate interest on the starting
#'     balance, before prepayments and credit losses are removed (default: TRUE).
#'     TRUE reflects standard monthly-pay consumer loan servicing: borrowers owe
#'     a full month of interest on the balance outstanding at the start of the
#'     period, including balances that pay off during the month. Set FALSE to
#'     accrue interest on the post-prepay/post-loss balance (non-standard,
#'     conservative).
#'   \item credit_loss_reduces_interest: Whether credit losses are also deducted
#'     from interest cash flows (default: FALSE). FALSE applies losses through
#'     principal balance reduction only. TRUE additionally deducts charge-offs
#'     from interest, which double-counts the loss and is retained only for
#'     backward compatibility.
#'   \item de_minimis_balance: Threshold below which balance is forced to zero (default: 1.00)
#'   \item cpr_vec: Named vector of CPR rates by tier (default: c("default" = 0.0))
#'   \item credit_cost_vec: Named vector of annual credit cost rates by tier (default: c("default" = 0.0))
#'   \item pd_vec: Optional named vector of PD rates (overrides credit_cost_vec if provided)
#'   \item lgd_vec: Optional named vector of LGD rates (overrides credit_cost_vec if provided)
#'   \item return_monthly_totals: Whether to return aggregated monthly totals (default: FALSE)
#'   \item monthly_totals_group_vars: Optional character vector of column names to group monthly totals by,
#'     in addition to date (default: NULL). For example, c("tier") to see monthly totals by tier.
#'   \item show_progress: Show progress messages for large portfolios (default: TRUE)
#'   \item prepay_model: "tier_static" (default) or "linear_incentive"
#'   \item current_market_rate: Scalar market rate (decimal) for linear_incentive;
#'     rate_incentive = coupon - current_market_rate, coupon = col_rate (gross)
#'   \item base_cpr_vec: Tier-keyed intercept CPR (linear_incentive)
#'   \item beta_vec: Tier-keyed CPR sensitivity per decimal of rate incentive (linear_incentive)
#'   \item cpr_min_vec, cpr_max_vec: Tier-keyed clamp bounds on CPR (linear_incentive)
#'
#' }
#'
#' @importFrom dplyr bind_rows group_by summarise mutate if_else select left_join distinct across all_of arrange
#' @importFrom purrr pmap
#' @importFrom lubridate %m+%
#' @importFrom tibble tibble
#' @importFrom stats setNames median
#' @importFrom utils head
#'
#' @examples
#' \dontrun{
#' # Create sample loan portfolio
#' loan_data <- data.frame(
#'   LOAN_ID = c("L001", "L002", "L003"),
#'   balance = c(25000, 50000, 15000),
#'   current_interest_rate = c(0.0599, 0.0649, 0.0549),
#'   months_to_maturity = c(60, 48, 36),
#'   eff_date = as.Date("2025-01-01")
#' )
#'
#' # Define CPR and credit cost assumptions
#' config <- list(
#'   cpr_vec = c("default" = 0.05),
#'   credit_cost_vec = c("default" = 0.01),
#'   servicing_fee = 0.0025
#' )
#'
#' # Generate cash flows
#' cash_flows <- calculate_cash_flows(loan_data, config)
#'
#' # With monthly totals for portfolio yield calculation
#' config_totals <- list(
#'   cpr_vec = c("default" = 0.05),
#'   credit_cost_vec = c("default" = 0.01),
#'   return_monthly_totals = TRUE
#' )
#'
#' results <- calculate_cash_flows(loan_data, config_totals)
#'
#' # Calculate portfolio yield
#' library(FinCal)
#' pool_cfs <- data.frame(
#'   date = results$monthly_totals$date,
#'   amount = results$monthly_totals$investor_total
#' )
#'
#' portfolio_yield <- yield.actual(
#'   cf = pool_cfs,
#'   pv = sum(loan_data$balance),
#'   start_date = min(loan_data$eff_date),
#'   compounding = "monthly"
#' )
#' }
#'
#' @export
calculate_cash_flows <- function(data, config = list()) {

  # Default configuration
  default_config <- list(
    # Required column mappings
    col_loanid = "LOAN_ID",
    col_balance = "balance",
    col_rate = "current_interest_rate",
    col_term = "months_to_maturity",
    col_start_date = "eff_date",

    # Optional column mappings
    col_tier = NULL,
    col_monthly_payment = NULL,
    col_orig_balance = NULL,

    # Parameter defaults
    servicing_fee = 0.0025,
    annual_reporting_fee = 0.00,
    investor_share = 1.0,
    origination_fee = 0.0000,
    interest_on_starting_balance = TRUE,
    credit_loss_reduces_interest = FALSE,
    de_minimis_balance = 1.00,

    # Prepayment model: "tier_static" (default) or "linear_incentive"
    prepay_model = "tier_static",

    # CPR and credit cost vectors (all zeros by default)
    cpr_vec = c("default" = 0.0),
    credit_cost_vec = c("default" = 0.0),
    # Linear-incentive model parameters (tier-keyed; used when
    # prepay_model = "linear_incentive")
    current_market_rate = NULL,
    base_cpr_vec = NULL,
    beta_vec     = NULL,
    cpr_min_vec  = NULL,
    cpr_max_vec  = NULL,

    # Optional PD/LGD approach (overrides credit_cost_vec if provided)
    pd_vec = NULL,
    lgd_vec = NULL,

    # Output options
    return_monthly_totals = FALSE,
    monthly_totals_group_vars = NULL,
    show_progress = TRUE
  )

  # Merge user config with defaults
  cfg <- modifyList(default_config, config)

  # Validate configuration
  validate_config(cfg, data)

  # Validate prepay_model
  if (!cfg$prepay_model %in% c("tier_static", "linear_incentive")) {
    stop("prepay_model must be 'tier_static' or 'linear_incentive'. Got: '",
         cfg$prepay_model, "'")
  }

  # Validate required columns exist
  required_cols <- c(cfg$col_balance, cfg$col_rate, cfg$col_term, cfg$col_start_date)
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in data: ", paste(missing_cols, collapse = ", "),
      "\n\nExpected columns based on config:",
      "\n  - Balance: '", cfg$col_balance, "'",
      "\n  - Rate: '", cfg$col_rate, "'",
      "\n  - Term: '", cfg$col_term, "'",
      "\n  - Start Date: '", cfg$col_start_date, "'",
      "\n\nPlease check your column names or update the config parameter."
    )
  }

  # Add LOAN_ID if missing
  if (is.null(cfg$col_loanid) || !cfg$col_loanid %in% names(data)) {
    data$LOAN_ID <- seq_len(nrow(data))
    cfg$col_loanid <- "LOAN_ID"
  }

  # Validate and clean data (returns mutated data frame)
  data <- validate_data(data, cfg)

  # Check if tier column exists
  has_tier <- !is.null(cfg$col_tier) && cfg$col_tier %in% names(data)

  # If PD and LGD provided, calculate credit costs
  if (!is.null(cfg$pd_vec) && !is.null(cfg$lgd_vec)) {
    if (!is.null(config$credit_cost_vec)) {
      warning(
        "Both credit_cost_vec and pd_vec/lgd_vec provided. ",
        "Using PD/LGD approach (credit_cost = PD * LGD). ",
        "The credit_cost_vec will be ignored."
      )
    }
    # Override credit_cost_vec with PD * LGD
    tier_names <- names(cfg$pd_vec)
    cfg$credit_cost_vec <- setNames(
      cfg$pd_vec * cfg$lgd_vec,
      tier_names
    )
  }

  # First tier as default. For linear_incentive the prepay tiers live in
  # base_cpr_vec rather than cpr_vec.
  default_tier <- if (cfg$prepay_model == "linear_incentive") {
    names(cfg$base_cpr_vec)[1]
  } else {
    names(cfg$cpr_vec)[1]
  }
  prepay_tier_names <- if (cfg$prepay_model == "linear_incentive") {
    names(cfg$base_cpr_vec)
  } else {
    names(cfg$cpr_vec)
  }

  # Credit costs must cover the default tier, or the engine's tier fallback
  # would fail (credit cost is looked up by tier regardless of prepay model).
  if (!default_tier %in% names(cfg$credit_cost_vec)) {
    stop("Default tier '", default_tier, "' is not present in credit_cost_vec ",
         "(or the derived PD*LGD vector). Define credit cost for all tiers.")
  }

  # Validate tier values if tier column exists
  if (has_tier) {
    unique_tiers <- unique(data[[cfg$col_tier]])
    missing_tiers <- setdiff(unique_tiers, prepay_tier_names)

    if (length(missing_tiers) > 0) {
      warning(
        "The following tier values are not in cpr_vec/credit_cost_vec: ",
        paste(missing_tiers, collapse = ", "),
        ". Using first tier '", default_tier, "' as default for these loans."
      )
    }
  }

  # Resolve per-loan annual CPR via the configured prepayment model.
  # Keystone for v0.2.4: CPR becomes a per-loan input to the engine.
  cpr_per_loan <- resolve_prepay_cpr(data, cfg, default_tier)

  # Show progress for large portfolios
  if (cfg$show_progress && nrow(data) > 1000) {
    message("Processing ", format(nrow(data), big.mark = ","), " loans...")
  }

  # Apply cash flow generation to each loan
  cash_flows_list <- purrr::pmap(
    list(
      loan_id = data[[cfg$col_loanid]],
      principal = data[[cfg$col_balance]],
      rate = data[[cfg$col_rate]],
      term = data[[cfg$col_term]],
      start_date = data[[cfg$col_start_date]],
      tier = if (has_tier) data[[cfg$col_tier]] else default_tier,
      cpr = cpr_per_loan,
      monthly_payment_val = if (!is.null(cfg$col_monthly_payment)) data[[cfg$col_monthly_payment]] else NA_real_,
      origbalance = if (!is.null(cfg$col_orig_balance)) data[[cfg$col_orig_balance]] else NA_real_
    ),
    .f = generate_single_loan_cash_flow,
    cfg = cfg,
    default_tier = default_tier
  )

  # Bind all loan cash flows
  loan_cash_flows <- dplyr::bind_rows(cash_flows_list)

  # Join grouping columns back if needed (simpler than passing through pmap)
  if (!is.null(cfg$monthly_totals_group_vars)) {
    group_cols_available <- intersect(cfg$monthly_totals_group_vars, names(data))
    # Only join columns that don't already exist in loan_cash_flows
    cols_to_join <- setdiff(group_cols_available, names(loan_cash_flows))

    if (length(cols_to_join) > 0) {
      lookup <- data[, c(cfg$col_loanid, cols_to_join), drop = FALSE]
      names(lookup)[1] <- "LOAN_ID"
      loan_cash_flows <- dplyr::left_join(
        loan_cash_flows,
        dplyr::distinct(lookup),  # Ensure unique loan IDs
        by = "LOAN_ID"
      )
    }
  }

  if (cfg$show_progress && nrow(data) > 1000) {
    message("Cash flows generated successfully: ",
            format(nrow(loan_cash_flows), big.mark = ","), " monthly payments")
  }

  # Return based on monthly_totals flag
  if (cfg$return_monthly_totals) {

    # Build grouping variables - always include date
    group_cols <- c("date", cfg$monthly_totals_group_vars)

    # Validate group_vars exist in loan_cash_flows
    if (!is.null(cfg$monthly_totals_group_vars)) {
      missing_group_cols <- setdiff(cfg$monthly_totals_group_vars, names(loan_cash_flows))
      if (length(missing_group_cols) > 0) {
        warning(
          "The following monthly_totals_group_vars are not in cash flows data: ",
          paste(missing_group_cols, collapse = ", "),
          ". They will be ignored."
        )
        group_cols <- intersect(group_cols, names(loan_cash_flows))
      }
    }

    monthly_totals <- loan_cash_flows %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(
        starting_balance = sum(starting_balance, na.rm = TRUE),
        adjusted_balance = sum(adjusted_balance, na.rm = TRUE),
        scheduled_payment = sum(scheduled_payment, na.rm = TRUE),
        gross_interest = sum(gross_interest, na.rm = TRUE),
        servicing_fee_amt = sum(servicing_fee_amt, na.rm = TRUE),
        reporting_fee_amt = sum(reporting_fee_amt, na.rm = TRUE),
        total_fees = sum(total_fees, na.rm = TRUE),
        scheduled_principal = sum(scheduled_principal, na.rm = TRUE),
        prepayment = sum(prepayment, na.rm = TRUE),
        total_principal = sum(total_principal, na.rm = TRUE),
        credit_loss = sum(credit_loss, na.rm = TRUE),
        remaining_balance = sum(remaining_balance, na.rm = TRUE),
        net_interest = sum(net_interest, na.rm = TRUE),
        total_payment = sum(total_payment, na.rm = TRUE),
        investor_principal = sum(investor_principal, na.rm = TRUE),
        investor_interest = sum(investor_interest, na.rm = TRUE),
        investor_total = sum(investor_total, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(group_cols)))  # Ensure sorted by date + groups

    return(list(
      loan_cash_flows = loan_cash_flows,
      monthly_totals = monthly_totals
    ))
  } else {
    return(loan_cash_flows)
  }
}


# Validation helper function
validate_config <- function(cfg, data) {

  # Check that vectors have matching names if both PD and LGD provided
  if (!is.null(cfg$pd_vec) && !is.null(cfg$lgd_vec)) {
    if (!identical(names(cfg$pd_vec), names(cfg$lgd_vec))) {
      stop(
        "pd_vec and lgd_vec must have identical tier names.\n",
        "pd_vec tiers: ", paste(names(cfg$pd_vec), collapse = ", "), "\n",
        "lgd_vec tiers: ", paste(names(cfg$lgd_vec), collapse = ", ")
      )
    }
  }

  # Validate prepay_model
  if (!cfg$prepay_model %in% c("tier_static", "linear_incentive")) {
    stop("prepay_model must be 'tier_static' or 'linear_incentive'. Got: '",
         cfg$prepay_model, "'")
  }

  # Prepayment-model-specific validation
  if (cfg$prepay_model == "tier_static") {

    # Check that cpr_vec and credit_cost_vec have matching names
    if (!identical(names(cfg$cpr_vec), names(cfg$credit_cost_vec))) {
      warning(
        "cpr_vec and credit_cost_vec have different tier names. ",
        "This may cause unexpected behavior. ",
        "Ensure both vectors contain the same tier values."
      )
    }

    # Enforce "default" tier when col_tier is NULL
    if (is.null(cfg$col_tier) || !cfg$col_tier %in% names(data)) {
      if (!"default" %in% names(cfg$cpr_vec)) {
        stop(
          "When col_tier is NULL or tier column is missing, ",
          "cpr_vec and credit_cost_vec must contain a 'default' tier.\n",
          "Current cpr_vec tiers: ", paste(names(cfg$cpr_vec), collapse = ", ")
        )
      }
    }

  } else if (cfg$prepay_model == "linear_incentive") {

    li <- list(base_cpr_vec = cfg$base_cpr_vec, beta_vec = cfg$beta_vec,
               cpr_min_vec = cfg$cpr_min_vec, cpr_max_vec = cfg$cpr_max_vec)
    missing <- names(li)[vapply(li, is.null, logical(1))]
    if (length(missing) > 0) {
      stop("prepay_model = 'linear_incentive' requires: ",
           paste(missing, collapse = ", "), ".")
    }
    if (is.null(cfg$current_market_rate) || !is.numeric(cfg$current_market_rate) ||
        length(cfg$current_market_rate) != 1L || !is.finite(cfg$current_market_rate)) {
      stop("prepay_model = 'linear_incentive' requires a single finite numeric ",
           "current_market_rate.")
    }
    tiers <- names(cfg$base_cpr_vec)
    if (!all(vapply(li, function(v) setequal(names(v), tiers), logical(1)))) {
      stop("base_cpr_vec, beta_vec, cpr_min_vec, and cpr_max_vec must cover the ",
           "same set of tier names.")
    }
    if (is.null(cfg$col_tier) || !cfg$col_tier %in% names(data)) {
      if (!"default" %in% tiers) {
        stop("When col_tier is NULL, the linear_incentive vectors must contain a ",
             "'default' tier. Current tiers: ", paste(tiers, collapse = ", "))
      }
    }
    base <- cfg$base_cpr_vec[tiers]; beta <- cfg$beta_vec[tiers]
    cmin <- cfg$cpr_min_vec[tiers];  cmax <- cfg$cpr_max_vec[tiers]
    if (any(base < 0) || any(base > 1)) {
      stop("All base_cpr_vec values must be between 0 and 1.")
    }
    if (any(cmin < 0) || any(cmax > 1)) {
      stop("Require 0 <= cpr_min_vec and cpr_max_vec <= 1 for every tier.")
    }
    if (any(cmin > cmax)) {
      stop("cpr_min_vec must be <= cpr_max_vec for every tier. Violations: ",
           paste(tiers[cmin > cmax], collapse = ", "))
    }
    if (any(beta < 0)) {
      warning("Negative beta_vec for tier(s): ", paste(tiers[beta < 0], collapse = ", "),
              ". A negative beta means prepayment falls as the rate incentive rises, ",
              "which is economically unusual. Proceeding as specified.")
    }
  }

  # Validate parameter ranges
  if (cfg$servicing_fee < 0 || cfg$servicing_fee > 1) {
    stop("servicing_fee must be between 0 and 1 (as a decimal). Got: ", cfg$servicing_fee)
  }
  if (cfg$annual_reporting_fee < 0 || cfg$annual_reporting_fee > 1) {
    stop("annual_reporting_fee must be between 0 and 1 (as a decimal). Got: ", cfg$annual_reporting_fee)
  }
  if (cfg$investor_share < 0 || cfg$investor_share > 1) {
    stop("investor_share must be between 0 and 1. Got: ", cfg$investor_share)
  }
  if (cfg$de_minimis_balance < 0) {
    stop("de_minimis_balance must be non-negative. Got: ", cfg$de_minimis_balance)
  }

  # Validate CPR values (should be between 0 and 1)
  if (any(cfg$cpr_vec < 0) || any(cfg$cpr_vec > 1)) {
    stop("All CPR values must be between 0 and 1 (as decimals). Check cpr_vec.")
  }

  # Validate credit cost values (should be between 0 and 1)
  if (any(cfg$credit_cost_vec < 0) || any(cfg$credit_cost_vec > 1)) {
    stop("All credit cost values must be between 0 and 1 (as decimals). Check credit_cost_vec.")
  }

  # Validate monthly_totals_group_vars
  if (!is.null(cfg$monthly_totals_group_vars)) {
    if (!is.character(cfg$monthly_totals_group_vars)) {
      stop("monthly_totals_group_vars must be a character vector of column names.")
    }
  }

  invisible(TRUE)
}


# Data validation helper function - RETURNS mutated data
validate_data <- function(data, cfg) {

  # Check for NA values in required columns
  na_checks <- list(
    balance = sum(is.na(data[[cfg$col_balance]])),
    rate = sum(is.na(data[[cfg$col_rate]])),
    term = sum(is.na(data[[cfg$col_term]])),
    start_date = sum(is.na(data[[cfg$col_start_date]]))
  )

  na_found <- na_checks[na_checks > 0]

  if (length(na_found) > 0) {
    warning(
      "NA values found in required columns:\n",
      paste(sprintf("  - %s: %d NA values", names(na_found), unlist(na_found)), collapse = "\n"),
      "\nLoans with NA values will be skipped."
    )
  }

  # Validate balance values
  if (any(data[[cfg$col_balance]] <= 0, na.rm = TRUE)) {
    n_invalid <- sum(data[[cfg$col_balance]] <= 0, na.rm = TRUE)
    warning(
      n_invalid, " loans have balance <= 0. These loans will be skipped."
    )
  }

  # Validate rate values (should be between 0 and 1 for decimal rates)
  rates <- data[[cfg$col_rate]]
  if (any(rates > 1, na.rm = TRUE)) {
    warning(
      "Some interest rates are greater than 1. ",
      "Rates should be specified as decimals (e.g., 0.0599 for 5.99%), not percentages. ",
      "Please verify your rate values."
    )
  }

  if (any(rates < 0, na.rm = TRUE)) {
    n_negative <- sum(rates < 0, na.rm = TRUE)
    warning(n_negative, " loans have negative interest rates. These loans will be skipped.")
  }

  # Validate term values
  if (any(data[[cfg$col_term]] <= 0, na.rm = TRUE)) {
    n_invalid <- sum(data[[cfg$col_term]] <= 0, na.rm = TRUE)
    warning(n_invalid, " loans have term <= 0. These loans will be skipped.")
  }

  # Check for unreasonably high servicing fees relative to rates
  if (!is.null(cfg$servicing_fee) && cfg$servicing_fee > 0) {
    median_rate <- median(rates, na.rm = TRUE)
    if (cfg$servicing_fee > median_rate) {
      warning(
        "servicing_fee (", sprintf("%.4f", cfg$servicing_fee), ") is greater than median portfolio rate (",
        sprintf("%.4f", median_rate), "). This may result in negative net interest. Please verify."
      )
    }
  }

  # Validate and convert date column - PERSIST THE CONVERSION (CHANGE 4: Remove message)
  if (!inherits(data[[cfg$col_start_date]], "Date")) {
    tryCatch({
      data[[cfg$col_start_date]] <- as.Date(data[[cfg$col_start_date]])
    }, error = function(e) {
      stop(
        "Column '", cfg$col_start_date, "' cannot be converted to Date format. ",
        "Please ensure it contains valid dates."
      )
    })
  }

  # Return the mutated data frame
  return(data)
}


# Internal function to generate cash flows for a single loan
generate_single_loan_cash_flow <- function(loan_id,
                                           principal,
                                           rate,
                                           term,
                                           start_date,
                                           tier,
                                           cpr,
                                           monthly_payment_val,
                                           origbalance,
                                           cfg,
                                           default_tier) {

  # Input validation - return empty if invalid
  if (is.na(principal) || is.na(rate) || is.na(term) || is.na(start_date) ||
      principal <= 0 || rate < 0 || term <= 0) {
    return(tibble::tibble())
  }

  # Convert and validate start_date
  start_date <- as.Date(start_date)
  if (is.na(start_date)) {
    return(tibble::tibble())
  }

  # Set origbalance if not provided
  if (is.na(origbalance)) {
    origbalance <- principal
  }

  # Tier is used for credit-cost lookup and labeling; CPR is pre-resolved.
  if (!tier %in% names(cfg$credit_cost_vec)) {
    tier <- default_tier
  }

  # Monthly rates and factors
  monthly_rate        <- rate / 12
  monthly_servicing   <- cfg$servicing_fee / 12
  monthly_reporting   <- cfg$annual_reporting_fee / 12
  monthly_credit_cost <- cfg$credit_cost_vec[[tier]] / 12
  monthly_smm         <- 1 - (1 - cpr)^(1 / 12)   # SMM (CPR pre-resolved per loan)

  # Scheduled payment calculation
  use_monthly_payment <- !is.na(monthly_payment_val) && monthly_payment_val > 0
  if (use_monthly_payment) {
    scheduled_payment <- monthly_payment_val
  } else {
    if (monthly_rate == 0) {
      scheduled_payment <- principal / term
    } else {
      scheduled_payment <- principal * monthly_rate / (1 - (1 + monthly_rate)^(-term))
    }
  }

  # Pre-computed payment dates: first payment one month AFTER the as-of date
  # (start_date is the settlement/valuation anchor, t = 0; payments are t = 1..term).
  # Uses lubridate month arithmetic rather than seq.Date to handle month-ends:
  # seq.Date from Jan 31 rolls to Mar 3; %m+% months() clamps to Feb 28/29.
  all_dates <- start_date %m+% months(seq_len(term))

  # Pre-allocated atomic accumulators (one slot per scheduled month)
  starting_balance_v    <- numeric(term)
  adjusted_balance_v    <- numeric(term)
  accrual_balance_v     <- numeric(term)
  gross_interest_v      <- numeric(term)
  servicing_fee_v       <- numeric(term)
  reporting_fee_v       <- numeric(term)
  total_fees_v          <- numeric(term)
  scheduled_principal_v <- numeric(term)
  prepayment_v          <- numeric(term)
  total_principal_v     <- numeric(term)
  credit_loss_v         <- numeric(term)
  remaining_balance_v   <- numeric(term)

  balance     <- principal
  min_balance <- 0.01
  n_months    <- 0L

  # Sequential balance roll-forward (each month depends on the prior)
  i <- 1L
  while (i <= term && balance >= min_balance) {
    sb <- balance

    # Cap credit loss at 100% of starting balance
    cl <- min(sb * monthly_credit_cost, sb)

    # Prepayment capped at balance available after credit loss
    max_prepay <- max(0, sb - cl)
    pp <- min(sb * monthly_smm, max_prepay)

    # Balance after prepayment and credit loss
    ab <- max(0, sb - pp - cl)

    # Balance used for interest and fees
    acc <- if (cfg$interest_on_starting_balance) sb else ab

    gi <- acc * monthly_rate
    sf <- acc * monthly_servicing
    rf <- acc * monthly_reporting
    tf <- sf + rf

    # Scheduled principal
    sp <- max(0, scheduled_payment - gi)
    sp <- min(sp, ab)

    # Total principal returned
    tp <- min(sp + pp, ab)

    # Ending balance before de minimis adjustment
    rb <- max(0, sb - tp - cl)

    # De minimis handling
    if (rb > 0 && rb < cfg$de_minimis_balance) {
      tp <- tp + rb
      rb <- 0
    }

    starting_balance_v[i]    <- sb
    adjusted_balance_v[i]    <- ab
    accrual_balance_v[i]     <- acc
    gross_interest_v[i]      <- gi
    servicing_fee_v[i]       <- sf
    reporting_fee_v[i]       <- rf
    total_fees_v[i]          <- tf
    scheduled_principal_v[i] <- sp
    prepayment_v[i]          <- pp
    total_principal_v[i]     <- tp
    credit_loss_v[i]         <- cl
    remaining_balance_v[i]   <- rb

    n_months <- i
    balance  <- rb
    i        <- i + 1L
    if (balance == 0) break
  }

  if (n_months == 0L) {
    return(tibble::tibble())
  }

  idx <- seq_len(n_months)

  # Origination fee (constant per loan, using ORIGINAL term)
  orig_fee <- if (term > 0 && cfg$origination_fee > 0) {
    (cfg$origination_fee * origbalance) / term
  } else {
    0
  }

  gi <- gross_interest_v[idx]
  tf <- total_fees_v[idx]
  tp <- total_principal_v[idx]
  cl <- credit_loss_v[idx]

  # Net interest by accounting treatment
  net_interest_raw <- if (cfg$credit_loss_reduces_interest) {
    gi - tf - orig_fee - cl
  } else {
    gi - tf - orig_fee
  }
  net_interest       <- pmax(net_interest_raw, 0)
  orig_fee_absorbed  <- dplyr::if_else(net_interest_raw < 0,
                                       orig_fee + net_interest_raw, orig_fee)
  total_payment      <- gi + tp
  investor_principal <- tp * cfg$investor_share
  investor_interest  <- net_interest * cfg$investor_share
  investor_total     <- investor_principal + investor_interest

  # Single tibble construction (column order matches v0.2.3 output exactly)
  tibble::tibble(
    LOAN_ID             = loan_id,
    eff_date            = start_date,
    rate                = rate,
    tier                = tier,
    month               = as.numeric(idx),
    date                = all_dates[idx],
    starting_balance    = starting_balance_v[idx],
    adjusted_balance    = adjusted_balance_v[idx],
    accrual_balance     = accrual_balance_v[idx],
    scheduled_payment   = scheduled_payment,
    gross_interest      = gi,
    servicing_fee_amt   = servicing_fee_v[idx],
    reporting_fee_amt   = reporting_fee_v[idx],
    total_fees          = tf,
    scheduled_principal = scheduled_principal_v[idx],
    prepayment          = prepayment_v[idx],
    total_principal     = tp,
    credit_loss         = cl,
    remaining_balance   = remaining_balance_v[idx],
    orig_fee            = orig_fee,
    net_interest_raw    = net_interest_raw,
    net_interest        = net_interest,
    orig_fee_absorbed   = orig_fee_absorbed,
    total_payment       = total_payment,
    investor_principal  = investor_principal,
    investor_interest   = investor_interest,
    investor_total      = investor_total
  )
}
