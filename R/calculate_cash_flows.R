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
#'       LOAN_ID, eff_date, rate, tier, month, date, starting_balance, adjusted_balance, accrual_balance,
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
#'   \item interest_on_starting_balance: Calculate interest before or after prepay/losses (default: FALSE)
#'   \item credit_loss_reduces_interest: Whether credit losses reduce interest cash flows (default: TRUE).
#'     By default, credit losses are applied against investor cash flows before distribution to investors.
#'     When set to FALSE, credit losses reduce principal balances only and do not directly reduce interest cash flows.
#'   \item de_minimis_balance: Threshold below which balance is forced to zero (default: 1.00)
#'   \item cpr_vec: Named vector of CPR rates by tier (default: c("default" = 0.0))
#'   \item credit_cost_vec: Named vector of annual credit cost rates by tier (default: c("default" = 0.0))
#'   \item pd_vec: Optional named vector of PD rates (overrides credit_cost_vec if provided)
#'   \item lgd_vec: Optional named vector of LGD rates (overrides credit_cost_vec if provided)
#'   \item return_monthly_totals: Whether to return aggregated monthly totals (default: FALSE)
#'   \item monthly_totals_group_vars: Optional character vector of column names to group monthly totals by,
#'     in addition to date (default: NULL). For example, c("tier") to see monthly totals by tier.
#'   \item show_progress: Show progress messages for large portfolios (default: TRUE)
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
    interest_on_starting_balance = FALSE,
    credit_loss_reduces_interest = TRUE,
    de_minimis_balance = 1.00,

    # CPR and credit cost vectors (all zeros by default)
    cpr_vec = c("default" = 0.0),
    credit_cost_vec = c("default" = 0.0),

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

  # Get first tier for default (when col_tier is NULL)
  default_tier <- names(cfg$cpr_vec)[1]

  # Validate tier values if tier column exists
  if (has_tier) {
    unique_tiers <- unique(data[[cfg$col_tier]])
    missing_tiers <- setdiff(unique_tiers, names(cfg$cpr_vec))

    if (length(missing_tiers) > 0) {
      warning(
        "The following tier values are not in cpr_vec/credit_cost_vec: ",
        paste(missing_tiers, collapse = ", "),
        ". Using first tier '", default_tier, "' as default for these loans."
      )
    }
  }

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

  # Validate tier - use default if not in vectors
  if (!tier %in% names(cfg$cpr_vec)) {
    tier <- default_tier
  }

  # Monthly rates and factors
  monthly_rate <- rate / 12
  monthly_servicing <- cfg$servicing_fee / 12
  monthly_reporting <- cfg$annual_reporting_fee / 12
  monthly_credit_cost <- cfg$credit_cost_vec[[tier]] / 12
  monthly_smm <- 1 - (1 - cfg$cpr_vec[[tier]])^(1/12)  # Single Monthly Mortality

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

  # Initialize
  balance <- principal
  min_balance <- 0.01

  # CHANGE 2: Use base R seq() with months for better month-end handling than lubridate
  max_months <- term
  all_dates <- seq.Date(start_date, by = "month", length.out = max_months)

  # Pre-allocate list with expected size
  cash_flows <- vector("list", term)
  i <- 1

  # Cash flow generation loop
  while (i <= term && balance >= min_balance) {

    starting_balance <- balance

    # CHANGE 1: Cap credit loss at 100% of starting balance
    credit_loss <- min(starting_balance * monthly_credit_cost, starting_balance)

    # Calculate prepayment - Cap at available balance after credit loss
    max_prepayment <- max(0, starting_balance - credit_loss)
    prepayment <- min(starting_balance * monthly_smm, max_prepayment)

    # Calculate adjusted balance (after prepayment and credit loss)
    adjusted_balance <- starting_balance - prepayment - credit_loss
    adjusted_balance <- max(0, adjusted_balance)

    # Determine which balance to use for interest and fees
    if (cfg$interest_on_starting_balance) {
      accrual_balance <- starting_balance
    } else {
      accrual_balance <- adjusted_balance
    }

    # Calculate interest and fees on the chosen balance
    gross_interest <- accrual_balance * monthly_rate
    servicing_fee_amt <- accrual_balance * monthly_servicing
    reporting_fee_amt <- accrual_balance * monthly_reporting
    total_fees <- servicing_fee_amt + reporting_fee_amt

    # Scheduled principal payment
    scheduled_principal <- scheduled_payment - gross_interest
    scheduled_principal <- max(0, scheduled_principal)
    scheduled_principal <- min(scheduled_principal, adjusted_balance)

    # Total principal returned
    total_principal <- scheduled_principal + prepayment
    total_principal <- min(total_principal, adjusted_balance)

    # Ending balance (before de minimis adjustment)
    remaining_balance <- starting_balance - total_principal - credit_loss
    remaining_balance <- max(0, remaining_balance)

    # De minimis balance handling
    if (remaining_balance > 0 && remaining_balance < cfg$de_minimis_balance) {
      total_principal <- total_principal + remaining_balance
      remaining_balance <- 0
    }

    # Store cash flow with pre-calculated date
    cash_flows[[i]] <- tibble::tibble(
      LOAN_ID = loan_id,
      eff_date = start_date,
      rate = rate,
      tier = tier,
      month = i,
      date = all_dates[i],
      starting_balance = starting_balance,
      adjusted_balance = adjusted_balance,
      accrual_balance = accrual_balance,
      scheduled_payment = scheduled_payment,
      gross_interest = gross_interest,
      servicing_fee_amt = servicing_fee_amt,
      reporting_fee_amt = reporting_fee_amt,
      total_fees = total_fees,
      scheduled_principal = scheduled_principal,
      prepayment = prepayment,
      total_principal = total_principal,
      credit_loss = credit_loss,
      remaining_balance = remaining_balance
    )

    balance <- remaining_balance
    i <- i + 1

    if (balance == 0) break
  }

  # Bind cash flows
  cash_flows_df <- dplyr::bind_rows(cash_flows[1:(i-1)])

  if (nrow(cash_flows_df) == 0) {
    return(tibble::tibble())
  }

  # Calculate origination fee using ORIGINAL term
  monthly_orig_fee <- if (term > 0 && cfg$origination_fee > 0) {
    (cfg$origination_fee * origbalance) / term
  } else {
    0
  }

  # Add investor calculations with origination fee
  cash_flows_df <- cash_flows_df %>%
    dplyr::mutate(
      # Origination fee (same for all months)
      orig_fee = monthly_orig_fee,

      # Calculate net interest based on accounting treatment
      net_interest_raw = if (cfg$credit_loss_reduces_interest) {
        # Default: credit losses reduce interest (participation accounting)
        gross_interest - total_fees - orig_fee - credit_loss
      } else {
        # Alternative: credit losses only reduce principal (balance sheet accounting)
        gross_interest - total_fees - orig_fee
      },

      # Floor at zero (investor never pays servicer)
      net_interest = pmax(net_interest_raw, 0),

      # Track how much orig fee was "absorbed" due to insufficient interest
      orig_fee_absorbed = dplyr::if_else(
        net_interest_raw < 0,
        orig_fee + net_interest_raw,
        orig_fee
      ),

      # Total payment to all parties
      total_payment = gross_interest + total_principal,

      # Investor share
      investor_principal = total_principal * cfg$investor_share,
      investor_interest = net_interest * cfg$investor_share,
      investor_total = investor_principal + investor_interest
    )

  return(cash_flows_df)
}
