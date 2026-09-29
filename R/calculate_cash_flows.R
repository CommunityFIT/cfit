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
#' @param data A non-empty data frame containing one snapshot row per loan.
#'   Required numeric fields must be finite and non-missing; balances must be
#'   positive, rates non-negative, and terms positive integers. Dates must be
#'   valid and non-missing. Invalid records raise an error rather than being skipped.
#' @param config A named list of configuration parameters. Unknown, duplicate,
#'   or unnamed entries raise an error. See details for structure.
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
#' Loan IDs must be unique and non-missing. If the implicit default LOAN_ID
#' column is absent, or col_loanid is NULL, sequential IDs are generated.
#' Every explicitly requested column must exist, including col_loanid.
#'
#' Assumption vectors must have unique, non-empty tier names and finite values.
#' Probability and credit-cost values must be in `[0, 1]`. PD and LGD must be
#' supplied together, cover the same tiers, and are multiplied by matching name,
#' regardless of vector order. Effective credit costs must cover all tiers in
#' the active prepayment model. Without col_tier, a named "default" tier is
#' required. Unknown or missing loan tiers use that same named default for both
#' prepayment and credit costs, with a warning; without it, they raise an error.
#' The output tier records the resolved assumption tier. The first vector entry
#' is never used as an implicit default.
#'
#' Optional payment and original-balance columns must be numeric. NA values
#' request the calculated payment or current-balance fallback for that loan;
#' other supplied values must be finite and positive. Scalar settings must have
#' the documented numeric or logical type. Rates above 1 still warn without
#' automatic conversion. Validation does not add support for balloon payments
#' or negative amortization.
#'
#' Supplied payments (after survivor scaling, if enabled) must cover each
#' period's gross accrued interest under the selected accrual convention.
#' Payments below interest raise an error identifying the loan and month;
#' only floating-point roundoff is tolerated. Payments equal to interest are
#' allowed, but may leave principal outstanding at maturity.
#'
#' Small-balance cleanup is included in scheduled_principal, so total_principal
#' equals scheduled_principal + prepayment. scheduled_payment remains the
#' calculated or supplied payment before final payoff adjustments. Positive
#' sub-cent balances are projected rather than silently discarded.
#'
#' A portfolio-level warning identifies loans with residual balances at maturity
#' of at least max(0.01, de_minimis_balance), in either output mode. These balances
#' remain in remaining_balance; no balloon payment is assumed. Downstream PV and
#' WAL therefore describe only the projected collections, not full repayment.
#'
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
#'   \item de_minimis_balance: Positive residual balances below this threshold
#'     are collected as additional scheduled principal (default: 1.00).
#'     Set to 0 to disable this cleanup.
#'   \item reamortize_survivors: Prepayment cash flow convention (default: TRUE).
#'     TRUE treats SMM prepayments as full payoffs of a fraction of loans: the
#'     surviving balance re-amortizes over the remaining term each month, so the
#'     aggregate scheduled payment declines with the survival factor. This
#'     matches market/Bloomberg pool conventions. When col_monthly_payment is
#'     supplied, tape payments are scaled by the cumulative survival factor
#'     instead. FALSE retains the legacy fixed-payment behavior, which treats
#'     prepayments as curtailments (original dollar payment held constant),
#'     accelerating scheduled principal and shortening effective life; retained
#'     for backward compatibility only.
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

    # Prepayment cash flow convention
    reamortize_survivors = TRUE,

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

  validate_cash_flow_inputs(data, config, names(default_config))
  # Keep explicit NULLs so invalid required settings cannot disappear in merging.
  cfg <- modifyList(default_config, config, keep.null = TRUE)
  cfg <- validate_cash_flow_config(cfg)

  required_cols <- c(cfg$col_balance, cfg$col_rate, cfg$col_term, cfg$col_start_date)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in data: ", paste(missing_cols, collapse = ", "))
  }
  optional_mappings <- c("col_tier", "col_monthly_payment", "col_orig_balance")
  # An explicitly requested identifier must exist. Automatic IDs remain available
  # when col_loanid is NULL or the implicit LOAN_ID column is absent.
  if ("col_loanid" %in% names(config)) {
    optional_mappings <- c(optional_mappings, "col_loanid")
  }
  for (nm in optional_mappings) {
    if (!is.null(cfg[[nm]]) && !cfg[[nm]] %in% names(data)) {
      stop(nm, " specifies missing column '", cfg[[nm]], "'.")
    }
  }
  if (is.null(cfg$col_loanid) || !cfg$col_loanid %in% names(data)) {
    data$LOAN_ID <- seq_len(nrow(data))
    cfg$col_loanid <- "LOAN_ID"
  }
  data <- validate_cash_flow_data(data, cfg)

  if (!is.null(cfg$pd_vec)) {
    if (!is.null(config$credit_cost_vec)) {
      warning("Both credit_cost_vec and pd_vec/lgd_vec provided. ",
              "Using PD/LGD approach (credit_cost = PD * LGD). ",
              "The credit_cost_vec will be ignored.")
    }
    # Match by tier name, never by the position of a vector element.
    cfg$credit_cost_vec <- cfg$pd_vec * cfg$lgd_vec[names(cfg$pd_vec)]
    validate_cash_flow_vector(cfg$credit_cost_vec, "derived credit_cost_vec")
  }

  prepay_tier_names <- if (cfg$prepay_model == "linear_incentive") {
    names(cfg$base_cpr_vec)
  } else {
    names(cfg$cpr_vec)
  }
  missing_credit <- setdiff(prepay_tier_names, names(cfg$credit_cost_vec))
  if (length(missing_credit) > 0) {
    stop("credit_cost_vec (or derived PD*LGD) must cover all prepayment tiers. ",
         "Missing: ", paste(missing_credit, collapse = ", "))
  }

  default_tier <- "default"
  has_tier <- !is.null(cfg$col_tier)
  if (!has_tier && !default_tier %in% prepay_tier_names) {
    stop("When col_tier is NULL, prepayment and credit-cost vectors ",
         "must contain a 'default' tier.")
  }
  tiers <- if (has_tier) as.character(data[[cfg$col_tier]]) else
    rep(default_tier, nrow(data))
  unknown <- !tiers %in% prepay_tier_names
  if (any(unknown)) {
    unknown_labels <- unique(tiers[unknown])
    unknown_labels[is.na(unknown_labels)] <- "<NA>"
    msg <- paste0("Unknown or missing loan tiers: ",
                  paste(unknown_labels, collapse = ", "), ". ")
    if (!default_tier %in% prepay_tier_names) {
      stop(msg, "Define a 'default' tier in the assumption vectors or correct the data.")
    }
    warning(msg, "Using the named 'default' tier for prepayment and credit costs.")
    tiers[unknown] <- default_tier
  }
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
      tier = tiers,
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

  # Report incomplete maturity projections once per portfolio, in either output
  # mode. Residual principal remains outstanding; it is not an assumed balloon.
  terminal <- loan_cash_flows[!duplicated(loan_cash_flows$LOAN_ID, fromLast = TRUE), ]
  residuals <- terminal[terminal$remaining_balance >= max(0.01, cfg$de_minimis_balance), ]
  if (nrow(residuals) > 0L) {
    warning(
      nrow(residuals), " loan(s) retain a balance at contractual maturity (total ",
      format(sum(residuals$remaining_balance), digits = 10, trim = TRUE),
      "). Loan IDs: ", paste(utils::head(residuals$LOAN_ID, 5), collapse = ", "),
      if (nrow(residuals) > 5L) ", ..." else "",
      ". Projected principal collections are incomplete; verify payments and terms. ",
      "No balloon payoff has been added.", call. = FALSE
    )
  }

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

  # Base scheduled payment (original amortization; time-varying under
  # reamortize_survivors, so per-month values are stored in a vector)
  use_monthly_payment <- !is.na(monthly_payment_val) && monthly_payment_val > 0
  if (use_monthly_payment) {
    base_payment <- monthly_payment_val
  } else if (monthly_rate == 0) {
    base_payment <- principal / term
  } else {
    base_payment <- principal * monthly_rate / (1 - (1 + monthly_rate)^(-term))
  }

  # Pre-computed payment dates: first payment one month AFTER the as-of date
  # (start_date is the settlement/valuation anchor, t = 0; payments are t = 1..term).
  all_dates <- start_date %m+% months(seq_len(term))

  # Pre-allocated atomic accumulators (one slot per scheduled month)
  starting_balance_v    <- numeric(term)
  adjusted_balance_v    <- numeric(term)
  accrual_balance_v     <- numeric(term)
  scheduled_payment_v   <- numeric(term)
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
  n_months    <- 0L
  survival    <- 1.0   # cumulative surviving fraction (reamortize path)

  # Sequential balance roll-forward (each month depends on the prior)
  i <- 1L
  while (i <= term && balance > 0) {
    sb <- balance

    # Cap credit loss at 100% of starting balance
    cl <- min(sb * monthly_credit_cost, sb)

    if (isTRUE(cfg$reamortize_survivors)) {

      # ---- Market convention: prepays are full payoffs; survivors keep
      # ---- their own schedules, so the pool payment re-amortizes monthly.
      n_rem <- term - i + 1L
      pmt_i <- if (use_monthly_payment) {
        base_payment * survival
      } else if (monthly_rate == 0) {
        sb / n_rem
      } else {
        sb * monthly_rate / (1 - (1 + monthly_rate)^(-n_rem))
      }

      # Full-month interest on the starting balance (see validate_cash_flow_config:
      # post-prepay accrual would be circular under this ordering)
      acc <- sb
      gi  <- acc * monthly_rate
      sf  <- acc * monthly_servicing
      rf  <- acc * monthly_reporting
      tf  <- sf + rf

      # Scheduled amortization first, SMM on the post-scheduled balance
      sp <- max(0, pmt_i - gi)
      sp <- min(sp, max(0, sb - cl))
      pp <- max(0, sb - cl - sp) * monthly_smm

      ab <- max(0, sb - pp - cl)      # column meaning unchanged vs legacy
      tp <- sp + pp
      rb <- max(0, sb - tp - cl)

      survival <- survival * (1 - monthly_smm) *
        (1 - (if (sb > 0) cl / sb else 0))

    } else {

      # ---- Fixed-payment (curtailment) convention ----
      max_prepay <- max(0, sb - cl)
      pp <- min(sb * monthly_smm, max_prepay)

      ab <- max(0, sb - pp - cl)

      acc <- if (cfg$interest_on_starting_balance) sb else ab

      gi <- acc * monthly_rate
      sf <- acc * monthly_servicing
      rf <- acc * monthly_reporting
      tf <- sf + rf

      pmt_i <- base_payment
      sp <- max(0, pmt_i - gi)
      sp <- min(sp, ab)

      # pp is capped at sb - cl; sp is capped at the balance after pp.
      # Capping their sum at ab would discard principal already prepaid.
      tp <- sp + pp
      rb <- max(0, sb - tp - cl)
    }

    # Check the actual period's payment and accrual convention, including
    # survivor scaling. Allow only floating-point noise, not unpaid interest.
    payment_tolerance <- 64 * .Machine$double.eps * max(1, abs(pmt_i), abs(gi))
    if (use_monthly_payment && pmt_i < gi - payment_tolerance) {
      stop("Loan '", loan_id, "', month ", i, ": supplied payment (",
           format(pmt_i, digits = 10, trim = TRUE), ") is below accrued interest (",
           format(gi, digits = 10, trim = TRUE),
           "). Unpaid interest / negative amortization is not supported.", call. = FALSE)
    }

    # Treat a small final balance as scheduled payoff principal, preserving
    # both total collections and the component reconciliation.
    if (rb > 0 && rb < cfg$de_minimis_balance) {
      sp <- sp + rb
      tp <- tp + rb
      rb <- 0
    }

    starting_balance_v[i]    <- sb
    adjusted_balance_v[i]    <- ab
    accrual_balance_v[i]     <- acc
    scheduled_payment_v[i]   <- pmt_i
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
    scheduled_payment   = scheduled_payment_v[idx],
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
