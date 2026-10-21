# Global variables for NSE in dplyr
utils::globalVariables(c(
  "starting_balance", "adjusted_balance", "scheduled_payment",
  "gross_interest", "servicing_fee_amt", "reporting_fee_amt", "total_fees",
  "scheduled_principal", "balloon_principal", "prepayment", "total_principal", "credit_loss",
  "remaining_balance", "net_interest", "total_payment", "investor_principal",
  "investor_interest", "investor_total", "orig_fee", "net_interest_raw",
  "date", "."
))

#' Calculate Loan Portfolio Cash Flows
#'
#' Generates monthly cash flow projections for a loan portfolio, incorporating
#' user-defined prepayment speeds (CPR), credit costs, and various fee structures.
#'
#' @param data Required. A non-empty data frame containing one snapshot row per
#'   loan. It must contain the four required columns listed under
#'   \strong{Required data columns} below; every other column is optional.
#'   Required numeric fields must be finite and non-missing; balances must be
#'   positive, rates non-negative, and terms positive integers. Dates must be
#'   valid and non-missing. Invalid records raise an error rather than being skipped.
#' @param config Optional. A named list of configuration settings; every entry
#'   has a default, so \code{list()} (the default) is valid when \code{data}
#'   uses the default column names. Unknown, duplicate, or unnamed entries
#'   raise an error. See \strong{Configuration} below for each entry and
#'   whether it is required, optional, or conditionally required.
#'
#' @return Depending on return_monthly_totals setting:
#'   \itemize{
#'     \item If FALSE: A data frame with loan-level cash flows containing columns:
#'       LOAN_ID, eff_date, rate, tier, month, date (payment date; first payment
#'       falls one month after eff_date, which is the t=0 valuation anchor), starting_balance, adjusted_balance, accrual_balance,
#'       scheduled_payment, gross_interest, servicing_fee_amt, reporting_fee_amt,
#'       total_fees, scheduled_principal, balloon_principal, prepayment,
#'       total_principal, credit_loss,
#'       remaining_balance, orig_fee, net_interest, total_payment, investor_principal,
#'       investor_interest, investor_total
#'       and original_tier (input classification as character; NA when col_tier
#'       is not configured). Requested input grouping columns are also attached.
#'       balloon_principal is the balloon collected at maturity (zero for
#'       fully amortizing loans and in every other month); it is included in
#'       scheduled_principal.
#'     \item If TRUE: A list with two elements:
#'       \itemize{
#'         \item loan_cash_flows: Detailed loan-level cash flows (data frame)
#'         \item monthly_totals: Aggregated monthly totals across all loans (data frame)
#'       }
#'   }
#'
#' @details
#' \strong{Configuration}
#'
#' Every \code{config} entry has a default. "Required" below means the
#' \emph{data column} must exist; its name may be changed with the
#' \code{col_*} entry. "Optional" entries may be omitted.
#'
#' \emph{Required data columns} (each must exist in \code{data}):
#' \itemize{
#'   \item col_balance: Current balance (default column: "balance")
#'   \item col_rate: Current note rate as a decimal (default column: "current_interest_rate")
#'   \item col_term: Remaining months to maturity, as of the snapshot
#'     (default column: "months_to_maturity")
#'   \item col_start_date: Snapshot/effective date, the t = 0 valuation
#'     anchor (default column: "eff_date")
#' }
#'
#' \emph{Optional data columns} (default NULL unless noted; when set, the column must exist):
#' \itemize{
#'   \item col_loanid: Loan identifier (default: "LOAN_ID"). If that implicit
#'     column is absent, or col_loanid is NULL, sequential IDs are generated.
#'     An explicitly supplied col_loanid must exist.
#'   \item col_tier: Loan tier used to look up assumption vectors.
#'   \item col_monthly_payment: Contractual monthly payment. NA values use the
#'     calculated payment for that loan.
#'   \item col_orig_balance: Original balance, used for the origination fee.
#'     NA values use the current balance.
#'   \item col_amort_months_remaining: Remaining amortization months as of the
#'     snapshot (not the original amortization term). Payments are calculated
#'     over this term; a value greater than col_term makes the loan a balloon
#'     loan. Must be NA or a positive integer at least col_term; NA means
#'     fully amortizing over col_term. To convert an original amortization
#'     term, subtract the payments already made, usually
#'     original term - months_to_maturity.
#'   \item col_balloon: Per-loan balloon flag, logical or 0/1. TRUE collects
#'     any balance remaining at maturity as a balloon. NA uses
#'     balloon_at_maturity. Use it when the tape has payments but no
#'     amortization term.
#' }
#'
#' \emph{Optional settings} (defaults shown):
#' \itemize{
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
#'   \item balloon_at_maturity: Collect any balance remaining at maturity as a
#'     balloon for every loan without a col_balloon flag (default: FALSE).
#'     Intended for pools that are entirely balloon loans; on amortizing pools
#'     it would convert payment or term errors into balloons.
#'   \item reamortize_survivors: Prepayment cash flow convention (default: TRUE).
#'     TRUE treats SMM prepayments as full payoffs of a fraction of loans: the
#'     surviving balance re-amortizes over the remaining amortization term each
#'     month, so the
#'     aggregate scheduled payment declines with the survival factor. This
#'     matches market/Bloomberg pool conventions. When col_monthly_payment is
#'     supplied, tape payments are scaled by the cumulative survival factor
#'     instead. FALSE retains the legacy fixed-payment behavior, which treats
#'     prepayments as curtailments (original dollar payment held constant),
#'     accelerating scheduled principal and shortening effective life; retained
#'     for backward compatibility only.
#'   \item prepay_model: "tier_static" (default) or "linear_incentive"
#'   \item cpr_vec: Named vector of CPR rates by tier (default: c("default" = 0.0));
#'     used when prepay_model = "tier_static"
#'   \item credit_cost_vec: Named vector of annual credit cost rates by tier (default: c("default" = 0.0))
#'   \item pd_vec, lgd_vec: Optional named vectors of PD and LGD rates (default: NULL).
#'     Must be supplied together; when supplied they override credit_cost_vec.
#'   \item return_monthly_totals: Whether to return aggregated monthly totals (default: FALSE)
#'   \item monthly_totals_group_vars: Optional character vector of column names to group monthly totals by,
#'     in addition to date (default: NULL). Use "original_tier" for input
#'     classification or "tier" for the resolved assumption tier. Missing or
#'     ambiguous columns raise an error instead of being ignored.
#'   \item show_progress: Show progress messages for large portfolios and the
#'     balloon summary message (default: TRUE)
#' }
#'
#' \emph{Conditionally required settings}:
#' \itemize{
#'   \item With prepay_model = "linear_incentive", all of the following are
#'     required (default: NULL):
#'     \itemize{
#'       \item current_market_rate: Scalar market rate (decimal);
#'         rate_incentive = coupon - current_market_rate, coupon = col_rate (gross)
#'       \item base_cpr_vec: Tier-keyed intercept CPR
#'       \item beta_vec: Tier-keyed CPR sensitivity per decimal of rate incentive
#'       \item cpr_min_vec, cpr_max_vec: Tier-keyed clamp bounds on CPR
#'     }
#'   \item Without col_tier, the prepayment and credit-cost vectors must
#'     contain a tier named "default".
#' }
#'
#' \strong{Validation and conventions}
#'
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
#' original_tier preserves the input classification, including missing values,
#' independently of that fallback. Without col_tier it is NA, even if an unused
#' tier column is present in data.
#'
#' Grouping columns must exist in the input or be supported generated fields:
#' LOAN_ID, eff_date, rate, tier, original_tier, month, and date. date always means
#' projected payment date and is included once in monthly totals. Input columns
#' sharing a generated name are ambiguous unless that name is the corresponding
#' configured source column (for example, col_rate = "rate"). Rename conflicting
#' input classifications before grouping. Cash-flow amount columns cannot be
#' grouping keys. Missing grouping values are retained as NA groups. These checks
#' apply even when return_monthly_totals is FALSE. Metadata lookup requires one
#' input record per loan and preserves all projected rows and their order.
#'
#' Optional payment, original-balance, and amortization-term columns must be
#' numeric. NA values request the calculated payment, current-balance, or
#' fully amortizing fallback for that loan; other supplied values must be
#' finite and positive. Scalar settings must have the documented numeric or
#' logical type. Rates above 1 still warn without automatic conversion.
#' Negative amortization is not supported.
#'
#' Supplied payments (after survivor scaling, if enabled) must cover each
#' period's gross accrued interest under the selected accrual convention.
#' Payments below interest raise an error identifying the loan and month;
#' only floating-point roundoff is tolerated. Payments equal to interest are
#' allowed (interest-only); flag such loans as balloons to collect their
#' principal at maturity.
#'
#' Small-balance cleanup is included in scheduled_principal, so total_principal
#' equals scheduled_principal + prepayment. scheduled_payment remains the
#' calculated or supplied payment before final payoff adjustments. Positive
#' sub-cent balances are projected rather than silently discarded.
#'
#' \strong{Balloon loans}
#'
#' A loan is a balloon loan when col_amort_months_remaining exceeds col_term,
#' when its col_balloon flag is TRUE, or when its flag is NA (or col_balloon is
#' not set) and balloon_at_maturity is TRUE. An explicit col_balloon = FALSE
#' cannot override a longer amortization term; those loans pay a balloon and
#' are reported in a warning. In the maturity month, the balance remaining
#' after the regular payment and credit loss is collected as balloon_principal,
#' which is part of scheduled_principal; no prepayment is projected in that
#' month. A supplied payment that retires the loan before maturity leaves no
#' balloon. When show_progress is TRUE, a message reports the number of loans
#' paying a balloon and the total amount. Refinance or extension risk at
#' maturity is not modeled: the balloon is assumed paid in full.
#'
#' A portfolio-level warning identifies loans that are not balloon loans and
#' retain residual balances at maturity of at least max(0.01, de_minimis_balance),
#' in either output mode. These balances remain in remaining_balance. Downstream
#' PV and WAL therefore describe only the projected collections, not full
#' repayment.
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
#' # Balloon (e.g. CRE) loans: 5 years to maturity on a 25-year schedule,
#' # 3 years seasoned, so 264 amortization months remain
#' cre_data <- data.frame(
#'   LOAN_ID = c("CRE1", "AUTO1"),
#'   balance = c(1500000, 25000),
#'   current_interest_rate = c(0.0675, 0.0599),
#'   months_to_maturity = c(24, 60),
#'   amort_months_remaining = c(264, NA),   # NA: fully amortizing
#'   eff_date = as.Date("2025-01-01")
#' )
#' cre_flows <- calculate_cash_flows(cre_data, list(
#'   col_amort_months_remaining = "amort_months_remaining",
#'   cpr_vec = c("default" = 0.05)
#' ))
#' subset(cre_flows, balloon_principal > 0)
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
    col_amort_months_remaining = NULL,
    col_balloon = NULL,

    # Parameter defaults
    servicing_fee = 0.0025,
    annual_reporting_fee = 0.00,
    investor_share = 1.0,
    origination_fee = 0.0000,
    interest_on_starting_balance = TRUE,
    credit_loss_reduces_interest = FALSE,
    de_minimis_balance = 1.00,
    balloon_at_maturity = FALSE,

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
  optional_mappings <- c("col_tier", "col_monthly_payment", "col_orig_balance",
                         "col_amort_months_remaining", "col_balloon")
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
  balloon_terms <- resolve_balloon_terms(data, cfg)

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
      origbalance = if (!is.null(cfg$col_orig_balance)) data[[cfg$col_orig_balance]] else NA_real_,
      amort_months = balloon_terms$amort_months,
      balloon = balloon_terms$balloon
    ),
    .f = generate_single_loan_cash_flow,
    cfg = cfg,
    default_tier = default_tier
  )

  # Bind all loan cash flows
  loan_cash_flows <- dplyr::bind_rows(cash_flows_list)

  # Report incomplete maturity projections once per portfolio, in either output
  # mode. Only loans not identified as balloons can retain principal at maturity.
  terminal <- loan_cash_flows[!duplicated(loan_cash_flows$LOAN_ID, fromLast = TRUE), ]
  residuals <- terminal[terminal$remaining_balance >= max(0.01, cfg$de_minimis_balance), ]
  if (nrow(residuals) > 0L) {
    warning(
      nrow(residuals), " loan(s) retain a balance at contractual maturity (total ",
      format(sum(residuals$remaining_balance), digits = 10, trim = TRUE),
      "). Loan IDs: ", paste(utils::head(residuals$LOAN_ID, 5), collapse = ", "),
      if (nrow(residuals) > 5L) ", ..." else "",
      ". Projected principal collections are incomplete; verify payments and terms. ",
      "No balloon payoff has been added: identify balloon loans with ",
      "col_amort_months_remaining, col_balloon, or balloon_at_maturity.", call. = FALSE
    )
  }
  balloons <- loan_cash_flows$balloon_principal > 0
  if (cfg$show_progress && any(balloons)) {
    message(length(unique(loan_cash_flows$LOAN_ID[balloons])),
            " loan(s) pay a balloon at maturity (total ",
            format(round(sum(loan_cash_flows$balloon_principal), 2), big.mark = ",",
                   nsmall = 2), ").")
  }

  loan_cash_flows <- attach_cash_flow_groups(loan_cash_flows, data, cfg)

  if (cfg$show_progress && nrow(data) > 1000) {
    message("Cash flows generated successfully: ",
            format(nrow(loan_cash_flows), big.mark = ","), " monthly payments")
  }

  # Return based on monthly_totals flag
  if (cfg$return_monthly_totals) {

    group_cols <- unique(c("date", cfg$monthly_totals_group_vars))

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
        balloon_principal = sum(balloon_principal, na.rm = TRUE),
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
                                           default_tier,
                                           amort_months = NA_real_,
                                           balloon = FALSE) {

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

  # Payments amortize over the remaining amortization term, which exceeds the
  # remaining maturity for a balloon loan; NA means fully amortizing.
  amort_term <- if (is.na(amort_months)) term else as.integer(amort_months)
  balloon    <- isTRUE(balloon) || amort_term > term

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
    base_payment <- principal / amort_term
  } else {
    base_payment <- principal * monthly_rate / (1 - (1 + monthly_rate)^(-amort_term))
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
  balloon_principal_v   <- numeric(term)
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

    # A balloon loan repays its whole balance at maturity, so no prepayment is
    # projected in that month: the payoff is scheduled (balloon) principal.
    balloon_month <- balloon && i == term
    smm_i <- if (balloon_month) 0 else monthly_smm

    if (isTRUE(cfg$reamortize_survivors)) {

      # ---- Market convention: prepays are full payoffs; survivors keep
      # ---- their own schedules, so the pool payment re-amortizes monthly.
      n_rem <- amort_term - i + 1L
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
      pp <- max(0, sb - cl - sp) * smm_i

      ab <- max(0, sb - pp - cl)      # column meaning unchanged vs legacy
      tp <- sp + pp
      rb <- max(0, sb - tp - cl)

      survival <- survival * (1 - smm_i) *
        (1 - (if (sb > 0) cl / sb else 0))

    } else {

      # ---- Fixed-payment (curtailment) convention ----
      max_prepay <- max(0, sb - cl)
      pp <- min(sb * smm_i, max_prepay)

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

    # The balloon is scheduled principal, so total_principal still equals
    # scheduled_principal + prepayment. Cleanup above keeps roundoff on a fully
    # amortized loan from being reported as a balloon.
    bp <- 0
    if (balloon_month && rb > 0) {
      bp <- rb
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
    balloon_principal_v[i]   <- bp
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

  # Single tibble construction (v0.2.3 column order, plus balloon_principal)
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
    balloon_principal   = balloon_principal_v[idx],
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
