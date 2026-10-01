#' Calculate Single Monthly Mortality and Conditional Prepayment Rate
#'
#' Computes single monthly mortality (SMM) and conditional prepayment rate (CPR)
#' for loan pools by cohort, accounting for scheduled versus actual principal payments.
#' Supports flexible column naming, loan-level or global interest rate calculation methods
#' for different financial institution systems.
#'
#' @param df Data frame containing loan-level monthly snapshot data.
#'   Each row represents a loan's status for a given reporting period.
#'   Data should include only active loans (open and non-performing).
#'   **Important:** Each loan should appear exactly once per reporting period.
#'
#' @param group_vars Character vector specifying columns to group by when calculating
#'   aggregate prepayment rates (e.g., \code{c("EFFDATE", "TYPECODE")}).
#'   These typically represent reporting period and loan product type.
#'   **Note:** The effective date column must be included in group_vars.
#'
#' @param prepay_config List containing column name mappings and calculation parameters:
#'   \describe{
#'     \item{\code{col_effdate}}{Name of reporting/effective date column (default: "EFFDATE")}
#'     \item{\code{col_origdate}}{Name of origination date column (default: "ORIGDATE")}
#'     \item{\code{col_typecode}}{Name of loan type/product column (default: "TYPECODE")}
#'     \item{\code{col_balance}}{Name of current loan balance column (default: "BAL")}
#'     \item{\code{col_orig_balance}}{Name of original/funded balance column (default: "ORIGBAL")}
#'     \item{\code{col_payment}}{Name of contractual payment amount column (default: "PAYAMT")}
#'     \item{\code{col_rate}}{Name of current interest rate column (default: "CURRINTRATE").
#'       Rates should be in decimal form (e.g., 0.0729 for 7.29\%). The function will
#'       detect and convert percentage format with a warning.}
#'     \item{\code{col_interest_basis}}{Name of loan-level interest basis column (optional, default: NULL).
#'       If provided, uses loan-level interest calculation basis. If NULL, uses global \code{interest_basis} parameter.}
#'     \item{\code{col_loanid}}{Name of unique loan identifier column (optional but recommended, default: NULL).
#'       If provided: (1) validates that each loan appears only once per reporting period,
#'       and (2) enables more accurate scheduled principal calculation using beginning-of-period balances.
#'       Recommended for data quality assurance and calculation accuracy (e.g., "LOANNUMBER", "LOAN_ID").}
#'     \item{\code{interest_basis}}{Number of days in year for interest calculation (global fallback).
#'       Use 360 or 365 for daily interest method, or NULL/NA for simple monthly division.
#'       Ignored if \code{col_interest_basis} is specified (default: 365)}
#'     \item{\code{allow_negative_prepay}}{Logical indicating whether to allow negative
#'       SMM/CPR values. If FALSE, negative values are floored at 0 (default: FALSE)}
#'     \item{\code{min_begin_balance}}{Numeric threshold for minimum beginning balance.
#'       Diagnostic-free cohorts below this value are excluded; flagged rows are retained (default: 0)}
#'   }
#'
#' @param verbose Logical indicating whether to print informational messages
#'   about interest calculation methods and data filtering (default: FALSE)
#'
#' @return Data frame with columns:
#'   \describe{
#'     \item{Grouping columns}{As specified in \code{group_vars}}
#'     \item{\code{BEGIN_BAL}}{Beginning loan balance for the period}
#'     \item{\code{END_BAL}}{Ending loan balance for the period}
#'     \item{\code{SCHED_PRIN_TOTAL}}{Total scheduled principal for the period}
#'     \item{\code{FUNDED_BAL}}{Loans funded during the period (new originations, approximate)}
#'     \item{\code{ACTUAL_PRIN}}{Principal inferred from snapshots; NA when attribution is unresolved}
#'     \item{\code{PREPAYMENT}}{Principal paid in excess of scheduled amount}
#'     \item{\code{SMM}}{Bounded SMM used for CPR: `[0, 1]`, or `[-1, 1]` when negative prepayment is allowed}
#'     \item{\code{CPR}}{1 - (1 - SMM)^12; may be negative when allowed; NA for undefined estimates}
#'     \item{\code{AVAILABLE_TO_PREPAY}}{Beginning balance minus scheduled principal, before any bounding}
#'     \item{\code{SMM_RAW}}{Unbounded prepayment estimate; NA for unresolved attribution or a non-positive denominator}
#'     \item{\code{SMM_ADJUSTED}}{Whether SMM differs from SMM_RAW; NA when undefined}
#'     \item{\code{COHORT_DISAPPEARED}}{Cohort present in the prior snapshot but absent in the current one}
#'     \item{\code{UNRESOLVED_EXITS}}{Count of prior cohort loans absent from the current cohort; NA without loan IDs}
#'     \item{\code{UNRESOLVED_ENTRIES}}{Count of cohort entrants not originated in the reporting month; NA without loan IDs}
#'     \item{\code{DIAGNOSTIC}}{Semicolon-separated flags, or "ok" when no issue was detected}
#'   }
#'
#' @details
#'
#' \strong{Snapshot safeguards:}
#'
#' Reporting dates must cover consecutive calendar months with one date per month.
#' Missing whole-portfolio months raise an error. Cohorts are compared only with
#' the immediately preceding snapshot. Disappeared cohorts remain in the output
#' with unknown END_BAL and speeds, rather than being assumed paid off. A new or
#' reappearing cohort has unknown BEGIN_BAL and speeds for that observation.
#'
#' With loan IDs, changes in cohort membership are flagged. Exits may represent
#' payoffs, charge-offs, sales, transfers, or missing records. Entries other than
#' same-month originations are also unresolved. These rows have NA ACTUAL_PRIN,
#' PREPAYMENT, SMM_RAW, SMM, and CPR. Same-month originations retain the existing
#' approximate funding treatment. Without IDs, individual exits and transfers
#' cannot be detected; "ok" is not proof that all runoff was voluntary prepayment.
#' No disappearance after the final supplied snapshot can be detected.
#'
#' DIAGNOSTIC flags are cohort_disappeared, no_prior_cohort, unresolved_exits,
#' unresolved_entries, invalid_denominator, nonfinite_estimate,
#' negative_prepayment, and out_of_range_smm. Undefined estimates produce one
#' summary warning, even when verbose = FALSE. Negative and bounded estimates
#' retain flags in the output. Diagnostic rows bypass min_begin_balance filtering.
#'
#' \strong{Methodology:}
#'
#' SMM_RAW measures the monthly prepayment rate relative to the pool available to prepay
#' (beginning balance minus scheduled principal):
#' \deqn{SMM = \frac{PREPAYMENT}{BEGIN\_BAL - SCHED\_PRIN\_TOTAL}}{SMM = PREPAYMENT / (BEGIN_BAL - SCHED_PRIN_TOTAL)}
#'
#' This ratio is retained as SMM_RAW; SMM is its bounded value.
#' CPR annualizes SMM using the standard conversion formula:
#' \deqn{CPR = 1 - (1 - SMM)^{12}}{CPR = 1 - (1 - SMM)^12}
#'
#' Principal flow calculation accounts for new originations:
#' \deqn{ACTUAL\_PRIN = BEGIN\_BAL - END\_BAL + FUNDED\_BAL}
#'
#' \strong{Scheduled Principal Calculation:}
#'
#' The accuracy of scheduled principal depends on whether a loan ID is provided:
#'
#' \itemize{
#'   \item \strong{With col_loanid (recommended):} Scheduled principal is calculated using
#'     beginning-of-period balance for each loan, providing accurate interest and principal separation:
#'     \deqn{SCHEDPRIN = PAYMENT - (BEGIN\_BAL_{loan} \times MONTHLY\_RATE)}
#'   \item \strong{Without col_loanid:} Scheduled principal uses end-of-period balance as an
#'     approximation. This slightly overstates scheduled principal and understates prepayment
#'     (typically <1-2\% error):
#'     \deqn{SCHEDPRIN \approx PAYMENT - (END\_BAL_{loan} \times MONTHLY\_RATE)}
#' }
#'
#' \strong{Interest Calculation Methods:}
#' \itemize{
#'   \item \strong{Loan-level basis} (when \code{col_interest_basis} is specified):
#'     Each loan uses its own interest basis. For loans with basis 360 or 365:
#'     \eqn{INTEREST = BAL \times RATE \times \frac{DAYS\_IN\_MONTH}{LOAN\_BASIS}}
#'     For loans with invalid/missing basis: \eqn{INTEREST = BAL \times \frac{RATE}{12}}
#'   \item \strong{Global basis} (when \code{col_interest_basis} is NULL):
#'     If \code{interest_basis} is 360 or 365:
#'     \eqn{INTEREST = BAL \times RATE \times \frac{DAYS\_IN\_MONTH}{interest\_basis}}
#'     Otherwise: \eqn{INTEREST = BAL \times \frac{RATE}{12}}
#' }
#'
#' \strong{Data Requirements:}
#' \itemize{
#'   \item **One row per loan per reporting date** - Each loan should appear exactly once
#'     for each effective date. Use \code{col_loanid} parameter to validate this assumption.
#'   \item Loan-level data with monthly snapshots (one row per loan per reporting date)
#'   \item Data should include only active loans (open and non-performing)
#'   \item All required columns must be present (checked on function entry)
#'   \item Balances, original balances, payments, and rates must be finite, non-missing, non-negative numeric values
#'   \item Interest rates should be in decimal form (e.g., 0.0729 for 7.29\%). Percentage
#'     format (e.g., 7.29) will be detected and converted with a warning.
#'   \item Date columns must be coercible to Date format
#'   \item If using loan-level basis, values should be 360 or 365 (invalid values use simple monthly)
#' }
#'
#' \strong{FUNDED_BAL Interpretation:}
#'
#' FUNDED_BAL is an approximation of new loan originations, calculated by identifying
#' loans where the origination date (month and year) matches the reporting period. It uses
#' the original funded amount (ORIGBAL) rather than actual cash flows, and matches by
#' month/year only. For precise cash flow analysis, consider using actual funding data
#' if available.
#'
#' \strong{Data Quality Safeguards:}
#' \itemize{
#'   \item SMM denominator adjusted for scheduled principal (industry standard PSA method)
#'   \item SMM is clamped to valid ranges before CPR calculation to prevent mathematical errors
#'   \item Scheduled principal is floored at 0 to handle interest-only periods
#'   \item EFFDATE must be included in group_vars (validated on entry)
#'   \item Data is automatically sorted by EFFDATE to ensure correct lag operations
#'   \item Optional duplicate loan detection when col_loanid is provided
#'   \item Interest rate format validation (detects percentage vs decimal)
#' }
#'
#' @section Notes:
#' \itemize{
#'   \item The first month of data is excluded automatically (no prior month for lagging)
#'   \item By default, SMM and CPR are floored at 0 (set \code{allow_negative_prepay = TRUE} to disable)
#'   \item Missing or non-finite required numeric data raises an error; it is not treated as zero
#'   \item Only diagnostic-free cohorts below \code{min_begin_balance} are filtered out
#'   \item The function uses \code{lubridate} for date handling and \code{dplyr} for data manipulation
#'   \item Providing \code{col_loanid} improves both data quality and calculation accuracy
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage with loan ID validation (recommended)
#' result <- calculate_prepay_speed(
#'   df = loan_pool_data,
#'   group_vars = c("EFFDATE", "TYPECODE"),
#'   prepay_config = list(
#'     col_loanid = "LOANNUMBER"  # Validates uniqueness and improves accuracy
#'   )
#' )
#'
#' # Example 2: Custom column names with validation
#' custom_config <- list(
#'   col_effdate = "ReportDate",
#'   col_origdate = "OpenDate",
#'   col_typecode = "Product",
#'   col_balance = "CurrentBalance",
#'   col_orig_balance = "StartingBalance",
#'   col_payment = "MonthlyPayment",
#'   col_rate = "InterestRate",
#'   col_loanid = "LoanID",
#'   interest_basis = 360
#' )
#'
#' result <- calculate_prepay_speed(
#'   df = loan_pool_data,
#'   group_vars = c("ReportDate", "Product"),
#'   prepay_config = custom_config
#' )
#'
#' # Example 3: Without loan ID (less accurate scheduled principal)
#' result_approx <- calculate_prepay_speed(
#'   df = loan_pool_data,
#'   group_vars = c("EFFDATE", "TYPECODE")
#'   # No col_loanid: uses end balance approximation for scheduled principal
#' )
#' }
#'
#' @import dplyr
#' @import lubridate
#' @export
#'
calculate_prepay_speed <- function(
    df,
    group_vars,
    prepay_config = list(
      col_effdate = "EFFDATE",
      col_origdate = "ORIGDATE",
      col_typecode = "TYPECODE",
      col_balance = "BAL",
      col_orig_balance = "ORIGBAL",
      col_payment = "PAYAMT",
      col_rate = "CURRINTRATE",
      col_interest_basis = NULL,
      col_loanid = NULL,
      interest_basis = 365,
      allow_negative_prepay = FALSE,
      min_begin_balance = 0
    ),
    verbose = FALSE
) {

  # ========================================================================
  # INPUT VALIDATION
  # ========================================================================

  # Validate df is a data frame
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a data frame")
  }

  if (nrow(df) == 0) {
    stop("Input 'df' is empty (0 rows)")
  }

  # Validate prepay_config is a list
  if (!is.list(prepay_config)) {
    stop("'prepay_config' must be a list")
  }

  # Set defaults for any missing config values
  default_config <- list(
    col_effdate = "EFFDATE",
    col_origdate = "ORIGDATE",
    col_typecode = "TYPECODE",
    col_balance = "BAL",
    col_orig_balance = "ORIGBAL",
    col_payment = "PAYAMT",
    col_rate = "CURRINTRATE",
    col_interest_basis = NULL,
    col_loanid = NULL,
    interest_basis = 365,
    allow_negative_prepay = FALSE,
    min_begin_balance = 0
  )

  # Merge user config with defaults (user values take precedence)
  prepay_config <- modifyList(default_config, prepay_config)

  # Extract config values
  col_effdate <- prepay_config$col_effdate
  col_origdate <- prepay_config$col_origdate
  col_typecode <- prepay_config$col_typecode
  col_balance <- prepay_config$col_balance
  col_orig_balance <- prepay_config$col_orig_balance
  col_payment <- prepay_config$col_payment
  col_rate <- prepay_config$col_rate
  col_interest_basis <- prepay_config$col_interest_basis
  col_loanid <- prepay_config$col_loanid
  interest_basis <- prepay_config$interest_basis
  allow_negative_prepay <- prepay_config$allow_negative_prepay
  min_begin_balance <- prepay_config$min_begin_balance

  # Validate required columns exist (excluding optional col_interest_basis and col_loanid)
  required_cols <- c(
    col_effdate, col_origdate, col_typecode, col_balance,
    col_orig_balance, col_payment, col_rate
  )
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in input data: ",
      paste(missing_cols, collapse = ", "),
      "\nCheck 'prepay_config' column mappings."
    )
  }

  # Validate optional col_interest_basis if provided
  use_loan_level_basis <- FALSE
  if (!is.null(col_interest_basis)) {
    if (!col_interest_basis %in% names(df)) {
      stop(
        "col_interest_basis specified as '", col_interest_basis,
        "' but not found in data. Either remove col_interest_basis from config or add the column to df."
      )
    }
    use_loan_level_basis <- TRUE
  }

  # Validate optional col_loanid if provided
  use_loan_id <- FALSE
  if (!is.null(col_loanid)) {
    if (!col_loanid %in% names(df)) {
      stop(
        "col_loanid specified as '", col_loanid,
        "' but not found in data. Either remove col_loanid from config or add the column to df."
      )
    }
    use_loan_id <- TRUE
  }

  # Validate global interest_basis (only relevant if not using loan-level)
  if (!use_loan_level_basis) {
    if (!is.null(interest_basis) && !is.na(interest_basis)) {
      if (!is.numeric(interest_basis)) {
        warning(
          "interest_basis is not numeric (got: ", class(interest_basis), "). ",
          "Defaulting to simple monthly interest calculation (RATE / 12)."
        )
        interest_basis <- NULL
      } else if (!interest_basis %in% c(360, 365)) {
        warning(
          "interest_basis must be 360 or 365 (got: ", interest_basis, "). ",
          "Defaulting to simple monthly interest calculation (RATE / 12)."
        )
        interest_basis <- NULL
      }
    }
  }

  # Validate group_vars
  if (!is.character(group_vars) || length(group_vars) == 0) {
    stop("'group_vars' must be a non-empty character vector")
  }

  missing_group <- setdiff(group_vars, names(df))
  if (length(missing_group) > 0) {
    stop("'group_vars' columns not found in data: ", paste(missing_group, collapse = ", "))
  }

  reserved_output <- c("BEGIN_BAL", "END_BAL", "SCHED_PRIN_TOTAL", "FUNDED_BAL",
    "ACTUAL_PRIN", "PREPAYMENT", "SMM", "CPR", "AVAILABLE_TO_PREPAY", "SMM_RAW",
    "SMM_ADJUSTED", "COHORT_DISAPPEARED", "UNRESOLVED_EXITS", "UNRESOLVED_ENTRIES", "DIAGNOSTIC")
  if (any(group_vars %in% reserved_output)) {
    stop("Grouping columns conflict with reserved prepayment output names; rename them first.")
  }

  # Validate EFFDATE is in group_vars (critical for lag operation)
  if (!col_effdate %in% group_vars) {
    stop(
      "The effective date column ('", col_effdate, "') must be included in group_vars. ",
      "Without it, the lag operation for calculating beginning balances is meaningless. ",
      "Add '", col_effdate, "' to your group_vars parameter."
    )
  }

  # Validate logical parameters
  if (!is.logical(allow_negative_prepay) || length(allow_negative_prepay) != 1L || is.na(allow_negative_prepay)) {
    stop("'allow_negative_prepay' must be TRUE or FALSE")
  }

  # Validate numeric parameters
  if (!is.numeric(min_begin_balance) || length(min_begin_balance) != 1L ||
      !is.finite(min_begin_balance) || min_begin_balance < 0) {
    stop("'min_begin_balance' must be a non-negative numeric value")
  }

  # ========================================================================
  # DATA TRANSFORMATION
  # ========================================================================

  # Rename columns to standard internal names for clean logic
  rename_list <- list(
    EFFDATE = col_effdate,
    ORIGDATE = col_origdate,
    TYPECODE = col_typecode,
    BAL = col_balance,
    ORIGBAL = col_orig_balance,
    PAYAMT = col_payment,
    CURRINTRATE = col_rate
  )

  # Add interest basis column to rename list if using loan-level
  if (use_loan_level_basis) {
    rename_list$INTEREST_BASIS <- col_interest_basis
  }

  # Add loan ID column to rename list if provided
  if (use_loan_id) {
    rename_list$LOANID <- col_loanid
  }

  df <- df %>%
    rename(!!!rename_list)

  # Save original group_vars for output renaming later
  group_vars_original <- group_vars

  # Translate group_vars to internal names for processing
  # Create mapping: user's column name -> internal standard name
  column_mapping <- setNames(
    c("EFFDATE", "ORIGDATE", "TYPECODE", "BAL", "ORIGBAL", "PAYAMT", "CURRINTRATE"),
    c(col_effdate, col_origdate, col_typecode, col_balance, col_orig_balance, col_payment, col_rate)
  )

  # If loan ID provided, add to mapping
  if (use_loan_id) {
    column_mapping <- c(column_mapping, setNames("LOANID", col_loanid))
  }

  group_vars <- sapply(group_vars, function(gv) {
    if (gv %in% names(column_mapping)) {
      return(unname(column_mapping[gv]))
    } else {
      return(gv)  # Keep as-is (wasn't a renamed column)
    }
  }, USE.NAMES = FALSE)

  # Convert date columns and validate
  df <- df %>%
    mutate(
      EFFDATE = as.Date(EFFDATE),
      ORIGDATE = as.Date(ORIGDATE)
    )

  if (any(!is.finite(as.numeric(df$EFFDATE)))) {
    stop("Unable to convert 'col_effdate' to Date format. Check date values.")
  }

  if (any(!is.finite(as.numeric(df$ORIGDATE)))) {
    stop("Unable to convert 'col_origdate' to Date format. Check date values.")
  }

  reporting_dates <- validate_prepay_periods(df$EFFDATE)
  for (col in c("BAL", "ORIGBAL", "PAYAMT", "CURRINTRATE")) {
    values <- df[[col]]
    if (!is.numeric(values) || !is.null(dim(values)) || any(!is.finite(values)) || any(values < 0)) {
      stop(col, " must contain finite, non-missing, non-negative numeric values.")
    }
  }
  if (use_loan_id && (anyNA(df$LOANID) || any(!nzchar(trimws(as.character(df$LOANID)))))) {
    stop("Loan IDs must be non-missing and non-empty for exit diagnostics.")
  }

  # Check for duplicate loans if col_loanid provided
  if (use_loan_id) {
    duplicates <- df %>%
      group_by(EFFDATE, LOANID) %>%
      filter(n() > 1) %>%
      ungroup()

    if (nrow(duplicates) > 0) {
      n_dups <- duplicates %>%
        distinct(EFFDATE, LOANID) %>%
        nrow()

      stop(
        "Found ", n_dups, " duplicate loan(s) within the same reporting period. ",
        "Each loan should appear only once per EFFDATE. ",
        "Check for duplicate records in your data. ",
        "First few duplicates:\n",
        paste(utils::capture.output(print(head(duplicates %>%
                                                 select(EFFDATE, LOANID)))), collapse = "\n")
      )
    }

    if (verbose) {
      message("Validated: No duplicate loans within reporting periods")
    }
  }

  # Validate and convert interest rates if needed
  if (any(df$CURRINTRATE > 1, na.rm = TRUE)) {
    max_rate <- max(df$CURRINTRATE, na.rm = TRUE)

    # Check if it looks like percentages (most rates 1-20, not 0.01-0.20)
    if (max_rate > 1 && max_rate < 100) {
      warning(
        "Interest rates appear to be in percentage form (max rate: ",
        round(max_rate, 2), "%). ",
        "Rates should be in decimal form (e.g., 0.0729 for 7.29%). ",
        "Converting by dividing by 100.",
        call. = FALSE
      )
      df <- df %>%
        mutate(CURRINTRATE = CURRINTRATE / 100)
    } else if (max_rate >= 100) {
      stop(
        "Interest rates are unexpectedly high (max rate: ", round(max_rate, 2), "). ",
        "Rates should be in decimal form (e.g., 0.0729 for 7.29%). ",
        "Please verify your data."
      )
    }
  }

  # Ensure EFFDATE is properly ordered for lag operation
  # Sort by EFFDATE first, then by other cohort identifiers
  non_date_groups <- setdiff(group_vars, "EFFDATE")

  if (length(non_date_groups) > 0) {
    df <- df %>%
      arrange(EFFDATE, !!!syms(non_date_groups))
  } else {
    df <- df %>%
      arrange(EFFDATE)
  }

  # If loan ID provided, also sort by LOANID for loan-level lag
  if (use_loan_id) {
    df <- df %>%
      arrange(LOANID, EFFDATE)
  }

  # ========================================================================
  # INTEREST CALCULATION
  # ========================================================================

  if (use_loan_level_basis) {
    # Loan-level interest basis: each loan uses its own basis value
    df <- df %>%
      mutate(
        DAYS_IN_MONTH = days_in_month(EFFDATE),
        # Calculate MONTHLY_RATE based on each loan's interest basis
        MONTHLY_RATE = case_when(
          # Valid loan-level basis (360 or 365): use daily interest
          INTEREST_BASIS %in% c(360, 365) ~ CURRINTRATE * (DAYS_IN_MONTH / INTEREST_BASIS),
          # Invalid or missing basis: use simple monthly
          TRUE ~ CURRINTRATE / 12
        )
      )

    # Report summary of interest calculation methods used
    basis_summary <- df %>%
      group_by(INTEREST_BASIS) %>%
      summarise(n_loans = n(), .groups = "drop") %>%
      arrange(INTEREST_BASIS)

    if (verbose) {
      message("Using loan-level interest basis:")
      for (i in seq_len(nrow(basis_summary))) {
        basis_val <- basis_summary$INTEREST_BASIS[i]
        n_loans <- basis_summary$n_loans[i]
        if (basis_val %in% c(360, 365)) {
          message("  - ", n_loans, " loans with ", basis_val, "-day basis (daily interest)")
        } else {
          message("  - ", n_loans, " loans with invalid/missing basis (simple monthly)")
        }
      }
    }

  } else {
    # Global interest basis: all loans use same method
    if (!is.null(interest_basis) && !is.na(interest_basis) &&
        is.numeric(interest_basis) && interest_basis %in% c(360, 365)) {

      # Daily interest method: (Rate * Days in Month) / Interest Basis
      df <- df %>%
        mutate(
          DAYS_IN_MONTH = days_in_month(EFFDATE),
          MONTHLY_RATE = CURRINTRATE * (DAYS_IN_MONTH / interest_basis)
        )

      if (verbose) {
        message("Using global daily interest calculation with ", interest_basis, "-day basis")
      }

    } else {

      # Simple monthly method: Rate / 12
      df <- df %>%
        mutate(MONTHLY_RATE = CURRINTRATE / 12)

      if (verbose) {
        message("Using simple monthly interest calculation (RATE / 12)")
      }
    }
  }

  # ========================================================================
  # SCHEDULED PRINCIPAL CALCULATION
  # ========================================================================

  if (use_loan_id) {
    # With loan ID: Use beginning balance for accurate scheduled principal
    df <- df %>%
      group_by(LOANID) %>%
      mutate(
        BEGIN_BAL_LOAN = if_else(
          (year(EFFDATE) * 12 + month(EFFDATE)) - lag(year(EFFDATE) * 12 + month(EFFDATE)) == 1,
          lag(BAL), NA_real_
        )
      ) %>%
      ungroup() %>%
      mutate(
        # Scheduled principal = Payment - Interest on beginning balance
        # Floor at 0 to handle interest-only periods or payment < interest scenarios
        SCHEDPRIN = pmax(
          0,
          ifelse(
            !is.na(PAYAMT) & !is.na(BEGIN_BAL_LOAN) & !is.na(MONTHLY_RATE),
            PAYAMT - (BEGIN_BAL_LOAN * MONTHLY_RATE),
            NA_real_
          )
        )
      )

    if (verbose) {
      message("Using beginning-of-period balance for scheduled principal calculation (accurate method)")
    }

  } else {
    # Without loan ID: Use ending balance as approximation
    df <- df %>%
      mutate(
        # Scheduled principal = Payment - Interest on ending balance (approximation)
        # Floor at 0 to handle interest-only periods or payment < interest scenarios
        SCHEDPRIN = pmax(
          0,
          ifelse(
            !is.na(PAYAMT) & !is.na(BAL) & !is.na(MONTHLY_RATE),
            PAYAMT - (BAL * MONTHLY_RATE),
            NA_real_
          )
        )
      )

    if (verbose) {
      message("Using end-of-period balance for scheduled principal calculation (approximation)")
      message("Note: Provide col_loanid for more accurate scheduled principal calculation")
    }
  }

  # ========================================================================
  # STEP 1: MONTHLY BALANCE SNAPSHOT
  # ========================================================================

  balance_snapshot <- prepay_snapshot_pairs(df, group_vars, reporting_dates, use_loan_id)

  # ========================================================================
  # STEP 2: FUNDED BALANCE FROM NEW ORIGINATIONS
  # ========================================================================

  funded_summary <- df %>%
    # Capture loans originated in the same month/year as reporting period
    filter(
      year(ORIGDATE) == year(EFFDATE) &
        month(ORIGDATE) == month(EFFDATE)
    ) %>%
    group_by(!!!syms(group_vars)) %>%
    summarise(
      FUNDED_BAL = sum(ORIGBAL, na.rm = TRUE),
      .groups = "drop"
    )

  # ========================================================================
  # STEP 3: FINAL PREPAYMENT CALCULATION
  # ========================================================================

  df_summary <- balance_snapshot %>%
    left_join(funded_summary, by = group_vars) %>%
    mutate(
      # Loans not funded in this period get 0
      FUNDED_BAL = coalesce(FUNDED_BAL, 0),
      # Principal flow: Beginning - Ending + New Money
      ACTUAL_PRIN = BEGIN_BAL - END_BAL + FUNDED_BAL,
      # Prepayment: Principal paid beyond scheduled amount
      PREPAYMENT = ACTUAL_PRIN - SCHED_PRIN_TOTAL,
      # Attribution is withheld below when snapshot membership is unresolved.
      AVAILABLE_TO_PREPAY = BEGIN_BAL - SCHED_PRIN_TOTAL
    )
  df_summary <- add_prepay_diagnostics(df_summary, allow_negative_prepay)

  # Apply minimum balance filter
  if (min_begin_balance > 0) {
    rows_before <- nrow(df_summary)
    df_summary <- df_summary %>%
      filter(is.na(BEGIN_BAL) | BEGIN_BAL >= min_begin_balance | DIAGNOSTIC != "ok")
    rows_after <- nrow(df_summary)

    if (rows_before > rows_after && verbose) {
      message(
        "Filtered out ", rows_before - rows_after,
        " cohorts with beginning balance below $",
        format(min_begin_balance, big.mark = ",", scientific = FALSE)
      )
    }
  }

  # ========================================================================
  # FINALIZE OUTPUT
  # ========================================================================

  df_summary <- df_summary %>%
    select(
      all_of(group_vars),
      BEGIN_BAL,
      END_BAL,
      SCHED_PRIN_TOTAL,
      FUNDED_BAL,
      ACTUAL_PRIN,
      PREPAYMENT,
      SMM,
      CPR,
      AVAILABLE_TO_PREPAY,
      SMM_RAW,
      SMM_ADJUSTED,
      COHORT_DISAPPEARED,
      UNRESOLVED_EXITS,
      UNRESOLVED_ENTRIES,
      DIAGNOSTIC
    ) %>%
    arrange(across(all_of(group_vars)))

  n_undefined <- sum(is.na(df_summary$SMM))
  if (n_undefined > 0) {
    warning(n_undefined, " cohort-period(s) have undefined prepayment estimates. ",
            "Inspect DIAGNOSTIC; snapshot exits are not assumed to be payoffs.", call. = FALSE)
  }

  # Rename group columns back to user's original names
  if (length(group_vars) == length(group_vars_original)) {
    rename_back <- setNames(group_vars, group_vars_original)
    df_summary <- df_summary %>%
      rename(!!!rename_back)
  }

  return(df_summary)
}
