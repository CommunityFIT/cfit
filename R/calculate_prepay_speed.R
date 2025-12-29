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
#'       Cohorts with beginning balance below this value are excluded from results (default: 0)}
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
#'     \item{\code{ACTUAL_PRIN}}{Actual principal paid (including prepayments)}
#'     \item{\code{PREPAYMENT}}{Principal paid in excess of scheduled amount}
#'     \item{\code{SMM}}{Single monthly mortality (0 to 1 scale)}
#'     \item{\code{CPR}}{Conditional prepayment rate, annualized from SMM (0 to 1 scale)}
#'   }
#'
#' @details
#'
#' \strong{Methodology:}
#'
#' SMM measures the monthly prepayment rate relative to the pool available to prepay
#' (beginning balance minus scheduled principal):
#' \deqn{SMM = \frac{PREPAYMENT}{BEGIN\_BAL - SCHED\_PRIN\_TOTAL}}{SMM = PREPAYMENT / (BEGIN_BAL - SCHED_PRIN_TOTAL)}
#'
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
#'   \item Balances and payments should be positive numeric values
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
#'   \item NA values in balance or payment columns are removed with \code{na.rm = TRUE}
#'   \item Cohorts with beginning balance below \code{min_begin_balance} are filtered out
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

  # Validate EFFDATE is in group_vars (critical for lag operation)
  if (!col_effdate %in% group_vars) {
    stop(
      "The effective date column ('", col_effdate, "') must be included in group_vars. ",
      "Without it, the lag operation for calculating beginning balances is meaningless. ",
      "Add '", col_effdate, "' to your group_vars parameter."
    )
  }

  # Validate logical parameters
  if (!is.logical(allow_negative_prepay)) {
    stop("'allow_negative_prepay' must be TRUE or FALSE")
  }

  # Validate numeric parameters
  if (!is.numeric(min_begin_balance) || min_begin_balance < 0) {
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

  if (any(is.na(df$EFFDATE))) {
    stop("Unable to convert 'col_effdate' to Date format. Check date values.")
  }

  if (any(is.na(df$ORIGDATE))) {
    stop("Unable to convert 'col_origdate' to Date format. Check date values.")
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
        BEGIN_BAL_LOAN = lag(BAL)
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

  balance_snapshot <- df %>%
    group_by(!!!syms(group_vars)) %>%
    summarise(
      END_BAL = sum(BAL, na.rm = TRUE),
      SCHED_PRIN_TOTAL = sum(SCHEDPRIN, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(!!!syms(group_vars)) %>%
    # Lag within cohort (by all group_vars except EFFDATE)
    group_by(!!!syms(setdiff(group_vars, "EFFDATE"))) %>%
    mutate(
      BEGIN_BAL = lag(END_BAL)
    ) %>%
    ungroup() %>%
    # Remove first month (no prior balance for lagging)
    filter(!is.na(BEGIN_BAL))

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
      # Pool available to prepay (beginning balance minus scheduled principal) Industry standard: scheduled principal was going to paid anyway
      AVAILABLE_TO_PREPAY = pmax(BEGIN_BAL - SCHED_PRIN_TOTAL, 0),
      # Single Monthly Mortality: prepayment as % of available pool
      SMM = PREPAYMENT / AVAILABLE_TO_PREPAY
    )

  # Apply SMM clamping and CPR calculation based on allow_negative_prepay
  if (!allow_negative_prepay) {
    # Default behavior: Floor negative prepayments at 0
    df_summary <- df_summary %>%
      mutate(
        # Clamp SMM to [0, 1] to prevent mathematical errors
        SMM_CLAMPED = pmax(0, pmin(1, SMM)),
        # Calculate CPR from clamped SMM
        CPR = 1 - (1 - SMM_CLAMPED)^12,
        # Floor SMM at 0 for output
        SMM = pmax(0, SMM),
        # Floor CPR at 0 for output
        CPR = pmax(0, CPR)
      ) %>%
      select(-SMM_CLAMPED, -AVAILABLE_TO_PREPAY)

  } else {
    # Allow negative prepayments: Use raw SMM for CPR but still prevent extreme values
    df_summary <- df_summary %>%
      mutate(
        # Clamp SMM to [-1, 1] to prevent NaN/Inf but allow negative
        SMM_CLAMPED = pmax(-1, pmin(1, SMM)),
        # Calculate CPR from clamped SMM (can be negative)
        CPR = 1 - (1 - SMM_CLAMPED)^12
      ) %>%
      select(-SMM_CLAMPED, -AVAILABLE_TO_PREPAY)
    # Keep raw SMM and CPR values (can be negative)
  }

  # Apply minimum balance filter
  if (min_begin_balance > 0) {
    rows_before <- nrow(df_summary)
    df_summary <- df_summary %>%
      filter(BEGIN_BAL >= min_begin_balance)
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
      CPR
    )

  # Rename group columns back to user's original names
  if (length(group_vars) == length(group_vars_original)) {
    rename_back <- setNames(group_vars, group_vars_original)
    df_summary <- df_summary %>%
      rename(!!!rename_back)
  }

  return(df_summary)
}
