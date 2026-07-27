#' Calculate Portfolio Weighted Average Life (WAL)
#'
#' Calculates the weighted average life for a loan portfolio based on projected
#' principal cash flows from \code{calculate_cash_flows()}. WAL measures the
#' average time (in years) until principal is repaid, weighted by the amount
#' of principal in each payment.
#'
#' @param loan_cash_flows Data frame. Output from \code{calculate_cash_flows()}
#'   containing projected loan cash flows with required columns: LOAN_ID,
#'   eff_date, date, month, total_principal, investor_principal.
#' @param principal_column Character. Column name for principal cash flows to use.
#'   Options:
#'   \itemize{
#'     \item "total_principal" (default): Full principal cash flows regardless
#'       of ownership structure
#'     \item "investor_principal": Investor's share of principal cash flows
#'       after applying investor_share percentage
#'   }
#' @param validate_month_index Logical. If TRUE (default), errors when the month
#'   column does not begin at 1 or is not contiguous. Set FALSE to analyse a
#'   deliberately offset or filtered projection window.
#'
#' @return A single-row data frame with portfolio-level metric:
#'   \itemize{
#'     \item portfolio_wal: Weighted average life in years
#'   }
#'
#' @details
#' WAL measures the time-weighted principal repayments and does not include
#' interest payments or apply discounting. Unlike duration, WAL is not sensitive
#' to interest rates—it purely reflects the timing and amount of principal
#' repayment.
#'
#' Time measurement: The month column is treated as the projection index, where
#' month = 1 is the first projected principal payment, one month after eff_date.
#' Timing uses t = month / 12, so month = 1 has t = 1/12 years and month = 12 has t = 1 year.
#'
#' The function calculates:
#' \itemize{
#'   \item Loan-level WAL = Σ(t × Principal_t) / Σ(Principal_t), where t is
#'     time in years from eff_date
#'   \item Portfolio WAL = weighted average of loan-level WALs, weighted by
#'     total principal
#' }
#'
#' WAL is commonly used to:
#' \itemize{
#'   \item Assess prepayment risk and portfolio turnover
#'   \item Compare against duration to understand interest vs principal timing
#'   \item Evaluate reinvestment risk in different rate environments
#' }
#'
#' @examples
#' \dontrun{
#' # Calculate WAL using total principal
#' wal_result <- calculate_wal(loan_cash_flows)
#'
#' # Calculate WAL for investor's economic interest
#' wal_result <- calculate_wal(
#'   loan_cash_flows,
#'   principal_column = "investor_principal"
#' )
#' }
#'
#' @importFrom stats weighted.mean
#' @export
calculate_wal <- function(loan_cash_flows,
                          principal_column = "total_principal",
                          validate_month_index = TRUE
                          ) {

  # Input validation ----
  if (!is.data.frame(loan_cash_flows)) {
    stop("loan_cash_flows must be a data frame")
  }

  required_cols <- c("LOAN_ID", "eff_date", "date", "month",
                     "total_principal", "investor_principal")
  missing_cols <- setdiff(required_cols, names(loan_cash_flows))
  if (length(missing_cols) > 0) {
    stop("loan_cash_flows is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  if (!principal_column %in% c("total_principal", "investor_principal")) {
    stop("principal_column must be either 'total_principal' or 'investor_principal'")
  }

  if (!is.logical(validate_month_index) || length(validate_month_index) != 1) {
    stop("validate_month_index must be TRUE or FALSE")
  }

  # Ensure required columns are not all NA
  if (all(is.na(loan_cash_flows$LOAN_ID))) {
    stop("LOAN_ID column contains only NA values")
  }

  if (all(is.na(loan_cash_flows[[principal_column]]))) {
    stop(principal_column, " column contains only NA values")
  }

  # Data preparation ----
  # Remove rows with missing critical data
  wal_data <- loan_cash_flows %>%
    filter(!is.na(LOAN_ID),
           !is.na(eff_date),
           !is.na(date),
           !is.na(month),
           !is.na(.data[[principal_column]])) %>%
    mutate(
      eff_date = as.Date(eff_date),
      date = as.Date(date)
    )

  if (nrow(wal_data) == 0) {
    stop("No valid rows remaining after removing missing values in required columns")
  }

  # Validate date conversion succeeded ----
  if (any(is.na(wal_data$eff_date)) || any(is.na(wal_data$date))) {
    stop("Some date values could not be converted to Date format. Check eff_date and date columns.")
  }

  # Validate month column ----
  if (any(wal_data$month < 1, na.rm = TRUE)) {
    stop("month column contains values less than 1. Month must be >= 1")
  }

  if (any(wal_data$month != floor(wal_data$month), na.rm = TRUE)) {
    stop("month column contains non-integer values. Month must be an integer >= 1")
  }
  # Projection-index guard ----
  # Checked on the raw input, not the NA-filtered frame: a dropped row must not
  # be able to shift the apparent start of the index.
  if (validate_month_index) {
    observed <- sort(unique(loan_cash_flows$month[!is.na(loan_cash_flows$month)]))

    if (min(observed) != 1) {
      stop("month index starts at ", min(observed), ", expected 1. ",
           "As of v0.2.5.1 the month = 1 -> t = 1 month convention is handled ",
           "internally. If this reflects the mutate(month = month + 1) workaround ",
           "for the pre-v0.2.5.1 timing bug, remove it. To analyse a deliberately ",
           "offset or filtered projection, set validate_month_index = FALSE.")
    }

    expected <- seq_len(max(observed))
    if (length(observed) != length(expected) || any(observed != expected)) {
      gaps <- setdiff(expected, observed)
      stop("month index is not contiguous from 1. Missing month(s): ",
           paste(gaps[seq_len(min(10L, length(gaps)))], collapse = ", "),
           if (length(gaps) > 10L) ", ..." else "",
           ". Set validate_month_index = FALSE to override.")
    }
  }

  # Calculate time periods using month column ----
  wal_data <- wal_data %>%
    mutate(
      t_months = month,
      t_years = t_months / 12,
      weighted_principal = t_years * .data[[principal_column]]
    )

  # Aggregate at loan level ----
  loan_level <- wal_data %>%
    group_by(LOAN_ID) %>%
    summarise(
      principal_sum = sum(.data[[principal_column]], na.rm = TRUE),
      weighted_principal_sum = sum(weighted_principal, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      loan_wal = weighted_principal_sum / principal_sum
    )

  # Filter out loans with zero principal and warn if any removed ----
  zero_principal_loans <- loan_level %>% filter(principal_sum == 0)
  if (nrow(zero_principal_loans) > 0) {
    warning("Removed ", nrow(zero_principal_loans),
            " loan(s) with zero ", principal_column, " from WAL calculation")
  }
  loan_level <- loan_level %>% filter(principal_sum > 0)

  # Check for remaining loans
  if (nrow(loan_level) == 0) {
    stop("No loans remaining after filtering zero principal. Check principal cash flows in ",
         principal_column)
  }

  # Check for total principal sum
  total_principal_sum <- sum(loan_level$principal_sum, na.rm = TRUE)

  if (total_principal_sum == 0) {
    stop("Total principal is zero. Check principal cash flows in ", principal_column)
  }

  # Calculate portfolio-level WAL ----
  portfolio_wal <- weighted.mean(loan_level$loan_wal,
                                 loan_level$principal_sum,
                                 na.rm = TRUE)

  # Build output ----
  result <- data.frame(
    portfolio_wal = portfolio_wal
  )

  return(result)
}
