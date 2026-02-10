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
#' month = 1 corresponds to time 0 (the effective date). Timing calculations use
#' t = (month - 1) / 12. This means month = 1 has t = 0 years, month = 2 has
#' t = 1/12 years (one month from eff_date), and so on.
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
                          principal_column = "total_principal") {

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

  # Calculate time periods using month column ----
  wal_data <- wal_data %>%
    mutate(
      t_months = month - 1,
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
