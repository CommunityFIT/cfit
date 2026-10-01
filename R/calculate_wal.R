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
#'   column for any individual loan does not begin at 1 or is not contiguous.
#'   Set FALSE to analyse a deliberately offset or filtered projection window.
#'   Duplicate loan-month records and invalid data are always rejected.
#'
#' @return A single-row data frame with portfolio-level metric:
#'   \itemize{
#'     \item portfolio_wal: Weighted average life in years
#'   }
#'
#' @details
#' Input must be non-empty and use one common eff_date across the portfolio.
#' IDs and dates must be valid and non-missing; the selected cash-flow amounts
#' must be finite, non-negative numbers. Missing rows are never silently removed.
#' Month indices must be finite positive integers, unique within each loan;
#' loans may have different final months and input rows may be unordered.
#' Zero-weight loans continue to be excluded with a warning.
#' Timing is determined by month / 12, not by actual calendar day counts.
#' The sequence check cannot detect a missing final payment without contractual
#' maturity information; an incomplete projection can still produce a metric.
#'
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

  if (!is.character(principal_column) || length(principal_column) != 1L ||
      is.na(principal_column) || !principal_column %in% c("total_principal", "investor_principal")) {
    stop("principal_column must be either 'total_principal' or 'investor_principal'")
  }
  wal_data <- validate_analytics_data(
    loan_cash_flows, principal_column,
    c("LOAN_ID", "eff_date", "date", "month", "total_principal", "investor_principal"),
    validate_month_index
  )

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
      principal_sum = sum(.data[[principal_column]]),
      weighted_principal_sum = sum(weighted_principal),
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
  total_principal_sum <- sum(loan_level$principal_sum)

  if (total_principal_sum == 0) {
    stop("Total principal is zero. Check principal cash flows in ", principal_column)
  }

  # Calculate portfolio-level WAL ----
  portfolio_wal <- weighted.mean(loan_level$loan_wal,
                                 loan_level$principal_sum)

  # Build output ----
  result <- data.frame(
    portfolio_wal = portfolio_wal
  )

  if (any(!is.finite(as.matrix(result)))) {
    stop("Analytics produced non-finite results; check input magnitudes and discount rates.")
  }
  return(result)
}
