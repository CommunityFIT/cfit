#' @importFrom utils modifyList
NULL

# Suppress R CMD check NOTEs for non-standard evaluation in dplyr
# These are column names used in data frame operations
utils::globalVariables(c(
  "EFFDATE",
  "ORIGDATE",
  "TYPECODE",
  "BAL",
  "ORIGBAL",
  "PAYAMT",
  "CURRINTRATE",
  "INTEREST_BASIS",
  "LOANID",                # Added for loan ID support
  "DAYS_IN_MONTH",
  "MONTHLY_RATE",
  "BEGIN_BAL_LOAN",        # Added for loan-level beginning balance
  "SCHEDPRIN",
  "END_BAL",
  "BEGIN_BAL",
  "SCHED_PRIN_TOTAL",
  "FUNDED_BAL",
  "ACTUAL_PRIN",
  "PREPAYMENT",
  "AVAILABLE_TO_PREPAY",   # Added for adjusted SMM denominator
  "SMM",
  "SMM_CLAMPED",
  "CPR"
))
