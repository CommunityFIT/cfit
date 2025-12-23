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
  "DAYS_IN_MONTH",
  "MONTHLY_RATE",
  "SCHEDPRIN",
  "END_BAL",
  "BEGIN_BAL",
  "SCHED_PRIN_TOTAL",
  "FUNDED_BAL",
  "ACTUAL_PRIN",
  "PREPAYMENT",
  "SMM",
  "SMM_CLAMPED",  # NEW: Add this
  "CPR"
))
