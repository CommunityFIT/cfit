# Global variables for R CMD check
utils::globalVariables(c(
  "LOAN_ID",
  "SMM_RAW", "SMM_ADJUSTED", "COHORT_DISAPPEARED",
  "UNRESOLVED_EXITS", "UNRESOLVED_ENTRIES", "DIAGNOSTIC",
  "rate",
  "eff_date",
  "date",
  "month",
  "t_months",
  "t_years",
  "discount_rate_used",
  "pv",
  "pv_weighted_time",
  "loan_pv",
  "loan_modified_duration",
  "convexity_term",
  "weighted_principal",
  "weighted_principal_sum",
  "principal_sum"
))
