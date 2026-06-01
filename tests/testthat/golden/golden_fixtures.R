golden_portfolio <- function() {
  data.frame(
    LOAN_ID               = c("L001","L002","L003","L004","L005","L006"),
    balance               = c(10000, 25000, 5000, 100, 50000, 8000),
    current_interest_rate = c(0.06, 0.0725, 0.00, 0.055, 0.069, 0.0599), # incl. a 0% loan
    months_to_maturity    = c(12, 60, 24, 6, 48, 36),                    # short + long
    eff_date              = as.Date("2025-01-01"),
    tier                  = c("A","B","A","C","B","A"),
    stringsAsFactors      = FALSE
  )
}

golden_cases <- function() {
  port <- golden_portfolio()
  list(
    no_tier_default = list(
      data   = port,
      config = list(col_tier = NULL,
                    cpr_vec = c(default = 0.08),
                    credit_cost_vec = c(default = 0.015))
    ),
    tiered = list(
      data   = port,
      config = list(col_tier = "tier",
                    cpr_vec = c(A = 0.06, B = 0.10, C = 0.03),
                    credit_cost_vec = c(A = 0.01, B = 0.02, C = 0.005))
    ),
    pd_lgd = list(
      data   = port,
      config = list(col_tier = "tier",
                    cpr_vec = c(A = 0.06, B = 0.10, C = 0.03),
                    pd_vec  = c(A = 0.02, B = 0.04, C = 0.01),
                    lgd_vec = c(A = 0.45, B = 0.55, C = 0.40))
    ),
    monthly_totals = list(
      data   = port,
      config = list(col_tier = "tier",
                    cpr_vec = c(A = 0.06, B = 0.10, C = 0.03),
                    credit_cost_vec = c(A = 0.01, B = 0.02, C = 0.005),
                    return_monthly_totals = TRUE,
                    monthly_totals_group_vars = "tier")
    )
  )
}
