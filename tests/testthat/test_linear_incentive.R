mini <- function() data.frame(
  LOAN_ID = c("L1", "L2"), balance = 10000,
  current_interest_rate = 0.06, months_to_maturity = 12,
  eff_date = as.Date("2025-01-01"), stringsAsFactors = FALSE
)

test_that("linear_incentive CPR matches hand calculations", {
  loans <- data.frame(
    LOAN_ID = paste0("L", 1:5), balance = 10000,
    current_interest_rate = c(0.07, 0.05, 0.20, 0.045, 0.02),
    months_to_maturity = 12, eff_date = as.Date("2025-01-01"),
    tier = c("A", "A", "A", "B", "B"), stringsAsFactors = FALSE
  )
  cfg <- list(
    prepay_model = "linear_incentive", col_tier = "tier",
    col_rate = "current_interest_rate", current_market_rate = 0.05,
    base_cpr_vec = c(A = 0.06, B = 0.04), beta_vec = c(A = 2.0, B = 1.5),
    cpr_min_vec = c(A = 0.02, B = 0.01), cpr_max_vec = c(A = 0.30, B = 0.25)
  )
  # A: .07->.10, .05->.06, .20->clamp .30 ; B: .045->.0325, .02->clamp .01
  expect_equal(resolve_prepay_cpr(loans, cfg, default_tier = "A"),
               c(0.10, 0.06, 0.30, 0.0325, 0.01), tolerance = 1e-12)
})

test_that("linear_incentive falls back to default tier for unknown tiers", {
  loans <- data.frame(current_interest_rate = 0.07, tier = "Z",
                      stringsAsFactors = FALSE)
  cfg <- list(
    prepay_model = "linear_incentive", col_tier = "tier",
    col_rate = "current_interest_rate", current_market_rate = 0.05,
    base_cpr_vec = c(A = 0.06, B = 0.04), beta_vec = c(A = 2.0, B = 1.5),
    cpr_min_vec = c(A = 0.02, B = 0.01), cpr_max_vec = c(A = 0.30, B = 0.25)
  )
  expect_equal(resolve_prepay_cpr(loans, cfg, default_tier = "A"), 0.10)
})

test_that("linear_incentive requires current_market_rate", {
  cfg <- list(prepay_model = "linear_incentive", col_tier = NULL,
              base_cpr_vec = c(default = 0.06), beta_vec = c(default = 2.0),
              cpr_min_vec = c(default = 0.02), cpr_max_vec = c(default = 0.30),
              credit_cost_vec = c(default = 0.01))
  expect_error(calculate_cash_flows(mini(), cfg), "current_market_rate")
})

test_that("linear_incentive errors when cpr_min > cpr_max", {
  cfg <- list(prepay_model = "linear_incentive", col_tier = NULL,
              current_market_rate = 0.05,
              base_cpr_vec = c(default = 0.06), beta_vec = c(default = 2.0),
              cpr_min_vec = c(default = 0.30), cpr_max_vec = c(default = 0.10),
              credit_cost_vec = c(default = 0.01))
  expect_error(calculate_cash_flows(mini(), cfg), "cpr_min")
})

test_that("linear_incentive warns on negative beta", {
  cfg <- list(prepay_model = "linear_incentive", col_tier = NULL,
              current_market_rate = 0.05,
              base_cpr_vec = c(default = 0.06), beta_vec = c(default = -1.0),
              cpr_min_vec = c(default = 0.02), cpr_max_vec = c(default = 0.30),
              credit_cost_vec = c(default = 0.01))
  expect_warning(calculate_cash_flows(mini(), cfg), "egative beta")
})
