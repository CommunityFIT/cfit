# Test file for calculate_cash_flows()

library(testthat)
library(dplyr)

# Helper function to create simple test data
create_test_portfolio <- function(n_loans = 3) {
  data.frame(
    LOAN_ID = paste0("L", sprintf("%03d", 1:n_loans)),
    balance = rep(10000, n_loans),
    current_interest_rate = rep(0.06, n_loans),
    months_to_maturity = rep(12, n_loans),
    eff_date = as.Date("2025-01-01"),
    stringsAsFactors = FALSE
  )
}

# Test 1: Basic functionality with default config
test_that("calculate_cash_flows returns data frame with default config", {
  loan_data <- create_test_portfolio(2)

  result <- calculate_cash_flows(loan_data, config = list())

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("LOAN_ID" %in% names(result))
  expect_true("date" %in% names(result))
  expect_true("investor_total" %in% names(result))
})

# Test 2: Return structure with monthly_totals = TRUE
test_that("calculate_cash_flows returns list when return_monthly_totals = TRUE", {
  loan_data <- create_test_portfolio(2)

  config <- list(return_monthly_totals = TRUE)
  result <- calculate_cash_flows(loan_data, config)

  expect_type(result, "list")
  expect_true("loan_cash_flows" %in% names(result))
  expect_true("monthly_totals" %in% names(result))
  expect_s3_class(result$loan_cash_flows, "data.frame")
  expect_s3_class(result$monthly_totals, "data.frame")
})

# Test 3: Zero CPR and credit costs (conservative scenario)
test_that("calculate_cash_flows works with zero CPR and credit costs", {
  loan_data <- create_test_portfolio(1)

  config <- list(
    cpr_vec = c("default" = 0.0),
    credit_cost_vec = c("default" = 0.0),
    servicing_fee = 0.0,
    return_monthly_totals = TRUE
  )

  result <- calculate_cash_flows(loan_data, config)

  # With zero credit costs and prepayment, no credit losses or prepayments
  expect_equal(sum(result$loan_cash_flows$credit_loss), 0)
  expect_equal(sum(result$loan_cash_flows$prepayment), 0)

  # Total payments should equal roughly principal + interest
  total_payments <- sum(result$monthly_totals$total_payment)
  expect_gt(total_payments, loan_data$balance[1])  # Should be > principal
})

# Test 4: Tier-based differentiation
test_that("calculate_cash_flows handles tier-based assumptions", {
  loan_data <- data.frame(
    LOAN_ID = c("L001", "L002"),
    balance = c(10000, 10000),
    current_interest_rate = c(0.06, 0.06),
    months_to_maturity = c(12, 12),
    eff_date = as.Date("2025-01-01"),
    tier = c("A", "B"),
    stringsAsFactors = FALSE
  )

  config <- list(
    col_tier = "tier",
    cpr_vec = c("A" = 0.05, "B" = 0.10),  # Different CPRs
    credit_cost_vec = c("A" = 0.01, "B" = 0.02)  # Different credit costs
  )

  result <- calculate_cash_flows(loan_data, config)

  # Should have cash flows for both loans
  loan_ids <- unique(result$LOAN_ID)
  expect_equal(length(loan_ids), 2)
  expect_true(all(c("L001", "L002") %in% loan_ids))
})

# Test 5: Portfolio without tier column
test_that("calculate_cash_flows works without tier column", {
  loan_data <- create_test_portfolio(2)

  config <- list(
    col_tier = NULL,
    cpr_vec = c("default" = 0.05),
    credit_cost_vec = c("default" = 0.01)
  )

  result <- calculate_cash_flows(loan_data, config)

  expect_s3_class(result, "data.frame")
  expect_equal(length(unique(result$LOAN_ID)), 2)
})

# Test 6: PD/LGD approach overrides credit_cost_vec
test_that("PD/LGD vectors override credit_cost_vec", {
  loan_data <- create_test_portfolio(1)

  config <- list(
    credit_cost_vec = c("default" = 0.05),  # Should be ignored
    pd_vec = c("default" = 0.10),
    lgd_vec = c("default" = 0.50),  # 10% * 50% = 5% credit cost
    return_monthly_totals = TRUE
  )

  expect_warning(
    result <- calculate_cash_flows(loan_data, config),
    "Using PD/LGD approach"
  )

  # Credit losses should reflect PD * LGD
  total_credit_loss <- sum(result$monthly_totals$credit_loss)
  expect_gt(total_credit_loss, 0)
})

# Test 7: Missing required columns
test_that("calculate_cash_flows errors on missing required columns", {
  loan_data <- data.frame(
    LOAN_ID = "L001",
    balance = 10000
    # Missing rate, term, start_date
  )

  expect_error(
    calculate_cash_flows(loan_data, config = list()),
    "Missing required columns"
  )
})

# Test 8: Invalid rate values (> 1)
test_that("calculate_cash_flows warns on rates > 1", {
  loan_data <- create_test_portfolio(1)
  loan_data$current_interest_rate <- 5.99  # Should be 0.0599

  expect_warning(
    calculate_cash_flows(loan_data, config = list()),
    "greater than 1"
  )
})

# Test 9: Custom column mapping
test_that("calculate_cash_flows accepts custom column names", {
  loan_data <- data.frame(
    loan_number = "L001",
    principal = 10000,
    rate = 0.06,
    term = 12,
    snapshot_date = as.Date("2025-01-01")
  )

  config <- list(
    col_loanid = "loan_number",
    col_balance = "principal",
    col_rate = "rate",
    col_term = "term",
    col_start_date = "snapshot_date"
  )

  result <- calculate_cash_flows(loan_data, config)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

# Test 10: Investor share allocation
test_that("calculate_cash_flows applies investor_share correctly", {
  loan_data <- create_test_portfolio(1)

  config_full <- list(
    investor_share = 1.0,
    cpr_vec = c("default" = 0.0),
    credit_cost_vec = c("default" = 0.0),
    servicing_fee = 0.0
  )

  config_partial <- list(
    investor_share = 0.90,
    cpr_vec = c("default" = 0.0),
    credit_cost_vec = c("default" = 0.0),
    servicing_fee = 0.0
  )

  result_full <- calculate_cash_flows(loan_data, config_full)
  result_partial <- calculate_cash_flows(loan_data, config_partial)

  # Investor total should be 90% of full ownership
  expect_lt(
    sum(result_partial$investor_total),
    sum(result_full$investor_total)
  )

  # Check approximately 90%
  ratio <- sum(result_partial$investor_total) / sum(result_full$investor_total)
  expect_true(abs(ratio - 0.90) < 0.01)  # Within 1%
})

# Test 11: Monthly totals aggregation
test_that("monthly_totals correctly aggregates loan-level cash flows", {
  loan_data <- create_test_portfolio(3)

  config <- list(return_monthly_totals = TRUE)
  result <- calculate_cash_flows(loan_data, config)

  # Sum loan-level cash flows by date
  manual_totals <- result$loan_cash_flows %>%
    group_by(date) %>%
    summarise(
      total_principal = sum(total_principal),
      gross_interest = sum(gross_interest),
      .groups = "drop"
    )

  # Should match monthly_totals
  expect_equal(
    manual_totals$total_principal,
    result$monthly_totals$total_principal
  )
  expect_equal(
    manual_totals$gross_interest,
    result$monthly_totals$gross_interest
  )
})

# Test 12: Dates are sequential and start from eff_date
test_that("cash flow dates are sequential and start from eff_date", {
  loan_data <- create_test_portfolio(1)

  result <- calculate_cash_flows(loan_data, config = list())

  dates <- result$date
  expect_true(all(diff(dates) == 30 | diff(dates) == 31 | diff(dates) == 28))  # Monthly
  expect_equal(min(dates), as.Date("2025-01-01"))
})

# Test 13: Validation catches negative balances
test_that("calculate_cash_flows warns on negative or zero balances", {
  loan_data <- create_test_portfolio(2)
  loan_data$balance[1] <- -1000
  loan_data$balance[2] <- 0

  expect_warning(
    calculate_cash_flows(loan_data, config = list()),
    "balance <= 0"
  )
})

# Test 14: NA values in required columns
test_that("calculate_cash_flows warns on NA values", {
  loan_data <- create_test_portfolio(2)
  loan_data$current_interest_rate[1] <- NA

  expect_warning(
    calculate_cash_flows(loan_data, config = list()),
    "NA values found"
  )
})

# Test 15: Progress messages for large portfolios
test_that("calculate_cash_flows shows progress for large portfolios", {
  loan_data <- create_test_portfolio(1500)  # > 1000 loans

  config <- list(show_progress = TRUE)

  expect_message(
    calculate_cash_flows(loan_data, config),
    "Processing.*loans"
  )
})
