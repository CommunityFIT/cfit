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

# Helper function to create test data with tiers
create_test_portfolio_with_tiers <- function(n_loans = 3) {
  data.frame(
    LOAN_ID = paste0("L", sprintf("%03d", 1:n_loans)),
    balance = rep(10000, n_loans),
    current_interest_rate = rep(0.06, n_loans),
    months_to_maturity = rep(12, n_loans),
    eff_date = as.Date("2025-01-01"),
    tier = c("A", "B", "A")[1:n_loans],
    stringsAsFactors = FALSE
  )
}

# Test 16: monthly_totals_group_vars - group by tier
test_that("monthly_totals_group_vars allows grouping by additional variables", {
  loan_data <- create_test_portfolio_with_tiers(3)

  config <- list(
    col_tier = "tier",
    cpr_vec = c("A" = 0.05, "B" = 0.10),
    credit_cost_vec = c("A" = 0.01, "B" = 0.02),
    return_monthly_totals = TRUE,
    monthly_totals_group_vars = c("tier")
  )

  result <- calculate_cash_flows(loan_data, config)

  # Should have columns: date, tier, and all the totals
  expect_true("tier" %in% names(result$monthly_totals))
  expect_true("date" %in% names(result$monthly_totals))

  # Should have rows for each date-tier combination
  n_unique_dates <- length(unique(result$loan_cash_flows$date))
  n_unique_tiers <- length(unique(loan_data$tier))
  expect_gte(nrow(result$monthly_totals), n_unique_tiers)
})

# Test 17: monthly_totals_group_vars with invalid column
test_that("monthly_totals_group_vars warns on invalid columns", {
  loan_data <- create_test_portfolio_with_tiers(2)

  config <- list(
    return_monthly_totals = TRUE,
    monthly_totals_group_vars = c("nonexistent_column")
  )

  expect_warning(
    calculate_cash_flows(loan_data, config),
    "not in cash flows data"
  )
})

# Test 18: credit_loss_reduces_interest = TRUE (default)
test_that("credit_loss_reduces_interest = TRUE reduces net interest", {
  loan_data <- create_test_portfolio(1)

  config <- list(
    cpr_vec = c("default" = 0.0),
    credit_cost_vec = c("default" = 0.02),  # 2% credit cost
    servicing_fee = 0.0,
    credit_loss_reduces_interest = TRUE  # Default behavior
  )

  result <- calculate_cash_flows(loan_data, config)

  # Net interest should be less than gross interest due to credit losses
  expect_lt(
    sum(result$net_interest),
    sum(result$gross_interest)
  )

  # Verify credit losses were subtracted from interest
  # net_interest_raw should = gross_interest - credit_loss (no fees in this test)
  first_row <- result[1, ]
  expect_equal(
    first_row$net_interest,
    max(0, first_row$gross_interest - first_row$credit_loss)
  )
})

# Test 19: credit_loss_reduces_interest = FALSE
test_that("credit_loss_reduces_interest = FALSE does not reduce net interest", {
  loan_data <- create_test_portfolio(1)

  config <- list(
    cpr_vec = c("default" = 0.0),
    credit_cost_vec = c("default" = 0.02),  # 2% credit cost
    servicing_fee = 0.0,
    origination_fee = 0.0,
    credit_loss_reduces_interest = FALSE  # Alternative accounting
  )

  result <- calculate_cash_flows(loan_data, config)

  # Net interest should equal gross interest (credit losses don't affect it)
  expect_equal(
    sum(result$net_interest),
    sum(result$gross_interest)
  )

  # But credit losses still reduce principal
  expect_gt(sum(result$credit_loss), 0)
})

# Test 20: annual_reporting_fee is properly annualized
test_that("annual_reporting_fee is divided by 12 for monthly calculation", {
  loan_data <- create_test_portfolio(1)

  # Set annual reporting fee to 12 bps (0.0012)
  # Monthly should be 1 bp (0.0001)
  config <- list(
    servicing_fee = 0.0,
    annual_reporting_fee = 0.0012,
    return_monthly_totals = TRUE
  )

  result <- calculate_cash_flows(loan_data, config)

  # Check first month reporting fee
  first_month_reporting <- result$loan_cash_flows$reporting_fee_amt[1]
  first_month_balance <- result$loan_cash_flows$accrual_balance[1]

  # Should be approximately balance * (0.0012/12)
  expected_reporting <- first_month_balance * (0.0012 / 12)
  expect_equal(first_month_reporting, expected_reporting, tolerance = 0.01)
})

# Test 21: Fee columns are split correctly
test_that("servicing_fee_amt, reporting_fee_amt, and total_fees are correct", {
  loan_data <- create_test_portfolio(1)

  config <- list(
    servicing_fee = 0.0024,  # 24 bps annual
    annual_reporting_fee = 0.0012,  # 12 bps annual
    cpr_vec = c("default" = 0.0),
    credit_cost_vec = c("default" = 0.0)
  )

  result <- calculate_cash_flows(loan_data, config)

  # Check all three fee columns exist
  expect_true("servicing_fee_amt" %in% names(result))
  expect_true("reporting_fee_amt" %in% names(result))
  expect_true("total_fees" %in% names(result))

  # Verify total_fees = servicing_fee_amt + reporting_fee_amt
  expect_equal(
    result$total_fees,
    result$servicing_fee_amt + result$reporting_fee_amt
  )

  # Check first month calculation
  first_balance <- result$accrual_balance[1]
  expect_equal(
    result$servicing_fee_amt[1],
    first_balance * (0.0024 / 12),
    tolerance = 0.01
  )
  expect_equal(
    result$reporting_fee_amt[1],
    first_balance * (0.0012 / 12),
    tolerance = 0.01
  )
})

# Test 22: Prepayment is capped at available balance after credit loss
test_that("prepayment cannot exceed available balance after credit loss", {
  loan_data <- create_test_portfolio(1)

  # High CPR and high credit cost - might cause prepayment to exceed available
  config <- list(
    cpr_vec = c("default" = 0.50),  # 50% CPR
    credit_cost_vec = c("default" = 0.40)  # 40% credit cost
  )

  result <- calculate_cash_flows(loan_data, config)

  # For each month, verify prepayment + credit_loss <= starting_balance
  for (i in 1:nrow(result)) {
    expect_lte(
      result$prepayment[i] + result$credit_loss[i],
      result$starting_balance[i] + 0.01  # Small tolerance for rounding
    )
  }
})

# Test 23: Enforce "default" tier when col_tier is NULL
test_that("validate_config enforces 'default' tier when col_tier is NULL", {
  loan_data <- create_test_portfolio(1)

  # Config without "default" tier
  config <- list(
    col_tier = NULL,
    cpr_vec = c("A" = 0.05, "B" = 0.10),  # Missing "default"
    credit_cost_vec = c("A" = 0.01, "B" = 0.02)
  )

  expect_error(
    calculate_cash_flows(loan_data, config),
    "must contain a 'default' tier"
  )
})

# Test 24: "default" tier is used when tier column missing
test_that("'default' tier is used when col_tier is NULL", {
  loan_data <- create_test_portfolio(2)

  config <- list(
    col_tier = NULL,
    cpr_vec = c("default" = 0.08),
    credit_cost_vec = c("default" = 0.015)
  )

  result <- calculate_cash_flows(loan_data, config)

  # Should have cash flows for both loans using default tier
  expect_equal(length(unique(result$LOAN_ID)), 2)

  # All loans should have same prepayment rate (from default tier)
  prepay_rates <- result %>%
    group_by(LOAN_ID, month) %>%
    summarise(prepay_rate = prepayment / starting_balance, .groups = "drop")

  # Rates should be similar across loans (allowing for rounding)
  expect_lt(sd(prepay_rates$prepay_rate), 0.001)
})

# Test 25: Date conversion is persisted
test_that("character dates are converted and persisted", {
  loan_data <- create_test_portfolio(1)
  loan_data$eff_date <- "2025-01-01"  # Character, not Date

  # Should convert silently and work without error
  result <- calculate_cash_flows(loan_data, config = list())

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

# Test 26: Investor total calculation with credit_loss_reduces_interest
test_that("investor_total reflects credit_loss_reduces_interest setting", {
  loan_data <- create_test_portfolio(1)

  base_config <- list(
    cpr_vec = c("default" = 0.0),
    credit_cost_vec = c("default" = 0.02),
    servicing_fee = 0.001,
    investor_share = 1.0,
    return_monthly_totals = TRUE
  )

  # Test with credit_loss_reduces_interest = TRUE
  config_true <- modifyList(base_config, list(credit_loss_reduces_interest = TRUE))
  result_true <- calculate_cash_flows(loan_data, config_true)

  # Test with credit_loss_reduces_interest = FALSE
  config_false <- modifyList(base_config, list(credit_loss_reduces_interest = FALSE))
  result_false <- calculate_cash_flows(loan_data, config_false)

  # investor_total should be higher when credit losses don't reduce interest
  expect_gt(
    sum(result_false$monthly_totals$investor_total),
    sum(result_true$monthly_totals$investor_total)
  )
})
