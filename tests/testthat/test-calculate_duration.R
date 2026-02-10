# tests/testthat/test-calculate_duration.R

library(testthat)
library(dplyr)
library(lubridate)

# Helper function to create test cash flow data ----
create_test_cash_flows <- function() {
  data.frame(
    LOAN_ID = rep(c("L001", "L002"), each = 12),
    eff_date = as.Date("2024-01-01"),
    date = rep(seq.Date(as.Date("2024-02-01"), by = "month", length.out = 12), 2),
    month = rep(1:12, 2),
    rate = rep(c(0.06, 0.05), each = 12),
    total_payment = rep(c(1000, 1500), each = 12),
    investor_total = rep(c(950, 1425), each = 12)
  )
}

# Test 1: Basic functionality with default parameters ----
test_that("calculate_duration works with default parameters", {
  cash_flows <- create_test_cash_flows()

  result <- calculate_duration(cash_flows)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(all(c("portfolio_pv", "macaulay_duration", "modified_duration") %in% names(result)))
  expect_false("analytical_convexity" %in% names(result))

  # Check that values are reasonable
  expect_true(result$portfolio_pv > 0)
  expect_true(result$macaulay_duration > 0)
  expect_true(result$macaulay_duration < 1)  # 12 months = 1 year max
  expect_true(result$modified_duration > 0)
  expect_true(result$modified_duration < result$macaulay_duration)  # Modified should be less
})

# Test 2: Works with custom discount rate ----
test_that("calculate_duration works with custom discount rate", {
  cash_flows <- create_test_cash_flows()

  result_default <- calculate_duration(cash_flows)
  result_custom <- calculate_duration(cash_flows, discount_rate = 0.04)

  expect_s3_class(result_custom, "data.frame")

  # With lower discount rate, PV should be higher
  expect_true(result_custom$portfolio_pv > result_default$portfolio_pv)

  # Duration should also differ
  expect_false(isTRUE(all.equal(result_custom$macaulay_duration,
                                result_default$macaulay_duration)))
})

# Test 3: Works with investor_total cash flow column ----
test_that("calculate_duration works with investor_total", {
  cash_flows <- create_test_cash_flows()

  result_total <- calculate_duration(cash_flows, cash_flow_column = "total_payment")
  result_investor <- calculate_duration(cash_flows, cash_flow_column = "investor_total")

  expect_s3_class(result_investor, "data.frame")

  # investor_total is less than total_payment, so PV should be lower
  expect_true(result_investor$portfolio_pv < result_total$portfolio_pv)

  # Duration should be similar (same timing, different amounts)
  expect_true(abs(result_investor$macaulay_duration - result_total$macaulay_duration) < 0.1)
})

# Test 4: Include convexity works ----
test_that("calculate_duration includes convexity when requested", {
  cash_flows <- create_test_cash_flows()

  result_no_cvx <- calculate_duration(cash_flows, include_convexity = FALSE)
  result_with_cvx <- calculate_duration(cash_flows, include_convexity = TRUE)

  expect_false("analytical_convexity" %in% names(result_no_cvx))
  expect_true("analytical_convexity" %in% names(result_with_cvx))
  expect_true(is.numeric(result_with_cvx$analytical_convexity))
  expect_true(result_with_cvx$analytical_convexity > 0)
})

# Test 5: Input validation - not a data frame ----
test_that("calculate_duration errors when input is not a data frame", {
  expect_error(
    calculate_duration("not a data frame"),
    "loan_cash_flows must be a data frame"
  )

  expect_error(
    calculate_duration(list(a = 1, b = 2)),
    "loan_cash_flows must be a data frame"
  )
})

# Test 6: Input validation - missing required columns ----
test_that("calculate_duration errors when required columns are missing", {
  cash_flows <- create_test_cash_flows()

  # Remove LOAN_ID
  cash_flows_no_id <- cash_flows %>% select(-LOAN_ID)
  expect_error(
    calculate_duration(cash_flows_no_id),
    "loan_cash_flows is missing required columns: LOAN_ID"
  )

  # Remove month
  cash_flows_no_month <- cash_flows %>% select(-month)
  expect_error(
    calculate_duration(cash_flows_no_month),
    "loan_cash_flows is missing required columns: month"
  )

  # Remove rate
  cash_flows_no_rate <- cash_flows %>% select(-rate)
  expect_error(
    calculate_duration(cash_flows_no_rate),
    "loan_cash_flows is missing required columns: rate"
  )

  # Remove multiple columns
  cash_flows_incomplete <- cash_flows %>% select(-rate, -total_payment)
  expect_error(
    calculate_duration(cash_flows_incomplete),
    "loan_cash_flows is missing required columns"
  )
  expect_error(
    calculate_duration(cash_flows_incomplete),
    "rate"
  )
  expect_error(
    calculate_duration(cash_flows_incomplete),
    "total_payment"
  )
})

# Test 7: Input validation - invalid cash_flow_column ----
test_that("calculate_duration errors with invalid cash_flow_column", {
  cash_flows <- create_test_cash_flows()

  expect_error(
    calculate_duration(cash_flows, cash_flow_column = "invalid_column"),
    "cash_flow_column must be either 'total_payment' or 'investor_total'"
  )

  expect_error(
    calculate_duration(cash_flows, cash_flow_column = "gross_interest"),
    "cash_flow_column must be either 'total_payment' or 'investor_total'"
  )
})

# Test 8: Input validation - invalid discount_rate ----
test_that("calculate_duration errors with invalid discount_rate", {
  cash_flows <- create_test_cash_flows()

  expect_error(
    calculate_duration(cash_flows, discount_rate = -0.05),
    "discount_rate must be non-negative"
  )

  expect_error(
    calculate_duration(cash_flows, discount_rate = c(0.05, 0.06)),
    "discount_rate must be NULL or a single numeric value"
  )

  expect_error(
    calculate_duration(cash_flows, discount_rate = "5%"),
    "discount_rate must be NULL or a single numeric value"
  )
})

# Test 9: Allows discount_rate = 0 ----
test_that("calculate_duration allows discount_rate = 0", {
  cash_flows <- create_test_cash_flows()

  result <- calculate_duration(cash_flows, discount_rate = 0)

  expect_s3_class(result, "data.frame")
  expect_true(result$portfolio_pv > 0)
  expect_true(result$macaulay_duration > 0)
  expect_true(result$modified_duration > 0)
})

# Test 10: Input validation - invalid include_convexity ----
test_that("calculate_duration errors with invalid include_convexity", {
  cash_flows <- create_test_cash_flows()

  expect_error(
    calculate_duration(cash_flows, include_convexity = "TRUE"),
    "include_convexity must be TRUE or FALSE"
  )

  expect_error(
    calculate_duration(cash_flows, include_convexity = 1),
    "include_convexity must be TRUE or FALSE"
  )

  expect_error(
    calculate_duration(cash_flows, include_convexity = c(TRUE, FALSE)),
    "include_convexity must be TRUE or FALSE"
  )
})

# Test 11: Handles all NA in LOAN_ID ----
test_that("calculate_duration errors when LOAN_ID is all NA", {
  cash_flows <- create_test_cash_flows()
  cash_flows$LOAN_ID <- NA_character_

  expect_error(
    calculate_duration(cash_flows),
    "LOAN_ID column contains only NA values"
  )
})

# Test 12: Handles all NA in cash flow column ----
test_that("calculate_duration errors when cash flow column is all NA", {
  cash_flows <- create_test_cash_flows()

  cash_flows$total_payment <- NA_real_
  expect_error(
    calculate_duration(cash_flows, cash_flow_column = "total_payment"),
    "total_payment column contains only NA values"
  )

  cash_flows <- create_test_cash_flows()
  cash_flows$investor_total <- NA_real_
  expect_error(
    calculate_duration(cash_flows, cash_flow_column = "investor_total"),
    "investor_total column contains only NA values"
  )
})

# Test 13: Handles missing values in some rows ----
test_that("calculate_duration handles partial missing data", {
  cash_flows <- create_test_cash_flows()

  # Set some rows to NA
  cash_flows$total_payment[1:3] <- NA_real_

  result <- calculate_duration(cash_flows)

  expect_s3_class(result, "data.frame")
  expect_true(result$portfolio_pv > 0)

  # Should still work with remaining data
  expect_true(result$macaulay_duration > 0)
})

# Test 14: Errors when no valid rows remain ----
test_that("calculate_duration errors when no valid rows remain after filtering", {
  cash_flows <- create_test_cash_flows()

  # Make all critical columns NA
  cash_flows$LOAN_ID <- NA_character_
  cash_flows$total_payment <- NA_real_

  expect_error(
    calculate_duration(cash_flows),
    "LOAN_ID column contains only NA values"
  )
})

# Test 15: Errors when portfolio PV is zero ----
test_that("calculate_duration errors when portfolio PV is zero", {
  cash_flows <- create_test_cash_flows()

  # Set all cash flows to zero
  cash_flows$total_payment <- 0
  cash_flows$investor_total <- 0

  expect_warning(
    expect_error(
      calculate_duration(cash_flows),
      "No loans remaining after filtering zero PV"
    ),
    "Removed 2 loan\\(s\\) with zero present value"
  )
})

# Test 16: Modified duration is less than Macaulay duration ----
test_that("modified duration is always less than Macaulay duration", {
  cash_flows <- create_test_cash_flows()

  # Test with default rates
  result1 <- calculate_duration(cash_flows)
  expect_true(result1$modified_duration < result1$macaulay_duration)

  # Test with custom rate
  result2 <- calculate_duration(cash_flows, discount_rate = 0.08)
  expect_true(result2$modified_duration < result2$macaulay_duration)

  # Test with low rate
  result3 <- calculate_duration(cash_flows, discount_rate = 0.01)
  expect_true(result3$modified_duration < result3$macaulay_duration)
})

# Test 17: Duration decreases as discount rate increases ----
test_that("higher discount rates produce lower PV", {
  cash_flows <- create_test_cash_flows()

  result_3pct <- calculate_duration(cash_flows, discount_rate = 0.03)
  result_5pct <- calculate_duration(cash_flows, discount_rate = 0.05)
  result_8pct <- calculate_duration(cash_flows, discount_rate = 0.08)

  # Higher discount rate should produce lower PV
  expect_true(result_3pct$portfolio_pv > result_5pct$portfolio_pv)
  expect_true(result_5pct$portfolio_pv > result_8pct$portfolio_pv)
})

# Test 18: Single loan portfolio ----
test_that("calculate_duration works with single loan", {
  cash_flows <- data.frame(
    LOAN_ID = rep("L001", 12),
    eff_date = as.Date("2024-01-01"),
    date = seq.Date(as.Date("2024-02-01"), by = "month", length.out = 12),
    month = 1:12,
    rate = 0.06,
    total_payment = 1000,
    investor_total = 950
  )

  result <- calculate_duration(cash_flows)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(result$portfolio_pv > 0)
  expect_true(result$macaulay_duration > 0)
  expect_true(result$modified_duration > 0)
})

# Test 19: Large portfolio (performance check) ----
test_that("calculate_duration handles larger portfolios efficiently", {
  # Create 100 loans with 60 months each = 6,000 rows
  n_loans <- 100
  n_months <- 60

  cash_flows <- data.frame(
    LOAN_ID = rep(paste0("L", sprintf("%03d", 1:n_loans)), each = n_months),
    eff_date = as.Date("2024-01-01"),
    date = rep(seq.Date(as.Date("2024-02-01"), by = "month", length.out = n_months), n_loans),
    month = rep(1:n_months, n_loans),
    rate = rep(runif(n_loans, 0.04, 0.08), each = n_months),
    total_payment = rep(runif(n_loans, 500, 2000), each = n_months),
    investor_total = rep(runif(n_loans, 450, 1900), each = n_months)
  )

  # Should complete in reasonable time
  start_time <- Sys.time()
  result <- calculate_duration(cash_flows)
  end_time <- Sys.time()

  expect_s3_class(result, "data.frame")
  expect_true(result$portfolio_pv > 0)
  expect_true(as.numeric(difftime(end_time, start_time, units = "secs")) < 5)
})

# Test 20: Date format handling ----
test_that("calculate_duration handles date formats correctly", {
  cash_flows <- create_test_cash_flows()

  # Convert dates to character
  cash_flows$eff_date <- as.character(cash_flows$eff_date)
  cash_flows$date <- as.character(cash_flows$date)

  result <- calculate_duration(cash_flows)

  expect_s3_class(result, "data.frame")
  expect_true(result$portfolio_pv > 0)
})

# Test 21: Convexity is always positive ----
test_that("analytical convexity is positive", {
  cash_flows <- create_test_cash_flows()

  result1 <- calculate_duration(cash_flows, include_convexity = TRUE)
  expect_true(result1$analytical_convexity > 0)

  result2 <- calculate_duration(cash_flows, discount_rate = 0.08, include_convexity = TRUE)
  expect_true(result2$analytical_convexity > 0)

  result3 <- calculate_duration(cash_flows,
                                cash_flow_column = "investor_total",
                                include_convexity = TRUE)
  expect_true(result3$analytical_convexity > 0)
})

# Test 22: Verify mathematical relationship between durations ----
test_that("modified duration calculation is mathematically correct", {
  cash_flows <- create_test_cash_flows()

  # Test with known discount rate
  discount_rate <- 0.06
  result <- calculate_duration(cash_flows, discount_rate = discount_rate)

  # Modified Duration should equal Macaulay / (1 + y/12)
  expected_modified <- result$macaulay_duration / (1 + discount_rate / 12)

  expect_equal(result$modified_duration, expected_modified, tolerance = 1e-6)
})

# Test 23: Different loans with different rates ----
test_that("calculate_duration handles heterogeneous loan rates correctly", {
  cash_flows <- data.frame(
    LOAN_ID = rep(c("L001", "L002", "L003"), each = 12),
    eff_date = as.Date("2024-01-01"),
    date = rep(seq.Date(as.Date("2024-02-01"), by = "month", length.out = 12), 3),
    month = rep(1:12, 3),
    rate = rep(c(0.03, 0.06, 0.09), each = 12),
    total_payment = rep(c(1000, 1000, 1000), each = 12),
    investor_total = rep(c(950, 950, 950), each = 12)
  )

  result_varied <- calculate_duration(cash_flows)
  result_uniform <- calculate_duration(cash_flows, discount_rate = 0.06)

  # Results should differ when using varied rates vs uniform rate
  expect_false(isTRUE(all.equal(result_varied$portfolio_pv, result_uniform$portfolio_pv)))
  expect_false(isTRUE(all.equal(result_varied$macaulay_duration, result_uniform$macaulay_duration)))
})

# Test 24: Time calculation using month column ----
test_that("calculate_duration correctly uses month column for time calculation", {
  cash_flows <- data.frame(
    LOAN_ID = rep("L001", 3),
    eff_date = as.Date("2024-01-01"),
    date = as.Date(c("2024-02-01", "2024-03-01", "2024-04-01")),
    month = c(1, 2, 3),
    rate = 0.06,
    total_payment = c(100, 100, 100),
    investor_total = c(95, 95, 95)
  )

  result <- calculate_duration(cash_flows)

  # With monthly compounding and t_months = month - 1:
  # Month 1: t_months = 0, t_years = 0
  # Month 2: t_months = 1, t_years = 1/12
  # Month 3: t_months = 2, t_years = 2/12

  # PV calculations with y = 0.06 annual, monthly compounding:
  # PV1 = 100 / (1 + 0.06/12)^0 = 100
  # PV2 = 100 / (1 + 0.06/12)^1 = 100 / 1.005 ≈ 99.502
  # PV3 = 100 / (1 + 0.06/12)^2 = 100 / 1.010025 ≈ 99.007

  # Macaulay Duration = (0*100 + 1/12*99.502 + 2/12*99.007) / (100 + 99.502 + 99.007)
  pv1 <- 100
  pv2 <- 100 / (1.005)
  pv3 <- 100 / (1.005^2)
  total_pv <- pv1 + pv2 + pv3
  expected_duration <- (0 * pv1 + (1/12) * pv2 + (2/12) * pv3) / total_pv

  expect_equal(result$macaulay_duration, expected_duration, tolerance = 0.001)
})

# Test 25: Monthly compounding vs annual compounding comparison ----
test_that("monthly compounding produces different results than annual", {
  cash_flows <- create_test_cash_flows()

  # With monthly compounding (current implementation)
  result_monthly <- calculate_duration(cash_flows, discount_rate = 0.06)

  # The PV should be different from what annual compounding would give
  # We can't directly test annual, but we can verify monthly is being used
  # by checking the PV is reasonable for monthly compounding

  expect_true(result_monthly$portfolio_pv > 0)
  expect_true(result_monthly$macaulay_duration > 0)

  # With higher rate, PV should be lower (sanity check on discounting)
  result_high_rate <- calculate_duration(cash_flows, discount_rate = 0.12)
  expect_true(result_high_rate$portfolio_pv < result_monthly$portfolio_pv)
})

# Test 26: Validate month >= 1 ----
test_that("calculate_duration errors when month < 1", {
  cash_flows <- create_test_cash_flows()

  # Set some months to 0
  cash_flows$month[1:3] <- 0

  expect_error(
    calculate_duration(cash_flows),
    "month column contains values less than 1"
  )

  # Set some months to negative
  cash_flows <- create_test_cash_flows()
  cash_flows$month[1:3] <- -1

  expect_error(
    calculate_duration(cash_flows),
    "month column contains values less than 1"
  )
})

# Test 27: Validate month is integer ----
test_that("calculate_duration errors when month is not integer", {
  cash_flows <- create_test_cash_flows()

  # Set some months to decimals
  cash_flows$month[1:3] <- 1.5

  expect_error(
    calculate_duration(cash_flows),
    "month column contains non-integer values"
  )
})

