# tests/testthat/test-calculate_wal.R

library(testthat)
library(dplyr)
library(lubridate)

# Helper function to create test cash flow data ----
create_test_cash_flows_wal <- function() {
  data.frame(
    LOAN_ID = rep(c("L001", "L002"), each = 12),
    eff_date = as.Date("2024-01-01"),
    date = rep(seq.Date(as.Date("2024-02-01"), by = "month", length.out = 12), 2),
    month = rep(1:12, 2),
    total_principal = rep(c(800, 1200), each = 12),
    investor_principal = rep(c(760, 1140), each = 12)
  )
}

# Test 1: Basic functionality with default parameters ----
test_that("calculate_wal works with default parameters", {
  cash_flows <- create_test_cash_flows_wal()

  result <- calculate_wal(cash_flows)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true("portfolio_wal" %in% names(result))

  # Check that value is reasonable
  expect_true(result$portfolio_wal > 0)
  expect_true(result$portfolio_wal < 1)  # 12 months = 1 year max
})

# Test 2: Works with investor_principal ----
test_that("calculate_wal works with investor_principal", {
  cash_flows <- create_test_cash_flows_wal()

  result_total <- calculate_wal(cash_flows, principal_column = "total_principal")
  result_investor <- calculate_wal(cash_flows, principal_column = "investor_principal")

  expect_s3_class(result_investor, "data.frame")

  # WAL should be same timing (different amounts but same schedule)
  expect_equal(result_total$portfolio_wal, result_investor$portfolio_wal, tolerance = 0.001)
})

# Test 3: Input validation - not a data frame ----
test_that("calculate_wal errors when input is not a data frame", {
  expect_error(
    calculate_wal("not a data frame"),
    "loan_cash_flows must be a data frame"
  )

  expect_error(
    calculate_wal(list(a = 1, b = 2)),
    "loan_cash_flows must be a data frame"
  )
})

# Test 4: Input validation - missing required columns ----
test_that("calculate_wal errors when required columns are missing", {
  cash_flows <- create_test_cash_flows_wal()

  # Remove LOAN_ID
  cash_flows_no_id <- cash_flows %>% select(-LOAN_ID)
  expect_error(
    calculate_wal(cash_flows_no_id),
    "loan_cash_flows is missing required columns: LOAN_ID"
  )

  # Remove month
  cash_flows_no_month <- cash_flows %>% select(-month)
  expect_error(
    calculate_wal(cash_flows_no_month),
    "loan_cash_flows is missing required columns: month"
  )

  # Remove multiple columns
  cash_flows_incomplete <- cash_flows %>% select(-eff_date, -total_principal)
  expect_error(
    calculate_wal(cash_flows_incomplete),
    "loan_cash_flows is missing required columns"
  )
  expect_error(
    calculate_wal(cash_flows_incomplete),
    "eff_date"
  )
  expect_error(
    calculate_wal(cash_flows_incomplete),
    "total_principal"
  )
})

# Test 5: Input validation - invalid principal_column ----
test_that("calculate_wal errors with invalid principal_column", {
  cash_flows <- create_test_cash_flows_wal()

  expect_error(
    calculate_wal(cash_flows, principal_column = "invalid_column"),
    "principal_column must be either 'total_principal' or 'investor_principal'"
  )

  expect_error(
    calculate_wal(cash_flows, principal_column = "gross_principal"),
    "principal_column must be either 'total_principal' or 'investor_principal'"
  )
})

# Test 6: Handles all NA in LOAN_ID ----
test_that("calculate_wal errors when LOAN_ID is all NA", {
  cash_flows <- create_test_cash_flows_wal()
  cash_flows$LOAN_ID <- NA_character_

  expect_error(
    calculate_wal(cash_flows),
    "LOAN_ID column contains only NA values"
  )
})

# Test 7: Handles all NA in principal column ----
test_that("calculate_wal errors when principal column is all NA", {
  cash_flows <- create_test_cash_flows_wal()

  cash_flows$total_principal <- NA_real_
  expect_error(
    calculate_wal(cash_flows, principal_column = "total_principal"),
    "total_principal column contains only NA values"
  )

  cash_flows <- create_test_cash_flows_wal()
  cash_flows$investor_principal <- NA_real_
  expect_error(
    calculate_wal(cash_flows, principal_column = "investor_principal"),
    "investor_principal column contains only NA values"
  )
})

# Test 8: Handles missing values in some rows ----
test_that("calculate_wal handles partial missing data", {
  cash_flows <- create_test_cash_flows_wal()

  # Set some rows to NA
  cash_flows$total_principal[1:3] <- NA_real_

  result <- calculate_wal(cash_flows)

  expect_s3_class(result, "data.frame")
  expect_true(result$portfolio_wal > 0)
})

# Test 9: Errors when no valid rows remain ----
test_that("calculate_wal errors when no valid rows remain after filtering", {
  cash_flows <- create_test_cash_flows_wal()

  # Make all critical columns NA
  cash_flows$LOAN_ID <- NA_character_
  cash_flows$total_principal <- NA_real_

  expect_error(
    calculate_wal(cash_flows),
    "LOAN_ID column contains only NA values"
  )
})

# Test 10: Warns and filters loans with zero principal ----
test_that("calculate_wal warns and filters loans with zero principal", {
  cash_flows <- create_test_cash_flows_wal()

  # Set one loan to zero principal
  cash_flows$total_principal[cash_flows$LOAN_ID == "L001"] <- 0

  expect_warning(
    result <- calculate_wal(cash_flows, principal_column = "total_principal"),
    "Removed 1 loan\\(s\\) with zero total_principal from WAL calculation"
  )

  # Should still calculate for remaining loan
  expect_s3_class(result, "data.frame")
  expect_true(result$portfolio_wal > 0)
})

# Test 11: Errors when all loans have zero principal ----
test_that("calculate_wal errors when all loans have zero principal", {
  cash_flows <- create_test_cash_flows_wal()

  # Set all principal to zero
  cash_flows$total_principal <- 0

  expect_warning(
    expect_error(
      calculate_wal(cash_flows, principal_column = "total_principal"),
      "No loans remaining after filtering zero principal"
    ),
    "Removed 2 loan\\(s\\)"
  )
})

# Test 12: Single loan portfolio ----
test_that("calculate_wal works with single loan", {
  cash_flows <- data.frame(
    LOAN_ID = rep("L001", 12),
    eff_date = as.Date("2024-01-01"),
    date = seq.Date(as.Date("2024-02-01"), by = "month", length.out = 12),
    month = 1:12,
    total_principal = 800,
    investor_principal = 760
  )

  result <- calculate_wal(cash_flows)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(result$portfolio_wal > 0)
})

# Test 13: WAL with equal principal payments (constant amortization) ----
test_that("calculate_wal is correct for equal principal payments", {
  # Create loan with equal principal payments each month
  cash_flows <- data.frame(
    LOAN_ID = rep("L001", 12),
    eff_date = as.Date("2024-01-01"),
    date = seq.Date(as.Date("2024-02-01"), by = "month", length.out = 12),
    month = 1:12,
    total_principal = 100,  # Equal payments
    investor_principal = 95
  )

  result <- calculate_wal(cash_flows)

  # For equal payments over 12 months, using t = (month - 1) / 12:
  # WAL = (0*100 + 1/12*100 + 2/12*100 + ... + 11/12*100) / (12*100)
  # WAL = sum(0:11) / 12 / 12 = 66 / 144 = 0.458333 years
  expected_wal <- sum(0:11) / 12 / 12

  expect_equal(result$portfolio_wal, expected_wal, tolerance = 0.001)
})

# Test 14: WAL with front-loaded principal (bullet at start) ----
test_that("calculate_wal with front-loaded principal is shorter", {
  # Create loan with most principal paid early
  cash_flows_front <- data.frame(
    LOAN_ID = rep("L001", 12),
    eff_date = as.Date("2024-01-01"),
    date = seq.Date(as.Date("2024-02-01"), by = "month", length.out = 12),
    month = 1:12,
    total_principal = c(rep(200, 3), rep(50, 9)),
    investor_principal = c(rep(190, 3), rep(47.5, 9))
  )

  # Create loan with equal principal
  cash_flows_equal <- data.frame(
    LOAN_ID = rep("L001", 12),
    eff_date = as.Date("2024-01-01"),
    date = seq.Date(as.Date("2024-02-01"), by = "month", length.out = 12),
    month = 1:12,
    total_principal = 100,
    investor_principal = 95
  )

  result_front <- calculate_wal(cash_flows_front)
  result_equal <- calculate_wal(cash_flows_equal)

  # Front-loaded should have shorter WAL
  expect_true(result_front$portfolio_wal < result_equal$portfolio_wal)
})

# Test 15: WAL with back-loaded principal (bullet at end) ----
test_that("calculate_wal with back-loaded principal is longer", {
  # Create loan with most principal paid late
  cash_flows_back <- data.frame(
    LOAN_ID = rep("L001", 12),
    eff_date = as.Date("2024-01-01"),
    date = seq.Date(as.Date("2024-02-01"), by = "month", length.out = 12),
    month = 1:12,
    total_principal = c(rep(50, 9), rep(200, 3)),
    investor_principal = c(rep(47.5, 9), rep(190, 3))
  )

  # Create loan with equal principal
  cash_flows_equal <- data.frame(
    LOAN_ID = rep("L001", 12),
    eff_date = as.Date("2024-01-01"),
    date = seq.Date(as.Date("2024-02-01"), by = "month", length.out = 12),
    month = 1:12,
    total_principal = 100,
    investor_principal = 95
  )

  result_back <- calculate_wal(cash_flows_back)
  result_equal <- calculate_wal(cash_flows_equal)

  # Back-loaded should have longer WAL
  expect_true(result_back$portfolio_wal > result_equal$portfolio_wal)
})

# Test 16: Large portfolio (performance check) ----
test_that("calculate_wal handles larger portfolios efficiently", {
  # Create 100 loans with 60 months each = 6,000 rows
  n_loans <- 100
  n_months <- 60

  cash_flows <- data.frame(
    LOAN_ID = rep(paste0("L", sprintf("%03d", 1:n_loans)), each = n_months),
    eff_date = as.Date("2024-01-01"),
    date = rep(seq.Date(as.Date("2024-02-01"), by = "month", length.out = n_months), n_loans),
    month = rep(1:n_months, n_loans),
    total_principal = rep(runif(n_loans, 200, 800), each = n_months),
    investor_principal = rep(runif(n_loans, 190, 760), each = n_months)
  )

  # Should complete in reasonable time
  start_time <- Sys.time()
  result <- calculate_wal(cash_flows)
  end_time <- Sys.time()

  expect_s3_class(result, "data.frame")
  expect_true(result$portfolio_wal > 0)
  expect_true(as.numeric(difftime(end_time, start_time, units = "secs")) < 5)
})

# Test 17: Date format handling ----
test_that("calculate_wal handles date formats correctly", {
  cash_flows <- create_test_cash_flows_wal()

  # Convert dates to character
  cash_flows$eff_date <- as.character(cash_flows$eff_date)
  cash_flows$date <- as.character(cash_flows$date)

  result <- calculate_wal(cash_flows)

  expect_s3_class(result, "data.frame")
  expect_true(result$portfolio_wal > 0)
})

# Test 18: WAL is always positive ----
test_that("portfolio WAL is always positive for valid data", {
  cash_flows <- create_test_cash_flows_wal()

  result1 <- calculate_wal(cash_flows)
  expect_true(result1$portfolio_wal > 0)

  result2 <- calculate_wal(cash_flows, principal_column = "investor_principal")
  expect_true(result2$portfolio_wal > 0)
})

# Test 19: Multiple loans with different schedules ----
test_that("calculate_wal handles heterogeneous loan schedules correctly", {
  # Loan 1: Front-loaded
  # Loan 2: Equal payments
  # Loan 3: Back-loaded
  cash_flows <- data.frame(
    LOAN_ID = rep(c("L001", "L002", "L003"), each = 12),
    eff_date = as.Date("2024-01-01"),
    date = rep(seq.Date(as.Date("2024-02-01"), by = "month", length.out = 12), 3),
    month = rep(1:12, 3),
    total_principal = c(
      c(rep(200, 3), rep(50, 9)),   # Front-loaded
      rep(100, 12),                  # Equal
      c(rep(50, 9), rep(200, 3))     # Back-loaded
    ),
    investor_principal = c(
      c(rep(190, 3), rep(47.5, 9)),
      rep(95, 12),
      c(rep(47.5, 9), rep(190, 3))
    )
  )

  result <- calculate_wal(cash_flows)

  # Portfolio WAL should be weighted average of the three
  expect_s3_class(result, "data.frame")
  expect_true(result$portfolio_wal > 0)
  expect_true(result$portfolio_wal < 1)
})

# Test 20: Zero principal in some periods ----
test_that("calculate_wal handles zero principal in some periods", {
  cash_flows <- data.frame(
    LOAN_ID = rep("L001", 12),
    eff_date = as.Date("2024-01-01"),
    date = seq.Date(as.Date("2024-02-01"), by = "month", length.out = 12),
    month = 1:12,
    total_principal = c(0, 0, 0, rep(100, 9)),  # No principal first 3 months
    investor_principal = c(0, 0, 0, rep(95, 9))
  )

  result <- calculate_wal(cash_flows)

  expect_s3_class(result, "data.frame")
  expect_true(result$portfolio_wal > 0)

  # With t = (month - 1) / 12, first payment is at month 4, which is t = 3/12 = 0.25
  expect_true(result$portfolio_wal > 0.25)
})

# Test 21: Portfolio with varying loan sizes ----
test_that("calculate_wal properly weights loans by principal amount", {
  # Small loan with short WAL
  # Large loan with long WAL
  cash_flows <- data.frame(
    LOAN_ID = rep(c("SMALL", "LARGE"), each = 12),
    eff_date = as.Date("2024-01-01"),
    date = rep(seq.Date(as.Date("2024-02-01"), by = "month", length.out = 12), 2),
    month = rep(1:12, 2),
    total_principal = c(
      rep(10, 12),    # Small loan: $10/month
      rep(1000, 12)   # Large loan: $1000/month (100x larger)
    ),
    investor_principal = c(
      rep(9.5, 12),
      rep(950, 12)
    )
  )

  result <- calculate_wal(cash_flows)

  # Since both have same schedule, WAL should be same as equal payment WAL
  expected_wal <- sum(0:11) / 12 / 12

  expect_equal(result$portfolio_wal, expected_wal, tolerance = 0.001)
})

# Test 22: Time calculation using month column ----
test_that("calculate_wal correctly uses month column for time calculation", {
  cash_flows <- data.frame(
    LOAN_ID = rep("L001", 3),
    eff_date = as.Date("2024-01-01"),
    date = as.Date(c("2024-02-01", "2024-03-01", "2024-04-01")),
    month = c(1, 2, 3),
    total_principal = c(100, 100, 100),
    investor_principal = c(95, 95, 95)
  )

  result <- calculate_wal(cash_flows)

  # Manual calculation:
  # t_years for months 1,2,3 = (0, 1/12, 2/12)
  # WAL = (0*100 + 1/12*100 + 2/12*100) / 300
  # WAL = (0 + 100/12 + 200/12) / 300 = (300/12) / 300 = 25 / 300 = 1/12 = 0.08333
  expected_wal <- (0 + 1/12 + 2/12) / 3

  expect_equal(result$portfolio_wal, expected_wal, tolerance = 0.001)
})
