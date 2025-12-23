# ==============================================================================
# TESTS FOR calculate_prepay_speed()
# ==============================================================================

# Helper function to create basic test data
create_test_data <- function() {
  data.frame(
    EFFDATE = as.Date(c("2024-01-31", "2024-02-29", "2024-03-31")),
    ORIGDATE = as.Date(c("2023-01-15", "2023-01-15", "2023-01-15")),
    TYPECODE = c("AUTO", "AUTO", "AUTO"),
    BAL = c(10000, 9500, 9000),
    ORIGBAL = c(15000, 15000, 15000),
    PAYAMT = c(500, 500, 500),
    CURRINTRATE = c(0.0729, 0.0729, 0.0729)
  )
}

# ==============================================================================
# BASIC FUNCTIONALITY TESTS
# ==============================================================================

test_that("calculate_prepay_speed returns correct structure", {
  test_data <- create_test_data()

  result <- calculate_prepay_speed(
    df = test_data,
    group_vars = c("EFFDATE", "TYPECODE")
  )

  # Check expected columns exist
  expect_true("EFFDATE" %in% names(result))
  expect_true("TYPECODE" %in% names(result))
  expect_true("BEGIN_BAL" %in% names(result))
  expect_true("END_BAL" %in% names(result))
  expect_true("SCHED_PRIN_TOTAL" %in% names(result))
  expect_true("FUNDED_BAL" %in% names(result))
  expect_true("ACTUAL_PRIN" %in% names(result))
  expect_true("PREPAYMENT" %in% names(result))
  expect_true("SMM" %in% names(result))
  expect_true("CPR" %in% names(result))

  # Check result is non-empty
  expect_true(nrow(result) > 0)

  # Check no NaN or Inf values in critical columns
  expect_false(any(is.nan(result$SMM)))
  expect_false(any(is.nan(result$CPR)))
  expect_false(any(is.infinite(result$SMM)))
  expect_false(any(is.infinite(result$CPR)))
})

test_that("calculate_prepay_speed handles minimal valid data", {
  # Minimum viable dataset (2 months)
  minimal_data <- data.frame(
    EFFDATE = as.Date(c("2024-01-31", "2024-02-29")),
    ORIGDATE = as.Date(c("2023-01-15", "2023-01-15")),
    TYPECODE = c("AUTO", "AUTO"),
    BAL = c(10000, 9500),
    ORIGBAL = c(15000, 15000),
    PAYAMT = c(500, 500),
    CURRINTRATE = c(0.0729, 0.0729)
  )

  result <- calculate_prepay_speed(
    df = minimal_data,
    group_vars = c("EFFDATE", "TYPECODE")
  )

  expect_equal(nrow(result), 1)  # Only Feb result (Jan has no prior month)
})

# ==============================================================================
# EFFDATE VALIDATION TESTS
# ==============================================================================

test_that("EFFDATE must be included in group_vars", {
  test_data <- create_test_data()

  expect_error(
    calculate_prepay_speed(
      df = test_data,
      group_vars = c("TYPECODE")  # Missing EFFDATE!
    ),
    "effective date column"
  )
})

test_that("function handles unordered EFFDATE correctly", {
  # Create data with dates out of order
  unordered_data <- data.frame(
    EFFDATE = as.Date(c("2024-03-31", "2024-01-31", "2024-02-29")),
    ORIGDATE = as.Date(c("2023-01-15", "2023-01-15", "2023-01-15")),
    TYPECODE = c("AUTO", "AUTO", "AUTO"),
    BAL = c(9000, 10000, 9500),
    ORIGBAL = c(15000, 15000, 15000),
    PAYAMT = c(500, 500, 500),
    CURRINTRATE = c(0.0729, 0.0729, 0.0729)
  )

  result <- calculate_prepay_speed(
    df = unordered_data,
    group_vars = c("EFFDATE", "TYPECODE")
  )

  # Result should be in date order
  expect_equal(result$EFFDATE, sort(result$EFFDATE))
})

# ==============================================================================
# SMM CLAMPING TESTS
# ==============================================================================

test_that("SMM is clamped to prevent NaN in CPR", {
  # Create extreme prepayment scenario (SMM > 1)
  extreme_data <- data.frame(
    EFFDATE = as.Date(c("2024-01-31", "2024-02-29")),
    ORIGDATE = as.Date(c("2023-01-15", "2023-01-15")),
    TYPECODE = c("AUTO", "AUTO"),
    BAL = c(10000, 500),  # 95% balance reduction
    ORIGBAL = c(15000, 15000),
    PAYAMT = c(500, 500),
    CURRINTRATE = c(0.05, 0.05)
  )

  result <- calculate_prepay_speed(
    df = extreme_data,
    group_vars = c("EFFDATE", "TYPECODE")
  )

  # CPR should be valid (not NaN, not Inf)
  expect_false(any(is.nan(result$CPR)))
  expect_false(any(is.infinite(result$CPR)))

  # CPR should be between 0 and 1
  expect_true(all(result$CPR >= 0 & result$CPR <= 1))
})

# ==============================================================================
# SCHEDULED PRINCIPAL FLOORING TESTS
# ==============================================================================

test_that("scheduled principal is floored at 0", {
  # Create interest-only scenario where interest > payment
  interest_only_data <- data.frame(
    EFFDATE = as.Date(c("2024-01-31", "2024-02-29")),
    ORIGDATE = as.Date(c("2023-01-15", "2023-01-15")),
    TYPECODE = c("LOAN", "LOAN"),
    BAL = c(100000, 99900),
    ORIGBAL = c(100000, 100000),
    PAYAMT = c(500, 500),  # Payment < monthly interest
    CURRINTRATE = c(0.12, 0.12)  # ~$1000/month interest
  )

  result <- calculate_prepay_speed(
    df = interest_only_data,
    group_vars = c("EFFDATE", "TYPECODE")
  )

  # Scheduled principal total should not be negative
  expect_true(all(result$SCHED_PRIN_TOTAL >= 0))
})

# ==============================================================================
# VERBOSE PARAMETER TESTS
# ==============================================================================

test_that("verbose = FALSE suppresses messages", {
  test_data <- create_test_data()

  expect_silent(
    calculate_prepay_speed(
      df = test_data,
      group_vars = c("EFFDATE", "TYPECODE"),
      verbose = FALSE
    )
  )
})

test_that("verbose = TRUE shows messages", {
  test_data <- create_test_data()

  expect_message(
    calculate_prepay_speed(
      df = test_data,
      group_vars = c("EFFDATE", "TYPECODE"),
      verbose = TRUE
    ),
    "Using"  # Should contain "Using" in the message
  )
})

# ==============================================================================
# CUSTOM COLUMN NAME TESTS
# ==============================================================================

test_that("custom column names are preserved in output", {
  # Create data with custom column names
  custom_data <- data.frame(
    ReportDate = as.Date(c("2024-01-31", "2024-02-29", "2024-03-31")),
    OpenDate = as.Date(c("2023-01-15", "2023-01-15", "2023-01-15")),
    Product = c("AUTO", "AUTO", "AUTO"),
    CurrentBalance = c(10000, 9500, 9000),
    StartingBalance = c(15000, 15000, 15000),
    MonthlyPayment = c(500, 500, 500),
    InterestRate = c(0.0729, 0.0729, 0.0729)
  )

  custom_config <- list(
    col_effdate = "ReportDate",
    col_origdate = "OpenDate",
    col_typecode = "Product",
    col_balance = "CurrentBalance",
    col_orig_balance = "StartingBalance",
    col_payment = "MonthlyPayment",
    col_rate = "InterestRate"
  )

  result <- calculate_prepay_speed(
    df = custom_data,
    group_vars = c("ReportDate", "Product"),
    prepay_config = custom_config
  )

  # Output should use original column names
  expect_true("ReportDate" %in% names(result))
  expect_true("Product" %in% names(result))
  expect_false("EFFDATE" %in% names(result))
  expect_false("TYPECODE" %in% names(result))
})

# ==============================================================================
# ALLOW NEGATIVE PREPAY TESTS
# ==============================================================================

test_that("allow_negative_prepay parameter works correctly", {
  # Create scenario with negative prepayment (balance increases)
  negative_prepay_data <- data.frame(
    EFFDATE = as.Date(c("2024-01-31", "2024-02-29")),
    ORIGDATE = as.Date(c("2023-01-15", "2023-01-15")),
    TYPECODE = c("AUTO", "AUTO"),
    BAL = c(10000, 10500),  # Balance increased
    ORIGBAL = c(15000, 15000),
    PAYAMT = c(500, 500),
    CURRINTRATE = c(0.05, 0.05)
  )

  # With allow_negative_prepay = FALSE (default)
  result_no_neg <- calculate_prepay_speed(
    df = negative_prepay_data,
    group_vars = c("EFFDATE", "TYPECODE"),
    prepay_config = list(allow_negative_prepay = FALSE)
  )

  # SMM should be floored at 0
  expect_true(all(result_no_neg$SMM >= 0))

  # With allow_negative_prepay = TRUE
  result_with_neg <- calculate_prepay_speed(
    df = negative_prepay_data,
    group_vars = c("EFFDATE", "TYPECODE"),
    prepay_config = list(allow_negative_prepay = TRUE)
  )

  # SMM can be negative
  expect_true(any(result_with_neg$SMM < 0))
})

# ==============================================================================
# MINIMUM BALANCE FILTER TESTS
# ==============================================================================

test_that("minimum balance filter removes small cohorts", {
  # Create data with mixed balance sizes
  mixed_balance_data <- data.frame(
    EFFDATE = as.Date(c("2024-01-31", "2024-02-29", "2024-01-31", "2024-02-29")),
    ORIGDATE = as.Date(c("2023-01-15", "2023-01-15", "2023-06-10", "2023-06-10")),
    TYPECODE = c("AUTO", "AUTO", "SMALL", "SMALL"),
    BAL = c(10000, 9500, 100, 50),
    ORIGBAL = c(15000, 15000, 200, 200),
    PAYAMT = c(500, 500, 10, 10),
    CURRINTRATE = c(0.05, 0.05, 0.05, 0.05)
  )

  # Without filter
  result_no_filter <- calculate_prepay_speed(
    df = mixed_balance_data,
    group_vars = c("EFFDATE", "TYPECODE"),
    prepay_config = list(min_begin_balance = 0)
  )

  # With $5000 filter
  result_with_filter <- calculate_prepay_speed(
    df = mixed_balance_data,
    group_vars = c("EFFDATE", "TYPECODE"),
    prepay_config = list(min_begin_balance = 5000)
  )

  # Filtered result should have fewer rows
  expect_true(nrow(result_with_filter) < nrow(result_no_filter))
})

# ==============================================================================
# INTEREST BASIS METHOD TESTS
# ==============================================================================

test_that("different interest basis methods produce different results", {
  test_data <- create_test_data()

  # 360-day basis
  result_360 <- calculate_prepay_speed(
    df = test_data,
    group_vars = c("EFFDATE", "TYPECODE"),
    prepay_config = list(interest_basis = 360)
  )

  # 365-day basis
  result_365 <- calculate_prepay_speed(
    df = test_data,
    group_vars = c("EFFDATE", "TYPECODE"),
    prepay_config = list(interest_basis = 365)
  )

  # Results should be different (360 vs 365 produces different interest)
  expect_false(identical(result_360$SMM, result_365$SMM))

  # But both should be valid
  expect_false(any(is.nan(result_360$CPR)))
  expect_false(any(is.nan(result_365$CPR)))
})

# ==============================================================================
# INPUT VALIDATION TESTS
# ==============================================================================

test_that("function errors on missing required columns", {
  incomplete_data <- data.frame(
    EFFDATE = as.Date(c("2024-01-31", "2024-02-29")),
    TYPECODE = c("AUTO", "AUTO")
    # Missing other required columns
  )

  expect_error(
    calculate_prepay_speed(
      df = incomplete_data,
      group_vars = c("EFFDATE", "TYPECODE")
    ),
    "Missing required columns"
  )
})

test_that("function errors on empty dataframe", {
  empty_data <- data.frame(
    EFFDATE = as.Date(character(0)),
    ORIGDATE = as.Date(character(0)),
    TYPECODE = character(0),
    BAL = numeric(0),
    ORIGBAL = numeric(0),
    PAYAMT = numeric(0),
    CURRINTRATE = numeric(0)
  )

  expect_error(
    calculate_prepay_speed(
      df = empty_data,
      group_vars = c("EFFDATE", "TYPECODE")
    ),
    "empty"
  )
})
