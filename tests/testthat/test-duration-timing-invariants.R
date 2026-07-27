# tests/testthat/test-duration-timing-invariants.R
#
# Timing-convention tests for calculate_duration() and calculate_wal(), added
# in v0.2.5.1.
#
# Context
# -------
# calculate_cash_flows() emits month = 1 for the first projected cash flow,
# which falls one month AFTER eff_date (i.e. month = 1 is t = 1 month, not
# t = 0). Prior to v0.2.5.1, calculate_duration() and calculate_wal() treated
# month = 1 as t = 0 and computed t = (month - 1) / 12. Every cash flow was
# therefore discounted one period too few:
#
#   - portfolio_pv        overstated by a factor of (1 + y/12)
#   - macaulay_duration   understated by exactly 1/12 year
#   - modified_duration   understated correspondingly
#   - portfolio_wal       understated by exactly 1/12 year
#   - analytical_convexity understated
#
# TEST DESIGN NOTE (important)
# ---------------------------
# The algebraic relationships between a run and the same table with
# month + 1 (PV scales by 1/(1 + y/12); Macaulay and WAL each rise by 1/12)
# hold under BOTH the pre-fix and post-fix code, because the pre-fix exponent
# (month - 1) is shift-equivariant. Those relationships are therefore NOT
# capable of detecting this bug and are recorded below only as structural
# regression tests.
#
# The bug detectors are the ABSOLUTE tests: small hand-built cash flow tables
# with expected values derived from the mathematical definition, independent
# of the implementation. Run this file against unfixed code first and confirm
# the tests in sections 1-3 and 5 FAIL. Do not proceed to the fix until they
# have been observed failing.

# ---------------------------------------------------------------------------
# Helper: minimal cash flow table in calculate_cash_flows() output shape
# ---------------------------------------------------------------------------
# NOTE: the required-column set for calculate_duration() has not been verified
# against the implementation. If either function errors on a missing column,
# add it here rather than weakening the assertions. All four cash flow columns
# are set to the same vector so one table serves both functions; these
# functions aggregate columns independently, so internal economic consistency
# (principal == payment) is irrelevant to what is being tested.

mk_cf <- function(months,
                  amounts,
                  rate     = 0.12,
                  loan_id  = "L001",
                  eff_date = as.Date("2026-07-13")) {
  stopifnot(length(months) == length(amounts))
  all_dates <- seq(eff_date, by = "month", length.out = max(months) + 1L)
  tibble::tibble(
    LOAN_ID            = loan_id,
    eff_date           = eff_date,
    rate               = rate,
    month              = as.numeric(months),
    date               = all_dates[months + 1L],
    total_payment      = amounts,
    investor_total     = amounts,
    total_principal    = amounts,
    investor_principal = amounts
  )
}

# Bind several single-loan tables into one portfolio table
mk_pool <- function(...) dplyr::bind_rows(...)


# ===========================================================================
# 1. ABSOLUTE: single cash flow at month 1  (BUG DETECTOR)
# ===========================================================================
# One flow of 100 at month = 1, discounted at y = 12% (monthly i = 1%).
# Correct:  PV = 100 / 1.01 = 99.00990099009901
#           Macaulay = 1 month = 1/12 year
#           Modified = (1/12) / 1.01
#           Convexity = 1*2 / 1.01^(1+2) / PV / 144 = 2 / 1.01^2 / 144
# Pre-fix:  t = 0, so PV = 100, Macaulay = 0, Modified = 0, Convexity = 0.
# Every assertion below is a hardcoded literal, hand-checkable on a
# calculator, and independent of any formula re-implementation.

test_that("a single flow at month 1 is discounted one full period", {
  tab <- mk_cf(months = 1, amounts = 100, rate = 0.12)

  res <- calculate_duration(
    tab,
    cash_flow_column  = "investor_total",
    discount_rate     = 0.12,
    include_convexity = TRUE
  )

  expect_equal(res$portfolio_pv,      99.00990099009901, tolerance = 1e-10)
  expect_equal(res$macaulay_duration, 1 / 12,            tolerance = 1e-12)
  expect_equal(res$modified_duration, (1 / 12) / 1.01,   tolerance = 1e-12)

  # If PV and Macaulay pass but this fails, the implemented convexity formula
  # differs from C = sum(t(t+1)CF/(1+i)^(t+2)) / PV / 144. Reconcile the
  # formula before changing the expectation.
  expect_equal(res$analytical_convexity, 2 / (1.01^2) / 144, tolerance = 1e-12)
})


# ===========================================================================
# 2. ABSOLUTE: multi-period, closed form  (BUG DETECTOR)
# ===========================================================================
# Expected values written from the mathematical definition, not by calling
# the function under test.

test_that("multi-period PV, duration and convexity match closed form", {
  m   <- 1:6
  amt <- c(100, 150, 120, 200, 90, 300)
  y   <- 0.06
  i   <- y / 12

  tab <- mk_cf(m, amt, rate = y)

  pv_t     <- amt / (1 + i)^m          # correct convention: exponent = month
  pv       <- sum(pv_t)
  mac_mths <- sum(m * pv_t) / pv
  cvx_ann  <- sum(m * (m + 1) * amt / (1 + i)^(m + 2)) / pv / 144

  res <- calculate_duration(
    tab,
    cash_flow_column  = "investor_total",
    discount_rate     = y,
    include_convexity = TRUE
  )

  expect_equal(res$portfolio_pv,          pv,                        tolerance = 1e-10)
  expect_equal(res$macaulay_duration,     mac_mths / 12,             tolerance = 1e-10)
  expect_equal(res$modified_duration,     (mac_mths / 12) / (1 + i), tolerance = 1e-10)
  expect_equal(res$analytical_convexity,  cvx_ann,                   tolerance = 1e-10)
})


# ===========================================================================
# 3. ABSOLUTE: WAL  (BUG DETECTOR)
# ===========================================================================
# Undiscounted, principal-weighted. Deliberately chosen so the correct answer
# is exactly 0.25 years: sum(m * p) / sum(p) = 30000 / 10000 = 3 months.
# Pre-fix returns 2/12 = 0.1666...

test_that("WAL is undiscounted principal-weighted time in years", {
  m <- 1:4
  p <- c(1000, 2000, 3000, 4000)

  res <- calculate_wal(mk_cf(m, p), principal_column = "investor_principal")

  expect_equal(res$portfolio_wal, 0.25, tolerance = 1e-12)
  expect_equal(res$portfolio_wal, sum(m * p) / sum(p) / 12, tolerance = 1e-12)
})

test_that("WAL respects principal_column selection", {
  m   <- 1:3
  tab <- mk_cf(m, c(100, 100, 100))
  tab$investor_principal <- c(0, 0, 300)   # all investor principal in month 3

  expect_equal(
    calculate_wal(tab, principal_column = "total_principal")$portfolio_wal,
    2 / 12, tolerance = 1e-12
  )
  expect_equal(
    calculate_wal(tab, principal_column = "investor_principal")$portfolio_wal,
    3 / 12, tolerance = 1e-12
  )
})


# ===========================================================================
# 4. STRUCTURAL: shift equivariance
# ===========================================================================
# These pass both before and after the fix (see TEST DESIGN NOTE). They are
# retained because they pin the algebraic form of the timing dependence: they
# would catch a future change that made the exponent non-uniform across
# periods, or that decoupled duration from WAL. They are NOT the guard for
# the v0.2.5.1 bug.
#
# NOTE: exact only under a SCALAR discount_rate. Under discount_rate = NULL
# with heterogeneous loan rates, each loan's PV scales by its own
# 1/(1 + r_i/12), the cross-loan PV weights move, and the aggregate
# relationship becomes approximate. Do not restate these with NULL.

# These tests deliberately set validate_month_index = FALSE, because the
# offset month index is the thing under test. This is the intended use of
# the opt-out: a deliberate, documented shift, not a legacy workaround.

test_that("shifting month by 1 scales PV by exactly 1/(1 + y/12)", {
  m   <- 1:8
  amt <- c(500, 480, 460, 440, 420, 400, 380, 2000)
  y   <- 0.05138206
  i   <- y / 12

  tab <- mk_cf(m, amt, rate = y)
  shifted <- dplyr::mutate(tab, month = month + 1)

  base <- calculate_duration(tab,     cash_flow_column = "investor_total",
                             discount_rate = y, include_convexity = TRUE)
  shft <- calculate_duration(shifted, cash_flow_column = "investor_total",
                             discount_rate = y, include_convexity = TRUE,
                             validate_month_index = FALSE)

  expect_equal(shft$portfolio_pv, base$portfolio_pv / (1 + i), tolerance = 1e-10)
  expect_equal(shft$macaulay_duration,
               base$macaulay_duration + 1 / 12, tolerance = 1e-10)

  # Convexity shift: dC = 2 * D_after_months / ((1 + i)^2 * 144)
  d_after_months <- shft$macaulay_duration * 12
  expect_equal(
    shft$analytical_convexity - base$analytical_convexity,
    2 * d_after_months / ((1 + i)^2 * 144),
    tolerance = 1e-9
  )
})

test_that("shifting month by 1 raises WAL by exactly 1/12", {
  m   <- 1:8
  amt <- c(500, 480, 460, 440, 420, 400, 380, 2000)
  tab <- mk_cf(m, amt)

  base <- calculate_wal(tab, principal_column = "investor_principal")
  shft <- calculate_wal(dplyr::mutate(tab, month = month + 1),
                        principal_column = "investor_principal",
                        validate_month_index = FALSE)

  expect_equal(shft$portfolio_wal, base$portfolio_wal + 1 / 12, tolerance = 1e-12)
})


# ===========================================================================
# 5. ABSOLUTE: discount_rate = NULL, loan-level rates  (BUG DETECTOR)
# ===========================================================================

test_that("NULL discount_rate discounts a single loan at its own rate", {
  m   <- 1:5
  amt <- rep(100, 5)
  r   <- 0.075
  i   <- r / 12

  tab  <- mk_cf(m, amt, rate = r)
  pv_t <- amt / (1 + i)^m

  res <- calculate_duration(
    tab,
    cash_flow_column  = "investor_total",
    discount_rate     = NULL,
    include_convexity = TRUE
  )

  expect_equal(res$portfolio_pv,      sum(pv_t),                     tolerance = 1e-10)
  expect_equal(res$macaulay_duration, sum(m * pv_t) / sum(pv_t) / 12, tolerance = 1e-10)
})

test_that("NULL discount_rate aggregates loans as PV-weighted", {
  m <- 1:4
  r1 <- 0.055
  r2 <- 0.085

  l1 <- mk_cf(m, c(200, 200, 200, 1000), rate = r1, loan_id = "L001")
  l2 <- mk_cf(m, c(300, 300, 300,  900), rate = r2, loan_id = "L002")

  pv1 <- l1$investor_total / (1 + r1 / 12)^m
  pv2 <- l2$investor_total / (1 + r2 / 12)^m

  mac1 <- sum(m * pv1) / sum(pv1) / 12
  mac2 <- sum(m * pv2) / sum(pv2) / 12

  res <- calculate_duration(
    mk_pool(l1, l2),
    cash_flow_column = "investor_total",
    discount_rate    = NULL
  )

  expect_equal(res$portfolio_pv, sum(pv1) + sum(pv2), tolerance = 1e-10)

  # Portfolio Macaulay is the PV-weighted mean of loan-level Macaulays
  expect_equal(
    res$macaulay_duration,
    (mac1 * sum(pv1) + mac2 * sum(pv2)) / (sum(pv1) + sum(pv2)),
    tolerance = 1e-10
  )
})

test_that("scalar discount_rate overrides loan-level rates", {
  m   <- 1:4
  amt <- c(200, 200, 200, 1000)
  y   <- 0.09

  a <- calculate_duration(mk_cf(m, amt, rate = 0.04),
                          cash_flow_column = "investor_total",
                          discount_rate = y)
  b <- calculate_duration(mk_cf(m, amt, rate = 0.14),
                          cash_flow_column = "investor_total",
                          discount_rate = y)

  expect_equal(a$portfolio_pv,      b$portfolio_pv,      tolerance = 1e-12)
  expect_equal(a$macaulay_duration, b$macaulay_duration, tolerance = 1e-12)
})


# ===========================================================================
# 6. Month-index guard
# ===========================================================================
# These will fail until the guard is implemented (build step 5). They are
# written now so the guard has a specification before it has an
# implementation.
#
# Rationale: analysis scripts written against pre-v0.2.5.1 code carry
# mutate(month = month + 1) as a workaround. Left in place after the fix, that
# over-discounts by one period and silently understates PV by roughly 40bps at
# typical yields -- a plausible-looking number, which is what makes it
# dangerous. Fail loud.

test_that("a month index not starting at 1 errors by default", {
  tab <- dplyr::mutate(mk_cf(1:4, c(100, 100, 100, 100)), month = month + 1)

  expect_error(
    calculate_duration(tab, cash_flow_column = "investor_total",
                       discount_rate = 0.06),
    regexp = "month"
  )
  expect_error(
    calculate_wal(tab, principal_column = "investor_principal"),
    regexp = "month"
  )
})

test_that("the month index guard can be disabled explicitly", {
  tab <- dplyr::mutate(mk_cf(1:4, c(100, 100, 100, 100)), month = month + 1)

  expect_no_error(
    calculate_duration(tab, cash_flow_column = "investor_total",
                       discount_rate = 0.06,
                       validate_month_index = FALSE)
  )
})

test_that("non-integer and non-contiguous month indices are rejected", {
  frac <- dplyr::mutate(mk_cf(1:4, rep(100, 4)), month = month + 0.5)
  expect_error(
    calculate_duration(frac, cash_flow_column = "investor_total",
                       discount_rate = 0.06),
    regexp = "month"
  )

  gappy <- mk_cf(c(1, 2, 4, 5), rep(100, 4))
  expect_error(
    calculate_duration(gappy, cash_flow_column = "investor_total",
                       discount_rate = 0.06),
    regexp = "month"
  )
})

test_that("a zero month index is rejected", {
  zero <- mk_cf(0:3, rep(100, 4))
  expect_error(
    calculate_duration(zero, cash_flow_column = "investor_total",
                       discount_rate = 0.06),
    regexp = "month"
  )
})
