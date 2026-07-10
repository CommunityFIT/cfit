# ==========================================================================
# Regression suite for calculate_cash_flows() — cfit v0.2.5
#
# Pins the cash flow engine conventions established in the v0.2.5 release:
#   - Survivor re-amortization (market/Bloomberg prepay convention)
#   - First payment one month after the as-of date (t = 1, not t = 0)
#   - Month-end date clamping (lubridate %m+% arithmetic)
#   - Defaults: interest_on_starting_balance = TRUE,
#               credit_loss_reduces_interest = FALSE
#   - Unnamed-config-element guard
#
# Benchmark loan: 50,000 balance, 6.82% gross (6.57% net of 25 bps
# servicing), 76-month level pay, flat 18% CPR, price 102.25.
# Independent benchmark values (external replication, July 2026):
#   76 rows | WAL 2.337 | BEY 5.567% (monthly IRR, semiannual conversion)
# Legacy (fixed-payment) convention on the same loan: BEY ~5.32%.
# ==========================================================================

# --- Local helpers (no external yield dependencies in tests) ---------------

solve_monthly_irr <- function(purchase, amounts) {
  f <- function(y) sum(amounts / (1 + y)^seq_along(amounts)) - purchase
  uniroot(f, interval = c(-0.05, 0.10), tol = 1e-10)$root
}

bey_from_monthly <- function(irr_mo) 2 * ((1 + irr_mo)^6 - 1)

wal_years <- function(principal, month_index) {
  sum(principal * month_index) / sum(principal) / 12
}

benchmark_loan <- function(as_of = as.Date("2026-07-01")) {
  data.frame(
    loan_number           = "bench",
    current_loan_balance  = 50000,
    current_interest_rate = 0.0682,
    months_to_maturity    = 76,
    as_of_date            = as_of,
    orig_amt              = 50000
  )
}

benchmark_config <- function(...) {
  base <- list(
    col_loanid                   = "loan_number",
    col_balance                  = "current_loan_balance",
    col_rate                     = "current_interest_rate",
    col_term                     = "months_to_maturity",
    col_start_date               = "as_of_date",
    col_orig_balance             = "orig_amt",
    servicing_fee                = 0.0025,
    annual_reporting_fee         = 0,
    investor_share               = 1.0,
    interest_on_starting_balance = TRUE,
    credit_loss_reduces_interest = FALSE,
    reamortize_survivors         = TRUE,
    cpr_vec                      = c("default" = 0.18),
    credit_cost_vec              = c("default" = 0)
  )
  utils::modifyList(base, list(...))
}

PRICE <- 50000 * 1.0225   # 102.25 on 100% share

# --- 1. Headline benchmark: survivor re-amortization ties to market --------

test_that("re-amortized engine ties to independent market benchmark", {
  out <- calculate_cash_flows(benchmark_loan(), benchmark_config())

  expect_equal(nrow(out), 76)
  expect_equal(tail(out$remaining_balance, 1), 0)

  wal <- wal_years(out$total_principal, out$month)
  expect_equal(wal, 2.337, tolerance = 0.005)

  bey <- bey_from_monthly(solve_monthly_irr(PRICE, out$investor_total))
  expect_lt(abs(bey - 0.055731), 0.0002)   # within 2 bps absolute of pinned value
})

# --- 2. Legacy flag reproduces the fixed-payment (curtailment) engine ------

test_that("reamortize_survivors = FALSE reproduces legacy behavior", {
  out <- calculate_cash_flows(
    benchmark_loan(), benchmark_config(reamortize_survivors = FALSE)
  )

  # Fixed payment truncates the loan well before contractual maturity
  expect_lt(nrow(out), 55)

  bey <- bey_from_monthly(solve_monthly_irr(PRICE, out$investor_total))
  expect_equal(bey, 0.0532, tolerance = 0.001)

  # And the two conventions must differ materially (the v0.2.4 defect)
  out_new <- calculate_cash_flows(benchmark_loan(), benchmark_config())
  bey_new <- bey_from_monthly(solve_monthly_irr(PRICE, out_new$investor_total))
  expect_gt(bey_new - bey, 0.0015)   # > 15 bps
})

# --- 3. Zero-CPR invariant: branches identical with nothing to prepay ------

test_that("new and legacy branches are identical at zero CPR", {
  cfg_new <- benchmark_config(cpr_vec = c("default" = 0))
  cfg_leg <- benchmark_config(cpr_vec = c("default" = 0),
                              reamortize_survivors = FALSE)

  a <- calculate_cash_flows(benchmark_loan(), cfg_new)
  b <- calculate_cash_flows(benchmark_loan(), cfg_leg)

  expect_equal(nrow(a), 76)
  expect_equal(a$investor_total,     b$investor_total,     tolerance = 1e-8)
  expect_equal(a$scheduled_payment,  b$scheduled_payment,  tolerance = 1e-8)
  expect_equal(a$remaining_balance,  b$remaining_balance,  tolerance = 1e-8)

  # Level pay: payment constant, exact payoff at term
  expect_lt(diff(range(a$scheduled_payment)), 0.01)
  expect_equal(tail(a$remaining_balance, 1), 0)
})

# --- 4. Principal conservation ---------------------------------------------

test_that("principal conserves with and without credit losses", {
  out <- calculate_cash_flows(benchmark_loan(), benchmark_config())
  expect_equal(sum(out$total_principal), 50000, tolerance = 0.01)

  out_cl <- calculate_cash_flows(
    benchmark_loan(),
    benchmark_config(credit_cost_vec = c("default" = 0.01))
  )
  expect_equal(sum(out_cl$total_principal) + sum(out_cl$credit_loss),
               50000, tolerance = 0.01)
})

# --- 5. Date ladder: t = 1 start, monotonic, complete -----------------------

test_that("payment dates start one month after as-of and are complete", {
  out <- calculate_cash_flows(benchmark_loan(), benchmark_config())

  expect_false(anyNA(out$date))
  expect_equal(out$date[1], as.Date("2026-08-01"))
  expect_true(min(out$date) > as.Date("2026-07-01"))
  expect_equal(length(unique(out$date)), 76)
  expect_true(all(diff(out$date) >= 28 & diff(out$date) <= 31))
})

test_that("month-end as-of dates clamp instead of overflowing", {
  out <- calculate_cash_flows(
    benchmark_loan(as_of = as.Date("2026-01-31")), benchmark_config()
  )

  expect_equal(out$date[1:4],
               as.Date(c("2026-02-28", "2026-03-31",
                         "2026-04-30", "2026-05-31")))
  expect_false(anyNA(out$date))
})

# --- 6. Survivor scaling of scheduled payments ------------------------------

test_that("scheduled payment declines with the survival factor", {
  out <- calculate_cash_flows(benchmark_loan(), benchmark_config())

  expect_true(all(diff(out$scheduled_payment) < 0))

  # Month-over-month decay equals (1 - SMM) with zero credit losses
  smm <- 1 - (1 - 0.18)^(1 / 12)
  ratios <- out$scheduled_payment[-1] / out$scheduled_payment[-76]
  expect_equal(ratios, rep(1 - smm, 75), tolerance = 1e-6)
})

test_that("tape-supplied payments scale by the survival factor", {
  loan <- benchmark_loan()
  loan$monthly_payment <- 812.01   # ~original annuity payment

  out <- calculate_cash_flows(
    loan, benchmark_config(col_monthly_payment = "monthly_payment")
  )

  expect_equal(out$scheduled_payment[1], 812.01, tolerance = 0.01)
  smm <- 1 - (1 - 0.18)^(1 / 12)
  expect_equal(out$scheduled_payment[2] / out$scheduled_payment[1],
               1 - smm, tolerance = 1e-6)
})

# --- 7. Configuration guards -------------------------------------------------

test_that("incompatible interest convention warns under re-amortization", {
  expect_warning(
    calculate_cash_flows(
      benchmark_loan(),
      benchmark_config(interest_on_starting_balance = FALSE)
    ),
    regexp = "interest_on_starting_balance"
  )
})

test_that("unnamed config elements raise an error, not silence", {
  cfg_bad <- c(benchmark_config(), list(FALSE))
  expect_error(
    calculate_cash_flows(benchmark_loan(), cfg_bad),
    regexp = "unnamed"
  )
})

# --- 8. Default flips are live -----------------------------------------------

test_that("v0.2.5 defaults apply when flags are not set", {
  cfg_min <- list(
    col_loanid     = "loan_number",
    col_balance    = "current_loan_balance",
    col_rate       = "current_interest_rate",
    col_term       = "months_to_maturity",
    col_start_date = "as_of_date",
    cpr_vec        = c("default" = 0.18),
    credit_cost_vec = c("default" = 0)
  )
  out <- calculate_cash_flows(benchmark_loan(), cfg_min)

  # interest_on_starting_balance = TRUE: accrual on starting balance
  expect_equal(out$accrual_balance, out$starting_balance)

  # reamortize_survivors = TRUE: full term, declining payment
  expect_equal(nrow(out), 76)
  expect_true(all(diff(out$scheduled_payment) < 0))

  # credit_loss_reduces_interest = FALSE: default matches explicit FALSE,
  # differs from explicit TRUE when losses are nonzero
  cfg_loss   <- utils::modifyList(cfg_min,
                                  list(credit_cost_vec = c("default" = 0.01)))
  cfg_false  <- utils::modifyList(cfg_loss,
                                  list(credit_loss_reduces_interest = FALSE))
  cfg_true   <- utils::modifyList(cfg_loss,
                                  list(credit_loss_reduces_interest = TRUE))

  out_def   <- calculate_cash_flows(benchmark_loan(), cfg_loss)
  out_false <- calculate_cash_flows(benchmark_loan(), cfg_false)
  out_true  <- calculate_cash_flows(benchmark_loan(), cfg_true)

  expect_equal(out_def$net_interest, out_false$net_interest)
  expect_gt(sum(out_def$net_interest), sum(out_true$net_interest))
})
