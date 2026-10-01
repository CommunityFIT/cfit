# tests/testthat/test-golden-duration-wal.R
#
# Golden-master regression tests for calculate_duration() and calculate_wal(),
# frozen in v0.2.5.1 after the month-index timing correction.
#
# Inputs and expectations are both read from disk. Nothing here regenerates a
# fixture. Heterogeneous-rate sensitivity corrections are checked by finite
# differences; unchanged PV, Macaulay duration, scalar-rate metrics and WAL
# remain pinned to these historical expectations.
#
# The input table is frozen rather than produced by calculate_cash_flows() at
# test time, so that an engine change cannot move these expectations. Engine
# behaviour is pinned separately by test-golden-cash-flows.R.

golden_path <- function(...) test_path("golden", ...)

read_golden_input <- function() {
  p <- golden_path("duration_wal_input.rds")
  skip_if_not(file.exists(p), "missing golden input: duration_wal_input.rds")
  readRDS(p)
}

# Case definitions must stay in sync with
# golden/make_duration_wal_fixtures.R.
duration_golden_cases <- function() {
  list(
    scalar_total = list(
      discount_rate = 0.05, cash_flow_column = "total_payment",
      include_convexity = TRUE
    ),
    scalar_investor = list(
      discount_rate = 0.05, cash_flow_column = "investor_total",
      include_convexity = TRUE
    ),
    loanrate_total = list(
      discount_rate = NULL, cash_flow_column = "total_payment",
      include_convexity = TRUE
    ),
    loanrate_investor = list(
      discount_rate = NULL, cash_flow_column = "investor_total",
      include_convexity = TRUE
    ),
    scalar_no_convexity = list(
      discount_rate = 0.05, cash_flow_column = "total_payment",
      include_convexity = FALSE
    )
  )
}

wal_golden_cases <- function() {
  list(
    total    = list(principal_column = "total_principal"),
    investor = list(principal_column = "investor_principal")
  )
}


test_that("the frozen duration/WAL input table is intact", {
  gi <- read_golden_input()

  expect_true(is.data.frame(gi))
  expect_gt(nrow(gi), 0)
  expect_true(all(c("LOAN_ID", "rate", "eff_date", "date", "month",
                    "total_payment", "investor_total",
                    "total_principal", "investor_principal") %in% names(gi)))

  # Contiguous from 1: the fixtures were frozen under the corrected
  # convention, and the guard depends on this holding.
  m <- sort(unique(as.numeric(gi$month)))
  expect_equal(m, as.numeric(seq_len(max(m))))

  # Rate heterogeneity is what exercises the discount_rate = NULL path.
  expect_gt(length(unique(gi$rate)), 1)
})


test_that("duration preserves golden metrics except corrected loan-rate sensitivities", {
  gi    <- read_golden_input()
  cases <- duration_golden_cases()

  for (nm in names(cases)) {
    gf <- golden_path(paste0("duration_", nm, ".rds"))
    skip_if_not(file.exists(gf), paste("missing golden:", nm))

    golden  <- readRDS(gf)
    current <- do.call(calculate_duration, c(list(gi), cases[[nm]]))

    if (is.null(cases[[nm]]$discount_rate)) {
      stable <- c("portfolio_pv", "macaulay_duration")
      expect_equal(current[stable], golden[stable], tolerance = 1e-8)
      price <- function(shift) sum(gi[[cases[[nm]]$cash_flow_column]] /
                                    (1 + (gi$rate + shift) / 12)^gi$month)
      h <- 1e-4
      p0 <- price(0); up <- price(h); down <- price(-h)
      expect_lt(abs(current$modified_duration - (down - up) / (2 * h * p0)), 1e-6)
      expect_lt(abs(current$analytical_convexity - (up + down - 2 * p0) / (h^2 * p0)), 1e-4)
    } else {
      expect_equal(current, golden, tolerance = 1e-8,
                   info = paste("duration golden case:", nm))
    }
  }
})


test_that("calculate_wal reproduces the v0.2.5.1 golden masters", {
  gi    <- read_golden_input()
  cases <- wal_golden_cases()

  for (nm in names(cases)) {
    gf <- golden_path(paste0("wal_", nm, ".rds"))
    skip_if_not(file.exists(gf), paste("missing golden:", nm))

    golden  <- readRDS(gf)
    current <- do.call(calculate_wal, c(list(gi), cases[[nm]]))

    expect_equal(current, golden, tolerance = 1e-8,
                 info = paste("wal golden case:", nm))
  }
})


test_that("golden fixtures encode the corrected timing convention", {
  # Absolute anchor, not a shift comparison: a reverted convention would
  # move the frozen values, and only an independently computed expectation
  # can detect that. Shift-based checks are equivariant under the bug and
  # would pass either way.
  gi <- read_golden_input()
  gf <- golden_path("duration_scalar_total.rds")
  skip_if_not(file.exists(gf), "missing golden: duration_scalar_total")

  golden <- readRDS(gf)
  y <- 0.05
  i <- y / 12

  # Computed here from the definition, with t = month (month 1 = one month
  # after eff_date). PV is additive, so summing rows gives portfolio PV.
  pv_row  <- gi$total_payment / (1 + i)^gi$month
  pv_exp  <- sum(pv_row)
  mac_exp <- sum((gi$month / 12) * pv_row) / pv_exp

  expect_equal(golden$portfolio_pv,      pv_exp,  tolerance = 1e-8)
  expect_equal(golden$macaulay_duration, mac_exp, tolerance = 1e-8)

  # Under the pre-v0.2.5.1 convention PV would be higher by (1 + y/12) and
  # Macaulay lower by 1/12; assert the frozen values are not those.
  expect_false(isTRUE(all.equal(golden$portfolio_pv, pv_exp * (1 + i),
                                tolerance = 1e-8)))
  expect_false(isTRUE(all.equal(golden$macaulay_duration, mac_exp - 1 / 12,
                                tolerance = 1e-8)))
})
