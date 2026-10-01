analytics_fixture <- function() {
  data.frame(LOAN_ID = rep(c("A", "B"), c(3, 2)), month = c(1:3, 1:2),
    eff_date = as.Date("2026-01-01"),
    date = as.Date(c("2026-02-01", "2026-03-01", "2026-04-01", "2026-02-01", "2026-03-01")),
    rate = c(rep(0.02, 3), rep(0.18, 2)),
    total_payment = c(10, 10, 110, 20, 220), investor_total = c(9, 9, 99, 18, 198),
    total_principal = c(0, 0, 100, 0, 200), investor_principal = c(0, 0, 90, 0, 180))
}

test_that("each loan has its own contiguous projection and duplicates always fail", {
  x <- analytics_fixture()
  for (fun in list(calculate_duration, calculate_wal)) {
    baseline <- fun(x)
    expect_equal(fun(x[c(5, 1, 4, 3, 2), ]), baseline)
    # A missing month is hidden by B's month 2 in the former portfolio guard.
    expect_error(fun(x[-2, ]), "Loan 'A'.*not contiguous")
    expect_error(fun(x[-1, ]), "Loan 'A'.*starts at 2")
    expect_no_error(fun(x[-2, ], validate_month_index = FALSE))
    expect_no_error(fun(x[-1, ], validate_month_index = FALSE))
    for (guard in c(TRUE, FALSE)) {
      expect_error(fun(rbind(x, x[1, ]), validate_month_index = guard), "Duplicate loan-month")
      bad <- x; bad$month[1] <- 0
      expect_error(fun(bad, validate_month_index = guard), "month")
      bad$month[1] <- 1.5
      expect_error(fun(bad, validate_month_index = guard), "non-integer")
    }
  }
})

test_that("analytics reject malformed data without dropping rows", {
  x <- analytics_fixture()
  for (fun in list(calculate_duration, calculate_wal)) {
    expect_error(fun(x[FALSE, ]), "non-empty")
    for (bad in list(NA, NULL, c(TRUE, FALSE), 1, "TRUE")) {
      expect_error(fun(x, validate_month_index = bad), "validate_month_index")
    }
    for (col in c("LOAN_ID", "date", "eff_date", "month")) {
      bad <- x; bad[[col]][1] <- NA
      expect_error(fun(bad), col)
    }
    for (value in list(Inf, NaN, "one")) {
      bad <- x; bad$month[1] <- value
      expect_error(fun(bad), "month.*finite numeric")
    }
    bad <- x; bad$LOAN_ID[1] <- " "
    expect_error(fun(bad), "LOAN_ID")
    bad <- x; names(bad)[2] <- "LOAN_ID"
    expect_error(fun(bad), "column names")
    bad <- x; bad$eff_date[4:5] <- as.Date("2026-02-01")
    expect_error(fun(bad), "common eff_date")
    expect_error(fun(bad, validate_month_index = FALSE), "common eff_date")
    bad <- x; bad$date <- as.character(bad$date); bad$date[1] <- "invalid"
    expect_error(fun(bad), "date")
  }
})

test_that("rates, selected amounts and scalar parameters must be valid", {
  x <- analytics_fixture()
  for (col in c("rate", "total_payment", "investor_total", "total_principal", "investor_principal")) {
    for (value in list(NA_real_, Inf, NaN, -1, "bad")) {
      bad <- x; bad[[col]][1] <- value
      if (col %in% c("rate", "total_payment", "investor_total")) {
        selected <- if (col == "rate") "total_payment" else col
        expect_error(calculate_duration(bad, cash_flow_column = selected), col)
      } else {
        expect_error(calculate_wal(bad, principal_column = col), col)
      }
    }
  }
  for (value in list(NA_real_, NaN, Inf, c(.01, .02), "bad")) {
    expect_error(calculate_duration(x, discount_rate = value), "discount_rate")
  }
  for (value in list(NA, NULL, 1, c(TRUE, FALSE))) {
    expect_error(calculate_duration(x, include_convexity = value), "include_convexity")
  }
  for (value in list(NA_character_, NULL, c("a", "b"), 1)) {
    expect_error(calculate_duration(x, cash_flow_column = value), "cash_flow_column")
    expect_error(calculate_wal(x, principal_column = value), "principal_column")
  }
})

test_that("heterogeneous sensitivities agree with finite differences for both cash flows", {
  x <- analytics_fixture()
  for (col in c("total_payment", "investor_total")) {
    out <- calculate_duration(x, cash_flow_column = col, include_convexity = TRUE)
    price <- function(shift) sum(x[[col]] / (1 + (x$rate + shift) / 12)^x$month)
    h <- 1e-3
    expect_equal(out$portfolio_pv, price(0))
    expect_lt(abs(out$modified_duration - (price(-h) - price(h)) / (2 * h * price(0))), 1e-7)
    expect_lt(abs(out$analytical_convexity - (price(h) + price(-h) - 2 * price(0)) /
                    (h^2 * price(0))), 1e-7)
    loans <- lapply(split(x, x$LOAN_ID), function(d)
      calculate_duration(d, cash_flow_column = col, include_convexity = TRUE))
    weights <- vapply(loans, function(d) d$portfolio_pv, numeric(1))
    for (metric in c("modified_duration", "analytical_convexity")) {
      expect_equal(out[[metric]], weighted.mean(vapply(loans, function(d) d[[metric]], numeric(1)), weights))
    }
  }
})

test_that("zero-coupon sensitivity has independent closed-form values", {
  x <- analytics_fixture()
  x$total_payment <- x$total_principal
  out <- calculate_duration(x, include_convexity = TRUE)
  # Only A at month 3 and B at month 2 pay principal.
  a <- 1 + .02/12; b <- 1 + .18/12
  pv_a <- 100/a^3; pv_b <- 200/b^2
  expect_equal(out$modified_duration, (pv_a * 3/(12*a) + pv_b * 2/(12*b))/(pv_a+pv_b))
  expect_equal(out$analytical_convexity,
               (pv_a * 3*4/(144*a^2) + pv_b * 2*3/(144*b^2))/(pv_a+pv_b))
  expect_equal(calculate_wal(x)$portfolio_wal, (100*3+200*2)/300/12)
  x$rate <- .05
  expect_equal(calculate_duration(x, include_convexity = TRUE),
               calculate_duration(x, discount_rate = .05, include_convexity = TRUE))
  expect_equal(calculate_duration(x, discount_rate = 0)$modified_duration,
               calculate_duration(x, discount_rate = 0)$macaulay_duration)
})

test_that("numeric overflow cannot silently become a portfolio metric", {
  x <- analytics_fixture()
  x$total_payment <- 1e308
  x$total_principal <- 1e308
  expect_error(calculate_duration(x, discount_rate = 0), "non-finite results")
  expect_error(calculate_wal(x), "non-finite results")
})
