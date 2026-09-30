validation_portfolio <- function() {
  data.frame(LOAN_ID = c("A", "B"), balance = c(10000, 20000),
             current_interest_rate = c(0.06, 0.07), months_to_maturity = c(12, 24),
             eff_date = as.Date("2025-01-31"))
}

test_that("input and config structure fail before projection", {
  x <- validation_portfolio()
  expect_error(calculate_cash_flows(list()), "non-empty data frame")
  expect_error(calculate_cash_flows(x[FALSE, ]), "non-empty data frame")
  expect_error(calculate_cash_flows(x, c(servicing_fee = 0)), "named list")
  expect_error(calculate_cash_flows(x, list(0)), "unnamed")
  expect_error(calculate_cash_flows(x, setNames(list(0), NA_character_)), "unnamed")
  expect_error(calculate_cash_flows(x, list(servicing_fee = 0, servicing_fee = 0.1)), "duplicate keys")
  expect_error(calculate_cash_flows(x, list(servicing_feee = 0)), "Unknown config keys")
  names(x)[2] <- names(x)[1]
  expect_error(calculate_cash_flows(x), "column names")
})

test_that("scalar configuration rejects missing and malformed settings", {
  x <- validation_portfolio()
  for (key in c("servicing_fee", "annual_reporting_fee", "investor_share",
                "origination_fee", "de_minimis_balance")) {
    for (bad in list(NULL, NA_real_, NaN, Inf, c(0, 1), "0", -0.1)) {
      expect_error(calculate_cash_flows(x, setNames(list(bad), key)), key)
    }
  }
  for (key in c("servicing_fee", "annual_reporting_fee", "investor_share", "origination_fee")) {
    expect_error(calculate_cash_flows(x, setNames(list(1.1), key)), key)
  }
  for (key in c("reamortize_survivors", "interest_on_starting_balance",
                "credit_loss_reduces_interest", "show_progress", "return_monthly_totals")) {
    for (bad in list(NULL, NA, 1, "TRUE", c(TRUE, FALSE))) {
      expect_error(calculate_cash_flows(x, setNames(list(bad), key)), key)
    }
  }
  for (bad in list(NULL, NA_character_, c("tier_static", "linear_incentive"), "other")) {
    expect_error(calculate_cash_flows(x, list(prepay_model = bad)), "prepay_model")
  }
})

test_that("column mappings are validated and automatic IDs are intentional", {
  x <- validation_portfolio()
  for (key in c("col_balance", "col_rate", "col_term", "col_start_date",
                "col_loanid", "col_tier", "col_monthly_payment", "col_orig_balance")) {
    for (bad in list("", NA_character_, c("one", "two"), 1)) {
      expect_error(calculate_cash_flows(x, setNames(list(bad), key)), key)
    }
    expect_error(calculate_cash_flows(x, setNames(list("absent"), key)), "[Mm]issing")
  }
  expect_error(calculate_cash_flows(x, list(col_balance = NULL)), "col_balance")
  x$LOAN_ID <- NULL
  expect_equal(unique(calculate_cash_flows(x)$LOAN_ID), 1:2)
  expect_error(calculate_cash_flows(x, list(col_loanid = "LOAN_ID")), "missing column")
  expect_equal(unique(calculate_cash_flows(x, list(col_loanid = NULL))$LOAN_ID), 1:2)
})

test_that("invalid loan records cannot be skipped or duplicated by grouping", {
  base <- validation_portfolio()
  for (id in list(c("A", "A"), c("A", NA), c("A", " "), c(1, Inf))) {
    x <- base; x$LOAN_ID <- id
    expect_error(calculate_cash_flows(x), "[Ll]oan IDs")
  }
  x <- base; x$LOAN_ID <- c("A", "A"); x$segment <- c("one", "two")
  expect_error(calculate_cash_flows(x, list(return_monthly_totals = TRUE,
                 monthly_totals_group_vars = "segment")), "Duplicate loan IDs")
  for (col in c("balance", "current_interest_rate", "months_to_maturity")) {
    for (bad in list(NA_real_, Inf, NaN, -1, "bad")) {
      x <- base; x[[col]][1] <- bad
      expect_error(calculate_cash_flows(x), col)
    }
  }
  for (bad in c(0, 1.5, .Machine$integer.max + 1)) {
    x <- base; x$months_to_maturity[1] <- bad
    expect_error(calculate_cash_flows(x), "positive integer terms")
  }
  for (bad in list(NA_character_, "bad-date", "2025-02-30")) {
    x <- base; x$eff_date <- c("2025-01-31", bad)
    expect_error(calculate_cash_flows(x), "[Dd]ate|dates")
  }
  x <- base; x$current_interest_rate <- 0
  expect_no_warning(calculate_cash_flows(x, list(servicing_fee = 0)))
})

test_that("optional numeric fields retain NA fallbacks but reject invalid values", {
  x <- validation_portfolio()
  baseline <- calculate_cash_flows(x)
  for (key in c("col_monthly_payment", "col_orig_balance")) {
    x$optional <- c(NA_real_, NA_real_)
    expect_equal(calculate_cash_flows(x, setNames(list("optional"), key)), baseline)
    for (bad in list(0, -1, Inf, NaN, "100")) {
      x$optional <- rep(bad, 2)
      expect_error(calculate_cash_flows(x, setNames(list("optional"), key)), "optional")
    }
  }
})

test_that("assumption vectors require finite values and unique names", {
  x <- validation_portfolio()
  bad_vectors <- list(numeric(), 0.1, c(default = NA_real_), c(default = Inf),
                      c(default = "0.1"), c(default = 0.1, default = 0.2),
                      setNames(0.1, ""), setNames(0.1, NA_character_),
                      c(default = -0.1), c(default = 1.1))
  for (key in c("cpr_vec", "credit_cost_vec", "pd_vec", "lgd_vec")) {
    for (bad in bad_vectors) {
      cfg <- list()
      if (key %in% c("pd_vec", "lgd_vec")) {
        cfg <- list(pd_vec = c(default = 0.1), lgd_vec = c(default = 0.5))
      }
      cfg[key] <- list(bad)
      expect_error(calculate_cash_flows(x, cfg), key)
    }
  }
  expect_error(calculate_cash_flows(x, list(pd_vec = c(default = 0.1))), "supplied together")
  expect_error(calculate_cash_flows(x, list(lgd_vec = c(default = 0.5))), "supplied together")
})

test_that("PD and LGD align by name and effective credit costs cover all tiers", {
  x <- validation_portfolio(); x$tier <- c("A", "B")
  cfg <- list(col_tier = "tier", cpr_vec = c(A = 0, B = 0),
              pd_vec = c(A = 0.1, B = 0.2), lgd_vec = c(B = 0.5, A = 0.3))
  out <- calculate_cash_flows(x, cfg)
  expect_equal(out$credit_loss[out$month == 1], x$balance * c(0.03, 0.1) / 12)
  reordered <- cfg; reordered$pd_vec <- rev(cfg$pd_vec)
  expect_equal(calculate_cash_flows(x, reordered), out)
  cfg$lgd_vec <- c(A = 0.3, C = 0.5)
  expect_error(calculate_cash_flows(x, cfg), "same set of tier names")
  expect_error(calculate_cash_flows(x, list(col_tier = "tier", cpr_vec = c(A = 0, B = 0),
                 credit_cost_vec = c(A = 0))), "Missing: B")
  expect_error(calculate_cash_flows(x, list(col_tier = "tier", cpr_vec = c(A = 0, B = 0),
                 pd_vec = c(A = 0.1), lgd_vec = c(A = 0.3))), "Missing: B")
})

test_that("named default and known tiers do not depend on vector ordering", {
  x <- validation_portfolio()
  cfg <- list(cpr_vec = c(A = 0.5, default = 0), credit_cost_vec = c(default = 0, A = 0.1))
  out <- calculate_cash_flows(x, cfg)
  expect_equal(out$prepayment, rep(0, nrow(out)))
  expect_equal(out$credit_loss, rep(0, nrow(out)))
  expect_true(all(out$tier == "default"))
  cfg$cpr_vec <- rev(cfg$cpr_vec)
  cfg$credit_cost_vec <- rev(cfg$credit_cost_vec)
  expect_equal(calculate_cash_flows(x, cfg), out)
  x$tier <- factor(c("B", "A"))
  cfg <- list(col_tier = "tier", cpr_vec = c(A = 0.1, B = 0.2),
              credit_cost_vec = c(B = 0.01, A = 0.03))
  out <- calculate_cash_flows(x, cfg)
  cfg$cpr_vec <- rev(cfg$cpr_vec); cfg$credit_cost_vec <- rev(cfg$credit_cost_vec)
  expect_equal(calculate_cash_flows(x, cfg), out)
  expect_equal(unique(out$tier), c("B", "A"))
})

test_that("unknown tiers require an explicit default for both assumptions", {
  x <- validation_portfolio(); x$tier <- c("unknown", NA_character_)
  cfg <- list(col_tier = "tier", cpr_vec = c(A = 0.1), credit_cost_vec = c(A = 0.01))
  expect_error(calculate_cash_flows(x, cfg), "Define a 'default' tier")
  cfg$cpr_vec <- c(A = 0.1, default = 0)
  cfg$credit_cost_vec <- c(A = 0.01, default = 0, unknown = 0.2)
  expect_warning(out <- calculate_cash_flows(x, cfg), "named 'default'")
  expect_true(all(out$tier == "default"))
  expect_equal(sum(out$credit_loss), 0)
  expect_equal(sum(out$prepayment), 0)
})

test_that("linear incentive validates inputs and uses the named default", {
  x <- validation_portfolio()
  cfg <- list(prepay_model = "linear_incentive", current_market_rate = 0.05,
              base_cpr_vec = c(A = 0.3, default = 0.1), beta_vec = c(default = 2, A = 1),
              cpr_min_vec = c(A = 0, default = 0), cpr_max_vec = c(default = 1, A = 1),
              credit_cost_vec = c(A = 0.1, default = 0))
  out <- calculate_cash_flows(x, cfg)
  first <- out[out$month == 1, ]
  expect_equal(first$prepayment / (first$starting_balance - first$scheduled_principal),
               1 - (1 - c(0.12, 0.14))^(1/12))
  for (key in c("base_cpr_vec", "beta_vec", "cpr_min_vec", "cpr_max_vec")) {
    bad <- cfg; bad[[key]][1] <- Inf
    expect_error(calculate_cash_flows(x, bad), key)
  }
  for (bad_value in list(NULL, NA_real_, Inf, c(0.01, 0.02))) {
    bad <- cfg; bad["current_market_rate"] <- list(bad_value)
    expect_error(calculate_cash_flows(x, bad), "current_market_rate")
  }
  reordered <- cfg
  for (key in c("base_cpr_vec", "beta_vec", "cpr_min_vec", "cpr_max_vec", "credit_cost_vec")) {
    reordered[[key]] <- rev(cfg[[key]])
  }
  expect_equal(calculate_cash_flows(x, reordered), out)
  x$tier <- c("Z", "default"); cfg$col_tier <- "tier"
  expect_warning(fallback <- calculate_cash_flows(x, cfg), "named 'default'")
  expect_equal(fallback$original_tier, rep(c("Z", "default"), c(12, 24)))
  fallback$original_tier <- out$original_tier
  expect_equal(fallback, out)
})
