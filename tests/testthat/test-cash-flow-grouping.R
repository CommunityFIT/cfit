grouping_portfolio <- function() {
  data.frame(id = c("a", "b", "c"), balance = c(1000, 2000, 3000),
    current_interest_rate = c(0.04, 0.06, 0.08), months_to_maturity = c(6, 12, 18),
    eff_date = as.Date("2026-01-31"), classification = c("A", "C", NA),
    region = c("West", "East", NA))
}
grouping_config <- function(groups = NULL, totals = TRUE) {
  list(col_loanid = "id", col_tier = "classification",
    cpr_vec = c(A = 0.1, default = 0.2), credit_cost_vec = c(A = 0.01, default = 0.02),
    return_monthly_totals = totals, monthly_totals_group_vars = groups)
}

test_that("original classifications survive fallback and missing values", {
  x <- grouping_portfolio()
  expect_warning(out <- calculate_cash_flows(x, grouping_config(c("original_tier", "tier"))),
                 "named 'default'")
  first <- out$loan_cash_flows[out$loan_cash_flows$month == 1, ]
  expect_equal(first$original_tier, c("A", "C", NA_character_))
  expect_equal(first$tier, c("A", "default", "default"))
  expect_true(anyNA(out$monthly_totals$original_tier))
  x$classification <- factor(x$classification)
  expect_warning(factors <- calculate_cash_flows(x, grouping_config()), "named 'default'")
  expect_equal(factors$loan_cash_flows$original_tier, out$loan_cash_flows$original_tier)
  cfg <- grouping_config(); cfg$col_tier <- NULL
  plain <- calculate_cash_flows(x, cfg)
  expect_true(all(is.na(plain$loan_cash_flows$original_tier)))
})

test_that("grouped totals and loan projections preserve every monetary amount", {
  x <- grouping_portfolio()
  baseline <- suppressWarnings(calculate_cash_flows(x, grouping_config()))
  for (groups in list("region", "original_tier", c("region", "original_tier"),
                      c("date", "tier"), "id")) {
    out <- suppressWarnings(calculate_cash_flows(x, grouping_config(groups)))
    expect_equal(out$loan_cash_flows[, names(baseline$loan_cash_flows)], baseline$loan_cash_flows)
    amounts <- setdiff(names(baseline$monthly_totals), "date")
    collapsed <- out$monthly_totals %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(amounts), sum), .groups = "drop")
    expect_equal(collapsed, baseline$monthly_totals, tolerance = 1e-10)
    m <- out$monthly_totals
    expect_equal(m$total_principal, m$scheduled_principal + m$prepayment, tolerance = 1e-10)
    expect_equal(m$starting_balance, m$total_principal + m$credit_loss + m$remaining_balance,
                 tolerance = 1e-10)
    reversed <- suppressWarnings(calculate_cash_flows(x[3:1, ], grouping_config(groups)))
    expect_equal(reversed$monthly_totals, out$monthly_totals, tolerance = 1e-10)
  }
})

test_that("missing and unsafe grouping requests fail in both return modes", {
  x <- grouping_portfolio()
  x$classification <- "A"
  for (totals in c(TRUE, FALSE)) {
    expect_error(calculate_cash_flows(x, grouping_config("typo", totals)), "Missing grouping columns")
    expect_error(calculate_cash_flows(x, grouping_config("total_principal", totals)), "cash-flow measure")
    for (col in c("date", "month", "rate", "tier", "original_tier", "LOAN_ID")) {
      bad <- x; bad[[col]] <- "input classification"
      expect_error(calculate_cash_flows(bad, grouping_config(col, totals)), "Ambiguous grouping column")
    }
    x$list_group <- list(1, 2, 3)
    expect_error(calculate_cash_flows(x, grouping_config("list_group", totals)), "atomic vector")
  }
})

test_that("mapped output names remain unambiguous and grouping does not mutate input", {
  x <- grouping_portfolio(); x$classification <- "A"
  x$tier <- x$classification; x$LOAN_ID <- x$id; x$rate <- x$current_interest_rate
  before <- x
  cfg <- grouping_config(c("LOAN_ID", "tier", "rate", "eff_date", "month", "date"))
  cfg$col_loanid <- "LOAN_ID"; cfg$col_rate <- "rate"; cfg$col_tier <- "tier"
  expect_no_warning(out <- calculate_cash_flows(x, cfg))
  expect_equal(nrow(out$monthly_totals), nrow(out$loan_cash_flows))
  expect_identical(x, before)
})

test_that("metadata lookup explicitly rejects duplicate and unmatched IDs", {
  x <- grouping_portfolio(); x$classification <- "A"
  cfg <- grouping_config()
  out <- calculate_cash_flows(x, cfg)$loan_cash_flows
  expect_error(attach_cash_flow_groups(out, rbind(x, x[1, ]), cfg), "exactly one record")
  expect_error(attach_cash_flow_groups(out, x[-1, ], cfg), "missing a projected loan ID")
})
