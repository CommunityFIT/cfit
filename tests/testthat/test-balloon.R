balloon_loan <- function(balance = 1e6, rate = 0.06, term = 60, amort = 300, ...) {
  data.frame(LOAN_ID = "cre", balance = balance, current_interest_rate = rate,
             months_to_maturity = term, eff_date = as.Date("2026-01-01"),
             amort = amort, ...)
}

balloon_config <- function(...) {
  utils::modifyList(list(col_amort_months_remaining = "amort", servicing_fee = 0,
                         show_progress = FALSE), list(...))
}

# Independent amortization: payment over the amortization term, the remaining
# balance due at maturity.
hand_balloon_schedule <- function(balance, rate, term, amort) {
  r <- rate / 12
  pmt <- if (r == 0) balance / amort else balance * r / (1 - (1 + r)^(-amort))
  bal <- balance
  interest <- principal <- numeric(term)
  for (m in seq_len(term)) {
    interest[m] <- bal * r
    principal[m] <- if (m == term) bal else pmt - interest[m]
    bal <- bal - principal[m]
  }
  list(payment = pmt, interest = interest, principal = principal)
}

test_that("payments amortize over the remaining amortization term with a balloon at maturity", {
  out <- calculate_cash_flows(balloon_loan(), balloon_config())
  hand <- hand_balloon_schedule(1e6, 0.06, 60, 300)
  expect_equal(nrow(out), 60)
  expect_equal(out$scheduled_payment, rep(hand$payment, 60), tolerance = 1e-10)
  expect_equal(out$gross_interest, hand$interest, tolerance = 1e-8)
  expect_equal(out$total_principal, hand$principal, tolerance = 1e-8)
  expect_equal(out$balloon_principal[1:59], rep(0, 59))
  expect_equal(out$balloon_principal[60], hand$principal[60] - (hand$payment - hand$interest[60]),
               tolerance = 1e-8)
  expect_equal(tail(out$remaining_balance, 1), 0)
  # The balloon is part of scheduled principal; the reconciliation still holds.
  expect_equal(out$total_principal, out$scheduled_principal + out$prepayment)
  expect_equal(sum(out$total_principal), 1e6, tolerance = 1e-8)
})

test_that("balloon cash flows give hand-calculated WAL, PV, and duration", {
  out <- calculate_cash_flows(balloon_loan(), balloon_config())
  hand <- hand_balloon_schedule(1e6, 0.06, 60, 300)
  t <- seq_len(60)
  cf <- hand$interest + hand$principal
  pv <- cf / (1 + 0.06 / 12)^t
  expect_equal(calculate_wal(out)$portfolio_wal,
               sum(t / 12 * hand$principal) / sum(hand$principal), tolerance = 1e-10)
  dur <- calculate_duration(out)
  # Discounted at the note rate, the complete cash flows are worth par.
  expect_equal(dur$portfolio_pv, 1e6, tolerance = 1e-8)
  expect_equal(dur$macaulay_duration, sum(t / 12 * pv) / sum(pv), tolerance = 1e-10)
})

test_that("non-balloon loans are unchanged by the balloon options", {
  x <- data.frame(LOAN_ID = c("a", "b"), balance = c(20000, 35000),
                  current_interest_rate = c(0.07, 0.05), months_to_maturity = c(48, 60),
                  eff_date = as.Date("2026-01-01"), tier = c("A", "B"))
  cfg <- list(col_tier = "tier", cpr_vec = c(A = 0.1, B = 0.15),
              credit_cost_vec = c(A = 0.01, B = 0.02))
  for (survivors in c(TRUE, FALSE)) {
    cfg$reamortize_survivors <- survivors
    base <- calculate_cash_flows(x, cfg)
    expect_true(all(base$balloon_principal == 0))
    x$amort <- NA_real_
    expect_identical(calculate_cash_flows(x, c(cfg, col_amort_months_remaining = "amort")), base)
    x$amort <- x$months_to_maturity
    expect_identical(calculate_cash_flows(x, c(cfg, col_amort_months_remaining = "amort")), base)
    x$flag <- c(FALSE, NA)
    expect_identical(calculate_cash_flows(x, c(cfg, col_balloon = "flag")), base)
    # Flagging a loan that fully amortizes by maturity leaves nothing to balloon.
    x$flag <- c(TRUE, TRUE)
    expect_no_message(flagged <- calculate_cash_flows(x, c(cfg, col_balloon = "flag")))
    expect_identical(flagged, base)
    expect_identical(calculate_cash_flows(x, c(cfg, balloon_at_maturity = TRUE)), base)
    x$amort <- x$flag <- NULL
  }
})

test_that("mixed pools project balloon and amortizing loans side by side", {
  x <- rbind(balloon_loan(), balloon_loan(balance = 20000, rate = 0.07, term = 48, amort = NA))
  x$LOAN_ID <- c("cre", "auto")
  out <- calculate_cash_flows(x, balloon_config(return_monthly_totals = TRUE))
  auto <- out$loan_cash_flows[out$loan_cash_flows$LOAN_ID == "auto", ]
  cre <- out$loan_cash_flows[out$loan_cash_flows$LOAN_ID == "cre", ]
  expect_equal(nrow(auto), 48)
  expect_true(all(auto$balloon_principal == 0))
  expect_equal(sum(cre$balloon_principal > 0), 1)
  expect_equal(sum(out$monthly_totals$balloon_principal), sum(cre$balloon_principal))
  expect_equal(out$monthly_totals$balloon_principal[60], cre$balloon_principal[60])
})

test_that("a flagged interest-only loan repays its whole balance at maturity", {
  x <- balloon_loan(amort = NA_real_, payment = 5000, flag = TRUE)
  cfg <- balloon_config(col_monthly_payment = "payment", col_balloon = "flag")
  expect_no_warning(out <- calculate_cash_flows(x, cfg))
  expect_equal(out$scheduled_principal[1:59], rep(0, 59))
  expect_equal(out$balloon_principal[60], 1e6)
  expect_equal(out$total_principal[60], 1e6)
  # Without the flag the residual remains and the maturity warning names the options.
  x$flag <- FALSE
  expect_warning(out <- calculate_cash_flows(x, cfg),
                 "No balloon payoff has been added.*col_amort_months_remaining")
  expect_equal(tail(out$remaining_balance, 1), 1e6)
})

test_that("balloon_at_maturity applies to every loan without an explicit flag", {
  x <- balloon_loan(amort = NA_real_, payment = 5000)
  x <- rbind(x, x, x)
  x$LOAN_ID <- c("flag_na", "flag_false", "flag_true")
  x$flag <- c(NA, FALSE, TRUE)
  out <- suppressWarnings(calculate_cash_flows(x, balloon_config(
    col_monthly_payment = "payment", col_balloon = "flag", balloon_at_maturity = TRUE)))
  final <- out[out$month == 60, ]
  expect_equal(final$balloon_principal, c(1e6, 0, 1e6))
  # Scalar only, no flag column: every loan pays a balloon.
  x$flag <- NULL
  out <- calculate_cash_flows(x, balloon_config(col_monthly_payment = "payment",
                                                balloon_at_maturity = TRUE))
  expect_equal(out$balloon_principal[out$month == 60], rep(1e6, 3))
})

test_that("an amortization term beyond maturity pays a balloon even when flagged FALSE", {
  x <- balloon_loan(flag = FALSE)
  expect_warning(out <- calculate_cash_flows(x, balloon_config(col_balloon = "flag")),
                 "col_balloon = FALSE.*pay a balloon.*cre")
  expect_gt(out$balloon_principal[60], 0)
})

test_that("no prepayment is projected in the balloon month; losses still apply", {
  for (survivors in c(TRUE, FALSE)) {
    out <- calculate_cash_flows(balloon_loan(term = 24), balloon_config(
      reamortize_survivors = survivors, cpr_vec = c(default = 0.2),
      credit_cost_vec = c(default = 0.02)))
    expect_equal(nrow(out), 24)
    expect_true(all(out$prepayment[1:23] > 0))
    expect_equal(out$prepayment[24], 0)
    expect_gt(out$credit_loss[24], 0)
    expect_equal(out$balloon_principal[24],
                 out$starting_balance[24] - out$credit_loss[24] -
                   (out$scheduled_principal[24] - out$balloon_principal[24]), tolerance = 1e-8)
    expect_equal(tail(out$remaining_balance, 1), 0)
    expect_equal(out$total_principal, out$scheduled_principal + out$prepayment)
    expect_equal(sum(out$total_principal) + sum(out$credit_loss), 1e6, tolerance = 1e-8)
  }
})

test_that("a tape payment that retires the loan before maturity pays no balloon", {
  x <- balloon_loan(balance = 1000, rate = 0, term = 12, amort = 120, payment = 500)
  out <- calculate_cash_flows(x, balloon_config(col_monthly_payment = "payment"))
  expect_equal(nrow(out), 2)
  expect_equal(out$balloon_principal, c(0, 0))
  # With a supplied payment, the amortization term marks the balloon but the
  # payment amount comes from the tape.
  x$payment <- 10
  out <- calculate_cash_flows(x, balloon_config(col_monthly_payment = "payment"))
  expect_equal(out$scheduled_payment, rep(10, 12))
  expect_equal(out$balloon_principal[12], 1000 - 12 * 10)
})

test_that("zero-rate balloon loans amortize evenly over the amortization term", {
  out <- calculate_cash_flows(balloon_loan(balance = 1200, rate = 0, term = 12, amort = 120),
                              balloon_config())
  expect_equal(out$scheduled_payment, rep(10, 12))
  expect_equal(out$total_principal, c(rep(10, 11), 1090))
  expect_equal(out$balloon_principal[12], 1080)
  expect_equal(calculate_wal(out)$portfolio_wal,
               (sum(10 * 1:11) + 1090 * 12) / 1200 / 12, tolerance = 1e-12)
})

test_that("balloon inputs are validated", {
  bad_amort <- list(list(59, "at least the remaining maturity"), list(120.5, "positive integer"),
                    list(-1, "positive"), list(NaN, "missing or non-finite"),
                    list(Inf, "missing or non-finite"))
  for (case in bad_amort) {
    expect_error(calculate_cash_flows(balloon_loan(amort = case[[1]]), balloon_config()),
                 case[[2]])
  }
  x <- balloon_loan(); x$amort <- "300"
  expect_error(calculate_cash_flows(x, balloon_config()), "must be numeric")
  for (bad in list(2, "Y", NaN)) {
    expect_error(calculate_cash_flows(balloon_loan(flag = bad),
                                      balloon_config(col_balloon = "flag")), "col_balloon|logical")
  }
  expect_error(calculate_cash_flows(balloon_loan(), balloon_config(col_balloon = "missing")),
               "col_balloon specifies missing column")
  expect_error(calculate_cash_flows(balloon_loan(), balloon_config(
    col_amort_months_remaining = "missing")), "col_amort_months_remaining specifies missing column")
  for (bad in list(NA, "TRUE", c(TRUE, FALSE), 1)) {
    expect_error(calculate_cash_flows(balloon_loan(), balloon_config(balloon_at_maturity = bad)),
                 "balloon_at_maturity must be TRUE or FALSE")
  }
  expect_error(calculate_cash_flows(balloon_loan(), balloon_config(col_balloon = TRUE)),
               "col_balloon must be a single non-empty column name or NULL")
  expect_error(calculate_cash_flows(balloon_loan(), balloon_config(
    return_monthly_totals = TRUE, monthly_totals_group_vars = "balloon_principal")),
    "Cannot group by cash-flow measure")
  # 0/1 flags are accepted.
  out <- calculate_cash_flows(balloon_loan(amort = NA_real_, payment = 5000, flag = 1),
    balloon_config(col_monthly_payment = "payment", col_balloon = "flag"))
  expect_equal(out$balloon_principal[60], 1e6)
})

test_that("balloon payoffs are summarised when show_progress is TRUE", {
  expect_message(calculate_cash_flows(balloon_loan(), balloon_config(show_progress = TRUE)),
                 "1 loan\\(s\\) pay a balloon at maturity")
  expect_no_message(calculate_cash_flows(balloon_loan(), balloon_config()))
})
