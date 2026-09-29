accounting_loan <- function(balance = 1000, rate = 0.06, term = 2, payment = 995) {
  data.frame(LOAN_ID = "example", balance = balance, current_interest_rate = rate,
             months_to_maturity = term, eff_date = as.Date("2026-01-01"), payment = payment)
}

accounting_config <- function(...) {
  utils::modifyList(list(col_monthly_payment = "payment", servicing_fee = 0), list(...))
}

expect_principal_reconciles <- function(out, initial) {
  expect_equal(out$total_principal, out$scheduled_principal + out$prepayment, tolerance = 1e-10)
  expect_equal(out$starting_balance, out$total_principal + out$credit_loss + out$remaining_balance,
               tolerance = 1e-10)
  expect_equal(sum(out$total_principal) + sum(out$credit_loss) + tail(out$remaining_balance, 1),
               initial, tolerance = 1e-10)
  if (nrow(out) > 1) {
    expect_equal(out$starting_balance[-1], head(out$remaining_balance, -1), tolerance = 1e-10)
    expect_true(all(head(out$remaining_balance, -1) > 0))
  }
}

test_that("fixed-payment payoff includes prepayment at ordinary speeds", {
  out <- calculate_cash_flows(accounting_loan(), accounting_config(
    reamortize_survivors = FALSE, cpr_vec = c(default = 1 - 0.99^12)))
  expect_equal(nrow(out), 1)
  expect_equal(out$scheduled_principal, 990)
  expect_equal(out$prepayment, 10)
  expect_equal(out$total_principal, 1000)
  expect_equal(out$remaining_balance, 0)
  expect_equal(out$gross_interest, 5)
  expect_principal_reconciles(out, 1000)
})

test_that("full prepayment respects credit losses in both conventions", {
  for (survivors in c(TRUE, FALSE)) {
    for (loss in c(0, 0.12)) {
      out <- calculate_cash_flows(accounting_loan(), accounting_config(
        reamortize_survivors = survivors, cpr_vec = c(default = 1),
        credit_cost_vec = c(default = loss)))
      expect_equal(nrow(out), 1)
      expect_equal(out$credit_loss, 1000 * loss / 12)
      expect_equal(out$total_principal, 1000 - out$credit_loss)
      expect_equal(out$remaining_balance, 0)
      expect_principal_reconciles(out, 1000)
    }
  }
})

test_that("cleanup is scheduled principal and flows into investor and grouped totals", {
  for (survivors in c(TRUE, FALSE)) {
    x <- accounting_loan(balance = 100, rate = 0, term = 1, payment = 99.5)
    out <- calculate_cash_flows(x, accounting_config(reamortize_survivors = survivors,
      investor_share = 0.4, return_monthly_totals = TRUE))
    loan <- out$loan_cash_flows
    expect_equal(loan$scheduled_principal, 100)
    expect_equal(loan$scheduled_payment, 99.5)
    expect_equal(loan$prepayment, 0)
    expect_equal(loan$investor_principal, 40)
    expect_equal(out$monthly_totals$scheduled_principal, 100)
    expect_equal(out$monthly_totals$investor_total, 40)
    expect_principal_reconciles(loan, 100)
  }
})

test_that("sub-cent balances are projected and zero cleanup does not drop them", {
  out <- calculate_cash_flows(accounting_loan(balance = 0.005, rate = 0, term = 1,
                                              payment = 0.005), accounting_config())
  expect_equal(out$total_principal, 0.005)
  x <- accounting_loan(balance = 1, rate = 0, term = 2, payment = 0.995)
  out <- calculate_cash_flows(x, accounting_config(de_minimis_balance = 0))
  expect_equal(nrow(out), 2)
  expect_equal(out$total_principal, c(0.995, 0.005))
  expect_principal_reconciles(out, 1)
})

test_that("supplied underpayments fail under the selected interest convention", {
  for (survivors in c(TRUE, FALSE)) {
    expect_error(calculate_cash_flows(accounting_loan(payment = 4.99),
      accounting_config(reamortize_survivors = survivors)),
      "Loan 'example', month 1: supplied payment.*below accrued interest")
  }
  # Post-prepayment accrual in fixed-payment mode owes only 2.50 interest.
  x <- accounting_loan(payment = 3)
  cfg <- accounting_config(reamortize_survivors = FALSE, interest_on_starting_balance = FALSE,
                           cpr_vec = c(default = 1 - 0.5^12))
  expect_warning(out <- calculate_cash_flows(x, cfg), "contractual maturity")
  expect_equal(out$gross_interest[1], 2.5)
  expect_equal(out$scheduled_principal[1], 0.5)
  # A zero-rate loan cannot underpay positive accrued interest.
  expect_no_warning(calculate_cash_flows(accounting_loan(rate = 0, payment = 500), accounting_config()))
})

test_that("interest-only survivor payments tolerate rounding and retain residuals", {
  expect_warning(out <- calculate_cash_flows(accounting_loan(payment = 5), accounting_config(
    credit_cost_vec = c(default = 0.12))), "contractual maturity")
  expect_equal(out$scheduled_payment, c(5, 4.95))
  expect_equal(out$gross_interest, c(5, 4.95))
  expect_principal_reconciles(out, 1000)
})

test_that("maturity warning preserves residuals without inventing a balloon", {
  for (totals in c(TRUE, FALSE)) {
    x <- accounting_loan(payment = 5)
    expect_warning(result <- calculate_cash_flows(x, accounting_config(return_monthly_totals = totals)),
                   "1 loan.*contractual maturity.*1000.*example.*No balloon payoff")
    out <- if (totals) result$loan_cash_flows else result
    expect_equal(out$total_principal, c(0, 0))
    expect_equal(tail(out$remaining_balance, 1), 1000)
    expect_principal_reconciles(out, 1000)
  }
  x <- accounting_loan(balance = 100, rate = 0, term = 1, payment = 99)
  expect_warning(calculate_cash_flows(x, accounting_config()), "contractual maturity")
  # Roundoff dust is not a material residual, even with cleanup disabled.
  expect_no_warning(calculate_cash_flows(accounting_loan(balance = 100, rate = 0, term = 1,
    payment = 100 - 1e-12), accounting_config(de_minimis_balance = 0)))
})

test_that("principal reconciles over multi-period prepayments and losses", {
  for (survivors in c(TRUE, FALSE)) {
    for (rate in c(0, 0.06)) {
      for (cpr in c(0, 0.2, 1)) {
        x <- accounting_loan(rate = rate, term = 24)
        cfg <- list(servicing_fee = 0, reamortize_survivors = survivors,
                    cpr_vec = c(default = cpr), credit_cost_vec = c(default = 0.03))
        out <- calculate_cash_flows(x, cfg)
        expect_principal_reconciles(out, 1000)
        expect_equal(tail(out$remaining_balance, 1), 0, tolerance = 1e-10)
      }
    }
  }
})

test_that("a binding cap before payoff does not discard partial prepayments", {
  # 20% monthly prepayment and 650 scheduled principal: 850 collected, not 800.
  out <- calculate_cash_flows(accounting_loan(rate = 0, payment = 650), accounting_config(
    reamortize_survivors = FALSE, cpr_vec = c(default = 1 - 0.8^12)))
  expect_equal(out$total_principal, c(850, 150))
  expect_equal(out$remaining_balance, c(150, 0))
  expect_principal_reconciles(out, 1000)
})

test_that("maturity diagnostics aggregate multiple loans in a single warning", {
  x <- rbind(accounting_loan(payment = 5), accounting_loan(payment = 5))
  x$LOAN_ID <- c("one", "two")
  messages <- character()
  out <- withCallingHandlers(calculate_cash_flows(x, accounting_config()),
    warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_length(messages, 1)
  expect_match(messages, "2 loan.*2000.*one, two")
  expect_equal(sum(out$total_principal), 0)
})
