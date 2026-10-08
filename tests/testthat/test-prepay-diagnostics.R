prepay_history <- function() {
  data.frame(ID = rep(c("a", "b"), 3),
    EFFDATE = rep(as.Date(c("2024-01-31", "2024-02-29", "2024-03-31")), each = 2),
    ORIGDATE = as.Date("2023-01-01"), TYPECODE = rep(c("A", "B"), 3),
    BAL = c(1000, 2000, 900, 1800, 800, 1600), ORIGBAL = rep(c(1000, 2000), 3),
    PAYAMT = rep(c(100, 200), 3), CURRINTRATE = 0)
}
prepay_estimate <- function(x, ids = TRUE, ...) {
  calculate_prepay_speed(x, c("EFFDATE", "TYPECODE"),
    utils::modifyList(list(col_loanid = if (ids) "ID" else NULL, interest_basis = NA), list(...)))
}

test_that("complete monthly snapshots retain hand-calculated estimates", {
  x <- prepay_history()
  out <- prepay_estimate(x)
  expect_equal(nrow(out), 4)
  expect_equal(out$SMM, rep(0, 4))
  expect_equal(out$CPR, rep(0, 4))
  expect_equal(out$SMM_RAW, out$SMM)
  expect_false(any(out$SMM_ADJUSTED))
  expect_true(all(out$DIAGNOSTIC == "ok"))
  expect_equal(out$UNRESOLVED_EXITS, rep(0L, 4))
  expect_equal(prepay_estimate(x[6:1, ]), out)
  expect_true(all(is.na(prepay_estimate(x, ids = FALSE)$UNRESOLVED_EXITS)))
  x$BAL[3] <- 850
  out <- prepay_estimate(x)
  expect_equal(out$SMM[1], 50/900)
  expect_equal(out$CPR[1], 1 - (1 - 50/900)^12)
})

test_that("global gaps and duplicate calendar reporting months fail", {
  x <- prepay_history()
  expect_error(prepay_estimate(x[-c(3, 4), ]), "Missing reporting month")
  extra <- x[1, ]; extra$EFFDATE <- as.Date("2024-01-15")
  expect_error(prepay_estimate(rbind(x, extra)), "one reporting date")
  # December -> January is a valid consecutive calendar interval.
  x <- x[1:4, ]; x$EFFDATE <- rep(as.Date(c("2023-12-31", "2024-01-31")), each = 2)
  expect_no_warning(prepay_estimate(x))
})

test_that("disappearing and reappearing cohorts never bridge missing observations", {
  x <- prepay_history()[-4, ]
  for (ids in c(TRUE, FALSE)) {
    expect_warning(out <- prepay_estimate(x, ids), "undefined prepayment estimates")
    b <- out[out$TYPECODE == "B", ]
    expect_equal(nrow(b), 2)
    expect_true(b$COHORT_DISAPPEARED[1])
    expect_equal(b$BEGIN_BAL[1], 2000)
    expect_true(is.na(b$END_BAL[1]))
    expect_true(is.na(b$BEGIN_BAL[2]))
    expect_true(all(is.na(b$ACTUAL_PRIN)))
    expect_true(all(is.na(b$PREPAYMENT)))
    expect_true(all(is.na(b$SMM_RAW)))
    expect_true(all(is.na(b$CPR)))
    expect_match(b$DIAGNOSTIC[1], "cohort_disappeared")
    expect_match(b$DIAGNOSTIC[2], "no_prior_cohort")
    expect_equal(out$SMM[out$TYPECODE == "A"], c(0, 0))
  }
})

test_that("exits within surviving cohorts and transfers are unresolved", {
  x <- prepay_history(); x$TYPECODE <- "POOL"
  expect_warning(out <- prepay_estimate(x[-4, ]), "undefined")
  expect_equal(out$UNRESOLVED_EXITS, c(1L, 0L))
  expect_equal(out$UNRESOLVED_ENTRIES, c(0L, 1L))
  expect_true(all(is.na(out$CPR)))
  x <- prepay_history(); x$TYPECODE[3] <- "B"
  expect_warning(out <- prepay_estimate(x), "undefined")
  expect_true(any(out$UNRESOLVED_EXITS > 0))
  expect_true(any(out$UNRESOLVED_ENTRIES > 0))
  expect_true(all(is.na(out$CPR)))
})

test_that("new originations keep funding estimates without unresolved entry flags", {
  x <- prepay_history()[1:4, ]; x$TYPECODE <- "POOL"
  new <- x[3, ]; new$ID <- "new"; new$ORIGDATE <- as.Date("2024-02-01")
  new$BAL <- 500; new$ORIGBAL <- 500
  out <- prepay_estimate(rbind(x, new))
  expect_equal(out$FUNDED_BAL, 500)
  expect_equal(out$UNRESOLVED_ENTRIES, 0L)
  expect_equal(out$CPR, 0)
})

test_that("zero and negative denominators return explicit NA, never Inf or NaN", {
  x <- prepay_history()[c(1, 3), ]
  for (payment in c(1000, 1100)) {
    x$PAYAMT <- payment
    expect_warning(out <- prepay_estimate(x), "undefined")
    expect_true(is.na(out$SMM_RAW) && !is.nan(out$SMM_RAW))
    expect_true(is.na(out$SMM) && !is.nan(out$SMM))
    expect_true(is.na(out$CPR) && !is.nan(out$CPR))
    expect_true(is.na(out$SMM_ADJUSTED))
    expect_match(out$DIAGNOSTIC, "invalid_denominator")
    expect_equal(out$AVAILABLE_TO_PREPAY, 1000 - payment)
  }
})

test_that("raw and bounded SMM are distinct and CPR always uses reported SMM", {
  x <- prepay_history()[c(1, 3), ]; x$BAL[2] <- 4000
  for (negative in c(TRUE, FALSE)) {
    out <- prepay_estimate(x, allow_negative_prepay = negative)
    expect_equal(out$SMM_RAW, -3100/900)
    expect_equal(out$SMM, if (negative) -1 else 0)
    expect_true(out$SMM_ADJUSTED)
    expect_match(out$DIAGNOSTIC, "negative_prepayment")
    expect_match(out$DIAGNOSTIC, "out_of_range_smm")
    expect_equal(out$CPR, 1 - (1-out$SMM)^12)
  }
  # Funding at the reported balance cannot push the raw estimate above one, even
  # when ORIGBAL (here 2000) exceeds the balance actually reported (900).
  x$BAL[2] <- 900; x$ORIGDATE[2] <- as.Date("2024-02-01"); x$ORIGBAL[2] <- 2000
  out <- prepay_estimate(x, ids = FALSE)
  expect_equal(out$FUNDED_BAL, 900)
  expect_equal(out$SMM_RAW, 1)
  expect_false(out$SMM_ADJUSTED)
})

test_that("diagnostics survive filters, custom names, missing cohort labels and empty results", {
  x <- prepay_history()[-4, ]
  expect_warning(out <- prepay_estimate(x, min_begin_balance = 1e6), "undefined")
  expect_true(all(out$DIAGNOSTIC != "ok"))
  x <- prepay_history(); x$TYPECODE[x$TYPECODE == "B"] <- NA_character_
  expect_equal(nrow(prepay_estimate(x)), 4)
  names(x)[names(x) == "EFFDATE"] <- "ReportDate"
  names(x)[names(x) == "TYPECODE"] <- "Product"
  out <- calculate_prepay_speed(x, c("Product", "ReportDate"),
    list(col_effdate = "ReportDate", col_typecode = "Product", col_loanid = "ID"))
  expect_true(all(c("Product", "ReportDate", "DIAGNOSTIC") %in% names(out)))
  expect_equal(nrow(prepay_estimate(prepay_history()[1:2, ])), 0)
  # Portfolio-only grouping also supports exit detection.
  x <- prepay_history()[-4, ]
  expect_warning(out <- calculate_prepay_speed(x, "EFFDATE", list(col_loanid = "ID")), "undefined")
  expect_equal(out$UNRESOLVED_EXITS, c(1L, 0L))
})

test_that("missing numeric inputs cannot become zero-valued aggregates", {
  x <- prepay_history()
  for (col in c("BAL", "ORIGBAL", "PAYAMT", "CURRINTRATE")) {
    for (bad in list(NA_real_, Inf, -1, "bad")) {
      invalid <- x; invalid[[col]][1] <- bad
      expect_error(prepay_estimate(invalid), col)
    }
  }
  x$ID[1] <- NA_character_
  expect_error(prepay_estimate(x), "Loan IDs")
})

test_that("reserved grouping names and invalid diagnostic controls are rejected", {
  x <- prepay_history(); x$DIAGNOSTIC <- "classification"
  expect_error(calculate_prepay_speed(x, c("EFFDATE", "DIAGNOSTIC")), "reserved prepayment output")
  for (bad in list(NA, c(TRUE, FALSE), 1)) {
    expect_error(prepay_estimate(x, allow_negative_prepay = bad), "allow_negative_prepay")
  }
  for (bad in list(NA_real_, Inf, c(0, 1))) {
    expect_error(prepay_estimate(x, min_begin_balance = bad), "min_begin_balance")
  }
})

test_that("portfolio exits are payoffs by default and unresolved in strict mode", {
  # Loan b leaves the portfolio before March and never returns.
  x <- prepay_history()[-6, ]; x$TYPECODE <- "POOL"
  expect_no_warning(out <- prepay_estimate(x))
  expect_equal(out$PAYOFF_EXITS, c(0L, 1L))
  expect_equal(out$UNRESOLVED_EXITS, c(0L, 0L))
  expect_equal(out$DIAGNOSTIC, c("ok", "ok"))
  # b's final scheduled payment (200) is scheduled principal, not prepayment.
  expect_equal(out$EXIT_SCHED_PRIN, c(0, 200))
  expect_equal(out$SCHED_PRIN_TOTAL[2], 100 + 200)
  expect_equal(out$PREPAYMENT[2], 1800 - 200)
  expect_equal(out$SMM[2], 1600 / 2400)
  expect_equal(out$CPR[2], 1 - (1 - 1600 / 2400)^12)
  # Portfolio-only grouping, as used for whole-portfolio speeds.
  expect_no_warning(all <- calculate_prepay_speed(x, "EFFDATE",
    list(col_loanid = "ID", interest_basis = NA)))
  expect_equal(all$SMM, out$SMM)
  expect_warning(strict <- prepay_estimate(x, exit_treatment = "unresolved"), "undefined")
  expect_equal(strict$UNRESOLVED_EXITS, c(0L, 1L))
  expect_equal(strict$PAYOFF_EXITS, c(0L, 0L))
  expect_true(is.na(strict$CPR[2]))
  expect_match(strict$DIAGNOSTIC[2], "unresolved_exits")
})

test_that("listed non-prepayment exits are removed from runoff and the pool", {
  x <- prepay_history()[-6, ]; x$TYPECODE <- "POOL"
  for (mode in c("payoff", "unresolved")) {
    out <- prepay_estimate(x, exit_treatment = mode, non_prepay_exit_ids = "b")
    expect_equal(out$EXCLUDED_EXIT_BAL, c(0, 1800))
    expect_equal(out$PAYOFF_EXITS, c(0L, 0L))
    expect_equal(out$EXIT_SCHED_PRIN, c(0, 0))
    expect_equal(out$ACTUAL_PRIN[2], 100)
    expect_equal(out$AVAILABLE_TO_PREPAY[2], 800)
    expect_equal(out$SMM[2], 0)
    expect_equal(out$DIAGNOSTIC[2], "ok")
  }
})

test_that("a cohort whose loans all paid off ends at zero balance", {
  x <- prepay_history()[-6, ]
  out <- prepay_estimate(x)
  b <- out[out$TYPECODE == "B", ]
  expect_true(b$COHORT_DISAPPEARED[2])
  expect_equal(b$END_BAL[2], 0)
  expect_equal(b$SCHED_PRIN_TOTAL[2], 200)
  expect_equal(b$SMM[2], 1)
  expect_equal(b$DIAGNOSTIC[2], "ok")
  # Without loan IDs the disappearance remains unresolved.
  expect_warning(out <- prepay_estimate(x, ids = FALSE), "undefined")
  expect_match(out$DIAGNOSTIC[out$TYPECODE == "B"][2], "cohort_disappeared")
})

test_that("new loans are funding at their first reported balance", {
  x <- prepay_history()[1:4, ]; x$TYPECODE <- "POOL"
  late <- x[3, ]; late$ID <- "late"; late$ORIGDATE <- as.Date("2024-01-31")
  late$BAL <- 500; late$ORIGBAL <- 500
  out <- prepay_estimate(rbind(x, late))
  expect_equal(out$FUNDED_BAL, 500)
  expect_equal(out$UNRESOLVED_ENTRIES, 0L)
  expect_equal(out$LATE_FUNDED_ENTRIES, 0L)
  expect_equal(out$CPR, 0)
  # A loan first reported long after origination is new balance by default...
  late$ORIGDATE <- as.Date("2023-11-15")
  expect_no_warning(out <- prepay_estimate(rbind(x, late)))
  expect_equal(out$FUNDED_BAL, 500)
  expect_equal(out$LATE_FUNDED_ENTRIES, 1L)
  expect_equal(out$UNRESOLVED_ENTRIES, 0L)
  expect_equal(out$CPR, 0)
  # ...and unresolved when entry_treatment = "unresolved".
  expect_warning(out <- prepay_estimate(rbind(x, late), entry_treatment = "unresolved"), "undefined")
  expect_equal(out$UNRESOLVED_ENTRIES, 1L)
  expect_equal(out$LATE_FUNDED_ENTRIES, 0L)
  expect_equal(out$FUNDED_BAL, 0)
  expect_true(is.na(out$CPR))
})

test_that("exit controls are validated", {
  x <- prepay_history()
  for (bad in list("drop", NA_character_, c("payoff", "unresolved"), TRUE)) {
    expect_error(prepay_estimate(x, exit_treatment = bad), "exit_treatment")
  }
  expect_error(prepay_estimate(x, non_prepay_exit_ids = c("a", NA)), "non_prepay_exit_ids")
  expect_error(prepay_estimate(x, ids = FALSE, non_prepay_exit_ids = "a"), "requires 'col_loanid'")
  for (bad in list("drop", NA_character_, c("funding", "unresolved"), 1)) {
    expect_error(prepay_estimate(x, entry_treatment = bad), "entry_treatment")
  }
})

test_that("a loan making its final scheduled payment is not a prepayment", {
  d <- as.Date(c("2024-01-31", "2024-02-29"))
  base <- data.frame(ID = c("A", "B", "A"), EFFDATE = d[c(1, 1, 2)],
    ORIGDATE = as.Date("2020-01-15"), TYPECODE = "P", ORIGBAL = 15000, CURRINTRATE = 0)
  # B's 300 balance is due in full with its 300 payment: a maturity, not a prepayment.
  x <- cbind(base, BAL = c(10000, 300, 9700), PAYAMT = 300)
  out <- calculate_prepay_speed(x, "EFFDATE", list(col_loanid = "ID", interest_basis = NA))
  expect_equal(out$EXIT_SCHED_PRIN, 300)
  expect_equal(out$PREPAYMENT, 0)
  expect_equal(out$CPR, 0)
  # The final payment is capped at the remaining balance.
  x$PAYAMT[2] <- 450
  out <- calculate_prepay_speed(x, "EFFDATE", list(col_loanid = "ID", interest_basis = NA))
  expect_equal(out$EXIT_SCHED_PRIN, 300)
  expect_equal(out$PREPAYMENT, 0)
  # A voluntary payoff prepays its balance net of that month's scheduled principal.
  x <- cbind(base, BAL = c(3e5, 3e5, 299000), PAYAMT = 1000)
  out <- calculate_prepay_speed(x, "EFFDATE", list(col_loanid = "ID", interest_basis = NA))
  expect_equal(out$PREPAYMENT, 299000)
  expect_equal(out$SMM, 299000 / 598000)
})

test_that("payoff scheduled principal uses the exit month's interest accrual", {
  d <- as.Date(c("2024-01-31", "2024-02-29"))
  x <- data.frame(ID = c("A", "B", "A"), EFFDATE = d[c(1, 1, 2)],
    ORIGDATE = as.Date("2020-01-15"), TYPECODE = "P", BAL = c(10000, 10000, 9600),
    ORIGBAL = 15000, PAYAMT = 500, CURRINTRATE = 0.06, BASIS = c(365, 360, 365))
  feb_interest <- 10000 * 0.06 * 29
  out <- calculate_prepay_speed(x, "EFFDATE", list(col_loanid = "ID", interest_basis = 365))
  expect_equal(out$EXIT_SCHED_PRIN, 500 - feb_interest / 365)
  out <- calculate_prepay_speed(x, "EFFDATE", list(col_loanid = "ID", col_interest_basis = "BASIS"))
  expect_equal(out$EXIT_SCHED_PRIN, 500 - feb_interest / 360)
  out <- calculate_prepay_speed(x, "EFFDATE", list(col_loanid = "ID", interest_basis = NA))
  expect_equal(out$EXIT_SCHED_PRIN, 500 - 10000 * 0.06 / 12)
})

test_that("amortization before a loan is first reported is not prepayment", {
  d <- as.Date(c("2024-01-31", "2024-02-29"))
  x <- data.frame(ID = c("A", "A", "L"), EFFDATE = d[c(1, 2, 2)],
    ORIGDATE = as.Date(c("2020-01-15", "2020-01-15", "2024-01-20")), TYPECODE = "P",
    BAL = c(10000, 9700, 19500), ORIGBAL = c(15000, 15000, 20000), PAYAMT = c(300, 300, 500),
    CURRINTRATE = 0)
  # L paid 500 of scheduled principal before its first snapshot.
  out <- calculate_prepay_speed(x, "EFFDATE", list(col_loanid = "ID", interest_basis = NA))
  expect_equal(out$FUNDED_BAL, 19500)
  expect_equal(out$PREPAYMENT, 0)
  # ORIGBAL recorded as a commitment larger than the drawn balance.
  x$ORIGDATE[3] <- as.Date("2024-02-10"); x$ORIGBAL[3] <- 4e5; x$BAL[3] <- 250000
  for (ids in list("ID", NULL)) {
    out <- calculate_prepay_speed(x, "EFFDATE", list(col_loanid = ids, interest_basis = NA))
    expect_equal(out$FUNDED_BAL, 250000)
    expect_lte(out$SMM_RAW, 1)
  }
  out <- calculate_prepay_speed(x, "EFFDATE", list(col_loanid = "ID", interest_basis = NA))
  expect_equal(out$PREPAYMENT, 0)
})

test_that("a continuing loan whose origination date resets is not new funding", {
  # Modification or refinance under the same loan number: ORIGDATE moves to February.
  x <- prepay_history()[1:4, ]; x$TYPECODE <- "POOL"
  x$ORIGDATE[3] <- as.Date("2024-02-10")
  out <- prepay_estimate(x)
  expect_equal(out$FUNDED_BAL, 0)
  expect_equal(out$CPR, 0)
})

test_that("remapped columns may coexist with columns carrying the default names", {
  x <- prepay_history()
  x$FUND_DATE <- x$ORIGDATE; x$FUND_BAL <- x$ORIGBAL
  x$ORIGDATE <- as.Date("1999-01-01"); x$ORIGBAL <- 1
  cfg <- list(col_loanid = "ID", interest_basis = NA,
              col_origdate = "FUND_DATE", col_orig_balance = "FUND_BAL")
  out <- calculate_prepay_speed(x, c("EFFDATE", "TYPECODE"), cfg)
  expect_equal(out, prepay_estimate(prepay_history()))
  expect_error(calculate_prepay_speed(x, c("EFFDATE", "ORIGDATE"), cfg), "share an internal name")
})
