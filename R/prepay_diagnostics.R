# Require one portfolio reporting date per calendar month, with no skipped months.
validate_prepay_periods <- function(dates) {
  dates <- sort(unique(dates))
  periods <- lubridate::year(dates) * 12 + lubridate::month(dates)
  if (anyDuplicated(periods)) {
    stop("Monthly snapshots require exactly one reporting date per calendar month.")
  }
  if (any(diff(periods) != 1)) {
    stop("Missing reporting month(s): snapshots must cover consecutive calendar months. ",
         "Do not annualize a multi-month interval as one month.")
  }
  dates
}

# Align cohorts to the immediately preceding portfolio snapshot, never the last
# available observation of that cohort. A disappearance retains unknown values.
# With loan IDs, membership changes are classified rather than all treated as
# unresolved: loans leaving the portfolio are payoffs (exit_treatment = "payoff")
# unless listed in non_prepay_ids; cohort transfers and loans that reappear in a
# later snapshot stay unresolved; loans originated in the prior month that first
# appear now are counted as funding.
prepay_snapshot_pairs <- function(df, groups, dates, use_loan_id,
                                  exit_treatment = "payoff", non_prepay_ids = NULL) {
  snapshot <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
    dplyr::summarise(END_BAL = sum(BAL), SCHED_PRIN_TOTAL = sum(SCHEDPRIN, na.rm = TRUE),
                     .groups = "drop")
  prior <- snapshot[snapshot$EFFDATE != utils::tail(dates, 1), groups, drop = FALSE]
  prior$BEGIN_BAL <- snapshot$END_BAL[snapshot$EFFDATE != utils::tail(dates, 1)]
  prior$EFFDATE <- dates[match(prior$EFFDATE, dates) + 1L]
  current <- snapshot[snapshot$EFFDATE != dates[1], ]
  out <- dplyr::full_join(current, prior, by = groups)
  out$COHORT_DISAPPEARED <- is.na(out$END_BAL)
  out$UNRESOLVED_EXITS <- rep(NA_integer_, nrow(out))
  out$PAYOFF_EXITS <- rep(NA_integer_, nrow(out))
  out$EXCLUDED_EXIT_BAL <- rep(0, nrow(out))
  out$UNRESOLVED_ENTRIES <- rep(NA_integer_, nrow(out))
  out$LATE_FUNDED_BAL <- rep(0, nrow(out))
  if (use_loan_id) {
    keys <- unique(c(groups, "LOANID"))
    previous <- df[df$EFFDATE != utils::tail(dates, 1), keys, drop = FALSE]
    previous$.prior_bal <- df$BAL[df$EFFDATE != utils::tail(dates, 1)]
    previous$EFFDATE <- dates[match(previous$EFFDATE, dates) + 1L]
    current_loans <- df[df$EFFDATE != dates[1], ]
    exits <- dplyr::anti_join(previous, current_loans, by = keys)
    entries <- dplyr::anti_join(current_loans, previous, by = keys)

    # Portfolio-wide membership separates cohort transfers from true exits/entries.
    loan_key <- function(x) paste(as.numeric(x$EFFDATE), as.character(x$LOANID), sep = "\r")
    in_current <- loan_key(exits) %in% loan_key(current_loans)
    in_previous <- loan_key(entries) %in% loan_key(previous)
    last_seen <- tapply(as.numeric(df$EFFDATE), as.character(df$LOANID), max)
    returns_later <- last_seen[as.character(exits$LOANID)] > as.numeric(exits$EFFDATE)
    excluded <- !in_current & as.character(exits$LOANID) %in% as.character(non_prepay_ids)
    exits$.type <- ifelse(in_current, "unresolved",
                   ifelse(excluded, "excluded",
                   ifelse(returns_later | exit_treatment != "payoff", "unresolved", "payoff")))

    orig_month <- lubridate::floor_date(entries$ORIGDATE, "month")
    eff_month <- lubridate::floor_date(entries$EFFDATE, "month")
    # A same-month origination is handled by the existing funding estimate.
    entries$.type <- ifelse(in_previous, "unresolved",
                     ifelse(orig_month == eff_month, "same_month",
                     ifelse(orig_month == eff_month %m-% months(1), "late_funded", "unresolved")))

    exits$.unresolved <- exits$.type == "unresolved"
    exits$.payoff <- exits$.type == "payoff"
    exits$.excluded_bal <- ifelse(exits$.type == "excluded", exits$.prior_bal, 0)
    entries$.unresolved <- entries$.type == "unresolved"
    entries$.late_bal <- ifelse(entries$.type == "late_funded", entries$ORIGBAL, 0)
    cohort_total <- function(records, column) {
      totals <- records %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
        dplyr::summarise(.prepay_total = sum(.data[[column]]), .groups = "drop")
      joined <- dplyr::left_join(out[, groups, drop = FALSE], totals, by = groups)
      dplyr::coalesce(as.numeric(joined$.prepay_total), 0)
    }
    out$UNRESOLVED_EXITS <- as.integer(cohort_total(exits, ".unresolved"))
    out$PAYOFF_EXITS <- as.integer(cohort_total(exits, ".payoff"))
    out$EXCLUDED_EXIT_BAL <- cohort_total(exits, ".excluded_bal")
    out$UNRESOLVED_ENTRIES <- as.integer(cohort_total(entries, ".unresolved"))
    out$LATE_FUNDED_BAL <- cohort_total(entries, ".late_bal")

    # Every loan of a vanished cohort left as a payoff or listed exit: it ended at zero.
    resolved <- out$COHORT_DISAPPEARED & out$UNRESOLVED_EXITS == 0L
    out$END_BAL[resolved] <- 0
    out$SCHED_PRIN_TOTAL[resolved] <- 0
  }
  out
}

# SMM_RAW is the unbounded estimate; SMM is exactly the value used for CPR.
# Structural uncertainty makes principal attribution and speeds undefined.
add_prepay_diagnostics <- function(out, allow_negative) {
  n <- nrow(out)
  out$DIAGNOSTIC <- rep("", n)
  flag <- function(condition, label) {
    i <- which(condition)
    out$DIAGNOSTIC[i] <<- ifelse(nzchar(out$DIAGNOSTIC[i]),
                                paste(out$DIAGNOSTIC[i], label, sep = ";"), label)
  }
  flag(out$COHORT_DISAPPEARED & is.na(out$END_BAL), "cohort_disappeared")
  flag(is.na(out$BEGIN_BAL), "no_prior_cohort")
  flag(out$UNRESOLVED_EXITS > 0, "unresolved_exits")
  flag(out$UNRESOLVED_ENTRIES > 0, "unresolved_entries")
  unresolved <- nzchar(out$DIAGNOSTIC)
  out$ACTUAL_PRIN[unresolved] <- NA_real_
  out$PREPAYMENT[unresolved] <- NA_real_
  out$AVAILABLE_TO_PREPAY <- out$BEGIN_BAL - out$SCHED_PRIN_TOTAL - out$EXCLUDED_EXIT_BAL
  bad_denominator <- !is.finite(out$AVAILABLE_TO_PREPAY) | out$AVAILABLE_TO_PREPAY <= 0
  flag(bad_denominator, "invalid_denominator")
  out$SMM_RAW <- rep(NA_real_, n)
  usable <- !unresolved & !bad_denominator
  out$SMM_RAW[usable] <- out$PREPAYMENT[usable] / out$AVAILABLE_TO_PREPAY[usable]
  flag(usable & !is.finite(out$SMM_RAW), "nonfinite_estimate")
  out$SMM_RAW[!is.finite(out$SMM_RAW)] <- NA_real_
  flag(out$SMM_RAW < 0, "negative_prepayment")
  flag(out$SMM_RAW > 1 | out$SMM_RAW < -1, "out_of_range_smm")
  out$SMM <- pmin(1, pmax(if (allow_negative) -1 else 0, out$SMM_RAW))
  out$SMM_ADJUSTED <- out$SMM != out$SMM_RAW
  out$CPR <- 1 - (1 - out$SMM)^12
  out$DIAGNOSTIC[!nzchar(out$DIAGNOSTIC)] <- "ok"
  out
}
