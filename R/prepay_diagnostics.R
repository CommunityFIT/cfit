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
prepay_snapshot_pairs <- function(df, groups, dates, use_loan_id) {
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
  out$UNRESOLVED_ENTRIES <- rep(NA_integer_, nrow(out))
  if (use_loan_id) {
    keys <- unique(c(groups, "LOANID"))
    previous <- df[df$EFFDATE != utils::tail(dates, 1), keys, drop = FALSE]
    previous$EFFDATE <- dates[match(previous$EFFDATE, dates) + 1L]
    current_loans <- df[df$EFFDATE != dates[1], ]
    exits <- dplyr::anti_join(previous, current_loans, by = keys)
    entries <- dplyr::anti_join(current_loans, previous, by = keys)
    # A same-month origination is handled by the existing funding estimate.
    entries <- entries[lubridate::floor_date(entries$ORIGDATE, "month") !=
                         lubridate::floor_date(entries$EFFDATE, "month"), ]
    for (kind in c("exits", "entries")) {
      records <- if (kind == "exits") exits else entries
      counts <- records %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
        dplyr::summarise(.prepay_count = dplyr::n(), .groups = "drop")
      joined <- dplyr::left_join(out[, groups, drop = FALSE], counts, by = groups)
      out[[if (kind == "exits") "UNRESOLVED_EXITS" else "UNRESOLVED_ENTRIES"]] <-
        dplyr::coalesce(joined$.prepay_count, 0L)
    }
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
  flag(out$COHORT_DISAPPEARED, "cohort_disappeared")
  flag(is.na(out$BEGIN_BAL), "no_prior_cohort")
  flag(out$UNRESOLVED_EXITS > 0, "unresolved_exits")
  flag(out$UNRESOLVED_ENTRIES > 0, "unresolved_entries")
  unresolved <- nzchar(out$DIAGNOSTIC)
  out$ACTUAL_PRIN[unresolved] <- NA_real_
  out$PREPAYMENT[unresolved] <- NA_real_
  out$AVAILABLE_TO_PREPAY <- out$BEGIN_BAL - out$SCHED_PRIN_TOTAL
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
