#' Resolve per-loan amortization terms and balloon flags
#'
#' Internal helper. A loan pays a balloon at maturity when its remaining
#' amortization term exceeds its remaining maturity, or when it is flagged by
#' `col_balloon`. A missing flag (or no flag column) uses `balloon_at_maturity`.
#' An explicit FALSE flag cannot override a longer amortization term; such
#' conflicts are reported with a warning.
#'
#' @param data Validated loan data frame.
#' @param cfg Merged configuration list.
#' @return List with `amort_months` (numeric, NA = fully amortizing over the
#'   remaining term) and `balloon` (logical), each of length nrow(data).
#' @keywords internal
#' @noRd
resolve_balloon_terms <- function(data, cfg) {
  n <- nrow(data)
  amort <- if (is.null(cfg$col_amort_months_remaining)) rep(NA_real_, n) else
    as.numeric(data[[cfg$col_amort_months_remaining]])
  flag <- if (is.null(cfg$col_balloon)) rep(NA, n) else
    as.logical(data[[cfg$col_balloon]])

  longer <- !is.na(amort) & amort > data[[cfg$col_term]]
  conflict <- longer & !is.na(flag) & !flag
  if (any(conflict)) {
    ids <- data[[cfg$col_loanid]][conflict]
    warning(sum(conflict), " loan(s) have col_balloon = FALSE but an amortization term ",
            "longer than the remaining maturity; they pay a balloon at maturity. Loan IDs: ",
            paste(utils::head(ids, 5), collapse = ", "),
            if (length(ids) > 5L) ", ..." else "", call. = FALSE)
  }

  flag[is.na(flag)] <- cfg$balloon_at_maturity
  list(amort_months = amort, balloon = flag | longer)
}
