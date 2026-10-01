# Shared validation for monthly-index portfolio analytics. No rows are dropped.
validate_analytics_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || !is.null(dim(value)) || is.na(value)) {
    stop(name, " must be TRUE or FALSE")
  }
}

validate_analytics_data <- function(data, amount_column, required_cols, validate_month_index,
                                    check_rate = FALSE) {
  if (!is.data.frame(data)) stop("loan_cash_flows must be a data frame")
  if (nrow(data) == 0L) stop("loan_cash_flows must be non-empty")
  if (anyNA(names(data)) || any(!nzchar(trimws(names(data)))) || anyDuplicated(names(data))) {
    stop("loan_cash_flows column names must be non-empty and unique")
  }
  missing <- setdiff(required_cols, names(data))
  if (length(missing)) {
    stop("loan_cash_flows is missing required columns: ", paste(missing, collapse = ", "))
  }
  validate_analytics_flag(validate_month_index, "validate_month_index")
  for (col in c("LOAN_ID", amount_column)) {
    if (all(is.na(data[[col]]))) stop(col, " column contains only NA values")
  }
  ids <- data$LOAN_ID
  if (!(is.character(ids) || is.factor(ids) || is.numeric(ids)) ||
      !is.null(dim(ids)) || anyNA(ids) || any(!nzchar(trimws(as.character(ids)))) ||
      (is.numeric(ids) && any(!is.finite(ids)))) {
    stop("LOAN_ID must contain non-missing, non-empty character or finite numeric identifiers")
  }
  for (col in c("month", amount_column, if (check_rate) "rate")) {
    x <- data[[col]]
    if (!is.numeric(x) || !is.null(dim(x)) || any(!is.finite(x))) {
      stop(col, " must contain finite numeric values without missing data")
    }
    if (col != "month" && any(x < 0)) stop(col, " must be non-negative")
  }
  if (any(data$month < 1)) stop("month column contains values less than 1. Month must be >= 1")
  if (any(data$month != floor(data$month))) {
    stop("month column contains non-integer values. Month must be an integer >= 1")
  }
  for (col in c("eff_date", "date")) {
    converted <- tryCatch(as.Date(data[[col]]), error = function(e) {
      stop(col, " could not be converted to Date format")
    })
    if (length(converted) != nrow(data) || any(!is.finite(as.numeric(converted)))) {
      stop(col, " must contain valid, non-missing dates")
    }
    data[[col]] <- converted
  }
  if (length(unique(data$eff_date)) != 1L) {
    stop("Portfolio analytics require a common eff_date. Analyze different snapshots separately.")
  }
  if (anyDuplicated(data[c("LOAN_ID", "month")])) {
    stop("Duplicate loan-month records: each LOAN_ID and month must be unique, ",
         "even when validate_month_index = FALSE")
  }
  if (validate_month_index) {
    for (rows in split(seq_len(nrow(data)), match(ids, unique(ids)))) {
      observed <- sort(data$month[rows])
      id <- as.character(ids[rows[1]])
      if (observed[1] != 1) {
        stop("Loan '", id, "': month index starts at ", observed[1], ", expected 1. ",
             "Remove any obsolete month + 1 workaround; use validate_month_index = FALSE ",
             "only for a deliberately offset or filtered projection.")
      }
      # Differences avoid allocating a potentially enormous seq_len(max(month)).
      if (any(diff(observed) != 1)) {
        stop("Loan '", id, "': month index is not contiguous from 1. ",
             "Use validate_month_index = FALSE only for a deliberately filtered projection.")
      }
    }
  }
  data
}
