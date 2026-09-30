# Attach reporting metadata through an explicit many-to-one lookup. Matching
# preserves projection order and cannot multiply rows as a permissive join can.
attach_cash_flow_groups <- function(cash_flows, data, cfg) {
  ids <- data[[cfg$col_loanid]]
  if (anyNA(ids) || anyDuplicated(ids)) {
    stop("Grouping metadata must contain exactly one record per loan ID.")
  }
  index <- match(cash_flows$LOAN_ID, ids)
  if (anyNA(index)) stop("Grouping metadata is missing a projected loan ID.")

  groups <- cfg$monthly_totals_group_vars
  generated <- c("LOAN_ID", "eff_date", "rate", "tier", "month", "date", "original_tier")
  measures <- setdiff(names(cash_flows), generated)
  invalid_measures <- intersect(groups, measures)
  if (length(invalid_measures)) {
    stop("Cannot group by cash-flow measure(s): ", paste(invalid_measures, collapse = ", "),
         ". Rename input classifications that use these reserved names.")
  }
  missing <- setdiff(groups, c(names(data), generated))
  if (length(missing)) {
    stop("Missing grouping columns: ", paste(missing, collapse = ", "))
  }
  # A mapped source with the same name has an explicit meaning. Payment date
  # and projection month are always generated, never taken from input metadata.
  mapped <- list(LOAN_ID = cfg$col_loanid, eff_date = cfg$col_start_date,
                 rate = cfg$col_rate, tier = cfg$col_tier, original_tier = cfg$col_tier)
  for (nm in intersect(groups, intersect(generated, names(data)))) {
    if (!identical(mapped[[nm]], nm)) {
      stop("Ambiguous grouping column '", nm, "': this name is reserved for generated output. ",
           "Rename the input classification before grouping.")
    }
  }

  original <- if (is.null(cfg$col_tier)) rep(NA_character_, nrow(data)) else
    as.character(data[[cfg$col_tier]])
  cash_flows$original_tier <- original[index]
  for (nm in setdiff(groups, generated)) {
    if (!is.atomic(data[[nm]]) || !is.null(dim(data[[nm]]))) {
      stop("Grouping column '", nm, "' must be an atomic vector, not a list or matrix.")
    }
    cash_flows[[nm]] <- data[[nm]][index]
  }
  cash_flows
}
