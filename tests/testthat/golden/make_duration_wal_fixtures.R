# tests/testthat/golden/make_duration_wal_fixtures.R
#
# Generator for the calculate_duration() / calculate_wal() golden fixtures,
# frozen in v0.2.5.1 after the month-index timing correction.
#
# RUN ONCE, MANUALLY, then commit the .rds files it writes.
# This script is NOT sourced by the test suite. Golden values must be read
# from disk, never regenerated at test time -- regenerated expectations
# always match, which defeats the purpose.
#
# Re-run only when a change to duration or WAL behaviour is DELIBERATE. In
# that case, treat the re-baseline as its own commit, separate from the code
# change, and say why in NEWS.
#
# Usage, from the package root:
#   devtools::load_all()
#   source("tests/testthat/golden/make_duration_wal_fixtures.R")

library(cfit)

golden_dir <- file.path("tests", "testthat", "golden")
stopifnot(dir.exists(golden_dir))

# ---------------------------------------------------------------------------
# 1. Input portfolio
# ---------------------------------------------------------------------------
# Deliberately heterogeneous in rate and term: the NULL discount_rate path
# discounts each loan at its own rate and PV-weights across loans, so a
# uniform-rate portfolio would not exercise it. Tiers give differing CPRs,
# so the loans also differ in cash flow shape rather than scale alone.

fixture_portfolio <- data.frame(
  LOAN_ID               = c("G001", "G002", "G003", "G004", "G005"),
  balance               = c(25000, 50000, 15000, 30000, 42000),
  current_interest_rate = c(0.0599, 0.0649, 0.0549, 0.0699, 0.0475),
  months_to_maturity    = c(60, 48, 36, 54, 72),
  eff_date              = as.Date("2025-01-01"),
  tier                  = c("A", "B", "A", "C", "B"),
  stringsAsFactors      = FALSE
)

fixture_config <- list(
  col_tier        = "tier",
  cpr_vec         = c("A" = 0.05,  "B" = 0.08,  "C" = 0.12),
  credit_cost_vec = c("A" = 0.008, "B" = 0.015, "C" = 0.025),
  servicing_fee   = 0.0025
)

# ---------------------------------------------------------------------------
# 2. Freeze the INPUT table
# ---------------------------------------------------------------------------
# Generated from the engine once, then frozen. The duration and WAL fixtures
# below are computed from this saved object, not from a fresh engine call, so
# that a future change to calculate_cash_flows() cannot silently move the
# duration goldens. Engine behaviour is pinned separately by the
# golden_cash_flows tests.

input_path <- file.path(golden_dir, "duration_wal_input.rds")

cf <- calculate_cash_flows(fixture_portfolio, fixture_config)

# calculate_cash_flows() returns the loan-level table directly unless
# return_monthly_totals = TRUE, in which case it returns a named list.
duration_wal_input <- if (is.data.frame(cf)) cf else cf$loan_cash_flows

# The month-index guard requires a contiguous index starting at 1.
stopifnot(
  is.data.frame(duration_wal_input),
  nrow(duration_wal_input) > 0,
  identical(
    sort(unique(as.numeric(duration_wal_input$month))),
    as.numeric(seq_len(max(duration_wal_input$month)))
  )
)

saveRDS(duration_wal_input, input_path)
message("wrote ", input_path, " (", nrow(duration_wal_input), " rows)")

# Read it back and build everything below from the round-tripped object, so
# the fixtures match exactly what the tests will load.
gi <- readRDS(input_path)

# ---------------------------------------------------------------------------
# 3. Case definitions
# ---------------------------------------------------------------------------
# Kept in sync with tests/testthat/test-golden-duration-wal.R. Adding a case
# here means adding the matching name there.

duration_cases <- list(
  scalar_total = list(
    discount_rate = 0.05, cash_flow_column = "total_payment",
    include_convexity = TRUE
  ),
  scalar_investor = list(
    discount_rate = 0.05, cash_flow_column = "investor_total",
    include_convexity = TRUE
  ),
  loanrate_total = list(
    discount_rate = NULL, cash_flow_column = "total_payment",
    include_convexity = TRUE
  ),
  loanrate_investor = list(
    discount_rate = NULL, cash_flow_column = "investor_total",
    include_convexity = TRUE
  ),
  scalar_no_convexity = list(
    discount_rate = 0.05, cash_flow_column = "total_payment",
    include_convexity = FALSE
  )
)

wal_cases <- list(
  total     = list(principal_column = "total_principal"),
  investor  = list(principal_column = "investor_principal")
)

# ---------------------------------------------------------------------------
# 4. Write fixtures
# ---------------------------------------------------------------------------

for (nm in names(duration_cases)) {
  res  <- do.call(calculate_duration, c(list(gi), duration_cases[[nm]]))
  path <- file.path(golden_dir, paste0("duration_", nm, ".rds"))
  saveRDS(res, path)
  message("wrote ", path)
  print(res)
}

for (nm in names(wal_cases)) {
  res  <- do.call(calculate_wal, c(list(gi), wal_cases[[nm]]))
  path <- file.path(golden_dir, paste0("wal_", nm, ".rds"))
  saveRDS(res, path)
  message("wrote ", path)
  print(res)
}

# ---------------------------------------------------------------------------
# 5. Pre-commit sanity check
# ---------------------------------------------------------------------------
# Do not commit until these read as plausible. A golden fixture that froze a
# wrong number is worse than no fixture: it converts an error into a
# permanent, tested-looking expectation.
#
#   - portfolio_pv below the 162,000 starting balance (discounted, net of
#     credit loss and fees on the investor cases)
#   - macaulay_duration between 1 and 3 years for this term mix
#   - modified_duration slightly below macaulay_duration
#   - portfolio_wal above macaulay_duration (WAL is undiscounted, and
#     principal is repaid later on average than the interest-inclusive
#     cash flow stream)
#
# Spot-check one value by hand against the corrected convention before
# committing -- the fixtures are only as trustworthy as that first check.

message("\nSanity check the printed values above, then commit the .rds files.")
