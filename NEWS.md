# cfit 0.2.5

## Significant Changes

* **Survivor re-amortization is now the default prepayment convention**
  (`reamortize_survivors = TRUE`). SMM prepayments are modeled as full
  payoffs, with the surviving balance re-amortizing over the remaining
  term — matching market/Bloomberg pool conventions. The prior behavior
  held the original dollar payment fixed (curtailment treatment),
  truncating loan life and understating premium-pool yields by ~25 bps
  at typical auto speeds. Legacy behavior available via
  `reamortize_survivors = FALSE`.
* **First payment date is now one month after the as-of date.** The
  as-of date is the t = 0 valuation anchor; payment dates run t = 1..term.
  Downstream scripts that manually re-dated output (or anchored yields
  to the first cash flow date) must remove those workarounds or they
  will double-shift.
* **`interest_on_starting_balance` now defaults to `TRUE`** (full-month
  interest on the starting balance, standard monthly-pay servicing) and
  **`credit_loss_reduces_interest` now defaults to `FALSE`** (losses
  flow through principal only). Configs setting these explicitly are
  unaffected.

## Other changes

* Payment dates use lubridate month arithmetic: month-end as-of dates
  clamp (Jan 31 → Feb 28) instead of overflowing (→ Mar 3). After a
  month-end anchor, all payments fall on month-ends.
* `scheduled_payment` output column is time-varying under the new
  convention (declines with the survival factor).
* Unnamed config elements (typically `<-` used instead of `=` inside
  `list()`) now raise an error instead of being silently ignored.
* Incompatible combination `reamortize_survivors = TRUE` with
  `interest_on_starting_balance = FALSE` warns.

## Validation

* Engine tied to an independent external benchmark (BEY 5.573%, WAL
  2.34 on the reference loan) and replicated a live Bloomberg quote to
  within tape-composition tolerance (5.455% vs 5.470% quoted).
* v0.2.4 cash flow amounts reproduced exactly under legacy flags across
  all five golden cases; pinned as a permanent backward-compatibility
  test.

# cfit 0.2.4

## New features

* `calculate_cash_flows()` gains a `prepay_model` option controlling how the
  conditional prepayment rate (CPR) is derived:
  * `"tier_static"` (default) — unchanged behaviour; applies tier-keyed CPR
    from `cpr_vec`.
  * `"linear_incentive"` — derives a per-loan CPR from the borrower's rate
    incentive: `CPR = clamp(base_cpr + beta * (coupon - current_market_rate),
    cpr_min, cpr_max)`. Parameters `base_cpr_vec`, `beta_vec`, `cpr_min_vec`,
    and `cpr_max_vec` are tier-keyed; `current_market_rate` is a scalar and
    `coupon` is the gross note rate. This makes prepayment responsive to the
    rate environment.

## Improvements

* Cash flow engine performance: per-loan projections are accumulated in
  preallocated atomic vectors and assembled in a single `tibble()` call,
  rather than constructing one data frame per projected month. The engine's
  internal performance test dropped from roughly 79s to 7s, with identical
  output.

## Internal

* CPR resolution is factored into an internal `resolve_prepay_cpr()` helper, so
  the engine treats CPR as a per-loan input independent of its derivation.
* Added golden-master regression tests freezing v0.2.3 output for the
  `tier_static` path and the new `linear_incentive` output, guarding against
  behavioural drift across the refactor.

## Notes

* No breaking changes. Calls default to `prepay_model = "tier_static"` and
  produce identical results to v0.2.3.

# cfit 0.2.3

## New Features

* Added `calculate_duration()` - Calculate Macaulay duration, modified duration, and optional analytical convexity for loan portfolios
* Added `calculate_wal()` - Calculate weighted average life (WAL) for loan portfolios based on principal cash flows

## Documentation

* Comprehensive documentation for duration and WAL calculations
* Examples demonstrating usage with different cash flow and principal columns

# cfit 0.2.2

## Bug Fixes

* **Critical**: Fixed credit loss calculation to cap at 100% of balance - prevents mathematical impossibility where credit losses could exceed loan balance
* Fixed prepayment calculation to properly cap at available balance after credit losses are applied
* Fixed date handling to use base R `seq.Date()` for more reliable month-end date arithmetic

## Improvements

* Simplified grouping column handling - now uses efficient lookup table join instead of complex parameter passing
* Removed unnecessary date conversion messages for cleaner output
* Monthly totals now automatically sorted by date and grouping variables for predictable output
* Minor documentation fixes and clarifications

# cfit 0.2.1

## Bug Fixes and Improvements

**Critical Fixes:**
* Fixed date conversion persistence - character dates are now properly converted and maintained throughout processing
* Fixed prepayment calculation to cap at available balance after credit losses
* Fixed `months()` namespace issue for better package reliability
* Renamed `monthly_reporting_fee` to `annual_reporting_fee` for consistency with other annual rate parameters

**New Features:**
* Added `credit_loss_reduces_interest` parameter (default: TRUE) - configurable accounting treatment for credit losses. By default, credit losses are applied against investor cash flows before distribution to investors. When set to FALSE, credit losses reduce principal balances only and do not directly reduce interest cash flows.
* Added `monthly_totals_group_vars` parameter - allows grouping monthly totals by additional variables (e.g., tier, product type) beyond date
* Split fee reporting for transparency: `servicing_fee_amt`, `reporting_fee_amt`, and `total_fees` columns

**Enhanced Validation:**
* Enforced "default" tier requirement when tier column is not specified
* Added explicit `dplyr::` namespace calls for better compatibility
* Improved error messages and validation checks
---

# cfit 0.2.0

## New Functions

* `calculate_cash_flows()` – Generate monthly cash flow projections for loan portfolios under customizable prepayment, credit loss, and fee assumptions.

## Features

* Flexible column mapping to support institution-specific data schemas
* Tier-based differentiation for CPR and credit cost assumptions
* Support for both direct credit cost inputs and PD/LGD-based methodology
* Optional aggregation of monthly cash flows for portfolio-level analysis
* Investor share allocation for participation and ownership scenarios
* Comprehensive input validation with informative warnings and errors

## Performance

* Improved date handling for large portfolios
* Progress messages for portfolios with 1,000+ loans
* Robust handling of edge cases including zero-interest loans and de minimis balances

## Output Options

* Detailed loan-level monthly cash flows
* Aggregated monthly totals across the entire portfolio
* `investor_total` column representing total cash flow due to the owner, suitable for external yield calculations

## Improvements

* Enhanced documentation and examples
---

# cfit 0.1.0

First stable release of cfit! 🎉

## New Functions

* `calculate_prepay_speed()` - Calculate Single Monthly Mortality (SMM) and Conditional Prepayment Rate (CPR) for loan portfolios

**Accuracy**
* Industry-standard SMM calculation using pool available to prepay
* Improved scheduled principal accuracy when loan ID provided
* Automatic interest rate format detection and conversion

**Validation**
* Optional loan ID validation prevents duplicate records
* Smart rate format handling (7.29% or 0.0729)
* EFFDATE ordering validation

**Flexibility**
* Custom column mapping for any FI's data structure
* Multiple interest calculation methods (360/365-day basis)
* Configurable prepayment handling and filtering

**Testing**
* 39 automated tests covering all functionality and edge cases

## Documentation

* Complete function documentation with examples
* README with installation and usage examples
* Comprehensive parameter descriptions
