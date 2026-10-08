# cfit 0.2.8

## Historical prepayment: scheduled repayments are no longer prepayments

Estimated speeds may be lower than v0.2.7. In a simulated amortizing portfolio
with maturities and late-reported originations, v0.2.7 estimated a 19.10% CPR
against a true 17.42%; v0.2.8 matches it.

* With loan IDs, a payoff exit's final-month scheduled principal (payment less
  interest on its prior balance, capped at that balance) is now included in
  `SCHED_PRIN_TOTAL`. Previously only loans in the current snapshot contributed,
  so a loan making its final scheduled payment counted entirely as prepayment.
  New output column `EXIT_SCHED_PRIN`.
* `FUNDED_BAL` is now the balance of new loans when first reported, not
  `ORIGBAL`. Amortization before a loan's first report, or an `ORIGBAL` recorded
  as a commitment above the drawn balance, no longer counts as prepayment. This
  applies with and without loan IDs. `SMM_RAW` can no longer exceed 1.
* With loan IDs, new loans are identified by ID rather than origination date. A
  continuing loan whose origination date is reset (e.g. a modification) is no
  longer counted as new funding.
* New `prepay_config$entry_treatment`. The default, `"funding"`, counts every
  loan not seen in an earlier snapshot as new funding, even when originated
  before the prior month (counted in new column `LATE_FUNDED_ENTRIES`). Use
  `"unresolved"` for the v0.2.7 behavior. Cohort transfers and loans returning
  after a gap remain unresolved in either mode.
* `col_origdate`, `col_orig_balance` and other mappings may point at a column
  other than the default name even when a column with the default name is also
  present; the unmapped column is ignored. Grouping by such a column errors.

# cfit 0.2.7

## Historical prepayment: exits are payoffs by default

v0.2.6 marked every period in which a loan left the portfolio as unresolved
when `col_loanid` was supplied. Payoffs are the runoff SMM measures, so real
portfolios returned `NA` SMM/CPR for every period with a summary warning.

* New `prepay_config$exit_treatment`. The default, `"payoff"`, counts a loan
  that leaves the portfolio and does not reappear later as a payoff. Use
  `"unresolved"` for the strict v0.2.6 behavior.
* New `prepay_config$non_prepay_exit_ids`: loan IDs whose exits are not
  prepayments (charge-offs, sales, transfers out of the data). Their prior
  balance is removed from `ACTUAL_PRIN` and from `AVAILABLE_TO_PREPAY`. This
  requires `col_loanid`.
* New output columns `PAYOFF_EXITS` and `EXCLUDED_EXIT_BAL`. `UNRESOLVED_EXITS`
  now counts only exits that could not be treated as payoffs.
* Cohort transfers, and loans that leave and later reappear, remain
  unresolved in either mode.
* With loan IDs, a loan originated in the prior month that first appears in the
  current snapshot (for example, funded after the month-end extract) is counted
  in `FUNDED_BAL` instead of being flagged as an unresolved entry.
* With loan IDs, a cohort whose loans all left as payoffs or listed exits ends
  at a zero balance instead of being flagged `cohort_disappeared`.
* Without loan IDs, results are unchanged from v0.2.6.
* The undefined-estimate warning now reads "Inspect DIAGNOSTIC for the cause."

# cfit 0.2.6

This release completes the reliable-core updates. See the README migration
notes before upgrading: stricter validation can reject previously accepted
inputs, and unresolved historical prepayment estimates now return `NA`.

## Release verification

* Historical cash-flow, duration, and WAL fixtures are required: missing fixture
  files now fail regression tests instead of silently skipping them. Existing
  fixtures remain unchanged.
* Declared R >= 3.5.0, the existing minimum required to read the serialized
  regression fixtures; package builds previously added this automatically.
* Added consolidated migration guidance covering validation, grouping, corrected
  principal accounting, portfolio sensitivities, and prepayment diagnostics.
* Balloon/interest-only schedules and a configurable month-end payment-date
  convention remain future work; this release does not add them.

## Reliable core: historical prepayment safeguards

* `calculate_prepay_speed()` requires consecutive calendar months with exactly
  one reporting date per month. It no longer lags across missing portfolio
  months or reuses stale observations for reappearing cohorts.
* Disappearing cohorts remain in the output with unknown ending balances.
  New/reappearing cohorts have unknown beginning balances. With loan IDs,
  cohort exits and entrants other than same-month originations are flagged;
  this includes transfers. Principal attribution and speeds are `NA` for these
  unresolved rows. No exit is automatically assumed to be voluntary prepayment.
* Added `AVAILABLE_TO_PREPAY`, `SMM_RAW`, `SMM_ADJUSTED`,
  `COHORT_DISAPPEARED`, `UNRESOLVED_EXITS`, `UNRESOLVED_ENTRIES`, and
  `DIAGNOSTIC`. Counts are `NA` when no loan ID is supplied. `SMM` is now exactly
  the bounded rate used for CPR, with raw values retained separately. Negative
  estimates carry a flag even when `allow_negative_prepay = TRUE`.
* Non-positive denominators or non-finite estimates yield `NA` speeds instead
  of `Inf`/`NaN`. Undefined estimates produce a summary warning independently
  of `verbose`. Diagnostic rows bypass `min_begin_balance` filtering.
* Required numeric snapshot fields must be finite, non-missing and non-negative;
  missing values are no longer silently summed as zero. Loan IDs, when supplied,
  must be non-missing and non-empty. Grouping names cannot collide with output
  metrics or diagnostics.
* README examples now supply loan IDs, demonstrate unresolved exits and raw
  versus bounded SMM, and isolate the intentional duplicate-ID example.
  Normal complete monthly estimates retain their calculations. Snapshot-based
  funding remains approximate; transaction-level attribution is deferred.

## Reliable core: portfolio analytics

* `calculate_duration()` and `calculate_wal()` now validate month sequences
  separately for each loan. Another loan can no longer conceal a missing month.
  Duplicate loan-month records always error, including with
  `validate_month_index = FALSE`. That option only permits intentional gaps or
  offset windows; positive integer indices are still required.
* Invalid required data now raises an error rather than silently dropping rows.
  Inputs must be non-empty, identifiers and dates valid, selected cash-flow
  amounts finite and non-negative, and all rows must share one `eff_date`.
  Duration also requires finite non-negative rates. Zero-weight loans retain
  their existing warning/exclusion behavior.
* Modified duration and analytical convexity now apply each cash flow's own
  discount-rate adjustment before portfolio aggregation. With heterogeneous
  rates and `discount_rate = NULL`, these sensitivities may change. PV,
  Macaulay duration, WAL, and common-rate sensitivities are unchanged for valid
  inputs. This is fixed-cash-flow sensitivity, not effective duration.
* Independent finite-difference and closed-form tests verify the correction.
  Historical fixtures have not been regenerated: unchanged metrics remain
  pinned, and heterogeneous-rate sensitivities are verified independently.
  README example metrics are updated. Payment-date conventions are unchanged.

## Reliable core: safe grouping and aggregation

* Loan cash flows now include `original_tier`, the configured input classification
  as character, retaining missing values. It is `NA` when `col_tier` is not set.
  `tier` continues to identify the resolved assumption tier. Group by either or
  both without changing projected amounts.
* Missing grouping columns now error in either return mode. Generated grouping
  fields are `LOAN_ID`, `eff_date`, `rate`, `tier`, `original_tier`, `month`, and
  `date`; monetary output fields cannot be grouping keys. Ambiguous input names
  that collide with generated fields must be renamed unless they are the
  corresponding explicitly mapped source. `date` is always payment date.
* Metadata attachment now uses a checked many-to-one lookup that preserves row
  order and cannot multiply cash flows. Missing classification values are kept
  in grouped totals. Requesting `date` explicitly does not duplicate the key.
* Added aggregation, row-order, and principal-reconciliation tests. Historical
  fixtures remain unchanged; golden comparisons exclude only the newly added
  `original_tier` metadata column. Existing financial calculations are unchanged.

## Reliable core: principal accounting and payment diagnostics

* Corrected `reamortize_survivors = FALSE`: total principal now includes both
  scheduled principal and prepayments. The prior cap incorrectly limited their
  sum to a balance already reduced by prepayment. This can change payoff timing,
  collections, interest, yield, duration, and WAL for fixed-payment projections.
* Small-balance cleanup is now included in `scheduled_principal` as well as
  `total_principal`, preserving component reconciliation in both conventions
  and monthly totals. `scheduled_payment` remains the payment before final payoff
  adjustments. Positive sub-cent balances are no longer silently dropped.
* Supplied payments below the period's gross accrued interest now error with
  loan ID and month. The check respects survivor scaling and the selected
  interest accrual convention, with a floating-point roundoff tolerance.
* Loans retaining at least `max(0.01, de_minimis_balance)` at maturity trigger
  one portfolio warning listing affected IDs and the total residual. Residuals
  remain outstanding; no balloon is added. This applies to both output modes.
* Existing survivor-convention golden fixtures remain unchanged. The historical
  fixed-payment fixture is retained, with explicit reconciled payoff expectations
  replacing exact reproduction of its defective terminal rows. Added independent
  payoff examples and principal-conservation tests across both conventions.

## Reliable core: cash-flow input validation

* `calculate_cash_flows()` now rejects malformed configuration keys and values,
  missing mapped columns, duplicate/missing loan IDs, invalid required data,
  and non-integer terms before projection. Invalid records previously warned
  and were skipped; callers must now correct or explicitly filter those records
  before calling the function. Empty input also raises an error.
* Automatic IDs remain available when the implicit `LOAN_ID` column is absent
  or `col_loanid = NULL`. An explicitly named ID column must exist.
* Optional payment and original-balance columns retain their per-loan `NA`
  fallback. Non-positive or non-finite supplied values are now rejected.
* PD and LGD must be supplied together and individually lie in [0, 1]. Tier
  names must match as sets; multiplication aligns by name, not position.
  Effective credit costs must cover every tier in the active prepayment model.
* Tier vectors require unique, non-empty names and finite numeric values.
  The named `default` tier is used regardless of vector order. Unknown/missing
  loan tiers use that default for both CPR and credit costs with a warning, or
  error if it is absent. Portfolios containing only known tiers do not need a
  default. The output `tier` records the resolved assumption tier.
* Existing valid golden cash-flow fixtures are unchanged. Results can change
  for inputs that previously selected a fallback by vector position. Define a
  named `default` explicitly when fallback behavior is intended.
* Added targeted validation/regression tests and GitHub Actions package checks
  on Linux, macOS, and Windows. Projection arithmetic and date conventions are
  unchanged in this validation update.

# cfit 0.2.5.1

## Bug fixes

* **`calculate_duration()` and `calculate_wal()` now interpret the `month`
  column correctly.** Both treated `month = 1` as t = 0 and computed
  `t = (month - 1) / 12`, discounting every projected cash flow one period
  too few. The `month` column is the projection index: `month = 1` is the
  first projected cash flow, falling one month after `eff_date`. Timing is
  now `t = month / 12`.

  This is a correction, not a modeling choice. The v0.2.5 changes moved
  numbers deliberately; these numbers were wrong.

  The correction is a uniform one-period shift, so its magnitude is exact
  and independently verifiable:

  - `portfolio_pv` falls by a factor of `1 / (1 + y/12)` — approximately
    42 bps at a 5.14% annual yield
  - `macaulay_duration` and `modified_duration` each rise by exactly
    1/12 year (0.0833)
  - `portfolio_wal` rises by exactly 1/12 year
  - `analytical_convexity` rises by `2 * D / ((1 + y/12)^2 * 144)`, where
    `D` is the corrected Macaulay duration in months

  Users can confirm their own figures moved by exactly these amounts.
  Under a scalar `discount_rate` the relationships are exact; under
  `discount_rate = NULL` they are approximate, because loan-level PV
  weights shift when each loan discounts at its own rate.

* **Scope.** Introduced in v0.2.5, which moved the first payment date to
  one month after the as-of date without updating the timing convention in
  `calculate_duration()` and `calculate_wal()`. Output from v0.2.4 and
  earlier is unaffected: those functions were correct for the engine they
  were written against. Only figures produced with v0.2.5 need restating.

* **Corrected documentation.** The `@details` sections of both functions
  stated `month = 1` corresponds to time 0 — a convention the engine has
  not emitted since v0.2.5, and which contradicted the v0.2.5 release
  notes. Error messages carrying the same claim have been corrected.

## New

* **`validate_month_index` argument** (default `TRUE`) on both functions.
  Errors when the `month` index does not begin at 1 or is not contiguous.
  Analysis code written against v0.2.5 may carry a
  `mutate(month = month + 1)` workaround; left in place it now
  double-shifts, understating PV by roughly 40 bps — a plausible-looking
  error. The guard makes that failure loud, and its message names the
  workaround as the likely cause. Set `FALSE` to analyze a deliberately
  offset or filtered projection window.

## Other changes

* README example outputs regenerated. These were last produced under
  v0.2.4 and so reflect both the v0.2.5 cash flow conventions and this
  timing correction. WAL moves from 1.78 to 2.020745: approximately 0.157
  from the v0.2.5 conventions and exactly 1/12 from the timing fix.
  Duration and convexity move for the same two reasons.

## Validation

* Timing convention pinned by absolute unit tests — hand-computed
  expectations on small cash flow tables, independent of the
  implementation — rather than by shift comparisons, which are
  equivariant under the bug and cannot detect it.
* Golden masters frozen for both functions at 1e-8 across five duration
  cases (scalar and loan-level discount rates, both cash flow columns,
  convexity on and off) and two WAL cases, over a frozen input table. The
  input is frozen rather than regenerated, so engine changes cannot move
  these expectations.
* A reversal guard recomputes the frozen PV and Macaulay duration from
  first principles, so re-freezing under a reverted convention fails.
* Full suite: 302 assertions passing before the golden additions.

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

* Payment dates use lubridate month arithmetic, calculated from the original
  as-of date. Invalid days clamp (Jan 31 → Feb 28) instead of overflowing
  (→ Mar 3); month-end alignment is not enforced (Apr 30 → May 30).
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
