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
