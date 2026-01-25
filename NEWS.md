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
