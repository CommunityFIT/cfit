# cfit 0.2.0

## New Functions

* `calculate_cash_flows()` - Generate monthly cash flow projections for loan portfolios with customizable prepayment speeds (CPR), credit costs, and fee structures

**Features**
* Flexible column name mapping for any institution's data structure
* Tier-based differentiation for CPR and credit cost assumptions
* Support for both direct credit cost specification and PD/LGD methodology
* Optional aggregated monthly totals for portfolio yield (IRR) calculations
* Investor share allocation for loan participation scenarios
* Comprehensive input validation with helpful error messages

**Performance**
* Optimized date calculations for large portfolios
* Progress indicators for portfolios with 1,000+ loans
* Handles edge cases including zero-interest loans and de minimis balances

**Output Options**
* Loan-level cash flows with detailed monthly breakdowns
* Aggregated monthly totals across entire portfolio
* `investor_total` output column calculates the total cash flow due to the owner. This column and the `date` column can be used to calculate the portfolio yield using `FinCal::yield.actual()`

## Improvements

* Enhanced package documentation
* Added dependencies: `purrr`, `tibble`

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
