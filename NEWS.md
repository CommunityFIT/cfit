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
