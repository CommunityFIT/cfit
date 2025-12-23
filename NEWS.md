# cfit 0.1.0

First stable release of cfit! 🎉

## New Functions

* `calculate_prepay_speed()` - Calculate Single Monthly Mortality (SMM) and Conditional Prepayment Rate (CPR) for loan portfolios

## Features

* **Flexible column mapping** - Works with any financial institution's data schema
* **Multiple interest calculation methods** - Supports loan-level or global basis (360/365 days)
* **Production-ready safeguards**:
  - SMM clamped to [0,1] to prevent invalid CPR calculations
  - Scheduled principal floored at 0 for interest-only periods
  - EFFDATE validation ensures proper time-series ordering
  - Automatic data sorting by effective date
* **User-friendly output** - Returns results using your original column names
* **Comprehensive validation** - Clear error messages for data issues
* **Verbose mode** - Optional detailed logging for debugging

## Configuration Options

* `allow_negative_prepay` - Control handling of negative prepayment scenarios
* `min_begin_balance` - Filter out small cohorts
* `interest_basis` - Choose 360-day, 365-day, or simple monthly interest (Ignored if col_interest_basis is specified)
* `verbose` - Enable/disable informational messages

## Testing

* 36 automated tests covering all functionality and edge cases
* Battle-tested with real-world scenarios

## Documentation

* Complete function documentation with examples
* README with installation and usage examples
* Comprehensive parameter descriptions
