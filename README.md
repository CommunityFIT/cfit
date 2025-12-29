# cfit

<!-- badges: start -->
<!-- badges: end -->

The goal of cfit is to provide transparent, reproducible computations for solving common financial reporting and analytic problems that community financial institutions (community banks and credit unions) regularly encounter. 

The intended audience is financial and treasury analysts who often solve these problems in Excel on an ad hoc basis. cfit aims to standardize and automate solving these computational problems using the open source software R.

cfit encourages feedback and collaboration with the long-term goal of improving trust, transparency, and efficiency in financial reporting and analytics for community financial institutions.

## Installation

cfit is currently in active development and not yet on CRAN. You can install the development version from [GitHub](https://github.com/CommunityFIT/cfit) with:
``` r
# install.packages("devtools")
#Install  from GitHub
devtools::install_github("CommunityFIT/cfit")
#Load cfit library
library(cfit)

```

## Functions

### Prepayment Analysis
- `calculate_prepay_speed()` - Calculate Single Monthly Mortality (SMM) and Conditional Prepayment Rate (CPR) for loan portfolios

*More functions coming soon!*

## Example

Here's how to calculate prepayment speeds for a sample auto loan portfolio:
```r
library(cfit)

# Sample loan portfolio data (two months of snapshots)
loan_data <- data.frame(
  EFFDATE = as.Date(c(
    "2024-01-31", "2024-01-31", "2024-01-31",
    "2024-02-29", "2024-02-29", "2024-02-29"
  )),
  ORIGDATE = as.Date(c(
    "2023-06-15", "2023-08-20", "2023-09-10",
    "2023-06-15", "2023-08-20", "2023-09-10"
  )),
  TYPECODE = c("AUTO", "AUTO", "AUTO", "AUTO", "AUTO", "AUTO"),
  BAL = c(18500, 22000, 15800, 17800, 21200, 15100),
  ORIGBAL = c(25000, 30000, 20000, 25000, 30000, 20000),
  PAYAMT = c(450, 520, 380, 450, 520, 380),
  CURRINTRATE = c(0.0729, 0.0649, 0.0799, 0.0729, 0.0649, 0.0799)
)

# Calculate monthly prepayment speeds
prepay_results <- calculate_prepay_speed(
  df = loan_data,
  group_vars = c("EFFDATE", "TYPECODE")
)

prepay_results

```
### Data Validation

Use `col_loanid` to validate that each loan appears only once per reporting period:
```r
# Add loan IDs to your data
loan_data$LOANNUMBER <- c(101, 101, 103, 101, 102, 103)

# Configure to check for duplicates
validated_config <- list(
  col_loanid = "LOANNUMBER"  # Validates unique loans per period  
)

prepay_results <- calculate_prepay_speed(
  df = loan_data,
  group_vars = c("EFFDATE", "TYPECODE"),
  prepay_config = validated_config
)

# If duplicates exist, the function will error with details:
# Found 1 duplicate loan(s) within the same reporting period
```

### Custom Column Names

If your data uses different column names, configure the mapping:
```r
# Example: Your institution uses different column names
custom_config <- list(
  col_effdate = "ReportDate",
  col_origdate = "OpenDate",
  col_typecode = "Product",
  col_balance = "CurrentBalance",
  col_orig_balance = "StartingBalance",
  col_payment = "MonthlyPayment",
  col_rate = "InterestRate",
  col_interest_basis = NULL,
  interest_basis = 360  # Or 365, global setting, depending on your calculation method
)

result <- calculate_prepay_speed(
  df = your_data,
  group_vars = c("ReportDate", "LoanType"),
  prepay_config = custom_config
)
```

For more details, see `?calculate_prepay_speed`.

## Roadmap

Planned functions include:
- Loan cash flow generation
- Duration and WAL calculations
- CECL analytics

## Contributing

This is an open-source project built for the community banking sector. Feedback, suggestions, and contributions are welcome! 

- Report bugs or request features via [GitHub Issues](https://github.com/CommunityFIT/cfit/issues)
- Questions? Start a [Discussion](https://github.com/CommunityFIT/cfit/discussions)

## About CommunityFIT

cfit is part of the CommunityFIT initiative - open-source computational finance tools for community financial institutions. Learn more at [github.com/CommunityFIT](https://github.com/CommunityFIT).

## License

MIT © Colin Paterson

