# cfit
<!-- badges: start -->
<!-- badges: end -->

cfit provides transparent, reproducible computations for financial reporting and analytics problems faced by community financial institutions (community banks and credit unions).

The intended audience is financial and treasury analysts who often solve these problems in Excel on an ad hoc basis. cfit aims to standardize and automate solving these computational problems using the open source software R.

cfit encourages feedback and collaboration with the long-term goal of improving trust, transparency, and efficiency in financial reporting and analytics for community financial institutions.

## Installation

cfit is currently in active development and not yet on CRAN. You can install the development version from [GitHub](https://github.com/CommunityFIT/cfit) with:
``` r
# install.packages("devtools")
# Install from GitHub
devtools::install_github("CommunityFIT/cfit") #From GitHub
# Load cfit library
library(cfit)
```

## Functions

### Prepayment Analysis
- `calculate_prepay_speed()` – Calculate SMM and CPR from portfolio snapshots, with validation and configurable column mappings

### Cash Flow Projection
- `calculate_cash_flows()` – Project monthly loan-level and portfolio-level cash flows under configurable prepayment, credit loss, and fee assumptions

### Duration and WAL Analysis
- `calculate_duration()` – Calculate Macaulay duration, modified duration, and analytical convexity for interest rate risk measurement
- `calculate_wal()` – Calculate weighted average life (WAL) for principal repayment timing analysis

## Examples

### Prepayment Speed Calculation

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

#### Data Validation

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

#### Custom Column Names

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
  interest_basis = 360  # Or 365, depending on your calculation method
)

result <- calculate_prepay_speed(
  df = your_data,
  group_vars = c("ReportDate", "LoanType"),
  prepay_config = custom_config
)
```

For more details, see `?calculate_prepay_speed`.

### Cash Flow Projection and Portfolio Yield

Generate monthly cash flow projections for a loan portfolio and calculate portfolio yield.

**Inputs**
- One row per loan (current snapshot)
- Required fields: balance, rate, months to maturity, effective date
- Optional tier classification for assumption mapping

**Outputs**
- Loan-level monthly projected cash flows
- Optional aggregated monthly totals for portfolio analysis
```r
# Optional: used here only to demonstrate portfolio yield calculation
# FinCal is not a dependency of cfit
install_github("felixfan/FinCal")
library(FinCal)

# Sample loan portfolio snapshot
loan_portfolio <- data.frame(
  LOAN_ID = c("L001", "L002", "L003"),
  balance = c(25000, 50000, 15000),
  current_interest_rate = c(0.0599, 0.0649, 0.0549),
  months_to_maturity = c(60, 48, 36),
  eff_date = as.Date("2025-01-01")
)

# Configure cash flow parameters
config <- list(
  cpr_vec = c("default" = 0.05),           # 5% CPR assumption
  credit_cost_vec = c("default" = 0.01),   # 1% annual credit cost
  servicing_fee = 0.0025,                  # 25 bps servicing fee
  return_monthly_totals = TRUE             # Return aggregated monthly totals
)

# Generate cash flows
results <- calculate_cash_flows(loan_portfolio, config)

# View aggregated monthly totals
head(results$monthly_totals)

# Calculate portfolio yield
pool_cfs <- data.frame(
  date = results$monthly_totals$date,
  amount = results$monthly_totals$investor_total  # Net cash flow to owner
)

portfolio_yield <- yield.actual(
  cf = pool_cfs,
  pv = sum(loan_portfolio$balance),
  start_date = min(loan_portfolio$eff_date),
  compounding = "monthly"
)

print(paste("Portfolio Yield:", round(portfolio_yield * 100, 2), "%"))
```

#### Tier-Based Assumptions

Use different CPR and credit cost assumptions by loan tier:
```r
# Portfolio with tier classifications
loan_portfolio_tiered <- data.frame(
  LOAN_ID = c("L001", "L002", "L003", "L004"),
  balance = c(25000, 50000, 15000, 30000),
  current_interest_rate = c(0.0599, 0.0649, 0.0549, 0.0699),
  months_to_maturity = c(60, 48, 36, 54),
  eff_date = as.Date("2025-01-01"),
  tier = c("A", "B", "A", "C")
)

# Configure tier-based assumptions
config_tiered <- list(
  col_tier = "tier",
  cpr_vec = c("A" = 0.05, "B" = 0.08, "C" = 0.12),
  credit_cost_vec = c("A" = 0.008, "B" = 0.015, "C" = 0.025),
  servicing_fee = 0.0025
)

cash_flows_tiered <- calculate_cash_flows(loan_portfolio_tiered, config_tiered)
```

For more details, see `?calculate_cash_flows`.

### Duration and WAL Analysis

Measure interest rate risk and principal repayment timing using the projected cash flows.

**Duration Analysis**

Calculate Macaulay duration, modified duration, and analytical convexity:
```r
library(cfit)

# Using cash flows from previous example
cash_flows <- results$loan_cash_flows

# Calculate duration metrics
duration_results <- calculate_duration(
  loan_cash_flows = cash_flows,
  include_convexity = TRUE
)

print(duration_results)
#portfolio_pv macaulay_duration modified_duration analytical_convexity
#      88867.3           1.64498          1.636546             3.951401

# Interpretation:
# - Macaulay Duration (1.65 years): Average time to receive cash flows
# - Modified Duration (1.64): Portfolio value changes ~1.64% for 1% rate change
# - Convexity (3.95): Measures curvature of price-yield relationship
```

**Weighted Average Life Analysis**

Calculate WAL to understand principal repayment timing:
```r
# Calculate weighted average life
wal_results <- calculate_wal(
  loan_cash_flows = cash_flows
)

print(wal_results)
# portfolio_wal
#      1.78

# Interpretation: 
# Principal is repaid in an average of 1.78 years
```

**Compare Gross vs Net Metrics**

Analyze both total cash flows and investor's economic interest:
```r
# Gross portfolio metrics (full cash flows)
duration_gross <- calculate_duration(
  cash_flows,
  cash_flow_column = "total_payment"
)

wal_gross <- calculate_wal(
  cash_flows,
  principal_column = "total_principal"
)

# Net investor metrics (after fees and investor share)
duration_net <- calculate_duration(
  cash_flows,
  cash_flow_column = "investor_total"
)

wal_net <- calculate_wal(
  cash_flows,
  principal_column = "investor_principal"
)

# Compare results
cat("Gross Duration:", duration_gross$macaulay_duration, "years\n")
cat("Net Duration:", duration_net$macaulay_duration, "years\n")
cat("Gross WAL:", wal_gross$portfolio_wal, "years\n")
cat("Net WAL:", wal_net$portfolio_wal, "years\n")
```

For more details, see `?calculate_duration` and `?calculate_wal`.

## Roadmap

Planned functions include:
- CECL-oriented cash flow and loss analytics

## Contributing

This is an open-source project built for the community banking sector. Feedback, suggestions, and contributions are welcome! 

- Report bugs or request features via [GitHub Issues](https://github.com/CommunityFIT/cfit/issues)
- Questions? Start a [Discussion](https://github.com/CommunityFIT/cfit/discussions)

## About CommunityFIT

cfit is part of the CommunityFIT initiative - open-source computational finance tools for community financial institutions. Learn more at [github.com/CommunityFIT](https://github.com/CommunityFIT).

## License

MIT © Colin Paterson
