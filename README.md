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
- `calculate_cash_flows()` – Project monthly loan-level and portfolio-level cash flows under configurable prepayment (static by tier, or a rate-responsive linear-incentive model), credit loss, and fee assumptions

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

**Input validation (development version)**

`calculate_cash_flows()` rejects invalid records before projection. Supply a
non-empty snapshot with unique, non-missing loan IDs, positive finite balances,
non-negative finite decimal rates, positive integer remaining terms, and valid
dates. Correct or deliberately filter invalid records before calling the function.
Unknown or duplicated configuration keys and missing explicitly mapped columns
raise errors. IDs are generated only when the implicit `LOAN_ID` column is absent
or `col_loanid = NULL`.

Assumption vectors must have unique tier names and finite values. PD and LGD must
be supplied together, each within `[0, 1]`, and are matched by tier name rather
than position. Credit costs must cover every tier in the selected prepayment
model. Without a tier column, define a named `default` tier. Unknown or missing
loan tiers use that default for both prepayment and credit assumptions, with a
warning; they raise an error if no default exists. Known-tier portfolios can omit
the default. Reordering assumption vectors does not change results.

Optional payment and original-balance columns accept `NA` to request the existing
per-loan fallback; other supplied values must be positive and finite. These checks
do not add balloon or negative-amortization support.

Supplied payments must cover each period's gross accrued interest after any
survivor scaling. Underpayments raise an error identifying the loan and month.
Interest-only payments are accepted, but the engine warns when loans retain
balances at maturity of at least `max(0.01, de_minimis_balance)`. The warning
reports affected IDs and total residual principal; it does not add a balloon.
PV and WAL calculated from these projections describe only the collections
actually projected.

Small-balance cleanup is reported as additional `scheduled_principal`, so
`total_principal = scheduled_principal + prepayment` in both conventions.
`scheduled_payment` remains the payment before final payoff adjustments.
The fixed-payment convention now includes all prepayments in total principal;
this corrects a cap that could delay or omit principal near payoff.

**Modeling conventions (v0.2.5+)**

- **Prepayments are full payoffs** (`reamortize_survivors = TRUE`, default):
  each month, an SMM-derived fraction of loans pays off entirely and the
  surviving balance re-amortizes over the remaining term. Aggregate scheduled
  payments decline with the survival factor, consistent with market/Bloomberg
  pool conventions. Set `reamortize_survivors = FALSE` for legacy
  fixed-payment (curtailment) behavior, appropriate only for modeling
  individual borrowers who keep their original payment while paying extra
  principal.
- **Payment timing**: `eff_date` is the t = 0 valuation/settlement anchor;
  the first projected payment falls one month later. When calculating yield,
  always anchor `start_date` to the effective date from the loan data (as the
  example below does) — never to the first cash flow date.
- **Interest accrual**: full-month interest accrues on the starting balance
  (`interest_on_starting_balance = TRUE`, default), matching standard
  monthly-pay consumer loan servicing.
- **Credit losses** reduce principal balances only
  (`credit_loss_reduces_interest = FALSE`, default); deducting charge-offs
  from interest as well would double-count the loss.
- **Application order** within each month: credit loss, then scheduled
  principal, then prepayment (SMM applied to the post-scheduled balance).

**Grouping and original classifications (development version)**

Loan cash flows include `original_tier` (the configured input classification) and
`tier` (the assumptions actually used). For example, a Commercial-C loan using
fallback assumptions retains `original_tier = "Commercial-C"` while
`tier = "default"`. Without `col_tier`, `original_tier` is `NA`.

The tier example below demonstrates grouping by original classification and
inspecting fallback assumptions.

Grouping retains missing values and must preserve portfolio totals. Missing
grouping columns now raise errors, including when only loan-level output is
requested. Generated grouping fields are `LOAN_ID`, `eff_date`, `rate`, `tier`,
`original_tier`, `month`, and `date`; cash-flow amount columns cannot be keys.
Rename input classifications that conflict with generated names unless they
are the corresponding configured source column. In particular, `date` always
means projected payment date, not an input reporting or origination date.


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
  start_date = min(loan_portfolio$eff_date),  # anchor = effective date (t = 0),
                                              # NOT the first cash flow date
  compounding = "monthly"    # use "semiannual" for bond-equivalent yield
                             # comparable to Bloomberg quotes
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

# Each configured tier matches directly, so original_tier and tier are identical
unique(cash_flows_tiered[c("LOAN_ID", "original_tier", "tier")])
```

All input tiers are defined above, so no `default` is needed. To summarize cash
flows by original classification, request monthly totals:

```r
config_grouped <- config_tiered
config_grouped$return_monthly_totals <- TRUE
config_grouped$monthly_totals_group_vars <- "original_tier"

results_tiered <- calculate_cash_flows(loan_portfolio_tiered, config_grouped)
head(results_tiered$monthly_totals)
```

If a classification has no explicit assumptions, define a named `default` in
both vectors. Here C retains its classification but uses default assumptions:

```r
config_fallback <- config_grouped
config_fallback$cpr_vec <- c(A = 0.05, B = 0.08, default = 0.12)
config_fallback$credit_cost_vec <- c(A = 0.008, B = 0.015, default = 0.025)
config_fallback$monthly_totals_group_vars <- c("original_tier", "tier")

# Expected warning: unknown tier C uses the named default
results_fallback <- calculate_cash_flows(loan_portfolio_tiered, config_fallback)
unique(results_fallback$loan_cash_flows[c("LOAN_ID", "original_tier", "tier")])
# LOAN_ID original_tier tier
# L001    A             A
# L002    B             B
# L003    A             A
# L004    C             default
```

This example gives C the same rates through the default, so its projected cash
flows are unchanged. Grouping by `original_tier` keeps C separate; grouping by
`tier` combines it with any other loans using default assumptions.

#### Rate-Responsive Prepayment (Linear Incentive)

Instead of fixed CPR assumptions, derive each loan's prepayment speed from its *rate incentive* — the gap between its coupon and the current market rate. As market rates fall, in-the-money borrowers prepay faster:

```r
# Same tiered portfolio as above
config_incentive <- list(
  col_tier            = "tier",
  prepay_model        = "linear_incentive",
  current_market_rate = 0.05,                          # current market/refi rate
  base_cpr_vec = c("A" = 0.06, "B" = 0.08, "C" = 0.04), # intercept CPR by tier
  beta_vec     = c("A" = 2.0,  "B" = 2.5,  "C" = 1.5),  # CPR sensitivity per unit of incentive
  cpr_min_vec  = c("A" = 0.02, "B" = 0.02, "C" = 0.01), # lower clamp
  cpr_max_vec  = c("A" = 0.35, "B" = 0.40, "C" = 0.25), # upper clamp
  credit_cost_vec = c("A" = 0.008, "B" = 0.015, "C" = 0.025),
  servicing_fee   = 0.0025
)

# Per loan: CPR = clamp(base_cpr + beta * (coupon - current_market_rate), cpr_min, cpr_max)
cash_flows_incentive <- calculate_cash_flows(loan_portfolio_tiered, config_incentive)
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
#     88341.55          1.861414           1.85187             5.037683

# Interpretation:
# - Macaulay Duration (1.86 years): Average time to receive cash flows
# - Modified Duration (1.85): Portfolio value changes ~1.85% for 1% rate change
# - Convexity (5.04): Measures curvature of price-yield relationship
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
#      2.020745

# Interpretation: 
# Principal is repaid in an average of 2.02 years
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

Planned improvements include:

- `calculate_effective_duration()` — interest-rate sensitivity under parallel rate shocks (e.g. ±100 bps), using the rate-responsive cash flows introduced in v0.2.4

## Contributing

This is an open-source project built for the community banking sector. Feedback, suggestions, and contributions are welcome! 

- Report bugs or request features via [GitHub Issues](https://github.com/CommunityFIT/cfit/issues)
- Questions? Start a [Discussion](https://github.com/CommunityFIT/cfit/discussions)

## About CommunityFIT

cfit is part of the CommunityFIT initiative - open-source computational finance tools for community financial institutions. Learn more at [github.com/CommunityFIT](https://github.com/CommunityFIT).

## License

MIT © Colin Paterson
