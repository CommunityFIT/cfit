# cfit
<!-- badges: start -->
<!-- badges: end -->

cfit is an R package for financial and treasury analysts at community banks and
credit unions. It provides reproducible loan cash flows, historical prepayment speeds,
and portfolio analytics.

## Installation

Install from GitHub (not yet on CRAN):

```r
# install.packages("devtools")
devtools::install_github("CommunityFIT/cfit")
library(cfit)
```

## Upgrading to v0.2.6

Key changes from 0.2.5.1:

- Invalid inputs now error rather than being skipped or silently corrected.
- Fixed-payment projections correctly include prepayments in total principal.
- Modified duration and convexity are corrected for heterogeneous discount rates.
  PV, Macaulay duration, and WAL calculations are unchanged for unchanged valid inputs.
- Output adds `original_tier` and prepayment diagnostics. Unresolved historical
  prepayment estimates return `NA`.

Review input requirements below and [NEWS.md](NEWS.md) for migration details.
Payment-date conventions are unchanged; balloon schedules remain future work.

## Functions

| Function | Purpose |
| --- | --- |
| `calculate_prepay_speed()` | Calculate SMM and CPR from monthly snapshots |
| `calculate_cash_flows()` | Project loan cash flows with prepayment, credit loss, and fees |
| `calculate_duration()` | Calculate Macaulay/modified duration and analytical convexity |
| `calculate_wal()` | Calculate weighted average life of principal repayments |

## Examples

### Prepayment Speed Calculation

Calculate single monthly mortality (SMM) and its annualized conditional prepayment
rate (CPR) from balance changes after scheduled principal and funding adjustments.

Use consecutive monthly snapshots with one reporting date per month. Loan IDs,
when supplied, must be non-missing and unique within each date. Required balances,
payments, and rates must be finite, non-missing, and non-negative.

```r
# Sample loan portfolio data (two months of snapshots)
loan_data <- data.frame(
  LOANNUMBER = rep(c(101, 102, 103), 2),
  EFFDATE = as.Date(c(
    "2024-01-31", "2024-01-31", "2024-01-31",
    "2024-02-29", "2024-02-29", "2024-02-29"
  )),
  ORIGDATE = as.Date(c(
    "2023-06-15", "2023-08-20", "2023-09-10",
    "2023-06-15", "2023-08-20", "2023-09-10"
  )),
  TYPECODE = "AUTO",
  BAL = c(18500, 22000, 15800, 17800, 21200, 15100),
  ORIGBAL = c(25000, 30000, 20000, 25000, 30000, 20000),
  PAYAMT = c(450, 520, 380, 450, 520, 380),
  CURRINTRATE = c(0.0729, 0.0649, 0.0799, 0.0729, 0.0649, 0.0799)
)

# Calculate monthly prepayment speeds
prepay_results <- calculate_prepay_speed(
  df = loan_data,
  group_vars = c("EFFDATE", "TYPECODE"),
  prepay_config = list(col_loanid = "LOANNUMBER")
)

prepay_results
```

#### Snapshot Diagnostics

Inspect diagnostics before using estimates as projection assumptions:

```r
prepay_results[c("EFFDATE", "TYPECODE", "SMM_RAW", "SMM", "CPR",
                 "SMM_ADJUSTED", "DIAGNOSTIC")]
```

`SMM_RAW` is the unbounded estimate. `SMM` is the bounded value used for CPR:
`[0, 1]` by default, or `[-1, 1]` with `allow_negative_prepay = TRUE`.
`SMM_ADJUSTED` identifies bounding. Non-positive denominators yield `NA` speeds,
not `Inf` or `NaN`. Undefined estimates produce a summary warning.

An absent loan is not automatically a payoff. This example removes loan 102
from February while keeping the AUTO cohort:

```r
incomplete_snapshots <- subset(
  loan_data, !(LOANNUMBER == 102 & EFFDATE == as.Date("2024-02-29"))
)
# Expected warning: one cohort-period has an undefined estimate
exit_results <- calculate_prepay_speed(
  incomplete_snapshots,
  group_vars = c("EFFDATE", "TYPECODE"),
  prepay_config = list(col_loanid = "LOANNUMBER")
)
exit_results[c("EFFDATE", "TYPECODE", "UNRESOLVED_EXITS", "CPR", "DIAGNOSTIC")]
# February AUTO: UNRESOLVED_EXITS = 1, CPR = NA, DIAGNOSTIC = "unresolved_exits"
```

A disappearing cohort remains in the output with `COHORT_DISAPPEARED = TRUE`,
unknown ending balance, and `NA` speeds. Reappearing cohorts do not reuse stale
balances. With IDs, transfers and unexplained entrants are also flagged;
same-month originations use approximate funding. Flagged rows bypass
`min_begin_balance` filtering.

Without IDs, individual exits within a surviving cohort cannot be detected.
`DIAGNOSTIC = "ok"` means no issue was detected, not that attribution is certain.
Do not treat undefined estimates as zero or missing loans as confirmed payoffs.

#### Custom Column Names

Map institution-specific names through `prepay_config`; for example:

```r
your_data <- dplyr::rename(loan_data, LoanID = LOANNUMBER, ReportDate = EFFDATE)
result <- calculate_prepay_speed(
  your_data,
  group_vars = c("ReportDate", "TYPECODE"),
  prepay_config = list(col_loanid = "LoanID", col_effdate = "ReportDate")
)
```

See `?calculate_prepay_speed` for all column mappings and interest-basis options.

### Cash Flow Projection

Supply one row per loan with a positive finite balance, non-negative finite decimal rate,
positive integer remaining term, and valid effective date. IDs must be unique
and non-missing; they are generated if the implicit `LOAN_ID` column is absent
or `col_loanid = NULL`. Invalid configuration keys or missing mapped columns error.
Optional payment and original-balance values may be `NA` to use per-loan fallbacks;
otherwise they must be positive and finite.

#### Modeling Conventions

- **Prepayment:** by default, `reamortize_survivors = TRUE` treats prepayments as
  full payoffs and re-amortizes survivors over the remaining term. Aggregate
  scheduled payments decline with survival. Set `FALSE` for fixed-payment
  curtailment modeling.
- **Timing:** `eff_date` is time zero; the first payment is one month later.
  Anchor yield calculations to `eff_date`.
- **Interest:** accrues on starting balances by default
  (`interest_on_starting_balance = TRUE`). Credit losses reduce principal only
  (`credit_loss_reduces_interest = FALSE`).
- **Order:** credit loss, scheduled principal, then prepayment on the remaining balance.
- **Principal:** `total_principal = scheduled_principal + prepayment`.
  Small-balance cleanup is included in scheduled principal; `scheduled_payment`
  is the payment before final payoff adjustments.

Payments below accrued interest error. Interest-only payments are accepted, but
residual balances at maturity of at least `max(0.01, de_minimis_balance)` warn.
No balloon is added; analytics reflect only projected collections. Balloon and
negative-amortization schedules are not supported.

#### Basic Projection

```r
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
```

#### Portfolio Yield (Optional)

This example uses the separate FinCal package:

```r
# devtools::install_github("felixfan/FinCal")
library(FinCal)

pool_cfs <- data.frame(
  date = results$monthly_totals$date,
  amount = results$monthly_totals$investor_total  # Net cash flow to owner
)

portfolio_yield <- yield.actual(
  cf = pool_cfs,
  pv = sum(loan_portfolio$balance),
  start_date = min(loan_portfolio$eff_date),
  compounding = "monthly"  # "semiannual" for bond-equivalent yield
)

print(paste("Portfolio Yield:", round(portfolio_yield * 100, 2), "%"))
```

#### Tier-Based Assumptions

Assumption vectors require unique tier names and finite values. Credit costs must
cover every active prepayment tier. Alternatively, supply PD and LGD together
with matching tier names and values in `[0, 1]`; they are matched by name.

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

`original_tier` retains the input classification; `tier` identifies the assumptions
used. All tiers above match, so no `default` is needed. Without `col_tier`,
`original_tier` is `NA` and a named `default` is required.

To aggregate by original classification:

```r
config_grouped <- config_tiered
config_grouped$return_monthly_totals <- TRUE
config_grouped$monthly_totals_group_vars <- "original_tier"

results_tiered <- calculate_cash_flows(loan_portfolio_tiered, config_grouped)
head(results_tiered$monthly_totals)
```

Unknown or missing tiers use a named `default` with a warning, or error if it is
absent. Here C retains its classification while using default assumptions:

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

C's rates and cash flows are unchanged. Grouping by `original_tier` keeps C
separate; grouping by `tier` combines loans using the same assumptions.

Grouping retains missing values and preserves totals. Keys may be loan metadata
or generated fields (`LOAN_ID`, `eff_date`, `rate`, `tier`, `original_tier`, `month`,
`date`), but not cash-flow amounts. Missing keys error in either return mode.
Rename conflicting input columns unless explicitly mapped to the corresponding
field; `date` always means projected payment date.

#### Rate-Responsive Prepayment (Linear Incentive)

The linear-incentive model derives CPR from the gap between the loan's coupon
and the market rate. With positive `beta`, lower market rates increase CPR:

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

Use loan-level cash flows with a common `eff_date`, valid IDs/dates, and finite
non-negative amounts (and rates for duration). Each loan's month indices must be
unique and contiguous from 1; different maturities are allowed.
`validate_month_index = FALSE` permits intentional gaps or offsets, but not
duplicates or invalid data. Check maturity warnings too: a missing final payment
cannot be detected from the month sequence alone.

Timing is `month / 12`. Duration defaults to each loan's rate; use
`discount_rate = 0.05`, for example, for a common 5% rate. Sensitivities measure
parallel discount-rate shifts with cash flows held fixed.

```r
cash_flows <- results$loan_cash_flows
duration_results <- calculate_duration(cash_flows, include_convexity = TRUE)
wal_results <- calculate_wal(cash_flows)

duration_results
# portfolio_pv macaulay_duration modified_duration analytical_convexity
#     88341.55          1.861414          1.851849             5.037529

wal_results
# portfolio_wal
#      2.020745
```

- **Macaulay duration (1.86 years):** present-value-weighted time to receive cash flows.
- **Modified duration (1.85):** approximately a 1.85% value decline for a
  one-percentage-point rate increase, before convexity effects.
- **Convexity (5.04):** curvature of the price–yield relationship.
- **WAL (2.02 years):** principal-weighted time to repayment.

Defaults use gross payments and principal. To analyze investor cash flows after
fees and ownership share:

```r
duration_net <- calculate_duration(cash_flows, cash_flow_column = "investor_total")
wal_net <- calculate_wal(cash_flows, principal_column = "investor_principal")
```

See `?calculate_duration` and `?calculate_wal` for options.

## Roadmap

Planned improvements after v0.2.6 include:

- Balloon and interest-only loan schedules, including commercial loans
- A configurable month-end payment-date convention
- Effective duration with rate-responsive cash flows under parallel rate shocks

## Contributing

Report bugs and feature requests through [GitHub Issues](https://github.com/CommunityFIT/cfit/issues),
or ask questions in [Discussions](https://github.com/CommunityFIT/cfit/discussions).

cfit is part of [CommunityFIT](https://github.com/CommunityFIT), an open-source
initiative for community financial institutions.

## License

MIT © Colin Paterson
