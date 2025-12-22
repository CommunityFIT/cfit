# cfit

<!-- badges: start -->
<!-- badges: end -->

The goal of cfit is to provide transparent, reproducible computations for solving common financial problems that community financial institutions (community banks and credit unions) regularly encounter. 

The intended audience is financial and treasury analysts who often solve these problems in Excel on an ad hoc basis. cfit aims to standardize and automate solving these computational problems using the open source software R.

cfit encourages feedback and collaboration with the long-term goal of improving trust and transparency in financial services and institutions.

## Installation

cfit is currently in active development and not yet on CRAN. You can install the development version from [GitHub](https://github.com/CommunityFIT/cfit) with:
``` r
# install.packages("devtools")
devtools::install_github("CommunityFIT/cfit")
```

## Functions

### Prepayment Analysis
- `calculate_prepay_speed()` - Calculate Single Monthly Mortality (SMM) and Conditional Prepayment Rate (CPR) for loan portfolios

*More functions coming soon!*

## Example

Coming soon - examples will be added as functions are documented.

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

