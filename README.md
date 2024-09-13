
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SAvalidation

![](https://seasadjwg.r-universe.dev/badges/SAvalidation)

SAvalidation provides simple functions for running validation checks on
seasonally adjusted time series and generating dashboards for further
analysis of validation checks.

## Installation

You can install the development version of SAvalidation like so:

``` r
# Using r-universe
install.packages('SAvalidation', repos = c('https://seasadjwg.r-universe.dev', 'https://cloud.r-project.org'))
```

Other solution using devtools:

``` r
# install.packages("devtools")
devtools::install_github("SeasAdjwG/SAvalidation")
```

## Example

This code shows how to run a simple level 1 validation check on a pair
of time series (the unadjusted and seasonally adjusted):

``` r
library(SAvalidation)
level1_validation(data_to_check$nsa, data_to_check$sa)
#> [1] "FAIL: EVIDENCE OF RESIDUAL SEASONALITY OR CALENDAR EFFECTS IN SA SERIES"
```

A level 2 validation report can be created with `level2_validation()`
which returns an html dashboard either in current working directory or a
user specified directory:

``` r
level2_validation(data_to_check$nsa, data_to_check$sa, data_to_check$name)
```

You can also create plots from level 2 validation code for example the
NSA against the SA:

``` r
nsa_sa_plot(data_to_check$nsa, data_to_check$sa)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

Or the derived adjustment factor plot:

``` r
adjust_fact_plot(data_to_check$nsa, data_to_check$sa)
#> Joining with `by = join_by(quarter, year)`
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />
