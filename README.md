
<!-- README.md is generated from README.Rmd. Please edit that file -->

# expstudy

<!-- badges: start -->

[![R-CMD-check](https://github.com/cb12991/expstudy/workflows/R-CMD-check/badge.svg)](https://github.com/cb12991/expstudy/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/expstudy)](https://CRAN.R-project.org/package=expstudy)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Launch
binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/cb12991/expstudy/main)
[![Codecov test
coverage](https://codecov.io/gh/cb12991/expstudy/branch/main/graph/badge.svg)](https://app.codecov.io/gh/cb12991/expstudy?branch=main)
<!-- badges: end -->

The goal of **expstudy** is to provide a set of tools to quickly conduct
analysis of an experience study. Commonly used techniques (such as
actual-to-expected analysis) are generalized and streamlined so that
repetitive coding is avoided.

## Installation

``` r
# This package has not yet been submitted to CRAN, however,
# you can install the development version from GitHub:

# install.packages('devtools')
devtools::install_github('cb12991/expstudy')
```

## Usage

``` r
library(expstudy)
#> 
#> Attaching package: 'expstudy'
#> The following objects are masked from 'package:stats':
#> 
#>     aggregate, filter
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, union
```

First you need a dataset that can be used for an experience study. This
package provides a sample mortality experience study to aid with
examples:

``` r
glimpse(mortexp)
#> Rows: 175,491
#> Columns: 18
#> $ AS_OF_DATE            <date> 1998-04-30, 1998-05-31, 1998-06-30, 1998-07-31,~
#> $ POLICY_HOLDER         <fct> PH_0001, PH_0001, PH_0001, PH_0001, PH_0001, PH_~
#> $ GENDER                <fct> MALE, MALE, MALE, MALE, MALE, MALE, MALE, MALE, ~
#> $ SMOKING_STATUS        <fct> NON-SMOKER, NON-SMOKER, NON-SMOKER, NON-SMOKER, ~
#> $ UNDERWRITING_CLASS    <fct> STANDARD, STANDARD, STANDARD, STANDARD, STANDARD~
#> $ INSURED_DOB           <date> 1948-09-10, 1948-09-10, 1948-09-10, 1948-09-10,~
#> $ ISSUE_DATE            <date> 1998-04-12, 1998-04-12, 1998-04-12, 1998-04-12,~
#> $ ISSUE_AGE             <dbl> 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, ~
#> $ ATTAINED_AGE          <dbl> 50, 50, 50, 50, 50, 51, 51, 51, 51, 51, 51, 51, ~
#> $ DURATION_MONTH        <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1~
#> $ DURATION_YEAR         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, ~
#> $ POLICY_STATUS         <fct> DEATH, DEATH, DEATH, DEATH, DEATH, DEATH, DEATH,~
#> $ TERMINATION_DATE      <date> 2012-04-13, 2012-04-13, 2012-04-13, 2012-04-13,~
#> $ EXPOSURE              <dbl> 0.04931507, 0.08219178, 0.07945205, 0.08219178, ~
#> $ ACTUAL_DEATHS         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
#> $ EXPECTED_MORTALITY_RT <dbl> 0.01428571, 0.01428571, 0.01428571, 0.01428571, ~
#> $ EXPECTED_DEATHS       <dbl> 0.000704501, 0.001174168, 0.001135029, 0.0011741~
#> $ VARIANCE_DEATHS       <dbl> 0.0007040047, 0.0011727896, 0.0011337411, 0.0011~
```

Now you can convert to an `expstudy` object:

``` r
# Only need to specify which variables correlate to metric variable `actuals`, 
# `expecteds`, and `exposures`. `variances` and `keys` are beneficial, but not
# required. 

es <- expstudy(
  data = mortexp,
  actuals = ACTUAL_DEATHS,
  expecteds = EXPECTED_DEATHS,
  exposures = EXPOSURE,
  variances = VARIANCE_DEATHS
)
```

With this `expstudy` object, a quick example can be shown using
`compile_results()`:

``` r
# We will supply only the dataset and grouping variables to use for the 
# summaries to generate a more robust list of results. 

results <- compile_results(
  expstudy = es,
  GENDER,
  ATTAINED_AGE
)
```

The `compile_results()` function combines many `expstudy` functions to
produce many summaries. Since nothing other than the potential groupings
were provided, `coompile_results()` will generate a nested list of
summaries:

``` r
glimpse(results, give.attr = FALSE)
#> List of 2
#>  $ UNFORMATTED:List of 2
#>   ..$ METRICS    :List of 4
#>   .. ..$ AGGREGATE                 : tbl_es [1 x 7] (S3: tbl_es/tbl_df/tbl/data.frame)
#>   .. ..$ BY GENDER                 : tbl_es [2 x 8] (S3: tbl_es/tbl_df/tbl/data.frame)
#>   .. ..$ BY ATTAINED_AGE           : tbl_es [83 x 8] (S3: tbl_es/tbl_df/tbl/data.frame)
#>   .. ..$ BY GENDER AND ATTAINED_AGE: tbl_es [166 x 9] (S3: tbl_es/tbl_df/tbl/data.frame)
#>   ..$ PROPORTIONS:List of 4
#>   .. ..$ AGGREGATE                 : tbl_es [1 x 8] (S3: tbl_es/tbl_df/tbl/data.frame)
#>   .. ..$ BY GENDER                 : tbl_es [2 x 9] (S3: tbl_es/tbl_df/tbl/data.frame)
#>   .. ..$ BY ATTAINED_AGE           : tbl_es [83 x 9] (S3: tbl_es/tbl_df/tbl/data.frame)
#>   .. ..$ BY GENDER AND ATTAINED_AGE: tbl_es [166 x 10] (S3: tbl_es/tbl_df/tbl/data.frame)
#>  $ FORMATTED  :List of 2
#>   ..$ METRICS    :List of 4
#>   .. ..$ AGGREGATE                 : tbl_es [1 x 7] (S3: tbl_es/tbl_df/tbl/data.frame)
#>   .. ..$ BY GENDER                 : tbl_es [2 x 8] (S3: tbl_es/tbl_df/tbl/data.frame)
#>   .. ..$ BY ATTAINED_AGE           : tbl_es [83 x 8] (S3: tbl_es/tbl_df/tbl/data.frame)
#>   .. ..$ BY GENDER AND ATTAINED_AGE: tbl_es [166 x 9] (S3: tbl_es/tbl_df/tbl/data.frame)
#>   ..$ PROPORTIONS:List of 4
#>   .. ..$ AGGREGATE                 : tbl_es [1 x 8] (S3: tbl_es/tbl_df/tbl/data.frame)
#>   .. ..$ BY GENDER                 : tbl_es [2 x 9] (S3: tbl_es/tbl_df/tbl/data.frame)
#>   .. ..$ BY ATTAINED_AGE           : tbl_es [83 x 9] (S3: tbl_es/tbl_df/tbl/data.frame)
#>   .. ..$ BY GENDER AND ATTAINED_AGE: tbl_es [166 x 10] (S3: tbl_es/tbl_df/tbl/data.frame)
```

An unformatted summary can be used for subsequent analysis…

``` r
results$UNFORMATTED$METRICS$`BY GENDER`
#> # A tibble: 2 x 8
#>   GENDER ACTUAL_DEATHS EXPECTED_DEATHS EXPOSURE VARIANCE_DEATHS ACTUAL_TO_EXPEC~
#> * <fct>          <dbl>           <dbl>    <dbl>           <dbl>            <dbl>
#> 1 FEMALE           134            107.    6098.            107.             1.25
#> 2 MALE             215            145.    8166.            145.             1.48
#> # ... with 2 more variables: ACTUAL_TO_EXPOSED <dbl>, EXPECTED_TO_EXPOSED <dbl>
```

…and a formatted summary can be quickly and simply inserted into a
report to distribute nicely.

``` r
results$FORMATTED$PROPORTIONS$`BY ATTAINED_AGE`
#> # A tibble: 83 x 9
#>    ATTAINED_AGE ACTUAL_DEATHS EXPECTED_DEATHS EXPOSURE VARIANCE_DEATHS
#>  *        <dbl> <chr>         <chr>           <chr>    <chr>          
#>  1           19 0.00          0.21            21.66    0.21           
#>  2           20 0.00          0.57            56.52    0.56           
#>  3           21 0.00          0.66            65.52    0.66           
#>  4           22 0.00          0.71            69.88    0.71           
#>  5           23 0.00          0.83            80.32    0.83           
#>  6           24 0.00          0.88            84.89    0.88           
#>  7           25 0.00          0.98            93.39    0.98           
#>  8           26 0.00          1.07            100.19   1.06           
#>  9           27 2.00          1.20            111.25   1.20           
#> 10           28 0.00          1.30            119.39   1.30           
#> # ... with 73 more rows, and 4 more variables: PROP_ACTUAL_DEATHS <chr>,
#> #   PROP_EXPECTED_DEATHS <chr>, PROP_EXPOSURE <chr>, PROP_VARIANCE_DEATHS <chr>
```

You can review and trim the summaries down to what you want without much
effort. You also can access the formulae that are used in
`compile_results()` to fully customize the analysis to fit your needs.

## Code of Conduct

Please note that the expstudy project is released with a [Contributor
Code of
Conduct](https://cb12991.github.io/expstudy/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
