
<!-- README.md is generated from README.Rmd. Please edit that file -->

# expstudy

<!-- badges: start -->

[![R-CMD-check](https://github.com/cb12991/expstudy/workflows/R-CMD-check/badge.svg)](https://github.com/cb12991/expstudy/actions)
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

First you need a dataset that can be used for an experience study:

``` r
dataset <- data.frame(
  GENDER = sample(c('MALE','FEMALE'), size = 10000, replace = TRUE),
  ATTAINED_AGE = sample(
    c('Under 40', '40 to 65', '65+'), size = 10000, replace = TRUE
  ),
  RECORD_YEARSPAN = runif(10000, max = 5),
  EXPECTED_DEATHS = runif(10000, max = 5),
  ACTUAL_DEATHS = runif(10000, max = 5)
)
```

Now you can convert to an `expstudy` object:

``` r
# Only need to specify which variables correlate to metric variable `actuals`, 
# `expecteds`, and `exposures`. `variances` and `keys` are beneficial, but not
# required. 

es <- expstudy(
  data = dataset,
  actuals = ACTUAL_DEATHS,
  expecteds = EXPECTED_DEATHS,
  exposures = RECORD_YEARSPAN
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
glimpse(results)
#> List of 2
#>  $ UNFORMATTED:List of 2
#>   ..$ METRICS    :List of 4
#>   .. ..$ AGGREGATE                 : tbl_es [1 x 6] (S3: tbl_es/tbl_df/tbl/dat..
#>   .. .. ..- attr(*, "metric_vars")=List of 3
#>   .. .. ..- attr(*, "metrics_applied")=List of 2
#>   .. ..$ BY GENDER                 : tbl_es [2 x 7] (S3: tbl_es/tbl_df/tbl/dat..
#>   .. .. ..- attr(*, "metric_vars")=List of 3
#>   .. .. ..- attr(*, "metrics_applied")=List of 2
#>   .. ..$ BY ATTAINED_AGE           : tbl_es [3 x 7] (S3: tbl_es/tbl_df/tbl/dat..
#>   .. .. ..- attr(*, "metric_vars")=List of 3
#>   .. .. ..- attr(*, "metrics_applied")=List of 2
#>   .. ..$ BY GENDER AND ATTAINED_AGE: tbl_es [6 x 8] (S3: tbl_es/tbl_df/tbl/dat..
#>   .. .. ..- attr(*, "metric_vars")=List of 3
#>   .. .. ..- attr(*, "metrics_applied")=List of 2
#>   ..$ PROPORTIONS:List of 4
#>   .. ..$ AGGREGATE                 : tbl_es [1 x 6] (S3: tbl_es/tbl_df/tbl/dat..
#>   .. .. ..- attr(*, "metric_vars")=List of 3
#>   .. .. ..- attr(*, "metrics_applied")=List of 2
#>   .. ..$ BY GENDER                 : tbl_es [2 x 7] (S3: tbl_es/tbl_df/tbl/dat..
#>   .. .. ..- attr(*, "metric_vars")=List of 3
#>   .. .. ..- attr(*, "metrics_applied")=List of 2
#>   .. ..$ BY ATTAINED_AGE           : tbl_es [3 x 7] (S3: tbl_es/tbl_df/tbl/dat..
#>   .. .. ..- attr(*, "metric_vars")=List of 3
#>   .. .. ..- attr(*, "metrics_applied")=List of 2
#>   .. ..$ BY GENDER AND ATTAINED_AGE: tbl_es [6 x 8] (S3: tbl_es/tbl_df/tbl/dat..
#>   .. .. ..- attr(*, "metric_vars")=List of 3
#>   .. .. ..- attr(*, "metrics_applied")=List of 2
#>  $ FORMATTED  :List of 2
#>   ..$ METRICS    :List of 4
#>   .. ..$ AGGREGATE                 : tbl_es [1 x 6] (S3: tbl_es/tbl_df/tbl/dat..
#>   .. .. ..- attr(*, "metric_vars")=List of 3
#>   .. .. ..- attr(*, "metrics_applied")=List of 2
#>   .. ..$ BY GENDER                 : tbl_es [2 x 7] (S3: tbl_es/tbl_df/tbl/dat..
#>   .. .. ..- attr(*, "metric_vars")=List of 3
#>   .. .. ..- attr(*, "metrics_applied")=List of 2
#>   .. ..$ BY ATTAINED_AGE           : tbl_es [3 x 7] (S3: tbl_es/tbl_df/tbl/dat..
#>   .. .. ..- attr(*, "metric_vars")=List of 3
#>   .. .. ..- attr(*, "metrics_applied")=List of 2
#>   .. ..$ BY GENDER AND ATTAINED_AGE: tbl_es [6 x 8] (S3: tbl_es/tbl_df/tbl/dat..
#>   .. .. ..- attr(*, "metric_vars")=List of 3
#>   .. .. ..- attr(*, "metrics_applied")=List of 2
#>   ..$ PROPORTIONS:List of 4
#>   .. ..$ AGGREGATE                 : tbl_es [1 x 6] (S3: tbl_es/tbl_df/tbl/dat..
#>   .. .. ..- attr(*, "metric_vars")=List of 3
#>   .. .. ..- attr(*, "metrics_applied")=List of 2
#>   .. ..$ BY GENDER                 : tbl_es [2 x 7] (S3: tbl_es/tbl_df/tbl/dat..
#>   .. .. ..- attr(*, "metric_vars")=List of 3
#>   .. .. ..- attr(*, "metrics_applied")=List of 2
#>   .. ..$ BY ATTAINED_AGE           : tbl_es [3 x 7] (S3: tbl_es/tbl_df/tbl/dat..
#>   .. .. ..- attr(*, "metric_vars")=List of 3
#>   .. .. ..- attr(*, "metrics_applied")=List of 2
#>   .. ..$ BY GENDER AND ATTAINED_AGE: tbl_es [6 x 8] (S3: tbl_es/tbl_df/tbl/dat..
#>   .. .. ..- attr(*, "metric_vars")=List of 3
#>   .. .. ..- attr(*, "metrics_applied")=List of 2
```

An unformatted summary can be used for subsequent analysis…

``` r
results$UNFORMATTED$METRICS$`BY GENDER`
#> # A tibble: 2 x 7
#>   GENDER ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN ACTUAL_TO_EXPECTED
#> * <chr>          <dbl>           <dbl>           <dbl>              <dbl>
#> 1 FEMALE        12506.          12658.          12585.              0.988
#> 2 MALE          12648.          12671.          12483.              0.998
#> # ... with 2 more variables: ACTUAL_TO_EXPOSED <dbl>, EXPECTED_TO_EXPOSED <dbl>
```

…and a formatted summary can be quickly and simply inserted into a
report to distribute nicely.

``` r
results$FORMATTED$PROPORTIONS$`BY ATTAINED_AGE`
#> # A tibble: 3 x 7
#>   ATTAINED_AGE ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN PROP_ACTUAL_DEATHS
#> * <chr>        <chr>         <chr>           <chr>           <chr>             
#> 1 40 to 65     8,359.39      8,360.20        8,316.51        33.23%            
#> 2 65+          8,394.15      8,457.91        8,368.34        33.37%            
#> 3 Under 40     8,400.83      8,511.20        8,383.20        33.4%             
#> # ... with 2 more variables: PROP_EXPECTED_DEATHS <chr>,
#> #   PROP_RECORD_YEARSPAN <chr>
```

You can review and trim the summaries down to what you want without much
effort. You also can access the formulae that are used in
`compile_results()` to fully customize the analysis to fit your needs.

## Code of Conduct

Please note that the expstudy project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.