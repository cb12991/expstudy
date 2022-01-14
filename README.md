
<!-- README.md is generated from README.Rmd. Please edit that file -->

# expstudy

<!-- badges: start -->
<!-- badges: end -->

The goal of **expstudy** is to provide a set of tools to quickly conduct
analysis of an experience study. Commonly used techniques (such as
actual-to-expected analysis) are generalized and streamlines so that
repetitive coding is avoided.

## Prerequisites

-   You will need to be able to access the Ameritas Bitbucket
    repository. Please refer to [this Confluence
    article](https://confluence.ameritas.com/pages/viewpage.action?pageId=93457168 "Version Control (Git)")
    for detail on how to do so.
-   To connect RStudio to Bitbucket, you will need to have set up your
    SSH key and added it to your Bitbucket profile. Please refer to [yet
    another Confluence
    article](https://confluence.ameritas.com/display/ACC/Version+Control+in+Rstudio "SSH Key Setup")
    for detail on how to do so.
-   R and RStudio need to be installed on your machine in a local
    directory, such as C:\\Users\\<corporate login ID>.

## Installation

``` r
# This package has not yet been submitted to CRAN, however,
# you can install the development version from Bitbucket:
# install.packages('devtools')
devtools::install_bitbucket('CorpActuarial/expstudy')
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
compile_results(
  expstudy = es,
  GENDER,
  ATTAINED_AGE
)
#> $UNFORMATTED
#> $UNFORMATTED$METRICS
#> $UNFORMATTED$METRICS$AGGREGATE
#> # A tibble: 1 x 6
#>   ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN ACTUAL_TO_EXPECTED
#> *         <dbl>           <dbl>           <dbl>              <dbl>
#> 1        25109.          24880.          25015.               1.01
#> # ... with 2 more variables: ACTUAL_TO_EXPOSED <dbl>, EXPECTED_TO_EXPOSED <dbl>
#> 
#> $UNFORMATTED$METRICS$`BY GENDER`
#> # A tibble: 2 x 7
#>   GENDER ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN ACTUAL_TO_EXPECTED
#> * <chr>          <dbl>           <dbl>           <dbl>              <dbl>
#> 1 FEMALE        12629.          12512.          12600.               1.01
#> 2 MALE          12479.          12368.          12415.               1.01
#> # ... with 2 more variables: ACTUAL_TO_EXPOSED <dbl>, EXPECTED_TO_EXPOSED <dbl>
#> 
#> $UNFORMATTED$METRICS$`BY ATTAINED_AGE`
#> # A tibble: 3 x 7
#>   ATTAINED_AGE ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN ACTUAL_TO_EXPECTED
#> * <chr>                <dbl>           <dbl>           <dbl>              <dbl>
#> 1 40 to 65             8387.           8463.           8277.              0.991
#> 2 65+                  8223.           8112.           8263.              1.01 
#> 3 Under 40             8499.           8304.           8475.              1.02 
#> # ... with 2 more variables: ACTUAL_TO_EXPOSED <dbl>, EXPECTED_TO_EXPOSED <dbl>
#> 
#> $UNFORMATTED$METRICS$`BY GENDER AND ATTAINED_AGE`
#> # A tibble: 6 x 8
#>   GENDER ATTAINED_AGE ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN
#> * <chr>  <chr>                <dbl>           <dbl>           <dbl>
#> 1 FEMALE 40 to 65             4256.           4340.           4177.
#> 2 FEMALE 65+                  4070.           4041.           4138.
#> 3 FEMALE Under 40             4303.           4130.           4285.
#> 4 MALE   40 to 65             4130.           4123.           4100.
#> 5 MALE   65+                  4153.           4071.           4126.
#> 6 MALE   Under 40             4196.           4174.           4189.
#> # ... with 3 more variables: ACTUAL_TO_EXPECTED <dbl>, ACTUAL_TO_EXPOSED <dbl>,
#> #   EXPECTED_TO_EXPOSED <dbl>
#> 
#> 
#> $UNFORMATTED$PROPORTIONS
#> $UNFORMATTED$PROPORTIONS$AGGREGATE
#> # A tibble: 1 x 6
#>   ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN PROP_ACTUAL_DEATHS
#> *         <dbl>           <dbl>           <dbl>              <dbl>
#> 1        25109.          24880.          25015.                  1
#> # ... with 2 more variables: PROP_EXPECTED_DEATHS <dbl>,
#> #   PROP_RECORD_YEARSPAN <dbl>
#> 
#> $UNFORMATTED$PROPORTIONS$`BY GENDER`
#> # A tibble: 2 x 7
#>   GENDER ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN PROP_ACTUAL_DEATHS
#> * <chr>          <dbl>           <dbl>           <dbl>              <dbl>
#> 1 FEMALE        12629.          12512.          12600.              0.503
#> 2 MALE          12479.          12368.          12415.              0.497
#> # ... with 2 more variables: PROP_EXPECTED_DEATHS <dbl>,
#> #   PROP_RECORD_YEARSPAN <dbl>
#> 
#> $UNFORMATTED$PROPORTIONS$`BY ATTAINED_AGE`
#> # A tibble: 3 x 7
#>   ATTAINED_AGE ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN PROP_ACTUAL_DEATHS
#> * <chr>                <dbl>           <dbl>           <dbl>              <dbl>
#> 1 40 to 65             8387.           8463.           8277.              0.334
#> 2 65+                  8223.           8112.           8263.              0.327
#> 3 Under 40             8499.           8304.           8475.              0.338
#> # ... with 2 more variables: PROP_EXPECTED_DEATHS <dbl>,
#> #   PROP_RECORD_YEARSPAN <dbl>
#> 
#> $UNFORMATTED$PROPORTIONS$`BY GENDER AND ATTAINED_AGE`
#> # A tibble: 6 x 8
#>   GENDER ATTAINED_AGE ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN
#> * <chr>  <chr>                <dbl>           <dbl>           <dbl>
#> 1 FEMALE 40 to 65             4256.           4340.           4177.
#> 2 FEMALE 65+                  4070.           4041.           4138.
#> 3 FEMALE Under 40             4303.           4130.           4285.
#> 4 MALE   40 to 65             4130.           4123.           4100.
#> 5 MALE   65+                  4153.           4071.           4126.
#> 6 MALE   Under 40             4196.           4174.           4189.
#> # ... with 3 more variables: PROP_ACTUAL_DEATHS <dbl>,
#> #   PROP_EXPECTED_DEATHS <dbl>, PROP_RECORD_YEARSPAN <dbl>
#> 
#> 
#> 
#> $FORMATTED
#> $FORMATTED$METRICS
#> $FORMATTED$METRICS$AGGREGATE
#> # A tibble: 1 x 6
#>   ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN ACTUAL_TO_EXPECTED
#> * <chr>         <chr>           <chr>                        <dbl>
#> 1 25,108.70     24,879.52       25,015.08                     1.01
#> # ... with 2 more variables: ACTUAL_TO_EXPOSED <dbl>, EXPECTED_TO_EXPOSED <dbl>
#> 
#> $FORMATTED$METRICS$`BY GENDER`
#> # A tibble: 2 x 7
#>   GENDER ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN ACTUAL_TO_EXPECTED
#> * <chr>  <chr>         <chr>           <chr>                        <dbl>
#> 1 FEMALE 12,629.32     12,511.65       12,600.34                     1.01
#> 2 MALE   12,479.38     12,367.86       12,414.75                     1.01
#> # ... with 2 more variables: ACTUAL_TO_EXPOSED <dbl>, EXPECTED_TO_EXPOSED <dbl>
#> 
#> $FORMATTED$METRICS$`BY ATTAINED_AGE`
#> # A tibble: 3 x 7
#>   ATTAINED_AGE ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN ACTUAL_TO_EXPECTED
#> * <chr>        <chr>         <chr>           <chr>                        <dbl>
#> 1 40 to 65     8,386.55      8,463.07        8,277.11                     0.991
#> 2 65+          8,223.10      8,112.16        8,263.18                     1.01 
#> 3 Under 40     8,499.06      8,304.28        8,474.79                     1.02 
#> # ... with 2 more variables: ACTUAL_TO_EXPOSED <dbl>, EXPECTED_TO_EXPOSED <dbl>
#> 
#> $FORMATTED$METRICS$`BY GENDER AND ATTAINED_AGE`
#> # A tibble: 6 x 8
#>   GENDER ATTAINED_AGE ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN
#> * <chr>  <chr>        <chr>         <chr>           <chr>          
#> 1 FEMALE 40 to 65     4,256.08      4,340.08        4,177.22       
#> 2 FEMALE 65+          4,070.07      4,041.19        4,137.65       
#> 3 FEMALE Under 40     4,303.18      4,130.38        4,285.47       
#> 4 MALE   40 to 65     4,130.47      4,122.99        4,099.89       
#> 5 MALE   65+          4,153.03      4,070.98        4,125.53       
#> 6 MALE   Under 40     4,195.88      4,173.90        4,189.32       
#> # ... with 3 more variables: ACTUAL_TO_EXPECTED <dbl>, ACTUAL_TO_EXPOSED <dbl>,
#> #   EXPECTED_TO_EXPOSED <dbl>
#> 
#> 
#> $FORMATTED$PROPORTIONS
#> $FORMATTED$PROPORTIONS$AGGREGATE
#> # A tibble: 1 x 6
#>   ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN PROP_ACTUAL_DEATHS
#> * <chr>         <chr>           <chr>           <chr>             
#> 1 25,108.70     24,879.52       25,015.08       100%              
#> # ... with 2 more variables: PROP_EXPECTED_DEATHS <chr>,
#> #   PROP_RECORD_YEARSPAN <chr>
#> 
#> $FORMATTED$PROPORTIONS$`BY GENDER`
#> # A tibble: 2 x 7
#>   GENDER ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN PROP_ACTUAL_DEATHS
#> * <chr>  <chr>         <chr>           <chr>           <chr>             
#> 1 FEMALE 12,629.32     12,511.65       12,600.34       50.3%             
#> 2 MALE   12,479.38     12,367.86       12,414.75       49.7%             
#> # ... with 2 more variables: PROP_EXPECTED_DEATHS <chr>,
#> #   PROP_RECORD_YEARSPAN <chr>
#> 
#> $FORMATTED$PROPORTIONS$`BY ATTAINED_AGE`
#> # A tibble: 3 x 7
#>   ATTAINED_AGE ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN PROP_ACTUAL_DEATHS
#> * <chr>        <chr>         <chr>           <chr>           <chr>             
#> 1 40 to 65     8,386.55      8,463.07        8,277.11        33.4%             
#> 2 65+          8,223.10      8,112.16        8,263.18        32.75%            
#> 3 Under 40     8,499.06      8,304.28        8,474.79        33.85%            
#> # ... with 2 more variables: PROP_EXPECTED_DEATHS <chr>,
#> #   PROP_RECORD_YEARSPAN <chr>
#> 
#> $FORMATTED$PROPORTIONS$`BY GENDER AND ATTAINED_AGE`
#> # A tibble: 6 x 8
#>   GENDER ATTAINED_AGE ACTUAL_DEATHS EXPECTED_DEATHS RECORD_YEARSPAN
#> * <chr>  <chr>        <chr>         <chr>           <chr>          
#> 1 FEMALE 40 to 65     4,256.08      4,340.08        4,177.22       
#> 2 FEMALE 65+          4,070.07      4,041.19        4,137.65       
#> 3 FEMALE Under 40     4,303.18      4,130.38        4,285.47       
#> 4 MALE   40 to 65     4,130.47      4,122.99        4,099.89       
#> 5 MALE   65+          4,153.03      4,070.98        4,125.53       
#> 6 MALE   Under 40     4,195.88      4,173.90        4,189.32       
#> # ... with 3 more variables: PROP_ACTUAL_DEATHS <chr>,
#> #   PROP_EXPECTED_DEATHS <chr>, PROP_RECORD_YEARSPAN <chr>
```

The `compile_results()` function combines many `expstudy` functions to
produce many summaries easily. You can review and trim the summaries
down to what you want without much effort. An unformatted summary can be
used for subsequent analysis, or a formatted summary can be quickly and
simply inserted into a report for communicative purposes.
