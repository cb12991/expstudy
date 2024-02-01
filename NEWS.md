# expstudy 2.0.0

* Complete overhaul of expstudy functionality
* Does not lock into data.table or dtplyr::lazy_dt but instead can be any tabular object.
* Moved away from S3 class because it felt as though there wasn't significant difference from underlying tabular data other than mapping of study measures.
* Focused on function output instead of producing highly-specific, formatted results.
* Built aggregation functionality to perform similarly to dplyr::summarise()
* Built addition of metrics and adjustment of expected/variance measures to perform similarly to dplyr::mutate()
* Added additional function that generates adjustment factors recursively for less-credible mortality assumptions

# expstudy 1.0.3

* Small hotfix for unit test issue identified by data.table-revdeps 

# expstudy 1.0.2

* Patch for compile_results

# expstudy 1.0.1

* Minor fixes

# expstudy 1.0.0

* Added `mortexp` dataset to use for examples.
* Added examples for main functions as requested upon CRAN submission.

# expstudy 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
