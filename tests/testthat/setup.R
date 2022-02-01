# data -------------------------------------------------------------------------
es <- expstudy(
  data = mortexp,
  actuals = ACTUAL_DEATHS,
  expecteds = EXPECTED_DEATHS,
  exposures = EXPOSURE,
  variances = VARIANCE_DEATHS,
  keys = c(AS_OF_DATE, POLICY_HOLDER)
)

# utils ------------------------------------------------------------------------
attr_preserved <- function(x, f, ...) {
  x_meta <- attributes(x)[c('metric_variables', 'metrics_applied')]

  y <- do.call(f, list2(x, ...))
  y_meta <- attributes(y)[c('metric_variables', 'metrics_applied')]

  testthat::expect_equal(x_meta, y_meta)
}
class_preserved <- function(x, f, ...) {
  expect_s3_class(do.call(f, list2(x, ...)), class = 'tbl_es')
}
