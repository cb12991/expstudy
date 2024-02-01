# nocov start

.onLoad = function(libname, pkgname) {
  # Mimicked dplyr (https://github.com/tidyverse/dplyr/blob/main/R/zzz.R).

  op <- options()
  op.expstudy <- list(
    expstudy.default_measure_regexs = list(
      actuals = '_?ACTUAL_?',
      exposures = '_?EXPOSURE_?',
      expecteds = '_?EXPECTED_?',
      variances = '_?VARIANCE_?'
    ),
    expstudy.default_measure_set_prefixes = NULL,
    expstudy.default_measure_set_suffixes = c('CNT', 'AMT')
  )
  toset <- !(names(op.expstudy) %in% names(op))
  if (any(toset)) options(op.expstudy[toset])

  invisible()
}

# nocov end
