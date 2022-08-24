test_that('results are compiled correctly', {
  x <- compile_results(es, GENDER)

  expect_type(x, 'list')

  expect_named(x, c('UNFORMATTED', 'FORMATTED'))
  expect_named(x$UNFORMATTED, c('METRICS', 'PROPORTIONS'))
  expect_named(x$UNFORMATTED$METRICS, c('AGGREGATE', 'BY GENDER'))
  expect_named(x$FORMATTED, c('METRICS', 'PROPORTIONS'))
  expect_named(x$FORMATTED$METRICS, c('AGGREGATE', 'BY GENDER'))

  expect_s3_class(x$UNFORMATTED$METRICS$AGGREGATE, 'tbl_es')
  expect_s3_class(x$UNFORMATTED$METRICS$`BY GENDER`, 'tbl_es')
  expect_s3_class(x$FORMATTED$METRICS$AGGREGATE, 'tbl_es')
  expect_s3_class(x$FORMATTED$METRICS$`BY GENDER`, 'tbl_es')

  invisible(lapply(
    x$UNFORMATTED$METRICS$AGGREGATE,
    FUN = expect_vector,
    ptype = numeric(),
    size = 1
  ))

  invisible(lapply(
    x$FORMATTED$METRICS$AGGREGATE,
    FUN = expect_vector,
    ptype = character(),
    size = 1
  ))

  invisible(lapply(
    x$UNFORMATTED$METRICS$`BY GENDER`[-1],
    FUN = expect_vector,
    ptype = numeric(),
    size = length(levels(es$parent$GENDER))
  ))
  expect_vector(
    object = x$UNFORMATTED$METRICS$`BY GENDER`[[1]],
    ptype = factor(levels = levels(es$parent$GENDER)),
    size = length(levels(es$parent$GENDER))
  )

  invisible(lapply(
    x$FORMATTED$METRICS$`BY GENDER`[-1],
    FUN = expect_vector,
    ptype = character(),
    size = length(levels(es$parent$GENDER))
  ))
  expect_vector(
    object = x$FORMATTED$METRICS$`BY GENDER`[[1]],
    ptype = factor(levels = levels(es$parent$GENDER)),
    size = length(levels(es$parent$GENDER))
  )
})
