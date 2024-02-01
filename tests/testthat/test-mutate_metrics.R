test_that('metrics correctly added', {
  expect_contains(
    colnames(mutate_metrics(mortexp, .keep = 'none')),
    as.character(sapply(
      c('CNT', 'AMT'),
      \(x) paste(
        c('AVG_EXPEC', 'AVG_OBSRV', 'CREDIBILITY', 'AE_RATIO', 'CI_FCTR'),
        x,
        sep = '_'
      )
    ))
  )
})

test_that('measure_sets passed as attribute in results', {
  expect_identical(
    attr(mutate_metrics(mortexp), 'measure_sets'),
    guess_measure_sets(mortexp)
  )
})
