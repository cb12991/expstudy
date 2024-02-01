test_that('measures summed up correctly', {
  expect_mapequal(
    as.list(summarise_measures(mortexp)),
    lapply(
      X = mortexp[unlist(guess_measure_sets(mortexp), use.names = FALSE)],
      FUN = sum,
      na.rm = TRUE
    )
  )
})

test_that('measure_sets passed as attribute in results', {
  expect_identical(
    attr(summarise_measures(mortexp), 'measure_sets'),
    guess_measure_sets(mortexp)
  )
})
