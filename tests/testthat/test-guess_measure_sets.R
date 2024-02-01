test_that('correct columns guessed for measure sets', {
  expect_identical(
    guess_measure_sets(mortexp),
    list(
      CNT = c(
        actuals = 'MORT_ACTUAL_CNT',
        exposures = 'MORT_EXPOSURE_CNT',
        expecteds = 'MORT_EXPECTED_CNT',
        variances = 'MORT_VARIANCE_CNT'
      ),
      AMT = c(
        actuals = 'MORT_ACTUAL_AMT',
        exposures = 'MORT_EXPOSURE_AMT',
        expecteds = 'MORT_EXPECTED_AMT',
        variances = 'MORT_VARIANCE_AMT'
      )
    )
  )
})
