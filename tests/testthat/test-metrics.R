

test_that('measure set version `avg_expected` equals vector version', {
  measure_set <- list(
    actuals = 'MORT_ACTUAL_CNT',
    expecteds = 'MORT_EXPECTED_CNT',
    exposures = 'MORT_EXPOSURE_CNT',
    variances = 'MORT_VARIANCE_CNT'
  )
  x <- mortexp |>
    summarise_measures(
      measure_sets = list(BY_CNT = measure_set)
    ) |>
    mutate_metrics(
      metrics = list(AVG_EXPECTED = avg_expected)
    ) |>
    dplyr::mutate(
      AVG_EXPECTED_VEC_BY_CNT = avg_expected_vec(
        !!rlang::sym(measure_set$expecteds),
        !!rlang::sym(measure_set$exposures)
      )
    )
  expect_identical(
    x$AVG_EXPECTED_BY_CNT,
    x$AVG_EXPECTED_VEC_BY_CNT,
  )
})

test_that('measure set version `avg_observed` equals vector version', {
  measure_set <- list(
    actuals = 'MORT_ACTUAL_CNT',
    expecteds = 'MORT_EXPECTED_CNT',
    exposures = 'MORT_EXPOSURE_CNT',
    variances = 'MORT_VARIANCE_CNT'
  )
  x <- mortexp |>
    summarise_measures(
      measure_sets = list(BY_CNT = measure_set)
    ) |>
    mutate_metrics(
      metrics = list(AVG_OBSERVED = avg_observed)
    ) |>
    dplyr::mutate(
      AVG_OBSERVED_VEC_BY_CNT = avg_observed_vec(
        !!rlang::sym(measure_set$actuals),
        !!rlang::sym(measure_set$exposures)
      )
    )
  expect_identical(
    x$AVG_OBSERVED_BY_CNT,
    x$AVG_OBSERVED_VEC_BY_CNT,
  )
})

test_that('measure set version `ci_fctr` equals vector version', {
  measure_set <- list(
    actuals = 'MORT_ACTUAL_CNT',
    expecteds = 'MORT_EXPECTED_CNT',
    exposures = 'MORT_EXPOSURE_CNT',
    variances = 'MORT_VARIANCE_CNT'
  )
  x <- mortexp |>
    summarise_measures(
      measure_sets = list(BY_CNT = measure_set)
    ) |>
    mutate_metrics(
      metrics = list(CI_FCTR = ci_fctr)
    ) |>
    dplyr::mutate(
      CI_FCTR_VEC_BY_CNT = ci_fctr_vec(
        !!rlang::sym(measure_set$exposures),
        !!rlang::sym(measure_set$variances)
      )
    )
  expect_identical(
    x$CI_FCTR_BY_CNT,
    x$CI_FCTR_VEC_BY_CNT,
  )
})

test_that('measure set version `ae_ratio` equals vector version', {
  measure_set <- list(
    actuals = 'MORT_ACTUAL_CNT',
    expecteds = 'MORT_EXPECTED_CNT',
    exposures = 'MORT_EXPOSURE_CNT',
    variances = 'MORT_VARIANCE_CNT'
  )
  x <- mortexp |>
    summarise_measures(
      measure_sets = list(BY_CNT = measure_set)
    ) |>
    mutate_metrics(
      metrics = list(AE_RATIO = ae_ratio)
    ) |>
    dplyr::mutate(
      AE_RATIO_VEC_BY_CNT = ae_ratio_vec(
        !!rlang::sym(measure_set$actuals),
        !!rlang::sym(measure_set$expecteds)
      )
    )
  expect_identical(
    x$AE_RATIO_BY_CNT,
    x$AE_RATIO_VEC_BY_CNT,
  )
})

test_that('measure set version `credibility` equals vector version', {
  measure_set <- list(
    actuals = 'MORT_ACTUAL_CNT',
    expecteds = 'MORT_EXPECTED_CNT',
    exposures = 'MORT_EXPOSURE_CNT',
    variances = 'MORT_VARIANCE_CNT'
  )
  x <- mortexp |>
    summarise_measures(
      measure_sets = list(BY_CNT = measure_set)
    ) |>
    mutate_metrics(
      metrics = list(CREDIBILITY = credibility)
    ) |>
    dplyr::mutate(
      CREDIBILITY_VEC_BY_CNT = credibility_vec(
        expecteds = !!rlang::sym(measure_set$expecteds),
        variances = !!rlang::sym(measure_set$variances)
      )
    )
  expect_identical(
    x$CREDIBILITY_BY_CNT,
    x$CREDIBILITY_VEC_BY_CNT,
  )
})
