test_that('measure_sets passed as attribute in results', {
  expect_identical(
    attr(
      mutate_expecvar(
        mortexp |> dplyr::mutate(NEW_RT = runif(nrow(mortexp))),
        new_expected_rates = NEW_RT,
        amount_scalar = FACE_AMOUNT
      ),
      'measure_sets'
    ),
    guess_measure_sets(mortexp)
  )
})
