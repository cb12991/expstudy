test_that('ungrouped, unweighted factor adjustments compute', {
  expect_no_error(
    mortexp |>
      compute_fct_adjs(
        expected_rate = EXPECTED_MORTALITY_RT,
        amount_scalar = FACE_AMOUNT
      )
  )
})

test_that('ungrouped, weighted factor adjustments compute', {
  expect_no_error(
    mortexp |>
      compute_fct_adjs(
        expected_rate = EXPECTED_MORTALITY_RT,
        amount_scalar = FACE_AMOUNT,
        cred_wt_adjs = TRUE
      )
  )
})

test_that('grouped, unweighted factor adjustments compute', {
  expect_no_error(
    mortexp |>
      dplyr::group_by(
        GENDER,
        SMOKING_STATUS
      ) |>
      compute_fct_adjs(
        expected_rate = EXPECTED_MORTALITY_RT,
        amount_scalar = FACE_AMOUNT
      )
  )
})

test_that('grouped, cred-weighted, unbalanced factor adjustments compute', {
  expect_no_error(
    mortexp |>
      dplyr::group_by(
        GENDER,
        SMOKING_STATUS
      ) |>
      compute_fct_adjs(
        expected_rate = EXPECTED_MORTALITY_RT,
        amount_scalar = FACE_AMOUNT,
        cred_wt_adjs = TRUE
      )
  )
})

test_that('grouped, cred-weighted, balanced factor adjustments compute', {
  expect_no_error(
    mortexp |>
      dplyr::group_by(
        GENDER,
        SMOKING_STATUS
      ) |>
      compute_fct_adjs(
        expected_rate = EXPECTED_MORTALITY_RT,
        amount_scalar = FACE_AMOUNT,
        cred_wt_adjs = TRUE,
        balance_adjs = TRUE
      )
  )
})

test_that('ungrouped weighted & unweighted adjs differ', {
  expect_not_identical(
    mortexp |>
      compute_fct_adjs(
        expected_rate = EXPECTED_MORTALITY_RT,
        amount_scalar = FACE_AMOUNT,
        cred_wt_adjs = FALSE
      ),
    mortexp |>
      compute_fct_adjs(
        expected_rate = EXPECTED_MORTALITY_RT,
        amount_scalar = FACE_AMOUNT,
        cred_wt_adjs = TRUE
      ),
    label = 'ungrouped, unweighted adjustment factors',
    expected.label = 'ungrouped, cred-weighted adjustment factors'
  )
})

test_that('weighted & unweighted adjs for the same groups differ', {
  expect_not_identical(
    mortexp |>
      dplyr::group_by(
        UNDERWRITING_CLASS
      ) |>
      compute_fct_adjs(
        expected_rate = EXPECTED_MORTALITY_RT,
        amount_scalar = FACE_AMOUNT,
        cred_wt_adjs = FALSE
      ),
    mortexp |>
      dplyr::group_by(
        UNDERWRITING_CLASS
      ) |>
      compute_fct_adjs(
        expected_rate = EXPECTED_MORTALITY_RT,
        amount_scalar = FACE_AMOUNT,
        cred_wt_adjs = TRUE
      ),
    label = 'grouped, unweighted adjustment factors',
    expected.label = 'grouped, cred-weighted adjustment factors'
  )
})

test_that('ungrouped & weighted balanced adjs differ from unbalanced', {
  expect_not_identical(
    mortexp |>
      compute_fct_adjs(
        expected_rate = EXPECTED_MORTALITY_RT,
        amount_scalar = FACE_AMOUNT,
        cred_wt_adjs = TRUE,
        balance_adjs = FALSE
      ),
    mortexp |>
      compute_fct_adjs(
        expected_rate = EXPECTED_MORTALITY_RT,
        amount_scalar = FACE_AMOUNT,
        cred_wt_adjs = TRUE,
        balance_adjs = TRUE
      ),
    label = 'ungrouped, cred-weighted, unbalanced adjustment factors',
    expected.label = 'ungrouped, cred-weighted, balanced adjustment factors'
  )
})

test_that('grouped & weighted balanced adjs differ from unbalanced', {
  expect_not_identical(
    mortexp |>
      dplyr::group_by(
        UNDERWRITING_CLASS
      ) |>
      compute_fct_adjs(
        expected_rate = EXPECTED_MORTALITY_RT,
        amount_scalar = FACE_AMOUNT,
        cred_wt_adjs = TRUE,
        balance_adjs = FALSE
      ),
    mortexp |>
      dplyr::group_by(
        UNDERWRITING_CLASS
      ) |>
      compute_fct_adjs(
        expected_rate = EXPECTED_MORTALITY_RT,
        amount_scalar = FACE_AMOUNT,
        cred_wt_adjs = TRUE,
        balance_adjs = TRUE
      ),
    label = 'grouped, cred-weighted, unbalanced adjustment factors',
    expected.label = 'grouped, cred-weighted, balanced adjustment factors'
  )
})
