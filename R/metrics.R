#' Experience study metrics
#'
#' A collection of common metrics used in an actuarial environment are provided.
#' Two versions of each metric functions have been developed: one where it takes
#' a measure set for an experience study as its primary argument, and one where
#' vectors can be provided instead.
#'
#' Metric functions that use a measure set as its primary argument are intended
#' to be used with [mutate_metrics()] and return a (`quosure`)[rlang::quo()].
#' Use the vector versions (those ending in `_vec`) if instead a numeric vector
#' result is desired.
#'
#' @param measure_set
#'   A named character vector or list with each element mapping a column in
#'   the experience study to one of the following measures: `actuals`,
#'   `expecteds`, `exposures`, or `variances`.
#' @param actuals,expecteds,exposures,variances
#'   Columns in experience study that correspond to individual measures for
#'   vector versions of metric functions.
#' @param ... Not used directly and be left blank.
#'
#' @returns
#'   Measure set versions return a (`quosure`)[rlang::quo()] to be evaluated in
#'   [mutate_metrics()]. Vector versions numeric vector
#'   of the same length of measures used in the calculation per group (if
#'   grouping applied).
#'
#' @name metrics
NULL

#' @describeIn
#'   metrics
#'   Calculates the average actual decrements observed per unit of exposure.
#' @export
avg_observed <- function(measure_set, ...) {
  rlang::check_dots_empty()
  rlang::quo(
    !!rlang::sym(measure_set$actuals) / !!rlang::sym(measure_set$exposures)
  )
}
#' @rdname metrics
#' @export
avg_observed_vec <- function(actuals, exposures, ...) {
  rlang::check_dots_empty()
  actuals / exposures
}

#' @describeIn
#'   metrics
#'   Calculates the average expected decrements per unit of exposure.
#' @export
avg_expected <- function(measure_set, ...) {
  rlang::check_dots_empty()
  rlang::quo(
    !!rlang::sym(measure_set$expecteds) / !!rlang::sym(measure_set$exposures)
  )
}
#' @rdname metrics
#' @export
avg_expected_vec <- function(expecteds, exposures, ...) {
  rlang::check_dots_empty()
  expecteds / exposures
}

#' @param se_conf
#'   A number between 0 and 1 corresponding to the confidence level surrounding
#'   the standard error calculation.
#' @param two_tailed
#'   A boolean indicating whether or not a two-tailed hypothesis test should be
#'   utilized.
#' @describeIn
#'   metrics
#'   Calculates the additive factor which constructs a confidence interval
#'   around the expected decrement rate for a given level of confidence.
#' @export
ci_fctr <- function(
    measure_set,
    se_conf = 0.95,
    two_tailed = TRUE,
    ...
) {
  rlang::check_dots_empty()

  if (two_tailed) {
    se_zed <- stats::qnorm((1 + se_conf) / 2)
  } else {
    se_zed <- stats::qnorm(se_conf)
  }
  rlang::quo(
    (
      se_zed * (sqrt(!!rlang::sym(measure_set$variances)))
    ) / (
      !!rlang::sym(measure_set$exposures)
    )
  )
}
#' @rdname metrics
#' @export
ci_fctr_vec <- function(
    exposures,
    variances,
    se_conf = 0.95,
    two_tailed = TRUE,
    ...
) {
  rlang::check_dots_empty()
  if (two_tailed) {
    se_zed <- stats::qnorm((1 + se_conf) / 2)
  } else {
    se_zed <- stats::qnorm(se_conf)
  }
  (se_zed * (sqrt(variances))) / exposures
}

#' @describeIn
#'   metrics
#'   Calculates the ratio of actual decrements to expected decrements, also
#'   referred to as the AE ratio.
#' @export
ae_ratio <- function(measure_set, ...) {
  rlang::check_dots_empty()
  rlang::quo(
    !!rlang::sym(measure_set$actuals) / !!rlang::sym(measure_set$expecteds)
  )
}
#' @rdname metrics
#' @export
ae_ratio_vec <- function(actuals, expecteds, ...) {
  rlang::check_dots_empty()
  actuals / expecteds
}

#' @param distance_from_mean
#'   A number between 0 and 1 representing the precision of the credibility
#'   estimate.
#' @param cred_conf
#'   A number between 0 and 1 corresponding to the confidence level surrounding
#'   the credibility calculation.
#' @describeIn
#'   metrics
#'   Calculates the credibility score according to limited fluctuation
#'   credibility theory.
#' @export
credibility <- function(
    measure_set,
    distance_from_mean = 0.05,
    cred_conf = 0.95,
    ...
) {
  rlang::check_dots_empty()
  rlang::quo(
    pmin(
      1,
      (
        distance_from_mean * !!rlang::sym(measure_set$expecteds)
      ) / (
        stats::qnorm((1 + cred_conf) / 2) *
          sqrt(!!rlang::sym(measure_set$variances))
      )
    )
  )
}
#' @rdname metrics
#' @export
credibility_vec <- function(
    expecteds,
    variances,
    distance_from_mean = 0.05,
    cred_conf = 0.95,
    ...
) {
  rlang::check_dots_empty()
  pmin(
      1,
      (distance_from_mean * expecteds) /
        (stats::qnorm((1 + cred_conf) / 2) * sqrt(variances))
    )
}
