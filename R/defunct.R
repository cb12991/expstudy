# nocov start

#' Defunct functions
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' These functions were part of the initial release of `expstudy` but are now
#' defunct. If there's a known replacement, calling the function
#' will tell you about it.
#'
#' @keywords internal
#' @name defunct
NULL

#' @usage # Deprecated in 2.0.0 -------------------------------------
#' @name defunct
NULL

#' @export
#' @rdname defunct
add_proportions <- function(
    expstudy,
    ...,
    .base_grp_nms = character(0),
    .min_ungrpd = 0L
) {
  lifecycle::deprecate_stop('2.0.0', 'add_proportions()')
}

#' @export
#' @rdname defunct
add_metrics <- function(
    expstudy,
    ...,
    .metrics = c('act2expec', 'act2expos', 'expec2expos'),
    .metric_nms = list(
      act2expec = 'ACTUAL_TO_EXPECTED',
      act2expos = 'ACTUAL_TO_EXPOSED',
      expec2expos = 'EXPECTED_TO_EXPOSED'
    )
) {
  lifecycle::deprecate_stop('2.0.0', 'add_metrics()', 'mutate_metrics()')
}

#' @export
#' @rdname defunct
add_credibility <- function(
    expstudy,
    .cred_k = 0.05,
    .cred_p = 0.95,
    .cred_nms = 'CREDIBILITY'
) {
  lifecycle::deprecate_stop('2.0.0', 'add_credibility()', 'mutate_metrics()')
}

#' @export
#' @rdname defunct
compile_results <- function(
    expstudy,
    ...,
    output = c('metrics', 'proportions'),
    output_args = list(
      metrics = NULL,
      proportions = NULL
    ),
    output_format = c('unformatted', 'formatted')
) {
  lifecycle::deprecate_stop('2.0.0', 'compile_results()')
}

#' @export
#' @rdname defunct
aggregate <- function(
    expstudy,
    ...,
    .oth_sum_vars = NULL
) {
  lifecycle::deprecate_stop('2.0.0', 'aggregate()', 'summarise_measures()')
}

#' @export
#' @rdname defunct
expstudy <- function(
    data,
    actuals,
    expecteds,
    exposures,
    variances = NULL,
    keys = NULL
) {
  lifecycle::deprecate_stop('2.0.0', 'expstudy()')
}

#' @export
#' @rdname defunct
format_metrics <- function(expstudy) {
  lifecycle::deprecate_stop('2.0.0', 'add_proportions()')
}

# nocov end
