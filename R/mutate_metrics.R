#' Add common metrics to an experience study
#'
#' `mutate_metrics()` calculates metrics for an experience study using common
#' measures associated with the data. These measures are identified via the
#' `measure_sets` argument which can be provided directly or be guessed using
#' regular expressions (`regexs`). See [`guess_measure_sets()`] for additional
#' detail on how this guessing is implemented.
#'
#' This function is structured in a way that uses sets of measures within the
#' study as the first function argument of each metric function. The default
#' argument uses a set of metric functions, provided by `expstudy`, which are
#' commonly requested metrics used in actuarial analyses. For convenience,
#' a vectorized version of these default metric functions have also been
#' provided; see [metrics] for more information.
#'
#' @param metrics
#'   A named list of functions to calculate [metrics]. Each function will be
#'   applied to each set identified in `measure_sets`.
#' @param ...
#'   Additional (optional) arguments passed along to each
#'   (metric function)[metrics].
#' @inheritParams summarise_measures
#' @inheritParams dplyr::mutate
#' @inheritSection summarise_measures Naming convention
#' @inherit dplyr::mutate return
#'
#' @examples
#' # Metrics can be added at a seriatim level, but often are
#' # calculated after some aggregation is applied to a cohort:
#' mortexp |>
#'   dplyr::group_by(
#'     GENDER
#'   ) |>
#'   summarise_measures() |>
#'   mutate_metrics()
#'
#' @export
mutate_metrics <- function(
    .data,
    measure_sets = guess_measure_sets(.data),
    metrics = list(
      AVG_OBSRV = avg_observed,
      AVG_EXPEC = avg_expected,
      CI_FCTR = ci_fctr,
      AE_RATIO = ae_ratio,
      CREDIBILITY = credibility
    ),
    ...,
    .by = NULL,
    .keep = c("all", "used", "unused", "none"),
    .before = NULL,
    .after = NULL
) {
  mutate_metrics_error_call <- rlang::current_env()

  if (missing(measure_sets) && !is.null(attr(.data, 'measure_sets'))) {
    measure_sets <- attr(.data, 'measure_sets')
  } else {
    validate_measure_sets(
      x = measure_sets,
      data = .data,
      data_arg = rlang::caller_arg(.data),
      error_call = mutate_metrics_error_call
    )
  }

  mutate_quos <- unlist(lapply(
    seq_along(measure_sets),
    \(i) {
      lapply(
        seq_along(metrics),
        \(j) {
          stats::setNames(
            list(metrics[[j]](as.list(measure_sets[[i]]), ...)),
            nm =  paste(
              toupper(names(metrics)[j]),
              ifelse(is.null(names(measure_sets[i])),i,names(measure_sets[i])),
              sep = '_'
            )
          )
        }
      )
    }
  ))

  structure(
    dplyr::mutate(
      .data = .data,
      !!!mutate_quos,
      .by = {{ .by }},
      .keep = {{ .keep }},
      .before = {{ .before }},
      .after = {{ .after }}
    ),
    measure_sets = measure_sets
  )
}
