#' Aggregate an experience study
#'
#' `summarise_measures()` functions the same as [dplyr::summarise()] and
#' returns a new data frame per combination of grouping variable. However,
#' this function is is streamlined to return the sum of an experience study's
#' measures instead of any arbitrary summary function. These measures are
#' identified via the `measure_sets` argument which can be provided directly
#' or be guessed using regular expressions (`regexs`). See
#' [guess_measure_sets()] for additional detail on how this guessing is
#' implemented.
#'
#' @section Naming convention:
#'
#' `expstudy` uses a naming convention where some functions are prefixed by the
#' underling `dplyr` verb. The purpose of this is to associate the resulting
#' structure of the `expstudy` function with a very similar output as what the
#' `dplyr` function would produce. Note that the intention here is not replace
#' all `dplyr` use cases but instead add specific functionality to streamline
#' routine experience study analyses.
#'
#' @param .data
#'   A [base::data.frame()] that houses an experience study.
#' @param measure_sets
#'   A (potentially named) list of measure sets. Only need to specify once if
#'   chaining multiple `expstudy` functions as the `measure_sets` will be
#'   passed as an attribute in results.
#'
#' @inheritParams dplyr::summarise
#' @inheritParams base::sum
#'
#' @inherit dplyr::summarise return
#'
#' @export
#'
#' @examples
#' mortexp |>
#'   dplyr::group_by(
#'     UNDERWRITING_CLASS
#'   ) |>
#'   summarise_measures()
summarise_measures <- function(
    .data,
    measure_sets = guess_measure_sets(.data),
    na.rm = TRUE,
    .groups = 'drop',
    .by = NULL
) {
  summarise_measures_error_call <- rlang::current_env()

  if (!missing(.by) && missing(.groups)) {
    .groups <- NULL
  }

  if (missing(measure_sets) && !is.null(attr(.data, 'measure_sets'))) {
    measure_sets <- attr(.data, 'measure_sets')
  } else {
    validate_measure_sets(
      x = measure_sets,
      data = .data,
      data_arg = rlang::caller_arg(.data),
      error_call = summarise_measures_error_call
    )
  }

  structure(
    dplyr::summarise(
      .data = .data,
      dplyr::across(
        .cols = dplyr::all_of(unlist(use.names = FALSE, measure_sets)),
        .fns = \(x) sum(x, na.rm = na.rm)
      ),
      .groups = .groups,
      .by = {{ .by }}
    ),
    measure_sets = measure_sets
  )
}
