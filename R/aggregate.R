#' Aggregate an `expstudy`
#'
#' @description
#' Often an `expstudy` needs to be aggregated according to different variables
#' for analysis. This function provides easy aggregation of the `expstudy`
#' metric variables `exposures`, `expected`, and `actuals`.
#'
#' Grouping can be performed by passing variables as unnamed arguments. If no
#' groups are specified, the grand total of the `expstudy` metric variables will
#' be returned.
#'
#' By default, only the metric variables are summed and returned. Other
#' variables can also be totaled by using the `.oth_sum_vars` to specify.
#'
#' @param expstudy
#'   an [expstudy()]
#' @param ...
#'   variables to use as groups
#' @param .oth_sum_vars
#'   additional variable to total. Multiple variables can be provided using
#'   [c()].
#'
#' @return
#'   an aggregated `expstudy`.
#'
#' @seealso [expstudy()] [tbl_es]
#'
#' @export
aggregate <- function(
  expstudy,
  ...,
  .oth_sum_vars = NULL
) {
  assert_that(inherits(expstudy, 'tbl_es'))

  metric_var_syms <- attr(expstudy, 'metric_vars') %>%
    reduce(c) %>%
    syms

  expstudy %>%
    group_by(
      ...
    ) %>%
    summarise(
      across(c(!!!metric_var_syms, {{ .oth_sum_vars }}), sum),
      .groups = 'drop'
    )
}
