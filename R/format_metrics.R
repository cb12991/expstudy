#' Format `expstudy` metrics
#'
#' Easily format metrics and metric variables for better readability.
#'
#' @param expstudy
#'   an `expstudy` object
#'
#' @return
#'   the same `expstudy` with formatted metrics
#'
#' @examples
#'   es <- expstudy(
#'     data = mortexp,
#'     actuals = ACTUAL_DEATHS,
#'     expecteds = EXPECTED_DEATHS,
#'     exposures =  EXPOSURE,
#'     variances = VARIANCE_DEATHS
#'   )
#'
#'   # Unformatted result:
#'   es %>%
#'     aggregate(
#'       GENDER,
#'       UNDERWRITING_CLASS
#'     ) %>%
#'     add_proportions %>%
#'     add_metrics %>%
#'     add_credibility
#'
#'   # Formatted result:
#'   es %>%
#'     aggregate(
#'       GENDER,
#'       UNDERWRITING_CLASS
#'     ) %>%
#'     add_proportions %>%
#'     add_metrics %>%
#'     add_credibility %>%
#'     format_metrics
#'
#' @export
format_metrics <- function(expstudy) {
  assert_that(inherits(expstudy, 'tbl_es'))

  met_vars <- expstudy %>%
    attr('metric_vars') %>%
    squash_chr

  mets_app <- expstudy %>%
    attr('metrics_applied') %>%
    as_tibble

  numeric_vars <- mets_app %>%
    filter(
      .data$format == 'numeric'
    ) %>%
    pull(
      .data$name
    ) %>%
    c(met_vars) %>%
    unname

  percent_vars <- mets_app %>%
    filter(
      .data$format == 'percent'
    ) %>%
    pull(
      .data$name
    ) %>%
    unname

  expstudy %>%
    mutate(
      across(
        .cols = all_of(!!numeric_vars),
        .fns = formatC,
        format = 'f',
        big.mark = ',',
        digits = 2
      ),
      across(
        .cols = all_of(!!percent_vars),
        .fns = ~ paste0(round(.x * 100, 2), '%')
      )
    )
}
