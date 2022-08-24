#' Add Metrics
#'
#' @description
#' Add commonly used metrics to an [expstudy()]. This typically would be after
#' grouping by variables of interest, but doesn't have to be. The following
#' metrics are available by default and use the `expstudy`'s metric variables
#' (`actuals`, `expecteds`, and`exposures`).
#'
#'   * `'act2expec'`: actuals to expecteds (i.e., A/E ratios)
#'   * `'act2expos'`: actuals to exposures (i.e., average actually observed)
#'   * `'expec2expos'`: expecteds to exposures (i.e., average expected)
#'
#' You can also create custom metrics to add by providing (optionally named)
#' metric formulae.
#'
#' @param expstudy
#'   an [expstudy()]
#' @param ...
#'   additional metrics to add. Can be name-value pairs or simply the
#'   metric's formula.
#' @param .metrics
#'   default methods to add; if omitted, all metrics listed will
#'   be added
#' @param .metric_nms
#'   pairlist of .metric names; can be character vector for
#'   `expstudy` object with multiple columns per `.metrics` argument
#'
#' @return
#'    An `expstudy` object with added metrics. See [expstudy()] for
#'    additional detail on `expstudy` objects.
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
#'   # If no arguments are provided, all default metrics will be generated. This
#'   # mostly makes sense for already aggregated expstudy objects, but can be
#'   # used with unaggregated objects as well.
#'    es %>%
#'      aggregate(ATTAINED_AGE) %>%
#'      add_metrics
#'
#'    es %>%
#'      aggregate(
#'        UNDERWRITING_CLASS,
#'        GENDER,
#'        SMOKING_STATUS
#'      ) %>%
#'      add_metrics
#'
#' @export
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
  assert_that(inherits(expstudy, 'tbl_es'))

  metric_vars <- attr(expstudy, 'metric_vars')

  .metrics <- match.arg(.metrics, several.ok = TRUE)

  oth_metrics <- quos_auto_name(enquos(...)) %>%
    lmap_if(
      .p = quo_is_null,
      .f = set_names,
      nm = NULL
    ) %>%
    discard(quo_is_null) %>%
    as_quosures

  metric_cols <- set_names(list(), character())

  metrics_applied <- attr(expstudy, 'metrics_applied')

  if ('act2expec' %in% .metrics) {
    if (
      max(
        length(metric_vars['actuals']),
        length(metric_vars['expecteds'])
      ) > length(.metric_nms[['act2expec']])
    ) {
      .metric_nms[['act2expec']] <- paste(
        .metric_nms[['act2expec']],
        seq.int(
          max(
            length(metric_vars['actuals']),
            length(metric_vars['expecteds'])
          )
        ),
        sep = '_'
      )
    }
    metrics_applied <- metrics_applied %>%
      list_merge(
        name = .metric_nms[['act2expec']],
        format = 'percent'
      )

    metric_cols <- c(
      metric_cols,
      map2(
        .x = metric_vars['actuals'],
        .y = metric_vars['expecteds'],
        .f = ~ parse_expr(paste0(.x, '/', .y))
      ) %>%
        structure(
          names = .metric_nms[['act2expec']],
          class = c('quosures', 'list')
        )
    )
  }
  if ('act2expos' %in% .metrics) {
    if (
      max(
        length(metric_vars['actuals']),
        length(metric_vars['exposures'])
      ) > length(.metric_nms[['act2expos']])
    ) {
      .metric_nms[['act2expos']] <- paste(
        .metric_nms[['act2expos']],
        seq.int(
          max(
            length(metric_vars['actuals']),
            length(metric_vars['exposures'])
          )
        ),
        sep = '_'
      )
    }
    metrics_applied <- metrics_applied %>%
      list_merge(
        name = .metric_nms[['act2expos']],
        format = 'percent'
      )

    metric_cols <- c(
      metric_cols,
      map2(
        .x = metric_vars['actuals'],
        .y = metric_vars['exposures'],
        .f = ~ parse_expr(paste0(.x, '/', .y))
      ) %>%
        structure(
          names = .metric_nms['act2expos'],
          class = c('quosures', 'list')
        )
    )
  }
  if ('expec2expos' %in% .metrics) {
    if (
      max(
        length(metric_vars['expecteds']),
        length(metric_vars['exposures'])
      ) > length(.metric_nms[['expec2expos']])
    ) {
      .metric_nms[['expec2expos']] <- paste(
        .metric_nms[['expec2expos']],
        seq.int(
          max(
            length(metric_vars['expecteds']),
            length(metric_vars['exposures'])
          )
        ),
        sep = '_'
      )
    }
    metrics_applied <- metrics_applied %>%
      list_merge(
        name = .metric_nms[['expec2expos']],
        format = 'percent'
      )

    metric_cols <- c(
      metric_cols,
      map2(
        .x = metric_vars['expecteds'],
        .y = metric_vars['exposures'],
        .f = ~ parse_expr(paste0(.x, '/', .y))
      ) %>%
        structure(
          names = .metric_nms['expec2expos'],
          class = c('quosures', 'list')
        )
    )
  }
  return(
    structure(
      .Data = expstudy %>% mutate(!!!oth_metrics, !!!metric_cols),
      metrics_applied = metrics_applied
    )
  )
}
