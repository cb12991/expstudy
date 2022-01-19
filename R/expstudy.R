#' Create an expstudy object
#'
#' @description
#' `expstudy()` creates a subclass, [`tbl_es`], of a [dtplyr::lazy_dt()], that
#' stores attributes relating to an experience study.These attributes provide
#' other package functions arguments which reduce time needed to review an
#' assumption.
#'
#' @param data
#'   the dataset of an experience study. Can be any kind of organized data
#'   (e.g., [base::data.frame()], [tibble::tibble()], etc.) but must be able to
#'   be converted to a [data.table::data.table()].
#' @param actuals
#'   the observed variable (or variables) within the experience study (e.g.,
#'   actual lapse counts for a lapse study).
#' @param expecteds
#'   the expected variable (or variables) within the experience study (e.g.,
#'   actual deaths for a mortality study).
#' @param exposures
#'   the exposure variable (or variables) within the experience study. This will
#'   be the time the records spans in years (or within (0, 1) for records
#'   covering less than a year).
#' @param variances
#'   the variance variable (or variables) within the experience study. This may
#'   not be available for all studies, thus can be omitted. Variance is used for
#'   credibility calculations.
#' @param keys
#'   variables that uniquely describe every record within the experience study.
#'   This typically is policy number and a measure of time, such as annual or
#'   monthly duration. Can be omitted, but providing enables quicker binary
#'   searches within the [dtplyr::lazy_dt()]. For more information, refer to the
#'   [data.table] vignette:
#'   `vignette('datatable-keys-fast-subset', package = 'data.table')`.
#'
#' @return
#'   an `expstudy`
#'
#' @export
expstudy <- function(
  data,
  actuals,
  expecteds,
  exposures,
  variances = NULL,
  keys = NULL
) {
  metric_vars <- list(
    actuals = colnames(select(data, {{ actuals }})),
    expecteds = colnames(select(data, {{ expecteds }})),
    exposures = colnames(select(data, {{ exposures }}))
  )

  if (!quo_is_null(enquo(variances))) {
    metric_vars[['variances']] <- colnames(select(data, {{ variances }}))
  }

  non_numerics <- data %>%
    slice_head %>%
    select(
      {{ actuals }},
      {{ expecteds }},
      {{ exposures }},
      {{ variances }}
    ) %>%
    collect %>%
    map_df(
      mode
    ) %>%
    pivot_longer(
      cols = everything()
    ) %>%
    filter(
      .data$value != 'numeric'
    )

  if (nrow(non_numerics) > 0) {
    abort(
      'error_var_not_numeric',
      message = c(
        'Non-Numeric Variable(s)',
        i = 'Experience study metric variables must all be numeric.',
        x = paste(
          'Variable(s) ',
          paste(non_numerics$name, collapse = ', '),
          'is(are) of type(s)',
          paste(non_numerics$value, collapse = ', '),
          'respectively.'
        )
      ),
      data = data,
      metric_vars = metric_vars,
      non_numerics = non_numerics
    )
  }

  result <- lazy_dt(data, key_by = {{ keys }})

  return(
    structure(
      .Data = result,
      class = c('tbl_es', class(result)),
      metric_vars = metric_vars,
      metrics_applied = list(name = character(0), format = character(0))
    )
  )
}
