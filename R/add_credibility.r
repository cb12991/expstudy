#' Add Credibility Factors
#'
#' @description
#' Add credibility factors for an `expstudy`'s `expecteds` metric variable.
#' The credibility calculation uses a classical credibility approach
#' also known as limited fluctuation partial credibility. Under this approach,
#' the credibility factor is calculated so that `actuals` are within \eqn{k}\%
#' of `expecteds` with probability \eqn{p}.
#'
#' Credibility range parameter \eqn{k} and probability level \eqn{p} are set
#' using the function arguments `.cred_k` and `.cred_p`, respectively.
#'
#' @param expstudy
#'   an [expstudy()]
#' @param .cred_k
#'   number within range (0, 1); range parameter of credibility equation
#' @param .cred_p
#'   number within range (0, 1); probability parameter of credibility equation
#' @param .cred_nms
#'   character vector of column names for the added credibility column. If more
#'   than one credibility column will be created, you can distinguish them here.
#'
#' @return
#'   An `expstudy` with added credibility factors.
#'
#' @export
add_credibility <- function(
  expstudy,
  .cred_k = 0.05,
  .cred_p = 0.95,
  .cred_nms = 'CREDIBILITY'
) {
  assert_that(inherits(expstudy, 'tbl_es'))

  exp_cols <- attr(expstudy, 'metric_vars')[['expected']]
  var_cols <- attr(expstudy, 'metric_vars')[['variances']]

  if (is.null(var_cols)) {
    abort(
      'error_cannot_compute',
      message = c(
        'No Variance Variable',
        x = str_wrap(
          paste(
            'No variance variable declared for this experience study object,',
            'so credibility metric cannot be computed.'
          )
        ),
        data = expstudy,
        metric_vars = attr(expstudy, 'metric_vars')
      )
    )
  }
  z_p <- stats::qnorm((1 + .cred_p)/2)

  cred_cols <- set_names(list(), character())
  metrics_applied <- attr(expstudy, 'metrics_applied')

  if (max(length(exp_cols), length(var_cols)) > length(.cred_nms)) {
    .cred_nms <- paste(
      .cred_nms,
      seq.int(
        max(
          length(exp_cols),
          length(var_cols)
        )
      ),
      sep = '_'
    )
  }
  metrics_applied <- metrics_applied %>%
    list_merge(
      name = .cred_nms,
      format = 'percent'
    )

  cred_cols <- map2(
    .x = exp_cols,
    .y = var_cols,
    .f = ~ parse_quo(
      glue(
        'pmin(1, {.cred_k} * ', .x, '/', 'sqrt({z_p} * ', .y, ')'
      ),
      env = as_environment(result)
    )
  ) %>%
    structure(
      names = .cred_nms,
      class = c('quosures', 'list')
    )

  expstudy %>% mutate(!!!cred_cols)
}
