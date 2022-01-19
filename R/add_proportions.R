#' Add Proportions
#'
#' @description
#' Add proportions of `expstudy` metric variables. Other variables can be
#' provided via the <[`dynamic-dots`][rlang::dyn-dots]> argument if proportions
#' are wanted as well. Proportions will sum to 100% per group if `.base_grp_nms`
#' are provided. If `.base_grp_nms` are omitted, proportions will sum to 100%
#' in total.
#'
#' @param expstudy
#'   an [expstudy()]
#' @param ...
#'   other columns other than the `expstudy` metric variables (`actuals`,
#'   `expecteds`, and `exposures`) to generate proportions of
#' @param .base_grp_nms
#'   character vector of column names to use as the base of added proportions
#' @param .min_ungrpd
#'   minimum number of non-grouping columns required before using
#'   `.base_grp_nms` as proportion base
#'
#' @return
#'    An `expstudy` with added proportions.
#'
#' @export
add_proportions <- function(
  expstudy,
  ...,
  .base_grp_nms = character(0),
  .min_ungrpd = 0L
) {
  assert_that(inherits(expstudy, 'tbl_es'))

  if (is_missing(...) || is_null(...)) {
    prop_cols <- attr(expstudy, 'metric_vars') %>%
      squash_chr %>%
      unname
  } else {
    prop_cols <- ensyms(...) %>%
      map(
        as_string
      ) %>%
      list_merge(
        attr(expstudy, 'metric_vars')
      ) %>%
      squash_chr %>%
      unname
  }

  if (
    ncol(expstudy) - length(.base_grp_nms) - length(prop_cols) < .min_ungrpd
  ) {
    .base_grp_nms <- character(0)
  }

  result <- expstudy %>%
    group_by(
      across(
        any_of(
          !!.base_grp_nms
        )
      )
    ) %>%
    mutate(
      across(
        .cols = all_of(!!prop_cols),
        .fns = ~ .x / sum(.x),
        .names = 'PROP_{.col}'
      )
    )

  prop_metrics <- setdiff(
    colnames(result) %||% result$vars,
    colnames(expstudy) %||% expstudy$vars
  )

  attr(result, 'metrics_applied') <- list_merge(
      attr(expstudy, 'metrics_applied'),
      name = prop_metrics,
      format = rep('percent', length(prop_metrics))
  )
  return(result)
}
