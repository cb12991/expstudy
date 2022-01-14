#' Compile resulting analyses
#'
#' @description
#' For a streamlined approach, this function combines multiple other
#' [expstudy-package] functions to generate multiple analyses simultaneously.
#' All grouping combinations will be generated from variables passed as
#' <[`dynamic-dots`][rlang::dyn-dots]>, then analytics will be created for each.
#'
#' Resulting output can have the typical metrics via [add_metrics()], or
#' proportions via [add_proportions()]. Default parameters are used for each
#' function, however, you can pass additional parameters in a list to the
#' corresponding function element via the `output_args` argument.
#'
#' Furthermore, you can choose to have the results return unformatted (for
#' further calculations or analysis) or formatted (for presentation purposes).
#'
#' This function was meant to provide quick results for routine analysis. Any
#' additional in-depth analyses should make use of individual `expstudy`
#' functions instead.
#'
#' @param expstudy
#'   an [expstudy()]
#' @param ...
#'   variables to generate grouped analyses. All combinations of variables
#'   provided will be generated and used as groups in results.
#' @param output
#'   type of result to output (i.e., with metrics or proportions). If omitted,
#'   both will be generated and returned in a list.
#' @param output_args
#'   additional arguments to pass to [add_metrics()] and [add_proportions()]. If
#'   omitted, default parameters will be used for both functions.
#' @param output_format
#'   resulting output format. If omitted, both will be generated and returned as
#'   a list.
#'
#' @return
#'   a (potentially nested) list with resulting analysis according to arguments
#'   passed to the function.
#'
#' @seealso
#'   [aggregate()] [add_metrics()] [add_proportions()] [format_metrics()]
#'
#' @export
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
  assert_that(inherits(expstudy, 'tbl_es'))

  output <- match.arg(output, several.ok = TRUE)
  output_format <- match.arg(output_format, several.ok = TRUE)
  groups <- enquos(...)

  if (!is_empty(groups)) {
    group_combos <- map(
      .x = 1:length(groups),
      .f = ~ combn(map_chr(groups, as_name), .x, simplify = FALSE)
    ) %>%
      squash %>%
      map(
        .f = syms
      ) %>%
      append(
        values = list(
          list(
            NULL
          )
        ),
        after = 0
      )
  }

  nms <- group_combos %>%
    modify_depth(
      .depth = 2,
      .f = ~ as_string(.x %||% sym('AGGREGATE'))
    ) %>%
    map(
      .f = ~ .x %>%
        squash_chr %>%
        glue_collapse(
          x = .,
          sep = ', ',
          last = ifelse(length(.) > 2, ', AND ', ' AND ')
        ) %>%
        paste0(
          ifelse(. == 'AGGREGATE', '', 'BY '),
          .
        )
    )

  base <-  map(
    .x = group_combos,
    .f = ~ expstudy %>%
      aggregate(!!!.x) %>%
      collect
  ) %>%
    set_names(nms)

  nested_result <- set_names(list(), character(0))
  if ('metrics' %in% output) {
    nested_result <- nested_result %>%
      append(
        list(
          METRICS = base %>%
            map(
              .f = ~ eval(
                call_modify(
                  call2(
                    add_metrics,
                    expstudy = .x
                  ),
                  !!!output_args[['metrics']]
                )
              ) %>%
                collect
            )
        )
      )
  }

  if ('proportions' %in% output) {
   nested_result <- nested_result %>%
      append(
        list(
          PROPORTIONS = base %>%
            map(
              .f = ~ eval(
                call_modify(
                  call2(
                    add_proportions,
                    expstudy = .x
                  ),
                  !!!output_args[['proportions']]
                )
              ) %>%
                collect
            )
        )
      )
  }

  result <- set_names(list(), character(0))
  if ('unformatted' %in% output_format) {
    result <- result %>%
      append(
        list(
          UNFORMATTED = nested_result
        )
      )
  }

  if ('formatted' %in% output_format) {
    result <- result %>%
      append(
        list(
          FORMATTED = nested_result %>%
            modify_depth(
              .depth = 2,
              .f = format_metrics
            )
        )
      )
  }
  return(result)
}
