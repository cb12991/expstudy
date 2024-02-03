#' Guess a measure set using regexs
#'
#' Attempt to guess the names of a **measure set** using regular expressions
#' (or regexs).
#'
#' @param data
#'   A [`data.frame`] that houses an experience study.
#' @param measure_regexs
#'   A named list of patterns to use as regexs when guessing columns in the
#'   study dataset to be used for one study measure in each measure set. There
#'   must be one column for each measure in a measure set (actuals, expecteds,
#'   exposures, and variances). Defaults to
#'    `getOption('expstudy.default_measure_regexs')`.
#' @param measure_set_prefixes,measure_set_suffixes
#'   Character vectors that will be use to differentiate the same measure in
#'   one measure set from another measure set. Using `NULL` indicates that
#'   the study measures do not differ by prefix/suffix and will error if more
#'   than one column is guessed using the measure regex for a single measure.
#'   Defaults to measures sets not differing by prefix
#'   (`measure_set_prefixes = NULL`) but do differ by count and amount
#'   suffixes (`measure_set_prefixes = c('_CNT', '_AMT')`).
#'
#' If the experience study has columns that follow a consistent naming
#' structure, this function can seamlessly provide other `expstudy` functions
#' information on the study measures to use for various calculations.
#'
#' @return
#'   A named list of measure sets that identify common variables used for
#'   `expstudy` analyses.
#'
#' @examples
#' guess_measure_sets(mortexp)
#'
#' @export
guess_measure_sets <- function(
  data,
  measure_regexs = getOption('expstudy.default_measure_regexs'),
  measure_set_prefixes = getOption('expstudy.default_measure_set_prefixes'),
  measure_set_suffixes = getOption('expstudy.default_measure_set_suffixes')
) {
  guess_measure_sets_error_call <- rlang::current_env()

  validate_obj_class(
    x = data,
    ptype = data.frame(),
    arg = rlang::caller_arg(data),
    error_call = guess_measure_sets_error_call
  )
  validate_list_obj_lax(
    x = measure_regexs,
    ptype = getOption('expstudy.default_measure_regexs'),
    error_call = guess_measure_sets_error_call
  )

  for (
    arg in list(measure_set_prefixes, measure_set_suffixes)
  ) {
    force(arg)
    if (!is.null(arg)) {
      validate_obj_type(
        x = arg,
        ptype = character(),
        arg = rlang::caller_arg(arg),
        error_call = guess_measure_sets_error_call
      )
    }
  }
  regexs <- utils::modifyList(
    getOption('expstudy.default_measure_regexs'),
    as.list(measure_regexs)
  )

  zero_len_measure_set_pref <- length(measure_set_prefixes) == 0
  zero_len_measure_set_suf <- length(measure_set_suffixes) == 0

  if (zero_len_measure_set_pref) {
    measure_set_prefixes <- ''
  }
  if (zero_len_measure_set_suf) {
    measure_set_suffixes <- ''
  }

  measure_sets_grid <- expand.grid(
    prefixes = measure_set_prefixes,
    suffixes = measure_set_suffixes,
    stringsAsFactors = FALSE
  )
  measure_sets_iterations <- seq.int(nrow(measure_sets_grid))
  if (!zero_len_measure_set_pref & !zero_len_measure_set_pref) {
    names(measure_sets_iterations) <- paste0(
      measure_sets_grid$prefixes,
      measure_sets_grid$suffixes
    )
  } else if (!zero_len_measure_set_pref) {
    names(measure_sets_iterations) <- measure_sets_grid$prefixes
  } else if (!zero_len_measure_set_suf) {
    names(measure_sets_iterations) <- measure_sets_grid$suffixes
  }

  lapply(
    measure_sets_iterations,
    \(x) {
      rgxs <- paste0(
        measure_sets_grid$prefixes[[x]],
        unlist(regexs),
        measure_sets_grid$suffixes[[x]]
      )
      names(rgxs) <- names(regexs)

      guesses <- sapply(
          X = rgxs,
          FUN = \(rgx) {
            unique(grep(pattern = rgx, x = colnames(data), value = TRUE))
          }
      )

      validate_guessed_measure_set(
        guesses = guesses,
        regexs = rgxs,
        error_call = guess_measure_sets_error_call
      )
    }
  )
}

