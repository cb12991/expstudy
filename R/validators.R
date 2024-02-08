# nocov start

#' Validation functions used throughout various expstudy functions
#' @noRd

validate_measure_sets <- function(
    x,
    data,
    x_arg = rlang::caller_arg(x),
    data_arg = rlang::caller_arg(data),
    error_call = rlang::caller_env()
) {
  # A valid `measure_sets` object will be a list with each element being a
  # measure set. So, it should have two levels: the outermost level's names
  # will either be NULL or the names of the measure set, if provided. The inner
  # level's names will be the measure names (actuals, expecteds, exposures,
  # and variances).
  # browser()
  # User can provide a single measure set, so will need to make that into a list
  # before proceeding.
  if (
    !is.null(names(x)) &&
    !as.logical(length(setdiff(
      names(x),
      c('actuals', 'expecteds', 'exposures', 'variances')
    ))) &&
    list_depth(x) <= 1
  ) {
    x <- list(x)
  }

  # Now loop through each element in the provided `measure_sets` and validate.
  invisible(
    lapply(
      x,
      \(ms) {
        validate_measure_set(ms, data, x_arg, data_arg, error_call)
      }
    )
  )
}

validate_measure_set <- function(
    x,
    data,
    x_arg = rlang::caller_arg(x),
    data_arg = rlang::caller_arg(data),
    error_call = rlang::caller_env()
) {
  # A valid measure set will have four named elements (actuals, expecteds,
  # exposures, and variances) that identify which columns in the dataset to
  # use to calculate metrics. These identified columns should be in the dataset
  # and be numeric.

  # User could provide a named character vector or named list, so cast as list
  # before proceeding for consistency.
  if (is.atomic(x)) {
    x <- as.list(x)
  }

  # Make sure all measures are provided (element names are 'actuals',
  # 'expecteds', 'exposures', and 'variances') and list values are character
  # vectors.
  validate_list_obj_strict(
    x,
    getOption('expstudy.default_measure_regexs'),
    arg = x_arg,
    error_call = error_call
  )

  # Make sure measures identified in measure set are actually columns within
  # the dataset and of numeric type.
  for (ms in x) {
    validate_data_column(
      ms,
      data,
      numeric(),
      data_arg = data_arg,
      error_call = error_call
    )
  }

  invisible(x)
}

validate_guessed_measure_set <- function(
    guesses,
    regexs,
    error_call = rlang::caller_env()
) {
  too_many_guesses <- guesses[vapply(guesses, \(x) length(x) > 1, TRUE)]
  no_guesses <- names(guesses[vapply(guesses, \(x) length(x) == 0, TRUE)])

  if (as.logical(length(too_many_guesses))) {
    cli::cli_abort(
      'error_guessed_measure_sets',
      message = c(
        'More than one column guessed for a given measure',
        '',
        i = paste(
          'Try using more restrictive regex components so that only one',
          'column matches for a particular measure. For example, use',
          '{.arg measure_set_prefix = \'LAPSE\'} (in addition to default',
          'arguments) to only guess {.var LAPSE_ACTUAL_AMT} if both lapse',
          'and surrender actuals are within the experience study dataset.'
        ),
        '',
        paste(
          'Regexes below should have only matched {.emph one column}, but',
          'instead matched the following:'
        ),
        '',
        sapply(
          seq_along(too_many_guesses),
          \(x) paste0(
            names(too_many_guesses)[[x]],
            cli::cli_fmt(cli::cli_text(':\t{.var {too_many_guesses[[x]]}}'))
          )
        ),
        ''
      ),
      call = error_call
    )
  }

  if (as.logical(length(no_guesses))) {
    cli::cli_abort(
      'error_guessed_measure_sets',
      message = c(
        'No column guessed for a given measure',
        '',
        i = paste(
          'Try updating the regex components to match a column in the dataset.'
        ),
        '',
        paste(
          'No columns in the dataset were matched using the provided regex',
          'for the following measures:'
        ),
        '',
        sapply(
          no_guesses,
          \(x) paste0(
            x,
            cli::cli_fmt(cli::cli_text(':\t{.var {regexs[[x]]}}'))
          ),
          USE.NAMES = FALSE
        ),
        ''
      ),
      call = error_call
    )
  }
  invisible(guesses)
}

validate_data_column <- function(
    x,
    data,
    ptype = NULL,
    data_arg = rlang::caller_arg(data),
    error_call = rlang::caller_env()
) {
  # Check to see if a column in in a data set and (optionally) that the column
  # is of the intended data type.

  if (!x %in% colnames(data)) {
    cli::cli_abort(
      'error_column_not_in_data',
      message = 'There is no {.var {x}} column found in {.arg {data_arg}}.',
      call = error_call
    )
  }

  if (!is.null(ptype) && !inherits(data[[x]], class(ptype))) {
    cli::cli_abort(
      'error_column_type',
      message = paste(
        '{.var {x}} must inherit class {.cls {class(ptype)}} but is class',
        '{.cls {class(data[[x]])}} instead.'
      ),
      call = error_call
    )
  }
}

validate_list_obj_strict <- function(
    x,
    ptype,
    arg = rlang::caller_arg(x),
    error_call = rlang::caller_env()
) {
  # First make sure object validating is actually a list.
  validate_obj_type(x, list(), error_call = error_call)

  # Strict validation requires:
  #   1. length(x) must be equal to length(ptype)
  #   2. names(x) must be equal to names(ptype)
  #   3. typeof(x[[nm]]) must be equal to typeof(ptype[[nm]]) for each nm in
  #     both x and ptype
  #
  # Order of elements should not matter unless elements are unnamed or names
  # are not unique.

  # Validation Step 1 ----
  if (length(x) != length(ptype)) {
    cli::cli_abort(
      'error_bad_list_length',
      message = c(
        'Unexpected list element',
        paste(
          '{.arg {arg}} has {.val {length(x)}} element{?s} but should have',
          '{.val {length(ptype)}} elements{?s} instead.'
        ),
        call = error_call
      )
    )
  }

  # Validation Step 2 ----
  if (!identical(sort(names(x)), sort(names(ptype)))) {
    if (is.null(names(ptype))) {
      msg_x <- '{.arg {arg}} has elements named {.val {names(x)}}'
      msg_target <- 'but all elements should be unnamed.'
    } else if (is.null(names(x))) {
      msg_x <- '{.arg {arg}} has unnamed elements'
      msg_target <- 'but needs to have elements named {.val {names(ptype)}}.'
    } else {
      extra_elements <- setdiff(names(x), names(ptype))
      missing_elements <- setdiff(names(ptype), names(x))
      if (as.logical(length(extra_elements))) {
        msg_x <- paste(
          '{.arg {arg}} has {cli::qty(length(extra_elements))} {?an/}',
          'additional element{?s} named {.val {extra_elements}}'
        )
        msg_target <- paste(
          '{cli::qty(length(names(ptype)))} but can only have {?one/}',
          'element{?s} named {.val {names(ptype)}}.'
        )
      } else {
        msg_x <- paste(
          '{.arg {arg}} is missing required',
          '{cli::qty(length(missing_elements))} element{?s}',
          '{{.val {missing_elements}}.'
        )
        msg_target <- character()
      }
    }
    cli::cli_abort(
      'error_bad_list_element_nm',
      message = paste(msg_x, msg_target),
      call = error_call
    )
  }

  # Validation Step 3 ----
  if (
    is.null(names(x)) ||
    is.null(names(ptype)) ||
    any(duplicated(names(x))) ||
    any(duplicated(names(ptype)))
  ) {
    indx <- seq_along(ptype)
  } else {
    indx <- names(ptype)
  }
  dif_types <- indx[
    vapply(indx, \(i) typeof(x[[i]]) != typeof(ptype[[i]]), TRUE)
  ]
  bad_element_type <- x[dif_types]
  if (as.logical(length(bad_element_type))) {
    cli::cli_abort(
      'error_bad_element_type',
      message = c(
        paste(
          '{cli::qty(length(bad_element_type))} List element{?s} not correct',
          'data type'
        ),
        '',
        paste(
          'Elements of {.arg {arg}} should follow the data type mapping',
          'below:'
        ),
        '',
        stats::setNames(
          sapply(
            X = seq_along(ptype),
            FUN = \(i) {
              if (
                is.null(names(ptype[i])) ||
                names(ptype[i]) == '' ||
                identical(names(ptype[i]), character())
              ) {
                lbl <- paste0('Element ', i, ':')
              } else if (sum(names(ptype) == names(ptype[i])) > 1) {
                lbl <- paste0(names(ptype[i]), '..', i, ':')
              } else {
                lbl <- paste0(names(ptype[i]), ':')
              }
              paste(
                cli::cli_fmt(cli::cli_text('{.var {lbl}}')),
                cli::cli_fmt(cli::cli_text('\t{.cls {class(ptype[[i]])}}')),
                sep = '\t'
              )
            },
            USE.NAMES = FALSE
          ),
          nm = rep(' ', times = length(ptype))
        ),
        '',
        paste(
          '{cli::qty(length(bad_element_type))} However, the following',
          'element{?s} of {.arg {arg}} {?has/have} different type{?s}:'
        ),
        '',
        stats::setNames(
          sapply(
            X = seq_along(bad_element_type),
            FUN = \(i) {
              if (
                is.null(names(bad_element_type[i])) ||
                names(bad_element_type[i]) == '' ||
                identical(names(bad_element_type[i]), character())
              ) {
                lbl <- paste0('Element ', i, ':')
              } else if (
                sum(
                  names(bad_element_type[i]) == names(bad_element_type[i])
                ) > 1
              ) {
                lbl <- paste0(names(bad_element_type[i]), '..', i, ':')
              } else {
                lbl <- paste0(names(bad_element_type[i]), ':')
              }
              paste(
                cli::cli_fmt(cli::cli_text('{.var {lbl}}')),
                cli::cli_fmt(
                  cli::cli_text('\t{.cls {class(bad_element_type[[i]])}}')
                ),
                sep = '\t'
              )
            },
            USE.NAMES = FALSE
          ),
          nm =  rep(' ', times = length(bad_element_type))
        ),
        ''
      ),
      call = error_call
    )
  }
}

validate_list_obj_lax <- function(
    x,
    ptype,
    check_names = TRUE,
    arg = rlang::caller_arg(x),
    error_call = rlang::caller_env()
) {
  # First make sure object validating is actually a list.
  validate_obj_type(x, list(), error_call = error_call)

  # Lax validation requires:
  #   * length(x) be less than or equal to length(ptype)
  #   * names(x) must be one of names(ptype) if `check_names = TRUE`
  #   * typeof(x[[nm]]) must be equal to typeof(ptype[[nm]]) for each nm in x
  #
  # Order of elements should not matter unless elements are unnamed or names
  # are not unique.

  # Validation Step 1 ----
  if (length(x) > length(ptype)) {
    cli::cli_abort(
      'error_bad_list_length',
      message = c(
        'Unexpected list element',
        paste(
          '{.arg {arg}} has {.val {length(x)}} element{?s} but should have',
          '{.val {length(ptype)}} elements{?s} at most.'
        ),
        call = error_call
      )
    )
  }

  # Validation Step 2 ----
  if (check_names) {
    if (is.null(names(ptype)) & !is.null(names(x))) {
      msg_x <- '{.arg {arg}} has elements named {.val {names(x)}}'
      msg_target <- 'but all elements should be unnamed.'
    } else if (is.null(names(x)) & !is.null(names(ptype))) {
      msg_x <- '{.arg {arg}} has unnamed elements'
      msg_target <- 'but should have elements named {.val {names(ptype)}}.'
    } else if (!is.null(names(x)) & !is.null(names(ptype))) {
      extra_elements <- setdiff(names(x), names(ptype))
      if (as.logical(length(extra_elements))) {
        cli::cli_abort(
          'error_bad_list_element_nm',
          message = paste(
            '{.arg {arg}} has {cli::qty(length(extra_elements))} {?an/}',
            'additional element{?s} named {.val {extra_elements}}',
            '{cli::qty(length(names(ptype)))} but can only have {?one/}',
            'element{?s} named {.val {names(ptype)}}.'
          ),
          call = error_call
        )
      }
    }
  }

  # Validation Step 3 ----
  if (
    is.null(names(x)) ||
    is.null(names(ptype)) ||
    any(duplicated(names(x))) ||
    any(duplicated(names(ptype)))
  ) {
    indx <- seq_along(ptype)
  } else {
    indx <- names(ptype)
  }
  dif_types <- indx[
    vapply(indx, \(i) typeof(x[[i]]) != typeof(ptype[[i]]), TRUE)
  ]
  bad_element_type <- x[dif_types]
  if (as.logical(length(bad_element_type))) {
    cli::cli_abort(
      'error_bad_element_type',
      message = c(
        paste(
          '{cli::qty(length(bad_element_type))} List element{?s} not correct',
          'data type'
        ),
        '',
        paste(
          'Elements of {.arg {arg}} should follow the data type mapping',
          'below:'
        ),
        '',
        stats::setNames(
          sapply(
            X = seq_along(ptype[names(x)]),
            FUN = \(i) {
              if (
                is.null(names(ptype[names(x)][i])) ||
                names(ptype[names(x)][i]) == '' ||
                identical(names(ptype[names(x)][i]), character())
              ) {
                lbl <- paste0('Element ', i, ':')
              } else if (
                sum(names(ptype[names(x)]) == names(ptype[names(x)][i])) > 1
              ) {
                lbl <- paste0(names(ptype[names(x)][i]), '..', i, ':')
              } else {
                lbl <- paste0(names(ptype[names(x)][i]), ':')
              }
              paste(
                cli::cli_fmt(cli::cli_text('{.var {lbl}}')),
                cli::cli_fmt(
                  cli::cli_text('\t{.cls {class(ptype[names(x)][[i]])}}')
                ),
                sep = '\t'
              )
            },
            USE.NAMES = FALSE
          ),
          nm = rep(' ', times = length(ptype[names(x)]))
        ),
        '',
        paste(
          '{cli::qty(length(bad_element_type))} However, the following',
          'element{?s} of {.arg {arg}} {?has/have} different type{?s}:'
        ),
        '',
        stats::setNames(
          sapply(
            X = seq_along(bad_element_type),
            FUN = \(i) {
              if (
                is.null(names(bad_element_type[i])) ||
                names(bad_element_type[i]) == '' ||
                identical(names(bad_element_type[i]), character())
              ) {
                lbl <- paste0('Element ', i, ':')
              } else if (
                sum(
                  names(bad_element_type[i]) == names(bad_element_type[i])
                ) > 1
              ) {
                lbl <- paste0(names(bad_element_type[i]), '..', i, ':')
              } else {
                lbl <- paste0(names(bad_element_type[i]), ':')
              }
              paste(
                cli::cli_fmt(cli::cli_text('{.var {lbl}}')),
                cli::cli_fmt(
                  cli::cli_text('\t{.cls {class(bad_element_type[[i]])}}')
                ),
                sep = '\t'
              )
            },
            USE.NAMES = FALSE
          ),
          nm =  rep(' ', times = length(bad_element_type))
        ),
        ''
      ),
      call = error_call
    )
  }
}

validate_obj_type <- function(
    x,
    ptype,
    arg = rlang::caller_arg(x),
    error_call = rlang::caller_env()
) {

  if (typeof(x) !=  typeof(ptype)) {
    cli::cli_abort(
      'error_bad_obj_type',
      message = paste(
        '{.arg {arg}} must be {.obj_type_friendly {ptype}}, not',
        '{.obj_type_friendly {x}}.'
      ),
      call = error_call
    )
  }
  invisible(x)
}

validate_obj_class <- function(
    x,
    ptype,
    arg = rlang::caller_arg(x),
    error_call = rlang::caller_env()
) {

  if (!inherits(x, class(ptype))) {
    cli::cli_abort(
      'error_bad_obj_class',
      message = paste(
        '{.arg {arg}} must inherit the class {.cls {class(ptype)}} but has class',
        '{.cls {class(x)}}.'
      ),
      call = error_call
    )
  }
  invisible(x)
}

# Helpers ---------------------------------------------------------------------

list_depth <- function(x) {
  ifelse(is.list(x), 1L + max(sapply(x, list_depth)), 0L)
}

# nocov end
