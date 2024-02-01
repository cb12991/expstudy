#' Add new expecteds and variances to an experience study
#'
#' `mutate_expecvar()` uses a new expected rate for a decrement of interest and
#' adds a corresponding expected decrements column and corresponding variance
#' of expected decrements column. If there are already expecteds and variances
#' measures within the study dataset, either new, prefixed columns will be
#' added or the current expecteds and variances can be overwritten.
#'
#' # Underlying Assumptions
#'
#' This function was developed according to current industry practice relating
#' to experience study calculations. Some of the assumptions incorporated are
#' briefly outlined below.
#'
#' 1. The experience study data is at a seriatim level where repeated
#' observations of multiple units can exist. For example, the study data can
#' contain experience for multiple policies over multiple calendar or policy
#' years.
#' 1. Each decrement event can be described as a Bernoulli random variable with
#' expected rate of decrement equal to $p$. Furthermore, combining multiple
#' observation units with equal rates of decrement $p$ can be considered a
#' Binomial random variable with $n$ equal to the number of observation units.
#' 1. Decrements are considered to be uniform between observations.
#'
#' With these assumptions, new expecteds that are not amount-weighted are
#' calculated as the product of exposures and the expected decrement rate, new
#' variances are calculated as the product of the previously calculated new
#' expecteds and 1 minus the previously calculated new expecteds.
#' Amount-weighted expecteds and variances follow the prior calculations and
#' additionally multiply by the amount scalar and amount scalar squared,
#' respectively.
#'
#' For a more detailed explanation of these methods used, please refer to the
#' [Society of Actuary's publication over experience study calculations](https://www.soa.org/globalassets/assets/Files/Research/2016-10-experience-study-calculations.pdf).
#'
#' @param new_expected_rates
#'   A numeric vector to use as the expected probability for the study's event
#'   of interest (i.e., policy lapse or insured death). This can be a column
#'   in the dataset or a new numeric vector of length 1 or `nrow(.data)`.
#' @param new_expecvar_prefix
#'   A string to distinguish the new expecteds and variances columns in the
#'   dataset. To overwrite existing expecteds and variances columns, use an
#'   argument value of `NULL`, `character()`, or `''`. The default `'auto'` will
#'   add a numeric prefix based on the previous names of expecteds/variances so
#'   that names will remain unique.
#' @param amount_scalar
#'   A numeric vector to use when determining amount-weighted expecteds and
#'   variances. The function will determine whether or not the new
#'   expecteds/variances are amount-weighted if the corresponding actuals in
#'   the study have values greater than 1 (actuals that are not
#'   amount-weighted, i.e., counts, should only be 0 or 1).
#' @inheritParams summarise_measures
#' @inheritParams dplyr::mutate
#' @inheritSection summarise_measures Naming convention
#' @inherit dplyr::mutate return
#'
#' @examples
#' mortexp |>
#'   dplyr::mutate(
#'     NEW_EXPECTED_MORT_RT = runif(n = nrow(mortexp))
#'   ) |>
#'   mutate_expecvar(
#'     new_expected_rates = NEW_EXPECTED_MORT_RT,
#'     new_expecvar_prefix = 'ADJ_',
#'     amount_scalar = FACE_AMOUNT
#'   )
#'
#' @export
mutate_expecvar <- function(
    .data,
    new_expected_rates,
    new_expecvar_prefix = 'auto',
    measure_sets = guess_measure_sets(.data),
    amount_scalar = NULL,
    .by = NULL,
    .keep = c("all", "used", "unused", "none"),
    .before = NULL,
    .after = NULL
) {
  mutate_expecvar_error_call <- rlang::current_env()

  if (missing(new_expected_rates)) {
    cli::cli_abort(
      'Need to provide column to use for new expected and variance measures.'
    )
  } else {
    new_expected_rates <- rlang::as_label(rlang::ensym(new_expected_rates))
    validate_data_column(
      new_expected_rates,
      data = .data,
      ptype = double(),
      error_call = mutate_expecvar_error_call
    )
  }

  amount_scalar <- rlang::enquo(amount_scalar)
  if (rlang::quo_is_null(amount_scalar)) {
    amount_scalar <- NULL
  } else {
    amount_scalar <- rlang::as_label(amount_scalar)
  }

  if (missing(measure_sets) && !is.null(attr(.data, 'measure_sets'))) {
    measure_sets <- attr(.data, 'measure_sets')
  } else {
    validate_measure_sets(
      x = measure_sets,
      data = .data,
      data_arg = rlang::caller_arg(.data),
      error_call = mutate_expecvar_error_call
    )
  }

  mutate_quos <- unlist(lapply(
    seq_along(measure_sets),
    \(i) {
      if (
        length(new_expecvar_prefix) == 0 ||
        is.na(new_expecvar_prefix) ||
        new_expecvar_prefix == ''
      ) {
        expec_nm <- measure_sets[[i]][['expecteds']]
        var_nm <- measure_sets[[i]][['variances']]
      } else if (new_expecvar_prefix == 'auto') {
        expec_pref_id <- var_pref_id <- 1L

        expec_nm <- paste(
          'NEW',
          measure_sets[[i]][['expecteds']],
          expec_pref_id,
          sep = '_'
        )
        while (expec_nm %in% colnames(.data)) {
          expec_pref_id <- expec_pref_id + 1L
          expec_nm <- paste(
            'NEW',
            measure_sets[[i]][['expecteds']],
            expec_pref_id,
            sep = '_'
          )
        }

        var_nm <- paste(
          'NEW',
          measure_sets[[i]][['variances']],
          var_pref_id,
          sep = '_'
        )
        while (var_nm %in% colnames(.data)) {
          var_pref_id <- var_pref_id + 1L
          var_nm <- paste(
            'NEW',
            measure_sets[[i]][['variances']],
            var_pref_id,
            sep = '_'
          )
        }
      } else {
        expec_nm <- paste0(new_expecvar_prefix, measure_sets[[i]][['expecteds']])
        var_nm <- paste0(new_expecvar_prefix, measure_sets[[i]][['variances']])
      }

      expec_expr_string <- paste(
        measure_sets[[i]][['exposures']],
        '*',
        new_expected_rates
      )
      if (max(.data[[measure_sets[[i]][['actuals']]]], na.rm = TRUE) <= 1) {
        var_component <- '1'
      } else if (is.null(amount_scalar)) {
        cli::cli_abort(
          message = c(
            paste(
              'Actuals column {.var {measure_sets[[i]][[\'actuals\']]}} has',
              'values greater than 1'
            ),
            '',
            paste(
              'This suggests either the study data has been aggregated or',
              'one of the measure sets provided is on an amount basis. This',
              'leads to a few potential options to resolve:'
            ),
            '',
            '*' = paste(
              'This function has only been developed to work experience',
              'studies at a seriatim level, so you will need to use an',
              'unaggregated dataset. If not available, you will need to',
              'adjust expecteds and variances manually.'
            ),
            '*' = paste(
              'If measures are on an amount basis, pass the column to',
              'scale study measures by to the {.arg amount_scalar} argument.'
            ),
            '*' = paste(
              'Exclude the amount-based measure set (that contains the',
              'measure {.var {measure_sets[[i]][[\'actuals\']]}}, at minimum)',
              'from {.arg measure_sets} when calling the function.'
            ),
            ''
          ),
          call = mutate_expecvar_error_call
        )
      } else {
        var_component <- amount_scalar
      }

      var_expr_string <- paste(
        expec_nm,
        '*',
        paste0(
          '(',
          var_component,
          ' - ',
          expec_nm,
          ')'
        )
      )

      stats::setNames(
        list(
          rlang::quo(!!rlang::parse_expr(expec_expr_string)),
          rlang::quo(!!rlang::parse_expr(var_expr_string))
        ),
        nm = c(expec_nm, var_nm)
      )
    }
  ))

  structure(
    dplyr::mutate(
      .data = .data,
      !!!mutate_quos,
      .by = {{ .by }},
      .keep = {{ .keep }},
      .before = {{ .before }},
      .after = {{ .after }}
    ),
    measure_sets = measure_sets
  )
}
