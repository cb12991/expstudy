#' Calculate adjustment factors for an underlying assumption
#'
#' There often are situations where an industry table is used for an assumed
#' rate due to a company lacking sufficient credibility to write their own
#' assumption. However, as experience becomes more available, a company would
#' likely want to incorporate this experience into the industry assumption
#' because it provides valuable insight into their own policyholders. A common
#' industry approach is to apply "factor adjustments" developed using company
#' experience to the industry assumption.
#'
#' This function piggy-backs off of `measure_sets` defined in other expstudy
#' functions to quickly produce factor adjustments under a variety of methods.
#' Providing a [dplyr::grouped_df()] will generate factors for each group
#' according to the method specified. If two or more grouping variables are
#' provided, an additional "composite" factor adjustment will also be generated
#' which is the product of each individual adjustment.
#'
#' @param expected_rate
#'   The underlying expected rate in the experience study for which factor
#'   adjustments are being generated for.
#' @param method
#'   String indicating the method of determining factor adjustments:
#'
#'     * `simultaneous` will calculate factor adjustments for all combinations
#'     of group values in one iteration.
#'     * `sequential` will calculate factor adjustments for each grouping
#'     variable individually and applies that factor adjustment to the
#'     underlying expected rate before continuing with the next grouping
#'     variable's factor computation.
#'
#' @param cred_wt_adjs
#'   Logical indicating if factor adjustments should be credibility-weighted
#'   using partial credibility scores.
#' @param balance_adjs
#'   Logical indicating if credibility-weighted adjustments should be scaled to
#'   produce a 100% A/E ratio in aggregate (has no effect if
#'   `cred_wt_adjs = FALSE`).
#'
#' @inheritParams summarise_measures
#' @inheritParams mutate_expecvar
#' @inheritParams summarise_measures
#' @inheritParams summarise_measures
#'
#' @returns
#'   A list of data frames that house factor adjustments for each measure set
#'   provided in `measure_sets`.
#'
#' @examples
#' mortexp |>
#'   dplyr::group_by(
#'     GENDER,
#'     SMOKING_STATUS
#'   ) |>
#'   compute_fct_adjs(
#'     EXPECTED_MORTALITY_RT,
#'     amount_scalar = FACE_AMOUNT
#'   )
#'
#' @export
compute_fct_adjs <- function(
    .data,
    expected_rate,
    measure_sets = guess_measure_sets(.data),
    amount_scalar = NULL,
    method = c('simultaneous', 'sequential'),
    cred_wt_adjs = FALSE,
    balance_adjs = FALSE,
    na.rm = FALSE
) {
  error_call <- rlang::current_env()

  if (missing(measure_sets) && !is.null(attr(.data, 'measure_sets'))) {
    measure_sets <- attr(.data, 'measure_sets')
  } else {
    validate_measure_sets(
      x = measure_sets,
      data = .data,
      data_arg = rlang::caller_arg(.data),
      error_call = error_call
    )
  }
  method <- rlang::arg_match(
    arg = method,
    error_call = error_call
  )
  for (arg in list(cred_wt_adjs, balance_adjs)) {
    validate_obj_type(arg, logical(1), error_call = error_call)
  }

  # Assign names to measure sets if not provided.
  if (is.null(names(measure_sets))) {
    cli::cli_warn(paste(
      'Measure sets unnamed; resulting factor adjustments designated in order',
      'of {.arg measure_sets} provided.'
    ))
    names(measure_sets) <- paste0('MS', seq_along(measure_sets))
  }

  # TODO: incorporate `method` argument (currently ignored).

  # Determine iterations using groups (if any). If .data not grouped then
  # results will also not be grouped and an aggregate adjustment will be
  # returned.
  if (dplyr::is_grouped_df(.data)) {
    grps <- dplyr::groups(.data)
  } else {
    grps <- rlang::quos(NULL)
  }

  # Initialize result by combining grouping structure (a 0-column tibble will
  # be returned if .data not grouped) and an empty double column per each
  # factor adjustment name.
  adjs <- list()
  ungrpd_data_table <- data.table::as.data.table(dplyr::ungroup(.data))
  for (measure_set_nm in names(measure_sets)) {
    adjs[[measure_set_nm]] <- list()

    for (grp_i in seq_along(grps)) {
      if (rlang::is_quosure(grps[[grp_i]])) {
        adj_grp_nm <- 'AGGREGATE'
      } else {
        adj_grp_nm <- rlang::as_name(grps[[grp_i]])
      }
      adjs[[measure_set_nm]][[adj_grp_nm]] <- (
        if (grp_i == 1) {
          ungrpd_data_table
        } else {
          prev_adjs <- adjs[[measure_set_nm]][1:(grp_i - 1)]
          prev_adjs_grid <- Reduce(
            f = \(grid, prev_adj) merge(grid, prev_adj),
            x = prev_adjs,
            init = expand.grid(lapply(prev_adjs, \(x) x[[1]]))
          )
          mutate_quo <- rlang::parse_expr(paste(
            c(
              rlang::as_name(rlang::ensym(expected_rate)),
              paste0(names(prev_adjs), '_FCT_ADJ')
            ),
            collapse = ' * '
          ))
          merge(
            ungrpd_data_table,
            prev_adjs_grid,
            all.x = TRUE
          ) |>
            dplyr::mutate(
              FCT_ADJ_RATE = !!mutate_quo
            ) |>
            mutate_expecvar(
              new_expected_rates = 'FCT_ADJ_RATE',
              new_expecvar_prefix = NULL,
              measure_sets = measure_sets,
              amount_scalar = {{ amount_scalar }}
            )
        }
      ) |>
        dplyr::group_by(
          !!grps[[grp_i]]
        ) |>
        summarise_measures(
          na.rm = na.rm,
          .groups = 'keep'
        ) |>
        dplyr::mutate(
          !!paste0(adj_grp_nm, '_FCT_ADJ') := ae_ratio_vec(
            actuals = !!rlang::sym(
              measure_sets[[measure_set_nm]][['actuals']]
            ),
            expecteds = !!rlang::sym(
              measure_sets[[measure_set_nm]][['expecteds']]
            )
          ),
          CRED_WT = credibility_vec(
            expecteds = !!rlang::sym(
              measure_sets[[measure_set_nm]][['expecteds']]
            ),
            variances = !!rlang::sym(
              measure_sets[[measure_set_nm]][['variances']]
            )
          ),
          .keep = 'none'
        ) |>
        dplyr::ungroup()

      # Credibility weight adjustments by linearly interpolating with
      # adjustment factor of 1 (i.e., no adjustment to the underlying
      # expected rate) subject to partial credibility score.
      if (cred_wt_adjs) {
        adjs[[measure_set_nm]][[adj_grp_nm]] <- dplyr::mutate(
          adjs[[measure_set_nm]][[adj_grp_nm]],
          !!paste0(adj_grp_nm, '_FCT_ADJ') := (1 - .data$CRED_WT) +
            (.data$CRED_WT * !!rlang::sym(paste0(adj_grp_nm, '_FCT_ADJ')))
        )

        # Balance adjustments by applying all credibility-weighted
        # adjustments then multiplying by a scalar to arrive at a 100% AE
        # ratio in aggregate.
        if (balance_adjs) {
          # This scalar is simply the AE after applying all
          # credibility-weighted adjustments up to this iteration.

          scalar <- (
            if (grp_i == 1) {
              ungrpd_data_table
            } else {
              curr_iter_adj_grid <- Reduce(
                f = \(grid, adj) merge(grid, adj),
                x = adjs[[measure_set_nm]],
                init = expand.grid(lapply(adjs[[measure_set_nm]], \(x) x[[1]]))
              )
              mutate_quo <- rlang::parse_expr(paste(
                c(
                  rlang::as_name(rlang::ensym(expected_rate)),
                  paste0(names(adjs[[measure_set_nm]]), '_FCT_ADJ')
                ),
                collapse = ' * '
              ))
              merge(
                ungrpd_data_table,
                curr_iter_adj_grid,
                all.x = TRUE
              ) |>
                dplyr::mutate(
                  FCT_ADJ_RATE = !!mutate_quo
                ) |>
                mutate_expecvar(
                  new_expected_rates = 'FCT_ADJ_RATE',
                  new_expecvar_prefix = NULL,
                  measure_sets = measure_sets,
                  amount_scalar = {{ amount_scalar }}
                )
            }
          ) |>
            summarise_measures(
              na.rm = na.rm
            ) |>
            dplyr::mutate(
              scalar = ae_ratio_vec(
                actuals = !!rlang::sym(
                  measure_sets[[measure_set_nm]][['actuals']]
                ),
                expecteds = !!rlang::sym(
                  measure_sets[[measure_set_nm]][['expecteds']]
                )
              ),
              .keep = 'none'
            ) |>
            dplyr::pull(
              scalar
            )

          adjs[[measure_set_nm]][[adj_grp_nm]] <- dplyr::mutate(
            adjs[[measure_set_nm]][[adj_grp_nm]],
            !!paste0(adj_grp_nm, '_FCT_ADJ') :=
              !!rlang::sym(paste0(adj_grp_nm, '_FCT_ADJ')) * scalar
          )
        }
      } else if (balance_adjs) {
        cli::cli_warn(paste(
          'Balancing adjustment factors that are not credibility-weighted',
          'has no effect.'
        ))
      }

      # Remove the credibility weight as it's no longer needed.
      adjs[[measure_set_nm]][[adj_grp_nm]]$CRED_WT <- NULL
    }

    # Capture names of individual factor adjustments prior to combining.
    measure_set_fct_adj_nms <- paste0(
      names(adjs[[measure_set_nm]]),
      '_FCT_ADJ'
    )

    # Once all adjustments have been determined, combine into a single
    # data frame for each measure set.
    adjs[[measure_set_nm]] <- Reduce(
      f = \(grid, adj) merge(grid, adj, all = TRUE),
      x = adjs[[measure_set_nm]],
      init = expand.grid(lapply(adjs[[measure_set_nm]], \(x) x[[1]]))
    )

    # Add a composite factor adjustment (which is the product of all
    # individual factor adjustments) if 2 or more adjustment factors were
    # computed.
    if (length(measure_set_fct_adj_nms) > 1) {
      adjs[[measure_set_nm]] <- dplyr::mutate(
        adjs[[measure_set_nm]],
        COMPOSITE_FCT_ADJ = !!rlang::parse_expr(paste(
          measure_set_fct_adj_nms,
          collapse = ' * '
        ))
      )
    }
  }
  adjs
}
