for (f in list.files('R', full.names = TRUE)) source(f)
mult <- 50L
data(mortexp)
big_df <- structure(
  do.call(rbind, lapply(seq_len(mult), \(x) mortexp)),
  class = 'data.frame',
  row.names = .set_row_names(nrow(mortexp) * mult)
)
big_df_grouped <- dplyr::group_by(big_df, GENDER, SMOKING_STATUS)

op <- options()
op.expstudy <- list(
  expstudy.default_measure_regexs = list(
    actuals = '_?ACTUAL_?',
    exposures = '_?EXPOSURE_?',
    expecteds = '_?EXPECTED_?',
    variances = '_?VARIANCE_?'
  ),
  expstudy.default_measure_set_prefixes = NULL,
  expstudy.default_measure_set_suffixes = c('CNT', 'AMT')
)
toset <- !(names(op.expstudy) %in% names(op))
if (any(toset)) options(op.expstudy[toset])

p <- profvis::profvis(
  compute_fct_adjs(
    .data = big_df_grouped,
    expected_rate = EXPECTED_MORTALITY_RT,
    amount_scalar = FACE_AMOUNT,
    method = 'sequential',
    cred_wt_adjs = TRUE,
    balance_adjs = TRUE
  )
)
print(p, aggregate = TRUE)
