data <- tibble::tibble(
  GENDER = sample(
    x = c('MALE', 'FEMALE'),
    size = 1000,
    replace = TRUE,
    prob = c(.55, .45)
  ),
  SMOKING_STATUS = sample(
    x = c('SMOKER', 'NON-SMOKER'),
    size = 1000,
    replace = TRUE,
    prob = c(.3, .7)
  ),
  UNDERWRITING_CLASS = sample(
    x = c('STANDARD', 'SELECT', 'PREFERRED'),
    size = 1000,
    replace = TRUE,
    prob = c(.65, .25, 0.1)
  ),
  ATTAINED_AGE = rnorm(n = 1000, mean = 50, sd = 20) %>%
    trunc %>%
    pmin(120) %>%
    pmax(0),
  RECORD_YEARSPAN = runif(1000),
  ACTUAL_MORTALITY_RT = 1 - exp(-0.0075 * RECORD_YEARSPAN), # constant force
  ACTUAL_DEATHS = RECORD_YEARSPAN * ACTUAL_MORTALITY_RT,
  EXPECTED_MORTALITY_RT = 1 / (120 - pmin(119, ATTAINED_AGE)), # De Moivre's
  EXPECTED_DEATHS = RECORD_YEARSPAN * EXPECTED_MORTALITY_RT,
  VARIANCE_EXP_DEATHS = EXPECTED_DEATHS * (1 - EXPECTED_DEATHS)
)

es <- expstudy(
  data = data,
  actuals = ACTUAL_DEATHS,
  expecteds = EXPECTED_DEATHS,
  exposures = RECORD_YEARSPAN,
  variances = VARIANCE_EXP_DEATHS,
  keys = c(GENDER, SMOKING_STATUS, UNDERWRITING_CLASS, ATTAINED_AGE)
)
