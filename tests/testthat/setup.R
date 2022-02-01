es <- expstudy(
  data = mortexp,
  actuals = ACTUAL_DEATHS,
  expecteds = EXPECTED_DEATHS,
  exposures = EXPOSURE,
  variances = VARIANCE_DEATHS,
  keys = c(AS_OF_DATE, POLICY_HOLDER)
)
