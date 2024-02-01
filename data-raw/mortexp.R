library(tidyverse)
library(lubridate)

mortexp <- tibble(
  POLICY_HOLDER = factor(paste0('PH_', str_pad(seq_len(1000), 4, pad = '0'))),
  MAX_DURATION = pmax(1, rpois(n = 1000, lambda = 15)),
  POLICY_STATUS = as_factor(
    sample(
      x = c('INFORCE', 'SURRENDERED', 'DEATH'),
      size = 1000,
      replace = TRUE
    )
  ),
  GENDER = factor(
    sample(
      x = c('MALE', 'FEMALE'),
      size = 1000,
      replace = TRUE,
      prob = c(.55, .45)
    )
  ),
  SMOKING_STATUS = factor(
    sample(
      x = c('SMOKER', 'NON-SMOKER'),
      size = 1000,
      replace = TRUE,
      prob = c(.3, .7)
    )
  ),
  UNDERWRITING_CLASS = factor(
    sample(
      x = c('STANDARD', 'SELECT', 'PREFERRED'),
      size = 1000,
      replace = TRUE,
      prob = c(.65, .25, 0.1)
    )
  ),
  FACE_AMOUNT = sample(
    x = c(5000, 10000, 25000, 50000, 100000),
    size = 1000,
    replace = TRUE,
    prob = c(0.55, 0.225, 0.125, 0.075, 0.025)
  ),
  ISSUE_YEAR = if_else(
    POLICY_STATUS == 'INFORCE',
    year(now()) - MAX_DURATION,
    year(now()) - MAX_DURATION - trunc(rgamma(n = 1000, shape = 10))
  ),
  ISSUE_MONTH = sample.int(n = 12, size = 1000, replace = TRUE),
  ISSUE_DAY = map_dbl(
    .x = days_in_month(ym(paste(ISSUE_YEAR, ISSUE_MONTH))),
    .f = sample.int,
    size = 1
  ),
  ISSUE_DATE = ymd(
    ISSUE_YEAR * 10000 + ISSUE_MONTH * 100 + ISSUE_DAY
  ),
  TERMINATION_MONTH = if_else(
    POLICY_STATUS == 'INFORCE',
    NA_integer_,
    sample.int(n = 12, size = 1000, replace = TRUE)
  ),
  TERMINATION_DAY = modify_if(
    .x = days_in_month(TERMINATION_MONTH),
    .p = ~ !is.na(.x),
    .f = ~ sample.int(.x, size = 1)
  ),
  TERMINATION_YEAR = case_when(
    POLICY_STATUS == 'INFORCE' ~ NA_real_,
    TERMINATION_MONTH < ISSUE_MONTH ~ ISSUE_YEAR + MAX_DURATION,
    TERMINATION_MONTH > ISSUE_MONTH ~ ISSUE_YEAR + MAX_DURATION - 1,
    TERMINATION_DAY < ISSUE_DAY ~ ISSUE_YEAR + MAX_DURATION,
    TERMINATION_DAY >= ISSUE_DAY ~ ISSUE_YEAR + MAX_DURATION - 1
  ),
  TERMINATION_DATE = ymd(
    TERMINATION_YEAR * 10000 + TERMINATION_MONTH * 100 + TERMINATION_DAY
  ),
  ISSUE_AGE = rnorm(n = 1000, mean = 50, sd = 20) %>%
    trunc %>%
    pmin(75) %>%
    pmax(18),
  BIRTH_MONTH = sample.int(n = 12, size = 1000, replace = TRUE),
  BIRTH_DAY = map_dbl(
    .x = days_in_month(BIRTH_MONTH),
    .f = sample.int,
    size = 1
  ),
  BIRTH_YEAR = year(now()) - ISSUE_AGE - MAX_DURATION - case_when(
    BIRTH_MONTH < month(now()) ~ 0,
    BIRTH_MONTH > month(now()) ~ 1,
    BIRTH_DAY < day(now()) ~ 0,
    BIRTH_DAY >= day(now()) ~ 1
  ) - if_else(
    POLICY_STATUS != 'INFORCE',
    TERMINATION_DATE %--% now() %/% years(1),
    0
  ),
  INSURED_DOB = ymd(
    BIRTH_YEAR * 10000 + BIRTH_MONTH * 100 + BIRTH_DAY
  ),
  POLICY_DURATION_MNTH = map(
    .x = ISSUE_DATE %--% coalesce(TERMINATION_DATE, now()) %/% months(1) + 2,
    .f = seq_len
  )
) %>%
  unnest(
    POLICY_DURATION_MNTH
  ) %>%
  mutate(
    POLICY_DURATION_YR = (POLICY_DURATION_MNTH - 1) %/% 12 + 1,
    AS_OF_DATE = rollforward(ISSUE_DATE %m+% months(POLICY_DURATION_MNTH - 1)),
    NEXT_ANNIVERSARY_DATE = ISSUE_DATE + years(POLICY_DURATION_YR),
    ATTAINED_AGE = INSURED_DOB %--% AS_OF_DATE %/% years() + 1,
    EXPECTED_MORTALITY_RT = 1 / (120 - pmin(120, ATTAINED_AGE)), # De Moivre's
    EXPOSURE_START_DATE = if_else(
      POLICY_DURATION_MNTH == 1,
      ISSUE_DATE,
      rollbackward(AS_OF_DATE, roll_to_first = TRUE)
    ),
    EXPOSURE_END_DATE = case_when(
      year(TERMINATION_DATE) == year(AS_OF_DATE) &
        month(TERMINATION_DATE) == month(AS_OF_DATE) &
        POLICY_STATUS == 'SURRENDERED' ~ TERMINATION_DATE,
      year(TERMINATION_DATE) == year(AS_OF_DATE) &
        month(TERMINATION_DATE) == month(AS_OF_DATE) &
        POLICY_STATUS == 'DEATH' ~ NEXT_ANNIVERSARY_DATE,
      TRUE ~ AS_OF_DATE
    ),
    EXPOSURE_DAYS = EXPOSURE_START_DATE %--% EXPOSURE_END_DATE %/% days(),
    CALENDAR_YEAR_DAYS = 365 + if_else(leap_year(AS_OF_DATE), 1, 0),
    MORT_EXPOSURE_CNT = EXPOSURE_DAYS / CALENDAR_YEAR_DAYS,
    MORT_EXPOSURE_AMT = MORT_EXPOSURE_CNT * FACE_AMOUNT,
    MORT_ACTUAL_CNT = if_else(
      POLICY_STATUS == 'DEATH' &
        TERMINATION_DATE %within% (EXPOSURE_START_DATE %--% EXPOSURE_END_DATE),
      1,
      0
    ),
    MORT_ACTUAL_AMT = MORT_ACTUAL_CNT * FACE_AMOUNT,
    MORT_EXPECTED_CNT = MORT_EXPOSURE_CNT * EXPECTED_MORTALITY_RT,
    MORT_EXPECTED_AMT = MORT_EXPECTED_CNT * FACE_AMOUNT,
    MORT_VARIANCE_CNT = MORT_EXPECTED_CNT * (1 - MORT_EXPECTED_CNT),
    MORT_VARIANCE_AMT = MORT_VARIANCE_CNT * (FACE_AMOUNT^2)
  ) %>%
  select(
    AS_OF_DATE,
    POLICY_HOLDER,
    GENDER,
    SMOKING_STATUS,
    UNDERWRITING_CLASS,
    FACE_AMOUNT,
    INSURED_DOB,
    ISSUE_DATE,
    TERMINATION_DATE,
    ISSUE_AGE,
    ATTAINED_AGE,
    EXPECTED_MORTALITY_RT,
    POLICY_DURATION_YR,
    POLICY_DURATION_MNTH,
    POLICY_STATUS,
    MORT_EXPOSURE_CNT,
    MORT_EXPOSURE_AMT,
    MORT_ACTUAL_CNT,
    MORT_ACTUAL_AMT,
    MORT_EXPECTED_CNT,
    MORT_EXPECTED_AMT,
    MORT_VARIANCE_CNT,
    MORT_VARIANCE_AMT
  )

use_data(as.data.frame(mortexp), overwrite = TRUE)
