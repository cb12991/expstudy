#' Mortality experience study
#'
#' A dataset containing an example of a mortality experience study for 1000
#' fictional whole life insurance policyholders.
#'
#' @format A [data.frame()] with over 175,000 rows and 24 columns:
#'
#' \describe{
#'   \item{`AS_OF_DATE`}{
#'     This indicates which point in time a record encompasses.
#'   }
#'   \item{`POLICY_HOLDER`}{
#'     An index used to distinguish policyholders. In this example the
#'     policyholder is also the (only) insured.
#'   }
#'   \item{
#'     `GENDER`,
#'     `SMOKING_STATUS`,
#'     `UNDERWRITING_CLASS`,
#'     `INSURED_DOB`,
#'     `ISSUE_DATE`,
#'     `ISSUE_AGE`
#'   }{
#'     Various characteristics of an insured at time of issue.
#'   }
#'   \item{`FACE_AMOUNT`}{
#'     Face amount of insurance for a corresponding policy.
#'   }
#'   \item{`TERMINATION_DATE`}{
#'     If terminated, the effective date of termination. An `NA` value will be
#'     listed for policies that are still in-force.
#'   }
#'   \item{`ATTAINED_AGE`}{
#'     The age of the insured at the record's `AS_OF_DATE`
#'   }
#'   \item{`EXPECTED_MORTALITY_RT`}{
#'     An expected mortality rate for an insured. The rate is calculated
#'     according to De Moivre's Law (also known as uniform distribution of
#'     deaths, or \eqn{\text{UDD}}) with \eqn{\omega=120}.
#'   }
#'   \item{`POLICY_DURATION_MNTH`, `POLICY_DURATION_YR`}{
#'     Temporal indices describing how long a policy has been in-force at the
#'     `AS_OF_DATE`. For example, when a policy is first issued (i.e.,
#'     \eqn{t=0}), it is in policy duration year one and policy duration month
#'     one.
#'   }
#'   \item{`POLICY_STATUS`}{
#'     The current status of the policy, either in-force, surrendered, or
#'     death. The value will be listed for each policy record even though a
#'     decrement only occurs at the end of the policy's duration (for policies
#'     which are no longer in-force).
#'   }
#'   \item{`MORT_EXPOSURE_CNT`,`MORT_EXPOSURE_AMT`}{
#'     Measures how many policyholders or how much face amount of insurance is
#'     exposed to the risk of decrement for an associated observations.
#'   }
#'   \item{`MORT_ACTUAL_CNT`,`MORT_ACTUAL_AMT`}{
#'     Measures the decrement occurrence on a policy count or face amount of
#'     insurance basis.
#'   }
#'   \item{`MORT_EXPECTED_CNT`,`MORT_EXPECTED_AMT`}{
#'     Measures the expected decrement value for an associated observation on a
#'     policy count or face amount of insurance basis.
#'   }
#'   \item{`MORT_VARIANCE_CNT`,`MORT_VARIANCE_AMT`}{
#'     Measures the variance of the decrement expectation, also on a policy
#'     count or face amount of insurance basis. Used to calculate credibility
#'      scores and confidence intervals.
#'   }
#' }
#'
#' @source
#'   All policy record detail is randomly generated. See
#'   [the Society of Actuaries' publication on experience study calculations](https://www.soa.org/globalassets/assets/Files/Research/2016-10-experience-study-calculations.pdf)
#'   for additional information regarding experience study calculations.
"mortexp"
