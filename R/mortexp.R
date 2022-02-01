#' Sample mortality experience
#'
#' A dataset containing an example of a mortality experience study for 1000
#' fictional whole life insurance policyholders.
#'
#' @format
#'   A [`tibble`][tibble::tibble()] with over 175,000 rows and 18
#'   variables:
#' \describe{
#'   \item{AS_OF_DATE}{This indicates which point in time a record encompasses.}
#'   \item{POLICY_HOLDER}{An index used to distinguish policyholders. In this
#'     example the policyholder is also the (only) insured.}
#'   \item{GENDER, SMOKING_STATUS, UNDERWRITING_CLASS, INSURED_DOB, ISSUE_DATE,
#'     ISSUE_AGE}{Various characteristics of insured at time of issue.}
#'   \item{ATTAINED_AGE}{The age of the insured at the record's `AS_OF_DATE`}
#'   \item{DURATION_MONTH, DURATION_YEAR}{An index describing how long a policy
#'     has been in-force at the `AS_OF_DATE`. For example, when a policy is
#'     first issued (i.e., \eqn{t=0}), it is in duration year one, duration
#'     month one (i.e.., from \eqn{t=0} months to \eqn{t=1} months).}
#'   \item{POLICY_STATUS}{The current status of the policy, either in-force,
#'     surrendered, or death. The value will be listed for each policy record
#'     even though a decrement only occurs at the end of the policy's duration
#'     (for policies which are no longer in-force).}
#'   \item{TERMINATION_DATE}{If terminated, the effective date of termination.
#'     An `NA` value will be listed for policies that are still in-force.}
#'   \item{EXPOSURE}{A measure that reflects how many persons or contracts were
#'     exposed to the possibility or risk of the event under study, and for how
#'     long. This is calculated by dividing the number of days exposed by the
#'     number of days in the calendar year.}
#'   \item{ACTUAL_DEATHS}{The number of actual deaths reported. This will only
#'     be 0 or 1 for any given record and it's main purpose is for aggregation.}
#'   \item{EXPECTED_MORTALITY_RT}{An expected mortality rate for an insured.
#'     The rate is calculated according to De Moivre's Law (also known as
#'     uniform distribution of deaths, or UDD) with \eqn{\omega=120}}.
#'   \item{EXPECTED_DEATHS}{The number of deaths expected for a given record.
#'     This is calculated by multiplying exposure by the expected mortality
#'     rate, and it provides insight on the expected mortality rate's
#'     performance upon aggregation.}
#'   \item{VARIANCE_DEATHS}{The variance of the number of expected deaths, used
#'     primarily for assessing the credibility of a subsample's analyses.}
#' }
#'
#' @source All policy record detail is randomly generated. See
#'   \url{https://www.soa.org/globalassets/assets/Files/Research/2016-10-experience-study-calculations.pdf}
#'   for additional information regarding experience study calculations.
"mortexp"