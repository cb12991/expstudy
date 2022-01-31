#' @exportClass tbl_es
setOldClass('tbl_es', 'dtplyr_step')

#' `tbl_es` class
#'
#' @description
#' The `tbl_es` class is a subclass of [dtplyr::lazy_dt()] created in order to
#' store attributes for experience studies. The colloquial term 'expstudy'
#' refers to a [lazy_dt()] that has the `tbl_es` subclass.
#'
#' @section Properties of `tbl_es`:
#'
#' * Attributes that store variable names used for key metric calculations:
#'   - `actuals`: the observed variable (or variables) within the experience
#'     study (e.g., actual lapse counts for a lapse study).
#'   - `expecteds`: the expected variable (or variables) within the experience
#'     study (e.g., expected number of deaths for a mortality study).
#'   - `exposures`: the exposure variable (or variables) within the experience
#'     study. This will be the time the records spans in years (or within (0, 1)
#'     for records covering less than a year).
#'   - `variances`: optional, the variance variable (or variables) within the
#'     experience study. This may not be available for all studies, thus can be
#'     omitted. Variance is used for the credibility metric.
#' * An additional (optional) `keys` attribute that can uniquely describe every
#'   record within the experience study. This typically is policy number and a
#'   measure of time, such as annual or monthly duration. Can be omitted, but
#'   providing enables quicker binary searches within the [dtplyr::lazy_dt()].
#'   For more information, refer to the [data.table] vignette:
#'   `vignette('datatable-keys-fast-subset', package = 'data.table')`.
#'
#' @section Behavior of `tbl_es`:
#'
#' A `tbl_es` behaves the same as a [dtplyr::lazy_dt()] only differing by
#' retaining the attributes listed above. This saves time in routine, repetitive
#' coding, leading to increased time for analysis.
#'
#' @name tbl_es-class
#' @aliases tbl_es tbl_es-class
#' @seealso [dtplyr::lazy_dt]
NULL
