#' Update expstudy meta data
#'
#' Passing an `expstudy` through a function often causes the attributes to be
#' dropped. This is a helper function that retains original attributes.
#'
#' @param expstudy an `expstudy()` object
#' @keywords internal
update_meta <- function(new, old) {
  return(
    structure(
      .Data = new,
      class = c('tbl_es', class(new)),
      metric_vars = attr(old, 'metric_vars'),
      metrics_applied = attr(old, 'metrics_applied')
    )
  )
}
