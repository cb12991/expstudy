expect_not_identical <- function(
    object,
    expected,
    info = NULL,
    label = NULL,
    expected.label = NULL,
    ...
) {
  act <- quasi_label(rlang::enquo(object), label, arg = 'object')
  exp <- quasi_label(rlang::enquo(expected), expected.label, arg = 'expected')
  comp <- testthat:::waldo_compare(
    x = act$val,
    y = exp$val,
    ...,
    x_arg = 'actual',
    y_arg = 'expected'
  )
  expect(
    ok = length(comp) != 0,
    failure_message = sprintf(
      '%s (`actual`) does not differ from %s (`expected`).\n\n%s',
      act$lab,
      exp$lab,
      paste0(comp, collapse = '\n\n')
    ),
    info = info,
    trace_env = rlang::caller_env()
  )
  invisible(act$val)
}
