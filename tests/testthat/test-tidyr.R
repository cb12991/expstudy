test_that('complete method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, complete, es$ATTAINED_AGE)
  )
})

test_that('drop_na method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, drop_na)
  )
})

test_that('expand method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, expand, es$GENDER, es$UNDERWRITING_CLASS)
  )
})

test_that('fill method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, fill, .direction = 'down')
  )
})

test_that('nest method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, nest, data = es$GENDER)
  )
})

test_that('pivot_longer method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, pivot_longer, cols = 'GENDER')
  )
})

test_that('pivot_wider method preserves metadata', {
  # Suppress message of tidyr::pivot_wider not using all_of(id_cols).
  suppressMessages(
    purrr::walk(
      c(attr_preserved, class_preserved),
      exec,
      !!!list(
        es, pivot_wider, names_from = 'SMOKING_STATUS', values_from = 'GENDER'
      )
    )
  )
})

test_that('replace_na method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, replace_na, replace = list(GENDER = 'test'))
  )
})

test_that('separate method preserves metadata', {
  # Suppress warning of tidyr::separate using `as.character()` on a quosure.
  suppressWarnings(
    purrr::walk(
      c(attr_preserved, class_preserved),
      exec,
      !!!list(es, separate, col = 'SMOKING_STATUS', into = c('A', 'B'))
    )
  )
})
