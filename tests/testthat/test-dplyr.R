test_that('arrange method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, arrange, 1)
  )
})

test_that('collect method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, collect)
  )
})

test_that('compute method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, compute)
  )
})

test_that('count method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, count, es$ATTAINED_AGE)
  )
})

test_that('distinct method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, distinct, es$SMOKING_STATUS)
  )
})

test_that('filter method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, filter, es$GENDER == 'FEMALE')
  )
})

test_that('group_by method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, group_by, es$UNDERWRITING_CLASS)
  )
})

test_that('group_map method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, group_map, .f = as.data.frame)
  )
})

test_that('group_modify method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, group_modify, .f = as.list)
  )
})

test_that('intersect method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, intersect, y = es)
  )
})

test_that('left_join method preserves metadata', {
  suppressMessages(
    purrr::walk(
      c(attr_preserved, class_preserved),
      exec,
      !!!list(es, left_join, y = es)
    )
  )
})

test_that('mutate method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, mutate, test = 'test')
  )
})

test_that('relocate method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, relocate, es$ATTAINED_AGE, .before = 1)
  )
})

test_that('rename method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, rename, test = 'ACTUAL_DEATHS')
  )
})

test_that('rename_with method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, rename_with, .fn = tolower)
  )
})

test_that('select method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, select, es$RECORD_YEARSPAN)
  )
})

test_that('setdiff method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, setdiff, y = es)
  )
})

test_that('slice method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, slice, 40:99)
  )
})

test_that('slice_head method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, slice_head, prop = 0.1)
  )
})

test_that('slice_max method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, slice_max, order_by = es$ATTAINED_AGE, n = 100)
  )
})

test_that('slice_min method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(
      es,
      slice_min,
      order_by = es$EXPECTED_DEATHS,
      prop = .3,
      with_ties = FALSE
    )
  )
})

test_that('slice_tail method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, slice_tail, n = 50)
  )
})

test_that('summarise method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, summarise, test = sum(es$RECORD_YEARSPAN), .groups = 'drop')
  )
})

test_that('transmute method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, transmute, test = as.character(es$ACTUAL_DEATHS))
  )
})

test_that('ungroup method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(group_by(es, .data$GENDER), ungroup)
  )
})

test_that('union method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, union, y = es)
  )
})

test_that('union_all method preserves metadata', {
  purrr::walk(
    c(attr_preserved, class_preserved),
    exec,
    !!!list(es, union_all, y = es)
  )
})
