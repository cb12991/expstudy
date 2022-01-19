attr_preserved <- function(x, f, ...) {
  x_meta <- attributes(x)[c('metric_variables', 'metrics_applied')]

  y <- do.call(f, list2(x, ...))
  y_meta <- attributes(y)[c('metric_variables', 'metrics_applied')]

  expect_equal(x_meta, y_meta)
}

class_preserved <- function(x, f, ...) {
  expect_s3_class(do.call(f, list2(x, ...)), class = 'tbl_es')
}


