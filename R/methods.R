
# Passing an `expstudy` through a method often causes the attributes to be
# dropped. This is a helper function that retains original attributes.
#
update_meta <- function(new, old) {
  return(
    structure(
      .Data = new,
      class = unique(c('tbl_es', class(new))),
      metric_vars = attr(old, 'metric_vars'),
      metrics_applied = attr(old, 'metrics_applied')
    )
  )
}

# dplyr ------------------------------------------------------------------------

#' @export
dplyr::arrange
#' @export
arrange.tbl_es <- function(.data, ..., .by_group) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::collect
#' @export
collect.tbl_es <- function(x, ...) {
  update_meta(NextMethod(), x)
}

#' @export
dplyr::compute
#' @export
compute.tbl_es <- function(x, ...) {
  update_meta(NextMethod(), x)
}

#' @export
dplyr::count
#' @export
count.tbl_es <- function(x, ..., wt, sort, name) {
  update_meta(NextMethod(), x)
}

#' @export
dplyr::distinct
#' @export
distinct.tbl_es <- function(.data, ..., .keep_all) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::filter
#' @export
filter.tbl_es <- function(.data, ..., .preserve) {
  update_meta(NextMethod(), .data)
}

#' @export
pillar::glimpse
#' @export
glimpse.tbl_es <- function(x, width = NULL, ...) {
  update_meta(NextMethod(), x)
}

#' @export
dplyr::group_by
#' @export
group_by.tbl_es <- function(.data, ..., .add, .drop) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::group_map
#' @export
group_map.tbl_es <- function(.data, .f, ..., .keep) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::group_modify
#' @export
group_modify.tbl_es <- function(.data, .f, ..., .keep) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::intersect
#' @export
intersect.tbl_es <- function(x, y, ...) {
  update_meta(NextMethod(), x)
}

#' @export
dplyr::left_join
#' @export
left_join.tbl_es <- function(x, y, by, copy, suffix, ..., keep) {
  update_meta(NextMethod(), x)
}

#' @export
dplyr::mutate
#' @export
mutate.tbl_es <- function(.data, ...) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::relocate
#' @export
relocate.tbl_es <- function(.data, ..., .before, .after) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::rename
#' @export
rename.tbl_es <- function(.data, ...) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::rename_with
#' @export
rename_with.tbl_es <- function(.data, .fn, .cols, ...) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::select
#' @export
select.tbl_es <- function(.data, ...) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::setdiff
#' @export
setdiff.tbl_es <- function(x, y, ...) {
  update_meta(NextMethod(), x)
}

#' @export
dplyr::slice
#' @export
slice.tbl_es <- function(.data, ..., .preserve) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::slice_head
#' @export
slice_head.tbl_es <- function(.data, ..., n, prop) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::slice_max
#' @export
slice_max.tbl_es <- function(.data, order_by, ..., n, prop, with_ties) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::slice_min
#' @export
slice_min.tbl_es <- function(.data, order_by, ..., n, prop, with_ties) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::slice_tail
#' @export
slice_tail.tbl_es <- function(.data, ..., n, prop) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::summarise
#' @export
summarise.tbl_es <- function(.data, ..., .groups) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::transmute
#' @export
transmute.tbl_es <- function(.data, ...) {
  update_meta(NextMethod(), .data)
}

#' @export
dplyr::ungroup
#' @export
ungroup.tbl_es <- function(x, ...) {
  update_meta(NextMethod(), x)
}

#' @export
dplyr::union
#' @export
union.tbl_es <- function(x, y, ...) {
  update_meta(NextMethod(), x)
}

#' @export
dplyr::union_all
#' @export
union_all.tbl_es <- function(x, y, ...) {
  update_meta(NextMethod(), x)
}

# tidyr ------------------------------------------------------------------------

# There is currently an issue with tidyr using tidyselect within a call to
# NextMethod() for subclasses. Implement work around by manually removing class
# and embracing required args.
#
# https://github.com/RConsortium/OOP-WG/issues/119

#' @export
tidyr::complete
#' @export
complete.tbl_es <- function(data, ..., fill = list()) {
  out <- data
  class(out) <- setdiff(class(data), 'tbl_es')

  update_meta(
    new = complete(
      out,
      ...,
      fill = fill
    ),
    old = data
  )
}

#' @export
tidyr::drop_na
#' @export
drop_na.tbl_es <- function(data, ...) {
  out <- data
  class(out) <- setdiff(class(data), 'tbl_es')

  update_meta(
    new = drop_na(
      data = out,
      ...
    ),
    old = data
  )
}

#' @export
tidyr::expand
#' @export
expand.tbl_es <- function(
  data,
  ...,
  .name_repair = "check_unique"
) {
  out <- data
  class(out) <- setdiff(class(data), 'tbl_es')

  update_meta(
    new = expand(
      out,
      ...,
      .name_repair = .name_repair
    ),
    old = data
  )
}


#' @export
tidyr::fill
#' @export
fill.tbl_es <- function(
  data,
  ...,
  .direction = c("down", "up", "downup", "updown")
) {
  out <- data
  class(out) <- setdiff(class(data), 'tbl_es')

  update_meta(
    new = fill(
      out,
      ...,
      .direction = .direction
    ),
    old = data
  )
}

#' @export
tidyr::nest
#' @export
nest.tbl_es <- function(
  .data,
  ...,
  .names_sep = NULL,
  .key = deprecated()
) {
  out <- .data
  class(out) <- setdiff(class(.data), 'tbl_es')

  update_meta(
    new = nest(
      out,
      ...,
      .names_sep = .names_sep,
      .key = .key
    ),
    old = .data
  )
}

#' @export
tidyr::pivot_longer
#' @export
pivot_longer.tbl_es <- function(
  data,
  cols,
  names_to = "name",
  names_prefix = NULL,
  names_sep = NULL,
  names_pattern = NULL,
  names_ptypes = NULL,
  names_transform = NULL,
  names_repair = "check_unique",
  values_to = "value",
  values_drop_na = FALSE,
  values_ptypes = NULL,
  values_transform = NULL,
  ...
) {
  out <- data
  class(out) <- setdiff(class(data), 'tbl_es')

  update_meta(
    new = pivot_longer(
      out,
      {{ cols }},
      names_to = names_to,
      names_prefix = names_prefix,
      names_sep = names_sep,
      names_pattern = names_pattern,
      names_ptypes = names_ptypes,
      names_transform = names_transform,
      names_repair = names_repair,
      values_to = values_to,
      values_drop_na = values_drop_na,
      values_ptypes = values_ptypes,
      values_transform = values_transform,
      ...
    ),
    old = data
  )
}

#' @export
tidyr::pivot_wider
#' @export
pivot_wider.tbl_es <- function(
  data,
  id_cols = NULL,
  names_from,
  names_prefix = "",
  names_sep = "_",
  names_glue = NULL,
  names_sort = FALSE,
  names_repair = "check_unique",
  values_from,
  values_fill = NULL,
  values_fn = NULL,
  ...
) {
  out <- data
  class(out) <- setdiff(class(data), 'tbl_es')

  update_meta(
    new = pivot_wider(
      out,
      id_cols = id_cols,
      names_from = {{ names_from }},
      names_prefix = names_prefix,
      names_sep = names_sep,
      names_glue = names_glue,
      names_sort = names_sort,
      names_repair = names_repair,
      values_from = {{ values_from }},
      values_fill = values_fill,
      values_fn = values_fn,
      ...
    ),
    old = data
  )
}

#' @export
tidyr::replace_na
#' @export
replace_na.tbl_es <- function(data, replace = list(), ...) {
  out <- data
  class(out) <- setdiff(class(data), 'tbl_es')

  update_meta(
    new = replace_na(
      out,
      replace = replace
    ),
    old = data
  )
}

#' @export
tidyr::separate
#' @export
separate.tbl_es <- function(
  data,
  col,
  into,
  sep = '[^[:alnum:]]+',
  remove = TRUE,
  convert = FALSE,
  extra = 'warn',
  fill = 'warn',
  ...
) {
  out <- data
  class(out) <- setdiff(class(data), 'tbl_es')

  update_meta(
    new = separate(
      data = out,
      col = {{ col }},
      into = {{ into }},
      sep = sep,
      remove = remove,
      convert = convert,
      extra = extra,
      ...
    ),
    old = data
  )
}

# utils ------------------------------------------------------------------------

#' @export
utils::head
#' @export
head.tbl_es <- function(x, ...) {
  update_meta(NextMethod(), x)
}

#' @export
utils::tail
#' @export
tail.tbl_es <- function(x, ...) {
  update_meta(NextMethod(), x)
}
