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
tidyr::complete
#' @export
complete.tbl_es <- function(data, ..., fill) {
  update_meta(NextMethod(), data)
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
tidyr::drop_na
#' @export
drop_na.tbl_es <- function(data, ...) {
  update_meta(NextMethod(), data)
}

#' @export
tidyr::expand
#' @export
expand.tbl_es <- function(data, ..., .name_repair) {
  update_meta(NextMethod(), data)
}


#' @export
tidyr::fill
#' @export
fill.tbl_es <- function(data, ..., .direction) {
  update_meta(NextMethod(), data)
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
utils::head
#' @export
head.tbl_es <- function(x, ...) {
  update_meta(NextMethod(), x)
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
tidyr::nest
#' @export
nest.tbl_es <- function(.data, ..., .names_sep, .key) {
  update_meta(NextMethod(), .data)
}

#' @export
tidyr::pivot_longer
#' @export
pivot_longer.tbl_es <- function(
  data, cols, names_to, names_prefix, names_sep, names_pattern, names_ptypes,
  names_transform, names_repair, values_to, values_drop_na, values_ptypes,
  values_transform, ...
) {
  update_meta(NextMethod(), data)
}

#' @export
tidyr::pivot_wider
#' @export
pivot_wider.tbl_es <- function(
  data, id_cols, names_from, names_prefix, names_sep, names_glue, names_sort,
  names_repair, values_from, values_fill, values_fn, ...
) {
  update_meta(NextMethod(), data)
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
tidyr::replace_na
#' @export
replace_na.tbl_es <- function(data, replace, ...) {
  update_meta(NextMethod(), data)
}

#' @export
dplyr::select
#' @export
select.tbl_es <- function(.data, ...) {
  update_meta(NextMethod(), .data)
}

#' @export
tidyr::separate
#' @export
separate.tbl_es <- function(
  data, col, into, sep, remove, convert, extra, fill, ...
) {
  update_meta(NextMethod(), data)
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
utils::tail
#' @export
tail.tbl_es <- function(x, ...) {
  update_meta(NextMethod(), x)
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
