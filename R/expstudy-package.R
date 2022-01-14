#' expstudy: experience study tools for analytics and communication
#'
#' @details
#' The `expstudy` package provides an subclass, [tbl_es], for
#' [dtplyr::lazy_dt()] that stores attributes relating to an experience study.
#' These attributes along with other functions reduces the time needed to
#' review an assumption's efficacy via actual-to-expected analysis in addition
#' to exploring new relevant patterns or correlations.
#' @keywords internal
#'
#' @aliases NULL expstudy-package
#' @import rlang dtplyr
#' @importFrom utils head tail
#' @importFrom pillar glimpse
#' @importFrom dplyr %>% across pull as_tibble any_of all_of everything arrange
#' @importFrom dplyr collect compute count distinct filter group_by
#' @importFrom dplyr group_map group_modify intersect left_join mutate relocate
#' @importFrom dplyr rename rename_with select setdiff slice slice_head
#' @importFrom dplyr slice_max slice_min slice_tail summarise transmute ungroup
#' @importFrom dplyr union union_all
#' @importFrom data.table as.data.table
#' @importFrom tidyr pivot_longer pivot_wider complete drop_na expand fill nest
#' @importFrom tidyr replace_na separate
#' @importFrom glue glue_collapse
#' @importFrom assertthat assert_that
#' @importFrom stringr str_wrap
#' @importFrom purrr map map2 map_chr map_lgl map_df lmap_if list_merge discard
#' @importFrom purrr reduce modify_depth
#'
'_PACKAGE'
