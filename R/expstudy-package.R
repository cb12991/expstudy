#' expstudy: experience study tools for analytics and communication
#'
#' @details
#' The `expstudy` package provides an subclass, [tbl_es], for
#' [dtplyr::lazy_dt()] that stores attributes relating to an experience study.
#' These attributes along with other functions reduce the time needed to
#' review an assumption via actual-to-expected analysis in addition
#' to exploring new relevant patterns or correlations.
#'
#' @keywords internal
#'
#' @aliases NULL expstudy-package
"_PACKAGE"

## usethis namespace: start
#' @importFrom assertthat assert_that
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr any_of
#' @importFrom dplyr arrange
#' @importFrom dplyr as_tibble
#' @importFrom dplyr collect
#' @importFrom dplyr compute
#' @importFrom dplyr count
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr group_map
#' @importFrom dplyr group_modify
#' @importFrom dplyr intersect
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr relocate
#' @importFrom dplyr rename
#' @importFrom dplyr rename_with
#' @importFrom dplyr select
#' @importFrom dplyr setdiff
#' @importFrom dplyr slice
#' @importFrom dplyr slice_head
#' @importFrom dplyr slice_max
#' @importFrom dplyr slice_min
#' @importFrom dplyr slice_tail
#' @importFrom dplyr summarise
#' @importFrom dplyr transmute
#' @importFrom dplyr ungroup
#' @importFrom dplyr union
#' @importFrom dplyr union_all
#' @importFrom dtplyr lazy_dt
#' @importFrom glue glue
#' @importFrom glue glue_collapse
#' @importFrom lifecycle deprecated
#' @importFrom methods setOldClass
#' @importFrom pillar glimpse
#' @importFrom purrr discard
#' @importFrom purrr list_merge
#' @importFrom purrr lmap_if
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr map_df
#' @importFrom purrr map_lgl
#' @importFrom purrr map2
#' @importFrom purrr modify_depth
#' @importFrom purrr reduce
#' @importFrom purrr walk
#' @importFrom rlang %||%
#' @importFrom rlang abort
#' @importFrom rlang as_quosures
#' @importFrom rlang as_string
#' @importFrom rlang ensyms
#' @importFrom rlang exec
#' @importFrom rlang is_empty
#' @importFrom rlang is_missing
#' @importFrom rlang is_null
#' @importFrom rlang list2
#' @importFrom rlang quo_is_null
#' @importFrom rlang quos_auto_name
#' @importFrom rlang set_names
#' @importFrom rlang squash
#' @importFrom rlang squash_chr
#' @importFrom rlang syms
#' @importFrom stringr str_wrap
#' @importFrom tibble tibble
#' @importFrom tidyr complete
#' @importFrom tidyr drop_na
#' @importFrom tidyr expand
#' @importFrom tidyr fill
#' @importFrom tidyr nest
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr replace_na
#' @importFrom tidyr separate
#' @importFrom tidyselect all_of
#' @importFrom tidyselect any_of
#' @importFrom tidyselect everything
#' @importFrom utils head
#' @importFrom utils tail
## usethis namespace: end
NULL
