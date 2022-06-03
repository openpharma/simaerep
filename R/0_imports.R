# satisfy lintr
# lintr falsely flags possibly_ecdf as unused variable
# using rlang::.data here causes error with furrr in sim_test_data_portfolio
# patnum, n_ae, visit

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("possibly_ecdf", "patnum", "n_ae", "visit"))
}

#' @importFrom progressr progressor
#' @importFrom cowplot get_legend plot_grid ggdraw draw_label plot_grid plot_grid
#' @importFrom cowplot ggdraw draw_label
#' @importFrom forcats fct_relevel
#' @importFrom RColorBrewer brewer.pal
#' @importFrom utils head
#' @importFrom stats p.adjust quantile median runif poisson.test ecdf rnorm rpois sd
#' @importFrom purrr safely possibly pmap map map2 pmap_dbl map2_dbl map_dbl
#' @importFrom purrr map_int map_chr
#' @importFrom furrr future_map future_pmap furrr_options
#' @importFrom progressr with_progress
#' @importFrom stringr str_count str_pad str_length
#' @importFrom rlang := .data enexpr env_has
#' @importFrom dplyr select mutate filter summarise group_by summarise_all summarise_at
#' @importFrom dplyr mutate_all mutate_at ungroup vars bind_cols bind_rows pull
#' @importFrom dplyr n_distinct distinct arrange right_join left_join inner_join
#' @importFrom dplyr rename sample_n between row_number dense_rank desc case_when
#' @importFrom dplyr group_by_at n is_grouped_df everything one_of lag any_of across
#' @importFrom dplyr lead all_equal
#' @importFrom tidyr tibble unnest nest fill
#' @importFrom knitr kable
#' @importFrom tibble tibble
NULL
