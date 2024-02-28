# satisfy CMDcheck
# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when # nolint
# TODO: possible to avoid by pre-processing outside ggplot() call and using aes_str() instead of aes()
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "color_prob_cut", "label", "max_x", "max_y", "mean_ae", "min_x",
                            "min_y", "n_ae", "n_ae_site", "n_ae_study", "n_pat", "patnum",
                            "prob_low_prob_ur", "pval_prob_ur", "ratio_cut", "site",
                            "title_add", "visit", "x", "y"))
}

#' @title Plots AE per site as dots.
#' @description This plot is meant to supplement the package documentation.
#' @param df dataframe, cols = c('site', 'patients', 'n_ae')
#' @param nrow integer, number of rows, Default: 10
#' @param ncols integer, number of columns, Default: 10
#' @param col_group character, grouping column, Default: 'site'
#' @param thresh numeric, threshold to determine color of mean_ae annotation, Default: NULL
#' @param color_site_a character, hex color value, Default: '#BDBDBD'
#' @param color_site_b character, hex color value, Default: '#757575'
#' @param color_site_c character, hex color value, Default: 'gold3'
#' @param color_high character, hex color value, Default: '#00695C'
#' @param color_low character, hex color value, Default: '#25A69A'
#' @param size_dots integer, Default: 10
#' @return ggplot object
#' @examples
#' study <- tibble::tibble(
#'   site = LETTERS[1:3],
#'   patients = c(list(seq(1, 50, 1)), list(seq(1, 40, 1)), list(seq(1, 10, 1)))
#' ) %>%
#'   tidyr::unnest(patients) %>%
#'   dplyr::mutate(n_ae = as.integer(runif(min = 0, max = 10, n = nrow(.))))
#'
#' plot_dots(study)
#' @rdname plot_dots
#' @export
plot_dots <- function(df,
                      nrow = 10,
                      ncols = 10,
                      col_group = "site",
                      thresh = NULL,
                      color_site_a = "#BDBDBD",
                      color_site_b = "#757575",
                      color_site_c = "gold3",
                      color_high = "#00695C",
                      color_low = "#25A69A",
                      size_dots = 10) {
  df <- df %>%
    mutate(x = rep(seq(1, ncols, 1), nrow)) %>%
    group_by(.data$x) %>%
    mutate(y = row_number()) %>%
    ungroup() %>%
    mutate(
      y = nrow - .data$y
    ) %>%
    group_by_at(vars(one_of(col_group))) %>%
    mutate(
      mean_ae = round(mean(.data$n_ae), 1),
      min_x = min(.data$x),
      min_y = min(.data$y),
      max_x = max(.data$x),
      max_y = max(.data$y)
    ) %>%
    ungroup() %>%
    mutate(color = case_when(
      .data$site == "A" ~ color_site_a,
      .data$site == "B" ~ color_site_b,
      .data$site == "C" ~ color_site_c
    ))


  df_label <- df %>%
    group_by_at(vars(one_of(col_group))) %>%
    summarise(
      x = max(.data$x),
      y = max(.data$y),
      mean_ae = mean(.data$n_ae),
      label = paste("\u00d8:", round(mean(.data$n_ae), 1))
    )

  if (!is.null(thresh)) {
    df_label <- df_label %>%
      mutate(color = ifelse(.data$mean_ae >= thresh, color_high, color_low))
  } else {
    df_label <- df_label %>%
      mutate(color = "black")
  }

  p <- df %>%
    ggplot(aes(x, y, color = .data$color)) +
    geom_point(size = size_dots) +
    geom_text(aes(label = n_ae),
      color = "black"
    ) +
    geom_rect(aes(
      xmin = min_x - 0.5,
      xmax = max_x + 0.5,
      ymin = min_y - 0.4,
      ymax = max_y + 0.4
    ),
    color = "grey",
    show.legend = FALSE,
    alpha = 0
    ) +
    geom_text(aes(x + 1.5,
      y,
      label = label
    ),
    df_label,
    show.legend = FALSE
    ) +
    theme_minimal() +
    labs(x = "x", y = "y", color = "site") +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      line = element_blank(),
      aspect.ratio = 0.8,
      legend.position = "top"
    ) +
    scale_color_identity(labels = df$site, breaks = df$color, guide = "legend") +
    coord_cartesian(xlim = c(0, ncols + 2))


  return(p)
}


#' @title Plot simulation example.
#' @description This plots supplements the package documentation.
#' @param substract_ae_per_pat integer, subtract aes from patients at site C, Default: 0
#' @param size_dots integer, Default: 10
#' @param size_raster_label integer, Default: 12
#' @param color_site_a character, hex color value, Default: '#BDBDBD'
#' @param color_site_b character, hex color value, Default: '#757575'
#' @param color_site_c character, hex color value, Default: 'gold3'
#' @param color_high character, hex color value, Default: '#00695C'
#' @param color_low character, hex color value, Default: '#25A69A'
#' @param title logical, include title, Default: T
#' @param legend logical, include legend, Default: T
#' @param seed pass seed for simulations Default: 5
#' @return ggplot
#' @details uses [plot_dots()][plot_dots()] and adds 2 simulation panels, uses made-up
#' site config with three sites A,B,C simulating site C
#' @examples
#' \donttest{
#' plot_sim_example(size_dots = 5)
#' }
#' @seealso
#'  \code{\link[cowplot]{get_legend}},\code{\link[cowplot]{plot_grid}}
#' @rdname plot_sim_example
#' @export
plot_sim_example <- function(substract_ae_per_pat = 0,
                             size_dots = 10,
                             size_raster_label = 12,
                             color_site_a = "#BDBDBD",
                             color_site_b = "#757575",
                             color_site_c = "gold3",
                             color_high = "#00695C",
                             color_low = "#25A69A",
                             title = TRUE,
                             legend = TRUE,
                             seed = 5) {
  set.seed(seed)

  study <- tibble(
    site = LETTERS[1:3],
    patients = c(list(seq(1, 50, 1)), list(seq(1, 40, 1)), list(seq(1, 10, 1)))
  ) %>%
    tidyr::unnest("patients") %>%
    mutate(n_ae = as.integer(runif(min = 0, max = 10, n = nrow(.)))) %>%
    mutate(
      n_ae = ifelse(.data$site == "C", .data$n_ae - .env$substract_ae_per_pat, .data$n_ae),
      n_ae = ifelse(.data$n_ae < 0, 0, .data$n_ae)
    )

  rep10 <- study %>%
    sample_n(100, replace = TRUE) %>%
    mutate(rep = rep(seq(1, 10, 1), 10)) %>%
    arrange(.data$rep, .data$site)

  mean_ae_site_c <- study %>%
    group_by(.data$site) %>%
    summarise(mean_ae = mean(.data$n_ae)) %>%
    filter(.data$site == "C") %>%
    .$mean_ae

  p_sites <- plot_dots(study,
    size_dots = size_dots,
    color_site_a = color_site_a,
    color_site_b = color_site_b,
    color_site_c = color_site_c,
    color_high = color_high
  )

  p_sim10 <- plot_dots(rep10,
    col_group = "rep",
    thresh = mean_ae_site_c,
    size_dots = size_dots,
    color_site_a = color_site_a,
    color_site_b = color_site_b,
    color_site_c = color_site_c,
    color_high = color_high
  )

  # plot sim1000 as raster -----------------------------------------------

  rep1000 <- study %>%
    sample_n(10240, replace = TRUE) %>%
    mutate(rep = rep(seq(1, 1024, 1), 10)) %>%
    arrange(.data$rep, .data$site) %>%
    group_by(.data$rep) %>%
    summarise(mean_ae = mean(.data$n_ae))

  prop <- rep1000 %>%
    filter(.data$rep <= 1000) %>%
    mutate(is_under = ifelse(.data$mean_ae <= mean_ae_site_c, 1, 0)) %>%
    summarise(prop = sum(.data$is_under) / nrow(.)) %>%
    .$prop %>%
    round(3)

  p_sim100 <- rep1000 %>%
    mutate(color = ifelse(.data$mean_ae <= mean_ae_site_c,
                          color_low, color_high)) %>%
    mutate(x = rep(seq(1, 32, 1), 32)) %>%
    group_by(.data$x) %>%
    mutate(y = row_number()) %>%
    filter(.data$rep <= 1000) %>%
    ggplot(aes(x, y, fill = .data$color)) +
    geom_raster() +
    scale_fill_identity() +
    theme_minimal() +
    theme(
      aspect.ratio = 1,
      axis.line = element_blank(),
      axis.text = element_blank(),
      line = element_blank()
    ) +
    annotate("text",
      label = paste(prop * 100, "%"),
      alpha = 0.9,
      color = "ivory",
      x = 16,
      y = 16,
      size = size_raster_label
    ) +
    labs(x = " ", y = " ", subtitle = paste(
      "probabilty mean AE of",
      mean_ae_site_c,
      "or lower"
    ))

  # titles ------------------------------------

  if (title) {
    p_sites <- p_sites +
      labs(title = "Total AEs per Patient Until a Given Visit")

    p_sim10 <- p_sim10 +
      labs(title = "10x Simulation of Site C")

    p_sim100 <- p_sim100 +
      labs(title = "1000x Simulation of Site C")
  }


  # rearange legend ----------------------------

  leg <- get_legend(p_sites)

  p_sites <- p_sites +
    theme(legend.position = "none")

  p_sim10 <- p_sim10 +
    theme(legend.position = "none")

  p_dots <- cowplot::plot_grid(p_sites, p_sim10, nrow = 1)

  if (legend) {
    p_dots <- cowplot::plot_grid(p_dots, leg,
      ncol = 1,
      rel_heights = c(10, 1)
    )
  }

  p <- cowplot::plot_grid(p_dots, p_sim100, nrow = 1, rel_widths = c(2, 1))

  return(p)
}

#' @title Plot multiple simulation examples.
#' @description This plot is meant to supplement the package documentation.
#' @param substract_ae_per_pat integer, Default: c(0, 1, 3)
#' @param ... parameters passed to plot_sim_example()
#' @details This function is a wrapper for plot_sim_example()
#' @examples
#' \donttest{
#' plot_sim_examples(size_dot = 3, size_raster_label = 10)
#' plot_sim_examples()
#' }
#' @seealso
#'  \code{\link[cowplot]{ggdraw}},\code{\link[cowplot]{draw_label}},\code{\link[cowplot]{plot_grid}}
#' @rdname plot_sim_examples
#' @return ggplot
#' @export

plot_sim_examples <- function(substract_ae_per_pat = c(0, 1, 3), ...) {
  make_title <- function(x, fontface = "bold", size = 14, angle = 0) {
    cowplot::ggdraw() +
      cowplot::draw_label(
        x,
        fontface = fontface,
        size = size,
        angle = angle
      )
  }

  df <- tibble(
    substract_ae_per_pat = substract_ae_per_pat,
    title = FALSE,
    legend = FALSE
  ) %>%
    mutate(
      legend = FALSE
      , title_add = map_chr(.data$substract_ae_per_pat, ~ paste(
        "-",
        .,
        "AEs per patient at site C"
      )),
      title_add = ifelse(row_number() == 1, " ", title_add),
      title_add = map(.data$title_add, make_title, angle = 90),
      p = pmap(
        list(
          substract_ae_per_pat = substract_ae_per_pat,
          title = .data$title,
          legend = .data$legend
        ),
        plot_sim_example,
        ...
      ),
      p = map2(.data$title_add, .data$p, cowplot::plot_grid, nrow = 1, rel_widths = c(0.05, 1))
    )

  p <- cowplot::plot_grid(plotlist = df$p, ncol = 1)

  # legend -------------------------------------------------------------------
  study <- tibble(
      site = LETTERS[1:3],
      patients = c(list(seq(1, 50, 1)), list(seq(1, 40, 1)), list(seq(1, 10, 1)))
    ) %>%
    tidyr::unnest("patients") %>%
    mutate(n_ae = as.integer(runif(min = 0, max = 10, n = nrow(.))))

  p_legend <- plot_dots(study) +
    guides(color = guide_legend(override.aes = list(size = 2)))

  p_legend <- get_legend(p_legend)

  p_empty <- ggplot() + theme_void()

  legend_pane <- cowplot::plot_grid(
    p_legend,
    p_empty,
    rel_widths = c(2 / 3, 1 / 3),
    nrow = 1
  )

  cowplot::plot_grid(legend_pane)
  # title --------------------------------------------------------------------

  title_1 <- make_title("Total AEs per Patient Until a Given Visit", size = 12)
  title_2 <- make_title("10x Simulation of Site C", size = 12)
  title_3 <- make_title("1000x Simulation of Site C", size = 12)

  title_pane <- cowplot::plot_grid(title_1, title_2, title_3, nrow = 1)

  p <- cowplot::plot_grid(title_pane, p, legend_pane,
    ncol = 1,
    rel_heights = c(1, 20, 0.5)
  )

  return(p)
}

#' replace cowplot::get_legend, to silence warning
#' Multiple components found; returning the first one. To return all, use `return_all = TRUE
#' @keywords internal
get_legend <- function(p) {
  leg <- cowplot::get_plot_component(p, pattern = "guide-box", return_all = TRUE)

  if (is.list(leg)) {
    leg <- leg[[1]]
  }

  return(leg)
}


#' @title Plot ae development of study and sites highlighting at risk sites.
#' @description Most suitable visual representation of the AE under-reporting statistics.
#' @param df_visit dataframe, created by [sim_sites()][sim_sites()]
#' @param df_site dataframe created by [site_aggr()][site_aggr()]
#' @param df_eval dataframe created by [eval_sites()][eval_sites()]
#' @param df_al dataframe containing study_id, site_number, alert_level_site,
#'   alert_level_study (optional), Default: NA
#' @param study study
#' @param n_sites integer number of most at risk sites, Default: 16
#' @param pval logical show p-value, Default:FALSE
#' @param prob_col character, denotes probability column, Default: "prob_low_prob_ur"
#' @return ggplot
#' @details Left panel shows mean AE reporting per site (lightblue and darkblue
#'   lines) against mean AE reporting of the entire study (golden line). Single
#'   sites are plotted in descending order by AE under-reporting probability on
#'   the right panel in which grey lines denote cumulative AE count of single
#'   patients. Grey dots in the left panel plot indicate sites that were picked
#'   for single plotting. AE under-reporting probability of dark blue lines
#'   crossed threshold of 95%. Numbers in the upper left corner indicate the
#'   ratio of patients that have been used for the analysis against the total
#'   number of patients. Patients that have not been on the study long enough to
#'   reach the evaluation point (visit_med75) will be ignored.
#' @examples
#' \donttest{
#' df_visit <- sim_test_data_study(n_pat = 1000, n_sites = 10,
#'     frac_site_with_ur = 0.2, ur_rate = 0.15, max_visit_sd = 8)
#'
#' df_visit$study_id <- "A"
#' df_site <- site_aggr(df_visit)
#'
#' df_sim_sites <- sim_sites(df_site, df_visit, r = 100)
#'
#' df_eval <- eval_sites(df_sim_sites)
#'
#' plot_study(df_visit, df_site, df_eval, study = "A")
#' }
#' @rdname plot_study
#' @export
#' @import ggplot2
plot_study <- function(df_visit,
                       df_site,
                       df_eval,
                       study,
                       df_al = NULL,
                       n_sites = 16,
                       pval = FALSE,
                       prob_col = "prob_low_prob_ur") {

  # TODO: parametrize scores, fix legend

  df_visit <- check_df_visit(df_visit)

  stopifnot(study %in% unique(df_visit$study_id))
  stopifnot(study %in% unique(df_site$study_id))
  stopifnot(study %in% unique(df_eval$study_id))

  # alert level -------------------------------------------------------------

  if (is.null(df_al)) {
    df_visit <- df_visit %>%
      mutate(alert_level_site = NA,
             alert_level_study = NA)
  } else {
    df_visit <- df_visit %>%
      left_join(select(df_al, c(
                       "study_id",
                       "site_number",
                       "alert_level_site",
                       "alert_level_study")))
  }

  # fill in pvalues when missing --------------------------------------------

  if (! "pval_prob_ur" %in% colnames(df_eval)) {
    df_eval <- df_eval %>%
      mutate(pval_prob_ur = NA)
  }

  # make sure ids are character columns

  df_visit <- df_visit %>%
    mutate_at(vars(c("study_id", "patnum", "site_number")), as.character)

  df_eval <- df_eval %>%
    mutate_at(vars(c("study_id", "site_number")), as.character)

  df_site <- df_site %>%
    mutate_at(vars(c("study_id", "site_number")), as.character)

  # filter studies -----------------------------------------------------------

  df_visit <- df_visit %>%
    filter(.data$study_id %in% study)

  df_site <- df_site %>%
    filter(.data$study_id %in% study)

  df_eval <- df_eval %>%
    filter(.data$study_id %in% study)

  # ordered sites -------------------------------------------------------------

  n_site_ur_gr_0p5 <- df_eval %>%
    filter(.data[[prob_col]] > 0.5) %>%
    nrow()

  if (n_site_ur_gr_0p5 > 0) {
    sites_ordered <- df_eval %>%
      arrange(.data$study_id, desc(.data[[prob_col]]), .data$mean_ae_site_med75) %>%
      filter(.data[[prob_col]] > 0.5) %>%
      head(n_sites) %>%
      .$site_number
  } else {
    sites_ordered <- df_eval %>%
      arrange(.data$study_id, desc(.data[[prob_col]]), .data$mean_ae_site_med75) %>%
      head(6) %>%
      .$site_number
  }

  # mean AE development ------------------------------------------------------

  df_mean_ae_dev_site <- df_visit %>%
    group_by(.data$study_id, .data$site_number, .data$patnum) %>%
    mutate(max_visit_per_pat = max(.data$visit)) %>%
    ungroup() %>%
    left_join(df_site, by = c("study_id", "site_number")) %>%
    filter(.data$visit <= .data$visit_med75, .data$max_visit_per_pat >= .data$visit_med75) %>%
    group_by(.data$study_id,
             .data$site_number,
             .data$visit_med75,
             .data$n_pat,
             .data$visit,
             .data$alert_level_site) %>%
    summarise(mean_ae = mean(.data$n_ae)) %>%
    ungroup()

  df_mean_ae_dev_study <- df_visit %>%
    group_by(.data$study_id,
             .data$site_number,
             .data$patnum) %>%
    mutate(max_visit_per_pat = max(.data$visit)) %>%
    group_by(.data$study_id) %>%
    mutate(
      visit_med75 = ceiling(median(.data$max_visit_per_pat) * 0.75),
      n_pat = n_distinct(.data$patnum)
    ) %>%
    ungroup() %>%
    filter(.data$visit <= .data$visit_med75, .data$max_visit_per_pat >= .data$visit_med75) %>%
    group_by(.data$study_id) %>%
    mutate(n_pat_with_med75 = n_distinct(.data$patnum)) %>%
    group_by(.data$study_id,
             .data$visit,
             .data$n_pat,
             .data$n_pat_with_med75) %>%
    summarise(mean_ae = mean(.data$n_ae)) %>%
    ungroup()

  df_ae_dev_patient <- df_visit %>%
    select(c("study_id",
           "site_number",
           "patnum",
           "visit",
           "n_ae")) %>%
    mutate(max_visit_per_pat = max(.data$visit)) %>%
    filter(.data$site_number %in% sites_ordered) %>%
    ungroup() %>%
    mutate(site_number = fct_relevel(.data$site_number, sites_ordered))

  # define score cut-offs + labels----------------------------------------------

  palette <- RColorBrewer::brewer.pal(9, "Blues")[c(3, 5, 7, 9)]
  breaks <- c(0, 0.5, 0.75, 0.95, ifelse(max(df_eval[[prob_col]], na.rm = TRUE) > 0.95,
                                    max(df_eval[[prob_col]], na.rm = TRUE) + 0.1,
                                    NA)
              )

  breaks <- breaks[! is.na(breaks)]

  df_eval <- df_eval %>%
    mutate(
      prob_cut = cut(.data[[prob_col]], breaks = breaks, include.lowest = TRUE),
      color_prob_cut = palette[as.numeric(.data$prob_cut)]
    )

  df_mean_ae_dev_site <- df_mean_ae_dev_site %>%
    select(- "visit_med75") %>%
    left_join(df_eval, by = c("study_id", "site_number"))

  # we have to split site ae dev up because alert sites get plotted
  # over if there are many sites

  df_mean_ae_dev_site_no_alert <- df_mean_ae_dev_site %>%
    filter(.data$prob_cut == levels(.data$prob_cut)[1])

  df_mean_ae_dev_site_alert <- df_mean_ae_dev_site %>%
    filter(.data$prob_cut != levels(.data$prob_cut)[1])

  df_alert <- df_visit %>%
    select(c("study_id",
           "site_number",
           "alert_level_site",
           "alert_level_study")) %>%
    distinct()

  df_label <- df_mean_ae_dev_site %>%
    filter(.data$visit == .data$visit_med75, .data$site_number %in% sites_ordered) %>%
    select(c("study_id",
           "site_number",
           "visit",
           "mean_ae")) %>%
    left_join(df_site, by = c("study_id", "site_number")) %>%
    select(c("study_id",
           "site_number",
           "visit",
           "mean_ae",
           "n_pat",
           "n_pat_with_med75")) %>%
    left_join(
      select(df_eval, - c(
              "n_pat",
              "n_pat_with_med75"))
      , by = c("study_id", "site_number")
    ) %>%
    left_join(df_alert, by = c("study_id", "site_number")) %>%
    select(any_of(c(
      "study_id",
      "site_number",
      "visit",
      "mean_ae",
      "n_pat",
      "n_pat_with_med75",
      prob_col,
      "prob_cut",
      "color_prob_cut",
      "pval_prob_ur",
      "alert_level_site"
    ))) %>%
    mutate(
      site_number = fct_relevel(.data$site_number, sites_ordered),
      color_alert_level = case_when(
        .data$alert_level_site == 2 ~ "tomato",
        .data$alert_level_site == 1 ~ "orange",
        .data$alert_level_site == 0 ~ "blue",
        TRUE ~ "grey"
      )
    )

  # study plot -----------------------------------------------------------------

  max_visit_study <- max(df_mean_ae_dev_site$visit)
  max_ae_study <- max(df_mean_ae_dev_site$mean_ae)
  n_pat <- unique(df_mean_ae_dev_study$n_pat)
  n_pat_with_med75 <- unique(df_mean_ae_dev_study$n_pat_with_med75)
  alert_level_study <- round(unique(df_visit$alert_level_study), 1)
  color_alert_level_study <- case_when(
    round(alert_level_study, 0) == 2 ~ "tomato",
    round(alert_level_study, 0) == 1 ~ "orange",
    round(alert_level_study, 0) == 0 ~ "blue",
    TRUE ~ "grey"
  )

  p_study <- df_mean_ae_dev_site_no_alert %>%
    ggplot(aes(visit, mean_ae), na.rm = TRUE) +
    geom_line(aes(
      group = .data$site_number,
      color = color_prob_cut
    )) +
    geom_line(aes(
        group = .data$site_number,
        color = color_prob_cut
      ),
      data = df_mean_ae_dev_site_alert,
      linewidth = 1
    ) +
    geom_line(aes(group = .data$study_id),
      data = df_mean_ae_dev_study,
      color = "gold3",
      linewidth = 1,
      alpha = 0.5
    ) +
    geom_point(
      data = df_label,
      color = "grey"
    ) +
    annotate("text",
      x = 0.2 * max_visit_study,
      y = 0.9 * max_ae_study,
      label = paste0(n_pat_with_med75, "/", n_pat),
      na.rm = TRUE
    ) +
    annotate("label",
      x = 0.5 * max_visit_study,
      y = 0.9 * max_ae_study,
      label = alert_level_study,
      color = color_alert_level_study,
      na.rm = TRUE
    ) +
    labs(color = "Probability Under-Reporting") +
    scale_color_identity() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(y = "Mean Cumulative AE Count per Site")

  if (length(sites_ordered) == 0) {
    message("no sites with P/FP ratio > 1")
    return(p_study)
  }

  # site plot -------------------------------------------------------------------

  df_mean_ae_dev_site <- df_mean_ae_dev_site %>%
    filter(.data$site_number %in% sites_ordered) %>%
    mutate(site_number = fct_relevel(.data$site_number, sites_ordered))

  max_visit <- max(df_ae_dev_patient$visit)
  max_ae <- max(df_ae_dev_patient$n_ae)

  max_ae <- ifelse(max_ae < max(df_mean_ae_dev_study$mean_ae),
    max(df_mean_ae_dev_study$mean_ae),
    max_ae
  )

  p_site <- df_ae_dev_patient %>%
    ggplot(aes(visit, n_ae), na.rm = TRUE) +
    geom_line(aes(group = patnum),
      color = "grey",
      alpha = 0.5
    ) +
    geom_line(aes(
        y = mean_ae,
        color = color_prob_cut,
        alpha = 0.5
      ),
      data = df_mean_ae_dev_site,
      linewidth = 1
    ) +
    geom_line(aes(y = mean_ae),
              data = df_mean_ae_dev_study,
              color = "gold3",
              linewidth = 1,
              alpha = 0.5) +
    geom_text(aes(label = paste0(n_pat_with_med75, "/", n_pat)),
              data = df_label,
              x = 0.2 * max_visit,
              y = 0.9 * max_ae,
              na.rm = TRUE) +
    geom_label(aes(label = paste(round(.data[[prob_col]], 3) * 100, "%"),
                   color = color_prob_cut),
              data = df_label,
              x = 0.8 * max_visit,
              y = 0.9 * max_ae,
              na.rm = TRUE) +
    geom_label(aes(label = .data$alert_level_site,
                   color = .data$color_alert_level),
              data = df_label,
              x = 0.5 * max_visit,
              y = 0.9 * max_ae,
              alpha = 0.5,
              na.rm = TRUE
              ) +
    facet_wrap(~site_number) +
    scale_color_identity() +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(y = "Cumulative AE Count per Patient")

  if (pval) {
    p_site <- p_site +
      geom_label(aes(label = paste(round(pval_prob_ur, 3) * 100, "%")),
                 data = df_label,
                 x = 0.8 * max_visit,
                 y = 0.7 * max_ae,
                 color = "grey",
                 na.rm = TRUE
      )
  }

  # title -----------------------------------------------------

  make_title <- function(x) {
    cowplot::ggdraw() +
      cowplot::draw_label(
        x,
        fontface = "bold"
      )
  }

  t <- make_title(study)

  # compose plots ---------------------------------------------

  lwr <- cowplot::plot_grid(p_study, p_site, nrow = 1)

  gr <- cowplot::plot_grid(t, lwr, ncol = 1, rel_heights = c(0.05, 1))

  return(gr)
}


#' @title Plot patient visits against visit_med75.
#' @description Plots cumulative AEs against visits for patients at sites of
#'   given study and compares against visit_med75.
#' @param df_visit dataframe
#' @param df_site dataframe, as returned by [site_aggr()][site_aggr()]
#' @param study_id_str character, specify study in study_id column
#' @param n_sites integer, Default: 6
#' @param verbose logical, Default: TRUE
#' @inheritParams site_aggr
#' @return ggplot
#' @examples
#' df_visit <- sim_test_data_study(n_pat = 120, n_sites = 6,
#'     frac_site_with_ur = 0.4, ur_rate = 0.6)
#'
#' df_visit$study_id <- "A"
#' df_site <- site_aggr(df_visit)
#'
#' plot_visit_med75(df_visit, df_site, study_id_str = "A", n_site = 6)
#' @rdname plot_visit_med75
#' @export
plot_visit_med75 <- function(df_visit,
                             df_site = NULL,
                             study_id_str,
                             n_sites = 6,
                             min_pat_pool = 0.2,
                             verbose = TRUE) {

  df_visit <- check_df_visit(df_visit) %>%
    filter(.data$study_id == study_id_str)

  # to suppress warning about unused argument
  df_site_deprecated <- df_site # nolint

  df_pat <- pat_aggr(df_visit)

  df_site_min_med75 <- site_aggr(df_visit, method = "med75")
  df_site_max_med75 <- site_aggr(df_visit, method = "med75_adj")

  study_possible_max_visit <- df_pat %>%
    filter(.data$study_id == study_id_str) %>%
    summarise(study_possible_max_visit = quantile(.data$max_visit_per_pat, 1 - min_pat_pool)) %>%
    pull(.data$study_possible_max_visit) %>%
    round(0)

  df_mean_ae_dev <- get_site_mean_ae_dev(df_visit, df_pat, df_site_max_med75)

  df_plot <- df_site_min_med75 %>%
    rename(visit_med75_min = "visit_med75") %>%
    left_join(
      select(
        df_site_max_med75, c(
        "study_id",
        "site_number",
        visit_med75_max = "visit_med75"
        )),
      by = c(
        "study_id",
        "site_number"
      )
    ) %>%
    mutate(rnk_sites = rank(desc(.data$n_pat), ties.method = "first")) %>%
    filter(.data$rnk_sites <= n_sites) %>%
    left_join(df_visit, by = c("study_id", "site_number")) %>%
    left_join(select(df_mean_ae_dev, - "visit_med75"),
      by = c("study_id", "site_number", "visit")
    ) %>%
    group_by(.data$study_id, .data$site_number, .data$patnum) %>%
    mutate(
      max_visit_pat = max(.data$visit),
      has_med75 = ifelse(.data$max_visit_pat >= .data$visit_med75_max, "yes", "no")
    ) %>%
    ungroup()

  df_label <- df_plot %>%
    select(c(
      "study_id",
      "site_number",
      "n_pat",
      "n_pat_with_med75"
    )) %>%
    distinct() %>%
    mutate(label = paste0(.data$n_pat_with_med75, "/", .data$n_pat))

  visit_max <- df_plot %>%
    .$max_visit_pat %>%
    max()

  ae_max <- df_plot %>%
    .$n_ae %>%
    max()

  df_mean_ae_dev <- df_mean_ae_dev %>%
    filter(.data$site_number %in% unique(df_plot$site_number))

  p <- df_plot %>%
    ggplot(aes(visit, n_ae)) +
    geom_line(
      aes(group = patnum),
      data = filter(df_plot, .data$has_med75 == "yes"),
      color = "grey"
    ) +
    geom_line(
      aes(y = .data$mean_ae_site, group = .data$site_number),
      data = df_mean_ae_dev,
      color = "slateblue3",
      alpha = 0.5,
      linewidth = 2
    ) +
    geom_line(aes(group = patnum),
      filter(df_plot, .data$has_med75 == "no"),
      color = "black",
      linetype = 2
    ) +
    geom_vline(aes(xintercept = .data$visit_med75_max),
               linetype = 1,
               color = "slateblue3"
    ) +
    geom_vline(
      aes(xintercept = .data$visit_med75_min),
      linetype = 3,
      color = "slateblue3"
    ) +
    geom_vline(
      xintercept = study_possible_max_visit,
      linetype = 2,
      color = "slateblue3"
    ) +
    geom_text(aes(label = label),
      df_label,
      x = 0.15 * visit_max,
      y = 0.75 * ae_max,
      na.rm = TRUE
    ) +
    facet_wrap(study_id ~ site_number) +
    labs(
      title = "Evaluation Point visit_med75",
      y = "# Cumulated AEs"
    ) +
    theme_minimal() +
    theme(plot.caption.position = "panel")
  # nolint start
  cap <- paste(c(
    "purple line:          mean site ae of patients with visit_med75",
    "grey line:            patient included",
    "black dashed line:    patient excluded",
    "dotted vertical line: visit_med75, 0.75 x median of maximum patient visits of site ",
    "solid vertical line:  visit_med75 adjusted, increased to minimum maximum patient visit of included patients",
    "dashed vertical line: maximum value for visit_med75 adjusted, 80% quantile of maximum patient visits of study",
    "\n"

  ), collapse = "\n")
  # nolint end

  if (verbose) {
    cat(cap)
  }

  return(p)
}
