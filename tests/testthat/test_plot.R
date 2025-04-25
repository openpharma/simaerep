# test data is automatically loaded from R/sysdata.rda, check ./data-raw/generate_test_data.R

df_visit <- get_df_visit_test()
df_site <- site_aggr(df_visit)
df_sim_sites <- sim_sites(df_site, df_visit, r = 100)
df_eval <- eval_sites(df_sim_sites)

test_that("plot_sim_demo() must return ggplot object", {
  p <- plot_sim_examples(size_dots = 4, size_raster_label = 10)
  p
  expect_true(all(c("gg", "ggplot") %in% class(p)))

  p <- plot_sim_examples(substract_ae_per_pat = c(0, 2), size_dots = 5, size_raster_label = 10)
  p
  expect_true(all(c("gg", "ggplot") %in% class(p)))
})




test_that("plot_studies() must return ggplot object", {

  p <- plot_study(df_visit, df_site_test, df_eval, study = "A", delta = FALSE)
  p
  p <- plot_study(df_visit, df_site_test, df_eval, study = "B", delta = FALSE)
  p

  expect_true(all(c("gg", "ggplot") %in% class(p)))
})


test_that("plot_visit_med75() must return ggplot object", {

  p <- plot_visit_med75(df_visit, df_site_test, study_id_str = "A", verbose = FALSE)

  expect_true(all(c("gg", "ggplot") %in% class(p)))
})

test_that("plot_visit_med75() pastes a caption if verbose is TRUE", {
  cap <- paste(c(
    "purple line:          mean site ae of patients with visit_med75",
    "grey line:            patient included",
    "black dashed line:    patient excluded",
    "dotted vertical line: visit_med75, 0.75 x median of maximum patient visits of site ",
    "solid vertical line:  visit_med75 adjusted, increased to minimum maximum patient visit of included patients",
    "dashed vertical line: maximum value for visit_med75 adjusted, 80% quantile of maximum patient visits of study",
    ""
  ), collapse = "\n")
  expect_output(plot_visit_med75(df_visit, df_site_test, study_id_str = "A", verbose = TRUE), regex = cap)
})

test_that("plot_sim_example() includes titles when title = TRUE", {
  plot_test <- plot_sim_example(title = TRUE)

  lab1 <- plot_test$layers[[1]]$geom_params$grob$grobs
  lab1 <- lab1[[6]][["children"]][[3]][["children"]][[1]][["children"]][["layout"]]$grobs
  title1 <- lab1[[6]][["children"]][[3]][["children"]][[1]][["children"]][["layout"]]$grobs[[21]]$children[[1]]$label
  expect_true(title1 == "Total AEs per Patient Until a Given Visit")

  title2 <- lab1[[6]][["children"]][[4]][["children"]][[1]][["children"]][["layout"]]$grobs[[21]]$children[[1]]$label
  expect_true(title2 == "10x Simulation of Site C")

  title3 <- plot_test$layers[[2]]$geom_params$grob$grob[[21]]$children[[1]]$label
  expect_true(title3 == "1000x Simulation of Site C")

})

test_that("plot_study() produces an error when an invalid plot_event is submitted", {
  events <- c("ae", "y")
  df_visit_events_test <- sim_test_data_events(event_names = events, ae_per_visit_mean = c(0.5, 0.4))
  aerep <- simaerep(
    df_visit = df_visit_events_test,
    event_names = events,
    under_only = FALSE,
    inframe = TRUE,
    mult_corr = TRUE
  )
  error <- "plot_event (x) not found within event_names"
  expect_error(plot(aerep, what = "ur", study = "A", plot_event = "x"),
               regexp = error, fixed = TRUE)
  expect_error(plot(aerep, what = "med75", study = "A", plot_event = "x"),
               regexp = error, fixed = TRUE)

})



