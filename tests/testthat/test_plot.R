# test data is automatically loaded from R/sysdata.rda, check ./data-raw/generate_test_data.R


test_that("plot_sim_demo() must return ggplot object", {
  p <- plot_sim_examples(size_dots = 4, size_raster_label = 10)
  p
  expect_true(all(c("gg", "ggplot") %in% class(p)))

  p <- plot_sim_examples(substract_ae_per_pat = c(0, 2), size_dots = 5, size_raster_label = 10)
  p
  expect_true(all(c("gg", "ggplot") %in% class(p)))
})




test_that("plot_studies() must return ggplot object", {

  p <- plot_study(df_visit_test, df_site_test, df_eval_test, study = "A")
  p
  p <- plot_study(df_visit_test, df_site_test, df_eval_test, study = "B")
  p

  expect_true(all(c("gg", "ggplot") %in% class(p)))
})


test_that("plot_visit_med75() must return ggplot object", {

  p <- plot_visit_med75(df_visit_test, df_site_test, study_id_str = "A", verbose = FALSE)

  expect_true(all(c("gg", "ggplot") %in% class(p)))
})
#Newly Added
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
  expect_output(plot_visit_med75(df_visit_test, df_site_test, study_id_str = "A", verbose = TRUE), regex = cap)
})
