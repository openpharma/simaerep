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
