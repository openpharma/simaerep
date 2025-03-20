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

test_that("plot_study displays pvals when pval = TRUE", {
  plot_check <- plot_study(df_visit_test, df_site_test, df_eval_test, study = "A", pval = TRUE)
  tab <- plot_check$layers[[2]]$geom_params$grob$grobs
  tab <- tab[[6]][["children"]][[4]][["children"]][[1]][["children"]][["layout"]]$grobs
  pval1 <- tab[[2]][["children"]][[9]][["children"]][[1]][["children"]][[2]][["children"]][[1]][["label"]]
  expect_true(pval1 == "100 %")

  pval2 <- tab[[3]][["children"]][[9]][["children"]][[1]][["children"]][[2]][["children"]][[1]][["label"]]
  expect_true(pval2 == "100 %")
})

test_that("plot_study() displays alert levels when df_al parameter is not Null", {
  df_al_test <- data.frame(study_id = "A", site_number = c("S0001", "S0002"),
                           alert_level_site = c(11, 22), alert_level_study = 12345)
  tab <- plot_study(df_visit_test, df_site_test, df_eval_test, study = "A", df_al = df_al_test)
  tab <- tab$layers[[2]]$geom_params$grob$grobs
  tab2 <- tab[[6]][["children"]][[3]][["children"]][[1]][["children"]][["layout"]]$grobs
  lab1 <- tab2[[6]][["children"]][[7]][["children"]][[1]][["children"]][[2]][["children"]][[1]][["label"]]
  expect_true(lab1 == "12345")

  tab3 <- tab[[6]][["children"]][[4]][["children"]][[1]][["children"]][["layout"]]$grobs
  lab2 <- tab3[[2]][["children"]][[8]][["children"]][[1]][["children"]][[2]][["children"]][[1]][["label"]]
  expect_true(lab2 == "11")

  lab3 <- tab3[[3]][["children"]][[8]][["children"]][[1]][["children"]][[2]][["children"]][[1]][["label"]]
  expect_true(lab3 == "22")
})

test_that("plot_study() produces plots for the top 16 sites if none meet the prob threshold", {
  df_eval_test_prob <- df_eval_test
  df_eval_test_prob ["prob_low_prob_ur"] <- 0
  tab <- plot_study(df_visit_test, df_site_test, df_eval_test_prob, study = "A")
  tab <- tab[["layers"]][[2]][["geom_params"]][["grob"]]$grobs
  len <- length(tab[[6]][["children"]][[4]][["children"]][[1]][["children"]][["layout"]]$grobs)
  expect_true(len == 49)
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
  expect_error(plot(aerep, what = "ur", study = "A", event_names = events, plot_event = "x"),
               regexp = error, fixed = TRUE)
  expect_error(plot(aerep, what = "med75", study = "A", event_names = events, plot_event = "x"),
               regexp = error, fixed = TRUE)

})
