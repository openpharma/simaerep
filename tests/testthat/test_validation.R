

# The principles of the algorithm have been published and peer reviewed thus the
# statistical core principles are out of scope of the validation.
#
# Instead we will formulate expectations from the stakeholders point of view for
# which we will then write validation test cases:
#
# We expect that sites that report fewer event counts have negative reporting probabilities
# while sites with high counts have positive reporting probabilities. When outlying
# sites are plotted we expect them to stand out in respect to their reporting levels.
#
# - We generate two data sets one including under-reporting and one including
#.  over reporting sites.
# - Each data set contains 4 Studies with 500 patients over 50 sites with 10
#   patients per site for which 2 sites are under or over-reporting events with
#.  either 25%, 50%, 75% or 100%.
# - Test Simulation, count events at all sites, check that outlier sites
#   have fewer or more events as expected
# - Apply {simaerep}
# - Test reporting probability, which should be negative for sites with low event
#   counts and positive for sites with high event counts.
# - Generate Plots for Test Cases
# - Visually inspect plots and set them up as reference plots for unit testing.
#   Plot integrity will be tested, every change on the plots requires repeated
#   visual inspection.


df_grid <- tibble(
    factor_event_rate = c(1, 0.75, 0.5, 0.25),
    study_id = c("100", "075", "050", "025")
  ) %>%
  mutate(
    type = list(c(-1, 1))
  ) %>%
  unnest(type) %>%
  mutate(
    study_id = ifelse(type == -1, glue("study_{study_id}_ur"), glue("study_{study_id}_or")),
    factor_event_rate = factor_event_rate * type,
    data = map(
      factor_event_rate,
      ~ sim_test_data_study(
        n_pat = 500,
        n_sites = 50,
        ratio_out = 0.04,
        factor_event_rate = .,
        event_rates = (dgamma(seq(1, 20, 0.5), shape = 5, rate = 2) * 5) + 0.1,
        max_visit = 20,
        max_visit_sd = 10
      )
    ),
    data = map2(data, study_id, ~ mutate(.x, study_id = .y))
  )

df_visit_val <- bind_rows(df_grid$data)

test_that(paste("validation requirement - Event reporting probability must correlate with",
                "with Pearson cofficient > 0.9 and P value < 0.001 with the average event rate",
                "in test set with 25, 50, 75, 100% simulated event over and under-reporting."), {

  evrep <- simaerep(df_visit_val)

  df_comp <- df_visit_val %>%
    distinct(study_id, site_id, is_out, event_per_visit_mean) %>%
    left_join(evrep$df_eval, by = c("study_id", "site_id")) %>%
    filter(is_out)

  correl <- cor.test(df_comp$event_per_visit_mean, df_comp$event_prob)

  expect_true(correl$estimate > 0.9)
  expect_true(correl$p.value < 0.001)

})


test_that(paste("validation requirement - visually inspect plotted event reporting probability",
                "outlier in a test set with 25, 50, 75, 100% over and under-reporting"), {

  skip_on_cran() # graphics engines on CRAN not always compatible

  evrep <- simaerep(df_visit_val)

  for (study in unique(df_visit_val$study_id)) {
    # expect_snapshot_file will return different results on different systems
    vdiffr::expect_doppelganger(
      study,
      plot(evrep, study = study)
    )
  }

})
