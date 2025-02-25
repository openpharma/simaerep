

# The principles of the algorithm have been published and peer reviewed thus the
# statistical core principles are out of scope of the validation.
#
# Instead we will formulate expectations from the stakeholders point of view for
# which we will then write validation test cases:
#
# We expect that sites that report fewer AEs than other sites in the same study
# have higher AE under-reporting probabilities. We expect that sites with high
# AE under-reporting probabilities visually stand out when their cumulative AE
# count is plotted against the average AE count of all sites.
#
# - Simulate 4 Studies with 100 patients over 10 sites with 10 patients per site
#   for which 3 sites are under-reporting AEs with either 25%, 50%, 75% or 100%.
# - Test Simulation, count AEs at all sites, check that AE under-reporting sites
#   have fewer AEs as expected
# - Apply {simaerep}
# - Test AE under-reporting probability, needs to increase for sites with higher
#   percentages of AE under-reporting.
# - Generate Plots for Test Cases
# - Visually inspect plots and set them up as reference plots for unit testing.
#   Plot integrity will be tested, every change on the plots requires repeated
#   visual inspection.

# test data is automatically loaded, check ./data-raw/generate_test_data.R


test_that(paste("validation requirement - under-reporting probability must",
                "negatively correlate with low AE per visit rates using validation",
                "test set with 25, 50, 75, 100% simulated AE under-reporting"), {

  aerep <- simaerep(df_visit_val)

  df_comp <- df_visit_val %>%
    select(study_id, site_number, is_ur, ae_per_visit_mean) %>%
    distinct() %>%
    left_join(aerep$df_eval, by = c("study_id", "site_number")) %>%
    filter(is_ur)

  correl <- cor.test(df_comp$ae_per_visit_mean, df_comp$prob_low_prob_ur)

  expect_true(correl$statistic < 0)
  expect_true(correl$p.value < 0.05)

})


test_that(paste("validation requirement - visually inspect plotted AE under-reporting",
                "and Ae under-reporting probabilities using validation",
                "test set with 25, 50, 75, 100% simulated AE under-reporting"), {

  skip_on_cran() # graphics engines on CRAN not always compatible

  aerep <- simaerep(df_visit_val)

  for (study in c("study_025", "study_050", "study_075", "study_100")) {
    # expect_snapshot_file will return different results on different systems
    vdiffr::expect_doppelganger(
      study,
      plot(aerep, study = study)
    )
  }

})


test_that(paste("validation requirement - check reproducibililty of validation",
                "test set with 25, 50, 75, 100% simulated AE under-reporting"), {

  set.seed(1)

  study_025 <- sim_test_data_study(
    n_pat = 200,
    n_sites = 20,
    frac_site_with_ur = 0.3,
    ur_rate = 0.25
  )

  study_025$study_id <- "study_025"

  study_050 <- sim_test_data_study(
    n_pat = 200,
    n_sites = 20,
    frac_site_with_ur = 0.3,
    ur_rate = 0.5
  )

  study_050$study_id <- "study_050"

  study_075 <- sim_test_data_study(
    n_pat = 200,
    n_sites = 20,
    frac_site_with_ur = 0.3,
    ur_rate = 0.75
  )

  study_075$study_id <- "study_075"


  study_100 <- sim_test_data_study(
    n_pat = 200,
    n_sites = 20,
    frac_site_with_ur = 0.3,
    ur_rate = 1
  )

  study_100$study_id <- "study_100"

  df_visit_val_check <- bind_rows(
    study_025,
    study_050,
    study_075,
    study_100
  )

  expect_equal(df_visit_val, df_visit_val_check)

})
