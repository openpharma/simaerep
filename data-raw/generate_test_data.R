## code to prepare `DATASET` dataset goes here
library(simaerep)
library(tidyverse)

# regular test case

set.seed(1)

df_visit1 <- sim_test_data_study(
  n_pat = 100,
  n_sites = 5,
  frac_site_with_ur = 0.4,
  ur_rate = 0.6
)

df_visit1$study_id <- "A"

set.seed(2)
df_visit2 <- sim_test_data_study(n_pat = 100, n_sites = 5,
                                 frac_site_with_ur = 0.2, ur_rate = 0.1)

df_visit2$study_id <- "B"

df_visit_test <- dplyr::bind_rows(df_visit1, df_visit2)

df_site_test <- site_aggr(df_visit_test)

df_sim_sites_test <- sim_sites(df_site_test, df_visit_test, r = 100)

df_eval_test <- eval_sites(df_sim_sites_test)


aerep_test <- simaerep(df_visit_test, param_sim_sites = list(r = 100))
visit_test <- orivisit(df_visit_test)

# portfolio performance

df_site_max_test <- df_visit_test %>%
  group_by(study_id, site_number, patnum) %>%
  summarise(max_visit = max(visit),
            max_ae = max(n_ae),
            .groups = "drop")

df_config_test <- simaerep::get_config(
  df_site_max_test,
  anonymize = TRUE,
  min_pat_per_study = 100,
  min_sites_per_study = 5
)

set.seed(1)
df_portf_test <- sim_test_data_portfolio(df_config_test, progress = FALSE)

set.seed(1)
df_scen_test <- sim_ur_scenarios(
  df_portf_test,
  extra_ur_sites = 2,
  ur_rate = c(0.5, 1),
  parallel = FALSE,
  poisson = FALSE,
  prob_lower = TRUE,
  progress = FALSE,
  site_aggr_args = list(method = "med75_adj")
)

df_perf_test <- get_portf_perf(df_scen_test)

# validation test cases

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

df_visit_val <- bind_rows(
  study_025,
  study_050,
  study_075,
  study_100
)

usethis::use_data(
  df_visit_test,
  df_site_test,
  df_sim_sites_test,
  df_eval_test,
  df_portf_test,
  df_scen_test,
  df_config_test,
  df_perf_test,
  df_site_max_test,
  df_visit_val,
  aerep_test,
  visit_test,
  overwrite = TRUE,
  internal = TRUE
)
