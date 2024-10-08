% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_test_data.R
\name{sim_ur_scenarios}
\alias{sim_ur_scenarios}
\title{Simulate Under-Reporting Scenarios}
\usage{
sim_ur_scenarios(
  df_portf,
  extra_ur_sites = 3,
  ur_rate = c(0.25, 0.5),
  r = 1000,
  poisson_test = FALSE,
  prob_lower = TRUE,
  parallel = FALSE,
  progress = TRUE,
  site_aggr_args = list(),
  eval_sites_args = list(),
  check = TRUE
)
}
\arguments{
\item{df_portf}{dataframe as returned by \code{\link{sim_test_data_portfolio}}}

\item{extra_ur_sites}{numeric, set maximum number of additional
under-reporting sites, see details Default: 3}

\item{ur_rate}{numeric vector, set under-reporting rates for scenarios
Default: c(0.25, 0.5)}

\item{r}{integer, denotes number of simulations, default = 1000}

\item{poisson_test}{logical, calculates poisson.test pvalue}

\item{prob_lower}{logical, calculates probability for getting a lower value}

\item{parallel}{logical, use parallel processing see details, Default: FALSE}

\item{progress}{logical, show progress bar, Default: TRUE}

\item{site_aggr_args}{named list of parameters passed to
\code{\link{site_aggr}}, Default: list()}

\item{eval_sites_args}{named list of parameters passed to
\code{\link{eval_sites}}, Default: list()}

\item{check}{logical, perform data check and attempt repair with}
}
\value{
dataframe with the following columns:
\describe{
\item{\strong{study_id}}{study identification}
\item{\strong{site_number}}{site identification}
\item{\strong{n_pat}}{number of patients at site}
\item{\strong{n_pat_with_med75}}{number of patients at site with visit_med75}
\item{\strong{visit_med75}}{median(max(visit)) * 0.75}
\item{\strong{mean_ae_site_med75}}{mean AE at visit_med75 site level}
\item{\strong{mean_ae_study_med75}}{mean AE at visit_med75 study level}
\item{\strong{n_pat_with_med75_study}}{number of patients at site with
visit_med75 at study excl site}
\item{\strong{extra_ur_sites}}{additional sites
with under-reporting patients}
\item{\strong{frac_pat_with_ur}}{ratio of
patients in study that are under-reporting}
\item{\strong{ur_rate}}{under-reporting rate}
\item{\strong{pval}}{p-value as
returned by \code{\link[stats]{poisson.test}}}
\item{\strong{prob_low}}{bootstrapped probability for having mean_ae_site_med75
or lower} \item{\strong{pval_adj}}{adjusted p-values}
\item{\strong{prob_low_adj}}{adjusted bootstrapped probability for having
mean_ae_site_med75 or lower} \item{\strong{pval_prob_ur}}{probability
under-reporting as 1 - pval_adj, poisson.test (use as benchmark)}
\item{\strong{prob_low_prob_ur}}{probability under-reporting as 1 -
prob_low_adj, bootstrapped (use)}
}
}
\description{
Use with simulated portfolio data to generate under-reporting
stats for specified scenarios.
}
\details{
The function will apply under-reporting scenarios to each site.
Reducing the number of AEs by a given under-reporting (ur_rate) for all
patients at the site and add the corresponding under-reporting statistics.
Since the under-reporting probability is also affected by the number of
other sites that are under-reporting we additionally calculate
under-reporting statistics in a scenario where additional under reporting
sites are present. For this we use the median number of patients per site
at the study to calculate the final number of patients for which we lower
the AEs in a given under-reporting scenario. We use the furrr package to
implement parallel processing as these simulations can take a long time to
run. For this to work we need to specify the plan for how the code should
run, e.g. plan(multisession, workers = 18)
}
\examples{
\donttest{
df_visit1 <- sim_test_data_study(n_pat = 100, n_sites = 10,
                                 frac_site_with_ur = 0.4, ur_rate = 0.6)

df_visit1$study_id <- "A"

df_visit2 <- sim_test_data_study(n_pat = 100, n_sites = 10,
                                 frac_site_with_ur = 0.2, ur_rate = 0.1)

df_visit2$study_id <- "B"

df_visit <- dplyr::bind_rows(df_visit1, df_visit2)

df_site_max <- df_visit \%>\%
  dplyr::group_by(study_id, site_number, patnum) \%>\%
  dplyr::summarise(max_visit = max(visit),
            max_ae = max(n_ae),
            .groups = "drop")

df_config <- get_config(df_site_max)

df_config

df_portf <- sim_test_data_portfolio(df_config)

df_portf

df_scen <- sim_ur_scenarios(df_portf,
                            extra_ur_sites = 2,
                            ur_rate = c(0.5, 1))


df_scen

df_perf <- get_portf_perf(df_scen)

df_perf
}
}
\seealso{
\code{\link{sim_test_data_study}}
\code{\link{get_config}}
\code{\link{sim_test_data_portfolio}}
\code{\link{sim_ur_scenarios}}
\code{\link{get_portf_perf}}
}
