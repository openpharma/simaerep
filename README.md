
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simaerep <a href='https://openpharma.github.io/simaerep'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R build
status](https://github.com/openpharma/simaerep/workflows/R-CMD-check/badge.svg)](https://github.com/openpharma/simaerep/actions)
[![codecov](https://codecov.io/gh/openpharma/simaerep/branch/master/graph/badge.svg)](https://codecov.io/gh/openpharma/simaerep)
<!-- badges: end -->

Simulate adverse event reporting in clinical trials with the goal of
detecting under-reporting sites.

Monitoring of Adverse Event (AE) reporting in clinical trials is
important for patient safety. We use bootstrap-based simulation to
assign an AE under-reporting probability to each site in a clinical
trial. The method is inspired by the ‘infer’ R package and Allen
Downey’s blog article: [“There is only one
test\!”](http://allendowney.blogspot.com/2011/05/there-is-only-one-test.html).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("openpharma/simaerep")
```

## Publication

Koneswarakantha, B., Barmaz, Y., Ménard, T. et al. Follow-up on the Use
of Advanced Analytics for Clinical Quality Assurance: Bootstrap
Resampling to Enhance Detection of Adverse Event Under-Reporting. Drug
Saf (2020). <https://doi.org/10.1007/s40264-020-01011-5>

# Vignettes/ Articles

  - [Introduction](https://openpharma.github.io/simaerep/articles/intro.html)
  - [Usability
    Limits](https://openpharma.github.io/simaerep/articles/usability_limits.html)
  - [Check Poisson Test
    Applicability](https://openpharma.github.io/simaerep/articles/check_poisson.html)

# Application

``` r

suppressPackageStartupMessages(library("simaerep"))

set.seed(1)

df_visit <- sim_test_data_study(
  n_pat = 1000, # number of patients in study
  n_sites = 100, # number of sites in study
  frac_site_with_ur = 0.05, # fraction of sites under-reporting
  ur_rate = 0.4, # rate of under-reporting
  ae_per_visit_mean = 0.5 # mean AE per patient visit
)

df_visit$study_id <- "A"

df_site <- site_aggr(df_visit)

df_sim_sites <- sim_sites(df_site, df_visit, r = 1000)

df_eval <- eval_sites(df_sim_sites, r_sim_sites = 1000)

plot_study(df_visit, df_site, df_eval, study = "A") 
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

*Left panel shows mean AE reporting per site (lightblue and darkblue
lines) against mean AE reporting of the entire study (golden line).
Single sites are plotted in descending order by AE under-reporting
probability on the right panel in which grey lines denote cumulative AE
count of single patients. Grey dots in the left panel plot indicate
sites that were picked for single plotting. AE under-reporting
probability of dark blue lines crossed threshold of 95%. Numbers in the
upper left corner indicate the ratio of patients that have been used for
the analysis against the total number of patients. Patients that have
not been on the study long enough to reach the evaluation point
(visit\_med75, see introduction) will be ignored.*
