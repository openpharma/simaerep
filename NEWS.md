# simaerep 0.7.0
- event_names argument, allow to calculate multiple reporting probabilities at once https://github.com/openpharma/simaerep/issues/73
- fix https://github.com/openpharma/simaerep/issues/82
- increase unit test coverage

# simaerep 0.6.1
- set dplyr requirement to v1.1.0
- fix validation report action

# simaerep 0.6.0
- inframe method use table operations only, do not use visit_med75
- https://github.com/openpharma/simaerep/issues/59
- https://github.com/openpharma/simaerep/issues/16
- update vignettes
- https://github.com/openpharma/simaerep/issues/62

# simaerep 0.5.0
- allow flexible AE rates in data simulations
- add vignette comparing simaerep to gsm performance
- add over-reporting probability
- fix dplyr warnings
- fix warnings around ggplot and cowplot
- https://github.com/openpharma/simaerep/issues/45

# simaerep 0.4.3
- delete performance unit tests (poisson faster than bootstrap) to accommodate CRAN request

# simaerep 0.4.2
- CRAN submission

# simaerep 0.4.1
- validation report

# simaerep 0.4.0
- S3 interface #17
- better performance specification #26
- more meaningful unit test names #24 #30 #33

# simaerep 0.3.3
- better data checks and error messages #19, #20, # 21
- data check and repair by check_df_visit() became optionable

# simaerep 0.3.2
- added portfolio performance assessment

# simaerep 0.3.1
- changed MIT License holder from openpharma to F. Hoffmann-La Roche Ltd and simaerep authors

# simaerep 0.3.0
- improved evaluation point visit_med75
- allow days instead of visit

# simaerep 0.2.0
- use Benjamin Hochberg procedure for alpha error correction
- fix warnings around parallel processing
- improved SAS files vignette
