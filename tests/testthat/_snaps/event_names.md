# plot.simaerep, print.simaerep and print.orivisit work with event_names

    Code
      aerep
    Output
      simaerep object:
      ----------------
      Plot results using plot() generic.
      Full results available in "df_eval".
      
      Summary:
      Number of sites: 10
      Number of studies: 2
      
      Multiplicity correction applied to '*_prob' columns.
      
      Reporting probabilities calculated for: ae, y 
      
      First 10 rows of df_eval:
      # A tibble: 10 x 16
         study_id site_id ae_count y_count ae_per_visit_site y_per_visit_site visits
         <chr>    <chr>      <dbl>   <dbl>             <dbl>            <dbl>  <dbl>
       1 A        S0001         72      70             0.193            0.188    373
       2 A        S0002         72      55             0.205            0.156    352
       3 A        S0003        180     163             0.452            0.410    398
       4 A        S0004        202     138             0.539            0.368    375
       5 A        S0005        201     167             0.521            0.433    386
       6 B        S0001        191     141             0.494            0.364    387
       7 B        S0002        204     152             0.531            0.396    384
       8 B        S0003        199     139             0.509            0.355    391
       9 B        S0004        213     144             0.559            0.378    381
      10 B        S0005        204     170             0.516            0.430    395
      # i 9 more variables: n_pat <int>, ae_per_visit_study <dbl>,
      #   y_per_visit_study <dbl>, ae_prob_no_mult <dbl>, ae_prob <dbl>,
      #   ae_delta <dbl>, y_prob_no_mult <dbl>, y_prob <dbl>, y_delta <dbl>

---

    Code
      aerep$visit
    Output
      orivisit object:
      ----------------
      Stores lazy reference to original visit data.
      Full data available via as.data.frame(x).
      
      Summary:
      Number of studies: 2
      Number of sites: 5
      Number of patients: 100
      Number of visits: 3822
      Number of ae : 1738
      Number of y : 1339
      
      Data dimensions: 3822 rows x 11 columns
      Data source: df_visit_events_test
      Event types: ae, y 
      
      Column mappings:
        study_id: study_id
        site_id: site_id
        patient_id: patient_id
        visit: visit

