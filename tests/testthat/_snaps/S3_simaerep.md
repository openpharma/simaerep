# print.simaerep generic must print object description

    Code
      print(aerep)
    Output
      simaerep object:
      ----------------
      Plot results using plot() generic.
      Full results available in "df_eval".
      
      Summary:
      Number of sites: 10
      Number of studies: 2
      
      Multiplicity correction applied to "*_prob" columns.
      
      First 10 rows of df_eval:
      # A tibble: 10 x 9
         study_id site_id event_count event_per_visit_site visits n_pat
         <chr>    <chr>         <dbl>                <dbl>  <dbl> <int>
       1 A        S0001           109                0.280    389    20
       2 A        S0002           114                0.302    378    20
       3 A        S0003           197                0.495    398    20
       4 A        S0004           184                0.474    388    20
       5 A        S0005           188                0.495    380    20
       6 B        S0001           177                0.449    394    20
       7 B        S0002           204                0.523    390    20
       8 B        S0003           198                0.524    378    20
       9 B        S0004           204                0.490    416    20
      10 B        S0005           166                0.438    379    20
      # i 3 more variables: event_per_visit_study <dbl>, event_prob <dbl>,
      #   event_delta <dbl>

