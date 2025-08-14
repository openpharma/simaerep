# print.simaerep generic must print object description

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

---

    Code
      evrep
    Output
      simaerep object:
      ----------------
      Plot results using plot() generic.
      Full results available in "df_eval".
      
      Summary:
      Number of sites: 10
      Number of studies: 2
      
      Classic algorithm used to calculate probabilities!!
      
      Multiplicity correction applied to prob column.
      
      First 10 rows of df_eval:
      # A tibble: 10 x 9
         study_id site_id n_pat n_pat_with_med75 visit_med75 mean_event_site_med75
         <chr>    <chr>   <int>            <dbl>       <dbl>                 <dbl>
       1 A        S0001      20               17          15                  4.18
       2 A        S0002      20               17          16                  4.71
       3 A        S0003      20               15          18                  9.27
       4 A        S0004      20               17          16                  7.29
       5 A        S0005      20               18          16                  8.06
       6 B        S0001      20               17          16                  7   
       7 B        S0002      20               17          15                  8   
       8 B        S0003      20               17          15                  7.71
       9 B        S0004      20               17          16                  7.29
      10 B        S0005      20               16          14                  6.31
      # i 3 more variables: mean_event_study_med75 <dbl>,
      #   n_pat_with_med75_study <int>, prob <dbl>

# simaerep() - poisson test

    Code
      evrep
    Output
      simaerep object:
      ----------------
      Plot results using plot() generic.
      Full results available in "df_eval".
      
      Summary:
      Number of sites: 10
      Number of studies: 2
      
      Classic algorithm used to calculate probabilities!!
      
      Only under-reporting probability calculated !!!
      
      Multiplicity correction applied to prob and pval column.
      
      First 10 rows of df_eval:
      # A tibble: 10 x 10
         study_id site_id n_pat n_pat_with_med75 visit_med75 mean_event_site_med75
         <chr>    <chr>   <int>            <dbl>       <dbl>                 <dbl>
       1 A        S0001      20               17          15                  4.18
       2 A        S0002      20               17          16                  4.71
       3 A        S0003      20               15          18                  9.27
       4 A        S0004      20               17          16                  7.29
       5 A        S0005      20               18          16                  8.06
       6 B        S0001      20               17          16                  7   
       7 B        S0002      20               17          15                  8   
       8 B        S0003      20               17          15                  7.71
       9 B        S0004      20               17          16                  7.29
      10 B        S0005      20               16          14                  6.31
      # i 4 more variables: mean_event_study_med75 <dbl>,
      #   n_pat_with_med75_study <int>, pval <dbl>, prob <dbl>

