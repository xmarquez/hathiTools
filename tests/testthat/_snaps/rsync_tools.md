# Can rsync

    Code
      rsync_result <- rsync_from_hathi(poetry$htid[1:10], dir = dir)
    Message <simpleMessage>
      Creating directory to sync JSON EF files at ./hathi-ef
    Output
      0 

# Can cache to csv

    Code
      get_hathi_counts(poetry$htid[1], dir = dir)
    Output
      # A tibble: 1,155 x 6
         htid               token      POS   count section  page
         <chr>              <chr>      <chr> <dbl> <chr>   <dbl>
       1 mdp.39015047759454 i          UNK       1 body        3
       2 mdp.39015047759454 diceres    NNS       1 body        5
       3 mdp.39015047759454 Gun        NN        1 body        5
       4 mdp.39015047759454 Cimrte     NNP       1 body        5
       5 mdp.39015047759454 for        IN        1 body        5
       6 mdp.39015047759454 *          SYM       2 body        5
       7 mdp.39015047759454 1700       CD        1 body        5
       8 mdp.39015047759454 quodcunque JJ        1 body        5
       9 mdp.39015047759454 MRDRYDEN   NN        1 body        5
      10 mdp.39015047759454 .          .         4 body        5
      # ... with 1,145 more rows
      # i Use `print(n = ...)` to see more rows

# Can cache to text2vec.csv

    Code
      get_hathi_counts(poetry$htid[1], dir = dir, cache_type = "text2vec.csv")
    Output
      # A tibble: 17 x 4
         htid               section  page token                                       
         <chr>              <chr>   <dbl> <chr>                                       
       1 mdp.39015047759454 body        3 "i_UNK"                                     
       2 mdp.39015047759454 body        5 "diceres_NNS Gun_NN Cimrte_NNP for_IN *_SYM~
       3 mdp.39015047759454 body        7 "pi_NN ._. A_DT DRTDEN_NNP Mr._NNP POEM_NN ~
       4 mdp.39015047759454 body        9 "E_NN Fate_NNP Frame_NN With_IN is_VBZ is_V~
       5 mdp.39015047759454 body       10 "sweeter_JJR tune_VB Tuscan_NNP Aur_NNP Apo~
       6 mdp.39015047759454 body       11 "lasting_JJ fragrant_JJ Tho_NN ``_`` s_NNS ~
       7 mdp.39015047759454 body       12 "Courage_NN Cloud_NN for_IN drive_VB Who_WP~
       8 mdp.39015047759454 body       13 "trtyny_NN 4j_NN 6rW_FW e_SYM tiope_NN Has_~
       9 mdp.39015047759454 body       14 "Land_NN profusely_RB foundBut_VBP s_VBZ pr~
      10 mdp.39015047759454 body       15 "Fate_NNP Laws_NNS hining_VBG ct_VB Who_WP ~
      11 mdp.39015047759454 body       16 "i_UNK"                                     
      12 mdp.39015047759454 body       21 "04775_UNK UNIVERSITY_UNK MICHIGAN_UNK OF_U~
      13 mdp.39015047759454 header     10 "Memory_NN ._. os_VBZ Mr._NNP 6_CD To_TO Dr~
      14 mdp.39015047759454 header     11 "._. Mr._NNP -RRB-_-RRB- Manor_NNP To_TO 7_~
      15 mdp.39015047759454 header     12 "8_CD Memory_NN ._. Mr._NNP To_TO Dryden_NN~
      16 mdp.39015047759454 header     14 "Memory_NN ._. Mr._NNP To_TO io_NN Dryden_N~
      17 mdp.39015047759454 header     15 "Memory_NN ._. T_NN Mr._NNP #_# tbe_FW Dryd~

# Can cache to rds

    Code
      get_hathi_counts(poetry$htid[1], dir = dir, cache_type = "rds")
    Output
      # A tibble: 1,155 x 6
         htid               token      POS   count section  page
         <chr>              <chr>      <chr> <dbl> <chr>   <dbl>
       1 mdp.39015047759454 i          UNK       1 body        3
       2 mdp.39015047759454 diceres    NNS       1 body        5
       3 mdp.39015047759454 Gun        NN        1 body        5
       4 mdp.39015047759454 Cimrte     NNP       1 body        5
       5 mdp.39015047759454 for        IN        1 body        5
       6 mdp.39015047759454 *          SYM       2 body        5
       7 mdp.39015047759454 1700       CD        1 body        5
       8 mdp.39015047759454 quodcunque JJ        1 body        5
       9 mdp.39015047759454 MRDRYDEN   NN        1 body        5
      10 mdp.39015047759454 .          .         4 body        5
      # ... with 1,145 more rows
      # i Use `print(n = ...)` to see more rows

# Can cache to arrow

    Code
      get_hathi_counts(poetry$htid[1], dir = dir, cache_type = "feather")
    Output
      # A tibble: 1,155 x 6
         htid               token      POS   count section  page
         <chr>              <chr>      <chr> <dbl> <chr>   <dbl>
       1 mdp.39015047759454 i          UNK       1 body        3
       2 mdp.39015047759454 diceres    NNS       1 body        5
       3 mdp.39015047759454 Gun        NN        1 body        5
       4 mdp.39015047759454 Cimrte     NNP       1 body        5
       5 mdp.39015047759454 for        IN        1 body        5
       6 mdp.39015047759454 *          SYM       2 body        5
       7 mdp.39015047759454 1700       CD        1 body        5
       8 mdp.39015047759454 quodcunque JJ        1 body        5
       9 mdp.39015047759454 MRDRYDEN   NN        1 body        5
      10 mdp.39015047759454 .          .         4 body        5
      # ... with 1,145 more rows
      # i Use `print(n = ...)` to see more rows

# Can cache workset

    Code
      get_hathi_counts(poetry$htid[1], dir = dir, cache_type = "feather")
    Output
      # A tibble: 1,155 x 6
         htid               token      POS   count section  page
         <chr>              <chr>      <chr> <dbl> <chr>   <dbl>
       1 mdp.39015047759454 i          UNK       1 body        3
       2 mdp.39015047759454 diceres    NNS       1 body        5
       3 mdp.39015047759454 Gun        NN        1 body        5
       4 mdp.39015047759454 Cimrte     NNP       1 body        5
       5 mdp.39015047759454 for        IN        1 body        5
       6 mdp.39015047759454 *          SYM       2 body        5
       7 mdp.39015047759454 1700       CD        1 body        5
       8 mdp.39015047759454 quodcunque JJ        1 body        5
       9 mdp.39015047759454 MRDRYDEN   NN        1 body        5
      10 mdp.39015047759454 .          .         4 body        5
      # ... with 1,145 more rows
      # i Use `print(n = ...)` to see more rows

