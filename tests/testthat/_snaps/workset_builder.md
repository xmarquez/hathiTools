# Worksets with single and multiple tokens can be produced

    Code
      workset1
    Output
      # A tibble: 5,086 x 2
         htid                   n
         <chr>              <int>
       1 mdp.39015078781187    44
       2 mdp.39015081502976    38
       3 uva.x004834895        38
       4 mdp.39015054182608    37
       5 inu.30000092907009    36
       6 mdp.39015060892620    35
       7 mdp.39015059138001    29
       8 mdp.39015061341395    28
       9 mdp.39015079668037    27
      10 mdp.39015063378007    26
      # ... with 5,076 more rows
      # i Use `print(n = ...)` to see more rows

---

    Code
      workset2
    Output
      # A tibble: 20 x 2
         htid                    n
         <chr>               <int>
       1 mdp.39015073894571    272
       2 mdp.39015038527639    178
       3 txu.059173023679147   166
       4 mdp.39015013308195    140
       5 mdp.39015062110021    139
       6 uc1.b3799939          135
       7 mdp.39015002340498    134
       8 mdp.39015028935495    134
       9 mdp.39015081502976    133
      10 mdp.49015002882919    130
      11 uc1.32106011675730    130
      12 uva.x000133700        129
      13 uva.x004394875        129
      14 mdp.39015048532892    127
      15 mdp.39015002447046    126
      16 mdp.39015066828552    124
      17 mdp.39015067682677    124
      18 mdp.39015056882486    122
      19 mdp.39015066730246    122
      20 mdp.39015079668037    121

---

    Code
      workset3
    Output
      # A tibble: 6,341 x 2
         htid                     id                                  
         <chr>                    <chr>                               
       1 aeu.ark:/13960/t05x3k82c aeu.ark:/13960/t05x3k82c.page-000075
       2 aeu.ark:/13960/t6pz5zs5h aeu.ark:/13960/t6pz5zs5h.page-000251
       3 aeu.ark:/13960/t8qc19m2f aeu.ark:/13960/t8qc19m2f.page-000222
       4 chi.096292271            chi.096292271.page-000364           
       5 chi.096292336            chi.096292336.page-000368           
       6 chi.101607416            chi.101607416.page-001182           
       7 chi.63733675             chi.63733675.page-000012            
       8 chi.65548487             chi.65548487.page-000438            
       9 chi.78011095             chi.78011095.page-000870            
      10 chi.78020645             chi.78020645.page-000400            
      # ... with 6,331 more rows
      # i Use `print(n = ...)` to see more rows

---

    Code
      workset4
    Output
      # A tibble: 1,503,617 x 2
         htid                     id                                  
         <chr>                    <chr>                               
       1 aeu.ark:/13960/t0000m66t aeu.ark:/13960/t0000m66t.page-000270
       2 aeu.ark:/13960/t00z7p50m aeu.ark:/13960/t00z7p50m.page-000060
       3 aeu.ark:/13960/t01z50k65 aeu.ark:/13960/t01z50k65.page-000483
       4 aeu.ark:/13960/t02z20s80 aeu.ark:/13960/t02z20s80.page-000319
       5 aeu.ark:/13960/t02z21698 aeu.ark:/13960/t02z21698.page-000064
       6 aeu.ark:/13960/t02z2238v aeu.ark:/13960/t02z2238v.page-000398
       7 aeu.ark:/13960/t02z2238v aeu.ark:/13960/t02z2238v.page-000395
       8 aeu.ark:/13960/t03x8s14z aeu.ark:/13960/t03x8s14z.page-000020
       9 aeu.ark:/13960/t03x9kv34 aeu.ark:/13960/t03x9kv34.page-000331
      10 aeu.ark:/13960/t03x9tk6w aeu.ark:/13960/t03x9tk6w.page-000406
      # ... with 1,503,607 more rows
      # i Use `print(n = ...)` to see more rows

# Workset can be produced with different combinations of language, name, genre, and publication date

    Code
      workset5
    Output
      # A tibble: 249 x 2
         htid                    n
         <chr>               <int>
       1 uc1.31158003886792    258
       2 mdp.39015019003337    184
       3 mdp.39015019003329    180
       4 mdp.39015008706338    163
       5 mdp.39015008852488    148
       6 inu.30000009143599    146
       7 ien.35556040838641    144
       8 pst.000027408636      144
       9 umn.31951001992356b   127
      10 pst.000027408643      126
      # ... with 239 more rows
      # i Use `print(n = ...)` to see more rows

---

    Code
      workset6
    Output
      # A tibble: 345 x 2
         htid                    n
         <chr>               <int>
       1 mdp.39015079304757   1358
       2 mdp.39015008706338   1213
       3 mdp.39015058109706    945
       4 uva.x000469924        909
       5 nyp.33433081795266    903
       6 nyp.33433081795381    901
       7 hvd.32044011894870    899
       8 uc1.b4765713          899
       9 umn.319510019923684   897
      10 uc1.31158003886792    893
      # ... with 335 more rows
      # i Use `print(n = ...)` to see more rows

---

    Code
      workset8
    Output
      # A tibble: 464 x 2
         htid                   n
         <chr>              <int>
       1 mdp.39015079304757  1358
       2 mdp.39015008706338  1213
       3 mdp.39015058109706   945
       4 nyp.33433081795357   910
       5 uva.x000469924       909
       6 hvd.32044051720316   906
       7 coo.31924030454809   904
       8 nyp.33433081795266   903
       9 ien.35556041207515   901
      10 nyp.33433081795381   901
      # ... with 454 more rows
      # i Use `print(n = ...)` to see more rows

# We can download metadata

    Code
      meta
    Output
      # A tibble: 4 x 16
        htid       acces~1 acces~2 url   title dateC~3 lastR~4 pubDate schem~5 typeO~6
        <chr>      <chr>   <chr>   <chr> <chr>   <dbl>   <dbl>   <dbl> <chr>   <chr>  
      1 aeu.ark:/~ open    pdus    http~ Demo~  2.02e7  2.01e7    1889 https:~ http:/~
      2 iau.31858~ google  pdus    http~ Demo~  2.02e7  2.02e7    1889 https:~ http:/~
      3 aeu.ark:/~ open    pdus    http~ Demo~  2.02e7  2.01e7    1889 https:~ http:/~
      4 iau.31858~ google  pdus    http~ Demo~  2.02e7  2.02e7    1889 https:~ http:/~
      # ... with 6 more variables: language <chr>, oclc <dbl>, genre <chr>,
      #   contributor <chr>, publisher <chr>, pubPlace <chr>, and abbreviated
      #   variable names 1: accessProfile, 2: accessRights, 3: dateCreated,
      #   4: lastRightsUpdateDate, 5: schemaVersion, 6: typeOfResource
      # i Use `colnames()` to see all variable names

