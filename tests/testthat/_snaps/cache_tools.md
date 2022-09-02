# Can cache to all formats

    Code
      files %>% count(cache_type, cache_format)
    Output
      # A tibble: 16 x 3
         cache_type cache_format     n
         <chr>      <chr>        <int>
       1 ef         csv.gz          10
       2 ef         feather         10
       3 ef         parquet         10
       4 ef         rds             10
       5 ef         text2vec.csv    10
       6 meta       csv.gz          10
       7 meta       feather         10
       8 meta       parquet         10
       9 meta       rds             10
      10 meta       text2vec.csv    10
      11 none       json.bz2        10
      12 pagemeta   csv.gz          10
      13 pagemeta   feather         10
      14 pagemeta   parquet         10
      15 pagemeta   rds             10
      16 pagemeta   text2vec.csv    10

# Can read from cache, and cache returns correct values

    Code
      ef
    Output
      # A tibble: 528,066 x 6
         htid               token      POS   count section  page
         <chr>              <chr>      <chr> <int> <chr>   <int>
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
      # ... with 528,056 more rows
      # i Use `print(n = ...)` to see more rows

---

    Code
      meta
    Output
      # A tibble: 10 x 23
         htid        schem~1 id    type  dateC~2 title contr~3 pubDate publi~4 pubPl~5
         <chr>       <chr>   <chr> <chr>   <int> <chr> <chr>     <int> <chr>   <chr>  
       1 mdp.390150~ https:~ http~ =Dat~  2.02e7 To t~ "id=ht~    1700 id=htt~ id=htt~
       2 mdp.390150~ https:~ http~ =Dat~  2.02e7 A ne~  <NA>      1700 id=htt~ id=htt~
       3 uc1.321060~ https:~ http~ =Dat~  2.02e7 The ~ "=list~    1700 id=htt~ <NA>   
       4 njp.321010~ https:~ http~ =Dat~  2.02e7 The ~ "=list~    1702 id=htt~ id=htt~
       5 nnc1.00233~ https:~ http~ =Dat~  2.02e7 The ~ "=list~    1702 id=htt~ id=htt~
       6 nyp.334330~ https:~ http~ =Dat~  2.02e7 Misc~ "id=ht~    1709 id=htt~ id=htt~
       7 nyp.334330~ https:~ http~ =Dat~  2.02e7 A Co~ "id=ht~    1702 id=htt~ id=htt~
       8 mdp.390150~ https:~ http~ =Dat~  2.02e7 Poem~ "=list~    1703 id=htt~ id=htt~
       9 mdp.390150~ https:~ http~ =Dat~  2.02e7 The ~ "id=ht~    1703 id=htt~ id=htt~
      10 njp.321010~ https:~ http~ =Dat~  2.02e7 The ~ "id=ht~    1703 id=htt~ id=htt~
      # ... with 13 more variables: language <chr>, accessRights <chr>,
      #   accessProfile <chr>, sourceInstitution <chr>, mainEntityOfPage <chr>,
      #   oclc <dbl>, genre <chr>, typeOfResource <chr>, lastRightsUpdateDate <int>,
      #   lcc <chr>, lccn <dbl>, category <chr>, enumerationChronology <chr>, and
      #   abbreviated variable names 1: schemaVersion, 2: dateCreated,
      #   3: contributor, 4: publisher, 5: pubPlace
      # i Use `colnames()` to see all variable names

---

    Code
      pagemeta
    Output
      # A tibble: 6,299 x 17
         htid       page seq   version token~1 lineC~2 empty~3 sente~4 calcu~5 secti~6
         <chr>     <int> <chr> <chr>     <int>   <int>   <int>   <int> <chr>     <int>
       1 mdp.3901~     3 0000~ 372e25~       1       1       0      NA <NA>          1
       2 mdp.3901~     5 0000~ a91f44~      60      13       2       5 en           60
       3 mdp.3901~     7 0000~ 10c038~       9       4       0       2 en            9
       4 mdp.3901~     9 0000~ 1d66b6~     118      19       0       4 en          118
       5 mdp.3901~    10 0000~ cc1fc8~     266      29       0       8 en            8
       6 mdp.3901~    10 0000~ cc1fc8~     266      29       0       8 en          258
       7 mdp.3901~    11 0000~ 06f2e2~     260      29       0      15 en            9
       8 mdp.3901~    11 0000~ 06f2e2~     260      29       0      15 en          251
       9 mdp.3901~    12 0000~ e88feb~     248      27       0       5 en            8
      10 mdp.3901~    12 0000~ e88feb~     248      27       0       5 en          240
      # ... with 6,289 more rows, 7 more variables: sectionLineCount <int>,
      #   sectionEmptyLineCount <int>, sectionSentenceCount <int>,
      #   sectionCapAlphaSeq <int>, sectionBeginCharCount <chr>,
      #   sectionEndCharCount <chr>, section <chr>, and abbreviated variable names
      #   1: tokenCount, 2: lineCount, 3: emptyLineCount, 4: sentenceCount,
      #   5: calculatedLanguage, 6: sectionTokenCount
      # i Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

---

    Code
      full_from_csv
    Output
      # A tibble: 528,066 x 42
         htid        token POS   count section  page schem~1 id    type  dateC~2 title
         <chr>       <chr> <chr> <int> <chr>   <int> <chr>   <chr> <chr>   <int> <chr>
       1 mdp.390150~ i     UNK       1 body        3 https:~ http~ =Dat~  2.02e7 To t~
       2 mdp.390150~ dice~ NNS       1 body        5 https:~ http~ =Dat~  2.02e7 To t~
       3 mdp.390150~ Gun   NN        1 body        5 https:~ http~ =Dat~  2.02e7 To t~
       4 mdp.390150~ Cimr~ NNP       1 body        5 https:~ http~ =Dat~  2.02e7 To t~
       5 mdp.390150~ for   IN        1 body        5 https:~ http~ =Dat~  2.02e7 To t~
       6 mdp.390150~ *     SYM       2 body        5 https:~ http~ =Dat~  2.02e7 To t~
       7 mdp.390150~ 1700  CD        1 body        5 https:~ http~ =Dat~  2.02e7 To t~
       8 mdp.390150~ quod~ JJ        1 body        5 https:~ http~ =Dat~  2.02e7 To t~
       9 mdp.390150~ MRDR~ NN        1 body        5 https:~ http~ =Dat~  2.02e7 To t~
      10 mdp.390150~ .     .         4 body        5 https:~ http~ =Dat~  2.02e7 To t~
      # ... with 528,056 more rows, 31 more variables: contributor <chr>,
      #   pubDate <int>, publisher <chr>, pubPlace <chr>, language <chr>,
      #   accessRights <chr>, accessProfile <chr>, sourceInstitution <chr>,
      #   mainEntityOfPage <chr>, oclc <dbl>, genre <chr>, typeOfResource <chr>,
      #   lastRightsUpdateDate <int>, lcc <chr>, lccn <dbl>, category <chr>,
      #   enumerationChronology <chr>, seq <chr>, version <chr>, tokenCount <int>,
      #   lineCount <int>, emptyLineCount <int>, sentenceCount <int>, ...
      # i Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

---

    Code
      full_from_rds
    Output
      # A tibble: 528,066 x 42
         htid        token POS   count section  page schem~1 id    type  dateC~2 title
         <chr>       <chr> <chr> <int> <chr>   <int> <chr>   <chr> <chr>   <int> <chr>
       1 mdp.390150~ i     UNK       1 body        3 https:~ http~ =Dat~  2.02e7 To t~
       2 mdp.390150~ dice~ NNS       1 body        5 https:~ http~ =Dat~  2.02e7 To t~
       3 mdp.390150~ Gun   NN        1 body        5 https:~ http~ =Dat~  2.02e7 To t~
       4 mdp.390150~ Cimr~ NNP       1 body        5 https:~ http~ =Dat~  2.02e7 To t~
       5 mdp.390150~ for   IN        1 body        5 https:~ http~ =Dat~  2.02e7 To t~
       6 mdp.390150~ *     SYM       2 body        5 https:~ http~ =Dat~  2.02e7 To t~
       7 mdp.390150~ 1700  CD        1 body        5 https:~ http~ =Dat~  2.02e7 To t~
       8 mdp.390150~ quod~ JJ        1 body        5 https:~ http~ =Dat~  2.02e7 To t~
       9 mdp.390150~ MRDR~ NN        1 body        5 https:~ http~ =Dat~  2.02e7 To t~
      10 mdp.390150~ .     .         4 body        5 https:~ http~ =Dat~  2.02e7 To t~
      # ... with 528,056 more rows, 31 more variables: contributor <chr>,
      #   pubDate <int>, publisher <chr>, pubPlace <chr>, language <chr>,
      #   accessRights <chr>, accessProfile <chr>, sourceInstitution <chr>,
      #   mainEntityOfPage <chr>, oclc <dbl>, genre <chr>, typeOfResource <chr>,
      #   lastRightsUpdateDate <int>, lcc <chr>, lccn <dbl>, category <chr>,
      #   enumerationChronology <chr>, seq <chr>, version <chr>, tokenCount <int>,
      #   lineCount <int>, emptyLineCount <int>, sentenceCount <int>, ...
      # i Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

---

    Code
      full_from_parquet
    Output
      FileSystemDataset (query)
      htid: string
      token: string
      POS: string
      count: int32
      section: string
      page: int32
      schemaVersion: string
      id: string
      type: string
      dateCreated: int32
      title: string
      contributor: string
      pubDate: int32
      publisher: string
      pubPlace: string
      language: string
      accessRights: string
      accessProfile: string
      sourceInstitution: string
      mainEntityOfPage: string
      oclc: double
      genre: string
      typeOfResource: string
      lastRightsUpdateDate: int32
      seq: string
      version: string
      tokenCount: int32
      lineCount: int32
      emptyLineCount: int32
      sentenceCount: int32
      calculatedLanguage: string
      sectionTokenCount: int32
      sectionLineCount: int32
      sectionEmptyLineCount: int32
      sectionSentenceCount: int32
      sectionCapAlphaSeq: int32
      sectionBeginCharCount: string
      sectionEndCharCount: string
      
      See $.data for the source Arrow object

---

    Code
      ef_from_text2vec
    Output
      # A tibble: 6,299 x 4
         htid                page section tokens                                      
         <chr>              <int> <chr>   <chr>                                       
       1 mdp.39015047759454     3 body    i_UNK                                       
       2 mdp.39015047759454     5 body    diceres_NNS Gun_NN Cimrte_NNP for_IN *_SYM ~
       3 mdp.39015047759454     7 body    pi_NN ._. A_DT DRTDEN_NNP Mr._NNP POEM_NN T~
       4 mdp.39015047759454     9 body    E_NN Fate_NNP Frame_NN With_IN is_VBZ is_VB~
       5 mdp.39015047759454    10 body    sweeter_JJR tune_VB Tuscan_NNP Aur_NNP Apot~
       6 mdp.39015047759454    10 header  Memory_NN ._. os_VBZ Mr._NNP 6_CD To_TO Dry~
       7 mdp.39015047759454    11 body    lasting_JJ fragrant_JJ Tho_NN ``_`` s_NNS s~
       8 mdp.39015047759454    11 header  ._. Mr._NNP -RRB-_-RRB- Manor_NNP To_TO 7_C~
       9 mdp.39015047759454    12 body    Courage_NN Cloud_NN for_IN drive_VB Who_WP ~
      10 mdp.39015047759454    12 header  8_CD Memory_NN ._. Mr._NNP To_TO Dryden_NNP~
      # ... with 6,289 more rows
      # i Use `print(n = ...)` to see more rows

---

    Code
      ef_from_text2vec_w_meta
    Output
      # A tibble: 6,299 x 26
         htid    page section tokens schem~1 id    type  dateC~2 title contr~3 pubDate
         <chr>  <int> <chr>   <chr>  <chr>   <chr> <chr>   <int> <chr> <chr>     <int>
       1 mdp.3~     3 body    i_UNK  https:~ http~ =Dat~  2.02e7 To t~ id=htt~    1700
       2 mdp.3~     5 body    dicer~ https:~ http~ =Dat~  2.02e7 To t~ id=htt~    1700
       3 mdp.3~     7 body    pi_NN~ https:~ http~ =Dat~  2.02e7 To t~ id=htt~    1700
       4 mdp.3~     9 body    E_NN ~ https:~ http~ =Dat~  2.02e7 To t~ id=htt~    1700
       5 mdp.3~    10 body    sweet~ https:~ http~ =Dat~  2.02e7 To t~ id=htt~    1700
       6 mdp.3~    10 header  Memor~ https:~ http~ =Dat~  2.02e7 To t~ id=htt~    1700
       7 mdp.3~    11 body    lasti~ https:~ http~ =Dat~  2.02e7 To t~ id=htt~    1700
       8 mdp.3~    11 header  ._. M~ https:~ http~ =Dat~  2.02e7 To t~ id=htt~    1700
       9 mdp.3~    12 body    Coura~ https:~ http~ =Dat~  2.02e7 To t~ id=htt~    1700
      10 mdp.3~    12 header  8_CD ~ https:~ http~ =Dat~  2.02e7 To t~ id=htt~    1700
      # ... with 6,289 more rows, 15 more variables: publisher <chr>, pubPlace <chr>,
      #   language <chr>, accessRights <chr>, accessProfile <chr>,
      #   sourceInstitution <chr>, mainEntityOfPage <chr>, oclc <dbl>, genre <chr>,
      #   typeOfResource <chr>, lastRightsUpdateDate <int>, lcc <chr>, lccn <dbl>,
      #   category <chr>, enumerationChronology <chr>, and abbreviated variable names
      #   1: schemaVersion, 2: dateCreated, 3: contributor
      # i Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

---

    Code
      ef_and_pagemeta_only
    Output
      # A tibble: 528,066 x 20
         htid    token POS   count section  page seq   version token~1 lineC~2 empty~3
         <chr>   <chr> <chr> <int> <chr>   <int> <chr> <chr>     <int>   <int>   <int>
       1 mdp.39~ i     UNK       1 body        3 0000~ 372e25~       1       1       0
       2 mdp.39~ dice~ NNS       1 body        5 0000~ a91f44~      60      13       2
       3 mdp.39~ Gun   NN        1 body        5 0000~ a91f44~      60      13       2
       4 mdp.39~ Cimr~ NNP       1 body        5 0000~ a91f44~      60      13       2
       5 mdp.39~ for   IN        1 body        5 0000~ a91f44~      60      13       2
       6 mdp.39~ *     SYM       2 body        5 0000~ a91f44~      60      13       2
       7 mdp.39~ 1700  CD        1 body        5 0000~ a91f44~      60      13       2
       8 mdp.39~ quod~ JJ        1 body        5 0000~ a91f44~      60      13       2
       9 mdp.39~ MRDR~ NN        1 body        5 0000~ a91f44~      60      13       2
      10 mdp.39~ .     .         4 body        5 0000~ a91f44~      60      13       2
      # ... with 528,056 more rows, 9 more variables: sentenceCount <int>,
      #   calculatedLanguage <chr>, sectionTokenCount <int>, sectionLineCount <int>,
      #   sectionEmptyLineCount <int>, sectionSentenceCount <int>,
      #   sectionCapAlphaSeq <int>, sectionBeginCharCount <chr>,
      #   sectionEndCharCount <chr>, and abbreviated variable names 1: tokenCount,
      #   2: lineCount, 3: emptyLineCount
      # i Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

# Can cache workset

    Code
      files
    Output
      # A tibble: 6 x 5
        htid               local_loc                            cache~1 cache~2 exists
        <chr>              <glue>                               <chr>   <chr>   <lgl> 
      1 mdp.39015047759454 C:\Users\marquexa\AppData\Local\Tem~ feather ef      TRUE  
      2 mdp.39015086635185 C:\Users\marquexa\AppData\Local\Tem~ feather ef      TRUE  
      3 mdp.39015047759454 C:\Users\marquexa\AppData\Local\Tem~ feather meta    TRUE  
      4 mdp.39015086635185 C:\Users\marquexa\AppData\Local\Tem~ feather meta    TRUE  
      5 mdp.39015047759454 C:\Users\marquexa\AppData\Local\Tem~ feather pageme~ TRUE  
      6 mdp.39015086635185 C:\Users\marquexa\AppData\Local\Tem~ feather pageme~ TRUE  
      # ... with abbreviated variable names 1: cache_format, 2: cache_type

