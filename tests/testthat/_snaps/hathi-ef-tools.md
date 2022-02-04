# get_hathi_counts returns a tibble

    Code
      ef
    Output
      # A tibble: 137,182 x 6
         htid                     token  POS   count section  page
         <chr>                    <chr>  <chr> <dbl> <chr>   <dbl>
       1 aeu.ark:/13960/t3qv43c3w "iJi"  NN        1 body        1
       2 aeu.ark:/13960/t3qv43c3w ".8"   CD        1 body        1
       3 aeu.ark:/13960/t3qv43c3w "11.6" CD        1 body        1
       4 aeu.ark:/13960/t3qv43c3w "."    .         2 body        1
       5 aeu.ark:/13960/t3qv43c3w "33"   CD        1 body        1
       6 aeu.ark:/13960/t3qv43c3w "U"    NNP       1 body        1
       7 aeu.ark:/13960/t3qv43c3w "TEST" NN        1 body        1
       8 aeu.ark:/13960/t3qv43c3w "\\"   SYM       2 body        1
       9 aeu.ark:/13960/t3qv43c3w "MT-3" NN        1 body        1
      10 aeu.ark:/13960/t3qv43c3w "J2"   NN        1 body        1
      # ... with 137,172 more rows

# get_hathi_counts returns a tibble with cache = 'rds'

    Code
      ef
    Output
      # A tibble: 137,182 x 6
         htid                     token  POS   count section  page
         <chr>                    <chr>  <chr> <dbl> <chr>   <dbl>
       1 aeu.ark:/13960/t3qv43c3w "iJi"  NN        1 body        1
       2 aeu.ark:/13960/t3qv43c3w ".8"   CD        1 body        1
       3 aeu.ark:/13960/t3qv43c3w "11.6" CD        1 body        1
       4 aeu.ark:/13960/t3qv43c3w "."    .         2 body        1
       5 aeu.ark:/13960/t3qv43c3w "33"   CD        1 body        1
       6 aeu.ark:/13960/t3qv43c3w "U"    NNP       1 body        1
       7 aeu.ark:/13960/t3qv43c3w "TEST" NN        1 body        1
       8 aeu.ark:/13960/t3qv43c3w "\\"   SYM       2 body        1
       9 aeu.ark:/13960/t3qv43c3w "MT-3" NN        1 body        1
      10 aeu.ark:/13960/t3qv43c3w "J2"   NN        1 body        1
      # ... with 137,172 more rows

# get_hathi_counts returns a tibble with cache = 'text2vec.csv'

    Code
      ef
    Output
      # A tibble: 508 x 4
         htid                     section  page token                                 
         <chr>                    <chr>   <dbl> <chr>                                 
       1 aeu.ark:/13960/t3qv43c3w body        1 "iJi_NN .8_CD 11.6_CD ._. ._. 33_CD U~
       2 aeu.ark:/13960/t3qv43c3w body        2 "IVIicroreproductions_NNS for_IN Seri~
       3 aeu.ark:/13960/t3qv43c3w body        3 "est_V bibliographique_ADJ supplement~
       4 aeu.ark:/13960/t3qv43c3w body        4 "est_JJS est_NN est_NN saui_NN darnlA~
       5 aeu.ark:/13960/t3qv43c3w body        5 "A_UNK WIT_UNK GEO_UNK 4i_UNK r_UNK D~
       6 aeu.ark:/13960/t3qv43c3w body        6 "KOYALE_UNK IN_UNK WITH_UNK *_UNK PAR~
       7 aeu.ark:/13960/t3qv43c3w body        7 "for_IN yesr_JJ in_IN VJ_NNP *_SYM En~
       8 aeu.ark:/13960/t3qv43c3w body        8 "reference_NN impressions_NNS At_IN o~
       9 aeu.ark:/13960/t3qv43c3w body        9 "ideas_NNS used_VBN procuring_VBG wri~
      10 aeu.ark:/13960/t3qv43c3w body       10 "At_IN opinions_NNS opinions_NNS opin~
      # ... with 498 more rows

# get_hathi_counts returns a tibble with cache = 'none'

    Code
      ef
    Output
      # A tibble: 137,182 x 6
         htid                     token  POS   count section  page
         <chr>                    <chr>  <chr> <dbl> <chr>   <dbl>
       1 aeu.ark:/13960/t3qv43c3w "iJi"  NN        1 body        1
       2 aeu.ark:/13960/t3qv43c3w ".8"   CD        1 body        1
       3 aeu.ark:/13960/t3qv43c3w "11.6" CD        1 body        1
       4 aeu.ark:/13960/t3qv43c3w "."    .         2 body        1
       5 aeu.ark:/13960/t3qv43c3w "33"   CD        1 body        1
       6 aeu.ark:/13960/t3qv43c3w "U"    NNP       1 body        1
       7 aeu.ark:/13960/t3qv43c3w "TEST" NN        1 body        1
       8 aeu.ark:/13960/t3qv43c3w "\\"   SYM       2 body        1
       9 aeu.ark:/13960/t3qv43c3w "MT-3" NN        1 body        1
      10 aeu.ark:/13960/t3qv43c3w "J2"   NN        1 body        1
      # ... with 137,172 more rows

# get_hathi_counts returns a tibble with cache = 'feather'

    Code
      ef
    Output
      # A tibble: 137,182 x 6
         htid                     token  POS   count section  page
         <chr>                    <chr>  <chr> <dbl> <chr>   <dbl>
       1 aeu.ark:/13960/t3qv43c3w "iJi"  NN        1 body        1
       2 aeu.ark:/13960/t3qv43c3w ".8"   CD        1 body        1
       3 aeu.ark:/13960/t3qv43c3w "11.6" CD        1 body        1
       4 aeu.ark:/13960/t3qv43c3w "."    .         2 body        1
       5 aeu.ark:/13960/t3qv43c3w "33"   CD        1 body        1
       6 aeu.ark:/13960/t3qv43c3w "U"    NNP       1 body        1
       7 aeu.ark:/13960/t3qv43c3w "TEST" NN        1 body        1
       8 aeu.ark:/13960/t3qv43c3w "\\"   SYM       2 body        1
       9 aeu.ark:/13960/t3qv43c3w "MT-3" NN        1 body        1
      10 aeu.ark:/13960/t3qv43c3w "J2"   NN        1 body        1
      # ... with 137,172 more rows

# get_hathi_meta returns correct metadata

    Code
      ef
    Output
      # A tibble: 1 x 20
        htid    schemaVersion     id       type  dateCreated title contributor pubDate
        <chr>   <chr>             <chr>    <lis>       <int> <chr> <list>        <int>
      1 aeu.ar~ https://schemas.~ http://~ <lis~    20200209 Demo~ <named lis~    1838
      # ... with 12 more variables: publisher <list>, pubPlace <list>,
      #   language <chr>, accessRights <chr>, accessProfile <chr>,
      #   sourceInstitution <list>, mainEntityOfPage <list>, oclc <chr>, isbn <chr>,
      #   genre <list>, typeOfResource <chr>, lastRightsUpdateDate <int>

