# download_hathi_ef downloads EF and returns a tibble

    Code
      ef
    Output
      # A tibble: 95,258 x 5
         token       POS   count section  page
         <chr>       <chr> <dbl> <chr>   <dbl>
       1 I           UNK       1 body        4
       2 IN          UNK       3 body        7
       3 II          UNK       1 body        7
       4 HADE        UNK       1 body        7
       5 WITH        UNK       1 body        7
       6 fOCQUEVILLE UNK       1 body        7
       7 PARIS       UNK       1 body        7
       8 REEVE       UNK       1 body        7
       9 .           UNK       9 body        7
      10 UNIVERSITY  UNK       1 body        7
      # ... with 95,248 more rows

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
         <chr>                    <chr>  <chr> <int> <chr>   <int>
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
         <chr>                    <chr>  <chr> <int> <chr>   <int>
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
         <chr>                    <chr>  <chr> <int> <chr>   <int>
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
      # A tibble: 30 x 3
         field         value                                         htid             
         <chr>         <chr>                                         <chr>            
       1 schemaVersion https://schemas.hathitrust.org/EF_Schema_Met~ aeu.ark:/13960/t~
       2 id            http://hdl.handle.net/2027/aeu.ark:/13960/t3~ aeu.ark:/13960/t~
       3 type          DataFeedItem                                  aeu.ark:/13960/t~
       4 type          Book                                          aeu.ark:/13960/t~
       5 dateCreated   20200209                                      aeu.ark:/13960/t~
       6 title         Democracy in America                          aeu.ark:/13960/t~
       7 contributor   http://www.viaf.org/viaf/66474207             aeu.ark:/13960/t~
       8 contributor   http://id.loc.gov/ontologies/bibframe/Person  aeu.ark:/13960/t~
       9 contributor   Tocqueville, Alexis de, 1805-1859             aeu.ark:/13960/t~
      10 pubDate       1838                                          aeu.ark:/13960/t~
      # ... with 20 more rows

