# get_hathi_counts returns a tibble

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
      # i Use `print(n = ...)` to see more rows

---

    Code
      ef2
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
      # i Use `print(n = ...)` to see more rows

---

    Code
      ef4
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
      # i Use `print(n = ...)` to see more rows

---

    Code
      ef5
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
      # i Use `print(n = ...)` to see more rows

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
      # i Use `print(n = ...)` to see more rows

# get_hathi_meta returns correct metadata

    Code
      meta
    Output
      # A tibble: 1 x 20
        htid         schem~1 id    type  dateC~2 title contr~3 pubDate publi~4 pubPl~5
        <chr>        <chr>   <chr> <chr>   <int> <chr> <chr>     <int> <chr>   <chr>  
      1 aeu.ark:/13~ https:~ http~ "[[\~  2.02e7 Demo~ "{\"id~    1838 "{\"id~ "{\"id~
      # ... with 10 more variables: language <chr>, accessRights <chr>,
      #   accessProfile <chr>, sourceInstitution <chr>, mainEntityOfPage <chr>,
      #   oclc <chr>, isbn <chr>, genre <chr>, typeOfResource <chr>,
      #   lastRightsUpdateDate <int>, and abbreviated variable names
      #   1: schemaVersion, 2: dateCreated, 3: contributor, 4: publisher, 5: pubPlace
      # i Use `colnames()` to see all variable names

# get_hathi_page_meta returns correct metadata but not from cache

    Code
      pagemeta
    Output
      # A tibble: 508 x 17
         htid       page seq   version token~1 lineC~2 empty~3 sente~4 calcu~5 section
         <chr>     <int> <chr> <chr>     <int>   <int>   <int>   <int> <chr>   <chr>  
       1 aeu.ark:~     1 0000~ 66cbd9~      67      70      36       3 en      body   
       2 aeu.ark:~     2 0000~ 95fff2~      21      14       6       3 en      body   
       3 aeu.ark:~     3 0000~ c713af~     516     138      49      10 fr      body   
       4 aeu.ark:~     4 0000~ cbcaaa~     457      87      21      19 en      body   
       5 aeu.ark:~     5 0000~ 39af72~       6      12       6      NA cy      body   
       6 aeu.ark:~     6 0000~ bb88c3~      80      47      23      NA <NA>    body   
       7 aeu.ark:~     7 0000~ f6e267~      72      15       6       6 en      body   
       8 aeu.ark:~     8 0000~ 28ca6a~     348      44       9      11 en      body   
       9 aeu.ark:~     9 0000~ e6e202~     443      50       9      11 en      body   
      10 aeu.ark:~    10 0000~ 9164b6~     500      44       5      12 en      body   
      # ... with 498 more rows, 7 more variables: sectionTokenCount <int>,
      #   sectionLineCount <int>, sectionEmptyLineCount <int>,
      #   sectionSentenceCount <int>, sectionCapAlphaSeq <int>,
      #   sectionBeginCharCount <chr>, sectionEndCharCount <chr>, and abbreviated
      #   variable names 1: tokenCount, 2: lineCount, 3: emptyLineCount,
      #   4: sentenceCount, 5: calculatedLanguage
      # i Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

---

    Code
      cached_pagemeta
    Output
      # A tibble: 508 x 17
         htid       page seq   version token~1 lineC~2 empty~3 sente~4 calcu~5 section
         <chr>     <int> <chr> <chr>     <int>   <int>   <int>   <int> <chr>   <chr>  
       1 aeu.ark:~     1 0000~ 66cbd9~      67      70      36       3 en      body   
       2 aeu.ark:~     2 0000~ 95fff2~      21      14       6       3 en      body   
       3 aeu.ark:~     3 0000~ c713af~     516     138      49      10 fr      body   
       4 aeu.ark:~     4 0000~ cbcaaa~     457      87      21      19 en      body   
       5 aeu.ark:~     5 0000~ 39af72~       6      12       6      NA cy      body   
       6 aeu.ark:~     6 0000~ bb88c3~      80      47      23      NA <NA>    body   
       7 aeu.ark:~     7 0000~ f6e267~      72      15       6       6 en      body   
       8 aeu.ark:~     8 0000~ 28ca6a~     348      44       9      11 en      body   
       9 aeu.ark:~     9 0000~ e6e202~     443      50       9      11 en      body   
      10 aeu.ark:~    10 0000~ 9164b6~     500      44       5      12 en      body   
      # ... with 498 more rows, 7 more variables: sectionTokenCount <int>,
      #   sectionLineCount <int>, sectionEmptyLineCount <int>,
      #   sectionSentenceCount <int>, sectionCapAlphaSeq <int>,
      #   sectionBeginCharCount <chr>, sectionEndCharCount <chr>, and abbreviated
      #   variable names 1: tokenCount, 2: lineCount, 3: emptyLineCount,
      #   4: sentenceCount, 5: calculatedLanguage
      # i Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

