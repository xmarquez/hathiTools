
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hathiTools

<!-- badges: start -->

<!-- badges: end -->

This package allows you to interact with the HathiTrust
[Bookworm](https://bookworm.htrc.illinois.edu/develop/), a similar tool
to the [Google ngram viewer](https://books.google.com/ngrams), as well
as to download and process [HathiTrust Extracted
Features](https://analytics.hathitrust.org/datasets) files on which the
Bookworm viewer is based. (The HathiTrust collection contains over 13
million digitised books, including many of those originally digitised by
Google for its Google Books project).

## Installation

This package is not yet on CRAN. Install from GitHub as follows:

``` r
remotes::install("xmarquez/hathiTools")
```

## Downloading word frequencies

The simplest task to use the package for is to download word frequencies
from the HathiTrust
[Bookworm](https://bookworm.htrc.illinois.edu/develop/):

``` r
library(hathiTools)
library(tidyverse)
#> -- Attaching packages --------------------------------------------------------------------------------- tidyverse 1.3.0 --
#> v ggplot2 3.3.2     v purrr   0.3.4
#> v tibble  3.0.3     v dplyr   1.0.2
#> v tidyr   1.1.1     v stringr 1.4.0
#> v readr   1.3.1     v forcats 0.5.0
#> -- Conflicts ------------------------------------------------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()

result <- query_bookworm(word = c("democracy", "monarchy"), lims = c(1760, 2000), counttype = c("WordsPerMillion", "TextPercent"))
#> No encoding supplied: defaulting to UTF-8.

result
#> # A tibble: 482 x 4
#>    date_year democracy monarchy counttype      
#>        <int>     <dbl>    <dbl> <chr>          
#>  1      1760     0.382     5.50 WordsPerMillion
#>  2      1760     2.50     10.3  TextPercent    
#>  3      1761     0.300     6.32 WordsPerMillion
#>  4      1761     2.13     11.8  TextPercent    
#>  5      1762     0.352     4.94 WordsPerMillion
#>  6      1762     2.00      8.95 TextPercent    
#>  7      1763     0.488     9.32 WordsPerMillion
#>  8      1763     1.94     13.9  TextPercent    
#>  9      1764     0.663     4.37 WordsPerMillion
#> 10      1764     2.37      6.87 TextPercent    
#> # ... with 472 more rows

result %>%
  pivot_longer(democracy:monarchy, names_to = "word") %>%
  group_by(word, counttype) %>%
  mutate(rolling_avg = slider::slide_dbl(value, mean, .before = 10, .after = 10)) %>%
  ggplot(aes(x = date_year, color = word)) +
  geom_line(aes(y = value), alpha = 0.3) +
  geom_line(aes(x = date_year, y = rolling_avg)) +
  facet_wrap(~counttype) +
  labs(x = "Year", y = "", subtitle = "10 year rolling average, books published between 1760-2000",
       title = "Frequency of 'democracy' and 'monarchy' in the HathiTrust corpus") +
  theme_bw()
```

<img src="man/figures/README-example-1.png" width="100%" />

It is also possible to do more complex queries, for example to look at
the relative frequency of a term across book classifications from the
Library of Congress system:

``` r
result2 <- query_bookworm(word = "democracy", groups = c("date_year", "class"),
                          lims = c(1900,2000))
#> No encoding supplied: defaulting to UTF-8.

result2
#> # A tibble: 2,121 x 4
#>    date_year word     class                                      WordsPerMillion
#>        <int> <chr>    <chr>                                                <dbl>
#>  1      1900 democra~ N/A                                                  4.03 
#>  2      1900 democra~ Agriculture                                          0.769
#>  3      1900 democra~ Education                                            9.56 
#>  4      1900 democra~ World History And History Of Europe, Asia~           5.50 
#>  5      1900 democra~ History Of The Americas                             15.7  
#>  6      1900 democra~ Fine Arts                                            0.816
#>  7      1900 democra~ Science                                              0.146
#>  8      1900 democra~ General Works                                       14.6  
#>  9      1900 democra~ Military Science                                     0.962
#> 10      1900 democra~ Geography.  Anthropology.  Recreation                0.691
#> # ... with 2,111 more rows

result2 %>%
  ggplot(aes(x = date_year, y = fct_reorder(str_trunc(class, 40), WordsPerMillion))) +
  geom_tile(aes(fill = WordsPerMillion)) +
  facet_wrap(~word) +
  scale_fill_gradient2() +
  theme_bw() +
  labs(y = "", x = "Year", title = "Frequency of 'democracy' \nacross library of congress classifications",
       fill = "Words per million") +
  theme(legend.position = "bottom")
```

<img src="man/figures/README-example2-1.png" width="100%" />

Or across literary forms:

``` r
result3 <- query_bookworm(word = "democracy", groups = c("date_year", "literary_form"),
                          lims = c(1900,2000))
#> No encoding supplied: defaulting to UTF-8.

result3 %>%
  ggplot(aes(x = date_year, y = fct_reorder(str_trunc(literary_form, 40), WordsPerMillion))) +
  geom_tile(aes(fill = WordsPerMillion)) +
  facet_wrap(~word) +
  scale_fill_gradient2() +
  theme_bw() +
  labs(y = "", x = "Year", title = "Frequency of 'democracy' \nacross literary forms",
       fill = "Words per million") +
  theme(legend.position = "bottom")
```

<img src="man/figures/README-example3-1.png" width="100%" />

It is also possible to further limit the query to, e.g., books published
in a particular language. (Find available fields by using `method =
"returnAvailableFields`). For example, this gives the number of
English-language texts that use the word “democracy” per year from
1760-2000.

``` r
result4 <- query_bookworm(word = c("democracy"), lims = c(1760, 2000), counttype = c("TotalTexts"), language = "English")
#> No encoding supplied: defaulting to UTF-8.

result4 
#> # A tibble: 241 x 3
#>    date_year democracy counttype 
#>        <int>     <int> <chr>     
#>  1      1760       388 TotalTexts
#>  2      1761       393 TotalTexts
#>  3      1762       319 TotalTexts
#>  4      1763       443 TotalTexts
#>  5      1764       320 TotalTexts
#>  6      1765       352 TotalTexts
#>  7      1766       439 TotalTexts
#>  8      1767       402 TotalTexts
#>  9      1768       480 TotalTexts
#> 10      1769       424 TotalTexts
#> # ... with 231 more rows
```

One can use `method = "returnPossibleFields"` to return the fields
available for grouping:

``` r
result5 <- query_bookworm(word = "", method = "returnPossibleFields")
#> No encoding supplied: defaulting to UTF-8.

result5
#> # A tibble: 21 x 6
#>    name           tablename            dbname         type    anchor description
#>    <chr>          <chr>                <chr>          <chr>   <chr>  <chr>      
#>  1 language       languageLookup       language       charac~ bookid ""         
#>  2 publication_c~ publication_country~ publication_c~ charac~ bookid ""         
#>  3 publication_s~ publication_stateLo~ publication_s~ charac~ bookid ""         
#>  4 subclass       subclassLookup       subclass       charac~ bookid ""         
#>  5 narrow_class   narrow_classLookup   narrow_class   charac~ bookid ""         
#>  6 class          classLookup          class          charac~ bookid ""         
#>  7 resource_type  resource_typeLookup  resource_type  charac~ bookid ""         
#>  8 target_audien~ target_audienceLook~ target_audien~ charac~ bookid ""         
#>  9 scanner        scannerLookup        scanner        charac~ bookid ""         
#> 10 first_author_~ first_author_birthL~ first_author_~ charac~ bookid ""         
#> # ... with 11 more rows
```

We can also get a sample of the book titles and links for a particular
year. For example, suppose we’re interested in why so many books in the
category “Education” mention the word “democracy” in 1941, as appears in
the second graph above. This query pulls the first 100 books in the
catalog for 1941 in the category “education”:

``` r

result2 %>% filter(class == "Education", WordsPerMillion == max(WordsPerMillion))
#> # A tibble: 1 x 4
#>   date_year word      class     WordsPerMillion
#>       <int> <chr>     <chr>               <dbl>
#> 1      1941 democracy Education            242.

result6 <- query_bookworm(word = "democracy", groups = "date_year",
                          date_year = "1941", class = "Education", method = "search_results")
#> No encoding supplied: defaulting to UTF-8.

result6 %>%
  head(10) %>%
  knitr::kable()
```

| htid                                                                                                                                           | title                                                           | url                                                                |
| :--------------------------------------------------------------------------------------------------------------------------------------------- | :-------------------------------------------------------------- | :----------------------------------------------------------------- |
| nc01.ark:/13960/t2v41mn4r                                                                                                                      | Teaching democracy in the North Carolina public schools /— 1941 | <https://babel.hathitrust.org/cgi/pt?id=nc01.ark:/13960/t2v41mn4r> |
| uc1.\(b67929 |The education of free men in American democracy. |https://babel.hathitrust.org/cgi/pt?id=uc1.\)b67929                            |                                                                 |                                                                    |
| mdp.39015062763720                                                                                                                             | The education of free men in American democracy.                | <https://babel.hathitrust.org/cgi/pt?id=mdp.39015062763720>        |
| mdp.39015068297905                                                                                                                             | The education of free men in American democracy.— suppl.        | <https://babel.hathitrust.org/cgi/pt?id=mdp.39015068297905>        |
| uc1.\(b67873 |Pennsylvania bill of rights week. Recommendations for school observance ... |https://babel.hathitrust.org/cgi/pt?id=uc1.\)b67873 |                                                                 |                                                                    |
| mdp.39015035886111                                                                                                                             | Education in a world of fear,                                   | <https://babel.hathitrust.org/cgi/pt?id=mdp.39015035886111>        |
| mdp.39015031665543                                                                                                                             | Education and the morale of a free people.                      | <https://babel.hathitrust.org/cgi/pt?id=mdp.39015031665543>        |
| uc1.\(b67928 |Education and the morale of a free people. |https://babel.hathitrust.org/cgi/pt?id=uc1.\)b67928                                  |                                                                 |                                                                    |
| uiug.30112108068831                                                                                                                            | Proceedings of the convention.— 19 1941                         | <https://babel.hathitrust.org/cgi/pt?id=uiug.30112108068831>       |
| uc1.b4305220                                                                                                                                   | Guidance in democratic living,— copy D11                        | <https://babel.hathitrust.org/cgi/pt?id=uc1.b4305220>              |

We can download the Extracted Features file associated with any of these
HathiTrust IDs:

``` r

tmp <- tempdir() 

result6$htid[2] %>%
  download_hathi(dir = tmp)

extracted_features <- get_hathi_counts(result6$htid[2], dir = tmp)

extracted_features
#> # A tibble: 18,482 x 6
#>    htid        token       POS   count section  page
#>    <chr>       <chr>       <chr> <int> <chr>   <int>
#>  1 uc1.$b67929 COMMISSION  NNP       1 body        1
#>  2 uc1.$b67929 in          IN        1 body        1
#>  3 uc1.$b67929 Free        NNP       1 body        1
#>  4 uc1.$b67929 Men         NNP       1 body        1
#>  5 uc1.$b67929 Democracy   NNP       1 body        1
#>  6 uc1.$b67929 School      NNP       1 body        1
#>  7 uc1.$b67929 POLICIES    NNP       1 body        1
#>  8 uc1.$b67929 National    NNP       1 body        1
#>  9 uc1.$b67929 The         DT        1 body        1
#> 10 uc1.$b67929 Association NNP       2 body        1
#> # ... with 18,472 more rows
```

And we can extract the metadata:

``` r
meta <- get_metadata(result6$htid[2], dir = tmp)

meta
#> # A tibble: 2 x 27
#>   schemaVersion dateCreated volumeIdentifier accessProfile rightsAttributes
#>   <chr>         <chr>       <chr>            <chr>         <chr>           
#> 1 1.3           2016-06-17~ uc1.$b67929      google        pd              
#> 2 1.3           2016-06-17~ uc1.$b67929      google        pd              
#> # ... with 22 more variables: hathitrustRecordNumber <chr>,
#> #   enumerationChronology <chr>, sourceInstitution <chr>,
#> #   sourceInstitutionRecordNumber <chr>, oclc <chr>, lccn <chr>, title <chr>,
#> #   imprint <chr>, lastUpdateDate <chr>, governmentDocument <lgl>,
#> #   pubDate <chr>, pubPlace <chr>, language <chr>, bibliographicFormat <chr>,
#> #   genre <chr>, issuance <chr>, typeOfResource <chr>, classification <chr>,
#> #   names <chr>, htBibUrl <chr>, handleUrl <chr>, htid <chr>
```

One can get info about the corpus itself by using `counttype =
"TotalWords"` or `counttype = "TotalTexts"` and omitting the word key.

``` r
result7 <- query_bookworm(counttype = c("TotalTexts"), groups = c("date_year", "language"),
                          lims = c(1500,2000))
#> No encoding supplied: defaulting to UTF-8.

result7 %>%
  summarise(TotalTexts = sum(TotalTexts))
#> # A tibble: 1 x 1
#>   TotalTexts
#>        <int>
#> 1   12534182

library(ggrepel)

result7 %>%
  mutate(language = fct_lump_n(language, 10, w = TotalTexts)) %>%
  group_by(date_year, language) %>%
  summarise(TotalTexts = sum(TotalTexts)) %>%
  group_by(language) %>%
  mutate(label = ifelse(date_year == max(date_year), as.character(language), NA_character_)) %>%
  group_by(language) %>%
  mutate(rolling_avg = slider::slide_dbl(TotalTexts, mean, .before = 10, .after = 10)) %>%
  ggplot() +
  geom_line(aes(x = date_year, y = rolling_avg, color = language), show.legend = FALSE) +
  geom_line(aes(x = date_year, y = TotalTexts, color = language), show.legend = FALSE, alpha = 0.3) +
  geom_text_repel(aes(x = date_year, y = TotalTexts, label = label, color = language), show.legend = FALSE) +
  scale_y_log10() +
  theme_bw() +
  labs(title = "Total texts per language in the HathiTrust bookworm", subtitle = "Log scale. Less common languages grouped as 'other'. 10 year rolling average.", x = "Year", y = "")
#> `summarise()` regrouping output by 'date_year' (override with `.groups` argument)
```

<img src="man/figures/README-example9-1.png" width="100%" />

Note that the accessible HathiTrust Bookworm database is the 2016
version. A more current version of the database exists (with some 17
million digitized texts), but is not publically accessible yet, I think.

## Credits

This package includes some code from the
[hathidy](https://github.com/HumanitiesDataAnalysis/hathidy) and
[edinburgh](https://github.com/bmschmidt/edinburgh/) repos by
@bmschmidt.
