# Hathifile Update can be downloaded

    Code
      hf
    Output
      [1] "./raw-hathifiles/hathi_upd_20220119.txt.gz"

# Hathifile can be loaded

    Code
      hf
    Output
      # A tibble: 22,092 x 5
         htid               rights_date_used author        title            imprint   
         <chr>                         <dbl> <chr>         <chr>            <chr>     
       1 mdp.39015046365196             1971 Kar<U+1E6D>a (Firm). Secure and reco~ 1971]     
       2 mdp.39015005644151             1971 Grimm, Ludwi~ Erinnerungen au~ H. Lang, ~
       3 mdp.39015001371791             1968 Chekhov, Ant~ The portable Ch~ Viking Pr~
       4 mdp.39015010410796             1959 Gibran, Kahl~ Kahlil Gibran, ~ Citadel P~
       5 mdp.39015007995239             1974 Chao, Mei Pa~ The yellow bell~ Gordon Pr~
       6 mdp.39015033218374             1958 Pagel, Walte~ Paracelsus ; an~ Karger, 1~
       7 inu.32000001235441             1980 Avedon, Burt. Ah, men! : What~ A & W Pub~
       8 mdp.39015022632585             1797 Holliday, Jo~ The life of Wil~ Printed f~
       9 mdp.35112104983533             1797 Holliday, Jo~ The life of Wil~ Printed f~
      10 mdp.39015068127649             1905 Deutsche Phy~ Verhandlungen.   F. Viewig~
      # ... with 22,082 more rows

# Imputed date is properly calculated

    Code
      hf_imputed
    Output
      # A tibble: 22,092 x 3
         rights_date_used imprint                      imputed_pub_date
                    <dbl> <chr>                                   <dbl>
       1             1971 1971]                                    1971
       2             1971 H. Lang, 1971.                           1971
       3             1968 Viking Press [1968]                      1968
       4             1959 Citadel Press [1959]                     1959
       5             1974 Gordon Press, 1974.                      1974
       6             1958 Karger, 1958.                            1958
       7             1980 A & W Publishers, c1980.                 1980
       8             1797 Printed for P. Elmsly, 1797.             1797
       9             1797 Printed for P. Elmsly, 1797.             1797
      10             1905 F. Viewig und sohn [etc.]                  NA
      # ... with 22,082 more rows

