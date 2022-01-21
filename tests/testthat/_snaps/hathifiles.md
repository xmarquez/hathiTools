# Hathifile can be loaded

    Code
      hf <- download_hathifile(dir = dir, full_catalog = FALSE) %>%
        load_raw_hathifile(dir = dir, cols = c("htid", "rights_date_used", "author",
          "title", "imprint"))
    Message <simpleMessage>
      Reading file list from https://www.hathitrust.org/hathifiles...
      Using ./raw-hathifiles to save file
      Reading json file list from https://www.hathitrust.org/filebrowser/download/244651 and extracting correct file
      File has already been downloaded. Returning filename.
    Message <vroom_dim_message>
      Rows: 17039 Columns: 5
    Message <vroom_spec_message>
      -- Column specification --------------------------------------------------------
      Delimiter: "\t"
      chr (4): htid, title, imprint, author
      dbl (1): rights_date_used
      
      i Use `spec()` to retrieve the full column specification for this data.
      i Specify the column types or set `show_col_types = FALSE` to quiet this message.
    Message <simpleMessage>
      Fixing rights_date_used column

