#' Downloads the Hathi Trust big hathifile
#'
#' This function downloads the [big hathifile
#' catalog](https://www.hathitrust.org/hathifiles) with simple metadata for the
#' digitized volumes in the Hathi Trust digital library collection. It can be
#' used in conjunction with [workset_builder] and `rsync` to select an
#' appropriate sample of Hathi Trust Extracted Features files and metadata for
#' further analysis. Warning - it's a 1GB file; if the file has been downloaded
#' already, the function will just return the file name and won't attempt to
#' download again.
#'
#' @param url The URL for the Hathi Trust hathifiles
#'   https://www.hathitrust.org/hathifiles
#' @param dir The directory to use to save the downloaded hathifile. Defaults to
#'   `getOption("hathiTools.hathifile.dir")`, which on loading the package is
#'   just `./raw-hathifiles`.
#' @param full_catalog Whether to download the full catalog, or just the latest
#'   update. Default is `TRUE` - download the full catalog.
#'
#' @return The downloaded filename.
#' @export
download_hathifile <- function(url = "https://www.hathitrust.org/hathifiles",
                               dir = getOption("hathiTools.hathifile.dir"),
                               full_catalog = TRUE) {
  full <- modified <- created <- NULL

  message(stringr::str_glue("Reading file list from {url}..."))
  hathi_page <- rvest::read_html(url)

  link_text <- hathi_page %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()

  link_href <- hathi_page %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  message(stringr::str_glue("Using {dir} to save file"))

  fs::dir_create(dir)

  json_file_url <- link_href[stringr::str_detect(link_text, "json")]
  json_file_name <- link_text[stringr::str_detect(link_text, "json")]
  json_file_name <- file.path(dir, json_file_name)

  message(stringr::str_glue("Reading json file list from {json_file_url} and extracting correct file"))
  utils::download.file(json_file_url, json_file_name)

  json_file_catalog <- jsonlite::fromJSON(json_file_name) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(c(modified, created),
                                ~as.Date(.x, format = "%Y-%m-%dT%T"))) %>%
    dplyr::filter(full == full_catalog)

  url <- json_file_catalog %>%
    dplyr::filter(created == max(created)) %>%
    dplyr::pull(url)

  if(is.null(getOption("hathiTools.hathifile"))) {
    filename <- json_file_catalog %>%
      dplyr::filter(created == max(created)) %>%
      dplyr::pull(filename)

    hathifile <- stringr::str_extract(filename, "[0-9]{8}")
    options(hathiTools.hathifile = hathifile)
  } else {
    filename <- json_file_catalog %>%
      dplyr::filter(stringr::str_detect(filename, getOption("hathiTools.hathifile"))) %>%
      dplyr::pull(filename)
  }


  filename <- file.path(dir, filename)
  fs::file_delete(json_file_name)

  if(fs::file_exists(filename)) {
    message("File has already been downloaded. Returning filename.")
    return(filename)
  }

  message(stringr::str_glue("Downloading {getOption('hathiTools.hathifile')} hathifile and saving it"))
  utils::download.file(url, filename)

  return(filename)

}

#'Loads the raw hathifile into memory
#'
#'This function loads a previously downloaded hathifile into memory (or
#'downloads the latest one if it can't find it). It also turns the column
#'`us_gov_doc_flag` into a logical and eliminates `9999` values for
#'`rights_date_used`.
#'
#'@param filename The name of the downloaded hathifile. If `NULL`, it will
#'  attempt to guess it from `getOption("hathiTools.hathifile.dir")` and
#'  `getOption("hathiTools.hathifile")`; if it can't find it or the file doesn't
#'  exist, it will attempt to download it to the directory in
#'  `getOption("hathiTools.hathifile.dir")` using [download_hathifile].
#'@param dir The directory where the raw hathifile is to be found.
#'@param hathifile_date The date of the hathifile. (A new one is released every
#'  month). This defaults to `getOption("hathiTools.hathifile")`, which is just
#'  the date of the last downloaded hathifile.
#'@param cols If wanted, a set of columns to load. Since the file is so large,
#'  one can reduce memory use by selecting only certain columns. These can be
#'  any of the following: `htid (required), access, rights, ht_bib_key,`
#'  `description, source, source_bib_num, oclc_num, isbn, issn, lccn, title,`
#'  `imprint, rights_reason_code, rights_timestamp, us_gov_doc_flag,`
#'  `rights_date_used, pub_place, lang, bib_fmt, collection_code,`
#'  `content_provider_code, responsible_entity_code, digitization_agent_code,`
#'  `access_profile_code, author`. If cols = "REDUCED", the function loads a
#'  reduced set of columns: `htid, ht_bib_key, description, source, title,`
#'  `imprint, rights_date_used, us_gov_doc_flag`, `lang`, `bib_fmt`, and
#'  `author`
#'@param fix_date Fixes `9999` values in `rights_date_used` by changing them to
#'  `NA`. Default is `TRUE`.
#'
#'@return A very large [tibble], with over 17 million records, loaded into
#'  memory. The tibble package does some lazy loading to minimize resource use,
#'  but fully loaded this data frame takes over 5GB in memory.
#'@export
#'
load_raw_hathifile <- function(filename = NULL,
                               dir = getOption("hathiTools.hathifile.dir"),
                               hathifile_date = getOption("hathiTools.hathifile"),
                               cols,
                               fix_date = TRUE) {

  us_gov_doc_flag <- NULL

  header_file <- "hathi_field_list.rds"

  if(is.null(filename)) {
    if (is.null(hathifile_date)) {
      hathifiles <- dir(dir,
                        pattern = "hathi_full_.+txt.gz") %>%
        sort(decreasing = TRUE)
      if (length(hathifiles) > 0) {
        hathi_filename <- hathifiles[1]
        hathifile_date <- stringr::str_extract(hathi_filename, "[0-9]{8}")
        message(paste("Using", hathifile_date,
                      "as the hathifile date and setting it as the hathifile option"))
        options(hathiTools.hathifile = hathifile_date)
      }
    }

    filename <- fs::dir_ls(
      dir,
      regexp = stringr::str_glue("hathi_full_{hathifile_date}.txt.gz")
    )
  }

  if(!file.exists(filename)) {
    filename <- download_hathifile()
  }

  if(!missing(cols)) {
    if(length(cols) == 1 && cols == "REDUCED") {
      cols <- c("htid", "ht_bib_key", "description", "source", "title",
                "imprint", "rights_date_used", "us_gov_doc_flag", "lang",
                "bib_fmt", "author")
    } else {
      cols <- cols[cols %in% hf_headers]
      if(!"htid" %in% cols) {
        stop("'cols' must contain 'htid'")
      }

    }
  } else {
    cols <- hf_headers
  }

  res <- vroom::vroom(filename, col_names = hf_headers,
                      delim = "\t", quote = "", col_select = dplyr::all_of(cols))

  current_year <- as.POSIXlt(Sys.time())$year + 1900

  if("us_gov_doc_flag" %in% names(res)) {
    res$us_gov_doc_flag <- as.logical(res$us_gov_doc_flag)
  }


  if(fix_date && "rights_date_used" %in% names(res)) {
    message("Fixing rights_date_used column")
    res$rights_date_used <- ifelse(res$rights_date_used > current_year,
                                   NA, res$rights_date_used)
  }


  res %>%
      dplyr::select(dplyr::all_of(cols))

}


#' Add imputed date
#'
#' Adds an 'imputed' date of publication column based on the `imprint` column of
#' the hathifile. This function checks for a year in the `imprint` column using
#' a regex that identifies 4 numbers starting with 1 or 2 and extracts that as
#' an imputed date, after checking that it's not greater than the current year.
#'
#' @param hathi_file The hathifile in memory, typically loaded by
#'   [load_raw_hathifile] (and perhaps filtered etc.). Must contain an "imprint"
#'   column.
#'
#' @return The hathifile with added `imputed date` column.
#' @export
add_imputed_date <- function(hathi_file) {

  stopifnot("imprint" %in% names(hathi_file))

  message("Adding imputed date variable")

  hathi_file$imputed_pub_date <- stringr::str_extract(iconv(hathi_file$imprint, to = "ASCII"),
                                                      "[12][0-9]{3}") %>%
    as.numeric()

  current_year <- as.POSIXlt(Sys.time())$year + 1900

  hathi_file$imputed_pub_date <- ifelse(hathi_file$imputed_pub_date > current_year,
                                   NA, hathi_file$imputed_pub_date)

  hathi_file
}


