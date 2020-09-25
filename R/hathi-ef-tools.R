id_encode <- function (htid) {
  htid %>%
    id_clean() %>%
    stringr::str_replace_all("\\.", ",")
}

id_clean <- function (htid) {
  htid %>%
    stringr::str_replace_all(":", "+") %>%
    stringr::str_replace_all("/", "=")
}

pairtree <- function (htid) {
  splitted <- stringr::str_split(htid, "\\.", n = 2)[[1]]
  if (length(splitted) == 1) {
    stop(stringr::str_glue("malformed htid {htid}: Hathi ids should contain a period"))
  }
  breaks <- seq(1, nchar(splitted[2]), by = 2)
  cleaned <- splitted[2] %>% id_encode()
  slashes <- stringr::str_sub(cleaned, breaks, breaks + 1) %>% str_c(sep = "/",
                                                                     collapse = "/")
  stringr::str_c(splitted[1], "pairtree_root", slashes, cleaned,
                 sep = "/")
}

local_loc <- function (htid, suffix = "json", dir)
{
  clean <- htid %>% id_clean()
  pairtr <- pairtree(htid)
  stringr::str_glue("{dir}/{pairtr}/{clean}.{suffix}")
}

load_json <- function (htid, check_suffixes = c("json", "json.bz2", "json.gz"), dir)
{
  for (suffix in check_suffixes) {
    fname <- local_loc(htid, suffix = suffix, dir = dir)
    if (file.exists(fname)) {
      tryCatch(return(jsonlite::read_json(fname)), error = function(e) {
        stop(stringr::str_glue("Unable to load features from {fname}: possibly a partially downloaded file that must be removed?"))
      })
    }
  }
  NULL
}

#' Reads the metadata of a downloaded Hathi Trust extracted features file
#'
#' @param htid The Hathi Trust id of the item whose extracted features files are
#'   to be downloaded.
#' @param dir The directory where the file is saved. Defaults to `hathi-ef`. If
#'   the file does not exist, it will first be downloaded using
#'   [download_hathi].
#'
#' @return a [tibble] with metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download the 1863 version of "Democracy in America" by Tocqueville
#'
#' tmp <- tempdir()
#' download_hathi("miun.aew4744.0001.001", dir = tmp)
#'
#' get_metadata("miun.aew4744.0001.001", dir = tmp)
#' }
get_metadata <- function (htid, dir = "hathi-ef")
{
  local_json <- local_loc(htid, suffix = "json", dir = dir)
  meta <- NULL
  if(!file.exists(local_json)) {
    download_hathi(htid, dir = dir)
  }
  meta <- load_json(htid, dir = dir)[[2]]

  length_zero_cols <- meta %>% map_lgl(~{length(.) == 0})
  meta_names <- names(meta)
  meta_names <- meta_names[ !length_zero_cols ]
  meta <- meta[ !length_zero_cols]
  meta <- meta %>% map(as_tibble_col) %>% bind_cols(.name_repair = ~meta_names)
  meta$htid <- htid
  meta %>%
    unnest(everything()) %>%
    unnest(everything())
}

download_http <- function(htid, dir)
{
  local_name <- local_loc(htid, dir = dir)
  url <- stringr::str_glue("https://data.analytics.hathitrust.org/htrc-ef-access/get?action=download-ids&id={htid}&output=json")
  dir.create(dirname(local_name), showWarnings = FALSE, recursive = TRUE)
  utils::download.file(url = url, destfile = local_name)
}

parse_page <- function (page)
{
  parts <- c("body", "header", "footer")
  seq <- as.numeric(page$seq)
  body <- parts %>%
    purrr::map(~parse_section(page, .x)) %>%
    purrr::discard(is.null) %>%
    purrr::map(~{
      .x$page = seq
      .x
    })
  body
}

parse_section <- function (page, section)
{
  d <- page[[section]]$tokenPosCount
  if (length(d)) {
    lens <- sapply(d, length)
    poses <- lapply(d, names) %>% unlist()
    return(tibble::tibble(token = rep(names(d), times = lens), POS = poses,
                          count = unlist(d), section = section))
  }
  return(NULL)
}

parse_listified_book <- function (listified_version)
{
  listified_version$features$pages %>%
    purrr::map(parse_page) %>%
    rlang::flatten() %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(page = as.numeric(page))
}

#' Downloads a Hathi Trust Extracted Features file for a given Hathi Trust id
#'
#' @param htid The Hathi Trust id of the item whose extracted features files are
#'   to be downloaded.
#' @param dir The directory to download the extracted features files. Defaults
#'   to "hathi-ef"; the directory will be automatically created if it doesn't
#'   already exist.
#'
#' @return Saves the json and csv versions of the extracted features.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Download the 1863 version of "Democracy in America" by Tocqueville
#' download_hathi("miun.aew4744.0001.001", dir = tempdir())
#' }
download_hathi <- function(htid, dir = "hathi-ef") {
  local_csv <- local_loc(htid, suffix = "csv.gz", dir = dir)
  if(file.exists(local_csv)) {
    return()
  } else {
    download_http(htid, dir = dir)
    listified_version <- load_json(htid, check_suffixes = c("json"), dir = dir)
    tibble <- listified_version %>% parse_listified_book()
    tibble %>% mutate(page = as.integer(page), count = as.integer(count)) %>%
      readr::write_csv(local_csv)
  }

}

#' Reads the downloaded extracted features file for a given Hathi Trust id
#'
#' @param htid The Hathi Trust id of the file whose extracted features are to be
#'   returned. If the file does not exist, it is first downloaded using [download_hathi].
#' @param dir The directory where the file is saved. Defaults to `hathi-ef`.
#'
#' @return a [tibble] with the extracted features, by page.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download the 1863 version of "Democracy in America" by Tocqueville
#'
#' tmp <- tempdir()
#' download_hathi("miun.aew4744.0001.001", dir = tmp)
#'
#' get_hathi_counts("miun.aew4744.0001.001", dir = tmp)
#'
#' }
get_hathi_counts <- function(htid, dir = "hathi-ef") {
  local_csv <- local_loc(htid, suffix = "csv.gz", dir = dir)
  if(!file.exists(local_csv)) {
    download_hathi(htid, dir = dir)
  }
  if (file.exists(local_csv)) {
    df <- readr::read_csv(local_csv, col_types = "ccici", progress = FALSE)
    df$htid <- htid
    df <- df %>%
      dplyr::relocate(htid)
    return(df)

  } else {
    return(tibble::tibble())
  }
}
