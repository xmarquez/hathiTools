#' Downloads a Hathi Trust Extracted Features file for a single Hathi Trust id
#'
#' This function downloads a Hathi Trust Extracted Features file for a single
#' Hathi Trust id and (typically, unless explicitly prevented) caches the result
#' as a CSV file or other type of columnar format suitable for fast loading into
#' R.
#'
#' Note that if you want to download the Extracted Features of many Hathi Trust
#' IDs, it is best to use [rsync_from_hathi] (and then run [cache_all] to
#' convert all the downloaded JSON into a format that is fast to load). That is
#' what you'd normally do if you first built a workset using [workset_builder].
#'
#' @param htid The Hathi Trust id of the item whose extracted features files are
#'   to be downloaded.
#' @param dir The directory to download the extracted features files. Defaults
#'   to `getOption("hathiTools.ef.dir")`, which is just "hathi-ef" on load; the
#'   directory will be automatically created if it doesn't already exist.
#' @param cache_type Type of caching used. Defaults to
#'   `getOption("hathiTools.cachetype")`, which is "csv.gz" on load. Allowed
#'   cache types are: compressed csv (the default), "none" (no local caching of
#'   JSON download; only JSON file kept), "rds", "feather" (suitable for use
#'   with [arrow]; needs the [arrow] package installed), or "text2vec.csv" (a
#'   csv suitable for use with the package
#'   [text2vec](https://cran.r-project.org/package=text2vec)).
#'
#' @return A [tibble::tibble] with the extracted features.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Download the 1862 version of "Democracy in America" by Tocqueville
#' download_hathi_ef("mdp.39015001796443", dir = tempdir())
#' }
download_hathi_ef <- function(htid,
                           dir = getOption("hathiTools.ef.dir"),
                           cache_type = getOption("hathiTools.cachetype")) {

  cache_type <- match.arg(cache_type, c("csv.gz", "none", "rds",
                                        "feather", "text2vec.csv"))

  local_cache <- local_loc(htid, suffix = cache_type, dir = dir)
  if(file.exists(local_cache)) {
    message("File has already been downloaded. Returning existing cached file.")
    res <- read_cached_ef_file(local_cache, cache_type)
    return(res)
  } else {
    local_json <- local_loc(htid, suffix = "json.bz2", dir = dir)
    if(!file.exists(local_json)) {
      download_http(htid, dir = dir)
    }
    ef <- read_json(htid, dir = dir)

    ef <- cache_ef_file(ef, local_cache, cache_type = cache_type)

  }
  ef %>%
    dplyr::mutate(htid = htid, .before = dplyr::everything())

}

#' Reads the downloaded extracted features file for a given Hathi Trust id
#'
#' @param htid The Hathi Trust id of the item whose extracted features files are
#'   to be loaded into memory. If it hasn't been downloaded, the function will
#'   try to download it first via [download_hathi_ef].
#' @param dir The directory where the download extracted features files are to
#'   be found. Defaults to `getOption("hathiTools.ef.dir")`, which is just
#'   "hathi-ef" on load.
#' @param cache_type Type of caching used. Defaults to
#'   `getOption("hathiTools.cachetype")`, which is "csv.gz" on load. Allowed
#'   cache types are: compressed csv (the default), "none" (no local caching of
#'   JSON download; only JSON file kept), "rds", "feather" (suitable for use
#'   with [arrow]; needs the [arrow] package installed), or "text2vec.csv" (a
#'   csv suitable for use with the package
#'   [text2vec](https://cran.r-project.org/package=text2vec)).
#'
#' @return a [tibble] with the extracted features.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download the 1863 version of "Democracy in America" by Tocqueville
#'
#' tmp <- tempdir()
#'
#' download_hathi_ef("aeu.ark:/13960/t3qv43c3w", dir = tmp)
#'
#' #Get the downloaded extracted features
#' get_hathi_counts("aeu.ark:/13960/t3qv43c3w", dir = tmp)
#'
#' }
get_hathi_counts <- function(htid,
                             dir = getOption("hathiTools.ef.dir"),
                             cache_type = getOption("hathiTools.cachetype")) {

  cache_type <- match.arg(cache_type, c("csv.gz", "none", "rds",
                                        "feather", "text2vec.csv"))

  local_cache <- local_loc(htid, suffix = cache_type, dir = dir)
  if(!file.exists(local_cache)) {
    ef <- download_hathi_ef(htid, dir = dir, cache_type = cache_type)
  } else {
    ef <- read_cached_ef_file(local_cache, cache_type = cache_type)
  }
  ef %>%
    dplyr::mutate(htid = htid, .before = dplyr::everything())
}

#' Reads the metadata of a single downloaded Hathi Trust extracted features file
#'
#' Note that if you want to extract the metadata of more than one Hathi Trust ID
#' at a time, it is best to simply query the Workset Builder database using
#' [get_workset_meta]. Using [get_workset_meta] also guarantees you'll get a
#' rectangular data with HT ids in the rows. It is also possible to get simple
#' metadata for large numbers of htids by downloading the big hathifile using
#' [download_hathifile] and then filtering it.
#'
#' @param htid The Hathi Trust id of the item whose extracted features files are
#'   to be downloaded.
#' @param dir The directory where the file is saved. Defaults to
#'   `getOption("hathiTools.ef.dir")`, which is just "./hathi-ef/ on load. If
#'   the file does not exist, this function will first attempt to download it
#'   using [download_hathi_ef].
#'
#' @return a [tibble] with metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download the 1862 version of "Democracy in America" by Tocqueville
#'
#' tmp <- tempdir()
#' download_hathi_ef("mdp.39015001796443", dir = tmp)
#'
#' get_hathi_meta("mdp.39015001796443", dir = tmp)
#' }
get_hathi_meta <- function (htid, dir = getOption("hathiTools.ef.dir")) {
  local_json <- local_loc(htid, suffix = "json.bz2", dir = dir)
  meta <- value <- NULL
  if(!file.exists(local_json)) {
    download_hathi_ef(htid, dir = dir)
  }
  meta <- load_json(htid, dir = dir)$metadata

  length_zero_cols <- meta %>% purrr::map_lgl(~{length(.) == 0})
  meta_names <- names(meta)
  meta_names <- meta_names[ !length_zero_cols ]
  meta <- meta[ !length_zero_cols]

  flatten_data <- function(x) {
    tibble::as_tibble_col(x) %>%
      tidyr::unnest(value) %>%
      dplyr::mutate(value = as.character(value))
  }
  meta <- meta %>% purrr::map(flatten_data) %>% dplyr::bind_rows(.id = "field")
  meta$htid <- htid
  meta
}

read_cached_ef_file <- function(filename, cache_type) {
  if(cache_type %in% c("csv.gz", "csv", "text2vec.csv")) {
    res <- vroom::vroom(filename)
  }
  if(cache_type %in% c("rds")) {
    res <- readRDS(filename)
  }
  if(cache_type %in% c("feather")) {
    res <- arrow::read_feather(filename)
  }
  res
}

cache_ef_file <- function(ef, filename, cache_type) {

  section <- page <- token <- count <- POS <- NULL

  if(stringr::str_detect(cache_type, "text2vec")) {

    ef <- ef %>%
      dplyr::group_by(section, page) %>%
      dplyr::summarise(token = stringr::str_c(rep(token, count), "_", rep(POS, count), collapse = " "),
                       .groups = "drop")

  }
  if(cache_type %in% c("csv.gz", "csv", "text2vec.csv")) {
    vroom::vroom_write(ef, filename, delim = ",")
  }
  if(cache_type == "rds") {
    saveRDS(ef, filename, compress = TRUE)
  }
  if(cache_type == "feather") {
    arrow::write_feather(ef, filename)
  }

  ef

}

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
  slashes <- stringr::str_sub(cleaned, breaks, breaks + 1) %>%
    stringr::str_c(sep = "/", collapse = "/")
  stringr::str_c(splitted[1], "pairtree_root", slashes, cleaned,
                 sep = "/")
}

local_loc <- function(htid, suffix = "json", dir) {
  clean <- htid %>% id_clean()
  stub <- stubbytree(htid)
  stringr::str_glue("{dir}/{stub}/{clean}.{suffix}")
}

load_json <- function(htid, check_suffixes = c("json", "json.bz2", "json.gz"), dir) {
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


stubbytree <- function(htid) {
  splitted <- stringr::str_split(htid, "\\.", n = 2)[[1]]
  if (length(splitted) == 1) {
    stop(stringr::str_glue("malformed htid {htid}: Hathi ids should contain a period"))
  }
  splitted[2]
  cleaned <- splitted[2] %>% id_clean()
  breaks <- seq(1, by = 3, length.out = nchar(cleaned)/3)
  stubbydir <- stringr::str_sub(cleaned, breaks, breaks) %>% stringr::str_c(collapse = "")
  stringr::str_c(splitted[1], stubbydir, sep = "/")
}

stubby_url_to_rsync <- function(htid) {
  tree <- stubbytree(htid)
  clean <- id_clean(htid)
  url <- stringr::str_glue("{tree}/{clean}.json.bz2")
  url
}

parse_page <- function(page) {
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

parse_section <- function(page, section) {
  d <- page[[section]]$tokenPosCount
  if (length(d)) {
    lens <- sapply(d, length)
    poses <- lapply(d, names) %>% unlist()
    return(tibble::tibble(token = rep(names(d), times = lens), POS = poses,
                          count = unlist(d), section = section))
  }
  return(NULL)
}

parse_listified_book <- function(listified_version) {
  page <- NULL
  listified_version$features$pages %>%
    purrr::map(parse_page) %>%
    purrr::flatten() %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(page = as.numeric(page))
}

download_http <- function(htid, dir) {
  local_name <- local_loc(htid, suffix = c("json.bz2"), dir = dir)
  if(file.exists(local_name)) {
    return(local_name)
  } else {
    tree <- stubbytree(htid)
    clean <- id_clean(htid)
    url <- stringr::str_glue("http://data.analytics.hathitrust.org/features-2020.03/{tree}/{clean}.json.bz2")
    dir.create(dirname(local_name), showWarnings = FALSE, recursive = TRUE)
    utils::download.file(url = url, destfile = local_name)
    local_name
  }
}

read_json <- function(htid, dir) {
  page <- count <- NULL

  listified_version <- load_json(htid,
                                 check_suffixes = c("json.bz2", "json"),
                                 dir = dir) %>%
    parse_listified_book() %>%
    dplyr::mutate(page = as.integer(page), count = as.integer(count))
}



