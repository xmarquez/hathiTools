#' Caches downloaded JSON Extracted Features files to another format
#'
#' This function takes a set of Hathi Trust IDs (usually already downloaded via
#' [rsync_from_hathi]) and caches the JSON files to another format (e.g., csv or
#' rds or parquet) along them. A typical workflow with this package normally
#' involves selecting an appropriate set of Hathi Trust IDs (via
#' [workset_builder]), downloading their Extracted Features files to your local
#' machine (via [rsync_from_hathi]), caching these slow-to-load JSON Extracted
#' Features files to a faster-loading format using [cache_htids], and then using
#' [read_cached_htids] to read them into a single data frame or [arrow
#' Dataset][arrow::Dataset] for further work.
#'
#' @param htids A character vector of Hathi Trust ids, a workset created with
#'   [workset_builder], or a data frame with a column named "htid" containing
#'   the Hathi Trust ids that require caching. If the JSON Extracted Features
#'   files for these htids have not been downloaded via [rsync_from_hathi] or
#'   [get_hathi_counts] to `dir`, nothing will be cached (unless `attempt_rsync`
#'   is `TRUE`).
#' @inheritParams get_hathi_counts
#' @param cache_type Type of information cached. The default is c("ef", "meta",
#'   "pagemeta"), which refers to the extracted features, the volume metadata,
#'   and the page metadata. Omitting one of these caches or finds only the rest
#'   (e.g., `cache_type = "ef"` caches only the EF files, not their associated
#'   metadata or page metadata).
#' @param keep_json Whether to keep the downloaded json files. Default is
#'   `TRUE`; if `FALSE`, it only keeps the local cached files (e.g., the csv
#'   files) and deletes the associated JSON files.
#' @param attempt_rsync If `TRUE`, and some JSON EF files are not found in
#'   `dir`, the function will call [rsync_from_hathi] to attempt to download
#'   these first.
#'
#' @return A [tibble] with the paths of the cached files and an indicator of
#'   whether each htid was successfully cached.
#'
#' @export
#' @examples
#' \donttest{
#' htids <- c("mdp.39015008706338", "mdp.39015058109706")
#' dir <- tempdir()
#'
#' # Caches nothing (nothing has been downloaded to `dir`):
#'
#' cache_htids(htids, dir = dir, cache_type = "ef")
#'
#' # Tries to rsync first, then caches
#'
#' cache_htids(htids, dir = dir, cache_type = "ef", attempt_rsync = TRUE)
#'
#' }
cache_htids <- function(htids,
                        dir = getOption("hathiTools.ef.dir"),
                        cache_type = c("ef", "meta", "pagemeta"),
                        cache_format = getOption("hathiTools.cacheformat"),
                        keep_json = TRUE,
                        attempt_rsync = FALSE) {

  exists.y <- exists.x <- page <- count <- htid <- NULL
  local_loc.x <- cache_type.x <- cache_format.x <- NULL

  cache_type <- match.arg(cache_type, c("ef", "meta", "pagemeta"),
                          several.ok = TRUE)

  cache_format <- match.arg(cache_format, c("csv.gz", "rds",
                                            "feather", "text2vec.csv",
                                            "parquet"),
                            several.ok = TRUE)

  htids <- check_htids(htids)

  json_file_locs <-  find_cached_htids(htids,
                                       dir = dir,
                                       cache_type = "none",
                                       cache_format = "none",
                                       existing_only = FALSE)

  not_found <- json_file_locs %>%
    dplyr::filter(!exists)

  if(attempt_rsync && nrow(not_found) > 0) {
    message("Attempting to rsync ", nrow(not_found),
            " Hathi Trust IDs before caching")

    rsync_from_hathi(not_found, dir = dir)

    json_file_locs <-  find_cached_htids(htids,
                                         dir = dir,
                                         cache_type = "none",
                                         cache_format = "none",
                                         existing_only = FALSE)
  }

  to_cache <- find_cached_htids(htids,
                                dir = dir,
                                cache_type = cache_type,
                                cache_format = cache_format,
                                existing_only = FALSE) %>%
    needs_cache(json_file_locs, cache_format) %>%
    dplyr::group_by(htid) %>%
    dplyr::summarise(local_cache = list(local_loc.x),
                     cache_type = list(cache_type.x),
                     cache_format = list(cache_format.x),
                     dir = dir)

  message("Preparing to cache ",
          nrow(to_cache), " EF files to ",
          fs::path_real(dir), " (",
          fs::path_rel(dir), ") ")

  to_cache %>%
    purrr::pwalk(cache_single_htid)

  res <- find_cached_htids(htids, dir = dir, cache_type = cache_type,
                           cache_format = cache_format)

  res

}


#' Finds cached Extracted Features files for a set of HT ids
#'
#' @inheritParams cache_htids
#' @param existing_only Whether to return only file paths to files that actually
#'   exist. Default is `TRUE`. Use `FALSE` to find whether some files still need
#'   to be cached.
#'
#' @return A [tibble] with the paths of the cached files and an indicator of
#'   whether each htid has an existing cached file.
#'
#' @export
#' @examples
#' \donttest{
#' htids <- c("mdp.39015008706338", "mdp.39015058109706")
#' dir <- tempdir()
#'
#' # Finds nothing (nothing has been downloaded or cached to `dir`):
#'
#' find_cached_htids(htids, cache_format = c("none", "csv"), dir = dir)
#'
#' cache_htids(htids, dir = dir, cache_type = "ef", attempt_rsync = TRUE)
#'
#' # Finds the cached files and their JSON ef files
#'
#' find_cached_htids(htids, cache_format = c("none", "csv"), dir = dir)
#' }
find_cached_htids <- function(htids,
                              dir = getOption("hathiTools.ef.dir"),
                              cache_type = c("ef", "meta", "pagemeta"),
                              cache_format = getOption("hathiTools.cacheformat"),
                              existing_only = TRUE) {

  cache_type <- match.arg(cache_type, c("none", "ef", "meta", "pagemeta"), several.ok = TRUE)

  cache_format <- match.arg(cache_format, c("csv.gz", "none", "rds",
                                            "feather", "text2vec.csv",
                                            "parquet"),
                            several.ok = TRUE)

  htids <- check_htids(htids)

  if("none" %in% cache_type) {
    cache_format <- unique(c("none", cache_format))
  }

  if("none" %in% cache_format) {
    cache_type <- unique(c("none", cache_type))
  }

  caches <- tidyr::expand_grid(cache_type, cache_format, htids) %>%
    dplyr::filter(!(cache_type == "none" & cache_format != "none"),
                  !(cache_type != "none" & cache_format == "none")) %>%
    dplyr::mutate(cache_format = dplyr::case_when(cache_format == "none" ~ "json.bz2",
                                                  TRUE ~ cache_format),
                  suffix = paste0(cache_type, ".", cache_format) %>%
                    stringr::str_remove("ef\\.|none\\."))

  res <- caches %>%
    purrr::pmap_df(find_cached_file, dir = dir)

  if(existing_only) {
    res <- res %>%
      dplyr::filter(exists)
  }

  res

}

find_cached_file <- function(cache_type, cache_format, htids, suffix, dir) {
  htid <- NULL

  tibble::tibble(htid = htids,
                 local_loc = local_loc(htid = htid,
                                       suffix = suffix,
                                       dir = dir),
                 cache_format = cache_format,
                 cache_type = cache_type,
                 exists = fs::file_exists(local_loc))
}

#' Removes cached files for a set of Hathi Trust ids
#'
#' @inheritParams find_cached_htids
#' @param cache_type Type of information to remove. The default is c("ef",
#'   "meta", "pagemeta"), which refers to the extracted features, the volume
#'   metadata, and the page metadata in `dir`. Omitting one of these removes
#'   only them (e.g., cache_type = "ef" removes only the EF files, not their
#'   associated metadata or page metadata).
#' @param cache_format The format of the cached EF files to remove. Defaults to
#'   c("csv.gz", "rds", "feather", "text2vec.csv", "parquet"), i.e., all
#'   formats.
#' @param keep_json Whether to keep any downloaded JSON files. Default is
#'   `TRUE`; if `FALSE` will delete all JSON extracted features associated with
#'   the set of htids.
#'
#' @note
#'
#' Warning! This function does not double-check that you want to delete your
#' cache. It will go ahead and do it.
#'
#' @return (Invisible) a character vector with the deleted paths.
#'
#' @export
#' @examples
#' \donttest{
#' dir <- tempdir()
#'
#' htids <- c("mdp.39015008706338", "mdp.39015058109706")
#' dir <- tempdir()
#'
#' cache_htids(htids, dir = dir, cache_type = "ef", attempt_rsync = TRUE)
#'
#' # Clears only "csv" cache
#'
#' deleted <- clear_cache(htids, dir = dir)
#' deleted
#'
#' # Clears also JSON files
#'
#' deleted <- clear_cache(htids, dir = dir, keep_json = FALSE)
#' deleted
#'
#' }
clear_cache <- function(htids,
                        dir = getOption("hathiTools.ef.dir"),
                        cache_type = c("ef", "meta", "pagemeta"),
                        cache_format = c("csv.gz", "rds", "feather",
                                         "text2vec.csv", "parquet"),
                        keep_json = TRUE) {

  htids <- check_htids(htids)

  cache_format <- match.arg(cache_format,
                            c("csv.gz", "rds",
                              "feather", "text2vec.csv",
                              "parquet"),
                            several.ok = TRUE)

  if(!keep_json) {
    cache_type <- c("none", cache_type)
  }

  caches <- find_cached_htids(htids,
                              dir = dir,
                              cache_type = cache_type,
                              cache_format = cache_format)

  message("Now deleting ",
          nrow(caches), " cached files in ",
          fs::path_real(dir), " (",
          fs::path_rel(dir), ") ")

  fs::file_delete(caches$local_loc)

}

#' Read Cached HTIDs
#'
#' Takes a set of Hathi Trust IDs and reads their extracted features and
#' associated (page- and volume- level) metadata into memory or into an [arrow
#' Dataset][arrow::Dataset]. A typical workflow with this package should
#' normally involve selecting an appropriate set of Hathi Trust IDs (via
#' [workset_builder]), downloading their Extracted Features files to your local
#' machine (via [rsync_from_hathi]), caching these slow-to-load JSON Extracted
#' Features files to a faster-loading format using [cache_htids], and then using
#' [read_cached_htids] to read them into a single data frame or [arrow
#' Dataset][arrow::Dataset] for further work.
#'
#' @inheritParams find_cached_htids
#' @param nest_char_count Whether to create a column with a tibble for the
#'   `sectionBeginCharCount` and `sectionEndCharCount` columns in the page
#'   metadata. The default is `FALSE`; if so the counts of characters at the
#'   beginning and end of lines are left as a JSON-formatted string (which can
#'   in turn be transformed into a tibble manually).
#'
#'
#' @return A [tibble][tibble::tibble] with the extracted features, plus the
#'   desired (volume-level or page-level) metadata, or an [arrow
#'   Dataset][arrow::Dataset].
#'
#' @export
#'
#' @examples
#' \donttest{
#' htids <- c("mdp.39015008706338", "mdp.39015058109706")
#' dir <- tempdir()
#'
#' # Download and cache files first:
#'
#' cache_htids(htids, dir = dir, cache_type = "ef", attempt_rsync = TRUE)
#'
#' # Now read them into memory:
#'
#' efs <- read_cached_htids(htids, dir = dir)
#' efs
#'
#' }
read_cached_htids <- function(htids,
                              dir = getOption("hathiTools.ef.dir"),
                              cache_type = c("ef", "meta", "pagemeta"),
                              cache_format = getOption("hathiTools.cacheformat"),
                              nest_char_count = FALSE) {
  htids <- check_htids(htids)

  cache_format <- match.arg(cache_format, c("csv.gz", "rds", "feather",
                                            "text2vec.csv", "parquet"))

  cached <- find_cached_htids(htids,
                              dir = dir,
                              cache_type = cache_type,
                              cache_format = cache_format) %>%
    tidyr::pivot_wider(id_cols = "htid", names_from = "cache_type", values_from = "local_loc")

  if(nrow(cached) == 0) {
    stop("No files cached to ", cache_format, " found. ",
         "Run cache_htids(htids, cache_format = ",cache_format,
         ") first.")
  }

  if(is.null(cached[[cache_format]])) {
    cached[[cache_format]] <- NA_character_
  }

  if(is.null(cached[["meta"]])) {
    cached[["meta"]] <- NA_character_
  }

  if(is.null(cached[["pagemeta"]])) {
    cached[["pagemeta"]] <- NA_character_
  }

  method_name <- get(paste0("assemble_from_cache.", cache_format))
  method_name(cached, cache_format, cache_type, nest_char_count)

}

assemble_from_cache.text2vec.csv <- function(cached, cache_format, cache_type, nest_char_count) {

  assemble_from_cache.csv.gz(cached, cache_format, cache_type, nest_char_count)

}


#' @importFrom stats na.omit
assemble_from_cache.csv.gz <- function(cached, cache_format, cache_type, nest_char_count) {

  ef <- meta <- pagemeta <- NULL

  fun_ef <- function(x) {vroom::vroom(x, delim = ",",
                                      show_col_types = FALSE,
                                      col_types = vroom::cols(count = "i",
                                                              page = "i"))}
  fun_meta <- function(x) {vroom::vroom(x, delim = ",",
                                        show_col_types = FALSE,
                                        col_types = vroom::cols(pubDate = "i",
                                                                dateCreated = "i",
                                                                lastRightsUpdateDate = "i"))}

  fun_pagemeta <- function(x) {vroom::vroom(x, delim = ",",
                                            show_col_types = FALSE,
                                            col_types = vroom::cols(tokenCount = "i",
                                                                    page = "i",
                                                                    lineCount = "i",
                                                                    emptyLineCount = "i",
                                                                    sentenceCount = "i",
                                                                    sectionTokenCount = "i",
                                                                    sectionLineCount = "i",
                                                                    sectionEmptyLineCount = "i",
                                                                    sectionSentenceCount = "i",
                                                                    sectionCapAlphaSeq = "i"))}


  if("ef" %in% cache_type && !all(is.na(cached$ef))) {
    ef_loc <- stats::na.omit(cached$ef)
    suppressWarnings(ef <- fun_ef(ef_loc))
  }
  if("meta" %in% cache_type && !all(is.na(cached$meta))) {
    meta_loc <- stats::na.omit(cached$meta)
    meta <- meta_loc %>%
      purrr::map_df(fun_meta)
  }
  if("pagemeta" %in% cache_type && !all(is.na(cached$pagemeta))) {
    pagemeta_loc <- stats::na.omit(cached$pagemeta)
    pagemeta <- pagemeta_loc %>%
      purrr::map_df(fun_pagemeta)

    if(nest_char_count) {
      pagemeta <- pagemeta %>%
        dplyr::rowwise() %>%
        dplyr::mutate(dplyr::across(dplyr::any_of(c("sectionBeginCharCount",
                                                    "sectionEndCharCount")),
                                    ~list(tibble::enframe(jsonlite::fromJSON(.))))) %>%
        dplyr::ungroup()

    }
  }
  to_join <- list(ef, meta, pagemeta) %>%
    purrr::compact()

  assemble_df(to_join)

}

#' @importFrom stats na.omit
assemble_from_cache.parquet <- function(cached, cache_format, cache_type, nest_char_count) {

  if(!(length(find.package("arrow", quiet = TRUE)) > 0)) {
    stop("Must have 'arrow' package installed to use 'parquet' cache")
  }

  ef <- meta <- pagemeta <- sectionBeginCharCount <- sectionEndCharCount <- NULL

  if("ef" %in% cache_type && !all(is.na(cached$ef))) {
    ef_loc <- stats::na.omit(cached$ef)
    ef <- arrow::open_dataset(ef_loc, format = "parquet")
  }
  if("meta" %in% cache_type && !all(is.na(cached$meta))) {
    meta_loc <- stats::na.omit(cached$meta)
    meta <- arrow::open_dataset(meta_loc, format = "parquet")
  }
  if("pagemeta" %in% cache_type && !all(is.na(cached$pagemeta))) {
    pagemeta_loc <- stats::na.omit(cached$pagemeta)
    pagemeta <- arrow::open_dataset(pagemeta_loc, format = "parquet")
  }
  to_join <- list(ef, meta, pagemeta) %>%
    purrr::compact()

  assemble_df(to_join)

}

#' @importFrom stats na.omit
assemble_from_cache.feather <- function(cached, cache_format, cache_type, nest_char_count) {

  if(!(length(find.package("arrow", quiet = TRUE)) > 0)) {
    stop("Must have 'arrow' package installed to use 'feather' cache")
  }

  ef <- meta <- pagemeta <- sectionBeginCharCount <- sectionEndCharCount <- NULL

  if("ef" %in% cache_type && !all(is.na(cached$ef))) {
    ef_loc <- stats::na.omit(cached$ef)
    ef <- arrow::open_dataset(ef_loc, format = "feather")
  }
  if("meta" %in% cache_type && !all(is.na(cached$meta))) {
    meta_loc <- stats::na.omit(cached$meta)
    meta <- arrow::open_dataset(meta_loc, format = "feather")
  }
  if("pagemeta" %in% cache_type && !all(is.na(cached$pagemeta))) {
    pagemeta_loc <- stats::na.omit(cached$pagemeta)
    pagemeta <- arrow::open_dataset(pagemeta_loc, format = "feather")
  }
  to_join <- list(ef, meta, pagemeta) %>%
    purrr::compact()

  assemble_df(to_join)

}

#' @importFrom stats na.omit
assemble_from_cache.rds <- function(cached, cache_format, cache_type, nest_char_count) {

  ef <- meta <- pagemeta <- NULL

  if("ef" %in% cache_type && !all(is.na(cached$ef))) {
    ef_loc <- stats::na.omit(cached$ef)
    ef <- ef_loc %>%
      purrr::map_df(readRDS)
  }
  if("meta" %in% cache_type && !all(is.na(cached$meta))) {
    meta_loc <- stats::na.omit(cached$meta)
    meta <- meta_loc %>%
      purrr::map_df(readRDS)
  }
  if("pagemeta" %in% cache_type && !all(is.na(cached$pagemeta))) {
    pagemeta_loc <- stats::na.omit(cached$pagemeta)
    pagemeta <- pagemeta_loc %>%
      purrr::map_df(readRDS)

    if(nest_char_count) {
      pagemeta <- pagemeta %>%
        dplyr::rowwise() %>%
        dplyr::mutate(dplyr::across(dplyr::any_of(c("sectionBeginCharCount",
                                                    "sectionEndCharCount")),
                                    ~list(tibble::enframe(jsonlite::fromJSON(.))))) %>%
        dplyr::ungroup()

    }

  }
  to_join <- list(ef, meta, pagemeta) %>%
    purrr::compact()

  assemble_df(to_join)

}

assemble_df <- function(to_join) {
  if(length(to_join) == 0) {
    return(NULL)
  }
  if(length(to_join) == 1) {
    return(to_join[[1]])
  }
  if(length(to_join) == 2) {
    return(suppressMessages(dplyr::left_join(to_join[[1]], to_join[[2]])))
  }
  if(length(to_join) == 3) {
    return(dplyr::left_join(to_join[[1]], to_join[[2]], by = "htid") %>%
             dplyr::left_join(to_join[[3]], by = c("htid", "page", "section")))
  }
  return(NULL)
}

read_cached_file <- function(filename, cache_format) {

  if(stringr::str_detect(cache_format, "csv")) {
    res <- vroom::vroom(filename, show_col_types = FALSE)
  }
  if(cache_format %in% c("rds")) {
    res <- readRDS(filename)
  }
  if(cache_format %in% c("feather")) {
    if(!(length(find.package("arrow", quiet = TRUE)) > 0)) {
      stop("Must have 'arrow' package installed to use 'feather' cache")
    }
    res <- arrow::read_feather(filename)
  }
  if(cache_format %in% c("parquet")) {
    if(!(length(find.package("arrow", quiet = TRUE)) > 0)) {
      stop("Must have 'arrow' package installed to use 'feather' cache")
    }
    res <- arrow::read_parquet(filename)
  }
  res %>%
    standardize_cols()
}

cache_single_htid <- function(htid, local_cache, cache_type, cache_format, dir) {
  stopifnot(length(htid) == 1)
  stopifnot(length(cache_format) == length(cache_type))
  stopifnot(length(cache_format) == length(local_cache))

  parsed_json <- load_json(htid = htid, dir = dir)

  ef <- meta <- pagemeta <- NULL

  for(ct in unique(cache_type)) {
    if(ct == "ef") {
      message("Now caching EF file for ", htid)
      ef <- parsed_json %>%
        parse_listified_book()

      for(i in which(cache_type == ct)) {
        cache_save(ef, local_cache[i], cache_format[i])
      }
    }
    if(ct == "meta") {
      message("Now caching volume-level metadata for ", htid)
      meta <- parsed_json %>%
        parse_meta_volume()

      for(i in which(cache_type == ct)) {
        cache_save(meta, local_cache[i], cache_format[i])
      }
    }
    if(ct == "pagemeta") {
      message("Now caching page-level metadata for ", htid)
      pagemeta <- parsed_json %>%
        parse_page_meta_volume()

      for(i in which(cache_type == ct)) {
        cache_save(pagemeta, local_cache[i], cache_format[i])
      }
    }
  }

  assemble_df(purrr::compact(list(ef, meta, pagemeta)))
}

standardize_cols <- function(obj) {
  obj %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("page", "count",
                                                "pubDate", "dateCreated",
                                                "lastRightsUpdateDate",
                                                "tokenCount", "lineCount",
                                                "emptyLineCount", "sentenceCount",
                                                "sectionTokenCount",
                                                "sectionLineCount",
                                                "sectionEmptyLineCount",
                                                "sectionSentenceCount",
                                                "sectionCapAlphaSeq")),
                                as.integer)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("lccn", "oclc")),
                                as.double))
}

cache_save <- function(obj, local_cache, cache_format) {
  obj <- standardize_cols(obj)

  if(stringr::str_detect(cache_format, "csv.gz")) {
    vroom::vroom_write(obj, local_cache, delim = ",")
  }

  if(stringr::str_detect(cache_format, "text2vec.csv")) {
    if(all(c("htid", "page", "token", "POS", "section", "count") %in% names(obj))) {
      htid <- page <- token <- POS  <- section <- count <- NULL
      obj <- obj %>%
        dplyr::group_by(htid, page, section) %>%
        dplyr::summarise(tokens = paste(rep(paste(token, POS, sep = "_"),
                                            times = count), collapse = " "))
    }
    vroom::vroom_write(obj, local_cache, delim = ",")
  }

  if(stringr::str_detect(cache_format, "rds")) {
    saveRDS(obj, local_cache, compress = TRUE)
  }

  if(stringr::str_detect(cache_format, "feather")) {
    if(!(length(find.package("arrow", quiet = TRUE)) > 0)) {
      stop("Must have 'arrow' package installed to use 'feather' cache")
    }
    arrow::write_feather(obj, local_cache)
  }

  if(stringr::str_detect(cache_format, "parquet")) {
    if(!(length(find.package("arrow", quiet = TRUE)) > 0)) {
      stop("Must have 'arrow' package installed to use 'parquet' cache")
    }
    arrow::write_parquet(obj, local_cache)
  }
  return(obj)
}

needs_cache <- function(cached_file_locs, json_file_locs, cache_format) {

  exists.y <- exists.x <- NULL

  total_file_locs <- nrow(cached_file_locs)

  to_cache <- cached_file_locs %>%
    dplyr::left_join(json_file_locs, by = "htid") %>%
    dplyr::filter(!exists.x, exists.y)

  if(nrow(to_cache) < total_file_locs) {
    non_existent_json <- cached_file_locs %>%
      dplyr::left_join(json_file_locs, by = "htid") %>%
      dplyr::filter(!exists.y) %>%
      nrow()

    cached_already_formats<- cached_file_locs %>%
      dplyr::left_join(json_file_locs, by = "htid") %>%
      dplyr::filter(exists.x)

    cached_already <- cached_already_formats %>%
      nrow()

    cached_already_formats <- unique(cached_already_formats$cache_format.x) %>%
      paste(collapse = ", ")

    if(non_existent_json > 0) {
      message(non_existent_json, " HTIDs cannot be cached, since their JSON EF",
              " files have not been downloaded or do not exist in the Hathi Trust rsync server.")
      message("Try using rsync_from_hathi(htids) to download them.")
    }
    if(cached_already > 0) {
      message(cached_already, " HTIDs have already been cached to ", cached_already_formats, " format.")
    }
  }

  to_cache

}
