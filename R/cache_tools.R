#' Caches downloaded JSON Extracted Features files to another format
#'
#' It is useful to run this function after running [rsync_from_hathi]; this way,
#' you can cache all your slow-to-load JSON Extracted Features files to a faster
#' to load format (e.g., `feather` or `csv`). By default, this function also
#' caches the associated metadata as separate .rds files.
#'
#' @param htids A character vector of Hathi Trust ids, a workset created with
#'   [workset_builder], or a data frame with a column named "htid" containing
#'   the Hathi Trust ids that require caching.
#' @inheritParams get_hathi_counts
#' @param cache_metadata Whether to cache the volume-level metadata associated
#'   with the specific Hathi Trust ID and contained in the downloaded JSON file.
#'   Default is `TRUE`. This metadata is cached as a tibble in .rds format. See
#'   [get_hathi_meta] for details of this metadata.
#' @param cache_page_metadata Whether to cache the page-level metadata
#'   associated with the specific Hathi Trust ID and contained in the downloaded
#'   JSON file. Default is `TRUE`. This metadata is cached as a tibble in .rds
#'   format. See [get_hathi_page_meta] for details of this metadata.
#' @param keep_json Whether to keep the downloaded json files. Default is
#'   `TRUE`; if false, it only keeps the local cached files (e.g., the csv
#'   files) and deletes the associated JSON files. This can save space.
#'
#' @return A [tibble] with the paths of the cached files and an indicator of
#'   whether each htid was successfully cached.
#'
#' @export
cache_htids <- function(htids,
                        dir = getOption("hathiTools.ef.dir"),
                        cache_type = getOption("hathiTools.cachetype"),
                        cache_metadata = TRUE,
                        cache_page_metadata = TRUE,
                        keep_json = TRUE) {

  exists.y <- exists.x <- page <- count <- htid <- NULL

  cache_type <- match.arg(cache_type, c("csv.gz", "none", "rds",
                                        "feather", "text2vec.csv"))

  htids <- check_htids(htids)

  json_file_locs <-  find_cached_htids(htids,
                                       dir = dir,
                                       cache_type = "none")

  if(cache_type == "none") {
    message("No files cached. Returning JSON EF file locations.")
    return(json_file_locs)
  }



  to_cache <- find_cached_htids(htids,
                                dir = dir,
                                cache_type = cache_type) %>%
    needs_cache(json_file_locs, cache_type)

  to_cache %>%
    dplyr::pull(htid) %>%
    purrr::iwalk(~{
      if((.y - 1) %% 5 == 0) {
        message("Now caching file ", .y, " of ", nrow(to_cache))
      }
      get_hathi_counts(.x, dir = dir, cache_type = cache_type)
    })

  to_cache <- find_cached_htids(htids,
                                dir = dir,
                                cache_type = "meta.rds") %>%
    needs_cache(json_file_locs, "meta.rds")

  to_cache %>%
    dplyr::pull(htid) %>%
    purrr::iwalk(~{
      if((.y - 1) %% 5 == 0) {
        message("Now caching metadata for file ", .y, " of ", nrow(to_cache))
      }
      get_hathi_meta(.x, dir = dir)
    })

  to_cache <- find_cached_htids(htids,
                                dir = dir,
                                cache_type = "pagemeta.rds") %>%
    needs_cache(json_file_locs, "pagemeta.rds")

  to_cache %>%
    dplyr::filter(htid %in% json_file_locs$htid[json_file_locs$exists]) %>%
    dplyr::pull(htid) %>%
    purrr::iwalk(~{
      if((.y - 1) %% 5 == 0) {
        message("Now caching page metadata for file ", .y, " of ", nrow(to_cache))
      }
      get_hathi_page_meta(.x, dir = dir)
    })

  if(!keep_json) {
    message("Now deleting associated JSON files!")
    fs::file_delete(json_file_locs$local_loc)
  }

  res <- find_cached_htids(htids, dir = dir, cache_type = cache_type) %>%
    dplyr::bind_rows(find_cached_htids(htids, dir = dir, cache_type = "meta.rds")) %>%
    dplyr::bind_rows(find_cached_htids(htids, dir = dir, cache_type = "pagemeta.rds")) %>%
    dplyr::mutate(cache_type = dplyr::case_when(stringr::str_detect(local_loc, "pagemeta.rds") ~
                                                  "Page metadata",
                                                stringr::str_detect(local_loc, "meta.rds") ~
                                                  "Metadata",
                                                TRUE ~ "Extracted features"))

  res

}


#' Finds cached Extracted Features files for a set of HT ids
#'
#' @param htids A character vector of Hathi Trust ids, a workset created with
#'   [workset_builder], or a data frame with a column named "htid" containing
#'   the Hathi Trust ids that require caching.
#' @inheritParams get_hathi_counts
#'
#' @return A [tibble] with the paths of the cached files and an indicator of
#'   whether each htid has an existing cached file.
#'
#' @export
find_cached_htids <- function(htids,
                              dir = getOption("hathiTools.ef.dir"),
                              cache_type = getOption("hathiTools.cachetype")) {

  cache_type <- match.arg(cache_type, c("csv.gz", "none", "rds",
                                        "feather", "text2vec.csv",
                                        "meta.rds", "pagemeta.rds"))

  htids <- check_htids(htids)

  if(cache_type == "none") {
    cache_type <- "json.bz2"
  }

  local_files <- htids %>%
    purrr::map_chr(local_loc, suffix = cache_type, dir = dir)

  tibble(htid = htids, local_loc = local_files, exists = fs::file_exists(local_files))

}

#' Removes cached Extracted Features files for a set of HT ids
#'
#' @inheritParams find_cached_htids
#' @param cache_type The type of cached file to remove. Defaults to
#'   `getOption("hathiTools.cachetype")`, which is "csv.gz" on load, as well as
#'   any cached volume-level or page-level metadata ("meta" and "pagemeta").
#'   Other possibilities are: "rds", "feather" (suitable for use with [arrow];
#'   needs the [arrow] package installed), or "text2vec.csv" (a csv suitable for
#'   use with the package
#'   [text2vec](https://cran.r-project.org/package=text2vec))
#' @param keep_json Whether to keep any downloaded JSON files. Default is
#'   `TRUE`; if `FALSE` will delete all JSON extracted features associated with
#'   the set of htids.
#'
#' @return A [tibble] with the paths of the cached files and an indicator of
#'   whether each htid has an existing cached file.
#'
#' @export
clear_cache <- function(htids,
                        dir = getOption("hathiTools.ef.dir"),
                        cache_type = c(getOption("hathiTools.cachetype"), "meta", "pagemeta"),
                        keep_json = TRUE) {

  htids <- check_htids(htids)

  cache_type <- match.arg(cache_type, c("csv.gz", "rds",
                                        "feather", "text2vec.csv",
                                        "meta.rds", "pagemeta.rds"),
                          several.ok = TRUE)

  if(!keep_json) {
    cache_type <- c(cache_type, "none")
  }

  caches <- cache_type %>%
    purrr::map_df(~find_cached_htids(htids = htids, dir = dir,
                              cache_type = .x)) %>%
    dplyr::filter(exists)

  message("Now deleting ", nrow(caches), " files!")

  fs::file_delete(caches$local_loc)

}

#' Read Cached HTIDs
#'
#' @inheritParams find_cached_htids
#' @param include_meta Whether to include the volume-level metadata in the
#'   returned [tibble][tibble::tibble]. Default is `TRUE`.
#' @param include_page_meta Whether to include the page-level metadata in the
#'   returned [tibble][tibble::tibble]. Default is `TRUE`.
#' @param filter_expression A filter expression to filter each returned
#'   [tibble][tibble::tibble] by (so as to only load into memory desired
#'   features).
#'
#' @return A [tibble][tibble::tibble] with the extracted features, plus the
#'   desired (volume-level or page-level) metadata.
#'
#' @export
#'
read_cached_htids <- function(htids,
                              dir = getOption("hathiTools.ef.dir"),
                              cache_type = getOption("hathiTools.cachetype"),
                              include_meta = TRUE,
                              include_page_meta = TRUE,
                              filter_expression = TRUE) {
  htids <- check_htids(htids)

  cache_type <- match.arg(cache_type, c("csv.gz", "rds", "feather",
                                        "text2vec.csv"))

  ef_locs <- find_cached_htids(htids = htids, dir = dir,
                               cache_type = cache_type)

  meta_locs <- find_cached_htids(htids = htids, dir = dir,
                                 cache_type = "meta")

  pagemeta_locs <- find_cached_htids(htids = htids, dir = dir,
                                     cache_type = "pagemeta")

  cached <- ef_locs %>%
    dplyr::filter(exists) %>%
    dplyr::left_join(meta_locs %>%
                       dplyr::filter(exists) %>%
                       dplyr::rename(meta_loc = local_loc),
                     by = c("htid", "exists")) %>%
    dplyr::left_join(pagemeta_locs %>%
                       dplyr::filter(exists) %>%
                       dplyr::rename(pagemeta_loc = local_loc),
                     by = c("htid", "exists")) %>%
    dplyr::mutate(cache_type = cache_type,
                  include_meta = include_meta,
                  include_page_meta = include_page_meta) %>%
    dplyr::relocate(exists, .after = dplyr::everything())


  efs <- cached %>%
    dplyr::select(-exists) %>%
    purrr::pmap_df(assemble_from_cache,
                   filter_expression = dplyr::enquo(filter_expression))

  efs



}

assemble_from_cache <- function(htid, local_loc, meta_loc,
                                pagemeta_loc, cache_type, include_meta,
                                include_page_meta, filter_expression) {

  mainEntityOfPage <- page <- NULL

  ef <- read_cached_ef_file(local_loc, cache_type = cache_type)
  ef$htid <- htid

  if(include_meta) {
    meta <- readRDS(meta_loc) %>%
      dplyr::mutate(mainEntityOfPage = list(mainEntityOfPage))
    ef <- ef %>%
      dplyr::left_join(meta, by = "htid")
  }

  if(include_page_meta) {
    pagemeta <- readRDS(pagemeta_loc)
    ef <- ef %>%
      dplyr::left_join(pagemeta, by = c("section", "page", "htid")) %>%
      dplyr::relocate(htid, page, .before = dplyr::everything())

  }

  ef %>%
    dplyr::filter(!!filter_expression)

}

read_cached_ef_file <- function(filename, cache_type) {
  if(cache_type %in% c("csv.gz", "csv", "text2vec.csv")) {
    res <- vroom::vroom(filename, show_col_types = FALSE)
  }
  if(cache_type %in% c("rds")) {
    res <- readRDS(filename)
  }
  if(cache_type %in% c("feather")) {
    if(!(length(find.package("arrow", quiet = TRUE)) > 0)) {
      stop("Must have 'arrow' package installed to use 'feather' cache")
    }
    res <- arrow::read_feather(filename)
  }
  if(cache_type != "text2vec.csv") {
    res$count <- as.numeric(res$count)
  }
  res$page <- as.numeric(res$page)
  res
}

cache_ef_file <- function(ef, filename, cache_type) {

  section <- page <- token <- count <- POS <- NULL

  if(stringr::str_detect(cache_type, "text2vec")) {

    ef <- ef %>%
      dplyr::group_by(section, page) %>%
      dplyr::summarise(token = stringr::str_c(rep(token, count), "_", rep(POS, count), collapse = " "),
                       .groups = "drop")

    ef$page <- as.numeric(ef$page)


  }
  if(cache_type %in% c("csv.gz", "csv", "text2vec.csv")) {
    vroom::vroom_write(ef, filename, delim = ",")
  }
  if(cache_type == "rds") {
    saveRDS(ef, filename, compress = TRUE)
  }
  if(cache_type == "feather") {
    if(!(length(find.package("arrow", quiet = TRUE)) > 0)) {
      stop("Must have 'arrow' package installed to use 'feather' cache")
    }
    arrow::write_feather(ef, filename)
  }

  ef

}

needs_cache <- function(cached_file_locs, json_file_locs, cache_type) {

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

    cached_already <- cached_file_locs %>%
      dplyr::left_join(json_file_locs, by = "htid") %>%
      dplyr::filter(exists.x) %>%
      nrow()

    if(non_existent_json > 0) {
      message(non_existent_json, " HTIDs cannot be cached, since their JSON EF",
              " files have not been downloaded or do not exist in the Hathi Trust rsync server.")
      message("Try using rsync_from_hathi(htids) to download them.")
    }
    if(cached_already > 0) {
      message(cached_already, " HTIDs have already been cached to ", cache_type, " format.")
    }
  }

  to_cache

}
