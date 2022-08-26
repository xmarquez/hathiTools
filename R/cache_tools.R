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
#' @param cache_type Type of information cached. The default is c("ef", "meta",
#'   "pagemeta"), which refers to the extracted features, the volume metadata,
#'   and the page metadata in `dir`. Omitting one of these caches or finds only
#'   the rest (e.g., cache_type = "ef" caches or finds only the EF files, not
#'   their associated metadata or page metadata).
#' @param cache_metadata_format File format of volume metadata cache. Default
#'   (and recommended) is "rds"; can also be "csv.gz" or "feather" (requires the
#'   [arrow] package). Can be multiple for [find_cached_htids] and
#'   [clear_cache].
#' @param cache_pagemetadata_format File format of page metadata cache. Default
#'   (and recommended) is "rds"; can also be "feather" (requires the [arrow]
#'   package). Can be multiple for [find_cached_htids] and [clear_cache].
#' @param keep_json Whether to keep the downloaded json files. Default is
#'   `TRUE`; if `FALSE`, it only keeps the local cached files (e.g., the csv
#'   files) and deletes the associated JSON files.
#'
#' @return A [tibble] with the paths of the cached files and an indicator of
#'   whether each htid was successfully cached.
#'
#' @export
cache_htids <- function(htids,
                        dir = getOption("hathiTools.ef.dir"),
                        cache_type = c("ef", "meta", "pagemeta"),
                        cache_format = getOption("hathiTools.cacheformat"),
                        cache_metadata_format =  "rds",
                        cache_pagemetadata_format = "rds",
                        keep_json = TRUE) {

  exists.y <- exists.x <- page <- count <- htid <- NULL

  cache_type <- match.arg(cache_type, c("ef", "meta", "pagemeta"),
                          several.ok = TRUE)

  cache_format <- match.arg(cache_format, c("csv.gz", "none", "rds",
                                        "feather", "text2vec.csv"))

  cache_metadata_format <- match.arg(cache_metadata_format,
                                     c("rds", "csv.gz", "feather"))

  cache_pagemetadata_format <- match.arg(cache_pagemetadata_format,
                                         c("rds", "feather"))

  htids <- check_htids(htids)

  json_file_locs <-  find_cached_htids(htids,
                                       dir = dir,
                                       cache_type = "ef",
                                       cache_format = "none",
                                       existing_only = FALSE)

  if(cache_format == "none") {
    message("No files cached. Returning JSON EF file locations.")
    return(json_file_locs)
  }

  if("ef" %in% cache_type) {
    to_cache <- find_cached_htids(htids,
                                  dir = dir,
                                  cache_type = "ef",
                                  cache_format = cache_format,
                                  existing_only = FALSE) %>%
      needs_cache(json_file_locs, cache_format)

    to_cache %>%
      dplyr::pull(htid) %>%
      purrr::iwalk(~{
        if((.y - 1) %% 5 == 0) {
          message("Now caching file ", .y, " of ", nrow(to_cache))
        }
        get_hathi_counts(.x, dir = dir, cache_format = cache_format)
      })

  }

  if("meta" %in% cache_type) {
    to_cache <- find_cached_htids(htids,
                                  dir = dir,
                                  cache_type = "meta",
                                  cache_format = cache_metadata_format,
                                  existing_only = FALSE) %>%
      needs_cache(json_file_locs, paste0("page.", cache_metadata_format))

    to_cache %>%
      dplyr::pull(htid) %>%
      purrr::iwalk(~{
        if((.y - 1) %% 5 == 0) {
          message("Now caching metadata for file ", .y, " of ", nrow(to_cache))
        }
        get_hathi_meta(.x, dir = dir, cache_format = cache_metadata_format)
      })
  }

  if("pagemeta" %in% cache_type) {
    to_cache <- find_cached_htids(htids,
                                  dir = dir,
                                  cache_type = "pagemeta",
                                  cache_format = cache_pagemetadata_format,
                                  existing_only = FALSE) %>%
      needs_cache(json_file_locs, paste0("pagemeta.", cache_pagemetadata_format))

    to_cache %>%
      dplyr::filter(htid %in% json_file_locs$htid[json_file_locs$exists]) %>%
      dplyr::pull(htid) %>%
      purrr::iwalk(~{
        if((.y - 1) %% 5 == 0) {
          message("Now caching page metadata for file ", .y, " of ", nrow(to_cache))
        }
        get_hathi_page_meta(.x, dir = dir, cache_format = cache_pagemetadata_format)
      })
  }

  if(!keep_json) {
    message("Now deleting associated JSON files!")
    fs::file_delete(json_file_locs$local_loc)
  }

  res <- find_cached_htids(htids, dir = dir, cache_type = cache_type,
                           cache_format = cache_format,
                           cache_metadata_format = cache_metadata_format,
                           cache_pagemetadata_format = cache_pagemetadata_format)

  res

}


#' Finds cached Extracted Features files for a set of HT ids
#'
#' @inheritParams cache_htids
#' @param existing_only Whether to return only file paths to files that actually
#'   exist. Default is `TRUE`.
#'
#' @return A [tibble] with the paths of the cached files and an indicator of
#'   whether each htid has an existing cached file.
#'
#' @export
find_cached_htids <- function(htids,
                              dir = getOption("hathiTools.ef.dir"),
                              cache_type = c("ef", "meta", "pagemeta"),
                              cache_format = getOption("hathiTools.cacheformat"),
                              cache_metadata_format =  c("rds", "csv.gz", "feather"),
                              cache_pagemetadata_format = c("rds", "feather"),
                              existing_only = TRUE) {

  cache_type <- match.arg(cache_type, c("ef", "meta", "pagemeta"), several.ok = TRUE)

  cache_format <- match.arg(cache_format, c("csv.gz", "none", "rds",
                                            "feather", "text2vec.csv"),
                            several.ok = TRUE)

  names(cache_format) <- rep("ef", length(cache_format))

  cache_metadata_format <- match.arg(cache_metadata_format,
                                     c("rds", "csv.gz", "feather"),
                                     several.ok = TRUE)

  names(cache_metadata_format) <- rep("meta", length(cache_metadata_format))

  cache_pagemetadata_format <- match.arg(cache_pagemetadata_format,
                                         c("rds", "feather"),
                                         several.ok = TRUE)

  names(cache_pagemetadata_format) <- rep("pagemeta", length(cache_pagemetadata_format))

  if("none" %in% cache_format) {
    cache_format[cache_format == "none"] <- "json.bz2"
  }

  cache_format <- c(cache_format,
                    cache_metadata_format,
                    cache_pagemetadata_format)

  cache_format <- cache_format[cache_type]

  htids <- check_htids(htids)

  suffixes <- paste0(names(cache_format), ".", cache_format) %>%
    stringr::str_remove("ef\\.")

  names(suffixes) <- names(cache_format)

  res <- tibble::tibble()

  for(suffix in suffixes) {
    local_files <- htids %>%
      purrr::map_chr(local_loc, suffix = suffix, dir = dir)

    res <- rbind(res,
                 tibble::tibble(htid = htids,
                                local_loc = local_files,
                                cache_format = suffix,
                                cache_type = names(suffix),
                                exists = fs::file_exists(local_files)))
  }

  if(existing_only) {
    res <- res %>%
      dplyr::filter(exists)
  }

  res


}

#' Removes cached files for a set of HT ids
#'
#' @inheritParams find_cached_htids
#' @param cache_type Type of information to remove. The default is c("ef",
#'   "meta", "pagemeta"), which refers to the extracted features, the volume
#'   metadata, and the page metadata in `dir`. Omitting one of these removes
#'   only the rest (e.g., cache_type = "ef" removes only the EF files, not their
#'   associated metadata or page metadata).
#' @param cache_format The format of the cached EF files to remove. Defaults to
#'   `getOption("hathiTools.cacheformat")`, which is "csv.gz" on load, as well
#'   as any cached volume-level or page-level metadata ("meta" and "pagemeta").
#'   Other possibilities are: "rds", "feather" (suitable for use with [arrow];
#'   needs the [arrow] package installed), or "text2vec.csv" (a csv suitable for
#'   use with the package
#'   [text2vec](https://cran.r-project.org/package=text2vec))
#' @param keep_json Whether to keep any downloaded JSON files. Default is
#'   `TRUE`; if `FALSE` will delete all JSON extracted features associated with
#'   the set of htids.
#'
#' @return (Invisible) a character vector with the locations of the deleted
#'   files.
#'
#' @export
clear_cache <- function(htids,
                        dir = getOption("hathiTools.ef.dir"),
                        cache_type = c("ef", "meta", "pagemeta"),
                        cache_format = getOption("hathiTools.cacheformat"),
                        cache_metadata_format =  c("rds", "csv.gz", "feather"),
                        cache_pagemetadata_format = c("rds", "feather"),
                        keep_json = TRUE) {

  htids <- check_htids(htids)

  if(!keep_json) {
    cache_type <- c("none", cache_type)
  }

  caches <- find_cached_htids(htids,
                              dir = dir,
                              cache_type = cache_type,
                              cache_format = cache_format,
                              cache_metadata_format = cache_metadata_format,
                              cache_pagemetadata_format = cache_pagemetadata_format) %>%
    dplyr::filter(exists)

  message("Now deleting ", nrow(caches), " files!")

  fs::file_delete(caches$local_loc)

}

#' Read Cached HTIDs
#'
#' @inheritParams find_cached_htids
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
                              cache_type = c("ef", "meta", "pagemeta"),
                              cache_format = getOption("hathiTools.cacheformat"),
                              cache_metadata_format =  "rds",
                              cache_pagemetadata_format = "rds",
                              filter_expression = TRUE) {
  htids <- check_htids(htids)

  stopifnot(length(cache_format) == 1)
  stopifnot(length(cache_metadata_format) == 1)
  stopifnot(length(cache_pagemetadata_format) == 1)

  cached <- find_cached_htids(htids,
                              dir = dir,
                              cache_type = cache_type,
                              cache_format = cache_format,
                              cache_metadata_format =  cache_metadata_format,
                              cache_pagemetadata_format = cache_pagemetadata_format) %>%
    tidyr::pivot_wider(id_cols = "htid", names_from = "cache_format", values_from = "local_loc")

  if(is.null(cached[[cache_format]])) {
    cached[[cache_format]] <- NA_character_
  }

  if(is.null(cached[[paste0("meta.", cache_metadata_format)]])) {
    cached[[paste0("meta.", cache_metadata_format)]] <- NA_character_
  }

  if(is.null(cached[[paste0("pagemeta.", cache_metadata_format)]])) {
    cached[[paste0("pagemeta.", cache_metadata_format)]] <- NA_character_
  }

  names(cached)[names(cached) == cache_format] <- "local_loc"
  names(cached)[names(cached) == paste0("meta.", cache_metadata_format)] <- "meta_loc"
  names(cached)[names(cached) == paste0("pagemeta.", cache_metadata_format)] <- "pagemeta_loc"

  efs <- cached %>%
    purrr::pmap_df(assemble_from_cache,
                   cache_format = cache_format,
                   cache_metadata_format = cache_metadata_format,
                   cache_pagemetadata_format = cache_pagemetadata_format,
                   filter_expression = dplyr::enquo(filter_expression))

  efs



}

assemble_from_cache <- function(htid, local_loc, meta_loc, pagemeta_loc,
                                cache_format,
                                cache_metadata_format,
                                cache_pagemetadata_format,
                                include_page_meta, filter_expression) {

  mainEntityOfPage <- page <- alternateTitle <- NULL

  if(!is.na(local_loc)) {
    ef <- read_cached_file(local_loc, cache_format = cache_format)
    ef$htid <- htid
  } else {
    ef <- tibble::tibble(htid = htid)
  }

  if(!is.na(meta_loc)) {
    meta <- read_cached_file(meta_loc, cache_metadata_format)
    if("alternateTitle" %in% names(meta) && is.character(meta[["alternateTitle"]])) {
      meta <- meta %>%
        dplyr::mutate(alternateTitle = list(alternateTitle))
    }
    if("mainEntityOfPage" %in% names(meta) && is.character(meta[["mainEntityOfPage"]])) {
      meta <- meta %>%
        dplyr::mutate(mainEntityOfPage = list(mainEntityOfPage))
    }
    ef <- ef %>%
      dplyr::left_join(meta, by = "htid")
  }

  if(!is.na(pagemeta_loc)) {
    pagemeta <- read_cached_file(pagemeta_loc, cache_metadata_format)
    ef <- ef %>%
      dplyr::left_join(pagemeta, by = c("htid", "section", "page"))
  }

  ef %>%
    dplyr::filter(!!filter_expression) %>%
    dplyr::relocate(htid, .before = dplyr::everything())

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
  res
}

cache_ef_file <- function(ef, htid, filename, cache_format) {

  section <- page <- token <- count <- POS <- NULL

  if(stringr::str_detect(cache_format, "text2vec")) {

    ef <- ef %>%
      dplyr::group_by(section, page) %>%
      dplyr::summarise(token = stringr::str_c(rep(token, count), "_", rep(POS, count), collapse = " "),
                       .groups = "drop")

    ef$page <- as.numeric(ef$page)


  }

  ef$htid <- htid

  if(cache_format %in% c("csv.gz", "csv", "text2vec.csv")) {
    vroom::vroom_write(ef, filename, delim = ",")
  }
  if(cache_format == "rds") {
    saveRDS(ef, filename, compress = TRUE)
  }
  if(cache_format == "feather") {
    if(!(length(find.package("arrow", quiet = TRUE)) > 0)) {
      stop("Must have 'arrow' package installed to use 'feather' cache")
    }
    arrow::write_feather(ef, filename)
  }

  ef

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
      message(cached_already, " HTIDs have already been cached to ", cache_format, " format.")
    }
  }

  to_cache

}
