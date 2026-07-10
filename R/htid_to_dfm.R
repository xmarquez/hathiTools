#' Create a Quanteda Document-Feature Matrix from a Set of HTIDs
#'
#' Reads the JSON Extracted Features files for a set of Hathi Trust IDs and
#' builds a single page-level [quanteda::dfm] whose documents are the individual
#' pages of the volumes and whose features are the (optionally lower-cased)
#' tokens selected via `pos_pattern`, `include_pattern`, and `min_length`.
#'
#' @param htids A character vector of Hathi Trust IDs, or a workset or data frame
#'   with an `htid` column, as returned by [workset_builder()].
#' @param attempt_rsync Whether to attempt to [rsync_from_hathi()] any requested
#'   IDs whose JSON Extracted Features files are not already available locally in
#'   `dir`. Defaults to `TRUE`. Requires an rsync client and network access.
#' @param dir The directory where the JSON Extracted Features files are cached.
#'   Defaults to `getOption("hathiTools.ef.dir")`.
#' @param pos_pattern A regular expression matched against the part-of-speech tag
#'   of each token; only tokens whose POS matches are retained. Defaults to
#'   `"NN|VB|JJ"` (nouns, verbs, adjectives).
#' @param include_pattern A regular expression that each token must match to be
#'   retained. Defaults to `"^\\p{L}+$"` (tokens made up solely of letters).
#' @param min_length The minimum token length (in characters) to retain.
#'   Defaults to `3`.
#' @param page_language Only pages whose `calculatedLanguage` equals this value
#'   are retained. Defaults to `"en"`.
#' @param min_sentence_count The minimum number of sentences a page must contain
#'   to be retained. Defaults to `3`.
#' @param to_lower Whether to lower-case the resulting features. Defaults to
#'   `TRUE`.
#'
#' @return A [quanteda::dfm] with one row per retained page and one column per
#'   retained feature. Page-level metadata (sequence number, token count, etc.)
#'   plus the `htid` is attached as document variables.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' htids <- workset_builder("democracy", pub_date = 1800:1810)
#' dfm <- htids_to_dfm(htids, dir = tempdir())
#' dfm
#' }
htids_to_dfm <- function(htids,
                         attempt_rsync = TRUE,
                         dir = getOption("hathiTools.ef.dir"),
                         pos_pattern = "NN|VB|JJ",
                         include_pattern = "^\\p{L}+$",
                         min_length = 3,
                         page_language = "en",
                         min_sentence_count = 3,
                         to_lower = TRUE) {

  exists <- local_loc <- NULL

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
                                         existing_only = TRUE)
  }

  paths <- json_file_locs %>%
    dplyr::filter(exists) |>
    dplyr::pull(local_loc)

  pb <- cli::cli_progress_bar("Building dfms from JSON EF files",
                              total = length(paths))

  dfms <- paths |>
    purrr::map(\(x) {
      cli::cli_progress_update(status = paste("Now reading", x), id = pb);
      json_to_dfm(x, pos_pattern = pos_pattern,
                  include_pattern = include_pattern,
                  min_length = min_length,
                  page_language = page_language,
                  min_sentence_count = min_sentence_count,
                  to_lower = to_lower)
    })
  cli::cli_progress_done()

  new_features <- dfms |>
    purrr::map(quanteda::featnames) |>
    purrr::reduce(union)

  new_docvars <- dfms |>
    purrr::map(quanteda::docvars) |>
    purrr::list_rbind()

  dfms <- dfms |>
    purrr::map(\(x) quanteda:::pad_dfm(x, new_features))

  result <- dfms |>
    purrr::map(\(x) as(x, "sparseMatrix")) |>
    purrr::reduce(Matrix::rbind2) |>
    quanteda::as.dfm()

  quanteda::docvars(result) <- new_docvars

  result
}

json_to_dfm <- function(path,
                        pos_pattern = "NN|VB|JJ",
                        include_pattern = "^\\p{L}+$",
                        min_length = 3,
                        page_language = "en",
                        min_sentence_count = 3,
                        to_lower = TRUE) {

  calculatedLanguage <- sentenceCount <- NULL

  parsed_json <- jsonlite::read_json(path)

  body <- parsed_json |>
    purrr::pluck("features", "pages")  |>
    purrr::map(\(x) purrr::pluck(x, "body", "tokenPosCount"))

  pagemeta <- parsed_json |>
    purrr::pluck("features", "pages") |>
    purrr::map(\(x) x[c("seq", "tokenCount",
                        "lineCount",
                        "emptyLineCount",
                        "sentenceCount",
                        "calculatedLanguage")]) |>
    purrr::map(\(x) purrr::discard(x, is.null) |>
                 tibble::as_tibble_row()) |>
    purrr::list_rbind()

  body <- body[ pagemeta$calculatedLanguage == page_language &
                  !is.na(pagemeta$calculatedLanguage) &
                  pagemeta$sentenceCount >= min_sentence_count ]

  if(length(body) == 0) {
    return(quanteda:::make_null_dfm())
  }

  pagemeta <- pagemeta |>
    dplyr::filter(calculatedLanguage == page_language,
                  sentenceCount >= min_sentence_count)

  body <- body |>
    purrr::map(\(x) x[stringr::str_detect(names(x), include_pattern)]) |>
    purrr::map(\(x) x[stringr::str_length(names(x)) >= min_length]) |>
    purrr::map(\(x) unlist(x, recursive = FALSE))

  pagemeta <- pagemeta[purrr::map_lgl(body, \(x) !is.null(x)), ]

  body <- body |>
    purrr::discard(is.null)

  flattened_body <- unlist(body)

  flattened_body <- flattened_body[ stringr::str_detect(names(flattened_body), pos_pattern) ]

  featnames <- sort(unique(names(flattened_body)))

  j <- body |>
    purrr::map(\(x) match(names(x), featnames)) |>
    purrr::map(na.omit)

  i <- rep(1:length(j), times = lengths(j))

  j <- unlist(j)

  x <- unlist(flattened_body)

  docnames <- paste(parsed_json$htid, as.numeric(pagemeta$seq), sep = "_")

  result <- Matrix::sparseMatrix(i = i, j = j, x = x,
                                 dimnames = list(docnames, featnames)) |>
    quanteda::as.dfm()

  colnames(result) <- stringr::str_replace_all(colnames(result), "(.+)\\.(.+)", "\\1_\\2")

  pagemeta$htid <- parsed_json$htid

  quanteda::docvars(result) <- pagemeta

  result <- result |>
    quanteda::dfm_subset(subset = quanteda::rowSums(result) > 0)

  if(to_lower) {
    result <- result |>
      quanteda::dfm_tolower()
  }

  result

}
