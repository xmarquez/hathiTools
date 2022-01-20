#' @importFrom utils URLencode
build_solr_query <- function(q = "((volumegenre_txt:fiction)) AND ((en_htrctokentext:democracy))",
                             fl= "volumeid_s,id",
                             fq= "volumegenre_htrcstrings:(\"Love stories.\")",
                             volumes_only = FALSE) {

  stream_handler <- "https://solr2.htrc.illinois.edu/robust-solr/solr3456-faceted-htrc-full-ef16/stream"
  solr_endpoint <- "solr3456-faceted-htrc-full-ef16"
  qt <- "/export"
  sort <- "volumeid_s asc"
  indent <- "off"
  wt <- "json"

  facet.field = c("volumegenre_htrcstrings",
                  "volumelanguage_htrcstring",
                  "volumerightsAttributes_htrcstring",
                  "volumenames_htrcstrings",
                  "volumepubPlace_htrcstring",
                  "volumebibliographicFormat_htrcstring",
                  "volumeclassification_lcc_htrcstrings",
                  "volumeconcept_htrcstrings")


  query_string <- paste0("search(",
                         solr_endpoint,
                         ",",
                         "qt=", "\"", qt, "\"",
                         ",",
                         "q=", "\"", q, "\"",
                         ",",
                         "sort=", "\"", sort, "\"",
                         ",",
                         "fl=", "\"", fl, "\"",
                         ",",
                         "indent=", "\"", indent, "\"",
                         ",",
                         "wt=", "\"", wt, "\"")

  if(!is.null(fq) && fq != "") {
    query_string <- paste0(query_string,
                           ",",
                           paste(stringr::str_c("facet.field=",
                                                facet.field, sep = ""),
                                 collapse = ","),
                           ",",
                           "fq=", fq)

  }

  query_string <- paste0(query_string, ")")

  if(volumes_only) {
    query_string <- paste0("expr=rollup(",
                           query_string,
                           ',over="volumeid_s",count(*))')
  } else {
    query_string <- paste0("expr=", query_string)
  }

  stream_query <- paste0(stream_handler,"?",URLencode(query_string))
  stream_query

}

read_solr_stream <- function(q = "((en_htrctokentext:liberal)) AND ((en_htrctokentext:democracy))",
                             fq= NULL,
                             volumes_only = FALSE) {

  `result-set` <- EOF <- RESPONSE_TIME <- `count(*)` <- volumeid_s <- NULL

  query_string <- build_solr_query(q = q,
                                   fl = "volumeid_s,id",
                                   fq = fq,
                                   volumes_only)

  tmp <- tempfile(fileext = "json")

  curl::curl_download(query_string, tmp)

  res <- vroom::vroom_lines(tmp) %>%
    jsonlite::fromJSON() %>%
    dplyr::as_tibble() %>%
    tidyr::unnest(`result-set`) %>%
    dplyr::select(-EOF, -RESPONSE_TIME)

  if(volumes_only) {
    res <- res %>%
      dplyr::rename(n = `count(*)`,
                    htid = volumeid_s)
  } else {
    res <- res %>%
      dplyr::rename(htid = volumeid_s)

  }

  unlink(tmp)

  res



}

#' Builds a Workset of Hathi Trust vol IDs by querying the Workset Builder 2.0
#'
#' Queries the SOLR endpoint of the Workset Builder 2.0 (beta) at
#' https://solr2.htrc.illinois.edu/solr-ef/. This API is experimental, and so
#' this function can fail at any time if the API changes.
#'
#' @param token The tokens to search for in the HathiTrust Extracted Features
#'   files. Can be a vector of characters, e.g., `c("liberal", "democracy")`; if
#'   a vector, the results are interpreted using the value of `token_join` -- by
#'   default AND, so that the query will find all volumes where the tokens
#'   appear in the same page, but not necessarily next to each other (in that
#'   case, all pages containing both "liberal" and "democracy"). If `token_join`
#'   is "OR" then the query will find all volumes where either of the tokens
#'   appear. Search is case-insensitive; phrases can be included (e.g., "liberal
#'   democracy"), and the database will then return matches that are separated
#'   by hyphens (i.e., pages that contain the token "liberal-democracy").
#' @param genre A genre string, e.g. "Fiction" or "Not Fiction" in the Hathi
#'   Trust full metadata. Can be a set of genre strings, e.g.,
#'   `c("dictionary","biography")`, which are interpreted using the value of
#'   `genre_join` (by default "AND", so the metadata must contain both genre
#'   strings; if "OR", the query will look for volumes that contain any of the
#'   genre strings).
#' @param lang Language. Default is "en" (english). Right now this function can
#'   only search one language at a time; if you wish to search for terms in more
#'   than one language, create multiple worksets and bind them together.
#' @param title Title field. Multiple words will be joined with "AND"; can be a
#'   phrase (e.g., "Democracy in America").
#' @param name Names associated with the book (e.g., author). Multiple terms
#'   will be joined with "AND"; can be a phrase (e.g., "Alexis de Tocqueville").
#' @param imprint Imprint information (e.g., published). Multiple terms will be
#'   joined with "AND"; can be a phrase (e.g., "University of Chicago Press").
#' @param pub_date Publication date in Hathi Trust metadata. Can be a range,
#'   e.g., `1800:1900`, or a set of years, e.g., `c(1800, 1805)`.
#' @param volumes_only If `TRUE` (the default), returns only volume IDs plus a
#'   count of the number of times the tokens appear in the volume; `FALSE`
#'   returns both volume and page IDs where the tokens are found.
#' @param token_join The logical connector for different tokens. Default is
#'   "AND"; the query will ask for all pages where all tokens occur. "OR" means
#'   the query will ask for all pages where any of the tokens occur.
#' @param genre_join The logical connector for different genre strings. Default
#'   is "AND"; the query will ask for all volumes containing all the selected
#'   genre strings. "OR" means the query will ask for all volumes where any of
#'   the genre strings occur.
#'
#' @return A [tibble] with volume_ids, number of occurrences of the terms in the
#'   volume, and if `volumes_only` is `FALSE` a column for page ids.
#' @export
#'
#' @examples
#' \dontrun{
#' res <- workset_builder("democracy", genre = c("biography", "dictionary"))
#' # Better to run two separate queries so the genres can be distinguished per
#' # volume, but one can in principle use "OR" to find volumes that contain
#' # "democracy" in either genre.
#' res <- workset_builder("democracy", genre = c("biography", "dictionary"),
#'        genre_join = "OR")
#'
#' # All volumes that mention "tylenol" and "paracetamol" in the same page
#' res <- workset_builder(c("tylenol", "paracetamol"), volumes_only = FALSE)
#'
#' # All volumes mentioning "demagogue" published between 1800 and 1900
#' res <- workset_builder("demagogue", pub_date = 1800:1900)
#'
#' # All volumes mentioning "demagogue" with "Tocqueville" and "Reeve"
#' # in the author field
#' res <- workset_builder("demagogue", name = c("Tocqueville", "Reeve"))
#'
#' # All volumes with "Tocqueville" in the author field
#' res <- workset_builder(name = "Tocqueville")
#' }
workset_builder <- function(token, genre, title,
                            name, imprint, pub_date,
                            lang = "en",
                            volumes_only = TRUE,
                            token_join = c("AND", "OR"),
                            genre_join = c("AND", "OR")) {

  volumeid_s <- htid <- NULL

  stopifnot(length(lang) == 1)

  token_join <- match.arg(token_join, c("AND", "OR"))

  token_join <- paste0(" ", token_join, " ")

  genre_join <- match.arg(genre_join, c("AND", "OR"))

  genre_join <- paste0(" ", genre_join, " ")

  if(!missing(token)) {
    token_query <- paste0(lang, "_htrctokentext:", token)
    token_query <- stringr::str_c("(", token_query, ")", sep = "") %>%
      paste(collapse = token_join)
  } else {
    token_query <- ""
  }


  if(!missing(genre)) {
    genre <- stringr::str_c("(volumegenre_htrcstrings:(\"", tolower(genre), "\"))")
    genre <- paste(genre, collapse = genre_join)
    if(token_query != "") {
      token_query <- paste0("(",
                            genre,
                            " AND ",
                            token_query,
                            ")")
    } else {
      token_query <- genre
    }
  }

  if(!missing(title)) {
    title <- stringr::str_split(title, " ") %>%
      unlist() %>%
      unique()
    title <- stringr::str_c("(volumetitle_txt:", title, ")")
    title <- paste(title, collapse = " AND ")
    if(token_query != "") {
      token_query <- paste0("(",
                            title,
                            " AND ",
                            token_query,
                            ")")
    } else {
      token_query <- title
    }
  }

  if(!missing(name)) {
    name <- stringr::str_split(name, " ") %>%
      unlist() %>%
      unique()
    name <- stringr::str_c("(volumenames_txt:", name, ")")
    name <- paste(name, collapse = " AND ")
    if(token_query != "") {
      token_query <- paste0("(",
                            name,
                            " AND ",
                            token_query,
                            ")")
    } else {
      token_query <- name
    }
  }

  if(!missing(imprint)) {
    imprint <- stringr::str_split(imprint, " ") %>%
      unlist() %>%
      unique()
    imprint <- stringr::str_c("(volumeimprint_txt:", imprint, ")")
    imprint <- paste(imprint, collapse = " AND ")
    if(token_query != "") {
      token_query <- paste0("(",
                            imprint,
                            " AND ",
                            token_query,
                            ")")
    } else {
      token_query <- imprint
    }
  }

  if(!missing(pub_date)) {
    stopifnot(is.numeric(pub_date))
    pub_date <- stringr::str_c("(volumepubDate_txt:", pub_date, ")")
    pub_date <- paste(pub_date, collapse = " OR ")
    if(token_query != "") {
      token_query <- paste0("(",
                            pub_date,
                            " AND ",
                            token_query,
                            ")")
    } else {
      token_query <- pub_date
    }
  }

  if(!missing(token) && length(token) > 1) {
    token_query <- paste0("(", token_query, ")")
  }

  res <- read_solr_stream(q = token_query,
                          volumes_only = volumes_only)

  attr(res, "query") <- token_query
  class(res) <- c("hathi_workset", class(res))

  if(length(names(res)) == 0) {
    message("No results!")
    return(NULL)
  } else {
    res %>%
      dplyr::filter(!is.na(htid))

  }

}

#' Get metadata for a set of Hathi Trust IDs
#'
#' Queries the SOLR endpoint of the Workset Builder 2.0 (beta) at
#' https://solr2.htrc.illinois.edu/solr-ef/ to download volume metadata. This
#' API is experimental, and so this function can fail at any time if the API
#' changes.
#'
#' Also, be mindful that dowloading a large number of metadata records can take
#' quite some time. Even a few hundred records can take a few minutes.
#'
#' @param workset A workset of htids, generated by [workset_builder] from [Hathi
#'   Trust's SOLR endpoint](https://solr2.htrc.illinois.edu/solr-ef/). One can
#'   also pass a data frame with a column labeled "htid" and containing valid
#'   Hathi Trust htids, or a character vector of htids (though the function will
#'   complain with a warning).
#' @param metadata_dir The directory used to cache the metadata file. Defaults
#'   to `getOption("hathiTools.metadata.dir")`, which is just "./metadata" on
#'   loading the package.
#' @param cache Whether to cache the resulting metadata as a CSV. Default is
#'   TRUE. The name of the resulting metadata file is generated by appending an
#'   MD5 hash (via [digest::digest]) to the string "metadata-", so each metadata
#'   download will have a different name.
#'
#' @return A [tibble::tibble] with the Hathi Trust metadata for all the volumes
#'   in the workset or the vector of htids.
#' @export
#'
#' @examples
#' \dontrun{
#' workset <- workset_builder(name = "Tocqueville")
#' meta <- get_workset_meta(workset)
#'
#' ## We can also pass a vector of htids:
#' meta <- get_workset_meta(workset$htid)
#' }
get_workset_meta <- function(workset,
                             metadata_dir = getOption("hathiTools.metadata.dir"),
                             cache = TRUE) {

  if(!missing(workset) && !"hathi_workset" %in% class(workset)) {
    warning("This function works best with worksets generated by ",
            "workset_builder. I cannot determine if this file was generated ",
            "by workset_builder; results may not be accurate or fail.")
  }



  if(is.character(workset)) {
    num_vols <- length(workset)
    htids <- stringr::str_c(workset, "-metadata") %>%
      paste0(collapse = ",")
  } else {
    num_vols <- nrow(workset)
    htids <- stringr::str_c(workset$htid, "-metadata") %>%
      paste0(collapse = ",")
  }

  htids <- paste0("value=", htids)

  if(cache) {
    if(is.null(metadata_dir)) {
      stop("Metadata directory not set")
    } else {
      fs::dir_create(metadata_dir)
      filename <- file.path(metadata_dir,
                            paste0("metadata-",
                                   digest::digest(htids),
                                   ".csv"))
      if(fs::file_exists(filename)) {
        message("Metadata has already been downloaded.",
                " Returning cached metadata.")
        return(vroom::vroom(filename))
      }
    }
  }

  message("Getting download key...")

  res <- httr::POST("https://solr2.htrc.illinois.edu/htrc-ef-access/get?action=url-shortener",
                    body = htids,
                    encode = "form",
                    httr::content_type("application/x-www-form-urlencoded"),
                    httr::verbose(data_out = FALSE))

  key <- res$content %>% rawToChar()



  download_url <- stringr::str_glue("https://solr2.htrc.illinois.edu/htrc-ef-access/get?action=download-ids&key={key}&output=csv")

  message("Downloading metadata for ", num_vols, " volumes. ",
          "This might take some time.")

  if(cache) {
    curl::curl_download(download_url, filename, quiet = FALSE)
    meta <- vroom::vroom(filename, delim = ",")
  } else {
    tmp <- tempfile(fileext = ".csv")
    curl::curl_download(download_url, tmp, quiet = FALSE)
    meta <- vroom::vroom(tmp, delim = ",")
  }

  meta

}
