#' Builds a Workset of Hathi Trust vol IDs by querying the Workset Builder 2.0
#'
#' Queries the SOLR endpoint of the Workset Builder 2.0 (beta) at
#' [https://solr2.htrc.illinois.edu/solr-ef/](https://solr2.htrc.illinois.edu/solr-ef/).
#' This API is experimental, and so this function can fail at any time if the
#' API changes.
#'
#' @param token The tokens to search for in the Hathi Trust Extracted Features
#'   files. Can be a vector of characters, e.g., `c("liberal", "democracy")`; if
#'   a character vector with more than one element, the results are interpreted
#'   using the value of `token_join` -- by default AND, so that the query will
#'   find all volumes where *all* the tokens appear, though not necessarily in
#'   the same page (in the example, all volumes containing both "liberal" and
#'   "democracy"). If `token_join` is "OR" then the query will find all volumes
#'   where either of the tokens appear. Search is case-insensitive; phrases can
#'   be included (e.g., "liberal democracy"), and the database will then return
#'   matches where both terms appear in the *same page* (though not necessarily
#'   next to each other).
#' @param genre A genre string, e.g. "Fiction" or "Not Fiction" in the Hathi
#'   Trust full metadata. Can be a set of genre strings, e.g.,
#'   `c("dictionary","biography")`, which are interpreted using the value of
#'   `genre_join` (by default "AND", so the metadata must contain both genre
#'   strings; if "OR", the query will look for volumes that contain any of the
#'   genre strings).
#' @param lang Language. Default is "eng" (English); a string like "English" or
#'   any 2 or 3 letter ISO639 code (available in the dataset [iso639] included
#'   with this package) is allowed. (If no language code is found, the default
#'   is to search all languages; set to `NULL` if you want to search all
#'   languages). Right now this function can only search one language at a time;
#'   if you wish to search for terms in more than one language, create multiple
#'   worksets and bind them together.
#' @param title Title field. Multiple words will be joined with "AND"; can be a
#'   phrase (e.g., "Democracy in America").
#' @param name Names associated with the book (e.g., author). Multiple terms
#'   will be joined with "AND"; can be a phrase (e.g., "Alexis de Tocqueville").
#' @param imprint Imprint information (e.g., publisher). Multiple terms will be
#'   joined with "AND"; can be a phrase (e.g., "University of Chicago Press").
#' @param pub_date Publication date in Hathi Trust metadata. Can be a range,
#'   e.g., `1800:1900`, or a set of years, e.g., `c(1800, 1805)`.
#' @param volumes_only If `TRUE` (the default), returns only volume IDs plus a
#'   count of the number of times the tokens appear in the volume; `FALSE`
#'   returns both volume and page IDs where the tokens are found. Note the page
#'   IDs are 0-based; when looking for the page at the Hathi Digital Library
#'   site, it's necessary to add 1. [browse_htids] does this automatically.
#' @param token_join The logical connector for the tokens in `token`, if more
#'   than one. Default is "AND"; the query will ask for all volumes where all
#'   tokens occur. "OR" means the query will ask for all volumes where any of
#'   the tokens occur.
#' @param genre_join The logical connector for different genre strings. Default
#'   is "AND"; the query will ask for all volumes containing all the selected
#'   genre strings. "OR" means the query will ask for all volumes where any of
#'   the genre strings occur.
#' @param max_vols Maximum number of volumes to return. Default is `Inf`, all
#'   volumes; for queries expected to return large numbers of volumes, it's
#'   sometimes best to specify a small number just to test that the query is
#'   what one wants.
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
                            lang = "eng",
                            volumes_only = TRUE,
                            token_join = c("AND", "OR"),
                            genre_join = c("AND", "OR"),
                            max_vols = Inf) {

  volumeid_s <- htid <- NULL

  token_join <- match.arg(token_join, c("AND", "OR"))

  token_join <- paste0(" ", token_join, " ")

  genre_join <- match.arg(genre_join, c("AND", "OR"))

  genre_join <- paste0(" ", genre_join, " ")

  if(missing(lang) || is.null(lang)) {
    lang <- "alllangs"
  }
  stopifnot(length(lang) == 1)

  if(!missing(token)) {
    if(any(stringr::str_detect(token, "[:space:]"))) {
      token <- paste0("\"", token, "\"")
    }
    lang_short <- find_language(lang)

    token_query <- paste0(lang_short, "_htrctokentext:", token)
    token_query <- stringr::str_c("(", token_query, ")", sep = "") %>%
      paste(collapse = token_join)
  } else {
    token_query <- ""
  }

  if(token_query == "" && !missing(lang)) {
    stopifnot(length(lang) == 1)
    lang <- find_language(lang, "alpha3-b")
    if(length(lang) > 0 && lang != "alllangs") {
      token_query <- paste0("(volumelanguage_txt:", lang, ")")
    } else{
      token_query <- ""
    }
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
    pub_date <- paste0("(", pub_date, ")")
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
                          volumes_only = volumes_only,
                          rows = max_vols)

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
#' Be mindful that downloading a large number of metadata records can take quite
#' some time. In practice I have found that downloading full metadata from more
#' than about 1000 records is a dicey proposition; if you need metadata for many
#' thousands of records, you are probably better off using the big hathifile
#' (see [download_hathifile] and [load_raw_hathifile]).
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
#'   in the workset or the vector of htids. #'   This [tibble] can contain the
#'   following fields (taken from
#'   [https://wiki.htrc.illinois.edu/pages/viewpage.action?pageId=79069329](https://wiki.htrc.illinois.edu/pages/viewpage.action?pageId=79069329)):
#'
#'
#'
#'   \describe{
#'
#'   \item{htBibUrl}{The Hathi Trust Bibliographic URL for the volume, which
#'   will point to the volume in the HathiTrust Digital Library. E.g.
#'   "http://catalog.hathitrust.org/api/volumes/full/htid/aeu.ark:/13960/t00z8277t.json"}
#'
#'
#'
#'   \item{schemaVersion}{The schema version for the metadata block of the
#'   Extracted Features file. A URL linking to the schema.
#'   "https://schemas.hathitrust.org/Extracted
#'   Features_Schema_MetadataSubSchema_v_3.0"}
#'
#'   \item{volumeIdentifier}{The Hathi Trust ID}
#'
#'   \item{rightsAttributes}{The copyright status of the volume.}
#'
#'   \item{title}{The title of the volume.}
#'
#'   \item{genre}{Information about the volume's genre, as determined by the
#'   cataloger of the work. Values are derived from the Genre/Form Field in the
#'   MARC record.}
#'
#'   \item{pubDate}{The year in which that edition of the volume was first
#'   published.}
#'
#'   \item{pubPlace}{Named list column.  Information about where the volume was
#'   first published. Includes id, type, and name. `type` is taken from the
#'   Bibframe Instance's provisionActivity's place rdf:about node, which are
#'   derived from the country codes in the MARC 008 field.}
#'
#'   \item{typeOfResource}{Type of resource, e.g., "text".}
#'
#'   \item{bibliographicFormat}{Bibliographic format (e.g. "BK").}
#'
#'   \item{language}{The cataloger-determined language or languages of the
#'   volume. Taken from the Bibframe Work's language's identifiedBy's value
#'   node, which is derived from the Language Code field in the MARC record.
#'   This may differ from the automatically detected language of any given page
#'   in the page-level metadata returned by [get_hathi_page_meta]}
#'
#'   \item{dateCreated}{The date on which the metadata portion of the Extracted
#'   Features file is generated, in YYYYMMDD format}
#'
#'   \item{lastUpdateDate}{The most recent date the volume's copyright status
#'   was updated.}
#'
#'   \item{imprint}{Information about the publisher of the volume.}
#'
#'   \item{isbn}{The ISBN of the volume (when a book).}
#'
#'   \item{issn}{The ISSN of the volume (when a journal).}
#'
#'   \item{oclc}{The OCLC number for the volume. An OCLC number is an identifier
#'   assigned to items as they are cataloged in a library.}
#'
#'   \item{lccn}{The Library of Congress Control Number for the volume. An LCCN
#'   is a unique number that is assigned during cataloging.}
#'
#'   \item{classification}{Library classification.}
#'
#'   \item{handleUrl}{The Handle URL for the volume, which will point to the
#'   volume in the HathiTrust Digital Library. E.g.
#'   "http://hdl.handle.net/2027/mdp.39015062779023"}
#'
#'   \item{hathiTrustRecordNumber}{The Hathi Trust Bibliographic record ID
#'   number.}
#'
#'   \item{sourceInstitutionRecordNumber}{The source institution record ID
#'   number.}
#'
#'   \item{sourceInstitution}{The source institution.}
#'
#'   \item{accessProfile}{Type of access rights.}
#'
#'   \item{enumerationChronology}{Information regarding which volume, issue,
#'   and/or year the HathiTrust volume was published.}
#'
#'   \item{governmentDocument}{Whether the item is a government document.}
#'
#'   \item{names}{Contains information regarding the author(s), editor(s), or
#'   other agents involved in creating the volume.}
#'
#'   \item{issuance}{The cataloger-determined resource type of the volume (e.g.,
#'   monographic, etc.).}
#'
#'   \item{subjectGenre, subjectName, subjectTitleInfo, subjectTemporal,
#'   subjectGeographic, subjectOccupation, subjectCartographics}{Columns
#'   containing subject info, if present.}
#'
#'   }
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
    num_vols <- length(unique(workset))
    htids <- stringr::str_c(unique(workset), "-metadata") %>%
      paste0(collapse = ",")
  } else {
    num_vols <- length(unique(workset$htid))
    htids <- stringr::str_c(unique(workset$htid), "-metadata") %>%
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

  httr::with_config(
    config = httr::config(ssl_verifypeer = FALSE),
    res <- httr::POST("https://solr2.htrc.illinois.edu/htrc-ef-access/get?action=url-shortener",
                      body = htids,
                      encode = "form",
                      httr::content_type("application/x-www-form-urlencoded"),
                      httr::verbose(data_out = FALSE))
  )

  key <- res$content %>% rawToChar()

  download_url <- stringr::str_glue("https://solr2.htrc.illinois.edu/htrc-ef-access/get?action=download-ids&key={key}&output=csv")

  message("Downloading metadata for ", num_vols, " volumes. ",
          "This might take some time.")

  h <- curl::new_handle()
  curl::handle_setopt(h, ssl_verifypeer = FALSE)

  if(cache) {
    curl::curl_download(download_url, filename, quiet = FALSE, handle = h)
    meta <- vroom::vroom(filename, delim = ",")
  } else {
    tmp <- tempfile(fileext = ".csv")
    curl::curl_download(download_url, tmp, quiet = FALSE, handle = h)
    meta <- vroom::vroom(tmp, delim = ",")
  }


  meta

}

#' @importFrom utils URLencode
build_solr_query <- function(q = "((volumegenre_txt:fiction)) AND ((en_htrctokentext:democracy))",
                             fl= "volumeid_s,id",
                             fq,
                             volumes_only = FALSE,
                             rows = Inf) {

  stream_handler <- "https://solr2.htrc.illinois.edu/robust-solr/solr3456-faceted-htrc-full-ef16/stream"
  solr_endpoint <- "solr3456-faceted-htrc-full-ef16"
  if(is.infinite(rows)) {
    qt <- "/export"
  } else {
    qt <- "/select"
  }
  if(qt == "/select") {
    rows <- ceiling(rows/15)
    sort <- paste0("rows=", rows, ",",
                   "sort=\"volumeid_s asc\",start=0")
  } else {
    sort <- "sort=\"volumeid_s asc\""
  }
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
                         sort,
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
                             volumes_only = FALSE,
                             rows = Inf) {

  `result-set` <- EOF <- RESPONSE_TIME <- `count(*)` <- volumeid_s <- htid <- NULL

  query_string <- build_solr_query(q = q,
                                   fl = "volumeid_s,id",
                                   fq = fq,
                                   volumes_only,
                                   rows = rows)

  tmp <- tempfile(fileext = "json")


  h <- curl::new_handle()
  curl::handle_setopt(h, ssl_verifypeer = FALSE)

  curl::curl_download(query_string, tmp, handle = h)

  res <- vroom::vroom_lines(tmp) %>%
    jsonlite::fromJSON() %>%
    dplyr::as_tibble() %>%
    tidyr::unnest(`result-set`) %>%
    dplyr::select(-EOF, -RESPONSE_TIME)

  if(volumes_only) {
    res <- res %>%
      dplyr::rename(n = `count(*)`,
                    htid = volumeid_s)
    if(!is.infinite(rows)) {
      res <- res %>%
        dplyr::slice(1:rows)
    }
  } else {
    res <- res %>%
      dplyr::rename(htid = volumeid_s)
    if(!is.infinite(rows)) {
      res <- res %>%
        dplyr::filter(htid %in% unique(htid)[1:min(rows, nrow(res))])
    }

  }

  unlink(tmp)

  res



}


find_language <- function(lang, code_type = c("alpha2", "alpha3-b", "alpha3-t")) {

  alpha2 <- `alpha3-b` <- `alpha3-t` <- English <- French <- NULL

  code_type <- match.arg(code_type, c("alpha2", "alpha3-b", "alpha3-t"))
  if(is.null(lang) || lang == "alllangs") {
    return("alllangs")
  }
  if(nchar(lang) == 2) {
    res <- hathiTools::iso639 %>%
      dplyr::filter(stringr::str_detect(alpha2, stringr::regex(lang, ignore_case = TRUE)))
    return(res[[code_type]])
  } else if(nchar(lang) == 3) {
    res <- hathiTools::iso639 %>%
      dplyr::filter(stringr::str_detect(`alpha3-b`, stringr::regex(lang, ignore_case = TRUE)) |
                      stringr::str_detect(`alpha3-t`, stringr::regex(lang, ignore_case = TRUE)))
    return(res[[code_type]])
  } else {
    res <- hathiTools::iso639 %>%
      dplyr::filter(stringr::str_detect(`alpha3-b`, stringr::regex(lang, ignore_case = TRUE)) |
                      stringr::str_detect(`alpha3-t`, stringr::regex(lang, ignore_case = TRUE)) |
                      stringr::str_detect(`alpha2`, stringr::regex(lang, ignore_case = TRUE)) |
                      stringr::str_detect(English, stringr::regex(lang, ignore_case = TRUE)) |
                      stringr::str_detect(French, stringr::regex(lang, ignore_case = TRUE)))
  }
  if(nrow(res) == 0) {
    return("alllangs")
  }
  res[[code_type]][!is.na(res[["alpha2"]])]

}
