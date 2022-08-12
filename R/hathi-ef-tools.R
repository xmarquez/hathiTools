#' Reads the downloaded extracted features file for a given Hathi Trust id
#'
#' Given a single Hathi Trust ID, this function returns a
#' [tibble][tibble::tibble] with its per-page word count and part of speech
#' information, and caches the results to the `getOption("hathiTools.ef.dir")`
#' directory (by default "./hathi-ef"). If the file has not been cached already,
#' it first attempts to download it directly from the Hathi Trust server. This
#' function uses code authored by Ben Schmidt, from his Hathidy package
#' ([https://github.com/HumanitiesDataAnalysis/hathidy](https://github.com/HumanitiesDataAnalysis/hathidy)).
#'
#' @param htid The Hathi Trust id of the item whose extracted features files are
#'   to be loaded into memory. If it hasn't been downloaded, the function will
#'   try to download it first.
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
#' @author Ben Schmidt
#'
#' @return a [tibble][tibble::tibble] with the extracted features.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download the 1863 version of "Democracy in America" by Tocqueville and get
#' # its extracted features
#'
#' tmp <- tempdir()
#'
#' get_hathi_counts("aeu.ark:/13960/t3qv43c3w", dir = tmp)
#'
#' }
get_hathi_counts <- function(htid,
                             dir = getOption("hathiTools.ef.dir"),
                             cache_type = getOption("hathiTools.cachetype")) {
  if(length(htid) != 1) {
    stop("This function only works with a single HT id. ",
         "Use `read_cached_htids()` to read extracted features for multiple HT ids.")
  }

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

#'Reads the volume-level metadata of a single downloaded Hathi Trust extracted
#'features file
#'
#'Given a single Hathi Trust ID, this function returns a
#'[tibble][tibble::tibble] with its volume-level metadata information. If the HT
#'EF file corresponding to this ID has not been downloaded already, it first
#'attempts to download it directly from the Hathi Trust server. This function
#'uses code authored by Ben Schmidt, from his Hathidy package
#'([https://github.com/HumanitiesDataAnalysis/hathidy](https://github.com/HumanitiesDataAnalysis/hathidy)).
#'
#'Note that if you want to extract the metadata of more than one Hathi Trust ID
#'at a time, it may be best to simply query the Workset Builder database using
#'[get_workset_meta], or to download the JSON files for these HTIDs first using
#'[rsync_from_hathi] and then running this function. It is also possible to get
#'simple metadata for large numbers of htids by downloading the big hathifile
#'using [download_hathifile] and then filtering it.
#'
#'@param htid The Hathi Trust id of the item whose metadata is to be read.
#'@param dir The directory where the JSON file for the extracted features is
#'  saved. Defaults to `getOption("hathiTools.ef.dir")`, which is just
#'  "./hathi-ef/" on load. If the file does not exist, this function will first
#'  attempt to download it.
#'
#'@return A [tibble][tibble::tibble] with the volume-level
#'  metadata for the corresponding Hathi Trust ID. This [tibble][tibble::tibble]
#'  can contain the following fields (taken from
#'  [https://wiki.htrc.illinois.edu/pages/viewpage.action?pageId=79069329](https://wiki.htrc.illinois.edu/pages/viewpage.action?pageId=79069329);
#'   if the field is `NULL`, it is not returned, so the metadata can contain
#'  fewer fields):
#'
#'  \describe{
#'
#'  \item{schemaVersion}{The schema version for the metadata block of the
#'  Extracted Features file. A URL linking to the schema.
#'  "https://schemas.hathitrust.org/Extracted
#'  Features_Schema_MetadataSubSchema_v_3.0"}
#'
#'  \item{id}{The Handle URL for the volume, which will point to the volume in
#'  the HathiTrust Digital Library. E.g.
#'  "http://hdl.handle.net/2027/mdp.39015062779023"}
#'
#'  \item{type}{Either "Book", "PublicationVolume", or "CreativeWork" depending
#'  on the value found in the Issuance field of the corresponding Bibframe
#'  record. When the Bibframe Issuance value is 'mono', the Book type is
#'  assigned. When the value is 'serl', the PublicationVolume type is assigned.
#'  In all other cases the CreativeWork type is assigned. The Bibframe Issuance
#'  is derived from a variety of fields in the MARC record, including Control
#'  Fields and Physical Description Fields.}
#'
#'  \item{dateCreated}{The date on which the metadata portion of the Extracted
#'  Features file is generated, in YYYYMMDD format}
#'
#'  \item{title}{The title of the volume when the type (above) is "Book" or
#'  "CreativeWork".}
#'
#'  \item{alternateTitle}{An alternate title for a bibliographic entity
#'  described by the Extracted Features file.}
#'
#'  \item{enumerationChronology}{Information regarding which volume, issue,
#'  and/or year the HathiTrust volume was published.}
#'
#'  \item{publisher}{Information about the publisher of the
#'  volume described by the Extracted Features file. Includes type, name, and
#'  id. `type` is either "Organization" or "Person". `id` is a URL identifying
#'  the publisher, such as a Handle URL, ORCID, etc. `name` is derived from the
#'  Imprint field in the MARC record.}
#'
#'  \item{pubPlace}{Information about where the volume was
#'  first published. Includes id, type, and name. `type` is taken from the
#'  Bibframe Instance's provisionActivity's place rdf:about node, which are
#'  derived from the country codes in the MARC 008 field.}
#'
#'  \item{pubDate}{The year in which that edition of the volume was first
#'  published.}
#'
#'  \item{genre}{Information about the volume's genre, as
#'  determined by the cataloger of the work. Values are derived from the
#'  Genre/Form Field in the MARC record.}
#'
#'  \item{category}{The volume's topic or topics.Derived from the
#'  Bibframe Work's ClassificationLcc node. Represents the natural language
#'  label for the Library of Congress Classification (LCC) value based upon the
#'  Library of Congress's LCC standard documentation.}
#'
#'  \item{language}{The cataloger-determined language or languages of the
#'  volume. Taken from the Bibframe Work's language's identifiedBy's value node,
#'  which is derived from the Language Code field in the MARC record. This may
#'  differ from the automatically detected language of any given page in the
#'  page-level metadata returned by [get_hathi_page_meta]}
#'
#'  \item{accessRights}{The copyright status of the volume. Corresponds to
#'  attributes in HathiTrust's accessRights database. Derived from a
#'  HathiTrust-local MARC field (974r) that is added to bibliographic records as
#'  they are processed at HathiTrust.}
#'
#'  \item{lastRightsUpdateDate}{The most recent date the volume's copyright
#'  status was updated.}
#'
#'  \item{Contributor}{Contains information regarding the
#'  author(s), editor(s), or other agents involved in creating the volume.
#'  Consists of id, type, and name. id is a URL taken from the Bibframe agent
#'  that links to an authorities database (e.g., VIAF). type is either the
#'  Person or Organization. Taken from the Bibframe agent type. name is The name
#'  of the person or organization who created the volume. Taken from the
#'  Bibframe agent's label. Derived from a variety of fields in the MARC record,
#'  including Main Entry Fields, Title and Title-Related Fields, and Added Entry
#'  Fields.}
#'
#'  \item{typeOfResource}{The cataloger-determined resource type of the volume
#'  (e.g., text, image, etc.).}
#'
#'  \item{sourceInstitution}{An array containing information about
#'  the institution that contributed the volume to HathiTrust. Always has a type
#'  node and a name node. `id` is a URL identifying the source institution.
#'  `name` is the name of the source institution.}
#'
#'  \item{mainEntityOfPage}{An array of URLs linking to various
#'  metadata records describing the volume represented by the Extracted Features
#'  file. The array typically contains 3 URLs that point to the HathiTrust
#'  Bibliographic API: HathiTrust brief bibliographic record, HathiTrust full
#'  bibliographic record, and the HathiTrust catalog record.}
#'
#'  \item{oclc}{The OCLC number for the volume. An OCLC number is an identifier
#'  assigned to items as they are cataloged in a library.}
#'
#'  \item{lcc}{The Library of Congress Classification number for the volume. An
#'  LCC number is a type of call number that would be used to locate an item on
#'  a library shelf.}
#'
#'  \item{lccn}{The Library of Congress Control Number for the volume. An LCCN
#'  is a unique number that is assigned during cataloging.}
#'
#'  \item{issn}{The ISSN of the volume (when a journal).}
#'
#'  \item{isbn}{The ISBN of the volume (when a book).}
#'
#'  }
#'
#'@author Ben Schmidt
#'@author Xavier Marquez
#'@export
#'
#' @examples
#' \dontrun{
#' # Download the 1862 version of "Democracy in America" by Tocqueville and get
#' # its metadata
#'
#' tmp <- tempdir()
#'
#' get_hathi_meta("mdp.39015001796443", dir = tmp)
#' }
get_hathi_meta <- function (htid, dir = getOption("hathiTools.ef.dir")) {
  if(length(htid) != 1) {
    stop("This function only works with a single HT id. ",
         "Use `read_cached_htids()` to read metadata for multiple HT ids.")
  }
  local_cache <- local_loc(htid, suffix = "meta.rds", dir = dir)

  if(!file.exists(local_cache)) {
    local_json <- local_loc(htid, suffix = "json.bz2", dir = dir)
    if(!file.exists(local_json)) {
      download_hathi_ef(htid, dir = dir)
    }
    meta <- load_json(htid, dir = dir)$metadata %>%
      purrr::compact() %>%
      purrr::map(flatten_data) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(htid = htid,
                    .before = dplyr::everything())
    saveRDS(meta, local_cache, compress = TRUE)
  }
  else {
    meta <- readRDS(local_cache)
  }

  meta
}

#' Reads the page-level metadata of a single Hathi Trust Extracted Features file
#'
#' Given a single Hathi Trust ID, this function returns a
#' [tibble][tibble::tibble] with its page-level metadata information. If the HT
#' EF file corresponding to this ID has not been downloaded already, it first
#' attempts to download it directly from the Hathi Trust server. This function
#' uses code authored by Ben Schmidt, from his Hathidy package
#' ([https://github.com/HumanitiesDataAnalysis/hathidy](https://github.com/HumanitiesDataAnalysis/hathidy)).
#'
#' Note that if you want to extract the page-level metadata of more than one
#' Hathi Trust ID at a time, it may be best to download the JSON files for these
#' HTIDs first using [rsync_from_hathi] and then running this function.
#'
#' @inheritParams get_hathi_meta
#'
#' @return A [tibble][tibble::tibble] with the page-level metadata for the
#'   corresponding Hathi Trust ID. The page-level metadata contains the
#'   following fields (taken from
#'   [https://wiki.htrc.illinois.edu/pages/viewpage.action?pageId=79069329](https://wiki.htrc.illinois.edu/pages/viewpage.action?pageId=79069329)):
#'
#'   \describe{
#'
#'   \item{seq}{The sequence number of the page in the volume. Corresponds to
#'   the digital object, so that the first scan in the volume is "00000001",
#'   which may be the cover, a title page, or something else.}
#'
#'   \item{Description}{A hash of the page content used to compute the features
#'   for the page. Volumes in HathiTrust may be updated to improve scan or OCR
#'   quality or correct an issue, which would cause the text data to change,
#'   and, if features are reprocessed, a new hash would result.}
#'
#'   \item{calculatedLanguage}{The most probable language of the text on the
#'   page. Determined algorithmically, and specified by language codes. Will be
#'   `NA` if no language detected, or if the language was not recognized by the
#'   algorithm.}
#'
#'   \item{tokenCount}{The total number of tokens detected on the page.}
#'
#'   \item{lineCount}{The total number of lines of text detected on the page.}
#'
#'   \item{emptyLineCount}{The total number of empty lines on the page.}
#'
#'   \item{sentenceCount}{The total number of sentences detected on the page.}
#'
#'   \item{sectionStats}{A [tibble][tibble::tibble] column with stats for each
#'   of the sections of the page ("header", "body", and "footer"). This tibble
#'   has the following fields:
#'
#'   * section: The seciton of the page.
#'
#'   * sectiontokenCount: The total number of tokens detected in the section of
#'   the page.
#'
#'   * sectionlineCount: The total number of lines detected in the section of
#'   the page.
#'
#'   * sectionemptyLineCount The total number of empty lines detected in the
#'   section of the page.
#'
#'   * sectionsentenceCount The total number of sentences detected in the
#'   section of the page.
#'
#'   * sectioncapAlphaSeq The longest length of the alphabetical sequence of
#'   capital characters starting a line. Only available for the "body" section.
#'
#'   * beginCharCount A [tibble][tibble::tibble] column with the first non-White
#'   Space characters detected on lines in the section (beginChar) and their
#'   occurrence counts (beginCharCount).
#'
#'   * endCharCount An [tibble][tibble::tibble] column of the last non-White
#'   Space characters on detected lines in the section (endChar) and their
#'   occurrence counts (endCharCount). }
#'
#'   }
#'
#' @author Ben Schmidt
#' @author Xavier Marquez
#' @export
#'
#' @examples
#' \dontrun{
#' # Download the 1862 version of "Democracy in America" by Tocqueville and get
#' # its page-level metadata
#'
#' tmp <- tempdir()
#'
#' page_meta <- get_hathi_page_meta("mdp.39015001796443", dir = tmp)
#'
#' page_meta %>% tidyr::unnest(sectionStats)
#' page_meta %>% tidyr::unnest(sectionStats) %>% tidyr::unnest(beginCharCount)
#'
#' }
get_hathi_page_meta <- function(htid, dir = getOption("hathiTools.ef.dir")) {
  if(length(htid) != 1) {
    stop("This function only works with a single HT id. ",
         "Use `read_cached_htids()` to read page metadata for multiple HT ids.")
  }
  local_cache <- local_loc(htid, suffix = "pagemeta.rds", dir = dir)

  if(!file.exists(local_cache)) {
    local_json <- local_loc(htid, suffix = "json.bz2", dir = dir)
    if(!file.exists(local_json)) {
      download_hathi_ef(htid, dir = dir)
    }
    page_meta <- load_json(htid, dir = dir) %>%
      parse_page_meta_volume()

    saveRDS(page_meta, local_cache, compress = TRUE)
  }
  else {
    page_meta <- readRDS(local_cache)
  }

  page_meta

}

download_hathi_ef <- function(htid,
                              dir = getOption("hathiTools.ef.dir"),
                              cache_type = getOption("hathiTools.cachetype")) {

  cache_type <- match.arg(cache_type, c("csv.gz", "none", "rds",
                                        "feather", "text2vec.csv"))

  local_cache <- local_loc(htid, suffix = cache_type, dir = dir)
  if(file.exists(local_cache)) {
    message("File has already been downloaded. Returning existing cached file.")
    ef <- read_cached_ef_file(local_cache, cache_type)
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

flatten_data <- function(x) {
  if(length(x) > 1) {
    if(!is.null(names(x))) {
      x <- stringr::str_c(names(x), x, sep = "=", collapse = ", ")
    } else {
      x <- toString(x)
    }
  }
  x
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

parse_page_meta <- function(page) {
  seq <- section <- beginChar <- beginCount <- endChar <- endCount <- NULL
  to_parse <- names(page)[!names(page) %in% c("body", "header", "footer")]
  beginCharCount <- page[c("body", "header", "footer")] %>%
    purrr::compact() %>%
    purrr::map("beginCharCount") %>%
    purrr::map_df(~tibble::enframe(.x, name = "beginChar", value = "beginCount") %>%
                 tidyr::unnest("beginCount"),
                 .id = "section")

  if(nrow(beginCharCount) == 0) {
    beginCharCount <- tibble(section = character(0),
                             beginCharCount = list(tibble(beginChar = character(0),
                                                           beginCount = numeric(0))))
  } else {
    beginCharCount <- beginCharCount %>%
      dplyr::group_by(section) %>%
      dplyr::summarise(beginCharCount = list(tibble(beginChar, beginCount)))
  }

  endCharCount <- page[c("body", "header", "footer")] %>%
    purrr::compact() %>%
    purrr::map("endCharCount") %>%
    purrr::map_df(~tibble::enframe(.x, name = "endChar", value = "endCount") %>%
                    tidyr::unnest("endCount"),
                  .id = "section")

  if(nrow(endCharCount) == 0) {
    endCharCount <- tibble(section = character(0),
                            endCharCount = list(tibble(endChar = character(0),
                                                       endCount = numeric(0))))
  } else {
    endCharCount <- endCharCount %>%
      dplyr::group_by(section) %>%
      dplyr::summarise(endCharCount = list(tibble(endChar, endCount)))
  }

  sectionStats <- page[c("body", "header", "footer")] %>%
    purrr::map_df(~dplyr::as_tibble(purrr::compact(.x[c("tokenCount",
                                                        "lineCount",
                                                        "emptyLineCount",
                                                        "sentenceCount",
                                                        "capAlphaSeq")])),
                  .id = "section")

  names(sectionStats)[-1] <- paste0("section", names(sectionStats)[-1])

  sectionStats <- sectionStats %>%
    dplyr::left_join(beginCharCount, by = "section") %>%
    dplyr::left_join(endCharCount, by = "section")

  page <- page[to_parse] %>%
    purrr::compact() %>%
    tibble::as_tibble_row() %>%
    dplyr::mutate(seq = as.numeric(seq)) %>%
    dplyr::bind_cols(sectionStats) %>%
    dplyr::rename(page = seq)

  page
}

parse_page_meta_volume <- function(listified_version) {
  page <- NULL
  res <- listified_version$features$pages %>%
    purrr::map_df(parse_page_meta)

  res$htid <- listified_version$htid
  res

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
    dplyr::mutate(page = as.numeric(page), count = as.numeric(count))
}



