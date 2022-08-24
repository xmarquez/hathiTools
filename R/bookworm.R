#' Queries the Hathi Trust Bookworm Server at
#' [https://bookworm.htrc.illinois.edu/develop/](https://bookworm.htrc.illinois.edu/develop/)
#'
#' This function retrieves word frequency data from the Hathi Trust Bookworm
#' Server at https://bookworm.htrc.illinois.edu/develop/, with options to group
#' the results according to various forms of metadata and to limit according to
#' that same metadata. It uses code authored by Ben Schmidt (from
#' [https://github.com/bmschmidt/edinburgh/](https://github.com/bmschmidt/edinburgh/)).
#'
#' @param word Term to get frequencies for. Can be a vector of strings. It can
#'   be left empty if one is interested primarily in statistics about the corpus
#'   as a whole.
#' @param groups Category to group results by. The default is `date_year`, which
#'   groups results by year.
#' @param ignore_case Default is `TRUE`, ignores case in search.
#' @param counttype The default is words per million, `counttype =
#'   "WordsPerMillion"`. According to the [API
#'   documentation](https://bookworm-project.github.io/Docs/query_structure.html),
#'    the following options are available:
#'
#'   `WordCount`: The number of words matching the terms in `search_limits` for
#'   each group. (If no `words` key is specified, the sum of all the words in
#'   the book).
#'
#'   `TextCount`: The number of texts matching the constraints on
#'   `search_limits` for each group.
#'
#'   `WordsPerMillion`: The number of words in the `search_limits` per million
#'   words in the broader set. (Words per million, rather than percent, gives a
#'   more legible number).
#'
#'   `TextPercent`: The percentage of texts in the broader group matching the
#'   search terms.
#'
#'   `TotalTexts`: The number of texts matching the constraints on
#'   `compare_limits`. (By selecting `TextCount` and `TotalTexts`, you can
#'   derive `TextPercent` locally, if you prefer).
#'
#'   `TotalWords`: The number of words in the larger set.
#'
#'   `WordsRatio`: equal to `WordCount/TotalWords`. Useful when `method =
#'   "search_results"`.
#'
#'   `SumWords`: equal to `TotalWords + WordCount`
#'
#'   `TextRatio`: equal to `TextCount/TotalTexts`.
#'
#'   `SumTexts`: equal to `TextCount + TotalTexts`
#'
#'   It is possible to combine some of these - e.g., counttype = c("TextCount",
#'   "TextPercent"). But it is not possible to combine `Text-` counts with
#'   `Word-` counts in this version of the API.
#'
#' @param method Type of results to return. Can be `data` (the default -
#'   automatically converted to a proper [tibble] when possible; the JSON is
#'   structured as "nested dicts for each grouping in `groups` pointing to an
#'   array consisting of the results for each count in `counttype`", according
#'   to the [API
#'   documentation](https://bookworm-project.github.io/Docs/query_structure.html).),
#'    `returnPossibleFields` (metadata fields available to use in `groups`), and
#'   `search_results` (a list of books and HathiTrust URLs matching a query).
#'   Note that `search_results` has a limit of 100 books at the moment, randomly
#'   selected. Notes:
#'
#'   * When using `returnPossibleFields` all other fields are ignored.
#'
#'   * When using `search_results` only the first 100 results are returned,
#'   sorted by the percentage of hits in the text. That biases towards either
#'   texts that use the words a lot, or texts that use it rarely. It is possible
#'   to use `counttype = "WordsRatio"` to return a list sorted randomly,
#'   weighted by the number of times the word appears in it. The [API
#'   documentation](https://bookworm-project.github.io/Docs/query_structure.html)
#'    notes that "this means that a random word from the first text should
#'   represent a random usage from the overall sample. The current MySQL-python
#'   implementation uses an approximation for this:
#'   `LOG(1-RAND())/sum(main.count)` that should mimic a weighted random
#'   ordering for most distributions, but in some cases it may not behave as
#'   intended."
#' @param format Format of returned results. In theory the Bookworm DB should be
#'   able to return results as "json", "tsv", "csv", or even "feather";
#'   currently only "json" works (and it's the only supported format here).
#' @param lims Min and max year as a two-element numeric vector. Default is
#'   `c(1920, 2000)`.
#' @param compare_to A word to compare relative frequencies to. Currently this
#'   is most useful with `counttype = "WordsRatio"`; this compares the relative
#'   frequency of two words.
#' @param as_json Whether to return the raw json. Useful for complex queries
#'   where the function does not know how to return a [tibble], or when you want
#'   to use the raw json to produce a different data structure.
#' @param verbose If `TRUE`, shows the JSON query once built.
#' @param query You can directly pass on a query string (in JSON). This is
#'   useful for very complex queries, but there's no checking that the
#'   parameters are correct so you may encounter unexpected errors. See
#'   https://bookworm-project.github.io/Docs/query_structure.html for more on
#'   the query structure. If you use `query`, all other parameters are silently
#'   ignored. Use with care!
#' @param ... Additional parameters passed to the query builder; these would be
#'   the fields that method = `returnPossibleFields` returns, including fields
#'   to group the query by (e.g., groups = "class"). At the date of this
#'   writing, these fields were: lc_classes, lc_subclass, fiction_nonfiction,
#'   genres, languages, htsource, digitization_agent_code, mainauthor,
#'   publisher, format, is_gov_doc, page_count_bin, word_count_bin,
#'   publication_country, publication_state, publication_place. These are not
#'   documented, and in some cases one must know the exact string to search for;
#'   for example, a search with `mainauthor = "Tocqueville"` won't find
#'   anything, but a search with `mainauthor = "Tocqueville, Alexis de
#'   1805-1859."` may. These fields should be accessible via
#'   `options("hathiTools.bookworm.fields")`
#'
#' @return A tidy [tibble] whenever possible, with columns for each grouping
#'   parameter, the word (if any), and the counts and counttypes. For `method =
#'   "search_result"`, a workset that can be used in [browse_htids] and
#'   [get_workset_meta].
#' @author Ben Schmidt
#'
#' @export
#'
#' @examples
#' \donttest{
#' query_bookworm(word = c("democracy", "monarchy"), lims = c(1760, 2000),
#'   counttype = c("WordsPerMillion", "WordCount"))
#'
#' query_bookworm(word = "democracy", groups = c("date_year", "lc_classes"),
#'   lims = c(1900,2000))
#'
#' query_bookworm(word = "democracy", groups = "date_year", date_year = "1941",
#'   lc_classes = "Education", method = "search_results")
#' }
query_bookworm <- function(word, groups = "date_year",
                           ignore_case =  TRUE,
                           counttype = "WordsPerMillion",
                           method = c("data", "returnPossibleFields", "search_results"),
                           format = c("json", "csv", "tsv", "feather"),
                           lims = c(1920, 2000),
                           compare_to,
                           as_json = FALSE,
                           verbose = TRUE,
                           query, ...) {

  if(missing(query)) {
    method <- match.arg(method, c("data", "returnPossibleFields", "search_results"))

    format <- match.arg(format, c("json", "csv", "tsv", "feather"))

    counttype <- match.arg(counttype, c("WordsPerMillion", "WordCount",
                                        "TextCount", "TextPercent", "TotalTexts",
                                        "TotalWords", "WordsRatio", "SumWords",
                                        "TextRatio", "SumTexts"), several.ok = TRUE)


    if(any(stringr::str_detect(counttype, "Word")) &&
       any(stringr::str_detect(counttype, "Text"))) {
      stop("Cannot combine word and text counttypes")
    }

    if(ignore_case) {
      words_collation = "Case_Insensitive"
    } else {
      words_collation = "Case_Sensitive"
    }

    if(!missing(compare_to) && length(compare_to) > 1) {
      stop("`compare_to` only supports comparisons to one term at a time")
    }

    if(length(lims) != 2) {
      stop("Lims must be a two-element vector of years")
    }
    stopifnot(is.numeric(lims))

    if(!missing(compare_to) && !counttype %in% c("WordsRatio", "TextRatio")) {
      warning("Comparison results may not be meaningful or interpretable if ",
              "`comparison_to` is specified and `counttype` is not equal to ",
              "'WordsRatio' or 'TextRatio'")

    }

    date_year <- links <- htid <- title <- NULL

    if(missing(word)) {
      word = ""
    }

    query <- build_json_query(word = word, groups = groups,
                              words_collation = words_collation,
                              counttype = counttype, method = method,
                              lims = lims, compare_to = compare_to,
                              format = format,
                              verbose = verbose,
                              ...)
  } else {
    query_from_json <- jsonlite::fromJSON(query)
    query <- paste0("https://bookworm.htrc.illinois.edu/develop/cgi-bin?query=",query)
    method <- query_from_json[["method"]]
    if(is.null(method)) {
      method <- "data"
    }
  }

  result <- httr::GET(query)

  if(httr::http_error(result)) {
    stop(stringr::str_glue("Error. Status code: {result$status_code}"))
  }

  result <- httr::content(result, as = "parsed", encoding = "UTF-8")

#  result <- stringr::str_replace_all(result, stringr::fixed("[Infinity]"), "[]")

  if(!is.null(result$status) && stringr::str_detect(result$status, "error")) {
    stop(result$message)
  }

  if(!is.null(result$data) && any(stringr::str_detect(unlist(result$data), "\"status\":\"error\",\"message\""))) {
    msg <- result$data %>%
      unlist() %>%
      unique() %>%
      paste(collapse = "\n ")
    stop(msg)
  }

  if(as_json) {
    data <- jsonlite::fromJSON(result)
    return(data)
  }

  if(method == "returnPossibleFields") {
    data <- result %>%
      purrr::map_depth(.depth = 1, unlist) %>%
      purrr::map_df(tibble::as_tibble_row)
    return(data)
  }

  if(method == "data") {
    data <- result$data %>%
      unlistify(words = word, groups = groups, counttype = counttype)

    if(is.null(data)) {
      message("No results!")
      return(invisible())
    }

    if("date_year" %in% names(data)) {
      data$date_year <- as.integer(data$date_year)
    }
    if("first_author_birth" %in% names(data)) {
      data$first_author_birth <- as.integer(data$first_author_birth)
    }
    if("first_author_death" %in% names(data)) {
      data$first_author_death <- as.integer(data$first_author_death)
    }
    if("record_date_year" %in% names(data)) {
      data$record_date_year <- as.integer(data$record_date_year)
    }
    if("is_gov" %in% names(data)) {
      data$is_gov <- toupper(data$is_gov) %>%
        as.logical()
    }
    if(!is.null(lims) && !("date_year" %in% names(data))) {
      data$min_year <- min(lims)
      data$max_year <- max(lims)
    }

  }

  if(method == "search_results") {
    data <- result %>%
      purrr::map_df(~tibble(url = stringr::str_extract(., "http:.+(?='>)"),
                            htid = stringr::str_remove(url, "http://hdl.handle.net/2027/"),
                            title = stringr::str_extract(., "(?<='>).+(?=</a>)"))) %>%
      dplyr::select(htid, title, url)

    class(data) <- c("hathi_workset", class(data))
  }

  if(!missing(compare_to)) {
    data$compare_to <- compare_to
  }

  data
}

unlistify <- function(res, words, groups, counttype) {
  if(is.null(unlist(res))) {
    return(NULL)
  }
  res <- res %>%
    purrr::map_depth(.depth = length(groups) + 1, unlist)
  for(i in length(groups):1) {
    res <- res %>%
      purrr::map_depth(fix_names,
                       .depth = i) %>%
      purrr::map_depth(.f = function(x) {
        purrr::map_dfr(x, ~dplyr::as_tibble(.x), .id = groups[i])
      },
      .depth = i)
  }
  names(res) <- words
  res <- res %>%
    purrr::map_df(dplyr::as_tibble, .id = "word") %>%
    dplyr::group_by(dplyr::across(!dplyr::last_col())) %>%
    dplyr::mutate(counttype = counttype) %>%
    dplyr::ungroup()

  if(length(words) == 1 && words == "") {
    res$word <- NULL
  }

  res
}

fix_names <- function(x) {
  names(x)[ names(x) == ""] <- "N/A"
  x
}

#' @importFrom utils URLencode
build_json_query <- function(word, groups,
                             words_collation,
                             counttype,
                             lims,
                             method,
                             compare_to,
                             database = "Bookworm2021",
                             format,
                             verbose,
                             ...) {

  word <- word[ word != ""]

  if(length(word) == 0) {
    word <- NULL
  }

  query <- list()

  query[["words_collation"]] <- jsonlite::unbox(words_collation)

  query[["groups"]] <- groups

  query[["database"]] <- jsonlite::unbox(database)

  query[["counttype"]] <- counttype

  additional_lims <- list(...)

  allowed_lims <-  getOption("hathiTools.bookworm.fields")

  if(!is.null(allowed_lims) && !is.null(names(additional_lims))) {
    if(all(!names(additional_lims) %in% allowed_lims)) {
      stop("The field(s) `",
           names(additional_lims)[ !names(additional_lims) %in% allowed_lims],
           "` in your query cannot be found in the Bookworm database.",
           " Allowable fields are ",
           paste(getOption("hathiTools.bookworm.fields"), collapse = ", "))
    }
  }

  if(!is.null(word)) {
    search_limits <- vector("list", length(word))
    for(i in 1:length(word)) {
      search_limits[[i]]$word <- word[i]
      if(!is.null(lims)) {
        search_limits[[i]]$date_year$`$gte` <- lims[1]
        search_limits[[i]]$date_year$`$lte` <- lims[2]
      }
      for (limit in names(additional_lims)) {
        # limit_id <- paste0(limit, "__id")
        search_limits[[i]][[limit]] <- additional_lims[[limit]]
      }
    }
    query[["search_limits"]] <- search_limits
  } else {
    search_limits <- vector("list", 1)
    if(!is.null(lims)) {
      search_limits[[1]]$date_year$`$gte` <- lims[1]
      search_limits[[1]]$date_year$`$lte` <- lims[2]
      }
      for (limit in names(additional_lims)) {
        # limit_id <- paste0(limit, "__id")
        search_limits[[1]][[limit]] <- additional_lims[[limit]]
      }
    query[["search_limits"]] <- search_limits
  }

  if(!missing(compare_to) && !is.null(word)) {
    compare_limits <- vector("list", length(compare_to))
    for(i in 1:length(compare_to)) {
      compare_limits[[i]]$word <- compare_to[i]
      if(!is.null(lims)) {
        compare_limits[[i]]$date_year$`$gte` <- lims[1]
        compare_limits[[i]]$date_year$`$lte` <- lims[2]
      }
      for (limit in names(additional_lims)) {
        # limit_id <- paste0(limit, "__id")
        compare_limits[[i]][[limit]] <- additional_lims[[limit]]
      }
    }
    query[["compare_limits"]] <- purrr::flatten(compare_limits)
  }

  query[["method"]] <- jsonlite::unbox(method)

  query[["format"]] <- jsonlite::unbox(format)

  query <- jsonlite::toJSON(query)

  if(verbose) {
    message(query)
  }


  query <- utils::URLencode(query, reserved = TRUE)

  query <- paste0("https://bookworm.htrc.illinois.edu/develop/cgi-bin?query=",query)

  query

}


