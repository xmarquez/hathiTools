#' Queries the Hathi Trust Bookworm Server at
#' https://bookworm.htrc.illinois.edu/develop/
#'
#' Retrieves word frequency data from the Hathi Trust Bookworm Server at
#' https://bookworm.htrc.illinois.edu/develop/, with options to group according
#' to various forms of metadata and to limit according to that same metadata.
#' This funciton uses code authored by Ben Schmidt
#' [https://github.com/bmschmidt/edinburgh/](https://github.com/bmschmidt/edinburgh/).
#'
#' @param word At least one term to get frequencies for. Can be a vector of
#'   strings.
#' @param groups At least one category to group results by. The default is
#'   `date_year`, which groups results by year.
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
#'   `TextRatio`: equal to `TextCount/TotalTexts` Currently does not work. Will
#'   throw an error in this version.
#'
#'   `SumTexts`: equal to `TextCount + TotalTexts` Currently does not work. Will
#'   throw an error in this version.
#' @param method Type of results to return. Can be `return_json` (the default -
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
#' @param lims Min and max year as a two-element numeric vector. Default is
#'   `c(1920, 2000)`.
#' @param compare_to A word to compare relative frequencies to. Currently this
#'   is most useful with `counttype = "WordsRatio"`; this compares the relative
#'   frequency of two words.
#' @param as_json Whether to return the raw json. Useful for complex queries
#'   where the function does not know how to return a [tibble], or when you want
#'   to use the raw json to produce a different data structure.
#' @param ... Additional parameters passed to the query builder; these would be
#'   the fields that method = `returnPossibleFields` returns, including fields
#'   to group the query by (e.g., groups = "class"). At the date of this
#'   writing, these fields were: language, publication_country,
#'   publication_state, subclass, narrow_class, class, resource_type,
#'   target_audience, scanner, first_author_birth, first_author_name,
#'   contributing_library, literary_form, cataloguing_source,
#'   first_author_death, first_place, first_publisher, is_gov, subject_places,
#'   date_year, and record_date_year. These are not documented, and in some
#'   cases one must know the exact string to search for; for example, a search
#'   with `first_author_name = "Tocqueville"` won't find anything, but a search
#'   with `first_author_name = "Tocqueville, Alexis de 1805-1859."` may.
#'
#' @return A tidy [tibble] whenever possible, with columns for each grouping
#'   parameter, the word (if any), and the counts and counttypes. For `method =
#'   "search_result"`, a workset that can be used in [browse_htids] and
#'   [get_workset_meta].
#' @export
#'
#' @examples
#' \dontrun{
#' result <- query_bookworm(word = c("democracy", "monarchy"), lims = c(1760, 2000),
#'   counttype = c("WordsPerMillion", "TextPercent"))
#'
#' result2 <- query_bookworm(word = "democracy", groups = c("date_year", "class"), lims = c(1900,2000))
#'
#' result4 <- query_bookworm(word = c("democracy"), lims = c(1760, 2000), counttype = c("TotalTexts"),
#'   language = "English")
#'
#' result5 <- query_bookworm(word = "democracy", groups = "date_year", date_year = "1941",
#'   class = "Education", method = "search_results")
#' }
query_bookworm <- function(word, groups = "date_year",
                           ignore_case =  TRUE,
                           counttype = "WordsPerMillion",
                           method = c("return_json", "returnPossibleFields", "search_results"),
                           lims = c(1920, 2000),
                           compare_to,
                           as_json = FALSE, ...) {

  method <- match.arg(method, c("return_json", "returnPossibleFields", "search_results"))

  counttype <- match.arg(counttype, c("WordsPerMillion", "WordCount",
                                      "TextCount", "TextPercent", "TotalTexts",
                                      "TotalWords", "WordsRatio", "SumWords",
                                      "TextRatio", "SumTexts"), several.ok = TRUE)

  if(ignore_case) {
    words_collation = "Case_Insensitive"
  } else {
    words_collation = "Case_Sensitive"
  }

  if(!missing(compare_to) && length(compare_to) > 1) {
    stop("`compare_to` only supports comparisons to one term at a time")
  }

  if(length(lims) > 2) {
    stop("Lims must be a two-element vector of years")
  }
  stopifnot(is.numeric(lims))

  if(!missing(compare_to) && counttype != "WordsRatio") {
    warning("Comparison results may not be meaningful or interpretable if ",
            "`comparison_to` is specified and `counttype` is not equal to ",
            "WordsRatio")

  }

  date_year <- links <- htid <- title <- NULL

  if(missing(word)) {
    word = ""
  }

  query <- build_json_query(word = word, groups = groups,
                            words_collation = words_collation,
                            counttype = counttype, method = method,
                            lims = lims, compare_to = compare_to,
                            ...)

  result <- httr::GET(query)

  if(httr::http_error(result)) {
    message(stringr::str_glue("Error. Status code: {result$status_code}"))
  }

  result <- httr::content(result, as = "text", encoding = "UTF-8")

  result <- stringr::str_replace_all(result, stringr::fixed("[Infinity]"), "[]")

  if(stringr::str_detect(result, "Database error. Try checking field names.+status.+error")) {
    stop("Database error. Try checking field names, if necessary using method = returnPossibleFields")
  }

  if(as_json) {
    data <- jsonlite::fromJSON(result)
    return(data)
  }

  if(method == "returnPossibleFields") {
    data <- jsonlite::fromJSON(result)
    return(tibble::as_tibble(data))
  }

  if(method == "return_json") {
    data <- jsonlite::fromJSON(result, simplifyDataFrame = FALSE) %>%
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

  }

  if(method == "search_results") {
    data <- jsonlite::fromJSON(result, simplifyDataFrame = TRUE)
    data <- tibble(links = data) %>%
      dplyr::mutate(htid = stringr::str_extract(links, "(?<=id=).+?(?=>)"),
                    title = stringr::str_extract(links, "(?<=<em>).+(?=</em>)"),
                    url = stringr::str_extract(links, "http.+?(?=>)")) %>%
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
                             database = "Bookworm2016",
                             ...) {

  query <- list()

  query[["method"]] <- jsonlite::unbox(method)

  query[["words_collation"]] <- jsonlite::unbox(words_collation)

  query[["groups"]] <- groups

  query[["database"]] <- jsonlite::unbox(database)

  query[["counttype"]] <- counttype

  additional_lims <- list(...)

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
    query[["compare_limits"]] <- compare_limits
  }

  query <- jsonlite::toJSON(query)

  # message(query)

  query <- utils::URLencode(query, reserved = TRUE)

  query <- paste0("https://bookworm.htrc.illinois.edu/cgi-bin/dbbindings.py?query=",query)

  # message(query)

  query
}


