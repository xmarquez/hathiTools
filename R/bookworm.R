

build_json_query <- function(word, groups,
                             words_collation,
                             counttype,
                             lims,
                             method,
                             database = "Bookworm2016",
                             ...) {

  words_collation <- match.arg(words_collation, c("Case_Insensitive", "Case_Sensitive", "Stem"))

  counttype <- match.arg(counttype, c("WordsPerMillion", "WordCount",
                                      "TextCount", "TextPercent", "TotalTexts",
                                      "TotalWords", "WordsRatio", "SumWords"), several.ok = TRUE)

  method <- match.arg(method, c("return_json", "returnPossibleFields", "search_results"))

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

  query <- jsonlite::toJSON(query)

  # message(query)

  query <- utils::URLencode(query, reserved = TRUE)

  query <- paste0("https://bookworm.htrc.illinois.edu/cgi-bin/dbbindings.py?query=",query)

  # message(query)

  query
}

#' Queries the HathiTrust Bookworm Server at
#' https://bookworm.htrc.illinois.edu/develop/
#'
#' @param word At least one term to get frequencies for. Can be a vector of
#'   strings.
#' @param groups At least one category to group results by. The default is
#'   `date_year`, which groups results by year.
#' @param words_collation Whether to use case-sensitive (`"Case_Sensitive"`) or
#'   case-insensitive (`"Case_Insensitive"`, the default) matching. Can also be
#'   `stem` to return word results for word stems. See the [Bookworm API
#'   documentation](https://bookworm-project.github.io/Docs/query_structure.html)
#'    for details.
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
#'   `TextRatio`: equal to `TextCount/TotalTexts`
#'
#'   `SumTexts`: equal to `TextCount + TotalTexts`
#' @param method Type of results to return. Can be `return_json` (the default -
#'   automatically converted to a proper tibble when possible; the JSON is
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
#' @param as_json Whether to return the raw json. Useful for complex queries
#'   where the function does not know how to return a [tibble], or when you want
#'   to use the raw json to produce a different data structure.
#' @param ... Additional parameters passed to `build_json_query`
#'
#' @return A [tibble] whenver possible, otherwise json-formatted text.
#' @importFrom rlang .data
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
                           words_collation = "Case_Insensitive",
                           counttype = "WordsPerMillion",
                           method = "return_json",
                           lims = c(1920, 2000),
                           as_json = FALSE, ...) {

  date_year <- links <- htid <- title <- NULL

  if(missing(word)) {
    word = ""
  }

  query <- build_json_query(word = word, groups = groups,
                            words_collation = words_collation,
                            counttype = counttype, method = method,
                            lims = lims,
                            ...)

  result <- httr::GET(query)

  if(httr::http_error(result)) {
    message(stringr::str_glue("Error. Status code: {result$status_code}"))
  }

  result <- httr::content(result, as = "text")

  if(stringr::str_detect(result, "error")) {
    stop(result)
  }

  data <- jsonlite::fromJSON(result)


  if(as_json) {
    return(data)
  }

  if(method == "returnPossibleFields") {
    return(tibble::as_tibble(data))
  }

  if(length(word) >= 1 & length(groups) == 1 &
     groups[1] == "date_year" & method == "return_json") {
    data <- data %>%
      purrr::map_df(tibble::as_tibble, .id = "date_year", .name_repair = ~word) %>%
      dplyr::mutate(date_year = as.integer(date_year)) %>%
        dplyr::mutate(counttype = rep(counttype, dplyr::n()/length(counttype)))

  }

  if(length(word) >= 1 & length(groups) == 2 & method == "return_json" & length(counttype) == 1) {

    data <- data %>%
      purrr::map_df(tibble::as_tibble, .id = groups[1], .name_repair = ~(ifelse(.x == "", "N/A", .x)))

    num_cols <- ncol(data)

    data <- data %>%
      dplyr::mutate(word = rep(word, dplyr::n()/length(word))) %>%
      tidyr::unnest(dplyr::everything()) %>%
      tidyr::pivot_longer(2:num_cols, names_to = groups[2], values_to = counttype) %>%
      dplyr::filter(dplyr::across({{ counttype }}, ~!is.na(.x)))

    if("date_year" %in% names(data)) {
      data <- data %>%
        dplyr::mutate(date_year = as.integer(date_year))
    }
  }

  if(method == "search_results") {
    data <- tibble(links = data) %>%
      dplyr::mutate(htid = stringr::str_extract(links, "(?<=id=).+?(?=>)"),
             title = stringr::str_extract(links, "(?<=<em>).+(?=</em>)"),
             url = stringr::str_extract(links, "http.+?(?=>)")) %>%
      dplyr::select(htid, title, url)
  }

  data
}

