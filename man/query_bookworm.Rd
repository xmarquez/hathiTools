% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bookworm.R
\name{query_bookworm}
\alias{query_bookworm}
\title{Queries the Hathi Trust Bookworm Server at
\url{https://bookworm.htrc.illinois.edu/develop/}}
\usage{
query_bookworm(
  word,
  groups = "date_year",
  ignore_case = TRUE,
  counttype = "WordsPerMillion",
  method = c("data", "returnPossibleFields", "search_results"),
  format = c("json", "csv", "tsv", "feather"),
  lims = c(1920, 2000),
  compare_to,
  as_json = FALSE,
  verbose = FALSE,
  query,
  ...
)
}
\arguments{
\item{word}{Term to get frequencies for. Can be a vector of strings. It can
be left empty if one is interested primarily in statistics about the corpus
as a whole.}

\item{groups}{Category to group results by. The default is \code{date_year}, which
groups results by year.}

\item{ignore_case}{Default is \code{TRUE}, ignores case in search.}

\item{counttype}{The default is words per million, \code{counttype = "WordsPerMillion"}. According to the \href{https://bookworm-project.github.io/Docs/query_structure.html}{API documentation},
the following options are available:

\code{WordCount}: The number of words matching the terms in \code{search_limits} for
each group. (If no \code{words} key is specified, the sum of all the words in
the book).

\code{TextCount}: The number of texts matching the constraints on
\code{search_limits} for each group.

\code{WordsPerMillion}: The number of words in the \code{search_limits} per million
words in the broader set. (Words per million, rather than percent, gives a
more legible number).

\code{TextPercent}: The percentage of texts in the broader group matching the
search terms.

\code{TotalTexts}: The number of texts matching the constraints on
\code{compare_limits}. (By selecting \code{TextCount} and \code{TotalTexts}, you can
derive \code{TextPercent} locally, if you prefer).

\code{TotalWords}: The number of words in the larger set.

\code{WordsRatio}: equal to \code{WordCount/TotalWords}. Useful when \code{method = "search_results"}.

\code{SumWords}: equal to \code{TotalWords + WordCount}

\code{TextRatio}: equal to \code{TextCount/TotalTexts}.

\code{SumTexts}: equal to \code{TextCount + TotalTexts}

It is possible to combine some of these - e.g., counttype = c("TextCount",
"TextPercent"). But it is not possible to combine \verb{Text-} counts with
\verb{Word-} counts in this version of the API.}

\item{method}{Type of results to return. Can be \code{data} (the default -
automatically converted to a proper \link{tibble} when possible; the JSON is
structured as "nested dicts for each grouping in \code{groups} pointing to an
array consisting of the results for each count in \code{counttype}", according
to the \href{https://bookworm-project.github.io/Docs/query_structure.html}{API documentation}.),
\code{returnPossibleFields} (metadata fields available to use in \code{groups}), and
\code{search_results} (a list of books and HathiTrust URLs matching a query).
Note that \code{search_results} has a limit of 100 books at the moment, randomly
selected. Notes:
\itemize{
\item When using \code{returnPossibleFields} all other fields are ignored.
\item When using \code{search_results} only the first 100 results are returned,
sorted by the percentage of hits in the text. That biases towards either
texts that use the words a lot, or texts that use it rarely. It is possible
to use \code{counttype = "WordsRatio"} to return a list sorted randomly,
weighted by the number of times the word appears in it. The \href{https://bookworm-project.github.io/Docs/query_structure.html}{API documentation}
notes that "this means that a random word from the first text should
represent a random usage from the overall sample. The current MySQL-python
implementation uses an approximation for this:
\code{LOG(1-RAND())/sum(main.count)} that should mimic a weighted random
ordering for most distributions, but in some cases it may not behave as
intended."
}}

\item{format}{Format of returned results. In theory the Bookworm DB should be
able to return results as "json", "tsv", "csv", or even "feather";
currently only "json" works (and it's the only supported format here).}

\item{lims}{Min and max year as a two-element numeric vector. Default is
\code{c(1920, 2000)}.}

\item{compare_to}{A word to compare relative frequencies to. Currently this
is most useful with \code{counttype = "WordsRatio"}; this compares the relative
frequency of two words.}

\item{as_json}{Whether to return the raw json. Useful for complex queries
where the function does not know how to return a \link{tibble}, or when you want
to use the raw json to produce a different data structure.}

\item{verbose}{If \code{TRUE}, shows the JSON query once built.}

\item{query}{You can directly pass on a query string (in JSON). This is
useful for very complex queries, but there's no checking that the
parameters are correct so you may encounter unexpected errors. See
https://bookworm-project.github.io/Docs/query_structure.html for more on
the query structure. If you use \code{query}, all other parameters are silently
ignored. Use with care!}

\item{...}{Additional parameters passed to the query builder; these would be
the fields that method = \code{returnPossibleFields} returns, including fields
to group the query by (e.g., groups = "class"). At the date of this
writing, these fields were: lc_classes, lc_subclass, fiction_nonfiction,
genres, languages, htsource, digitization_agent_code, mainauthor,
publisher, format, is_gov_doc, page_count_bin, word_count_bin,
publication_country, publication_state, publication_place. These are not
documented, and in some cases one must know the exact string to search for;
for example, a search with \code{mainauthor = "Tocqueville"} won't find
anything, but a search with \code{mainauthor = "Tocqueville, Alexis de 1805-1859."} may. These fields should be accessible via
\code{options("hathiTools.bookworm.fields")}}
}
\value{
A tidy \link{tibble} whenever possible, with columns for each grouping
parameter, the word (if any), and the counts and counttypes. For \code{method = "search_result"}, a workset that can be used in \link{browse_htids} and
\link{get_workset_meta}.
}
\description{
This function retrieves word frequency data from the Hathi Trust Bookworm
Server at https://bookworm.htrc.illinois.edu/develop/, with options to group
the results according to various forms of metadata and to limit according to
that same metadata. It uses code authored by Ben Schmidt (from
\url{https://github.com/bmschmidt/edinburgh/}).
}
\examples{
\donttest{
query_bookworm(word = c("democracy", "monarchy"), lims = c(1760, 2000),
  counttype = c("WordsPerMillion", "WordCount"))

query_bookworm(word = "democracy", groups = c("date_year", "lc_classes"),
  lims = c(1900,2000))

query_bookworm(word = "democracy", groups = "date_year", date_year = "1941",
  lc_classes = "Education", method = "search_results")
}
}
\author{
Ben Schmidt
}
