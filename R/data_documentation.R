#' Fiction Dataset
#'
#' A dataset containing the HTIDs of the volumes classified as "fiction" in Ted
#' Underwood, Boris Capitanu, Peter Organisciak, Sayan Bhattacharyya, Loretta
#' Auvil, Colleen Fallaw, J. Stephen Downie (2015). Word Frequencies in
#' English-Language Literature, 1700-1922 (0.2)
#' [Dataset](http://data.analytics.hathitrust.org/genre/fiction_metadata.csv).
#' HathiTrust Research Center. doi:10.13012/J8JW8BSJ. Taken from the summary
#' netadata file at
#' http://data.analytics.hathitrust.org/genre/fiction_metadata.csv
#'
#' \describe{
#'
#' \item{htid}{The Hathi Trust ID of the volume}
#'
#' \item{fiction_prob}{A confidence metric: the probability that more than 80%
#' of the pages in the volume assigned to "fiction" have been correctly
#' classified.}
#'
#' \item{fiction_prop}{The proportion of pages in the volume classified as
#' "fiction". Calculated from `genrepages`/`totalpages` in the original metadata
#' file.}
#'
#' }
#'
#' @source
#' \url{https://wiki.htrc.illinois.edu/display/COM/Word+Frequencies+in+English-Language+Literature,+1700-1922}
#'
"fiction"

#' Drama Dataset
#'
#' A dataset containing the HTIDs of the volumes classified as "drama" in Ted
#' Underwood, Boris Capitanu, Peter Organisciak, Sayan Bhattacharyya, Loretta
#' Auvil, Colleen Fallaw, J. Stephen Downie (2015). Word Frequencies in
#' English-Language Literature, 1700-1922 (0.2)
#' [Dataset](http://data.analytics.hathitrust.org/genre/drama_metadata.csv).
#' HathiTrust Research Center. doi:10.13012/J8JW8BSJ. Taken from the summary
#' netadata file at
#' http://data.analytics.hathitrust.org/genre/drama_metadata.csv
#'
#' \describe{
#'
#' \item{htid}{The Hathi Trust ID of the volume.}
#'
#' \item{drama_prob}{A confidence metric: the probability that more than 80% of
#' the pages in the volume assigned to "drama" have been correctly classified.}
#'
#' \item{drama_prop}{The proportion of pages in the volume classified as
#' "drama". Calculated from `genrepages`/`totalpages` in the original metadata
#' file.}
#'
#' }
#'
#' @source
#' \url{https://wiki.htrc.illinois.edu/display/COM/Word+Frequencies+in+English-Language+Literature,+1700-1922}
#'
"drama"

#' Poetry Dataset
#'
#' A dataset containing the HTIDs of the volumes classified as "poetry" in Ted
#' Underwood, Boris Capitanu, Peter Organisciak, Sayan Bhattacharyya, Loretta
#' Auvil, Colleen Fallaw, J. Stephen Downie (2015). Word Frequencies in
#' English-Language Literature, 1700-1922 (0.2)
#' [Dataset](http://data.analytics.hathitrust.org/genre/poetry_metadata.csv).
#' HathiTrust Research Center. doi:10.13012/J8JW8BSJ. Taken from the summary
#' netadata file at
#' http://data.analytics.hathitrust.org/genre/drama_metadata.csv
#'
#' \describe{
#'
#' \item{htid}{The Hathi Trust ID of the volume.}
#'
#' \item{poetry_prob}{A confidence metric: the probability that more than 80% of
#' the pages in the volume assigned to "drama" have been correctly classified.}
#'
#' \item{poetry_prop}{The proportion of pages in the volume classified as
#' "poetry". Calculated from `genrepages`/`totalpages` in the original metadata
#' file.}
#'
#' }
#'
#' @source
#' \url{https://wiki.htrc.illinois.edu/display/COM/Word+Frequencies+in+English-Language+Literature,+1700-1922}
#'
"poetry"

#' ISO639 language codes
#'
#' A dataset containing the ISO639 language codes. Used internally to match
#' languages.
#'
#' \describe{
#'
#' \item{alpha3-b}{alpha-3 bibliographic code}
#'
#' \item{alpha3-t}{alpha-3 terminologic code (when given)}
#'
#' \item{alpha2}{alpha-2 code (when given)}
#'
#' \item{English}{English name of language}
#'
#' \item{French}{French name of language}
#' }
#'
#' @source
#' \url{https://datahub.io/core/language-codes#resource-language-codes-full}
"iso639"
