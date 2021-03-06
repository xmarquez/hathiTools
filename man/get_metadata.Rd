% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hathi-ef-tools.R
\name{get_metadata}
\alias{get_metadata}
\title{Reads the metadata of a downloaded Hathi Trust extracted features file}
\usage{
get_metadata(htid, dir = "hathi-ef")
}
\arguments{
\item{htid}{The Hathi Trust id of the item whose extracted features files are
to be downloaded.}

\item{dir}{The directory where the file is saved. Defaults to \code{hathi-ef}. If
the file does not exist, it will first be downloaded using
\link{download_hathi}.}
}
\value{
a \link{tibble} with metadata.
}
\description{
Reads the metadata of a downloaded Hathi Trust extracted features file
}
\examples{
\dontrun{
# Download the 1862 version of "Democracy in America" by Tocqueville

tmp <- tempdir()
download_hathi("mdp.39015001796443", dir = tmp)

get_metadata("mdp.39015001796443", dir = tmp)
}
}
