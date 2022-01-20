## Downloading poetry indicator

poetry <- vroom::vroom("http://data.analytics.hathitrust.org/genre/poetry_metadata.csv")

poetry <- poetry %>%
  dplyr::select(htid,
                prob80precise,
                genrepages,
                totalpages) %>%
  dplyr::mutate(poetry_prop = genrepages/totalpages) %>%
  dplyr::rename(poetry_prob = prob80precise) %>%
  dplyr::select(htid, poetry_prob,
                poetry_prop)

usethis::use_data(poetry, overwrite = TRUE)

## Downloading fiction indicator

fiction <- vroom::vroom("http://data.analytics.hathitrust.org/genre/fiction_metadata.csv")

fiction <- fiction %>%
  dplyr::select(htid,
                prob80precise,
                genrepages,
                totalpages) %>%
  dplyr::mutate(fiction_prop = genrepages/totalpages) %>%
  dplyr::rename(fiction_prob = prob80precise) %>%
  dplyr::select(htid, fiction_prob,
                fiction_prop)

usethis::use_data(fiction, overwrite = TRUE)

## Downloading drama indicator

drama <- vroom::vroom("http://data.analytics.hathitrust.org/genre/drama_metadata.csv")

drama <- drama %>%
  dplyr::select(htid,
                prob80precise,
                genrepages,
                totalpages) %>%
  dplyr::mutate(drama_prop = genrepages/totalpages) %>%
  dplyr::rename(drama_prob = prob80precise) %>%
  dplyr::select(htid, drama_prob,
                drama_prop)

usethis::use_data(drama, overwrite = TRUE)

## Downloading header data for hathifile

url <- "https://www.hathitrust.org/hathifiles"

hathi_page <- rvest::read_html(url)

link_text <- hathi_page %>%
  rvest::html_nodes("a") %>%
  rvest::html_text()

link_href <- hathi_page %>%
  rvest::html_nodes("a") %>%
  rvest::html_attr("href")

headers_link <- link_href[which(link_text == "hathi_field_list.txt")]

message(stringr::str_glue("Reading headers from {headers_link}"))

hf_headers <- vroom::vroom(headers_link)

hf_headers <- names(hf_headers)

usethis::use_data(hf_headers, internal = TRUE, overwrite = TRUE)
