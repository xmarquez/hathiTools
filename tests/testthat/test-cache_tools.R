suppressWarnings(library(dplyr))
suppressWarnings(library(purrr))

dir <- tempdir()

test_that("Can cache to all formats", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  rsync_from_hathi(poetry$htid[1:10], dir = dir)
  cache_htids(poetry$htid[1:10], dir = dir,
              cache_format = c("csv.gz", "none", "rds",
                               "feather", "text2vec.csv", "parquet"))
  files <- find_cached_htids(poetry$htid[1:10], dir = dir,
                             cache_format = c("csv.gz", "none", "rds",
                                              "feather", "text2vec.csv",
                                              "parquet"))
  expect_equal(nrow(files), 160)
  expect_snapshot(files %>% count(cache_type, cache_format))

})

test_that("Can read from cache, and cache returns correct values", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  ef <- read_cached_htids(poetry$htid[1:10], cache_type = "ef", dir = dir)
  meta <- read_cached_htids(poetry$htid[1:10], cache_type = "meta", dir = dir)
  pagemeta <- read_cached_htids(poetry$htid[1:10], cache_type = "pagemeta", dir = dir)
  full_from_csv <- read_cached_htids(poetry$htid[1:10],
                                     cache_format = "csv.gz", dir = dir)
  full_from_csv.2 <- read_cached_htids(poetry$htid[1:10],
                                     cache_format = "csv.gz", dir = dir,
                                     nest_char_count = TRUE)
  expect_true(is.character(full_from_csv$sectionBeginCharCount))
  expect_true(is.list(full_from_csv.2$sectionBeginCharCount))
  full_from_rds <- read_cached_htids(poetry$htid[1:10],
                                     cache_format = "rds", dir = dir)
  full_from_parquet <- read_cached_htids(poetry$htid[1:10],
                                         cache_format = "parquet", dir = dir)
  pagemeta_from_parquet <- read_cached_htids(poetry$htid[1:10],
                                             cache_type = "pagemeta",
                                             cache_format = "parquet", dir = dir)
  ef_from_text2vec <- read_cached_htids(poetry$htid[1:10],
                                        cache_format = "text2vec",
                                        cache_type = "ef", dir = dir)
  ef_from_text2vec_w_meta <- read_cached_htids(poetry$htid[1:10],
                                               cache_format = "text2vec",
                                               cache_type = c("ef", "meta"),
                                               dir = dir)
  meta_and_pagemeta_only <- read_cached_htids(poetry$htid[1:10],
                                              cache_type = c("meta", "pagemeta"),
                                              dir = dir)
  expect_true(nrow(ef_from_text2vec) < nrow(full_from_csv))
  expect_equal(nrow(ef_from_text2vec), nrow(full_from_csv %>% distinct(htid, page, section)))
  expect_equal(nrow(ef_from_text2vec), nrow(meta_and_pagemeta_only))
  expect_equal(nrow(ef_from_text2vec_w_meta), nrow(meta_and_pagemeta_only))
  ef_and_pagemeta_only <- read_cached_htids(poetry$htid[1:10],
                                            cache_type = c("ef", "pagemeta"),
                                            dir = dir)
  expect_snapshot(ef)
  expect_identical(names(ef),
                   c("htid", "token", "POS", "count", "section", "page"))
  expect_snapshot(meta)
  expect_identical(names(meta),
                   c("htid", "schemaVersion", "id", "type", "dateCreated",
                     "title", "contributor", "pubDate", "publisher", "pubPlace",
                     "language", "accessRights", "accessProfile",
                     "sourceInstitution", "mainEntityOfPage", "oclc", "genre",
                     "typeOfResource", "lastRightsUpdateDate", "lcc", "lccn",
                     "category", "enumerationChronology"))
  expect_snapshot(pagemeta)
  expect_snapshot(full_from_csv)
  expect_snapshot(full_from_rds)
  expect_snapshot(full_from_parquet)
  expect_snapshot(ef_from_text2vec)
  expect_snapshot(ef_from_text2vec_w_meta)
  expect_snapshot(ef_and_pagemeta_only)
  expect_identical(full_from_csv, full_from_rds)
})

test_that("Can clear cache", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  closeAllConnections()
  expect_invisible(deleted_files <- clear_cache(poetry$htid[1:10], dir = dir))

})

test_that("Can cache workset", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  workset <- poetry[ 1:2, ]
  class(workset) <- c("hathi_workset", class(workset))
  cache_htids(workset, dir = dir, cache_format = "feather")
  files <- find_cached_htids(workset, cache_format = "feather",
                             dir = dir)
  expect_snapshot(files)
  expect_invisible(deleted <- clear_cache(workset, cache_format = "feather",
                                          dir = dir))
})

closeAllConnections()
fs::dir_delete(dir)
