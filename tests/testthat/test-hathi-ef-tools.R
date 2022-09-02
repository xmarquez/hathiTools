dir <- tempdir()

test_that("get_hathi_counts returns a tibble", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  ef <- get_hathi_counts("aeu.ark:/13960/t3qv43c3w", dir = dir)
  expect_snapshot(ef)
  ef2 <- get_hathi_counts("aeu.ark:/13960/t3qv43c3w",
                         dir = dir, cache_format = "rds")
  expect_snapshot(ef2)
  # We do it again
  ef2.1 <- get_hathi_counts("aeu.ark:/13960/t3qv43c3w",
                          dir = dir, cache_format = "rds")
  expect_identical(ef2, ef2.1)
  expect_identical(ef, ef2.1)
  ef3 <- get_hathi_counts("aeu.ark:/13960/t3qv43c3w",
                         dir = dir, cache_format = "text2vec.csv")
  expect_true(nrow(ef3) < nrow(ef2))
  expect_true(nrow(ef3) == length(unique(ef2$page)))
  expect_identical(names(ef3), c("htid", "page", "section", "tokens"))
  ef4 <- get_hathi_counts("aeu.ark:/13960/t3qv43c3w",
                         dir = dir, cache_format = "feather")
  expect_snapshot(ef4)
  expect_identical(ef, ef4)
  ef5 <- get_hathi_counts("aeu.ark:/13960/t3qv43c3w",
                          dir = dir, cache_format = "parquet")
  expect_snapshot(ef5)
  expect_identical(ef, ef5)
  ef6 <- get_hathi_counts("aeu.ark:/13960/t3qv43c3w",
                          dir = dir, cache_format = "parquet")
  expect_identical(ef5, ef6)
  clear_cache("aeu.ark:/13960/t3qv43c3w", dir = dir)
}
)

test_that("get_hathi_counts returns a tibble with cache = 'none'", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  ef <- get_hathi_counts("aeu.ark:/13960/t3qv43c3w",
                         dir = dir, cache_format = "none")
  expect_snapshot(ef)
})

test_that("get_hathi_meta returns correct metadata", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  clear_cache("aeu.ark:/13960/t3qv43c3w", dir = dir, keep_json = FALSE)
  meta <- get_hathi_meta("aeu.ark:/13960/t3qv43c3w", dir = dir)
  expect_snapshot(meta)
})

test_that("get_hathi_page_meta returns correct metadata but not from cache", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  clear_cache("aeu.ark:/13960/t3qv43c3w", dir = dir, keep_json = FALSE)
  pagemeta <- get_hathi_page_meta("aeu.ark:/13960/t3qv43c3w", dir = dir)
  expect_snapshot(pagemeta)
  cached_pagemeta <- get_hathi_page_meta("aeu.ark:/13960/t3qv43c3w", dir = dir)
  expect_snapshot(cached_pagemeta)
  expect_identical(pagemeta, cached_pagemeta)
})

unlink(dir)
