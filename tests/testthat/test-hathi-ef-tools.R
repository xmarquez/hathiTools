dir <- tempdir()

test_that("get_hathi_counts returns a tibble", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  ef <- get_hathi_counts("aeu.ark:/13960/t3qv43c3w", dir = dir)
  expect_snapshot(ef)
})

test_that("get_hathi_counts returns a tibble with cache = 'rds'", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  ef <- get_hathi_counts("aeu.ark:/13960/t3qv43c3w",
                         dir = dir, cache_type = "rds")
  expect_snapshot(ef)
})

test_that("get_hathi_counts returns a tibble with cache = 'text2vec.csv'", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  ef <- get_hathi_counts("aeu.ark:/13960/t3qv43c3w",
                         dir = dir, cache_type = "text2vec.csv")
  expect_snapshot(ef)
})

test_that("get_hathi_counts returns a tibble with cache = 'none'", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  ef <- get_hathi_counts("aeu.ark:/13960/t3qv43c3w",
                         dir = dir, cache_type = "none")
  expect_snapshot(ef)
})

test_that("get_hathi_counts returns a tibble with cache = 'feather'", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  ef <- get_hathi_counts("aeu.ark:/13960/t3qv43c3w",
                         dir = dir, cache_type = "feather")
  expect_snapshot(ef)
})

test_that("get_hathi_meta returns correct metadata", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  ef <- get_hathi_meta("aeu.ark:/13960/t3qv43c3w", dir = dir)
  expect_snapshot(ef)
})
unlink(dir)
