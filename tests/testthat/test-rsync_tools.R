dir <- path.expand("./hathi-ef")

test_that("Can rsync", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  expect_snapshot(rsync_result <- rsync_from_hathi(poetry$htid[1:10], dir = dir))
  expect_equal(rsync_result, 0)
  files <- fs::dir_ls(path = dir, recurse = TRUE, glob = "*.json*")
  expect_equal(length(files), 10)
})

test_that("Can cache to csv", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  cache_all(dir = dir)
  files <- fs::dir_ls(path = dir, recurse = TRUE, glob = "*.csv*")
  expect_equal(length(files), 10)
  expect_snapshot(get_hathi_counts(poetry$htid[1], dir = dir))
})

test_that("Can cache to text2vec.csv", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  cache_all(dir = dir, cache_type = "text2vec")
  files <- fs::dir_ls(path = dir, recurse = TRUE, glob = "*.text2vec.csv")
  expect_equal(length(files), 10)
  expect_snapshot(get_hathi_counts(poetry$htid[1], dir = dir, cache_type = "text2vec.csv"))
})

test_that("Can cache to rds", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  cache_all(dir = dir, cache_type = "rds")
  files <- fs::dir_ls(path = dir, recurse = TRUE, glob = "*.rds")
  expect_equal(length(files), 10)
  expect_snapshot(get_hathi_counts(poetry$htid[1], dir = dir, cache_type = "rds"))
})

test_that("Can cache to arrow", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  cache_all(dir = dir, cache_type = "feather")
  files <- fs::dir_ls(path = dir, recurse = TRUE, glob = "*.feather")
  expect_equal(length(files), 10)
  expect_snapshot(get_hathi_counts(poetry$htid[1], dir = dir, cache_type = "feather"))
})

unlink(dir, recursive = TRUE, force = TRUE)
unlink(dir)
