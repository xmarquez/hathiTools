dir <- path.expand("./hathi-ef")

test_that("Can cache to csv", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  rsync_from_hathi(poetry$htid[1:10], dir = dir)
  cache_htids(poetry$htid[1:10], dir = dir)
  files <- find_cached_htids(poetry$htid[1:10], dir = dir)
  expect_equal(nrow(files), 30)
  expect_snapshot(get_hathi_counts(poetry$htid[1], dir = dir))
})

test_that("Can cache to text2vec.csv", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  cache_htids(poetry$htid[1:10], dir = dir, cache_format = "text2vec")
  files <- fs::dir_ls(path = dir, recurse = TRUE, glob = "*.text2vec.csv")
  expect_equal(length(files), 10)
  expect_snapshot(get_hathi_counts(poetry$htid[1], dir = dir, cache_format = "text2vec.csv"))
})

test_that("Can cache to rds", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  cache_htids(poetry$htid[1:10], dir = dir, cache_format = "rds")
  files <- fs::dir_ls(path = dir, recurse = TRUE, glob = "*.rds")
  expect_equal(length(files), 30)
  expect_snapshot(get_hathi_counts(poetry$htid[1], dir = dir, cache_format = "rds"))
})

test_that("Can cache to arrow", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  cache_htids(poetry$htid[1:10], dir = dir, cache_format = "feather")
  files <- fs::dir_ls(path = dir, recurse = TRUE, glob = "*.feather")
  expect_equal(length(files), 10)
  expect_snapshot(get_hathi_counts(poetry$htid[1], dir = dir, cache_format = "feather"))
})

test_that("Can cache workset", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  workset <- poetry[ 1:10, ]
  class(workset) <- c("hathi_workset", class(workset))
  files <- fs::dir_ls(path = dir, recurse = TRUE, glob = "*.feather")
  fs::file_delete(files)
  cache_htids(workset, dir = dir, cache_format = "feather")
  files <- fs::dir_ls(path = dir, recurse = TRUE, glob = "*.feather")
  expect_equal(length(files), 10)
  expect_snapshot(get_hathi_counts(poetry$htid[1], dir = dir, cache_format = "feather"))
})


unlink(dir, recursive = TRUE, force = TRUE)
unlink(dir)
