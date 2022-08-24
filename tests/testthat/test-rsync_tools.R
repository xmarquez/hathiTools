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

unlink(dir, recursive = TRUE, force = TRUE)
unlink(dir)
