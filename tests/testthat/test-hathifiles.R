dir <- "./raw-hathifiles"

test_that("Hathifile Update can be downloaded", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  hf <- download_hathifile(dir = dir, full_catalog = FALSE)
  expect_true(file.exists(hf))
  expect_snapshot(hf)
})

test_that("Hathifile can be loaded", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  hf <- download_hathifile(dir = dir, full_catalog = FALSE) %>%
    load_raw_hathifile(dir = dir,
                       cols = c("htid","rights_date_used", "author", "title", "imprint"))
  expect_snapshot(hf)
})

test_that("Imputed date is properly calculated", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  hf <- download_hathifile(dir = dir, full_catalog = FALSE) %>%
    load_raw_hathifile(dir = dir,
                       cols = c("htid", "rights_date_used", "imprint"))
  hf_imputed <- hf %>%
    add_imputed_date() %>%
    dplyr::select(-htid)
  expect_snapshot(hf_imputed)
})
unlink(dir, recursive = TRUE, force = TRUE)
unlink(dir)
