test_that("Worksets with single and multiple tokens can be produced", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  workset1 <- workset_builder(c("liberal", "deliberative", "democracy"), verbose = TRUE)
  expect_snapshot(workset1)
  workset2 <- workset_builder(c("liberal", "democracy"), max_vols = 20, verbose = TRUE)
  expect_snapshot(workset2)
  workset3 <- workset_builder("liberal democracy", volumes_only = FALSE, verbose = TRUE)
  expect_snapshot(workset3)
  workset4 <- workset_builder(c("liberal", "democracy"), volumes_only = FALSE, verbose = TRUE)
  expect_snapshot(workset4)
  expect_true(length(unique(workset3$htid)) <= nrow(workset4))
  expect_true(nrow(dplyr::inner_join(workset1, workset3)) > 0)
  }
)

test_that("Workset can be produced with different combinations of language, name, genre, and publication date", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  workset5 <- workset_builder(token = "démocratie", lang = "fr", name = "Alexis de Tocqueville")
  expect_snapshot(workset5)

  workset6 <- workset_builder(lang = "fre", name = "Alexis de Tocqueville")
  expect_snapshot(workset6)
  expect_true(nrow(workset6) > nrow(workset5))

  workset7 <- workset_builder(lang = "French", name = "Alexis de Tocqueville")
  expect_true(nrow(workset7) ==  nrow(workset6))

  workset8 <- workset_builder(lang = NULL, name = "Alexis de Tocqueville")
  expect_snapshot(workset8)
  expect_true(nrow(workset8) > nrow(workset7))

  workset9 <- workset_builder(lang = "All languages", name = "Alexis de Tocqueville")
  expect_true(nrow(workset9) == nrow(workset8))

  workset10 <- workset_builder(name = "Alexis de Tocqueville", verbose = TRUE)
  expect_true(nrow(workset10) == nrow(workset9))

  workset11 <- workset_builder(query = paste('(volumecontributorName_txt:("Alexis"))',
                                             'AND (volumecontributorName_txt:("de"))',
                                             'AND (volumecontributorName_txt:("Tocqueville"))'),
                               verbose = TRUE)
  expect_identical(workset10, workset11)

  workset12 <- workset_builder(name = "Tocqueville")
  expect_true(nrow(workset12) > nrow(workset10))

  workset13 <- workset_builder(name = "Alexis de Tocqueville", imprint = "Longmans", verbose = TRUE)
  expect_true(nrow(workset13) > 0)

  workset14 <- workset_builder(name = "Alexis de Tocqueville", pub_date = 1800:1850)
  expect_true(nrow(workset14) > 0 && nrow(workset14) < 100)

  expect_error(workset_builder(lang = c("en", "fr"),
                               name = "Alexis de Tocqueville",
                               pub_date = 1800:1850))
  expect_error(workset_builder("democracy",
                               lang = c("en", "fr"),
                               name = "Alexis de Tocqueville",
                               pub_date = 1800:1850))
}
)

test_that("Language function finds language", {
  lang <- find_language("English")
  expect_equal(lang, "en")

  lang <- find_language(NULL)
  expect_equal(lang, "alllangs")

  lang <- find_language("alllangs")
  expect_equal(lang, "alllangs")

  lang <- find_language("eng")
  expect_equal(lang, "en")

  lang <- find_language("fre", "alpha3-b")
  expect_equal(lang, "fre")
})

test_that("We can download metadata", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  workset1 <- workset_builder(name = "Alexis de Tocqueville", imprint = "Longmans",
                               verbose = TRUE)
  meta <- get_workset_meta(workset1, metadata_dir = tempdir())
  expect_snapshot(meta)

})
