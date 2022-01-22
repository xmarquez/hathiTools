test_that("Worksets with single and multiple tokens can be produced", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  workset1 <- workset_builder(c("liberal", "deliberative", "democracy"))
  expect_true(!is.null(workset1))
  workset2 <- workset_builder(c("liberal", "democracy"), max_vols = 20)
  expect_true(!is.null(workset2))
  workset3 <- workset_builder("liberal democracy", volumes_only = FALSE)
  expect_true(!is.null(workset3))
  expect_true(length(unique(workset3$htid)) >= nrow(workset1))
  expect_true(nrow(dplyr::inner_join(workset1, workset3)) > 0)
  }
)

test_that("Workset can be produced with different combinations of language, name, genre, and publication date", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  workset5 <- workset_builder(token = "démocratie", lang = "fr", name = "Alexis de Tocqueville")
  expect_true(!is.null(workset5))

  workset6 <- workset_builder(lang = "fre", name = "Alexis de Tocqueville")
  expect_true(!is.null(workset6))
  expect_true(nrow(workset5) > nrow(workset6))

  workset7 <- workset_builder(lang = "French", name = "Alexis de Tocqueville")
  expect_true(nrow(workset7) ==  nrow(workset6))

  workset8 <- workset_builder(lang = NULL, name = "Alexis de Tocqueville")
  expect_true(nrow(workset8) > 400)
  expect_true(nrow(workset8) > nrow(workset7))

  workset9 <- workset_builder(lang = "All languages", name = "Alexis de Tocqueville")
  expect_true(nrow(workset9) == nrow(workset8))

  workset10 <- workset_builder(name = "Alexis de Tocqueville")
  expect_true(nrow(workset10) == nrow(workset9))

  workset11 <- workset_builder(name = "Alexis de Tocqueville", genre = c("Not fiction", "Biography"))
  expect_true(nrow(workset10) > nrow(workset11))

  workset12 <- workset_builder(name = "Tocqueville")
  expect_true(nrow(workset12) > nrow(workset10))

  workset13 <- workset_builder(name = "Alexis de Tocqueville", imprint = "New York")
  expect_true(nrow(workset13) > 0)

  workset14 <- workset_builder(name = "Alexis de Tocqueville", pub_date = 1800:1850)
  expect_true(nrow(workset14) > 0 && nrow(workset14) < 100)
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
