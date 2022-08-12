test_that("Bookworm query of single or multiple terms works", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  res1 <- query_bookworm("democracy")
  expect_s3_class(res1, c("tbl", "tbl_df"))
  expect_true(min(res1$date_year) == 1920)
  expect_false("democracy" %in% names(res1))
  expect_true(sum(res1$value) > 0)
  expect_snapshot(res1)
  res2 <- query_bookworm(c("democracy", "dictatorship"))
  expect_true(nrow(res2) == 2*nrow(res1))
  expect_true(all(c("democracy", "dictatorship") %in% res2$word))
  expect_snapshot(res2)
  res3 <- query_bookworm(c("liberal democracy"))
  expect_true(sum(res3$value) == 0)
  expect_snapshot(res3)
  expect_snapshot_value(res1, style = "serialize")
  expect_snapshot_value(res2, style = "serialize")
  expect_snapshot_value(res3, style = "serialize")

})

test_that("Bookworm query for different count types works", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  res1 <- query_bookworm("democracy",
                         counttype = c("WordsPerMillion", "WordCount",
                                       "TotalWords", "WordsRatio",
                                       "SumWords"))
  expect_true(nrow(res1) == 5*length(1920:2000))
  res1 <- query_bookworm("democracy",
                         counttype = c("TextCount", "TextPercent",
                                       "TotalTexts", "SumTexts"))
  expect_true(nrow(res1) == 4*length(1920:2000))
  res2 <- query_bookworm("democracy", counttype = "WordsRatio")
  res3 <- query_bookworm("democracy", counttype = "WordsRatio", compare_to = "dictatorship")
  expect_true(all(res3$value > res2$value))
  expect_warning(query_bookworm("democracy", counttype = "TextCount", compare_to = "dictatorship"))
  expect_snapshot(res1)
  expect_snapshot(res2)
  expect_snapshot(res3)
  expect_snapshot_value(res1, style = "serialize")
  expect_snapshot_value(res2, style = "serialize")
  expect_snapshot_value(res3, style = "serialize")
})

test_that("Bookworm query with method returnPossibleFields returns expected fields", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  res1 <- query_bookworm(method = "returnPossibleFields")
  expect_true(nrow(res1) == 17)
  expect_snapshot(res1)
  expect_snapshot_value(res1, style = "serialize")
})

test_that("Bookworm query with method search_results returns a small workset", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  res1 <- query_bookworm("democracy", method = "search_results")
  expect_true(nrow(res1) == 100)
  expect_s3_class(res1, c("hathi_workset", "tbl", "tbl_df"))
  expect_snapshot(res1)
  expect_snapshot_value(res1, style = "serialize")
})

test_that("Bookworm query accepts a variety of grouping vars", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  res1 <- query_bookworm("democracy", groups = c("languages", "publication_country"))
  expect_true("United States" %in% res1$publication_country)
  expect_true("lat" %in% res1$languages)
  res2 <- query_bookworm("democracy", groups = c("publication_state", "fiction_nonfiction"))
  expect_true(all(c("N/A", "Alabama") %in% res2$publication_state))
  expect_true(all(c("Fiction", "Not fiction") %in% res2$fiction_nonfiction))
  res3 <- query_bookworm(groups = c("page_count_bin", "is_gov_doc"),
                         counttype = c("TotalTexts"))
  expect_true(nrow(res3) == 4)
  expect_true(all(c("page_count_bin", "is_gov_doc") %in% names(res3)))
  res4 <- query_bookworm(groups = c("is_gov_doc"),
                         counttype = c("TotalTexts"),
                         lims = c(1700, 2020))
  expect_true(nrow(res4) == 1)
  res5 <- query_bookworm(groups = c("date_year", "htsource"),
                        counttype = c("TextPercent"))
  expect_true(all(res5$value == 100))
  res6 <- query_bookworm(groups = c("date_year", "*htsource"),
                         counttype = c("TextPercent"))
  expect_false(all(res6$value == 100))
  res7 <- query_bookworm(groups = c("*date_year", "*htsource"),
                         counttype = c("TextPercent"))
  expect_snapshot(res1)
  expect_snapshot_value(res1, style = "serialize")
  expect_snapshot(res2)
  expect_snapshot_value(res2, style = "serialize")
  expect_snapshot(res3)
  expect_snapshot_value(res3, style = "serialize")
  expect_snapshot(res4)
  expect_snapshot_value(res4, style = "serialize")
  expect_snapshot(res5)
  expect_snapshot_value(res5, style = "serialize")
  expect_snapshot(res6)
  expect_snapshot_value(res6, style = "serialize")
  expect_snapshot(res7)
  expect_snapshot_value(res7, style = "serialize")

})

test_that("Bookworm query can use a specific var as limit", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  res1 <- query_bookworm(c("democracy", "dictatorship"),
                         fiction_nonfiction = "Fiction",
                         counttype = c("WordsPerMillion", "WordCount"),
                         groups = c("fiction_nonfiction"))
  expect_error(query_bookworm(c("democracy", "dictatorship"),
                              fiction_nonfiction = "Not fiction",
                              groups = c("format"),
                              counttype = c("WordsPerMillion", "WordCount")))
  expect_error(query_bookworm(c("democracy", "dictatorship"),
                         somethingNotInField = "Not fiction",
                         groups = c("contributing_library", "is_gov",
                                    "literary_form", "scanner", "resource_type"),
                         counttype = c("WordsPerMillion", "WordCount")))
  res3 <- query_bookworm("democracy", date_year = 1945,
                         groups = c("date_year",
                                    "lc_classes",
                                    "genres",
                                    "htsource"),
                         counttype = c("WordsPerMillion",
                                       "WordCount"))
  expect_true(all(c("date_year", "lc_classes", "genres", "htsource") %in% names(res3)))
  expect_type(res3$date_year, "integer")
  res4 <- query_bookworm(mainauthor = c("Tocqueville, Alexis de, 1805-1859.",
                                        "Tocqueville, Alexis de, 1805-1859"),
                         groups = "mainauthor",
                         counttype = c("TotalWords"),
                         lims = c(1800, 2000))
  expect_true(nrow(res4) == 1)
  expect_true(all(res4$value > 0))
  res5 <- query_bookworm(mainauthor = c("Tocqueville, Alexis de, 1805-1859.",
                                        "Tocqueville, Alexis de, 1805-1859"),
                         groups = c("date_year", "mainauthor"),
                         counttype = c("TotalWords"),
                         lims = c(1800, 2000))
  expect_true(nrow(res5) == 88)
  res6 <- query_bookworm(mainauthor = c("Tocqueville, Alexis de, 1805-1859.",
                                        "Tocqueville, Alexis de, 1805-1859"),
                         date_year = c(1833, 1951),
                         groups = c("date_year", "mainauthor"),
                         counttype = c("TotalWords"),
                         lims = c(1800, 2000))
  expect_true(nrow(res6) == 2)
  expect_snapshot(res1)
  expect_snapshot_value(res1, style = "serialize")
  expect_snapshot(res3)
  expect_snapshot_value(res3, style = "serialize")
  expect_snapshot(res4)
  expect_snapshot_value(res4, style = "serialize")
  expect_snapshot(res5)
  expect_snapshot_value(res5, style = "serialize")
  expect_snapshot(res6)
  expect_snapshot_value(res6, style = "serialize")
})

test_that("Bookworm query can return a specific period", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  res1 <- query_bookworm(c("democracy", "dictatorship"),
                         fiction_nonfiction = "Not fiction",
                         counttype = c("WordsPerMillion", "WordCount"),
                         lims = c(1700,2000))
  expect_true(all(range(res1$date_year) == c(1700, 2000)))
  expect_snapshot(res1)
  expect_snapshot_value(res1, style = "serialize")
})

test_that("Bookworm query words for case sensitive and case insensitive search", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  res1 <- query_bookworm("democracy")
  res2 <- query_bookworm("democracy", ignore_case = FALSE)
  expect_true(all(res1$value >= res2$value))
  expect_snapshot(res1)
  expect_snapshot_value(res1, style = "serialize")
  expect_snapshot(res2)
  expect_snapshot_value(res2, style = "serialize")
})
