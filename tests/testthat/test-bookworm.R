test_that("Bookworm query of single or multiple terms works", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  res1 <- query_bookworm("democracy")
  expect_s3_class(res1, c("tbl", "tbl_df"))
  expect_true(min(res1$date_year) == 1920)
  expect_false("democracy" %in% names(res1))
  expect_true(sum(res1$value) > 0)
  res2 <- query_bookworm(c("democracy", "dictatorship"))
  expect_true(nrow(res2) == 2*nrow(res1))
  expect_true(all(c("democracy", "dictatorship") %in% res2$word))
  res3 <- query_bookworm(c("liberal democracy"))
  expect_true(sum(res3$value) == 0)

})

test_that("Bookworm query for different count types works", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  res1 <- query_bookworm("democracy",
                         counttype = c("WordsPerMillion", "WordCount",
                                       "TextPercent", "TextCount",
                                       "TotalWords", "WordsRatio",
                                       "SumWords", "TotalTexts"))
  expect_true(nrow(res1) == 8*length(1920:2000))
  expect_error(query_bookworm("democracy",
                              counttype = c("TextRatio")))
  expect_error(query_bookworm("democracy",
                              counttype = c("SumTexts")))
  res2 <- query_bookworm("democracy", counttype = "WordsRatio")
  res3 <- query_bookworm("democracy", counttype = "WordsRatio", compare_to = "dictatorship")
  expect_true(all(res3$value > res2$value))
  expect_warning(query_bookworm("democracy", counttype = "TextCount", compare_to = "dictatorship"))
})

test_that("Bookworm query with method returnPossibleFields returns expected fields", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  res1 <- query_bookworm(method = "returnPossibleFields")
  expect_true(nrow(res1) == 21)
})

test_that("Bookworm query with method search_results returns a small workset", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  res1 <- query_bookworm("democracy", method = "search_results")
  expect_true(nrow(res1) == 100)
  expect_s3_class(res1, c("hathi_workset", "tbl", "tbl_df"))
})

test_that("Bookworm query accepts a variety of grouping vars", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  res1 <- query_bookworm("democracy", groups = c("language", "publication_country"))
  expect_true("USA" %in% res1$publication_country)
  expect_true("Latin" %in% res1$language)
  res2 <- query_bookworm("democracy", groups = c("publication_state", "target_audience"))
  expect_true(all(c("N/A", "Alabama") %in% res2$publication_state))
  expect_true(all(c("Adult", "Preschool") %in% res2$target_audience))
  res3 <- query_bookworm(groups = c("contributing_library", "is_gov",
                                    "literary_form", "scanner", "resource_type"),
                         counttype = c("TotalTexts", "TotalWords"))
  expect_true(nrow(res3) > 1000)
  expect_type(res3$is_gov, "logical")
  expect_true(all(c("contributing_library", "is_gov",
                "literary_form", "scanner", "resource_type") %in% names(res3)))
})

test_that("Bookworm query can use a specific var as limit", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  res1 <- query_bookworm(c("democracy", "dictatorship"),
                         literary_form = "Not fiction",
                         groups = c("contributing_library", "is_gov",
                                    "scanner", "resource_type"),
                         counttype = c("WordsPerMillion", "WordCount"))
  res2 <- query_bookworm(c("democracy", "dictatorship"),
                         literary_form = "Not fiction",
                         groups = c("contributing_library", "is_gov",
                                    "literary_form", "scanner", "resource_type"),
                         counttype = c("WordsPerMillion", "WordCount"))
  expect_true(nrow(res1 %>% dplyr::anti_join(res2)) == 0)
  expect_error(query_bookworm(c("democracy", "dictatorship"),
                         somethingNotInField = "Not fiction",
                         groups = c("contributing_library", "is_gov",
                                    "literary_form", "scanner", "resource_type"),
                         counttype = c("WordsPerMillion", "WordCount")),
               regexp = "Database error. Try checking field names.")
  expect_warning(res3 <- query_bookworm("democracy", date_year = 1945,
                                        groups = c("date_year",
                                                   "first_author_birth",
                                                   "first_author_death",
                                                   "record_date_year"),
                                        counttype = c("WordsPerMillion",
                                                      "WordCount")))
  expect_type(res3$first_author_birth, "integer")
  expect_type(res3$first_author_death, "integer")
  expect_type(res3$record_date_year, "integer")
  expect_type(res3$date_year, "integer")
  expect_true(1805 %in% res3$first_author_birth)
  res4 <- query_bookworm(first_author_name = c("Tocqueville, Alexis de, 1805-1859.",
                                               "Tocqueville, Alexis de, 1805-1859"),
                         groups = "first_author_name",
                         counttype = c("TotalTexts", "TotalWords"),
                         lims = c(1800, 2000))
  expect_true(nrow(res4) == 4)
  expect_true(all(res4$value > 0))
})

test_that("Bookworm query can return a specific period", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  res1 <- query_bookworm(c("democracy", "dictatorship"),
                         literary_form = "Not fiction",
                         counttype = c("WordsPerMillion", "WordCount"),
                         lims = c(1700,2000))
  expect_true(all(range(res1$date_year) == c(1700, 2000)))
  expect_error(query_bookworm(c("democracy", "dictatorship"),
                              literary_form = "Not fiction",
                              counttype = c("WordsPerMillion", "WordCount"),
                              lims = c(1700:2000)))
})
