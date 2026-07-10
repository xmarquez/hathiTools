test_that("hash_fun is deterministic and returns a signed unit vector", {
  h1 <- hash_fun("democracy", dims = 160)
  h2 <- hash_fun("democracy", dims = 160)

  expect_length(h1, 160)
  expect_identical(h1, h2)
  expect_true(all(h1 %in% c(-1, 1)))
})

test_that("hash_fun honours the requested number of dimensions", {
  expect_length(hash_fun("token", dims = 32), 32)
  expect_length(hash_fun("token", dims = 200), 200)

  # The first 160 dims of a >160 request reuse the base 160-dim hash
  expect_identical(hash_fun("token", dims = 200)[1:160],
                   hash_fun("token", dims = 160))
})

test_that("hash_tokens returns one column per token", {
  m <- hash_tokens(c("a", "b", "c"), dims = 16)
  expect_equal(dim(m), c(16, 3))
  expect_equal(colnames(m), c("a", "b", "c"))
})

test_that("log_transform is non-negative and length preserving", {
  out <- log_transform(c(1, 10, 100, 1000))
  expect_length(out, 4)
  expect_true(all(out >= 0))
})

test_that("srp projects an extracted-features tibble to a numeric vector", {
  ef <- tibble::tibble(token = c("democracy", "monarchy", "republic"),
                       POS = c("NN", "NN", "NN"),
                       count = c(5, 3, 2))

  v_pos <- srp(ef, dims = 64, ignore_pos = TRUE, log = FALSE)
  expect_length(v_pos, 64)
  expect_true(is.numeric(v_pos))

  v_nopos <- srp(ef, dims = 64, ignore_pos = FALSE, log = FALSE)
  expect_length(v_nopos, 64)
})
