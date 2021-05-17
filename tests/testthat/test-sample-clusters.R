context("cluster sampling")

# Whole bunch of random inputs
for (limit in 1:10) {
  for (try in 1:10) {
    classes <- sample(1:limit, 100, replace = T)
    samp <- classes[sample_classes(classes, max(try, limit))]
    test_that("cluster sampling produces equal sampling", {
      expect_true(loosely_balanced(samp))
    })
  }
}

vec <- c(1, 2, rep(3, 97))
test_that("classes with one instance are sampled correctly", {
  samp <- vec[sample_classes(vec, 30)]
  expect_true(loosely_balanced(samp))
})

vec <- 1:10
test_that("case where classes must be dropped trims output", {
  samp <- vec[sample_classes(vec, 7)]
  expect_true(loosely_balanced(samp))
})
