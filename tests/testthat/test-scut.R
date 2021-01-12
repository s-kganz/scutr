data("wine")
wine$type <- as.factor(wine$type)

context("Undersampling")
m <- 50
undersamp <- .do.undersample(wine, "mclust", "type", 2, m)
test_that("mclust produces correct number of samples", {
    expect_equal(nrow(undersamp), m)
})
undersamp <- .do.undersample(wine, "random", "type", 2, m)
test_that("random produces correct number of samples", {
    expect_equal(nrow(undersamp), m)
})

context("Oversampling")
m <- 70
oversamp <- .do.oversample(wine, "SMOTE", "type", 3, m)
test_that("SMOTE produces correct number of samples", {
    expect_equal(nrow(oversamp), m)
})
oversamp <- .do.oversample(wine, "random", "type", 3, m)
test_that("SMOTE produces correct number of samples", {
    expect_equal(nrow(oversamp), m)
})
