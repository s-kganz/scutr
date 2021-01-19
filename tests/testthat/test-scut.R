wine$type <- as.factor(wine$type)

imbalance <- imbalance[imbalance$class %in% c(1, 2, 10), ]

context("Undersampling")
m <- 50
undersamp <- do.undersample(wine, "mclust", "type", 2, m)
test_that("mclust produces correct number of samples", {
    expect_equal(nrow(undersamp), m)
})
undersamp <- do.undersample(wine, "random", "type", 2, m)
test_that("random produces correct number of samples", {
    expect_equal(nrow(undersamp), m)
})
foo <- function(data, cls, cls.col, m){
    data[1:m, ]
}
undersamp <- do.undersample(wine, foo, "type", 2, m)
test_that("custom functions work",{
    expect_equal(nrow(undersamp), m)
})

context("Oversampling")
m <- 70
oversamp <- do.oversample(imbalance, "SMOTE", "class", 2, 70)
test_that("SMOTE produces correct number of samples", {
    expect_equal(nrow(oversamp), m)
})
oversamp <- do.oversample(wine, "random", "type", 3, m)
test_that("random produces correct number of samples", {
    expect_equal(nrow(oversamp), m)
})
foo <- function(data, cls, cls.col, m){
    data[rep(1, m), ]
}
oversamp <- do.oversample(wine, foo, "type", 3, m)
test_that("custom functions work", {
    expect_equal(nrow(oversamp), m)
})
