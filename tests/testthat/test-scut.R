context("scut")
scutted <- SCUT(type ~ ., wine)
test_that("SCUT results have equal class distribution", {
    counts <- table(scutted$type)
    expect_true(all(counts == counts[[1]]))
})
# speed it up w/ kmeans in the parallel version
scutted <- SCUT.parallel(class ~ ., imbalance, undersample="kmeans", ncores=2)
test_that("SCUT.parallel results have equal class distribution", {
    counts <- table(scutted$class)
    expect_true(all(counts == counts[[1]]))
})

# dummy func that follows the required signature
foo <- function(data, cls, cls.col, m){
    subset <- data[data[[cls.col]] == cls, ]
    subset[rep(1, m), ]
}
scutted <- SCUT(type ~ ., wine, undersample=foo)
test_that("Custom functions are valid with SCUT", {
    counts <- table(scutted$type)
    expect_true(all(counts == counts[[1]]))
})
