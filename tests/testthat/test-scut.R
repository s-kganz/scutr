context("scut")
for (osamp in c("SMOTE", "random")){
    for (usamp in c("mclust", "kmeans", "hclust", "mindist", "tomek")){
        scutted <- SCUT(type ~ ., wine, oversample=osamp, undersample=usamp)
        counts <- table(scutted$type)
        test_that(paste(osamp, usamp, "have equal class distribution"), {
            expect_true(all(counts == counts[[1]]))
        })
        # skip mclust in parallel test because it's really slow
        # if (usamp == "mclust") next
        # scutted <- SCUT.parallel(type ~ ., wine, oversample=osamp, underample=usamp)
        # counts <- table(scutted$type)
        # test_that(paste(osamp, usamp, "have equal class distribution in parallel version"), {
        #     expect_true(all(counts == counts[[1]]))
        # })
    }
}

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

scutted <- SCUT(type ~ ., wine, undersample="kmeans", usamp.opts = list(k=7))
test_that("Custom arguments are passed correctly", {
    counts <- table(scutted$type)
    expect_true(all(counts == counts[[1]]))
    expect_error(SCUT(type ~ ., wine, osamp.opts=list(k=7)))
})
