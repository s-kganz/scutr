context("scut")
scutenv <- as.environment("package:scutr")
oversamplers <- ls(envir=scutenv, pattern='oversample.*')
undersamplers <- ls(envir=scutenv, pattern='undersample.*')
for (osamp in oversamplers){
    for (usamp in undersamplers){
        scutted <- SCUT(type ~ ., wine, oversample=get(osamp), undersample=get(usamp))
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
test_that("Custom functions work", {
    counts <- table(scutted$type)
    expect_true(all(counts == counts[[1]]))
})

scutted <- SCUT(type ~ ., wine, undersample=undersample.kmeans, usamp.opts = list(k=7))
test_that("Custom arguments are passed correctly", {
    counts <- table(scutted$type)
    expect_true(all(counts == counts[[1]]))
    expect_error(SCUT(type ~ ., wine, osamp.opts=list(k=7)))
})
