context("scut")
scutenv <- as.environment("package:scutr")
oversamplers <- ls(envir=scutenv, pattern='oversample.*')
undersamplers <- ls(envir=scutenv, pattern='undersample.*')
for (osamp in oversamplers){
    for (usamp in undersamplers){
        scutted <- SCUT(wine, "type", oversample=get(osamp), undersample=get(usamp))
        counts <- table(scutted$type)
        test_that(paste(osamp, usamp, "have equal class distribution"), {
            expect_true(all(counts == counts[[1]]))
        })
    }
}

# dummy func that follows the required signature
foo <- function(data, cls, cls.col, m){
    subset <- data[data[[cls.col]] == cls, ]
    subset[rep(1, m), ]
}
scutted <- SCUT(wine, "type", undersample=foo)
test_that("Custom functions work", {
    counts <- table(scutted$type)
    expect_true(all(counts == counts[[1]]))
})

scutted <- SCUT(wine, "type", undersample=undersample.kmeans, usamp.opts = list(k=7))
test_that("Custom arguments are passed correctly", {
    counts <- table(scutted$type)
    expect_true(all(counts == counts[[1]]))
    expect_error(SCUT(wine, "type", osamp.opts=list(k=7)),
                 regexp="unused argument*")
})

context("bad input")
bad <- wine
bad$x1 <- as.character(bad$x1)
test_that("Non numeric columns are not allowed", {
    expect_error(SCUT(bad, "type"),
                 regexp="*Data frame must be only numeric*")
})
test_that("Class column must be present", {
    expect_error(SCUT(bad, "foo"),
                 regexp="Column not found in data*")
})
