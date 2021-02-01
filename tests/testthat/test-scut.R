context("scut")
scutenv <- as.environment("package:scutr")
oversamplers <- c(ls(envir=scutenv, pattern='oversample.*'), "resample.random")
undersamplers <- c(ls(envir=scutenv, pattern='undersample.*'), "resample.random")
for (osamp in oversamplers){
    for (usamp in undersamplers){
        scutted <- SCUT(wine, "type", oversample=get(osamp), undersample=get(usamp))
        test_that(paste(osamp, usamp, "have equal class distribution"), {
            expect_true(balanced.table(scutted$type))
        })
    }
}

# dummy func that follows the required signature
foo <- function(data, cls, cls.col, m){
    subset <- data[data[[cls.col]] == cls, ]
    subset[rep(1, m), ]
}
scutted <- SCUT(wine, "type", undersample=foo)
test_that("Custom functions can be passed", {
    expect_true(strictly.balanced(scutted$type))
})

scutted <- SCUT(wine, "type", undersample=undersample.kmeans, usamp.opts = list(k=7))
test_that("Custom arguments are passed correctly", {
    expect_true(strictly.balanced(scutted$type))
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
bad$x1 <- as.numeric(bad$x1)
bad$x1[1] <- NA
test_that("NAs are not allowed", {
    expect_error(SCUT(bad, "type"),
                 regexp="Data frame cannot contain NAs.")
})
