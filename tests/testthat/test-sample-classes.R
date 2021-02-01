context("test cluster sampling")
set.seed(1234)

# Whole bunch of random inputs
for (limit in 1:10){
    for (try in 1:10){
        classes <- sample(1:limit, 100, replace=T)
        samp <- classes[sample.classes(classes, max(try, limit))]
        test_that("cluster sampling produces equal sampling", {
            expect_true(loosely.balanced(samp))
        })
    }
}

# Test classes that only show up once
vec <- c(1, 2, rep(3, 97))
test_that("classes with one instance are sampled correctly", {
    samp <- vec[sample.classes(vec, 30)]
    expect_true(loosely.balanced(samp))
})
