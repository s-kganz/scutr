context("test cluster sampling")
set.seed(1234)

# Whole bunch of random inputs
for (limit in 1:10){
    for (try in 1:20){
        classes <- sample(1:limit, 100, replace=T)
        samp <- classes[sample.classes(classes, max(try, limit))]
        tab <- table(samp)
        test_that("cluster sampling produces equal sampling", {
            expect_true(all(tab == max(tab) | tab == max(tab)-1))
        })
    }
}

# Test classes that only show up once
vec <- c(1, 2, rep(3, 97))
test_that("classes with one instance are sampled right", {
    samp <- vec[sample.classes(vec, 30)]
    tab <- table(samp)
    expect_true(all(tab == max(tab) | tab == max(tab)-1))
})
