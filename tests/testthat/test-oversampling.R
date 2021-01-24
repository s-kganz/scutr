context("oversampling")
set.seed(1234)
rand <- data.frame(cbind(runif(100), as.factor(c(rep(1, 25), rep(2, 75)))))
r_m <- sum(rand$X2 == 2) * 2
w_m <- sum(wine$type == 1) * 2
w_cls <- 1
for (func in c(oversample.smote)) {
    oversamp <- func(wine, w_cls, "type", w_m)
    rownames(oversamp) <- 1:nrow(oversamp)
    test_that("Oversampling produces desired number of rows", {
        expect_equal(nrow(oversamp), w_m)
    })
    test_that("All rows in the oversampled dataset are of the right class", {
        expect_true(all(oversamp$type == w_cls))
    })
}
