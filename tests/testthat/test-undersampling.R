context("general undersampling")
set.seed(1234)
rand <- data.frame(cbind(runif(100), as.factor(c(rep(1, 25), rep(2, 75)))))
w_m <- sum(wine$type == 1) %/% 2
r_m <- sum(rand$X2 == 2) %/% 2
for (func in c(undersample.kmeans, undersample.mindist, undersample.mclust, undersample.hclust,
               undersample.tomek)) {
    subset <- func(wine, 1, "type", w_m)
    rownames(subset) <- 1:nrow(subset)
    test_that("Undersampling produces desired number of rows", {
        expect_equal(nrow(subset), w_m)
    })
    test_that("All rows in undersampled dataset are of the right class", {
        expect_true(all(subset$type == 1))
    })
    test_that("All rows in undersampled dataset are in the original dataset", {
        expect_true(all(apply(subset, 1,
            function(row) {
                any(apply(wine, 1, function(row2) {all(row == row2)}))
            }
        )))
    })
    rand_subset <- func(rand, 2, "X2", r_m)
    test_that("One-dimensional datasets work",{
        expect_equal(nrow(rand_subset), r_m)
        expect_true(all(rand_subset$X2 == 2))
    })
}
