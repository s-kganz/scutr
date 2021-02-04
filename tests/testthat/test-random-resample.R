context("random resampling")
count_1 <- sum(wine$type == 1)
for (try in 1:10){
    test_that("Random undersampling should not have duplicate rows", {
        rand.resamp <- resample.random(wine, 1, "type", count_1-1)
        expect_true(no.dupe.rows(rand.resamp))
    })
}

for (try in 1:10){
    test_that("Random oversampling should include all of the original data", {
        rand.resamp <- resample.random(wine, 1, "type", round(count_1 * 1.2))
        merged <- merge(rand.resamp, wine)
        nrow_merge <- nrow(merged[!duplicated(merged), ])
        expect_true(nrow_merge == count_1)
    })
}
