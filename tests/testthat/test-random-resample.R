context("random resampling")
count_1 <- sum(wine$type == 1)
for (try in 1:10) {
  test_that("Random undersampling should not have duplicate rows", {
    rand_resamp <- resample_random(wine, 1, "type", count_1 - 1)
    expect_true(no_dupe_rows(rand_resamp))
  })
}

for (try in 1:10) {
  test_that("Random oversampling should include all of the original data", {
    rand_resamp <- resample_random(wine, 1, "type", round(count_1 * 1.2))
    merged <- merge(rand_resamp, wine)
    nrow_merge <- nrow(merged[!duplicated(merged), ])
    expect_true(nrow_merge == count_1)
  })
}
