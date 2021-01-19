t1 <- wine[wine$type == 1, ]
subset <- undersample.mindist(t1, 1, "type", 20)
test_that("Undersampling makes the right number of samples", {
    expect_equal(nrow(subset), 20)
})

rand <- data.frame(cbind(runif(100), rep("foo", 100)))
subset <- undersample.mindist(rand, "foo", "X2", 20)
test_that("One dimensional datasets work", {
    expect_equal(nrow(subset), 20)
})
