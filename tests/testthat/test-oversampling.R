context("general oversampling")
for (w_cls in unique(wine$type)) {
  w_m <- sum(wine$type == w_cls) * 2
  for (oversampler in c(ls(envir = as.environment("package:scutr"), pattern = "oversample*"))) {
    func <- get(oversampler)
    oversamp <- func(wine, w_cls, "type", w_m)
    test_that("Oversampling produces desired number of rows", {
      expect_equal(nrow(oversamp), w_m)
    })
    test_that("All rows in the oversampled dataset are of the right class", {
      expect_true(all(oversamp$type == w_cls))
    })
    test_that("All original rows present, duplicates are not introduced if none in original", {
      expect_true(nrow(merge(oversamp, wine)) >= sum(wine$type == w_cls))
      if (no_dupe_rows(wine)) {
        expect_true(no_dupe_rows(oversamp))
      }
    })
  }
}
