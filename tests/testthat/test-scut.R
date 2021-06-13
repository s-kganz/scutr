context("scut")

# Get the core limit if we are running on R CMD check
# https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions
chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

if (nzchar(chk) && chk == "TRUE") {
  # use 2 cores in CRAN/Travis/AppVeyor
  ncores <- 2L
} else {
  # use all cores in devtools::test()
  ncores <- parallel::detectCores()
}

scutenv <- as.environment("package:scutr")
oversamplers  <- ls(envir = scutenv, pattern = "oversample_*")
undersamplers <- ls(envir = scutenv, pattern = "undersample_*")
n_oversamplers  <- length(oversamplers)
n_undersamplers <- length(undersamplers)

# Test each sampling method in turn, using random as the other method
oversamplers <- c(oversamplers, rep("resample_random", n_undersamplers))
undersamplers <- c(undersamplers, rep("resample_random", n_oversamplers))
if (length(oversamplers) != length(undersamplers)) {
  stop("Testing setup should have equal number of under/oversamplers.")
}

for (i in 1:length(undersamplers)) {
  osamp <- oversamplers[i]
  usamp <- undersamplers[i]

  test_that(paste(osamp, usamp, "have equal class distribution"), {
    scutted <-
      SCUT(wine,
           "type",
           oversample = get(osamp),
           undersample = get(usamp))
    expect_true(strictly_balanced(scutted$type))
  })

  test_that(paste(osamp, usamp, "(parallel) have equal class distribution"),
            {
              # Parallel version fails on windows if the package is not installed.
              # Since this is the case during testing, we skip the parallel version.
              skip_on_os("windows")


              scutted <- SCUT_parallel(
                wine,
                "type",
                ncores = ncores,
                oversample = get(osamp),
                undersample = get(usamp)
              )

              expect_true(strictly_balanced(scutted$type))
            })
}

# test custom functions
# dummy func that follows the required signature
foo <- function(data, cls, cls_col, m) {
  subset <- data[data[[cls_col]] == cls,]
  subset[rep(1, m),]
}
scutted <- SCUT(wine, "type", undersample = foo)
test_that("Custom functions can be passed", {
  expect_true(strictly_balanced(scutted$type))
})

scutted <-
  SCUT(wine,
       "type",
       undersample = undersample_kmeans,
       usamp_opts = list(k = 7))
test_that("Custom arguments are passed correctly", {
  expect_true(strictly_balanced(scutted$type))
  expect_error(SCUT(wine, "type", osamp_opts = list(k = 7)),
               regexp = "unused argument*")
})

context("bad input")
bad <- wine
bad$x1 <- as.character(bad$x1)
test_that("Non numeric columns are not allowed", {
  expect_error(SCUT(bad, "type"),
               regexp = "*Data frame must be only numeric*")
})
test_that("Class column must be present", {
  expect_error(SCUT(bad, "foo"),
               regexp = "Column not found in data*")
})
bad$x1 <- as.numeric(bad$x1)
bad$x1[1] <- NA
test_that("NAs are not allowed", {
  expect_error(SCUT(bad, "type"),
               regexp = "Data frame cannot contain NAs.")
})
