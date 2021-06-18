context("Tomek undersampling")

# make a synthetic dataset
# both "b" points at x < 0.0 become Tomek links. The one at -0.1 is removed first
# followed by the one at -0.5
x1 <- runif(10, 0.5, 1)
x2 <- c(-0.1, -0.5, -0.1)
y <- c(runif(10), c(0.6, 0.4, 0.5))
class <- c(rep(1, 12), 2)
df <- data.frame(x = c(x1, x2), y = y, class = class)

if (interactive()) plot(df$x, df$y, col = df$class)

tomret <- undersample_tomek(df, "a", "class", 10, tomek = "diff")

test_that("undersampling removes tomeks", {
  expect_true(all(tomret$x > 0))
})

tomret <- undersample_tomek(df, "a", "class", 9, tomek = "diff", force_m = T)

test_that("undersampling removes tomeks first", {
  expect_true(all(tomret$x > 0))
})
