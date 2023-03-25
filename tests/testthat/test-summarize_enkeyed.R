set.seed(123)
df <- data.frame(
  group = rep(c("A", "B"), each = 10),
  value = rnorm(20)
)

test_that("output checks for vectorized operations", {
  qtile <- summarize_enkeyed(df$value, func = quantile())

  expect_type(qtile, "list")
  expect_equal(dim(qtile[[1]]), c(1, 4))
  expect_identical(colnames(qtile[[1]]), c("25%", "50%", "75%", "quantile"))
})
