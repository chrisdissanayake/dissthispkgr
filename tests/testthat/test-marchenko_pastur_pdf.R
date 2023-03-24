test_that("mp pdf sanity check for wrapper transpose", {
  expect_equal(c(100, 2), dim(marchenko_pastur_pdf(variance = 1, q = 1/2, pts = 100)))
})
