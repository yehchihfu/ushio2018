context("test extract causality")
library(dplyr)

test_that("test that rows of output matched", {
  d_bestE0 <- read.csv(testthat::test_path("example_data", "Maizuru_ccm_res.csv"))

  expect_equal(unlist(extract_causality(d_bestE0)[1,],use.names = F), c(1,1,11))
  expect_equal(unlist(extract_causality(d_bestE0)[2,],use.names = F), c(1,5,11))
  expect_equal(unlist(extract_causality(d_bestE0)[10,],use.names = F), c(6,6,24))
})
