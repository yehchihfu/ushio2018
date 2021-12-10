context("test SmapCFunc function")
library(dplyr)


d_bestE0 <- read.csv(testthat::test_path("example_data", "Maizuru_ccm_res.csv"))
biw.data <- read.csv(testthat::test_path("example_data", "Maizuru_dominant_sp.csv"))
num.for.smapc <-extract_causality (d_bestE0)
d.name   <- names(biw.data)[4:18]
new.nums<- arrange_interactions(num.for.smapc)
test_set <-  new.nums[1:2, ]
test_output <- SmapCFunc( test_set, smapc.tp = 1, stats.output = T,
                          embedding = "best_E", original.data = biw.data)
test_that("test that rows of output matched", {

  expect_output(str(test_output ), "List of 2")
  expect_equal ( names(test_output), c("coefs", "stats"))
  expect_equal(names(test_output$stats), c("num_pred", "rho","mae",  "rmse"  )  )
})
