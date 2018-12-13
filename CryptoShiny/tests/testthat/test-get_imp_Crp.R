test_that("Type of elements dataset:", {
  datasetToTest <- get_imp_Crp()
  expect_is(datasetToTest, "character")
})
