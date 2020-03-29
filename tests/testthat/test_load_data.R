library("RCapstone")

test_that("Data loads cleaned correctly", {
  test_data_path <- system.file("extdata", "signif.txt", package="RCapstone")
  df <- load_data(test_data_path) %>% eq_clean_data()
  expect_is(df, "data.frame")        # the result will be a dataframe
  expect_is(df$LATITUDE, "numeric")  # that each of LATITUDE and LONGITUDE are numeric
  expect_is(df$LONGITUDE, "numeric")
})
