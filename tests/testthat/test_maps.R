library("RCapstone")
context("testing maps function")

test_that("eq_map is running", {
  test_data_path <- system.file("extdata", "signif.txt", package="RCapstone")
  NOAA_df <-load_data(test_data_path) %>% eq_clean_data() %>%
    dplyr::filter(COUNTRY =="MEXICO", YEAR > 2000)
  map1<-eq_map(NOAA_df,annot_col = "date")
  expect_is(map1, "leaflet")
  expect_is(map1, "htmlwidget")
})

