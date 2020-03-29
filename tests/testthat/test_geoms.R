library("RCapstone")

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
test_that("geom_timeline runs correctly", {
  test_data_path <- system.file("extdata", "signif.txt", package="RCapstone")
  df <-load_data(test_data_path) %>% eq_clean_data() %>% dplyr::filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
  gg_plot <-ggplot(df, aes(x = DATE, y = COUNTRY,
                                  color = as.numeric(TOTAL_DEATHS),
                                  size = as.numeric(EQ_PRIMARY),
                                  label = CLEAN_LOCATION_NAME)) +
      geom_timeline() +
      labs(size = "Richter scale value", color = "# deaths") +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     legend.position = "bottom",
                     axis.title.y = ggplot2::element_blank()) +
      ggplot2::xlab("DATE")

  expect_is(gg_plot, "gg")
  expect_is(gg_plot, "ggplot")
})

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
test_that("geom_timeline_label runs correctly", {
  test_data_path <- system.file("extdata", "signif.txt", package="RCapstone")
  df <-load_data(test_data_path) %>% eq_clean_data() %>% dplyr::filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
  gg_plot <-   ggplot(df , aes(x = DATE, y = COUNTRY,
                                   color = as.numeric(TOTAL_DEATHS),
                                   size = as.numeric(EQ_PRIMARY),
                                   label = CLEAN_LOCATION_NAME)) +
      geom_timeline() +
      labs(size = "Richter scale value", color = "# deaths") +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     legend.position = "bottom",
                     axis.title.y = ggplot2::element_blank()) +
      ggplot2::xlab("DATE") +
      geom_timeline_label(data=df)

  expect_is(gg_plot, "gg")
  expect_is(gg_plot, "ggplot")
})
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
