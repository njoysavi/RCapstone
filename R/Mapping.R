# library(dplyr)
# library(leaflet)
# library(lubridate)

#' @title Plot a leaflet map
#'
#' @description We take a subset of our earthquake data and plot this on a map
#' using leaflet. See the leaflet docs (?leaflet) for a few relevant examples.
#' The input dataframe requires columns including LONGITUDE, LATITUDE, EQ_PRIMARY.
#' The annotation column ('annot_col') is also a required argument (no defaults).
#'
#' @param df a dataframe
#' @param annot_col annotation column
#'
#' @return an interactive leaflet map
#'
#' @import leaflet
#' @import dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' NOAA_df<-eq_clean_data(loaded_data)
#' df <- NOAA_df %>% filter(COUNTRY %in% c("MEXICO"), YEAR > 2000)
#' map1<-eq_map(df,annot_col = "date")
#' print(map1)
#' }
#'
eq_map <- function(df, annot_col){
  lmap <- df %>% leaflet::leaflet() %>% leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng=df$LONGITUDE,
                              lat=df$LATITUDE,
                              radius=df$EQ_PRIMARY,
                              popup=df[[annot_col]],
                              color="blue",
                              weight=1,
                              opacity=0.5)
}

#' @title Create earthquake labels
#'
#' @description This function is used tp create earthquake labels
#'
#' @param df a dataframe
#'
#' @return Returns a character vector
#'
#' @import dplyr
#' @import lubridate
#'
#' @export
#'
#' @examples
#' \dontrun{
#' NOAA_df<-eq_clean_data(loaded_data)
#' df <- NOAA_df %>% filter(COUNTRY %in% c("MEXICO"), YEAR > 2000)
#' map2<-df %>% dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#' print(map2)
#' }
#'
eq_create_label <- function(df){
  len <- length(df$CLEAN_LOCATION_NAME)
  locations <- df$CLEAN_LOCATION_NAME
  magnitude <- df$EQ_PRIMARY
  deaths <- df$DEATHS

  ptxt <- rep("", len)
  for(i in 1:len){
    txt <- paste0("<b>Location: </b>", locations[i], "</br>",
                  "<b>Magnitude: </b>", magnitude[i], "</br>",
                  "<b>Total Deaths: </b>", deaths[i])
    ptxt[i] <- txt
  }
  return(ptxt)
}
