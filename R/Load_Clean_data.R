#  COURSERA Project: Capstone Project
#  File: Load_Clean_data.R
#  email: njoysavi@gmail.com
#  Start: March 2020
#  End:   April 2020


# file_path="data_raw/signif.txt"
# loaded_data<-readr::read_tsv(file_path)

#library(readr)
#library(magrittr)
#library(dplyr)
#library(stringr)

# download NOAA data from NOAA website.
# there are many fields with NAs

# Instructions:
# In the below functions we are doing the following things:
# 1. Loading the data into a dataframe
# 2. Cleaning the data and ficing date fields
# 3. Fixing location name field and also converting latitude and longitude fields


#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#' @title Loads raw data
#'
#' @description Loads raw data from a file folder
#'
#'
#' @return Returns a dataframe result after calling read_delim
#'
#' @importFrom readr read_delim
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   NOAA_df_raw<-load_data()
#' }
#'
#'
# load_data<-function(path=file.path("data_raw", "signif.txt")){
#   # file_path="data_raw/signif.txt"
#   loaded_data<-readr::read_tsv(path)
#   return(loaded_data)
# }

load_data<-function(path){
  # file_path="data_raw/signif.txt"
  loaded_data<-readr::read_tsv(path)
  return(loaded_data)
}


#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

#' @title cleans the loaded data
#'
#' @description It takes the raw data as input and then does the following things: 1.
#' 1. Creates a date column using year month and day
#' 2. Converts Latitude and longitude columns into numeric
#' 3. Converts FLAG_TSUNAMI column
#' 4. Calls eq_location_clean to clean the location column
#'
#' @param raw_data datafram of raw NOAA dataset
#'
#' @return Returns cleaned dataframe
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' NOAA_df_raw<-load_data()
#' NOAA_df<-eq_clean_data(NOAA_df_raw)
#' }
#'
#'
eq_clean_data<-function(raw_data){
  clean_df <- raw_data %>%
    dplyr::mutate(DATE = as.Date(paste0(YEAR,'-',MONTH,'-',DAY), format="%Y-%m-%d")) %>%
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE)) %>%
    dplyr::mutate(FLAG_TSUNAMI = as.factor(FLAG_TSUNAMI))
  # clean_df2<-eq_clean_location(clean_df)
  clean_df2<-eq_location_clean(clean_df)
  return(clean_df2)
}

#' @title Cleans the LOCATION_NAME column
#'
#' @description Cleans the location name column and strips out unwanted
#' characters
#'
#' @param df a dataframe
#'
#' @return Returns a cleaned dataframe with modified Location Name column
#'
#' @import dplyr
#' @import stringr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   df_location <- load_data() %>% eq_location_clean()
#' }
#'
eq_location_clean <- function(df){
  df <- df %>% dplyr::mutate(CLEAN_LOCATION_NAME = stringr::str_trim(gsub(".*:","", LOCATION_NAME))) %>%
    dplyr::mutate(CLEAN_LOCATION_NAME = stringr::str_to_title(CLEAN_LOCATION_NAME))
  return(df)
}

