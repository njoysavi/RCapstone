
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#' @title geom_timeline function
#'
#' @description This function is used in addition to GeomTimeline
#' See the layout contained within the course materials p.g. 452
#' Here the layout of this function is very similar.
#' This geom looks to chart a timeline of earthquakes for a given country / countries
#' with points (in the example) representing earthquake events, point size indicating
#' earthquake magnitude and colour representing number of deaths. x (the date) is a
#' required aesthetic whereas y (country) is optional.
#'
#' @import ggplot2
#' @import dplyr
#' @import grid
#'
#' @export
#'
#' @examples
#' \dontrun{
#' NOAA_df_China_USA <- NOAA_df %>%
#'  filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
#' ggplot(NOAA_df_China_USA, aes(x = DATE, y = COUNTRY,
#'                               color = as.numeric(TOTAL_DEATHS),
#'                               size = as.numeric(EQ_PRIMARY),
#'                               label = CLEAN_LOCATION_NAME)) +
#'   geom_timeline() +
#'   labs(size = "Richter scale value", color = "# deaths") +
#'   ggplot2::theme(panel.background = ggplot2::element_blank(),
#'                  legend.position = "bottom",
#'                  axis.title.y = ggplot2::element_blank()) +
#'   ggplot2::xlab("DATE")
#' }

geom_timeline <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity", na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @title geom_timeline_label function
#'
#' @description This function is used to label the points in the graph
#'
#' @param n_max
#'
#' @import ggplot2
#' @import dplyr
#' @import grid
#'
#' @export
#'
#' @examples
#' \dontrun{
#' NOAA_df_China_USA <- NOAA_df %>%
#' filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
#' ggplot(NOAA_df_China_USA , aes(x = DATE, y = COUNTRY,
#'                                color = as.numeric(TOTAL_DEATHS),
#'                                size = as.numeric(EQ_PRIMARY),
#'                                label = CLEAN_LOCATION_NAME)) +
#'   geom_timeline() +
#'   labs(size = "Richter scale value", color = "# deaths") +
#'   ggplot2::theme(panel.background = ggplot2::element_blank(),
#'                  legend.position = "bottom",
#'                  axis.title.y = ggplot2::element_blank()) +
#'   ggplot2::xlab("DATE") +
#'   geom_timeline_label(data=NOAA_df_China_USA)
#' }

geom_timeline_label <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity", na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE, n_max = 5, ...) {

  # here we alter the number of earthquakes we apply a label to as n_max
  data <- data %>% dplyr::mutate(COUNTRY = as.character(COUNTRY), EQ_PRIMARY = as.numeric(EQ_PRIMARY)) %>%
    dplyr::arrange(COUNTRY, desc(EQ_PRIMARY))

  countries <- unique(data$COUNTRY)
  df_all <- data.frame()
  for(country in countries){
    df <- data %>% dplyr::filter(COUNTRY == country) %>% head(n_max)
    df_all <- rbind(df_all, df)
  }
  data <- df_all
  #print(data)

  ggplot2::layer(
    geom = GeomTimelineLabel,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' @title GeomTimeline class
#'
#' @description building a GeomTimeline class which is used in a function
#'
#' @import ggplot2
#' @import scales
#' @import grid
#'
#' @export
#'
#' @examples
#' \dontrun{
#' \dontrun{
#' NOAA_df_China_USA <- NOAA_df %>%
#'  filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
#' ggplot(NOAA_df_China_USA, aes(x = DATE, y = COUNTRY,
#'                               color = as.numeric(TOTAL_DEATHS),
#'                               size = as.numeric(EQ_PRIMARY),
#'                               label = CLEAN_LOCATION_NAME)) +
#'   geom_timeline() +
#'   labs(size = "Richter scale value", color = "# deaths") +
#'   ggplot2::theme(panel.background = ggplot2::element_blank(),
#'                  legend.position = "bottom",
#'                  axis.title.y = ggplot2::element_blank()) +
#'   ggplot2::xlab("DATE")
#' }
#'
#'
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x"), # optional y aesthetic
                                 default_aes = ggplot2::aes(y=1, alpha=0.7, fill="grey", colour="grey", size=1, shape=21, stroke=1),
                                 draw_key = ggplot2::draw_key_point,

                                 # we'll need points across a line for each level / country
                                 draw_group = function(data, panel_scales, coord) {
                                   #print(head(data))
                                   coords <- coord$transform(data, panel_scales)

                                   #?grid::pointsGrob
                                   # pch -> numeric or character vector indicating what sort of plotting symbol to use
                                   # points for more details: pch = 21 is a filled circle. See page 259 of course materials
                                   points <- grid::pointsGrob(coords$x, coords$y,
                                                              pch = coords$shape,
                                                              size = grid::unit(coords$size / 6, "lines"),        # see ?grid::unit
                                                              gp = gpar(col = alpha(coords$colour, coords$alpha), # see ?scales::alpha
                                                                        fill = alpha(coords$colour, coords$alpha)
                                                              )
                                   )

                                   #?grid::segmentsGrob
                                   line <- grid::segmentsGrob(
                                     x0 = 0, y0 = coords$y,
                                     x1 = 1, y1 = coords$y,
                                     gp = gpar(col = "grey", alpha=0.7, size=1)
                                   )
                                   grid::gList(points, line)
                                 }
)

#' @title GeomTimelineLabel class
#'
#' @description This is a GeomTimelineLabel class which is used in the
#' functions
#'
#' @import ggplot2
#' @import scales
#' @import grid
#'
#' @export
#'
#' @examples
#' \dontrun{
#' NOAA_df_China_USA <- NOAA_df %>%
#' filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
#' ggplot(NOAA_df_China_USA , aes(x = DATE, y = COUNTRY,
#'                                color = as.numeric(TOTAL_DEATHS),
#'                                size = as.numeric(EQ_PRIMARY),
#'                                label = CLEAN_LOCATION_NAME)) +
#'   geom_timeline() +
#'   labs(size = "Richter scale value", color = "# deaths") +
#'   ggplot2::theme(panel.background = ggplot2::element_blank(),
#'                  legend.position = "bottom",
#'                  axis.title.y = ggplot2::element_blank()) +
#'   ggplot2::xlab("DATE") +
#'   geom_timeline_label(data=NOAA_df_China_USA)
#' }
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                      required_aes = c("x", "label"),
                                      default_aes = ggplot2::aes(y=1, alpha=0.7, fill="grey", colour="grey"),
                                      draw_key = ggplot2::draw_key_label,

                                      # we can already get the points and horizontal line using geom_timeline
                                      # here we look to add a vertical line to a label
                                      draw_group = function(data, panel_scales, coord) {
                                        #print(head(data))
                                        coords <- coord$transform(data, panel_scales)

                                        y_extension <- 0.05
                                        line <- grid::segmentsGrob(# get the vertical line
                                          x0 = coords$x, y0 = coords$y,
                                          x1 = coords$x, y1 = coords$y + y_extension,
                                          gp = grid::gpar(col = "grey", alpha=0.7, size=1)
                                        )

                                        text <- grid::textGrob(# ?grid::textGrob
                                          label=coords$label,
                                          x = coords$x,
                                          y = coords$y + y_extension,
                                          rot = 45,
                                          just = c("left", "bottom")
                                        )

                                        grid::gList(line, text)
                                      }
)

#' @title This is used to plot the earthquake timeline
#'
#' @description This is used to plot the earthquake timeline
#'
#' @param df A dataframe
#'
#' @return NULL
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' NOAA_df_China_USA <- NOAA_df %>%
#' filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
#' plot_earthquakes_timeline(NOAA_df_China_USA)
#' }
#'
plot_earthquakes_timeline <- function(df){
  ggplot(df, aes(x = date, y = COUNTRY,
                 color = as.numeric(TOTAL_DEATHS),
                 size = as.numeric(EQ_PRIMARY))) +
    geom_timeline() +
    labs(size = "Richter scale value", color = "# deaths") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.title.y = ggplot2::element_blank()) +
    ggplot2::xlab("DATE")
}

#' @title plotting the earthquake timeline
#'
#' @description plotting the earthquake timeline using a dataframe
#'
#' @param df A dataframe
#'
#' @return NULL
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' NOAA_df_China_USA <- NOAA_df %>%
#' filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000)
#' plot_earthquakes_timeline_label(NOAA_df_China_USA)
#' }
#'
plot_earthquakes_timeline_label <- function(df){
  ggplot(df, aes(x = date, y = COUNTRY,
                 color = as.numeric(TOTAL_DEATHS),
                 size = as.numeric(EQ_PRIMARY),
                 label = CLEAN_LOCATION_NAME)) +
    geom_timeline() +
    labs(size = "Richter scale value", color = "# deaths") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.title.y = ggplot2::element_blank()) + ggplot2::xlab("DATE") +
    geom_timeline_label(data=df)
}

