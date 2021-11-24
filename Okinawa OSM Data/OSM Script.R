# OSM Street Map of Okinawa Island
# functions courtesy of Amit Levinson - https://github.com/AmitLevinson/30daymapchallenge/blob/main/2021/05_osm/streets.R

library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)


# Data collections & setup ------------------------------------------------
# Thanks to Amit Levinson for the function - https://github.com/AmitLevinson/30daymapchallenge/blob/main/2021/05_osm/streets.R

large_features <- c("motorway","primary","secondary", "tertiary")
small_features <- c("residential", "living_street", "unclassified", "service", "footway")

get_osm_data <- function (city, features) {
  getbb(city)%>%
    opq()%>%
    add_osm_feature(key = "highway", 
                    value = features) %>% 
    osmdata_sf()
  
}


oka_large <- get_osm_data("Okinawa", features = large_features)
oka_small <- get_osm_data("Okinawa", features = small_features)

# Plot --------------------------------------------------------------------
Okinawa <- ggplot(data = oka_small$osm_lines)+
  geom_sf(color = "gray15", 
          size = 0.2) +
  geom_sf(data = jlm_large$osm_lines,
          color = 'black',
          size = 0.4) +
  theme (
    # For some reason I add this to solve FaceBook issues :(
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA)
  )

ggsave(plot = Okinawa, 
       filename = 'Okinawa1.png',
       width = 12,
       height = 15,
       units = 'in')


