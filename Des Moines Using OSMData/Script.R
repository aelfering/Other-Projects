library(tidyverse)
library(osmdata)
library(sf)

DallasCounty <- getbb("Dallas County Iowa United States")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

DallasCountySmall <- getbb("Dallas County Iowa United States")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()




PolkCounty <- getbb("Polk County Iowa United States")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

PolkCountySmall <- getbb("Polk County Iowa United States")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

polk_dallas_counties <- ggplot() +
  geom_sf(data = DallasCounty$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = 0.5,
          alpha = .6) +
  geom_sf(data = DallasCountySmall$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = 0.1,
          alpha = .6) +
  geom_sf(data = PolkCounty$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = 0.5,
          alpha = .6) +
  geom_sf(data = PolkCountySmall$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = 0.1,
          alpha = .6) +
  coord_sf(xlim = c(-94.2, -93.4), 
           ylim = c(41.5, 41.8),
           expand = FALSE)  +
  theme_void()








  