library(tidyverse)
library(ggmap)
library(sf)
library(mapview)

library(ggrepel)

rail = read_csv('rail.csv') %>% mutate(place= str_c(place,"METRO STATION", sep=" ")) # Add metro station for better localisation

rail1=mutate_geocode(rail,place)

rail2 = na.omit(rail1) # only take p completed object

rail3 = rail %>% filter(!place%in%rail2$place) # filter uncompleted fiels

rail4 = mutate_geocode(rail3,place)

rail5 = full_join(rail2,rail4)

rail5=rail5 %>% mutate(lon =abs(lon))

rail5=rail5 %>% mutate(place=str_replace(place,"METRO STATION","")) %>% mutate(place=tolower(place))

#rail5$lon[5]=77.27,rail5$lat[5]=28.671

#rail5$lon[2]=77.30
#rail5$lat[2]=28.675

#rail5$lon[14]=77.154
#rail5$lat[14]=28.685


#rail5$lon[4]=77.25
#rail5$lat[4]=28.673

#railx = rail5

locations_sf <- st_as_sf(rail5, coords = c("lon", "lat"), crs = 4326)

mapview(locations_sf)

map <- get_googlemap(center = c(77.10, 28.758), zoom = 11)
# look at these from map view if 12 zoom level in mapview keep 11 or 10 in google map rest same

ggmap(map)


bw_map <- get_googlemap(center = c(77.10, 28.758), zoom = 10,
                        color = "bw",
                        style = "feature:road|visibility:off&style=element:labels|visibility:off&style=feature:administrative|visibility:off")

ggmap(map) +
  geom_point(data = rail5, aes(x = lon, y = lat))

ggmap(map) +
  geom_point(data = rail5, aes(x = lon, y = lat))+

geom_text_repel(data = rail5, aes(x = lon, y = lat, label = place))

ggmap(bw_map) +
  geom_line(data = rail5, aes(x = lon, y = lat),color="red")+
  
  geom_text_repel(data = rail5, aes(x = lon, y = lat, label = place))

rm(map)
