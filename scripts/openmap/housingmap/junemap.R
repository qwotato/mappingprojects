library(sf)
library(ggplot2)
library(tidyverse)
library(extrafont)
library(scales)
library(osmdata)

install.packages("raster")
library(raster)

chiwards2023 <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/chiwards2023.geojson")

homeplan <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/housingmap/juneplancom.csv")

#####################################################################################################################

# A 2x2 matrix
chicago_bb <- matrix(data = c(-87.8, -87.6, 41.82, 42),
                    nrow = 2,
                    byrow = TRUE)
# Update column and row names
colnames(chicago_bb) <- c("min", "max")
rownames(chicago_bb) <- c("x", "y")

# Use the bounding box defined for Tucson by OSM
chicago_bb <- getbb("Chicago")
# Print the matrix to the console
chicago_bb

#####################################################################################################################

chicago_water <- getbb(place_name = "Chicago") %>%
  opq() %>%
  add_osm_feature(key = "natural", 
                  value = "water") %>%
  osmdata_sf()

chicago_water

ggplot()+
  geom_sf(data = chicago_water$osm_polygons,inherit.aes = FALSE,color = "lightblue",size = 30,fill="lightblue")+
  geom_sf(data = chicago_water$osm_multipolygons,inherit.aes = FALSE,color = "lightblue",size = 30,fill="lightblue")+
  coord_sf(xlim=c(-87.8,-87.6),ylim=c(41.82,42))

#####################################################################################################################

chicago_roads <- getbb(place_name = "Chicago") %>%
  opq(timeout=100) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary")) %>%
  osmdata_sf()

chicago_roads

ggplot()+
  geom_sf(data = chicago_roads$osm_lines,inherit.aes = FALSE,color = "darkgrey",size = 1)

#####################################################################################################################

chicago_rail <- getbb(place_name = "Chicago") %>%
  opq(timeout=100) %>%
  add_osm_feature(key = "railway", 
                  value = c("light_rail", "subway")) %>%
  osmdata_sf()

chicago_rail

ggplot()+
  geom_sf(data = chicago_rail$osm_lines,inherit.aes = FALSE,color = "lightred",size = 1)

#####################################################################################################################

chicago_parks <- getbb(place_name = "Chicago") %>%
  opq(timeout=100) %>%
  add_osm_feature(key = "leisure", 
                  value = c("park", "nature_reserve")) %>%
  osmdata_sf()

chicago_parks

ggplot()+
  geom_sf(data = chicago_parks$osm_polygons,inherit.aes = FALSE,color = "lightgreen",size = 1,fill="lightgreen")+
  geom_sf(data = chicago_parks$osm_multipolygons,inherit.aes = FALSE,color = "lightgreen",size=1,fill="lightgreen")

cta_lines <- chicago_rail$osm_lines

#####################################################################################################################
ggplot()+
  #geom_sf(data = chiwards2023,color="#fcfbf7",fill = "red",size=1,alpha=0.2)+
  geom_sf(data = chicago_parks$osm_polygons,inherit.aes = FALSE,color = "lightgreen",size = 1,fill="lightgreen")+
  geom_sf(data = chicago_parks$osm_multipolygons,inherit.aes = FALSE,color = "lightgreen",size=1,fill="lightgreen")+
  geom_sf(data = chicago_water$osm_polygons,inherit.aes = FALSE,color = "lightblue",size = 30,fill="lightblue")+
  geom_sf(data = chicago_water$osm_multipolygons,inherit.aes = FALSE,color = "lightblue",size = 30,fill="lightblue")+
  geom_sf(data = chicago_roads$osm_lines,inherit.aes = FALSE,color = "darkgrey",size = 1)+
  geom_sf(data = chicago_rail$osm_lines,inherit.aes = FALSE,color = "black",linewidth = 1)+
  geom_point(data=homeplan,aes(x=long,y=lat,size=units),alpha=0.5,color="red")+
  geom_point(data=homeplan,aes(x=long,y=lat,size=units),shape=1,color="red")+
  geom_label(data=homeplan,aes(x=long,y=lat,label=Project),hjust=1.1,size=2.5)+
  geom_label(data=homeplan,aes(x=long,y=lat,label=paste0(units," units")),hjust=1.5,vjust=1.5,size=2.5)+
  coord_sf(xlim=c(-87.7,-87.6),ylim=c(41.82,41.95))+
  labs(title="Chicago Plan Commission",
       subtitle="June 2024 Residential Considerations")+
  theme_void()+
  theme(legend.position=c(0.9,0.7))+
  #theme(plot.title=element_text(hjust=0.1,vjust=-11.5))+
  #theme(plot.subtitle=element_text(hjust=0.1,vjust=-15))+
  theme(panel.background = element_rect(fill = '#fcfbf7'))+
  theme(text=element_text(family="Tahoma"))



#########################################################################

coords <- matrix(c(-87.8, -87.6, 41.82, 42), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c('x','y'),c('min','max'))) 
location <- coords %>% opq(timeout=100)

water <- location %>%
  add_osm_feature(key = "natural", 
                  value = c("water")) %>%
  osmdata_sf()


#build different types of streets
main_st <- data.frame(type = c("motorway","trunk","primary","motorway_junction","trunk_link","primary_link","motorway_link"))
st <- data.frame(type = available_tags('highway'))
st <- subset(st, !type %in% main_st$type)
path <- data.frame(type = c("footway","path","steps","cycleway"))
st <- subset(st, !type %in% path$type)
st <- as.character(st$type)
main_st <- as.character(main_st$type)
path <- as.character(path$type)

#query OSM
main_streets <- location %>%
  add_osm_feature(key = "highway", 
                  value = main_st) %>%
  osmdata_sf()

path <- location %>%
  add_osm_feature(key = "highway", 
                  value = c("footway","path","steps","cycleway")) %>%
  osmdata_sf()

streets <- location %>%
  add_osm_feature(key = "highway", 
                  value = c("primary","secondary")) %>%
  osmdata_sf()
water <- location %>%
  add_osm_feature(key = "natural", 
                  value = c("water")) %>%
  osmdata_sf()
rail <- location %>%
  add_osm_feature(key = "railway", 
                  value = c("rail","light_rail","subway")) %>%
  osmdata_sf()
parks <- location %>%
  add_osm_feature(key = "leisure", 
                  value = c("park","nature_reserve","recreation_ground","golf_course","pitch","garden")) %>%
  osmdata_sf()

chiparks <- parks$osm_multipolygons

area(chiparks$geometry)

#plot map
ggplot() + geom_sf(data = water$osm_multipolygons, fill = 'light blue') + theme_minimal()
ggplot() + 
  geom_sf(data = main_streets$osm_lines, color = '#ff9999', size = 2) + 
  geom_sf(data = streets$osm_lines, size = 1.5, color = '#eedede') +
  geom_sf(data = water$osm_polygons, fill = '#c6e1e3') +
  geom_sf(data = water$osm_multipolygons, fill = '#c6e1e3') +
  geom_sf(data = rail$osm_lines, color = '#596060', size = 1) +
  geom_sf(data = parks$osm_polygons, fill = '#94ba8e') +
  #geom_sf(data = buildings$osm_points, color = '#40493f', fill = '#40493f', size = 2) +
  coord_sf(xlim = c(coords[1], coords[1,2]), 
           ylim = c(coords[2], coords[2,2]),
           expand = FALSE) + 
  theme_minimal()+
  coord_sf(xlim=c(-87.7,-87.6),ylim=c(41.82,41.95))
