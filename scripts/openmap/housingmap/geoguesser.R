library(sf)
library(ggplot2)
library(tidyverse)
library(extrafont)
library(scales)
library(osmdata)
library(OpenStreetMap)
library(rmapshaper)
library(ggmap)

install.packages("OpenStreetMap")

#####################################################################################################################

cityframe <- data.frame(long=-122.452192,lat=37.762036,zoom=0.15)

coords <- matrix(c(cityframe$long-cityframe$zoom, cityframe$long+cityframe$zoom, cityframe$lat-cityframe$zoom, cityframe$lat+cityframe$zoom), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c('x','y'),c('min','max'))) 
location <- coords %>% opq(timeout=40)


#query OSM
  main_streets <- location %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway","trunk","primary","motorway_junction","trunk_link","primary_link","motorway_link")) %>%
  osmdata_sf()


streets <- location %>%
  add_osm_feature(key = "highway", 
                  value = c("primary","secondary")) %>%
  osmdata_sf()

#######################################################

water <- location %>%
  add_osm_feature(key = "natural", 
                  value = c("water","strait","bay","sea","ocean","lagoon","coastline")) %>%
  osmdata_sf()


#########################################################

island <- location %>%
  add_osm_feature(key = "place", 
                  value = c("island")) %>%
  osmdata_sf()

########################################################

sea <- location %>%
  add_osm_feature(key = "place", 
                  value = c("ocean")) %>%
  osmdata_sf()

######################################################## 
 
rail <- location %>%
  add_osm_feature(key = "railway", 
                  value = c("light_rail","subway")) %>%
  osmdata_sf()

parks <- location %>%
  add_osm_feature(key = "leisure", 
                  value = c("park","nature_reserve","recreation_ground","golf_course","pitch","garden")) %>%
  osmdata_sf()



#plot map
guesscity <- ggplot()+
  #geom_sf(data = parks$osm_polygons, fill = '#94ba8e',color="#94ba8e") +
  #geom_sf(data = parks$osm_multipolygons, fill = '#94ba8e',,color="#94ba8e") +
  geom_sf(data = water$osm_multipolygons,fill = '#c6e1e3', color = '#c6e1e3') +
  geom_sf(data = water$osm_lines, color = '#c6e1e3') +
  geom_sf(data = water$osm_polygons,fill = '#c6e1e3', color = '#c6e1e3') +
  #geom_sf(data = main_streets$osm_lines, color = '#ff9999', size = 2) + 
  #geom_sf(data = island$osm_polygons, fill = 'white',color='white', size = 2) + 
  #geom_sf(data = streets$osm_lines, size = 1.5, color = '#eedede') +
  #geom_sf(data = rail$osm_lines, color = '#596060', size = 2.2) +
  coord_sf(xlim = c(coords[1], coords[1,2]), 
           ylim = c(coords[2], coords[2,2]),
           expand = FALSE) + 
  theme_void()

guesscity

ggsave(plot=guesscity, "guesscity9.png",height=8,width=8)

#############################

chibasemapbw <- ggplot()+
  geom_sf(data = parks$osm_polygons, fill = '#E0E0E0',color="#E0E0E0") +
  geom_sf(data = parks$osm_multipolygons, fill = '#E0E0E0',,color="#E0E0E0") +
  geom_sf(data = water$osm_multipolygons,fill = '#D2E0ED', color = '#D2E0ED') +
  geom_sf(data = water$osm_lines, color = '#D2E0ED') +
  geom_sf(data = water$osm_polygons,fill = '#D2E0ED', color = '#D2E0ED') +
  geom_sf(data = main_streets$osm_lines, color = '#9E9E9E', size = 2) + 
  geom_sf(data = island$osm_polygons, fill = 'white',color='white', size = 2) + 
  geom_sf(data = streets$osm_lines, size = 1.5, color = '#EEEEEE') +
  geom_sf(data = rail$osm_lines, color = '#596060', size = 2.2) +
  coord_sf(xlim = c(coords[1], coords[1,2]), 
           ylim = c(coords[2], coords[2,2]),
           expand = FALSE) + 
  theme_minimal()

###############################################

chibrat <- ggplot()+
  geom_sf(data = parks$osm_polygons, fill = '#8ace00',color="#8ace00") +
  geom_sf(data = parks$osm_multipolygons, fill = '#8ace00',,color="#8ace00") +
  geom_sf(data = water$osm_multipolygons,fill = '#8ace00', color = '#8ace00') +
  geom_sf(data = water$osm_lines, color = '#8ace00') +
  geom_sf(data = water$osm_polygons,fill = '#8ace00', color = '#8ace00') +
  geom_sf(data = main_streets$osm_lines, color = '#8ace00', size = 2) + 
  geom_sf(data = island$osm_polygons, fill = 'white',color='white', size = 2) + 
  geom_sf(data = streets$osm_lines, size = 1.5, color = '#EEEEEE') +
  geom_sf(data = rail$osm_lines, color = '#8ace00', size = 2.2) +
  geom_text(data=brat,aes(x=-87.55,y=41.95),size=10,label=brat)+
  coord_sf(xlim = c(coords[1], coords[1,2]), 
           ylim = c(coords[2], coords[2,2]),
           expand = FALSE) + 
    theme(text=element_text(family="Arial Narrow"))+
  theme_minimal()

ggsave(plot=chibrat, "chibrat.png",height=11,width=8.5)

brat <- data_frame(name="chicago")
###############################################


chibasemap <- ggplot()+
  geom_sf(data = parks$osm_polygons, fill = '#94ba8e',color="#94ba8e") +
  geom_sf(data = parks$osm_multipolygons, fill = '#94ba8e',,color="#94ba8e") +
  geom_sf(data = water$osm_multipolygons,fill = '#c6e1e3', color = '#c6e1e3') +
  geom_sf(data = water$osm_lines, color = '#c6e1e3') +
  geom_sf(data = water$osm_polygons,fill = '#c6e1e3', color = '#c6e1e3') +
  geom_sf(data = main_streets$osm_lines, color = '#ff9999', size = 2) + 
  geom_sf(data = island$osm_polygons, fill = 'white',color='white', size = 2) + 
  geom_sf(data = streets$osm_lines, size = 1.5, color = '#eedede') +
  geom_sf(data = rail$osm_lines, color = '#596060', size = 2.2) +
  coord_sf(xlim = c(coords[1], coords[1,2]), 
           ylim = c(coords[2], coords[2,2]),
           expand = FALSE) + 
    theme(text=element_text(family="Tahoma"))+
  theme_minimal()


register_google(key='AIzaSyA7Rh82CxysX2I2RtEps2seYFiPAYsutDU',write=TRUE)
chicago_bb <- getbb("Chicago")
chicago_map <- get_map(chicago_bb, maptype = "roadmap")

ggmap(chicago_map)

#############################################

may24 <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/DivvyData/202405-divvy-tripdata/202405-divvy-tripdata.csv")

# taking the starting points
heatstarts <- select(may24,c('start_lat','start_lng'))

#rounding these down to get full grid coverage
heatround <- heatstarts %>% mutate(across(where(is.numeric), round, 2))

# Adding a score of 1 to each row, and then aggregating to sum all of the duplicates of each lat/long pair. Also filtering out bad data (lng=0)
heatround$count <- 1
heatround <- aggregate(count ~ .,heatround,FUN=sum)
heatround <- filter(heatround, start_lng != 0)



#############################################

MayDivvyMap <- chibasemapbw+
  geom_tile(data=heatround, aes(x=start_lng,y=start_lat,fill=count),alpha=0.5)+
  scale_fill_continuous(label=scales::comma,high="darkred",low="#FFF8E1")+
  coord_sf(xlim=c(-87.8, -87.5),ylim=c(41.7, 42.02))+
  labs(title="Divvy Ride Starting Points",
       subtitle=paste0("May 2024: ",scales::comma(sum(heatround$count))," Rides"))+
  theme_void()+
  labs(fill="Rides From Zone")+
  theme(legend.position=c(0.85,0.7))+
  theme(plot.title=element_text(hjust=.95,vjust=-11.5))+
  theme(plot.subtitle=element_text(hjust=.95,vjust=-15))+
  theme(panel.background = element_rect(fill = '#fcfbf7', color = 'black'))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.5)))+
  theme(text=element_text(family="Tahoma"))

MayDivvyMap

ggsave(plot=MayDivvyMap, "MayDivvyMap.png",height=11,width=8.5)


ggmap(chicago_map)+
  geom_tile(data=heatround, aes(x=start_lng,y=start_lat,fill=count),alpha=0.5)+
  scale_fill_continuous(label=scales::comma,high="darkred",low="#FFF8E1")+
  coord_sf(xlim=c(-87.8, -87.5),ylim=c(41.7, 42.02))+
  labs(title="Divvy Ride Starting Points",
       subtitle=paste0("May 2024: ",scales::comma(sum(heatround$count))," Rides"))+
  theme_void()+
  labs(fill="Rides From Zone")+
  theme(legend.position=c(0.85,0.7))+
  theme(plot.title=element_text(hjust=.95,vjust=-11.5))+
  theme(plot.subtitle=element_text(hjust=.95,vjust=-15))+
  theme(panel.background = element_rect(fill = '#fcfbf7', color = 'black'))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.5)))+
  theme(text=element_text(family="Tahoma"))
