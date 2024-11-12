library(sf)
library(ggplot2)
library(tidyverse)
library(extrafont)
library(scales)
library(osmdata)
library(rmapshaper)

install.packages("rmapshaper")

#####################################################################################################################


coords <- matrix(c(-87.95, -87.45, 41.638, 42.07), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c('x','y'),c('min','max'))) 
location <- coords %>% opq(timeout=40)


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
                  value = c("motorway","trunk","primary","motorway_junction","trunk_link","primary_link","motorway_link")) %>%
  osmdata_sf()


streets <- location %>%
  add_osm_feature(key = "highway", 
                  value = c("primary","secondary")) %>%
  osmdata_sf()

#######################################################

water <- location %>%
  add_osm_feature(key = "natural", 
                  value = c("water")) %>%
  osmdata_sf()

#########################################################

island <- location %>%
  add_osm_feature(key = "place", 
                  value = c("island")) %>%
  osmdata_sf()

######################################################## 
 
rail <- location %>%
  add_osm_feature(key = "railway", 
                  value = c("rail","light_rail","subway")) %>%
  osmdata_sf()

parks <- location %>%
  add_osm_feature(key = "leisure", 
                  value = c("park","nature_reserve","recreation_ground","golf_course","pitch","garden")) %>%
  osmdata_sf()

osmdata_sf()
parks <- location %>%
  add_osm_feature(key = "leisure", 
                  value = c("park","nature_reserve","recreation_ground","golf_course","pitch","garden")) %>%
  osmdata_sf()

osmdata_sf()


#plot map
ggplot()+
  geom_sf(data = parks$osm_polygons, fill = '#94ba8e',color="#94ba8e") +
  geom_sf(data = parks$osm_multipolygons, fill = '#94ba8e',,color="#94ba8e") +
  geom_sf(data = water$osm_multipolygons,fill = '#c6e1e3', color = '#c6e1e3') +
  geom_sf(data = water$osm_lines, color = '#c6e1e3') +
  geom_sf(data = water$osm_polygons,fill = '#c6e1e3', color = '#c6e1e3') +
  geom_sf(data = main_streets$osm_lines, color = '#ff9999', size = 2) + 
  geom_sf(data = island$osm_polygons, fill = 'white',color='white', size = 2) + 
  geom_sf(data = streets$osm_lines, size = 1.5, color = '#eedede') +
  #geom_sf(data = rail$osm_lines, color = '#596060', size = 2.2) +
  coord_sf(xlim = c(coords[1], coords[1,2]), 
           ylim = c(coords[2], coords[2,2]),
           expand = FALSE) + 
  theme_minimal()+
  coord_sf(xlim=c(-87.75, -87.45),ylim=c(41.75, 42.07))










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
  #geom_sf(data = rail$osm_lines, color = '#596060', size = 2.2) +
  coord_sf(xlim = c(coords[1], coords[1,2]), 
           ylim = c(coords[2], coords[2,2]),
           expand = FALSE) + 
  theme_minimal()



#############################################


chibasemap+
  geom_sf(data = chiwards2023,color="black",aes(fill = as.factor(councilvotes$LSD)),size=1,alpha=0.5)+
  scale_fill_manual(values=c("lightgrey","#459ED8"),labels=c("","Signed Letter"))+
  geom_sf_label(data=chiwards2023,aes(label = ward),size=3)+
  coord_sf(xlim=c(-87.8, -87.6),ylim=c(41.8, 42.02))+
  labs(title="Redefine The Drive- Alders advocating for a Transit Oriented Design",
       subtitle="June 2024")+
  theme_void()+
  labs(fill="")+
  theme(legend.position=c(0.9,0.7))+
  theme(plot.title=element_text(hjust=.98,vjust=-11.5))+
  theme(plot.subtitle=element_text(hjust=.98,vjust=-15))+
  theme(text=element_text(family="Tahoma"))


ggsave(plot=redefine, "redefine.png",height=11,width=8.5)



