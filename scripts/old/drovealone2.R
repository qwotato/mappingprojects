library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)
library(sf)
library(extrafont)
library(ggrepel)
library(mapboxapi)


tmap_mode("plot")

# https://walker-data.com/census-r/mapping-census-data-with-r.html


chicago <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/chicagooutline.json")

dlsd <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/dlsd.geojson")

ctalines <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/ctalines.geojson")

##########################################
#Pulling Cook County drove-alone data from 2020

cook_county_drove <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook",
  variables = c(
    drove = "B08006_003"
    
  ),
  summary_var = "B08006_001",
  year = 2022,
  geometry = TRUE
) %>% mutate(droveshare = estimate / summary_est)

# remove empty geometry
cook_county_drove <- cook_county_drove %>% filter(!st_is_empty(cook_county_drove))

cook_county_drove <- cook_county_drove %>% filter(variable == "drove")




##########################################
#Pulling Cook County car-free data from 2010

cook_county_drove10 <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook",
  variables = c(
    drove10 = "B08006_003"
  ),
  summary_var = "B08006_001",
  year = 2010,
  geometry = TRUE
) %>% mutate(droveshare10 = estimate / summary_est) %>% mutate(pop10 = summary_est)


# remove empty geometry
cook_county_drove10 <- cook_county_drove10 %>% filter(!st_is_empty(cook_county_drove10))

cook_county_drove10 <- cook_county_drove10 %>% filter(variable == "drove10")
#######################################################################

cookdrove10short <- cook_county_drove10 %>% select( GEOID, droveshare10, pop10)

cookdrovechange <- st_join(cook_county_drove, cookdrove10short)

#cookdrovechange <- cookdrovechange %>% filter(GEOID.x == GEOID.y)

cookdrovechange <-  cookdrovechange %>% mutate(drovechange = cookdrovechange$droveshare - cookdrovechange$droveshare10)

cookdrovechange <-  cookdrovechange %>% mutate(popchange = cookdrovechange$summary_est - cookdrovechange$pop10)

#######################################################################

tm_shape(cook_county_drove) + 
  tm_polygons()


drovechangemap <- tm_shape(cookdrovechange) + 
  tm_polygons(col = "drovechange")

drovechangemap

tm_shape(cookchange) + 
  tm_polygons(col= "vchange",title="Cook County - Change in households with vehicles",subtitle="2010 to 2020") + 
  tm_layout(legend.outside = FALSE,
            legend.outside.position = "")


#################################################################################################################################
#Commuter Vehicles Per Capita

cookdrovealone <- ggplot(cook_county_drove)+
  geom_sf(data = water$osm_multipolygons,fill = '#c6e1e3', color = '#c6e1e3') +
  geom_sf(data = water$osm_lines, color = '#c6e1e3') +
  geom_sf(data = water$osm_polygons,fill = '#c6e1e3', color = '#c6e1e3') +
  geom_sf(aes(fill=droveshare))+
  scale_fill_stepsn(name="Share of Total Commuters \nThat Drove Alone",,labels=scales::label_percent(),colours = c("white","#fbfaa6","orange","red"),
                    breaks = (c(0, .1, .2, .3, .4, .5, .6,.7,.8)))+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.8)+
  geom_sf(data=ctalines,aes(color=Hex),linewidth=2)+
  scale_color_identity()+
  labs(title="Means of Transportation to Work: Drove Alone",subtitle="Cook County",caption="Source: U.S. Census Bureau American Community Survery Data 2020")+
  coord_sf(xlim=c(-88.3,-87.485),ylim=c(41.45,42.2),expand=FALSE)+
  theme_void()+
  theme(text=element_text(family="Tahoma"),legend.position=c(0.25,0.6))

cookdrovealone

ggsave(plot=cookdrovealone, "cookdrovealone.jpg",height=11,width=8.5)

nsidedrovealone<- ggplot(cook_county_drove)+
  geom_sf(data = water$osm_multipolygons,fill = '#c6e1e3', color = '#c6e1e3') +
  geom_sf(data = water$osm_lines, color = '#c6e1e3') +
  geom_sf(data = water$osm_polygons,fill = '#c6e1e3', color = '#c6e1e3') +
  geom_sf(aes(fill=droveshare),alpha=0.5)+
  scale_fill_stepsn(name="Share of Total Commuters \nThat Drove Alone",labels=scales::label_percent(),colours = c("white","#fbfaa6","orange","red","black"),
                    breaks = (c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6,0.7,0.8)))+
  #geom_sf(data=chicago,fill=NA,color="black",linewidth=0.5)+
  #geom_sf(data=dlsd,fill=NA,color="red",linewidth=1)+
  geom_text(aes(x=-87.578,y=41.86,label=" - Census Tract 3301.01, Cook, IL. \nMuseum Campus & McCormick Place"),color="black")+
  #geom_sf(data = main_streets$osm_lines, color = 'black', linewidth = 01) +
  geom_sf(data=ctalines,aes(color=Hex),linewidth=0.8)+
  geom_sf_text(aes(label=scales::percent(droveshare,accuracy=1)),size=1.8)+
  scale_color_identity()+
  coord_sf(xlim=c(-87.7,-87.52),ylim=c(41.8,41.95),expand=FALSE)+
  labs(title="2022 Commutes: Drove Alone",subtitle="Downtown Chicago, IL.",legend="percent",caption="Source: U.S. Census Bureau American Community Survery Data 2022")+
  theme_void()+
  theme(text=element_text(family="Tahoma"),legend.position=c(0.84,0.6))

nsidedrovealone

ggsave(plot=nsidedrovealone, "chidowndrovealone.jpg",height=11,width=8.5)

########################################################################################################################



ggplot(cook_county_drove)+
  #geom_sf(data = parks$osm_polygons, fill = '#94ba8e',color="#94ba8e") +
 # geom_sf(data = parks$osm_multipolygons, fill = '#94ba8e',,color="#94ba8e") +
  geom_sf(data = water$osm_multipolygons,fill = '#c6e1e3', color = '#c6e1e3') +
  geom_sf(data = water$osm_lines, color = '#c6e1e3') +
  geom_sf(data = water$osm_polygons,fill = '#c6e1e3', color = '#c6e1e3') +
  #geom_sf(data = main_streets$osm_lines, color = '#ff9999', linewidth = 3) + 
  #geom_sf(data = island$osm_polygons, fill = '#94ba8e',color='#94ba8e', size = 2) + 
  #geom_sf(data = streets$osm_lines, linewidth = 1.5, color = '#eedede') +
  #geom_sf(data = rail$osm_lines, color = '#596060', size = 2.2) +
  geom_sf(aes(fill=droveshare),alpha=0.5)+
  scale_fill_stepsn(name="Share of Total Commuters \nThat Drove Alone",labels=scales::label_percent(),colours = c("white","#fbfaa6","orange","red","black"),
                    breaks = (c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6,0.7,0.8)))+
  geom_sf(data=chicago,fill=NA,color="#00a1de",linewidth=1)+
  geom_sf(data=dlsd,fill=NA,color="red",linewidth=2)+
  geom_text(aes(x=-87.62,y=41.97,label="DLSD"),color="red")+
  geom_sf(data=ctalines,fill=NA,color="black",linewidth=1)+
  coord_sf(xlim=c(-87.85,-87.485),ylim=c(41.65,42.06),expand=FALSE)+
  labs(title="2020 Commutes: Drove Alone",subtitle="North Side of Chicago",legend="percent",caption="Source: U.S. Census Bureau American Community Survery Data 2020")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))
