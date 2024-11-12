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
  year = 2020,
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

cookdrove10short <- cook_county_drove10 %>% select(GEOID, droveshare10, pop10)

cookdrovechange <- st_join(cook_county_drove, cookdrove10short)

cookdrovechange <- cookdrovechange %>% filter(GEOID.x == GEOID.y)

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

###############################################################################################################################################################
#2010 to 2020 Change in commuter vehicles

ggplot(cookdrovechange)+
  geom_sf(aes(fill=drovechange))+
  scale_fill_stepsn(name="Change",colours = c("darkred","orange", "#fbfaa6","white", "#c3eac9", "#57bc65", "darkgreen"),
                    breaks = c(-0.25,-0.2,-0.15, -0.1,-0.05, 0,0.05, 0.1,0.15, 0.2, 0.25))+
  geom_sf(data=chicago,fill=NA,color="#00a1de",linewidth=1)+
  geom_sf(data=dlsd,fill=NA,color="red",linewidth=2)+
  geom_sf(data=ctalines,fill=NA,color="black",linewidth=1)+
  coord_sf(xlim=c(-87.75,-87.585),ylim=c(41.85,42.06),expand=FALSE)+
  labs(title="Share of Commuters Using drove - 2010 to 2020 Change",subtitle="North Side of Chicago",legend="percent",caption="Source: U.S. Census Bureau American Community Survery Data, 2010 & 2020")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))


ggplot(cookdrovechange)+
  geom_sf(aes(fill=drovechange))+
  scale_fill_stepsn(name="2010 to 2020 Change",labels=scales::label_percent(),colours = c("darkgreen","#57bc65", "#c3eac9","#fbfaa6", "orange","darkred"),
                    values = scales::rescale(c(-0.25,-0.2,-0.15, -0.1,-0.05, 0,0.05, 0.1,0.15, 0.2, 0.25)))+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.8)+
  #geom_sf(data=dlsd,fill=NA,color="red",linewidth=2)+
  #geom_sf(data=ctalines,fill=NA,color="black",linewidth=1)+
  labs(title="Commuter Vehicles Per Person - 2010 to 2020 Change",subtitle="Cook County",caption="Source: U.S. Census Bureau American Community Survery Data, 2010 & 2020")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))




#################################################################################################################################
#Commuter Vehicles Per Capita

cookdrovealone <- ggplot(cookdrovechange)+
  geom_sf(aes(fill=droveshare))+
  scale_fill_stepsn(name="Share of Total Commuters \nThat Drove Alone",,labels=scales::label_percent(),colours = c("white","#fbfaa6","orange","red"),
                    values = scales::rescale(c(0, .1, .2, .3, .4, .5, .6,.7,.8)))+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.8)+
  #geom_sf(data=dlsd,fill=NA,color="red",linewidth=1.5)+
  #geom_text(aes(x=-87.59,y=41.97,label="DLSD"),color="red")+
  #geom_sf(data=ctalines,fill=NA,color="black",linewidth=1)+
  labs(title="Means of Transportation to Work: Drove Alone",subtitle="Cook County",caption="Source: U.S. Census Bureau American Community Survery Data 2020")+
  theme_void()+
  theme(text=element_text(family="Tahoma"),legend.position=c(0.25,0.6))

cookdrovealone

ggsave(plot=cookdrovealone, "cookdrovealone.jpg",height=11,width=8.5)

nsidedrovealone<- ggplot(cookdrovechange)+
  geom_sf(aes(fill=droveshare))+
  scale_fill_stepsn(name="Share of Total Commuters \nThat Drove Alone",labels=scales::label_percent(),colours = c("white","#fbfaa6","orange","red"),
                    values = scales::rescale(c(0, .1, .2, .3, .4, .5, .6,.7,.8)))+
  geom_sf(data=chicago,fill=NA,color="#00a1de",linewidth=1)+
  geom_sf(data=dlsd,fill=NA,color="red",linewidth=2)+
  geom_text(aes(x=-87.62,y=41.97,label="DLSD"),color="red")+
  geom_sf(data=ctalines,fill=NA,color="black",linewidth=1)+
  coord_sf(xlim=c(-87.75,-87.585),ylim=c(41.85,42.06),expand=FALSE)+
  labs(title="2020 Commutes: Drove Alone",subtitle="North Side of Chicago",legend="percent",caption="Source: U.S. Census Bureau American Community Survery Data 2020")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))

nsidedrovealone

ggsave(plot=nsidedrovealone, "nsidedrovealone.jpg",height=11,width=8.5)

########################################################################################################################


#popchange


ggplot(cookchange)+
  geom_sf(aes(fill=popchange))+
  scale_fill_stepsn(name="Change",colours = c("darkred", "#fbfaa6", "#c3eac9", "#57bc65", "darkgreen"),
                    breaks = c(-2000,-1000, 0, 1000, 2000, 3000))+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.8)+
  geom_sf(data=dlsd,fill=NA,color="red",linewidth=1.5)+
  #geom_sf(data=ctalines,fill=NA,color="black",linewidth=1)+
  labs(title="Population Change - Cook County",subtitle="2010 to 2020",caption="Source: U.S. Census Bureau American Community Survery Data 2010 & 2020")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))

ggplot(cookchange)+
  geom_sf(aes(fill=popchange))+
  scale_fill_stepsn(name="Change",colours = c("darkred", "#fbfaa6", "#c3eac9", "#57bc65", "darkgreen"),
                    breaks = c(-2000,-1000, 0, 1000, 2000, 3000))+
  geom_sf(data=chicago,fill=NA,color="#00a1de",linewidth=1)+
  geom_sf(data=dlsd,fill=NA,color="red",linewidth=2)+
  geom_sf(data=ctalines,fill=NA,color="black",linewidth=1)+
  coord_sf(xlim=c(-87.75,-87.585),ylim=c(41.85,42.06),expand=FALSE)+
  labs(title="Population Change - North Side of Chicago",subtitle="2010 to 2020",legend="percent",caption="Source: U.S. Census Bureau American Community Survery Data 2010 & 2020")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))
