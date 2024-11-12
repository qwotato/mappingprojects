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
#Pulling Cook County car-free data from 2020

cook_county <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook",
  variables = c(
    vehiclesused = "B08015_001"
  ),
  summary_var = "B01001_001",
  year = 2020,
  geometry = TRUE
) %>% mutate(vofp = estimate / summary_est)

# remove empty geometry
cook_county <- cook_county %>% filter(!st_is_empty(cook_county))

cook_county <- cook_county %>% filter(variable == "vehiclesused")




##########################################
#Pulling Cook County car-free data from 2010

cook_county10 <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook",
  variables = c(
    vehiclesused10 = "B08015_001"
  ),
  summary_var = "B01001_001",
  year = 2010,
  geometry = TRUE
) %>% mutate(vofp10 = estimate / summary_est) %>% mutate(pop10 = summary_est)


# remove empty geometry
cook_county10 <- cook_county10 %>% filter(!st_is_empty(cook_county10))

cook_county10 <- cook_county10 %>% filter(variable == "vehiclesused10")
#######################################################################

cook10short <- cook_county10 %>% select(GEOID, vofp10, pop10)

cookchange <- st_join(cook_county, cook10short)

cookchange <- cookchange %>% filter(GEOID.x == GEOID.y)

cookchange <-  cookchange %>% mutate(vchange = cookchange$vofp - cookchange$vofp10)

cookchange <-  cookchange %>% mutate(popchange = cookchange$summary_est - cookchange$pop10)

#######################################################################

tm_shape(cook_county) + 
  tm_polygons()


carchangemap <- tm_shape(cookchange) + 
  tm_polygons(col = "vchange")

carchangemap

tm_shape(cookchange) + 
  tm_polygons(col= "vchange",title="Cook County - Change in households with vehicles",subtitle="2010 to 2020") + 
  tm_layout(legend.outside = FALSE,
            legend.outside.position = "")


nsidevperpchangeb <-ggplot(cookchange)+
  geom_sf(aes(fill=vchange))+
  scale_fill_stepsn(name="2010 to 2020 Change In \nVehicles Per Person",labels=scales::label_percent(),colours = c("#072a8E","#8BA5F2","#e9f5ff","white","white"),
                    values = scales::rescale(c(-0.25,-0.2,-0.15, -0.1,-0.05, 0,0.05, 0.1,0.15, 0.2, 0.25)))+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=1,size=0.5)+
  geom_sf(data=dlsd,fill=NA,color="#Fbd073",linewidth=1)+
  geom_text(aes(x=-87.62,y=41.97,label="DLSD"),color="#Fbd073")+
  geom_sf(data=ctalines,fill=NA,color="darkgrey",linewidth=1)+
  geom_sf_text(aes(label=scales::percent(vchange,accuracy=1)),size=1.8)+
  coord_sf(xlim=c(-87.75,-87.585),ylim=c(41.85,42.06),expand=FALSE)+
  labs(title="Commuter Vehicles Per Person - 2010 to 2020 Change",subtitle="North Side of Chicago",legend="percent",caption="Source: U.S. Census Bureau American Community Survery Data, 2010 & 2020")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))

nsidevperpchangeb

ggsave(plot=nsidevperpchangeb, "nsidevperpchangeb.jpg",height=11,width=8.5)


cookvperpchangeb <-ggplot(cookchange)+
  geom_sf(aes(fill=vchange))+
  scale_fill_stepsn(name="2010 to 2020 Change In \nVehicles Per Person",labels=scales::label_percent(),colours = c("#072a8E","#8BA5F2","#e9f5ff","white","white"),
                    values = scales::rescale(c(-0.25,-0.2,-0.15, -0.1,-0.05, 0,0.05, 0.1,0.15, 0.2, 0.25)))+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.8)+
  #geom_sf(data=dlsd,fill=NA,color="red",linewidth=2)+
  #geom_sf(data=ctalines,fill=NA,color="black",linewidth=1)+
  labs(title="Commuter Vehicles Per Person - 2010 to 2020 Change",subtitle="Cook County",caption="Source: U.S. Census Bureau American Community Survery Data, 2010 & 2020")+
  theme_void()+
  theme(text=element_text(family="Tahoma"),legend.position=c(0.25,0.6))

cookvperpchangeb

ggsave(plot=cookvperpchangeb, "cookvperpchangeb.jpg",height=11,width=8.5)


#################################################################################################################################
#Commuter Vehicles Per Person

cookvperpb <- ggplot(cookchange)+
  geom_sf(aes(fill=vofp))+
  scale_fill_stepsn(name="Commuter Vehicles Per Person",colours = c("#072a8E","#8BA5F2","#e9f5ff","white","white"),
                    values = scales::rescale(c(-0.25,-0.2,-0.15, -0.1,-0.05, 0,0.05, 0.1,0.15, 0.2, 0.25)))+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.8)+
  #geom_sf(data=dlsd,fill=NA,color="red",linewidth=1.5)+
  #geom_sf(data=ctalines,fill=NA,color="black",linewidth=1)+
  labs(title="Commuter Vehicles Per Person - 2020",subtitle="Cook County",caption="Source: U.S. Census Bureau American Community Survery Data 2020")+
  theme_void()+
  theme(text=element_text(family="Tahoma"),legend.position=c(0.25,0.6))

cookvperpb

ggsave(plot=cookvperpb, "cookvperpb.jpg",height=11,width=8.5)

nsidevperp <- ggplot(cookchange)+
  geom_sf(aes(fill=vofp))+
  scale_fill_stepsn(name="Commuter Vehicles Per Person",colours = c("#072a8E","#8BA5F2","#e9f5ff","white","white"),
                    values = scales::rescale(c(-0.25,-0.2,-0.15, -0.1,-0.05, 0,0.05, 0.1,0.15, 0.2, 0.25)))+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=1,size=0.5)+
  geom_sf(data=dlsd,fill=NA,color="#Fbd073",linewidth=1)+
  geom_text(aes(x=-87.62,y=41.97,label="DLSD"),color="#Fbd073")+
  geom_sf(data=ctalines,fill=NA,color="darkgrey",linewidth=1)+
  geom_sf_text(aes(label=round(vofp,2)),size=1.8)+
  coord_sf(xlim=c(-87.75,-87.585),ylim=c(41.85,42.06),expand=FALSE)+
  labs(title="Commuter Vehicles Per Person - 2020",subtitle="North Side of Chicago",legend="percent",caption="Source: U.S. Census Bureau American Community Survery Data 2020")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))

nsidevperp

ggsave(plot=nsidevperp, "nsidevperp.jpg",height=11,width=8.5)

########################################################################################################################


#popchange


ggplot(cookchange)+
  geom_sf(aes(fill=popchange))+
  scale_fill_stepsn(name="Change",colours = c("darkred","#fbfaa6","white", "#c3eac9", "#57bc65", "darkgreen"),
                    breaks = c(-2500, -2000,-1500,-1000,-500, 0,500, 1000,1500, 2000,2500))+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.8)+
  geom_sf(data=dlsd,fill=NA,color="red",linewidth=1.5)+
  #geom_sf(data=ctalines,fill=NA,color="black",linewidth=1)+
  labs(title="Commuter Vehicles Per Person - 2020",subtitle="Cook County",caption="Source: U.S. Census Bureau American Community Survery Data 2010 & 2020")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))

ggplot(cookchange)+
  geom_sf(aes(fill=popchange))+
  scale_fill_stepsn(name="Change",colours = c("darkred","#fbfaa6","white", "#c3eac9", "#57bc65", "darkgreen"),
                    breaks = c(-2500, -2000,-1500,-1000,-500, 0,500, 1000,1500, 2000,2500))+
  geom_sf(data=chicago,fill=NA,color="#00a1de",linewidth=1)+
  geom_sf(data=dlsd,fill=NA,color="red",linewidth=2)+
  geom_sf(data=ctalines,fill=NA,color="black",linewidth=1)+
  coord_sf(xlim=c(-87.75,-87.585),ylim=c(41.85,42.06),expand=FALSE)+
  labs(title="Commuter Vehicles Per Person - 2020",subtitle="North Side of Chicago",legend="percent",caption="Source: U.S. Census Bureau American Community Survery Data 2010 & 2020")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))
