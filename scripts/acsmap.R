library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)
library(sf)
library(extrafont)
library(ggrepel)
library(mapboxapi)
library(mapview)


tmap_mode("plot")

# https://walker-data.com/census-r/mapping-census-data-with-r.html


chicago <- read_sf("https://data.cityofchicago.org/resource/qqq8-j68g.geojson")

dlsd <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/dlsd.geojson")

ctalines <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/ctalines.geojson")

metralines <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/metra_lines.geojson")

lakemich <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/lakeblock.geojson")

#####################################################################################################################
#Looking at number of kids in tracts

cook_county <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook",
  variables = c(
    under_five = "B01001_003",
    five_to_nine = "B01001_004",
    ten_to_fourteen = "B01001_005",
    fifteen_to_seventeen = "B01001_006"
  ),
  summary_var = "B01001_001",
  year = 2022,
  geometry = TRUE
) %>% 
  select(-moe, -summary_moe) %>% 
  pivot_wider(names_from=variable,values_from = estimate) %>%
  mutate(kidshare = (under_five + five_to_nine + ten_to_fourteen + fifteen_to_seventeen) / summary_est)


#####################################################################################################################

ggplot(cook_county)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=kidshare),color=NA)+
  scale_fill_stepsn(name="Under 18",colours = c("purple", "#c3eac9", "#57bc65", "darkgreen"),breaks=c(0,.05,.1,.15,.2,.25,.3))+
  geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=0.8)+
  scale_color_identity()+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.8)+
  labs(title="Percent of population under 18",subtitle="Chicago",caption="Source: U.S. Census Bureau American Community Survery Data 2020")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))+
  coord_sf(xlim=c(-87.96,-87.515),ylim=c(41.625,42.05),expand=FALSE)+
  theme(plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=10),
        panel.grid.minor = element_blank(),
        #legend.position = c(0.2,0.4),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=15),
        text=element_text(family="Arial"),
        panel.background = element_rect(fill = "#fafbec",colour = "#eaeccf", size = 0.5, linetype = "solid"))


#####################################################################################################################
#Looking at share of transit types

cook_county <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook",
  variables = c(
    Bus = "B08006_009",
    L = "B08006_010",
    Metra = "B08006_011",
    Light_Rail = "B08006_012",
    Ferryboat = "B08006_012"
  ),
  summary_var = c(transit_commuters ="B08006_008"),
  year = 2022,
  geometry = TRUE
) %>% 
  select(-moe, -summary_moe) %>% 
  pivot_wider(names_from=variable,values_from = estimate) %>%
  mutate(All_Rail = L + Metra + Light_Rail) %>%
  mutate(mode =colnames(cook_county[,c(5,9)])[apply(cook_county[,c(5,9)],1,which.max)])
  

#####################################################################################################################

bustrain <- ggplot(cook_county)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=mode),color=NA)+
  scale_fill_manual(values=c("#CA7ECE","#FFC107","#A778CA"))+
  geom_sf(data=metralines,fill=NA,color="darkgrey",linewidth=0.5)+
  geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=1)+
  scale_color_identity()+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.4)+
  labs(title="Most Common Mode Among Transit Commuters",fill="Mode:",subtitle="All Bus Chicago Workers 16+",caption="Source: U.S. Census Bureau American Community Survery Data 2022")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))+
  coord_sf(xlim=c(-87.96,-87.515),ylim=c(41.625,42.05),expand=FALSE)+
  theme(plot.title = element_text(face="bold",size=25),
        plot.subtitle = element_text(size=15),
        panel.grid.minor = element_blank(),
        #legend.position = c(0.2,0.4),
        legend.key.size = unit(1, 'cm'),
        legend.position = c(0.9,0.8),
        legend.title = element_text(size=20),
        legend.text = element_text(size=15),
        text=element_text(family="Arial"),
        panel.background = element_rect(fill = "#fafbec",colour = "#eaeccf", size = 0.5, linetype = "solid"))

bustrain

ggsave(plot=bustrain, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/Chibustrain.jpeg",height=14,width=12)



options(viewer = NULL)
tmap_mode("view")
mapview(cook_county,zcol="mode",alpha=0.2)


#####################################################################################################################
#Looking at commute times

cook_county <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook",
  variables = c(
    travel_time = "B08013_001"
  ),
  summary_var = "B08014_001",
  year = 2022,
  geometry = TRUE
) %>% mutate(min_per_worker = estimate / summary_est)


#####################################################################################################################

ggplot(cook_county)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=min_per_worker),color=NA)+
  scale_fill_stepsn(name="Commute (minutes)",colours = c("blue","yellow","red"))+
  geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=0.8)+
  scale_color_identity()+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.8)+
  labs(title="Avg Commute Times",subtitle="Chicago Workers 16+",caption="Source: U.S. Census Bureau American Community Survery Data 2020")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))+
  coord_sf(xlim=c(-87.96,-87.515),ylim=c(41.625,42.05),expand=FALSE)+
  theme(plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=10),
        panel.grid.minor = element_blank(),
        #legend.position = c(0.2,0.4),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=15),
        text=element_text(family="Arial"),
        panel.background = element_rect(fill = "#fafbec",colour = "#eaeccf", size = 0.5, linetype = "solid"))


#####################################################################################################################
#Looking at vehicles available

cook_county <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook",
  variables = c(
    travel_time = "B08015_001"
  ),
  summary_var = "B08014_001",
  year = 2022,
  geometry = TRUE
) %>% mutate(vehicle_share = estimate / summary_est)


#####################################################################################################################

ggplot(cook_county)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=vehicle_share),color=NA)+
  scale_fill_stepsn(name="Vehicle Available",colours = c("#C2FBEC","#2063E4"),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))+
  #geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=0.8)+
  scale_color_identity()+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.8)+
  geom_sf(data=dlsd,fill=NA,color="#F87F85",linewidth=1)+
  labs(title="Motor Vehicles Available",subtitle="Chicago Workers 16+",caption="Source: U.S. Census Bureau American Community Survery Data 2020")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))+
  coord_sf(xlim=c(-87.96,-87.515),ylim=c(41.625,42.05),expand=FALSE)+
  theme(plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=10),
        panel.grid.minor = element_blank(),
        #legend.position = c(0.2,0.4),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=15),
        text=element_text(family="Arial"),
        panel.background = element_rect(fill = "#fafbec",colour = "#eaeccf", size = 0.5, linetype = "solid"))



#####################################################################################################################
#Take the BUS to work

cook_county <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook",
  variables = c(
    bus = "B08006_009"
  ),
  summary_var = "B08006_001",
  year = 2022,
  geometry = TRUE
) %>% mutate(bus_share = estimate / summary_est)


#####################################################################################################################

ggplot(cook_county)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=bus_share),color=NA)+
  scale_fill_stepsn(name="share of total workers:",colours = c("white","#C2FBEC","#262944"),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels = scales::label_percent())+
  #geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=0.8)+
  scale_color_identity()+
  geom_sf(data=chicago,fill=NA,color="gray",linewidth=0.3)+
  geom_sf(data=dlsd,fill=NA,color="#F87F85",linewidth=1)+
  labs(title="Chicagoans who take the bus to work",subtitle="Workers 16+",caption="Source: U.S. Census Bureau American Community Survery Data 2022")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))+
  coord_sf(xlim=c(-87.96,-87.515),ylim=c(41.625,42.05),expand=FALSE)+
  theme(plot.title = element_text(face="bold",size=15),
        plot.subtitle = element_text(size=10),
        panel.grid.minor = element_blank(),
        #legend.position = c(0.2,0.4),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        text=element_text(family="Arial"),
        panel.background = element_rect(fill = "#fafbec",colour = "#eaeccf", size = 0.5, linetype = "solid"))

##################
#Total Bus Users
bussers<- ggplot(cook_county)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=estimate),color=NA)+
  scale_fill_stepsn(name="Total Bus Users:",colours = c("#fafbec","#797591","#262944"),breaks=c(250,500,750,1000,1250,1500,1750),labels = scales::comma)+
  geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=0.8)+
  scale_color_identity()+
  geom_sf(data=chicago,fill=NA,color="darkgray",linewidth=0.5)+
  geom_sf(data=dlsd,fill=NA,color="#F87F85",linewidth=2)+
  geom_text(aes(x=-87.59,y=41.92,label="NDLSD"),color="#F87F85",size=10,face="bold")+
  labs(title="Chicago Bus Commuters",subtitle="Workers 16+",caption="Source: U.S. Census Bureau American Community Survery Data 2022")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))+
  coord_sf(xlim=c(-87.96,-87.515),ylim=c(41.625,42.05),expand=FALSE)+
  theme(plot.title = element_text(hjust = 0.01,vjust=-10,face="bold",size=35),
        plot.subtitle = element_text(hjust = 0.05,size=20,vjust=-20),
        plot.caption = element_text(size=10),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = c(0.9,0.8),
        text=element_text(family="Arial"),
        panel.background = element_rect(fill = "#fafbec",colour = "#eaeccf", size = 0.5, linetype = "solid"))

bussers

ggsave(plot=bussers, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/busriderstrn.jpeg",height=18,width=14)

#####################################################################################################################
#Take the BUS to work

cook_county <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook",
  variables = c(
    bus = "B08006_008"
  ),
  summary_var = "B08006_001",
  year = 2022,
  geometry = TRUE
) %>% mutate(bus_share = estimate / summary_est)

##################
#Total Bus Users
transitcommute<- ggplot(cook_county)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=estimate),color=NA)+
  scale_fill_stepsn(name="Total Transit Users:",colours = c("#fafbec","#797591","#262944"),labels = scales::comma)+
  geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=0.8)+
  scale_color_identity()+
  geom_sf(data=chicago,fill=NA,color="darkgray",linewidth=0.5)+
  geom_sf(data=dlsd,fill=NA,color="#F87F85",linewidth=2)+
  geom_text(aes(x=-87.59,y=41.92,label="NDLSD"),color="#F87F85",size=10,face="bold")+
  labs(title="Chicago Transit Commuters",subtitle="Workers 16+",caption="Source: U.S. Census Bureau American Community Survery Data 2022")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))+
  coord_sf(xlim=c(-87.96,-87.515),ylim=c(41.625,42.05),expand=FALSE)+
  theme(plot.title = element_text(hjust = 0.01,vjust=-10,face="bold",size=35),
        plot.subtitle = element_text(hjust = 0.05,size=20,vjust=-20),
        plot.caption = element_text(size=10),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = c(0.9,0.8),
        text=element_text(family="Arial"),
        panel.background = element_rect(fill = "#fafbec",colour = "#eaeccf", size = 0.5, linetype = "solid"))

transitcommute

ggsave(plot=transitcommute, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/transitcommuters.jpeg",height=18,width=14)

######################################################


library(mapview)

cook_block_groups <- block_groups("IL","Cook")
cook_tracts <- tracts("IL","Cook",cb=TRUE)

ggplot(cook_tracts) + 
  geom_sf() + 
  theme_void() + 
  labs(title = "Census tracts")

ggplot(cook_block_groups) + 
  geom_sf() + 
  theme_void() + 
  labs(title = "Census tracts")

options(viewer = NULL)
tmap_mode("view")
mapview(cook_county,zcol="estimate",alpha=0.2)


####################################################################






##################
#Old Code






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
