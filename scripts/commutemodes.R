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

ctastopkey <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata//sources/ctastopkey.csv")

metralines <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/metra_lines.geojson")

metrastops <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/metra_stations.geojson")

lakemich <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/lakeblock.geojson")

#####################################################################################################################
#Looking at share of transit types

cook_county_transit <- get_acs(
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
) 


cook_county_transit <-  cook_county_transit %>% select(-moe, -summary_moe) %>% 
  pivot_wider(names_from=variable,values_from = estimate) %>% 
  mutate(L = L + Light_Rail)

cook_county_transit <- subset(cook_county_transit, select = -c(Light_Rail) )
 
cook_county_transit <- cook_county_transit %>%
  mutate(mode =colnames(cook_county_transit[,c(5:7)])[apply(cook_county_transit[,c(5:7)],1,which.max)])
  

#####################################################################################################################

bustrain <- ggplot(cook_county_transit)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=mode),color=NA)+
  scale_fill_manual(values=c("#eaeccf","#2063E4","#A778CA"))+
  geom_sf(data=metralines,fill=NA,color="darkgrey",linewidth=0.5)+
  geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=1)+
  scale_color_identity()+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.4)+
  labs(title="Most Common Mode Among Transit Commuters",fill="Mode:",subtitle="Chicago Workers 16+",caption="Source: U.S. Census Bureau American Community Survery Data 2022")+
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

ggsave(plot=bustrain, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/ChiTransitMode.jpeg",height=14,width=12)



options(viewer = NULL)
tmap_mode("view")
mapview(cook_county,zcol="mode",alpha=0.2)


#####################################################################################################################
#Looking at light rail and L rail??

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
)  %>% 
  select(-moe, -summary_moe) %>% 
  pivot_wider(names_from=variable,values_from = estimate) %>%
  mutate(All_Rail = L + Metra + Light_Rail) #%>%
  #mutate(mode =colnames(cook_county[,c(6,8)])[apply(cook_county[,c(6,8)],1,which.max)])


#####################################################################################################################

bustrain <- ggplot(cook_county)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=Metra),color=NA)+
  scale_fill_continuous(low="lightgrey",high="darkgreen")+
    geom_sf(data=chicago,fill=NA,color="black",linewidth=0.4)+
  geom_sf(data=metralines,fill=NA,color="darkgrey",linewidth=0.5)+
  geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=1)+
  scale_color_identity()+
  labs(title="Chicago Rail Usage",fill="Users:",subtitle="Census data for riders using Long Distance Trains to commute",caption="Source: U.S. Census Bureau American Community Survery Data 2022")+
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

ggsave(plot=bustrain, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/ChiHeavyusage.jpeg",height=14,width=12)

############################################################################################

options(viewer = NULL)
tmap_mode("view")
mapview(cook_county,zcol="mode",alpha=0.2)

############################################################################################
#transitmode

cook_county_transit <- get_acs(
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
) 


cook_county_transit <-  cook_county_transit %>% select(-moe, -summary_moe) %>% 
  pivot_wider(names_from=variable,values_from = estimate) %>% 
  mutate(L = L + Light_Rail)

cook_county_transit <- subset(cook_county_transit, select = -c(Light_Rail) )

cook_county_transit <- cook_county_transit %>%
  mutate(mode =colnames(cook_county_transit[,c(5:7)])[apply(cook_county_transit[,c(5:7)],1,which.max)])


#####################################################################################################################

bustrain <- ggplot(cook_county_transit)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=mode),color=NA)+
  scale_fill_manual(values=c("#eaeccf","#2063E4","#A778CA","red"))+
  geom_sf(data=metralines,fill=NA,color="darkgrey",linewidth=0.5)+
  geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=1)+
  scale_color_identity()+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.4)+
  labs(title="Most Common Mode Among Transit Commuters",fill="Mode:",subtitle="Chicago Workers 16+",caption="Source: U.S. Census Bureau American Community Survery Data 2022")+
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

ggsave(plot=bustrain, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/ChiTransitMode.jpeg",height=14,width=12)



options(viewer = NULL)
tmap_mode("view")
mapview(cook_county_transit,zcol="mode",alpha=0.2)







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
bussers<- ggplot(cook_county_transit)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=Bus),color=NA)+
  scale_fill_stepsn(name="Total Bus Users:",colours = c("#fafbec","#797591","#262944"),breaks=c(250,500,750,1000,1250,1500,1750),labels = scales::comma)+
  geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=0.8)+
  scale_color_identity()+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.5)+
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

ggsave(plot=bussers, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/busriders.jpeg",height=18,width=14)

#####################################################################################################################
#Car Riders

cook_county <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook",
  variables = c(
    car = "B08006_002"
  ),
  summary_var = "B08006_001",
  year = 2022,
  geometry = TRUE
) %>% mutate(car_share = estimate / summary_est)

##################
#Share of car commuters
carcommute<- ggplot(cook_county)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=car_share),color=NA)+
  scale_fill_stepsn(name="Car Commute Share:",colours = c("red3","yellow2","white"),breaks=c(0.2,0.4,0.6,0.8),labels = scales::percent)+
  geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=0.8)+
  scale_color_identity()+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.5)+
  geom_sf(data=dlsd,fill=NA,color="navy",linewidth=2)+
  geom_text(aes(x=-87.59,y=41.92,label="NDLSD"),color="navy",size=10,face="bold")+
  labs(title="Chicago Car Commuters",subtitle="Workers 16+",caption="Source: U.S. Census Bureau American Community Survery Data 2022")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))+
  coord_sf(xlim=c(-87.96,-87.515),ylim=c(41.625,42.05),expand=FALSE)+
  theme(plot.title = element_text(hjust = 0.01,vjust=-10,face="bold",size=35),
        plot.subtitle = element_text(hjust = 0.05,size=20,vjust=-20),
        plot.caption = element_text(size=10),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=20), hjust=0.5,
        legend.text = element_text(size=20),
        legend.position = c(0.9,0.8),
        text=element_text(family="Arial"),
        panel.background = element_rect(fill = "#fafbec",colour = "#eaeccf", size = 0.5, linetype = "solid"))

carcommute

ggsave(plot=carcommute, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/carcommuters.jpeg",height=18,width=14)

######################################################

#Share of car commuters
carcommute2<- ggplot(cook_county)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=car_share),color=NA)+
  scale_fill_stepsn(name="Car Commute Share:",colours = c("red3","yellow2","white","lightblue","blue3"),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),labels = scales::percent)+
  geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=0.8)+
  scale_color_identity()+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.5)+
  geom_sf(data=metralines,fill=NA,color="darkgrey",linewidth=0.5)+
  #geom_sf(data=dlsd,fill=NA,color="navy",linewidth=2)+
  #geom_text(aes(x=-87.59,y=41.92,label="NDLSD"),color="navy",size=10,face="bold")+
  labs(title="Chicago Car Commuters",subtitle="Workers 16+",caption="Source: U.S. Census Bureau American Community Survery Data 2022")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))+
  coord_sf(xlim=c(-88,-87.515),ylim=c(41.625,42.12),expand=FALSE)+
  theme(plot.title = element_text(hjust = 0.01,face="bold",size=35),
        plot.subtitle = element_text(hjust = 0.05,size=20),
        plot.caption = element_text(size=10),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=16),
        legend.position = c(0.85,0.8),
        text=element_text(family="Arial"),
        panel.background = element_rect(fill = "#fafbec",colour = "#eaeccf", size = 0.5, linetype = "solid"))

carcommute2

ggsave(plot=carcommute2, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/carcommuters2.jpeg",height=18,width=14)

mapviewPalette()

options(viewer = NULL)
tmap_mode("view")
mapview(cook_county,zcol="car_share",alpha=0.05,col.regions =mapviewPalette("mapviewTopoColors"), legend = TRUE)


#########################################
#Oakwood Gateway

oakwood<- ggplot(cook_county)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=car_share),color=NA)+
  scale_fill_stepsn(name="Car Commute Share:",colours = c("red3","yellow2","white","lightblue","blue3"),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),labels = scales::percent)+
  geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=0.8)+
  geom_point(data=ctastopkey,aes(x=lng,y=lat,color=Hex))+
  scale_color_identity()+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.5)+
  geom_sf(data=metralines,fill=NA,color="darkgrey",linewidth=0.5)+
  geom_text(x=-87.602296,y=41.824735,label="Oakwood Blvd",hjust=-.3)+
  geom_point(x=-87.602296,y=41.824735,color="black",size=3,shape=17)+
  geom_sf(data=metrastops,color="darkgrey")+
  #geom_sf(data=dlsd,fill=NA,color="navy",linewidth=2)+
  #geom_text(aes(x=-87.59,y=41.92,label="NDLSD"),color="navy",size=10,face="bold")+
  labs(title="Chicago Car Commuters",subtitle="Workers 16+",caption="Source: U.S. Census Bureau American Community Survery Data 2022")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))+
  coord_sf(xlim=c(-87.75,-87.515),ylim=c(41.725,41.9),expand=FALSE)+
  theme(plot.title = element_text(hjust = 0.01,face="bold",size=35),
        plot.subtitle = element_text(hjust = 0.05,size=20),
        plot.caption = element_text(size=10),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=14),
        legend.text = element_text(size=10),
        legend.position = c(0.85,0.85),
        text=element_text(family="Arial"),
        panel.background = element_rect(fill = "#fafbec",colour = "#eaeccf", size = 0.5, linetype = "solid"))

oakwood

ggsave(plot=oakwood, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/oakwood.jpeg",height=14.5,width=14)



#####################################################################################################################
#Take the METRA to work

metrarider <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook",
  variables = c(
    Metra = "B08006_011"
  ),
  summary_var = "B08006_008",
  year = 2022,
  geometry = TRUE
) %>% mutate(metra_share = estimate / summary_est)

ggplot(metrarider)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=metra_share),color=NA)+
  scale_fill_stepsn(name="Total Metra Users:",colours = c("#fafbec","#797591","#A778CA"),labels = scales::comma)+
  geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=0.8)+
  scale_color_identity()+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.5)+
  geom_sf(data=dlsd,fill=NA,color="#F87F85",linewidth=2)+
  geom_text(aes(x=-87.59,y=41.92,label="NDLSD"),color="#F87F85",size=10,face="bold")+
  labs(title="Chicago Metra Commuters",subtitle="Transit Commuters age 16+",caption="Source: U.S. Census Bureau American Community Survery Data 2022")+
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

options(viewer = NULL)
tmap_mode("view")
mapview(metrarider,zcol="metra_share",alpha=0.05,col.regions =mapviewPalette("mapviewTopoColors"), legend = TRUE)




#####################################################################################################################
#Walk to work

walkcommute <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook",
  variables = c(
    Metra = "B08006_015"
  ),
  summary_var = "B08006_001",
  year = 2022,
  geometry = TRUE
) %>% mutate(walk_share = estimate / summary_est)

walkmap <- ggplot(walkcommute)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=walk_share),color=NA)+
  scale_fill_stepsn(name="Walk Share:",colours = c("#fafbec","#eaeccf","lightgreen","lightgreen","green3","green4","darkgreen"),labels = scales::percent)+
  geom_sf(data=metralines,fill=NA,color="darkgrey",linewidth=0.5)+
  geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=0.8)+
  scale_color_identity()+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.5)+
  #geom_sf(data=dlsd,fill=NA,color="#F87F85",linewidth=2)+
  #geom_text(aes(x=-87.59,y=41.92,label="NDLSD"),color="#F87F85",size=10,face="bold")+
  labs(title="Chicago Walk Commuters",subtitle="Commuters age 16+",caption="Source: U.S. Census Bureau American Community Survery Data 2022")+
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


walkmap

ggsave(plot=walkmap, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/walkmap.jpeg",height=14.5,width=11)



options(viewer = NULL)
tmap_mode("view")
mapview(walkcommute,zcol="walk_share",alpha=0.05,col.regions =mapviewPalette("mapviewTopoColors"), legend = TRUE)


