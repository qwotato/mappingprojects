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

cook_county <- get_decennial(
  geography = "block",
  state = "IL",
  county = "Cook",
  variables = c(
    Population = "P2_001N",
    hispanic = "P2_002N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(occupied = 100 * (value / value))

# remove empty geometry
cook_county <- cook_county %>% filter(!st_is_empty(cook_county))

cook_county <- cook_county %>% filter(variable == "Population")



tm_shape(cook_county) + 
  tm_polygons()


tm_shape(cook_county) + 
  tm_polygons(col = "summary_value")


tm_shape(cook_county) + 
  tm_polygons(col= "summary_value",title="Cook County - 2020 Cencus Tract Population") + 
  tm_layout(legend.outside = FALSE,
            legend.outside.position = "top")

  
popmap

ctalstops <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/ctalstops.csv")

orangeline <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/orangeline.geojson") 

ctaorangestops <- ctalstops %>% filter(O == "TRUE")

ctabrownstops <- ctalstops %>% filter(BRN == "TRUE")

ctapinkstops <- ctalstops %>% filter(Pnk == "TRUE")

orangelinepop <- ggplot(cook_county)+
  geom_sf(aes(fill=value))+
  geom_sf(data=orangeline,color="orange",linewidth=2)+
  geom_point(data=ctaorangestops, aes(x=lng, y= lat),color="black",size=3)+
  geom_point(data=ctaorangestops, aes(x=lng, y= lat),color="black",pch=1,size=40)+
  geom_label(data=ctaorangestops, aes(x=lng, y= lat,label=STATION_NAME),color="black",vjust=-3.2,fill="orange")+
  scale_fill_continuous(low="#F2F4FF",high="#002BFF",limits=c(1,750),name="population")+
  labs(title= "Orange Line Walksheds", subtitle ="Midway to Halsted",)+
  coord_sf(xlim=c(-87.762617,-87.634957),ylim=c(41.774591,41.859),expand=FALSE)+
  theme_void()+
  theme(text=element_text(family="Tahoma"))

ggsave(plot=orangelinepop, "orangelinepop.jpg",height=10,width=10)


ggplot(cook_county)+
  geom_sf(aes(fill=occupied))+
  geom_sf(data=orangeline,color="orange",linewidth=2)+
  geom_point(data=ctaorangestops, aes(x=lng, y= lat),color="black",size=3)+
  geom_point(data=ctaorangestops, aes(x=lng, y= lat),color="black",pch=1,size=46.5)+
  geom_label(data=ctaorangestops, aes(x=lng, y= lat,label=STATION_NAME),color="black",vjust=-2.3)+
  scale_fill_continuous(low="white",high="#813ba4",name="population")+
  labs(title= "Orange Line Walksheds", subtitle ="Midway to Halsted",)+
  coord_sf(xlim=c(-87.762617,-87.634957),ylim=c(41.774591,41.859),expand=FALSE)+
  theme_void()+
  theme(text=element_text(family="Tahoma"))


boplines <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/boplines.geojson") 

boprides <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/boprides.csv")

boprides <- filter(boprides, Branch!= "Mixed")

boprides$apr_24_avg_weekday_rides <- as.numeric(apr_24_avg_weekday_rides)



orecovery <- ggplot()+

  geom_sf(data=boplines,aes(color=Color),linewidth=2)+
  geom_text(data=boprides, aes(x=lng, y= lat, label = STATION_NAME),vjust=-1.5,hjust=1,size=4)+
  scale_color_manual(values=c("#62361b","black","#f9461c","#e27ea6","green"))+
  geom_point(data=boprides, aes(x=lng, y= lat, color = Color),pch=1,size=10)+
  geom_point(data=boprides, aes(x=lng, y= lat,color=Color,alpha=recovery),size=10)+
  geom_text(data=boprides, aes(x=lng, y= lat, label = scales::percent(recovery)),color="black",size=4)+
  geom_text(data=boprides, aes(x=lng, y= lat, label = paste("Apr '19:",apr_19_avg_weekday_rides)),vjust=4.5,color="black",size=2)+
  geom_text(data=boprides, aes(x=lng, y= lat, label = paste("Apr '24:",apr_24_avg_weekday_rides)),vjust=6,color="black",size=2)+
  labs(title="CTA Ridership Recovery - Orange Line ",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",fill="%")+
  #coord_sf(xlim=c(-87.762617,-87.634957),ylim=c(41.774591,41.85),expand=FALSE)+
  theme_void()+
  theme(text=element_text(family="Tahoma"))

orecovery

ggsave(plot=orecovery, "orecovery.jpg",height=10,width=10)

ggplot()+
    geom_point(data=boprides, aes(x=recovery,y=apr_24_avg_weekday_rides,color=Color))+
  geom_text_repel(data=boprides, aes(x=recovery,y=apr_24_avg_weekday_rides,label=STATION_NAME,color=Color))+
    scale_color_manual(values=c("#62361b","#f9461c","#e27ea6"))+
    theme_classic()+
  theme(text=element_text(family="Tahoma"))

ggplot()+
  geom_point(data=boprides, aes(x=recovery,y=X24_ridepop,color=Color))+
  geom_text_repel(data=boprides, aes(x=recovery,y=X24_ridepop,label=STATION_NAME,color=Color))+
  scale_color_manual(values=c("#62361b","#f9461c","#e27ea6"))+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))


ggplot()+
  geom_point(data=boprides, aes(x=Color,y=recovery,color=Color))+
  geom_text(data=boprides, aes(x=Color,y=recovery,label=STATION_NAME,hjust=1.2,color=Color))+
  scale_color_manual(values=c("#62361b","#f9461c","#e27ea6"))+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))


ggplot()+
  geom_boxplot(data=boprides, aes(x=Color,y=recovery,fill=Color),color="black",alpha=0.4)+
  geom_text_repel(data=boprides, aes(x=Color,y=recovery,label=STATION_NAME,hjust=1.2,color=Color))+
  theme_classic()+
  scale_fill_manual(values=c("#62361b","#f9461c","#e27ea6","green"))+
  scale_color_manual(values=c("#62361b","#f9461c","#e27ea6","green"))+
  theme(text=element_text(family="Tahoma"))

ggplot()+
  geom_boxplot(data=boprides, aes(x=Color,y=apr_24_avg_weekday_rides,fill=Color),color="black",alpha=0.4)+
  geom_text_repel(data=boprides, aes(x=Color,y=apr_24_avg_weekday_rides,label=STATION_NAME,hjust=1.2,color=Color))+
  theme_minimal()+
  scale_fill_manual(values=c("#62361b","#f9461c","#e27ea6","green"))+
  scale_color_manual(values=c("#62361b","#f9461c","#e27ea6","green"))+
  theme(text=element_text(family="Tahoma"))


ggplot()+
  geom_boxplot(data=boprides, aes(x=Color,y=walkshedrideshare,fill=Color),color="black",alpha=0.4)+
  geom_text_repel(data=boprides, aes(x=Color,y=walkshedrideshare,label=STATION_NAME,hjust=1.2,color=Color))+
  theme_minimal()+
  scale_fill_manual(values=c("#62361b","#f9461c","#e27ea6","green"))+
  scale_color_manual(values=c("#62361b","#f9461c","#e27ea6","green"))+
  theme(text=element_text(family="Tahoma"))





ctalines <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/ctalines.geojson") 

chicago <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/chicagooutline.json")

ggplot()+
  geom_sf(data=chicago,fill="white")+
  geom_sf(data=ctalines,aes(color=Color),linewidth=2)+
  scale_color_manual(values=c("#00a1de","#62361b","#009b3a","black","#f9461c","#e27ea6","#522398","#c60c30","red","red"))+
  theme_void()+
  theme(text=element_text(family="Tahoma"))



layer_static_mapbox(
  location = NULL,
  buffer_dist = 1000,
  units = "m",
  style_id,
  username,
  style_url = NULL,
  overlay_sf = NULL,
  overlay_style = NULL,
  overlay_markers = NULL,
  width = NULL,
  height = NULL,
  scale = 0.5,
  scaling_factor = c("1x", "2x"),
  attribution = TRUE,
  logo = TRUE,
  before_layer = NULL,
  access_token = NULL,
)
1
