
library(tidyverse)
library(extrafont)
library(sf)
library(grid)
library(gridExtra)

library(geosphere)
library(ggrepel)
library(seasonal)
library(scales)
library(RColorBrewer)
library(directlabels)
library(gt)
library(gridExtra)





chiwards2023 <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/chiwards2023.geojson")

wardcalls <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/citycouncil/wardcalls.csv")

wardcalls$tenure <- 2024 - wardcalls$Took_Office

wardcalls$ward <- as.integer(wardcalls$ward)
chiwards2023$ward <- as.integer(chiwards2023$ward)

warddata <- inner_join(chiwards2023,wardcalls, by="ward")

sum(warddata$tenure) / 50

wardtable <- st_drop_geometry(warddata[,c(7,9,14)])

wardtenure <- ggplot()+
  geom_sf(data = warddata,aes(fill = tenure),alpha=0.9)+
  scale_fill_continuous(low="white",high="#03a1fc")+
  geom_sf_label(data=warddata,aes(label = tenure,fill=tenure),size=2.5,vjust=1.5,alpha=0.9,hjust="left")+
  geom_sf_label(data=warddata,aes(label = alder_lastname,fill=tenure),size=2.5,alpha=0.9,hjust="left")+
  labs(title="Chicago City Council: Years in Office",subtitle="As of 2024")+
  theme_void()+
  theme(legend.position=c(0.3,0.55))+
  theme(plot.title=element_text(hjust=0.1,vjust=-11.5))+
  theme(plot.subtitle=element_text(hjust=0.1,vjust=-15))+
  labs(fill="Tenure (Years)")+
  theme(panel.background = element_rect(fill = '#fcfbf7', color = 'black'))+
  theme(text=element_text(family="Tahoma"))
  
wardtenure

ggsave(plot=wardtenure, "wardtenure.png",heigh=11,width=8.5)


ggplot()+
  geom_sf(data = warddata,aes(fill = Took_Office),alpha=0.9)+
  labs(title="Chicago City Council: Time in Office",subtitle="As of 2024",fill="Took Office:")+
  scale_fill_continuous(low="#03a1fc",high="white")+
  geom_sf_label(data=warddata,aes(label = tenure,fill=Took_Office),size=2.5,vjust=1.5,alpha=0.9,hjust="left")+
  geom_sf_label(data=warddata,aes(label = alder_lastname,fill=Took_Office),size=2.5,alpha=0.9,hjust="left")+
  annotation_custom(tableGrob(wardtable, rows=NULL),xmin=-87.95,ymin=41.65,xmax=-87.8,ymax=41.9)+
  theme_void()+
  theme(text=element_text(family="Tahoma"),legend.position=c(0.4,0.5))



