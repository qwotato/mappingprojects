library(sf)
library(tidyverse)
library(extrafont)


#Uploading source files

chiwards2023 <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/chiwards2023.geojson")

lakemich <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/lakeblock.geojson")

cookcounty <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/cookcounty.geojson")

votesource <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/citycouncil/councilvotekey/councilvotesource.csv")


#Joining JSON file with relevant vote
chiwards2023$ward <- as.integer(chiwards2023$ward)
chiwards2023 <- inner_join(chiwards2023,votesource, by="ward")


#Creating basemap for city council voting visualizations

basemap <- ggplot() +
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(data = cookcounty,fill="#fafbec",color="#eaeccf")+
  scale_fill_identity()+
  scale_color_identity()+
  theme_void()+
  theme(panel.background = element_rect(fill = "#fafbec",colour = "#eaeccf", size = 0.5, linetype = "solid"))

#

#RTA Medina


votemap <- basemap+
  geom_sf(data = chiwards2023,aes(fill=RTA_Medina),color="black")+
  scale_fill_manual(values=c("#f75261","lightgrey","#6f8ff1"))+
  geom_sf_label(data = chiwards2023, aes(label=ward,fill=RTA_Medina),show.legend=FALSE)+
  labs(title="Chicago RTA Nomination - Jarixon Medina",subtitle="10-30-2024",fill="Vote:")+
  coord_sf(xlim=c(-87.96,-87.515),ylim=c(41.625,42.05),expand=FALSE)+
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=25),
        plot.subtitle = element_text(hjust = 0.5,size=20),
        panel.grid.minor = element_blank(),
        legend.position = c(0.25,0.5),
        legend.key.size = unit(2, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=15),
        text=element_text(family="Arial"))

votemap

ggsave(plot=votemap, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/citycouncil/plots/RTA_Medina.jpeg",height=18,width=14)








# Mapping votes over basemap for speed camera threshold change

speedcamvotemap <- basemap+
   geom_sf(data = chiwards2023,aes(fill=stay6over),color="black")+
   scale_fill_manual(values=c("white","#f75261","lightgrey","#6f8ff1"),labels=c("Not in Office","Raise Threshold","No Vote","Remain at 6"))+
   geom_sf_label(data = chiwards2023, aes(label=ward,fill=stay6over),show.legend=FALSE)+
   labs(title="Chicago Speed Camera Threshold Vote",subtitle="2022 Measure to raise speed camera threshold from 6mph to 10mph",fill="Vote:")+
   coord_sf(xlim=c(-87.96,-87.515),ylim=c(41.625,42.05),expand=FALSE)+
   theme(plot.title = element_text(hjust = 0.5,face="bold",size=25),
         plot.subtitle = element_text(hjust = 0.5,size=20),
         panel.grid.minor = element_blank(),
         legend.position = c(0.25,0.5),
         legend.key.size = unit(2, 'cm'),
         legend.title = element_text(size=20),
         legend.text = element_text(size=15),
         text=element_text(family="Arial"))
  
speedcamvotemap

ggsave(plot=speedcamvotemap, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/citycouncil/plots/speedcamvoteaspng.png",height=18,width=14)



# Mapping votes over basemap for speed limit change

speedvotemap <- basemap+
  geom_sf(data = chiwards2023,aes(fill=speedlimit))+
  scale_fill_manual(values=c("lightgrey","#f75261","#f5949c","#b5c4f3","#6f8ff1"))+
  geom_sf_label(data = chiwards2023, aes(label=ward,fill=speedlimit),show.legend=FALSE)+
  labs(title="Chicago Speed Limit Vote",subtitle="Measure to lower residential speed limit from 30mph to 25mph",fill="Vote:")+
  coord_sf(xlim=c(-87.96,-87.515),ylim=c(41.625,42.08),expand=FALSE)+
  theme(plot.title = element_text(hjust = 0.01,vjust=-10,face="bold",size=35),
        plot.subtitle = element_text(hjust = 0.05,size=20,vjust=-20),
        panel.grid.minor = element_blank(),
        legend.position = c(0.9,0.8),
        legend.key.size = unit(2, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=15),
        text=element_text(family="Arial"))

speedvotemap

ggsave(plot=speedvotemap, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/citycouncil/plots/speedvote.png",height=18,width=14)




# Mapping votes over basemap for shotspotter

shotspot <- basemap+
  geom_sf(data = chiwards2023,aes(fill=shotspot),color="black")+
  scale_fill_manual(values=c("#f75261","lightgrey","#6f8ff1"))+
  geom_sf_label(data = chiwards2023, aes(label=ward,fill=shotspot),show.legend=FALSE)+
  labs(title="Chicago ShotSpotter Vote",subtitle="Vote Favoring Shotspotter Extension",fill="Vote:")+
  coord_sf(xlim=c(-87.96,-87.515),ylim=c(41.625,42.05),expand=FALSE)+
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=25),
        plot.subtitle = element_text(hjust = 0.5,size=20),
        panel.grid.minor = element_blank(),
        legend.position = c(0.4,0.5),
        legend.key.size = unit(2, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=15),
        text=element_text(family="Arial"))

shotspot

ggsave(plot=shotspot, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/citycouncil/plots/shotspot.png",height=18,width=14)
