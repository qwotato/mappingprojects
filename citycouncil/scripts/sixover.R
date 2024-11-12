library(sf)
library(tidyverse)
library(extrafont)


#Uploading source files

chiwards2023 <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/chiwards2023.geojson")

lakemich <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/lakeblock.geojson")

cookcounty <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/cookcounty.geojson")

speedlimitvote <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/citycouncil/speedlimitvote.csv")


#Joining JSON file with relevant vote
chiwards2023$ward <- as.integer(chiwards2023$ward)
chiwards2023 <- inner_join(chiwards2023,speedlimitvote, by="ward")


#Creating basemap for this vis

basemap <- ggplot() +
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(data = cookcounty,fill="#fafbec",color="#eaeccf")+
  scale_fill_identity()+
  scale_color_identity()+
  theme_void()+
  theme(panel.background = element_rect(fill = "#fafbec",colour = "#eaeccf", size = 0.5, linetype = "solid"))


# Mapping votes over basemap for speed camera change

speedvotemap <- basemap+
   geom_sf(data = chiwards2023,aes(fill=stay6over))+
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
  
speedvotemap

ggsave(plot=speedvotemap, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/citycouncil/plots/speedcamvote.jpeg",height=18,width=14)



# Mapping votes over basemap for speed limit change

speedvotemap <- basemap+
  geom_sf(data = chiwards2023,aes(fill=speedlimit))+
  scale_fill_manual(values=c("lightgrey","#f75261","#f5949c","#b5c4f3","#6f8ff1"))+
  geom_sf_label(data = chiwards2023, aes(label=ward,fill=speedlimit),show.legend=FALSE)+
  labs(title="Chicago Speed Limit Vote",subtitle="Measure to lower residential speed limit from 30mph to 25mph",fill="Vote:")+
  coord_sf(xlim=c(-87.96,-87.515),ylim=c(41.625,42.05),expand=FALSE)+
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=25),
        plot.subtitle = element_text(hjust = 0.5,size=20),
        panel.grid.minor = element_blank(),
        legend.position = c(0.25,0.5),
        legend.key.size = unit(2, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=15),
        text=element_text(family="Arial"))

speedvotemap

ggsave(plot=speedvotemap, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/citycouncil/plots/speedvote.jpeg",height=18,width=14)

