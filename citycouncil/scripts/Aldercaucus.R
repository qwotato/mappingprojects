library(sf)
library(ggplot2)
library(tidyverse)
library(extrafont)
library(geosphere)
library(ggrepel)
library(seasonal)
library(scales)
library(RColorBrewer)
library(directlabels)
library(gt)
library(gridExtra)



chiwards2023 <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/chiwards2023.geojson")

wardcaucus <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/citycouncil/wardcalls.csv")

wardcaucus$tenure <- 2024 - wardcaucus$Took_Office

wardcaucus$ward <- as.integer(wardcaucus$ward)
chiwards2023$ward <- as.integer(chiwards2023$ward)

wardcaucus <- inner_join(chiwards2023,wardcaucus, by="ward")

sum(warddata$tenure) / 50



######################################################################################################################################################


caucusmapall <- ggplot()+
  geom_sf(data = wardcaucus,aes(fill = as.factor(Caucus)),alpha=0.9)+
  geom_sf_label(data=wardcaucus,aes(label = ward,fill=as.factor(Caucus)),size=2,hjust="left",vjust=1.5,show.legend = F)+
  geom_sf_label(data=wardcaucus,aes(label = alder_lastname,fill=as.factor(Caucus)),size=2.5,alpha=0.9,hjust="left",show.legend = F)+
  scale_fill_manual(values=c("white","#7bcb73","#9a6c7d","pink","#669696","#a6a87b","#f5610a","#fffdc8","#B3DDF2","#da8fff","#FF0000"))+
  geom_text(aes(x=-87.52, y=41.642821),label="Source: chicago.councilmatic.org/compare-council-members/",size=2.5,hjust="right")+
  labs(title="Chicago City Council: Caucus",
       subtitle="As of 2024",
       fill="Caucus Affiliations",
  )+
  theme_void()+
  theme(legend.position=c(0.3,0.55))+
  theme(plot.title=element_text(hjust=0.1,vjust=-11.5))+
  theme(plot.subtitle=element_text(hjust=0.1,vjust=-15))+
  theme(panel.background = element_rect(fill = '#fcfbf7', color = 'black'))+
  theme(text=element_text(family="Tahoma"))

caucusmapall

ggsave(plot=caucusmapall, "caucusmapall.png",heigh=11,width=8.5)




######################################################################################################################################################
caucusmapbl <- ggplot()+
  geom_sf(data = wardcaucus,aes(fill = as.factor(Caucus)),alpha=0.9)+
  geom_sf_label(data=wardcaucus,aes(label = ward,fill=as.factor(Caucus)),size=2,hjust="left",vjust=1.5,show.legend = F)+
  geom_sf_label(data=wardcaucus,aes(label = alder_lastname,fill=as.factor(Caucus)),size=2.5,alpha=0.9,hjust="left",show.legend = F)+
  scale_fill_manual(values=c("white","#7bcb73","#7bcb73","#7bcb73","#7bcb73","#fffdc8","#fffdc8","#fffdc8","#fffdc8","white","white"))+
  geom_text(aes(x=-87.52, y=41.642821),label="Source: chicago.councilmatic.org/compare-council-members/",size=2.5,hjust="right")+
  labs(title="Chicago City Council: Caucus",
       subtitle="As of 2024",
       fill="Caucus Affiliations",
  )+
  theme_void()+
  theme(legend.position=c(0.3,0.55))+
  theme(plot.title=element_text(hjust=0.1,vjust=-11.5))+
  theme(plot.subtitle=element_text(hjust=0.1,vjust=-15))+
  theme(panel.background = element_rect(fill = '#fcfbf7', color = 'black'))+
  theme(text=element_text(family="Tahoma"))

caucusmapbl

ggsave(plot=caucusmapbl, "caucusmapbl.png",heigh=11,width=8.5)




######################################################################################################################################################

caucusmaplgbt <- ggplot()+
  geom_sf(data = wardcaucus,aes(fill = as.factor(LGBT_Caucus)),alpha=0.9)+
  geom_sf_label(data=wardcaucus,aes(label = alder_lastname,fill=as.factor(LGBT_Caucus)),size=2.5,alpha=0.9,hjust="left",show.legend = F)+
  scale_fill_manual(values=c("white","#da8fff"))+
  labs(title="Chicago City Council: Caucus",
       subtitle="As of 2024",
       fill="LGBT Caucus",
       )+
  theme_void()+
  theme(legend.position=c(0.3,0.55))+
  theme(plot.title=element_text(hjust=0.1,vjust=-11.5))+
  theme(plot.subtitle=element_text(hjust=0.1,vjust=-15))+
  theme(panel.background = element_rect(fill = '#fcfbf7', color = 'black'))+
  theme(text=element_text(family="Tahoma"))
  
caucusmaplgbt

ggsave(plot=caucusmaplgbt, "caucusmaplgbt.png",heigh=11,width=8.5)

######################################################################################################################################################

caucusmapprog <- ggplot()+
  geom_sf(data = wardcaucus,aes(fill = as.factor(Progressive_Caucus)),alpha=0.9)+
  geom_sf_label(data=wardcaucus,aes(label = ward,fill=as.factor(Progressive_Caucus)),size=2,hjust="left",vjust=1.5,show.legend = F)+
  geom_sf_label(data=wardcaucus,aes(label = alder_lastname,fill=as.factor(Progressive_Caucus)),size=2.5,alpha=0.9,hjust="left",show.legend = F)+
  geom_text(aes(x=-87.52, y=41.642821),label="Source: chicago.councilmatic.org/compare-council-members/",size=2.5,hjust="right")+
  scale_fill_manual(values=c("white","#f57369"))+
  labs(title="Chicago City Council: Caucus",
       subtitle="As of 2024",
       fill="Progressive Caucus",
  )+
  theme_void()+
  theme(legend.position=c(0.3,0.55))+
  theme(plot.title=element_text(hjust=0.1,vjust=-11.5))+
  theme(plot.subtitle=element_text(hjust=0.1,vjust=-15))+
  theme(panel.background = element_rect(fill = '#fcfbf7', color = 'black'))+
  theme(text=element_text(family="Tahoma"))

caucusmapprog

ggsave(plot=caucusmapprog, "caucusmapprog.png",heigh=11,width=8.5)

wardcaucus %>% group_by(Progressive_Caucus) %>% sum(wardcaucus$tenure)

sum(wardcaucus$tenure)



############################################


caucusmaplgbt <- ggplot()+
  geom_sf(data = wardcaucus,aes(fill = as.factor(Gays)),alpha=0.6)+
  geom_sf_label(data=wardcaucus,aes(label = alder_lastname,fill=as.factor(Gays)),size=3,alpha=0.6,hjust="left",show.legend = F)+
  scale_fill_manual(values=c("white","red","orange","yellow","green","blue","#4b0082","violet","red","orange"))+
  labs(title="Chicago City Council: LGBT Council Members",
       subtitle="2024",
       fill="LGBT",
  )+
  theme_void()+
  theme(legend.position="none")+
  theme(plot.title=element_text(hjust=0.1,vjust=-11.5))+
  theme(plot.subtitle=element_text(hjust=0.1,vjust=-15))+
  theme(panel.background = element_rect(fill = '#fcfbf7', color = 'black'))+
  theme(text=element_text(family="Tahoma"))

caucusmaplgbt

ggsave(plot=caucusmaplgbt, "caucusmapgay.png",height=11,width=8.5)
