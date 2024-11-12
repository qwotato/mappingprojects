
library(sf)
library(tidyverse)
library(extrafont)
library(ggrepel)

############################################################################################

#Chicago population by neighborhood area. 1950 and 2020
chiphist <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/sourcedata/chi_nhood_pop_history.csv")


#Density plot of max decade vs current

maxdensity <- ggplot(chiphist)+
  geom_density(aes(D2020),fill="#903162",color="black",linewidth=1,alpha=0.5)+
  geom_density(aes(maxdensity),fill="#41b6e6",color="black",linewidth=1,alpha=0.5)+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::comma,breaks=scales::pretty_breaks(n=10))+
  labs(y="Share of Population",x="Persons Per Square Mile",title="Chicago Community Areas - Historical Density ",subtitle="2020 (purple) vs Densest Decade (blue)",fill="Year")+
  theme_minimal()+
  theme(text=element_text(family="Arial"))

maxdensity

ggsave(plot=maxdensity, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/maxdensity.jpeg")


#bar plot of how many community areas had peak density in a given decade

decadepeaks <- ggplot(chiphist)+
  geom_bar(aes(x=maxdecade,fill=as.character(maxdecade)),color="black")+
  scale_fill_brewer(palette="GnBu",name="Decade")+
  labs(x="Decade:",title="Chicago Community Areas: Max Density Decade",subtitle="How many community areas hit peak density in each decade- 1930 to 2020.",fill="Year")+
  scale_x_continuous(breaks=scales::pretty_breaks(n=10))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),
        panel.border = element_blank(), plot.caption = element_text( color="#404040"),
        plot.subtitle = element_text(hjust = 0.5),panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(family="Arial"))

decadepeaks

ggsave(plot=decadepeaks, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/decadepeaks.jpg")


#######################################################################################################################################################
#Density 1950 vs 2020, split out by N W S Central


areapopchi2 <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/sourcedata/chihoodpop2.csv")

chipopsum <- areapopchi2 %>% group_by(popsqmi,year) %>% summarise(yrshare=sum(yrshare))


nhooddensity <- ggplot(areapopchi2)+
  geom_density(aes(density,fill=as.character(Region2),group=Region2),color="black",size=1,alpha=0.5)+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::comma,breaks=scales::pretty_breaks(n=10))+
  scale_fill_manual(values=c("#41b6e6","#903162","lightgreen","orange"))+
  facet_grid( . ~ year)+
  labs(y="Share of Population",x="Persons per Square Mile",title="Chicago Density by Community Areas",subtitle="1950 vs 2020",fill="Region")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),
        panel.border = element_blank(), plot.caption = element_text( color="#404040"),
        plot.subtitle = element_text(hjust = 0.5),panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(family="Arial"))

nhooddensity

ggsave(plot=nhooddensity, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/densesides.jpg",height=8,width=10)


####################################################################################################################################################
#this gets me the map of chicago with community areas
chi_map <- read_sf("https://raw.githubusercontent.com/thisisdaryn/data/master/geo/chicago/Comm_Areas.geojson") 
lakemich <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/lakeblock.geojson")
cookcounty <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/cookcounty.geojson")


chi_map$area_num_1 <- as.integer(chi_map$area_num_1)
chi_map$Region <-  c(2,3,3,3,3,4,3,2,2,3,3,3,3,3,3,3,3,3,2,4,3,2,2,3,2,3,3,3,3,3,3,2,4,3,2,2,3,2,2,2,2,1,4,3,2,3,3,1,1,4,3,2,4,2,2,3,2,2,3,3,2,3,3,3,4,3,2,3,3,3,3,4,3,3,2,4,3)
chi_map <- left_join(chi_map,chiphist,by="area_num_1")


#Maps the densest decade for each community area

basemap <- ggplot() +
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(data = cookcounty,fill="#fafbec",color="#eaeccf")+
  scale_fill_identity()+
  scale_color_identity()

maxregionmap <- basemap+
  geom_sf(data = chi_map,aes(fill = as.character(maxdecade)),alpha=1,color="black")+
  geom_sf_text(data=chi_map,aes(label=stringr::str_wrap(paste(Name,maxdecade),5),color=factor(maxdecade)),size=3)+
  scale_fill_brewer(palette="GnBu",name="Decade")+
  scale_color_manual(values=c("black","black","black","black","black","black","white","white","white"))+
  labs(title="Chicago Community Areas: Densest Decade")+
  coord_sf(xlim=c(-87.96,-87.515),ylim=c(41.625,42.05),expand=FALSE)+
  guides(color=FALSE)+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.01,vjust=-5,face="bold",size=35),
        plot.subtitle = element_text(hjust = 0.05,size=20,vjust=-20),
        panel.grid.minor = element_blank(),
        legend.position = c(0.2,0.4),
        legend.key.size = unit(2, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=15),
        text=element_text(family="Arial"),
        panel.background = element_rect(fill = "#fafbec",colour = "#eaeccf", size = 0.5, linetype = "solid"))


maxregionmap

ggsave(plot=maxregionmap, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/maxregionmap.jpg",height=18,width=14)


############################################################################################


maxdensepoints <- ggplot(chiphist)+
  geom_segment(aes(x=0,y=0,xend=60000,yend=60000),color="grey",alpha=0.8,linetype="dotted")+
  geom_point(aes(y=maxdensity,x=D2020,color=as.character(maxdecade),shape=Region2),size=2)+
  geom_text_repel(aes(y=maxdensity,x=D2020,label=Name),size=2)+
  scale_color_brewer(palette="GnBu",name="Decade")+
  scale_shape_manual(values=c(18,15,16,17))+
  labs(y="Max Historical Density",x="2020 Density",shape="Region:",color="Decade:",title="Chicago Community Areas: Densest Decade vs Now",subtitle="Persons per SQMI, 1930 to 2020",fill="Year")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)+
  theme_minimal()+
  theme(text=element_text(family="Tahoma"))

maxdensepoints

ggsave(plot=maxdensepoints, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/maxdensepoints.jpg",height=8,width=10)
