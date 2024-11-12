
library(sf)
library(tidyverse)
library(extrafont)
library(ggrepel)

############################################################################################

#Chicago population by neighborhood area. 1950 and 2020
areapopchi2 <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/chihoodpop2.csv")

areapopchi2$year <- as.character(areapopchi2$year)

popdensity <- ggplot(areapopchi2)+
  geom_density(aes(density,fill=as.character(year),group=year),color="black",linewidth=1,alpha=0.5)+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::comma,breaks=scales::pretty_breaks(n=10))+
  scale_fill_manual(values=c("#41b6e6","#903162"))+
  labs(y="Share of Population",x="Persons per Square Mile",title="Chicago Density by Community Areas",subtitle="1950 vs 2020",fill="Year")+
  theme_minimal()+
  theme(text=element_text(family="Tahoma"))

popdensity

ggsave(plot=popdensity, "popdensity19502020.jpg",height=11,width=8.5)

pophist <- ggplot(areapopchi2,aes(group=year))+
  geom_histogram(aes(x=density,fill=as.character(year)),binwidth=10000,color="black",width=0.9)+
  stat_bin(aes(x=density,label=..count..),binwidth=10000,geom="text",vjust=-0.5)+
  scale_fill_manual(values=c("#41b6e6","#903162"))+
  labs(x="Persons per Square Mile",title="Chicago Community Areas by Density",subtitle="1950 vs 2020",fill="Year")+
  facet_grid( . ~ year)+
  scale_x_continuous(labels=scales::comma,breaks=scales::pretty_breaks(n=10))+
  scale_y_continuous(labels=scales::comma)+
  theme_minimal()+
  theme(text=element_text(family="Tahoma"))

pophist

ggsave(plot=pophist, "pophist19502020.jpg",height=11,width=8.5)



ggplot(areapopchi2,aes(group=year))+
  geom_histogram(aes(x=density,fill=as.character(year)),binwidth=10000,color="black",width=0.9)+
  stat_bin(aes(x=density,label=..count..),binwidth=10000,geom="text",vjust=-0.5)+
  scale_fill_manual(values=c("#41b6e6","#903162"))+
  labs(x="Persons per Square Mile",title="Chicago Community Areas by Density",subtitle="1950 vs 2020",fill="Year")+
  facet_grid( . ~ year)+
  scale_x_continuous(labels=scales::comma,breaks=scales::pretty_breaks(n=10))+
  scale_y_continuous(labels=scales::comma)+
  theme_minimal()+
  theme(text=element_text(family="Tahoma"))

sharecols <- ggplot(areapopchi2,aes(group=year,x=popsqmi,y=yrshare))+
  geom_col(aes(fill=year))+
  scale_fill_manual(values=c("#41b6e6","#903162"))+
  geom_text(
    aes(label = scales::percent(after_stat(y)), group = year), 
    stat = 'summary', fun = sum,vjust=-1)+
  labs(x="Persons Per Square Mile",y="% Of Population",title="Chicago Community Areas by Density",subtitle="1950 vs 2020",fill="Year")+
  facet_grid( . ~ year)+
  theme_minimal()+
  theme(text=element_text(family="Tahoma"))

sharecols

ggsave(plot=sharecols, "popcols19502020.jpg",height=11,width=8.5)

chipopsum <- areapopchi2 %>% group_by(popsqmi,year) %>% summarise(yrshare=sum(yrshare))


ggplot(areapopchi2,aes(group=Region2))+
  geom_histogram(aes(x=density,fill=as.character(Region2)),binwidth=10000,color="black",width=0.9,alpha=0.7)+
  stat_bin(aes(x=density,label=..count..),binwidth=10000,geom="text",position=position_stack(vjust=0.5))+
  scale_fill_manual(values=c("#41b6e6","#903162","lightgreen","orange"))+
  labs(x="Persons per Square Mile",y="Number of Community Areas",title="Chicago Community Areas by Density",subtitle="1950 vs 2020",fill="Region")+
  facet_grid( . ~ year)+
  scale_x_continuous(labels=scales::comma,breaks=scales::pretty_breaks(n=10))+
  scale_y_continuous(labels=scales::comma)+
  theme_minimal()+
  theme(text=element_text(family="Tahoma"))


nhooddensity <- ggplot(areapopchi2)+
  geom_density(aes(density,fill=as.character(Region2),group=Region2),color="black",size=1,alpha=0.5)+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::comma,breaks=scales::pretty_breaks(n=10))+
  scale_fill_manual(values=c("#41b6e6","#903162","lightgreen","orange"))+
  facet_grid( . ~ year)+
  labs(y="Share of Population",x="Persons per Square Mile",title="Chicago Density by Community Areas",subtitle="1950 vs 2020",fill="Region")+
  theme_minimal()+
  theme(text=element_text(family="Tahoma"))

nhooddensity

ggsave(plot=nhooddensity, "nhooddensity19502020.jpg",height=11,width=8.5)

nhoodcol <- ggplot(areapopchi2,aes(group=Region2,x=popsqmi,y=yrshare))+
  geom_col(aes(fill=Region2))+
  scale_fill_manual(values=c("#41b6e6","#903162","lightgreen","orange"))+
  geom_text(
    aes(label = scales::percent(after_stat(y)), group = year), 
    stat = 'summary', fun = sum,vjust=-1)+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Persons Per Square Mile",y="Share Of Population",title="Chicago Community Areas by Density",subtitle="1950 vs 2020",fill="Year")+
  facet_grid( . ~ year)+
  theme_minimal()+
  theme(text=element_text(family="Tahoma"))

nhoodcol

ggsave(plot=nhoodcol, "nhoodcol19502020.jpg",height=11,width=8.5)


#this gets me the map of chicago with community areas
chi_map <- read_sf("https://raw.githubusercontent.com/thisisdaryn/data/master/geo/chicago/Comm_Areas.geojson") 

chi_map <- arrange(chi_map, community )


chi_map$Region <-  c(2,3,3,3,3,4,3,2,2,3,3,3,3,3,3,3,3,3,2,4,3,2,2,3,2,3,3,3,3,3,3,2,4,3,2,2,3,2,2,2,2,1,4,3,2,3,3,1,1,4,3,2,4,2,2,3,2,2,3,3,2,3,3,3,4,3,2,3,3,3,3,4,3,3,2,4,3)


regionmap <- ggplot()+
  geom_sf(data = chi_map,aes(fill = as.character(Region)),alpha=1,color="black")+
  scale_fill_manual(values=c("#41b6e6","#903162","lightgreen","orange"),name="Region",labels=c("Central","North","South","West"))+
  theme_void()


regionmap

ggsave(plot=regionmap, "regionmap.jpg",height=11,width=8.5)


############################################################################################


#Chicago population by neighborhood area. 1950 and 2020
areapopchi <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/chihoodpop.csv")

areapopchi$D1950 <- as.integer(areapopchi$P1950 / areapopchi$sqmiles)
areapopchi$D2020 <- as.integer(areapopchi$P2020 / areapopchi$sqmiles)

densepoints <- ggplot(areapopchi,aes(color=as.character(Region2)))+
  geom_segment(aes(x=0,y=0,xend=60000,yend=60000),color="black",alpha=0.5,linetype="dotted")+
  geom_point(aes(x=D1950,y=D2020),size=2)+
  geom_text_repel(aes(x=D1950,y=D2020,label=Name),size=2,vjust=-1)+
  scale_color_manual(values=c("#41b6e6","#903162","darkgreen","orange"),name="Region",labels=c("Central","North","South","West"))+
  labs(x="1950 Population Desnity",y="2020 Population Density",title="Chicago Community Areas by Density (Persons Per SQMI)",subtitle="1950 vs 2020",fill="Year")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)+
  theme_minimal()+
  theme(text=element_text(family="Tahoma"))

densepoints

ggsave(plot=densepoints, "densepoints.jpg",height=10,width=10)
