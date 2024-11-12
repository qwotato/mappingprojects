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

font_import()
loadfonts(device="win")


chiwards2023 <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/chiwards2023.geojson")

carterwards <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/newctapres.csv")

ctalstops <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/ctalstops.csv")


carterwards$ward <- as.character(carterwards$ward)


wardvotes <- inner_join(chiwards2023,carterwards, by="ward")


ggplot() +
  geom_sf(data = wardvotes, aes(fill = as.factor(new_cta_pres)),alpha=0.5)+
  scale_fill_manual(values=c("white","#005e8b","#c60c30"),labels=c("", "Supports Measure","Authored Measure"))+
  labs(title="New CTA President Resolution Support")+
  labs(fill="Support")+
  theme_void()+
  theme(legend.position=c(0.3,0.55))


ggplot() +
  geom_sf(data = wardvotes, aes(fill = as.factor(new_cta_pres)),alpha=0.5)+
  scale_fill_manual(values=c("white","#005e8b","#c60c30"),labels=c("", "Supports Measure","Authored Measure"))+
  labs(title="New CTA President Resolution Support",subtitle="5/14/24")+
  labs(fill="Support")+
  geom_sf_text(data=wardvotes,aes(label = ward),size=3)+
  theme_void()+
  theme(legend.position=c(0.3,0.55))+
  theme(plot.title=element_text(hjust=0.5))+
  theme(plot.subtitle=element_text(hjust=0.5))+
  theme(text=element_text(family="Tahoma"))



ggplot() +
  geom_sf(data = wardvotes, aes(fill = as.factor(new_cta_pres)),alpha=0.5)+
  scale_fill_manual(values=c("white","#005e8b","#c60c30"),labels=c("", "Supports Measure","Authored Measure"))+
  labs(title="New CTA President Resolution Support")+
  labs(fill="Support")+
  geom_point(data=ctalstops, aes(x=lng,y=lat))+
  geom_sf_text(data=wardvotes,aes(label = ward),size=5)+
  coord_sf(xlim=c(-87.656813,-87.602213),ylim=c(41.865,41.898418),expand=FALSE)+
  theme_void()+
  theme(legend.position=c(0.3,0.55))
