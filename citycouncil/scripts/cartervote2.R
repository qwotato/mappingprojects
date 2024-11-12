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

carterwards <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/citycouncil/newctapres.csv")

ctalstops <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/ctalstops.csv")




carterwards$ward <- as.character(carterwards$ward)

wardvotes <- inner_join(chiwards2023,carterwards, by="ward")


wardtable <- carterwards[,1:3] %>% gt() %>% data_color(target_columns=everything(),method="numeric",palette=c("white","#005e8b","#c60c30"),alpha=0.5)

wardtable

yesvotes <- sum(wardvotes$new_cta_pres)
novotes <- nrow(wardvotes) - sum(wardvotes$new_cta_pres)

yesvotes <- data.frame(yes = sum(wardvotes$new_cta_pres), support = "Supports Measure:")
yesvotes <- yesvotes %>% unite(me, support, yes, sep = " ")

novotes <- data.frame(no = nrow(wardvotes) - sum(wardvotes$new_cta_pres), support = "TBD:")
novotes <- novotes %>% unite(me, support, no, sep = " ")

votemap <- ggplot() +
  geom_sf(data = wardvotes, aes(fill = as.factor(new_cta_pres)),alpha=0.5)+
  labs(title="Support for resolution to replace CTA president Dorval Carter",subtitle="5/20/24")+
  labs(fill="Support")+
  geom_sf_text(data=wardvotes,aes(label = ward),size=3)+
  scale_fill_manual(values=c("white","#005e8b","#c60c30"),labels=c(novotes[1,1], yesvotes[1,1],"Authored Measure"))+
  theme_void()+
  theme(legend.position=c(0.3,0.55))+
  theme(plot.title=element_text(hjust=0.5))+
  theme(plot.subtitle=element_text(hjust=0.5))+
  theme(text=element_text(family="Tahoma"))

votemap

ggplot() +
  geom_sf(data = wardvotes, aes(fill = as.factor(new_cta_pres)),alpha=0.5)+
  labs(title="New CTA President Resolution Support",subtitle="5/14/24")+
  labs(fill="Support")+
  geom_sf_text(data=wardvotes,aes(label = ward),size=3)+
  geom_sf_label(data=wardvotes,aes(label = alder,fill = as.factor(new_cta_pres)),size=2.5,vjust=-.5,alpha=0.9)+
  scale_fill_manual(values=c("white","#005e8b","#c60c30"),labels=c("", "Supports Measure","Authored Measure"))+
  theme_void()+
  theme(legend.position=c(0.3,0.55))+
  theme(plot.title=element_text(hjust=0.5))+
  theme(plot.subtitle=element_text(hjust=0.5))+
  theme(text=element_text(family="Tahoma"))

grid.arrange(votemap, wardtable,ncol=2)

ggsave(plot=votemap, "CarterVoteMap.png")
