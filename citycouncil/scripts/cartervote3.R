library(sf)
library(ggplot2)
library(tidyverse)
library(extrafont)
library(geosphere)
library(ggrepel)
library(seasonal)
library(scales)
library(directlabels)
library(gt)
library(gtsummary)

chiwards2023 <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/chiwards2023.geojson")

councilvotes <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/citycouncil/councilvotes.csv")

councilvotes$Ward <- as.integer(councilvotes$Ward)
chiwards2023 <- rename(chiwards2023, Ward = ward)
chiwards2023$Ward <- as.integer(chiwards2023$Ward)

wardvotes <- inner_join(chiwards2023,councilvotes, by="Ward")


wardtable <- councilvotes[,1:3] %>% gt() %>% 
    tab_style(style = cell_fill(color="white"), locations= cells_body(rows=Fire_Carter == "No Vote")) %>% 
    tab_style(style = cell_fill(color="#005e8b",alpha=0.5), locations= cells_body(rows=Fire_Carter == "Yes")) %>%
    tab_style(style = cell_fill(color="#c60c30",alpha=0.5), locations= cells_body(rows=Fire_Carter == "No"))
wardtable

votes <- wardvotes %>% count(Fire_Carter)
votes <- as.data.frame(votes)
votes <- select(votes, Fire_Carter, n)
votes <- pivot_wider(votes, names_from = Fire_Carter, values_from = n)


yesvotes <- data.frame(yes = votes$Yes, support = "Yes:",color="#00538b")
yesvotes <- yesvotes %>% unite(me, support, yes, sep = " ")

novotes <- data.frame(no = votes$No, support = "No:",color="#c60c30")
novotes <- novotes %>% unite(me, support, no, sep = " ")

absent <- data.frame(no = votes[,2], support = "TBD:",color="white")
absent <- absent %>% unite(me, support, No.Vote, sep = " ")

votemap <- ggplot() +
  geom_sf(data = wardvotes, aes(fill = as.factor(Fire_Carter)),alpha=0.5)+
  labs(title="Support for Resolution to Replace CTA President Dorval Carter",subtitle="5/22/24")+
  labs(fill="Support")+
  geom_sf_text(data=wardvotes,aes(label = Ward),size=3)+
  scale_fill_manual(values=c("#c60c30","white","#005e8b"),labels=c(novotes[1,1], absent[1,1],yesvotes[1,1]))+
  theme_void()+
  theme(legend.position=c(0.3,0.55))+
  theme(plot.title=element_text(hjust=0.1,vjust=-11.5))+
  theme(plot.subtitle=element_text(hjust=0.1,vjust=-15))+
  theme(panel.background = element_rect(fill = '#fcfbf7', color = 'black'))+
  theme(text=element_text(family="Tahoma"))

votemap


ggsave(plot=votemap, "CarterVoteMap.png",height=8,width=7)
gtsave(wardtable,filename="VoteList.png")




votemap2 <- ggplot() +
  geom_sf(data = wardvotes, aes(fill = as.factor(Fire_Carter)),alpha=0.5)+
  labs(title="Support for Resolution to Replace CTA President Dorval Carter",subtitle="5/22/24")+
  labs(fill="Support")+
  geom_sf_text(data=wardvotes,aes(label = Ward),size=3)+
  scale_fill_manual(values=c("white","#005e8b"),labels=c("",yesvotes[1,1]))+
  theme_void()+
  theme(legend.position=c(0.3,0.55))+
  theme(plot.title=element_text(hjust=0.1,vjust=-11.5))+
  theme(plot.subtitle=element_text(hjust=0.1,vjust=-15))+
  theme(panel.background = element_rect(fill = '#fcfbf7', color = 'black'))+
  theme(text=element_text(family="Tahoma"))

votemap2

ggsave(plot=votemap2, "CarterVoteMap.png")
