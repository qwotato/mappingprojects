library(tidyverse)
library(sf)
library(ggrepel)
library(extrafont)

########################################

# This is my API pull from Chicago data portal
ctaridepull <- read.csv("https://data.cityofchicago.org/resource/t2rn-p8d7.csv?$limit=45000")

baseyear <- as.Date(18109, format = "%Y-%m-%d")

nowyear <- as.Date(19936, format = "%Y-%m-%d")

ctaridepull$date <-  as.Date(ctaridepull$month_beginning, format="%Y-%m-%d")
ctaridepull$month <- format(as.Date(ctaridepull$date, format="%Y-%m-%d"),"%m")
ctaridepull$year <- format(as.Date(ctaridepull$date, format="%Y-%m-%d"),"%Y")

ctaridepullbase <- subset(ctaridepull,date == baseyear)
ctaridepullnow <- subset(ctaridepull,date == nowyear)

#CTA Stop Key data
ctastopkey <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata//sources/ctastopkey.csv")

#bring in base rides
ctastopkey <- right_join(select(ctaridepullbase,station_id,monthtotal), ctastopkey, by = "station_id")

#Get column name right
ctastopkey <- rename(ctastopkey, basetotal = monthtotal)

#bring in new rides
ctastopkey <- right_join(select(ctaridepullnow,station_id,monthtotal), ctastopkey, by = "station_id")

#Get column name right
ctastopkey <- rename(ctastopkey, nowtotal = monthtotal)
  
#Calculate Recovery Ratio
ctastopkey$recovery <- as.numeric( ctastopkey$nowtotal / ctastopkey$basetotal  )

##################################################################################################################################

#CTA Line JSON
ctalines <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/sources/ctalines.geojson")
ctalines <- arrange(ctalines,Number)

##################################################################################################################################

#CTA Stop Key data
ctastopkeymixed <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/sources/ctastopkeymixed.csv")

ctastopkeymixed <- merge(ctastopkeymixed,select(ctastopkey,station_id,basetotal),by="station_id")
ctastopkeymixed <- merge(ctastopkeymixed,select(ctastopkey,station_id,nowtotal),by="station_id")
ctastopkeymixed <- merge(ctastopkeymixed,select(ctastopkey,station_id,recovery),by="station_id")

##################################################################################################################################

# Branch-by-Branch Recovery

branchsum <- ctastopkey %>% group_by(Branch) %>% summarise_at(c("basetotal","nowtotal"),sum,na.rm=TRUE)

branchsum$recovery <- branchsum$nowtotal / branchsum$basetotal

ctabranchback <- ggplot(ctastopkey)+
  geom_col(aes(x=basetotal,y=reorder(Branch,-basetotal),fill=Branch),alpha=0.2)+
  geom_col(aes(x=nowtotal,y=reorder(Branch,-basetotal),fill=Branch))+
  geom_text(data=branchsum,aes(x=nowtotal,reorder(Branch,-basetotal),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_manual(guide="none",values=c("#c60c30","#e27ea6","#009b3a","#00a1de","#c60c30","#62361b","#522398","grey","#f9461c","grey","#00a1de","#009b3a","#f9e300","black"))+
  labs(x="Monthly Riders",title="CTA Ridership Recovery",subtitle="August 2024 rides as a percent of August 2019 rides",y="Branch")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  #scale_y_discrete(breaks=NULL)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),
        panel.border = element_blank(), plot.caption = element_text( color="#404040"),
        plot.subtitle = element_text(hjust = 0.5),panel.grid.major.x = element_line(color="lightgrey"), panel.grid.minor = element_blank())+
  theme(text=element_text(family="Arial"))

ctabranchback

ggsave(plot=ctabranchback, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/branchrecoveryaug24.jpeg",height=10,width=10)


##################################################################################################################################
#Looking to Pink

pnkrecovery <- ggplot(ctastopkeymixed %>% filter(Pink=="Yes" & Branch!="Loop"))+
  geom_col(aes(x=basetotal,y=reorder(NAME,-nowtotal),fill=Hex),alpha=0.2,position="dodge")+
  geom_col(aes(x=nowtotal,y=reorder(NAME,-nowtotal),fill=Hex),position="dodge")+
  geom_text(aes(x=nowtotal,reorder(NAME,-nowtotal),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  labs(x="Monthly Riders",title="CTA Ridership Recovery",subtitle="Monthly Entries - August 2024 rides as a percent of August 2019 rides",y="Branch")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 5))+
  theme_classic()+  
  theme(plot.title = element_text(hjust = 0.5,face="bold"), axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),
                          panel.border = element_blank(), plot.caption = element_text( color="#404040"),
                          plot.subtitle = element_text(hjust = 0.5),panel.grid.major.x = element_line(color="lightgrey"), panel.grid.minor = element_blank())+
  theme(text=element_text(family="Arial"))

pnkrecovery

ggsave(plot=pnkrecovery, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/pinkrecoveryaug24.jpeg",height=10,width=10)



pnkmap <- ggplot(ctastopkey %>% filter(Pink=="Yes" & Branch!="Loop"))+
  geom_sf(data=ctalines,aes(color=Hex),linewidth=2)+
  geom_text_repel( aes(x=lng, y= lat, label = NAME),vjust=-2,hjust=1,size=3)+
  geom_point( aes(x=lng, y= lat,color=Hex),size=10)+
  geom_text( aes(x=lng, y= lat, label = scales::percent(recovery,accuracy=1)),color="white",size=3.5)+
  labs(title="CTA Ridership Recovery - Pink Line ",subtitle="Monthly Entries - Aug 2024 rides as a percent of Aug 2019 rides",fill="%")+
  scale_fill_identity()+
  scale_color_identity()+
  coord_sf(xlim=c(-87.78,-87.62),ylim=c(41.84,41.9),expand=FALSE)+
  theme_void()+
  theme(text=element_text(family="Arial"),legend.position="none")

pnkmap

ggsave(plot=pnkmap, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/pinkrecoverymapaug24.jpeg",height=8,width=10)

######################################################################################
# Most used stations

colback <- ggplot(head(arrange(ctastopkeymixed,-nowtotal),21))+
  geom_col(aes(x=basetotal,y=reorder(STATION,nowtotal),fill=Hex),alpha=0.2,position="dodge")+
  geom_col(aes(x=nowtotal,y=reorder(STATION,nowtotal),fill=Hex),position="dodge")+
  geom_text(aes(x=nowtotal,reorder(STATION,nowtotal),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  labs(title="CTA Ridership Recovery",subtitle="Aug 2024 rides as a percent of Aug 2019 rides",y="Station",x="Monthly Riders")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),
        panel.border = element_blank(), plot.caption = element_text( color="#404040"),
        plot.subtitle = element_text(hjust = 0.5),panel.grid.major.x = element_line(color="lightgrey"), panel.grid.minor = element_blank())+
  theme(text=element_text(family="Arial"))

colback

ggsave(plot=colback, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/topstationsaug24.jpeg",height=10,width=10)

######################################################################################
# Most recovered stations

colrecover <- ggplot(head(arrange(ctastopkeymixed,-recovery),12))+
  geom_col(aes(x=basetotal,y=reorder(STATION,recovery),fill=Hex),alpha=0.2,position="dodge")+
  geom_col(aes(x=nowtotal,y=reorder(STATION,recovery),fill=Hex),position="dodge")+
  geom_text(aes(x=nowtotal,reorder(STATION,recovery),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  labs(title="CTA Ridership Recovery",subtitle="Monthly Entries - Aug 2024 rides as a percent of Aug 2019 rides",y="Station",x="Monthly Riders")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),
        panel.border = element_blank(), plot.caption = element_text( color="#404040"),
        plot.subtitle = element_text(hjust = 0.5),panel.grid.major.x = element_line(color="lightgrey"), panel.grid.minor = element_blank())+
  theme(text=element_text(family="Arial"))

colrecover

ggsave(plot=colrecover, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/colrecoveraug.jpeg",height=10,width=10)

######################################################################################
##################################################################################################################################
#Loop

looprecovery <- ggplot(ctastopkeymixed %>% filter( Branch=="Loop"))+
  geom_col(aes(x=basetotal,y=reorder(STATION,-nowtotal),fill=Hex),alpha=0.2,position="dodge")+
  geom_col(aes(x=nowtotal,y=reorder(STATION,-nowtotal),fill=Hex),position="dodge")+
  geom_text(aes(x=nowtotal,reorder(STATION,-nowtotal),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  labs(x="Monthly Riders",title="CTA Ridership Recovery",subtitle="Monthly Entries - August 2024 rides as a percent of August 2019 rides",y="Branch")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  theme_classic()+
  theme(text=element_text(family="Arial"))

looprecovery

ggsave(plot=looprecovery, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/looprecoveryaug24.jpeg",height=10,width=10)



loopmap <- ggplot(ctastopkey %>% filter(Branch=="Loop"))+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(data = cookcounty,fill="#fafbec",color="#d5d492")+
  geom_sf(data=ctalines,aes(color=Hex),linewidth=2)+
  geom_sf(data=ctalines,aes(color=Hex),linewidth=2)+
  geom_text_repel( aes(x=lng, y= lat, label = NAME),vjust=-2,hjust=1.5,size=3)+
  geom_point( aes(x=lng, y= lat,color=Hex),size=10)+
  geom_text( aes(x=lng, y= lat, label = scales::percent(recovery,accuracy=1)),color="white",size=3.5)+
  labs(title="CTA Ridership Recovery - Loop ",subtitle="Monthly Entries - Aug 2024 rides as a percent of Aug 2019 rides",fill="%")+
  scale_fill_identity()+
  scale_color_identity()+
  coord_sf(xlim=c(-87.64,-87.62),ylim=c(41.87,41.891),expand=FALSE)+
  theme_void()+
  theme(text=element_text(family="Arial"),legend.position="none",panel.background = element_rect(fill = "#fafbec",colour = "#d5d492", size = 0.5, linetype = "solid"))

loopmap

ggsave(plot=loopmap, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/loopmapaug24.jpeg",height=8,width=10)

######################################################################################
##################################################################################################################################


westernback <- ggplot(ctastopkeymixed %>% filter(NAME=="Kedzie"))+
  geom_col(aes(x=basetotal,y=reorder(STATION,-nowtotal),fill=Hex),alpha=0.2)+
  geom_col(aes(x=nowtotal,y=reorder(STATION,-nowtotal),fill=Hex))+
  geom_text(aes(x=nowtotal,y=reorder(STATION,-nowtotal),label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  labs(x="Monthly Riders",title="CTA Ridership Recovery",subtitle="May 2024 rides as a percent of May 2019 rides",y="Branch")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))

westernback

ggsave(plot=westernback, "Kedzieback.jpg",height=10,width=10)





ctabasemap <- ggplot() +
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(data = cookcounty,fill="#fafbec",color="#d5d492")+
  geom_sf(data=ctalines,aes(color=Hex),linewidth=2)+
  coord_sf(xlim=c(-87.96,-87.515),ylim=c(41.625,42.12),expand=FALSE)+
  scale_fill_identity()+
  scale_color_identity()+
  theme_void()+
  theme(panel.background = element_rect(fill = "#fafbec",colour = "#d5d492", size = 0.5, linetype = "solid"))

ctabasemap

ggsave(plot=ctabasemap, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/ctabasemap.jpeg",height=10,width=6.7)



##################################################################################################################################
#Looking to Green - Oak Park

grnrecovery <- ggplot(ctastopkeymixed %>% filter(Green=="Yes" & Branch =="Oak Park"))+
  geom_col(aes(x=basetotal,y=reorder(NAME,-nowtotal),fill=Hex),alpha=0.2,position="dodge")+
  geom_col(aes(x=nowtotal,y=reorder(NAME,-nowtotal),fill=Hex),position="dodge")+
  geom_text(aes(x=nowtotal,reorder(NAME,-nowtotal),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  labs(x="Monthly Riders",title="CTA Ridership Recovery",subtitle="August 2024 rides as a percent of August 2019 rides",y="Branch")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  theme_classic()+
  theme(text=element_text(family="Arial"))

grnrecovery

ggsave(plot=grnrecovery, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/grnrecoveryaug24.jpeg",height=10,width=10)



pnkmap <- ggplot(ctastopkey %>% filter(Pink=="Yes" & Branch!="Loop"))+
  geom_sf(data=ctalines,aes(color=Hex),linewidth=2)+
  geom_text_repel( aes(x=lng, y= lat, label = NAME),vjust=-2,hjust=1,size=3)+
  geom_point( aes(x=lng, y= lat,color=Hex),size=10)+
  geom_text( aes(x=lng, y= lat, label = scales::percent(recovery,accuracy=1)),color="white",size=3.5)+
  labs(title="CTA Ridership Recovery - Pink Line ",subtitle="Monthly Entries - Aug 2024 rides as a percent of Aug 2019 rides",fill="%")+
  scale_fill_identity()+
  scale_color_identity()+
  coord_sf(xlim=c(-87.78,-87.62),ylim=c(41.84,41.9),expand=FALSE)+
  theme_void()+
  theme(text=element_text(family="Arial"),legend.position="none")

pnkmap

ggsave(plot=pnkmap, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/pinkrecoverymapaug24.jpeg",height=8,width=10)




