library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)
library(sf)
library(extrafont)
library(ggrepel)
library(mapboxapi)
library(lookup)

########################################

# This is my API pull from Chicago data portal
ctaridepull <- read.csv("https://data.cityofchicago.org/resource/t2rn-p8d7.csv?$limit=45000")

baseyear <- as.Date(18017, format = "%Y-%m-%d")

nowyear <- as.Date(19844, format = "%Y-%m-%d")

ctaridepull$date <-  as.Date(ctaridepull$month_beginning, format="%Y-%m-%d")
ctaridepull$month <- format(as.Date(ctaridepull$date, format="%Y-%m-%d"),"%m")
ctaridepull$year <- format(as.Date(ctaridepull$date, format="%Y-%m-%d"),"%Y")

ctaridepullbase <- subset(ctaridepull,date == baseyear)
ctaridepullnow <- subset(ctaridepull,date == nowyear)

#CTA Stop Key data
ctastopkey924 <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/CLIP/ctastopkey924.csv")

#bring in base rides
ctastopkey924 <- right_join(select(ctaridepullbase,station_id,avg_weekday_rides), ctastopkey924, by = "station_id")

#Get column name right
ctastopkey924 <- rename(ctastopkey924, base_weekday_rides = avg_weekday_rides)

#bring in new rides
ctastopkey924 <- right_join(select(ctaridepullnow,station_id,avg_weekday_rides), ctastopkey924, by = "station_id")

#Get column name right
ctastopkey924 <- rename(ctastopkey924, current_weekday_rides = avg_weekday_rides)
  
  
ctastopkey924$recovery <- as.numeric( ctastopkey924$current_weekday_rides / ctastopkey924$base_weekday_rides  )


##################################################################################################################################

#CTA Stop Key data
ctastopkeymixed924 <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/CLIP/ctastopkeymixed924.csv")

ctastopkeymixed924 <- merge(ctastopkeymixed924,select(ctastopkey924,station_id,base_weekday_rides),by="station_id")
ctastopkeymixed924 <- merge(ctastopkeymixed924,select(ctastopkey924,station_id,current_weekday_rides),by="station_id")
ctastopkeymixed924 <- merge(ctastopkeymixed924,select(ctastopkey924,station_id,recovery),by="station_id")

##################################################################################################################################

# Branch-by-Branch Recovery

branchsum <- ctastopkey924 %>% group_by(Branch) %>% summarise_at(c("base_weekday_rides","current_weekday_rides"),sum,na.rm=TRUE)

branchsum$recovery <- branchsum$current_weekday_rides / branchsum$base_weekday_rides

ctabranchback <- ggplot(ctastopkey924)+
  geom_col(aes(x=base_weekday_rides,y=reorder(Branch,-base_weekday_rides),fill=Branch),alpha=0.2)+
  geom_col(aes(x=current_weekday_rides,y=reorder(Branch,-base_weekday_rides),fill=Branch))+
  geom_text(data=branchsum,aes(x=current_weekday_rides,reorder(Branch,-base_weekday_rides),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_manual(values=c("#c60c30","#e27ea6","#009b3a","#00a1de","#c60c30","#62361b","#522398","grey","#f9461c","grey","#00a1de","#009b3a","#f9e300","black"))+
  labs(x="Average Weekday Riders",title="CTA Ridership Recovery",subtitle="Average Weekday Entries - May 2024 rides as a percent of May 2019 rides",y="Branch")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  #scale_y_discrete(breaks=NULL)+
  theme_classic()+
  theme(text=element_text(family="Tahoma"),)

ctabranchback

ggsave(plot=ctabranchback, "ctabranchback524.jpg",height=10,width=10)


##################################################################################################################################

#Looking to Pink

pnkrecovery <- ggplot(ctastopkeymixed924 %>% filter(Pink=="Yes" & Branch!="Loop"))+
  geom_col(aes(x=base_weekday_rides,y=reorder(NAME,-current_weekday_rides),fill=Hex),alpha=0.2,position="dodge")+
  geom_col(aes(x=current_weekday_rides,y=reorder(NAME,-current_weekday_rides),fill=Hex),position="dodge")+
  geom_text(aes(x=current_weekday_rides,reorder(NAME,-current_weekday_rides),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  labs(x="Average Weekday Riders",title="CTA Ridership Recovery",subtitle="Average Weekday Entries - May 2024 rides as a percent of May 2019 rides",y="Branch")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))

pnkrecovery

ggsave(plot=pnkrecovery, "pnkrecovery524.jpg",height=10,width=10)



pnkmap <- ggplot(ctastopkey924 %>% filter(Pink=="Yes" & Branch!="Loop"))+
  geom_sf(data=ctalines,aes(color=Hex),linewidth=2)+
  geom_text_repel( aes(x=lng, y= lat, label = NAME),vjust=-2,hjust=1,size=3)+
  geom_point( aes(x=lng, y= lat,color=Hex),size=10)+
  geom_text( aes(x=lng, y= lat, label = scales::percent(recovery,accuracy=1)),color="white",size=3.5)+
  #geom_text( aes(x=lng, y= lat, label = paste("Apr '19:",scales::comma(base_weekday_rides))),vjust=3.5,color="black",size=2)+
  #geom_text( aes(x=lng, y= lat, label = paste("Apr '24:",scales::comma(current_weekday_ride))),vjust=5,color="black",size=2)+
  labs(title="CTA Ridership Recovery - Pink Line ",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",fill="%")+
  scale_fill_identity()+
  scale_color_identity()+
  coord_sf(xlim=c(-87.78,-87.62),ylim=c(41.84,41.9),expand=FALSE)+
  theme_void()+
  theme(text=element_text(family="Tahoma"),legend.position="none")

pnkmap

ggsave(plot=pnkmap, "pnkmap.jpg",height=8,width=10)

######################################################################################

colback <- ggplot(head(arrange(ctastopkeymixed924,-recovery),27))+
  geom_col(aes(x=base_weekday_rides,y=reorder(STATION,current_weekday_rides),fill=Hex),alpha=0.2,position="dodge")+
  geom_col(aes(x=current_weekday_rides,y=reorder(STATION,current_weekday_rides),fill=Hex),position="dodge")+
  geom_text(aes(x=current_weekday_rides,reorder(STATION,current_weekday_rides),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  labs(title="CTA Ridership Recovery",subtitle="Average Weekday Entries - May 2024 rides as a percent of May 2019 rides",y="Station",x="Average Weekday Riders")+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))

colback

ggsave(plot=colback, "topstopsrecovery.jpg",height=10,width=10)


######################################################################################


westernback <- ggplot(ctastopkeymixed924 %>% filter(NAME=="Kedzie"))+
  geom_col(aes(x=base_weekday_rides,y=reorder(STATION,-current_weekday_rides),fill=Hex),alpha=0.2)+
  geom_col(aes(x=current_weekday_rides,y=reorder(STATION,-current_weekday_rides),fill=Hex))+
  geom_text(aes(x=current_weekday_rides,y=reorder(STATION,-current_weekday_rides),label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  labs(x="Average Weekday Riders",title="CTA Ridership Recovery",subtitle="Average Weekday Entries - May 2024 rides as a percent of May 2019 rides",y="Branch")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))

westernback

ggsave(plot=westernback, "Kedzieback.jpg",height=10,width=10)



ggplot(ctastopkeymixed924)+
  geom_point(aes(x=base_weekday_rides,y=current_weekday_rides,color=Hex),alpha=0.7,position=position_jitter(h=15,w=15))+
  geom_text_repel(data=ctastopkey924,aes(x=base_weekday_rides,y=current_weekday_rides,color=Hex,label=NAME),size=3)+
  scale_color_identity()+
  labs(title="CTA Ridership Recovery",subtitle="May 2024 and May 2019",x="May 2019",y="May 2024")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  xlim(0,25000)+
  ylim(0,25000)+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))














###
# PAST HERE IS THE OLD CODE, STILL WORKING ON REPLACING














################################################################################################################################################################
#Bringing in my data and formatting fixes

chicago <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/chicagooutline.json")

ctalines <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/ctalines.geojson") 

ctalines <- arrange(ctalines,Number)


################################################################################################################################################################
#Full Map

ctarecovery <- ggplot()+
  geom_sf(data=chicago,fill="white")+
  geom_sf(data=ctalines,aes(color=Hex),linewidth=2)+
  geom_point(data=ctastopkey,aes(x=lng,y=lat,color=Hex),size=2)+
  geom_label(data=ctastopkey, aes(x=lng, y= lat, label = Branch,fill=Hex),size=1,alpha=0.7)+
  labs(title="CTA Ridership Recovery",subtitle="Branch Key")+
  scale_fill_identity()+
  scale_color_identity()+
  theme_void()+
  theme(text=element_text(family="Tahoma"))

ctarecovery

ggsave(plot=ctarecovery, "ctarecovery.jpg",height=10,width=10)

################################################################################################################################################################
#Full Columns

colback <- ggplot(head(arrange(ctastopkeymixed,recovery),12))+
    geom_col(aes(x=recovery,y=reorder(STATION,recovery),fill=Hex),position="dodge")+
  scale_fill_identity()+
  labs(title="CTA Ridership Recovery",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",y="Branch")+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))

colback

ggsave(plot=colback, "colback.jpg",height=10,width=10)


bottomrec <- ggplot(tail(arrange(ctastopkeymixed,recovery),10))+
  geom_col(aes(x=APR_19,y=reorder(NAME,-recovery),fill=Hex),alpha=0.2,position = "dodge")+
  geom_col(aes(x=APR_24,y=reorder(NAME,-recovery),fill=Hex),position = "dodge")+
  geom_text(aes(x=APR_24,reorder(NAME,-recovery),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  labs(x="Average Weekday Riders",title="CTA Ridership Recovery - Worst Performing",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",y="Stop")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  scale_fill_identity()+
  scale_color_identity()+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))

bottomrec

ggsave(plot=bottomrec, "bottomrec.jpg",height=10,width=10)

################################################################################################################################################################
#Box Plots


boxback <- ggplot(ctastopkey)+
  geom_boxplot(aes(x=recovery,y=reorder(Branch,-recovery),fill=Branch))+
  scale_fill_identity()+
  scale_color_identity()+
  scale_fill_manual(values=c("#c60c30","#e27ea6","#009b3a","#00a1de","#c60c30","#62361b","#522398","grey","#f9461c","black","#00a1de","#009b3a","#f9e300","black"))+
  scale_x_continuous(labels = scales::percent)+
  labs(title="CTA Ridership Recovery",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",y="Branch")+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))

boxback

ggsave(plot=boxback, "boxback.jpg",height=10,width=10)




################################################################################################################################################################
# Branch-by-Branch Recovery

branchsum <- ctastopkey %>% group_by(Branch) %>% summarise_at(c("APR_19","APR_24"),sum,na.rm=TRUE)

branchsum$recovery <- branchsum$APR_24 / branchsum$APR_19

ctabranchback <- ggplot(ctastopkey)+
  geom_col(aes(x=APR_19,y=reorder(Branch,-APR_24),fill=Branch),alpha=0.2)+
  geom_col(aes(x=APR_24,y=reorder(Branch,-APR_24),fill=Branch))+
  geom_text(data=branchsum,aes(x=APR_24,reorder(Branch,-APR_24),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_manual(values=c("#c60c30","#e27ea6","#009b3a","#00a1de","#c60c30","#62361b","#522398","grey","#f9461c","grey","#00a1de","#009b3a","#f9e300","black"))+
  labs(x="Average Weekday Riders",title="CTA Ridership Recovery",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",y="Branch")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  #scale_y_discrete(breaks=NULL)+
  theme_classic()+
  theme(text=element_text(family="Tahoma"),)

ctabranchback

ggsave(plot=ctabranchback, "ctabranchback.jpg",height=10,width=10)

#################################################################################################################################################################################
#Looking at the Orange Line

orecovery <- ggplot(ctastopkey %>% filter(Branch=="Midway"))+
  geom_sf(data=ctalines,aes(color=Branch),linewidth=2)+
  geom_text( aes(x=lng, y= lat, label = NAME),vjust=-1.9,hjust=1,size=4)+
  scale_color_manual(values=c("#c60c30","#e27ea6","#009b3a","#00a1de","#c60c30","#62361b","#522398","grey","#f9461c","black","#00a1de","#009b3a","#f9e300","black","grey"))+
  #geom_point( aes(x=lng, y= lat, color = Branch),pch=1,size=12)+
  geom_point( aes(x=lng, y= lat,color=Branch),size=14)+
  geom_text( aes(x=lng, y= lat, label = scales::percent(recovery,accuracy=1)),color="black",size=4)+
  geom_text( aes(x=lng, y= lat, label = paste("Apr '19:",scales::comma(APR_19))),vjust=4.5,color="black",size=2.5)+
  geom_text( aes(x=lng, y= lat, label = paste("Apr '24:",scales::comma(APR_24))),vjust=6,color="black",size=2.5)+
  labs(title="CTA Ridership Recovery - Orange Line ",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",fill="%")+
  coord_sf(xlim=c(-87.762617,-87.634957),ylim=c(41.774591,41.87),expand=FALSE)+
  theme_void()+
  theme(text=element_text(family="Tahoma"),legend.position="none")

orecovery

ggsave(plot=orecovery, "orecovery.jpg",height=10,width=10)



ocol <- ggplot(ctastopkeymixed %>% filter(Orange=="Yes" & Branch != "Loop"))+
  geom_col(aes(x=APR_19,y=reorder(NAME,-APR_24),fill=Hex),alpha=0.2,position = "dodge")+
  geom_col(aes(x=APR_24,y=reorder(NAME,-APR_24),fill=Hex),position = "dodge")+
  geom_text(aes(x=APR_24,reorder(NAME,-APR_24),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  labs(x="Average Weekday Riders",title="CTA Ridership Recovery",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",y="Branch")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  scale_fill_identity()+
  scale_color_identity()+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))

ocol

ggsave(plot=ocol, "ocol.jpg",height=10,width=10)



#################################################################################################################################################################################
#Looking at the Testing Blue

bluecol <- ggplot(ctastopkeymixed %>% filter(Blue=="Yes"))+
  geom_col(aes(x=APR_19,y=reorder(STATION,-APR_24),fill=Hex,color=Branch),alpha=0.2,position = "dodge")+
  geom_col(aes(x=APR_24,y=reorder(STATION,-APR_24),fill=Hex,color=Branch),position = "dodge")+
  geom_text(aes(x=APR_24,reorder(STATION,-APR_24),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_color_manual(values=c("red","white","grey","grey"))+
  labs(x="Average Weekday Riders",title="CTA Ridership Recovery",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",y="Stop")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  scale_fill_identity()+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))

bluecol

ggsave(plot=bluecol, "bluecol.jpg",height=10,width=10)





blueback <- ggplot(ctastopkey %>% filter(Branch=="O'Hare" | Branch=="Forest Park"))+
  geom_sf(data=ctalines,aes(color=Hex),linewidth=2)+
  geom_text_repel( aes(x=lng, y= lat, label = NAME),vjust=-4,size=3)+
  scale_color_identity()+
  #geom_point( aes(x=lng, y= lat, color = Branch),pch=1,size=12)+
  geom_point( aes(x=lng, y= lat,color=Hex),size=10)+
  geom_text( aes(x=lng, y= lat, label = scales::percent(recovery,accuracy=1)),color="black",size=4)+
  #geom_text( aes(x=lng, y= lat, label = paste("Apr '19:",scales::comma(APR_19))),vjust=3.5,color="black",size=1,hjust=0)+
  #geom_text( aes(x=lng, y= lat, label = paste("Apr '24:",scales::comma(APR_24))),vjust=5,color="black",size=1,hjust=0)+
  labs(title="CTA Ridership Recovery - Blue Line ",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",fill="%")+
  coord_sf(xlim=c(-87.95,-87.61),ylim=c(41.85,42.0),expand=FALSE)+
  theme_void()+
  theme(text=element_text(family="Tahoma"),legend.position="none")

blueback

ggsave(plot=blueback, "blueback.jpg",height=10,width=10)





#################################################################################################################################################################################
#Looking at the LOOP

ggplot(ctastopkeymixed %>% filter(Branch=="Loop"))+
  geom_col(aes(x=APR_19,y=reorder(STATION,-APR_19),fill=Hex),alpha=0.2,position="dodge")+
  geom_col(aes(x=APR_24,y=reorder(STATION,-APR_19),fill=Hex),,position="dodge")+
  geom_text(aes(x=APR_24,reorder(STATION,-APR_19),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  labs(x="Average Weekday Riders",title="CTA Ridership Recovery",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",y="Branch")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))


ggplot(ctastopkey %>% filter(Branch=="Loop"))+
  geom_sf(data=ctalines,aes(color=Hex),linewidth=2)+
  geom_text_repel( aes(x=lng, y= lat, label = NAME),vjust=-2,hjust=0,size=3)+
  geom_point( aes(x=lng, y= lat,color=Hex),size=10)+
  geom_text( aes(x=lng, y= lat, label = scales::percent(recovery,accuracy=1)),color="black",size=4)+
  geom_text( aes(x=lng, y= lat, label = paste("Apr '19:",scales::comma(APR_19))),vjust=3.5,color="black",size=2.5)+
  geom_text( aes(x=lng, y= lat, label = paste("Apr '24:",scales::comma(APR_24))),vjust=5,color="black",size=2.5)+
  labs(title="CTA Ridership Recovery - Loop ",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",fill="%")+
  scale_fill_identity()+
  scale_color_identity()+
  coord_sf(xlim=c(-87.64,-87.6),ylim=c(41.873,41.889),expand=FALSE)+
  theme_void()+
  theme(text=element_text(family="Tahoma"),legend.position="none")


#################################################################################################################################################################################
#Looking at the Mixed Service Lines


ggplot(ctastopkeymixed %>% filter(Branch=="Mixed"))+
  geom_col(aes(x=APR_19,y=reorder(STATION,-APR_19),fill=Hex),alpha=0.2,position = "dodge")+
  geom_col(aes(x=APR_24,y=reorder(STATION,-APR_19),fill=Hex),position = "dodge")+
  geom_text(aes(x=APR_24,reorder(STATION,-APR_19),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  labs(x="Average Weekday Riders",title="CTA Ridership Recovery",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",y="Station")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))


ggplot(ctastopkey %>% filter(Branch=="Mixed"))+
  geom_sf(data=ctalines,aes(color=Hex),linewidth=2)+
  geom_text_repel( aes(x=lng, y= lat, label = NAME),vjust=-2,hjust=0,size=3)+
  geom_point(data=ctastopkeymixed, aes(x=lng, y= lat,color=Hex),size=10)+
  geom_text( aes(x=lng, y= lat, label = scales::percent(recovery,accuracy=1)),color="black",size=4)+
  geom_text( aes(x=lng, y= lat, label = paste("Apr '19:",scales::comma(APR_19))),vjust=3.5,color="black",size=2.5)+
  geom_text( aes(x=lng, y= lat, label = paste("Apr '24:",scales::comma(APR_24))),vjust=5,color="black",size=2.5)+
  labs(title="CTA Ridership Recovery - Loop ",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",fill="%")+
  scale_fill_identity()+
  scale_color_identity()+
  coord_sf(xlim=c(-87.6,-87.7),ylim=c(41.85,41.95),expand=FALSE)+
  theme_void()+
  theme(text=element_text(family="Tahoma"),legend.position="none")



#################################################################################################################################################################################
#Looking to Brown Town

ggplot(ctastopkey %>% filter(Branch=="Kimball"))+
  geom_col(aes(x=APR_19,y=reorder(NAME,-APR_19),fill=Hex),alpha=0.2)+
  geom_col(aes(x=APR_24,y=reorder(NAME,-APR_19),fill=Hex))+
  geom_text(aes(x=APR_24,reorder(NAME,-APR_19),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  labs(x="Average Weekday Riders",title="CTA Ridership Recovery",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",y="Branch")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))


ggplot(ctastopkey %>% filter(Branch=="Kimball"))+
  geom_sf(data=ctalines,aes(color=Hex),linewidth=2)+
  geom_text_repel( aes(x=lng, y= lat, label = NAME),vjust=-2,hjust=0,size=3)+
  geom_point( aes(x=lng, y= lat,color=Hex),size=10)+
  geom_text( aes(x=lng, y= lat, label = scales::percent(recovery,accuracy=1)),color="white",size=3.5)+
  geom_text( aes(x=lng, y= lat, label = paste("Apr '19:",scales::comma(APR_19))),vjust=3.5,color="black",size=2.5)+
  geom_text( aes(x=lng, y= lat, label = paste("Apr '24:",scales::comma(APR_24))),vjust=5,color="black",size=2.5)+
  labs(title="CTA Ridership Recovery - Brown Line ",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",fill="%")+
  scale_fill_identity()+
  scale_color_identity()+
  coord_sf(xlim=c(-87.75,-87.6),ylim=c(41.94,41.98),expand=FALSE)+
  theme_void()+
  theme(text=element_text(family="Tahoma"),legend.position="none")



#################################################################################################################################################################################
#Looking to Pink

pnkrecovery <- ggplot(ctastopkeymixed %>% filter(Pink=="Yes" & Branch!="Loop"))+
  geom_col(aes(x=APR_19,y=reorder(NAME,-APR_24),fill=Hex),alpha=0.2,position="dodge")+
  geom_col(aes(x=APR_24,y=reorder(NAME,-APR_24),fill=Hex),position="dodge")+
  geom_text(aes(x=APR_24,reorder(NAME,-APR_24),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  labs(x="Average Weekday Riders",title="CTA Ridership Recovery",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",y="Branch")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))

pnkrecovery

ggsave(plot=pnkrecovery, "pnkrecovery.jpg",height=10,width=10)



pnkmap <- ggplot(ctastopkey %>% filter(Pink=="Yes" & Branch!="Loop"))+
  geom_sf(data=ctalines,aes(color=Hex),linewidth=2)+
  geom_text_repel( aes(x=lng, y= lat, label = NAME),vjust=-2,hjust=1,size=3)+
  geom_point( aes(x=lng, y= lat,color=Hex),size=10)+
  geom_text( aes(x=lng, y= lat, label = scales::percent(recovery,accuracy=1)),color="white",size=3.5)+
  geom_text( aes(x=lng, y= lat, label = paste("Apr '19:",scales::comma(APR_19))),vjust=3.5,color="black",size=2)+
  geom_text( aes(x=lng, y= lat, label = paste("Apr '24:",scales::comma(APR_24))),vjust=5,color="black",size=2)+
  labs(title="CTA Ridership Recovery - Pink Line ",subtitle="Average Weekday Entries - April 2024 rides as a percent of April 2019 rides",fill="%")+
  scale_fill_identity()+
  scale_color_identity()+
  coord_sf(xlim=c(-87.78,-87.62),ylim=c(41.84,41.9),expand=FALSE)+
  theme_void()+
  theme(text=element_text(family="Tahoma"),legend.position="none")

pnkmap

ggsave(plot=pnkmap, "pnkmap.jpg",height=8,width=10)
