library(tidyverse)
library(sf)
library(extrafont)

########################################

ctabus <- read.csv("https://data.cityofchicago.org/resource/bynn-gwxy.csv?$limit=1500000")

baseyear <- as.Date(18109, format = "%Y-%m-%d")

nowyear <- as.Date(19936, format = "%Y-%m-%d")

ctabus$date <-  as.Date(ctabus$month_beginning, format="%Y-%m-%d")
ctabus$month <- format(as.Date(ctabus$date, format="%Y-%m-%d"),"%m")
ctabus$year <- format(as.Date(ctabus$date, format="%Y-%m-%d"),"%Y")

ctabusbase <- subset(ctabus,date == baseyear)

ctabusbase$basetotal <- ctabusbase$monthtotal

ctabusnow <- subset(ctabus,date == nowyear)

ctabusnow$nowtotal <- ctabusnow$monthtotal

#bring in base rides
ctabusrecovery <- right_join(select(ctabusbase,basetotal,route), ctabusnow, by = "route")

ctabusrecovery$recovery <- ctabusrecovery$nowtotal / ctabusrecovery$basetotal

#ctabusrecovery <- filter(ctabusrecovery,nowtotal > 100000)
ctabusrecovery <- filter(ctabusrecovery,route != 1001)

busrecover <- ggplot(head(arrange(ctabusrecovery,-nowtotal),20))+
  geom_col(aes(x=basetotal,y=reorder(paste(route,"-",routename),-nowtotal)),alpha=0.2,position="dodge")+
  geom_col(aes(x=nowtotal,y=reorder(paste(route,"-",routename),-nowtotal)),position="dodge")+
  geom_text(aes(x=nowtotal,reorder(paste(route,"-",routename),-nowtotal),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  labs(x="Monthly Rides",title="CTA Bus Ridership Recovery",subtitle="Monthly Rides - August 2024 rides as a percent of August 2019 rides",y="Route")+
  theme_classic()+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  theme(text=element_text(family="Arial"))

busrecover

ggsave(plot=busrecover, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/busrecover.jpeg",height=10,width=10)

##############################################
##############################################
# Lakefront Bus Numbers
##############################################
##############################################

ctabuslakefront <- right_join(select(ctabusbase,basetotal,route), ctabusnow, by = "route")

ctabuslakefront <- filter(ctabuslakefront,
                          route == 135 |
                          route == 136 |
                          route == 143 |
                          route == 146 |
                          route == 147 |
                          route == 148 |
                          route == 151 |
                          route == 156 |
                          route == 134 )

ctabuslakefront$recovery <- ctabuslakefront$nowtotal / ctabuslakefront$basetotal

lakefrontrecover <- ggplot(arrange(ctabuslakefront,-nowtotal))+
  geom_col(aes(x=basetotal,y=reorder(paste(route,"-",routename),-nowtotal)),alpha=0.2,position="dodge")+
  geom_col(aes(x=nowtotal,y=reorder(paste(route,"-",routename),-nowtotal)),position="dodge")+
  geom_text(aes(x=nowtotal,reorder(paste(route,"-",routename),-nowtotal),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  labs(x="Monthly Rides",title="CTA Bus Ridership Recovery - North lakefront Routes",subtitle="Monthly Rides - August 2024 rides as a percent of August 2019 rides",y="Route")+
  theme_classic()+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  theme(text=element_text(family="Arial"))

lakefrontrecover

ggsave(plot=lakefrontrecover, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/lakefrontrecover.jpeg",height=10,width=10)

































#Code from trains






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
ctastopkey <- right_join(select(ctaridepullbase,station_id,avg_weekday_rides), ctastopkey, by = "station_id")

#Get column name right
ctastopkey <- rename(ctastopkey, base_weekday_rides = avg_weekday_rides)

#bring in new rides
ctastopkey <- right_join(select(ctaridepullnow,station_id,avg_weekday_rides), ctastopkey, by = "station_id")

#Get column name right
ctastopkey <- rename(ctastopkey, current_weekday_rides = avg_weekday_rides)
  
#Calculate Recovery Ratio
ctastopkey$recovery <- as.numeric( ctastopkey$current_weekday_rides / ctastopkey$base_weekday_rides  )

##################################################################################################################################

#CTA Line JSON
ctalines <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/sources/ctalines.geojson")

##################################################################################################################################

#CTA Stop Key data
ctastopkeymixed <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/sources/ctastopkeymixed.csv")

ctastopkeymixed <- merge(ctastopkeymixed,select(ctastopkey,station_id,base_weekday_rides),by="station_id")
ctastopkeymixed <- merge(ctastopkeymixed,select(ctastopkey,station_id,current_weekday_rides),by="station_id")
ctastopkeymixed <- merge(ctastopkeymixed,select(ctastopkey,station_id,recovery),by="station_id")

##################################################################################################################################

# Branch-by-Branch Recovery

branchsum <- ctastopkey %>% group_by(Branch) %>% summarise_at(c("base_weekday_rides","current_weekday_rides"),sum,na.rm=TRUE)

branchsum$recovery <- branchsum$current_weekday_rides / branchsum$base_weekday_rides

ctabranchback <- ggplot(ctastopkey)+
  geom_col(aes(x=base_weekday_rides,y=reorder(Branch,-base_weekday_rides),fill=Branch),alpha=0.2)+
  geom_col(aes(x=current_weekday_rides,y=reorder(Branch,-base_weekday_rides),fill=Branch))+
  geom_text(data=branchsum,aes(x=current_weekday_rides,reorder(Branch,-base_weekday_rides),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_manual(values=c("#c60c30","#e27ea6","#009b3a","#00a1de","#c60c30","#62361b","#522398","grey","#f9461c","grey","#00a1de","#009b3a","#f9e300","black"))+
  labs(x="Average Weekday Riders",title="CTA Ridership Recovery",subtitle="Average Weekday Entries - August 2024 rides as a percent of August 2019 rides",y="Branch")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  #scale_y_discrete(breaks=NULL)+
  theme_classic()+
  theme(text=element_text(family="Arial"),)

ctabranchback

ggsave(plot=ctabranchback, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/branchrecoveryaug24.jpeg",height=10,width=10)


##################################################################################################################################
#Looking to Pink

pnkrecovery <- ggplot(ctastopkeymixed %>% filter(Pink=="Yes" & Branch!="Loop"))+
  geom_col(aes(x=base_weekday_rides,y=reorder(NAME,-current_weekday_rides),fill=Hex),alpha=0.2,position="dodge")+
  geom_col(aes(x=current_weekday_rides,y=reorder(NAME,-current_weekday_rides),fill=Hex),position="dodge")+
  geom_text(aes(x=current_weekday_rides,reorder(NAME,-current_weekday_rides),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  labs(x="Average Weekday Riders",title="CTA Ridership Recovery",subtitle="Average Weekday Entries - August 2024 rides as a percent of August 2019 rides",y="Branch")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  theme_classic()+
  theme(text=element_text(family="Arial"))

pnkrecovery

ggsave(plot=pnkrecovery, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/pinkrecoveryaug24.jpeg",height=10,width=10)



pnkmap <- ggplot(ctastopkey %>% filter(Pink=="Yes" & Branch!="Loop"))+
  geom_sf(data=ctalines,aes(color=Hex),linewidth=2)+
  geom_text_repel( aes(x=lng, y= lat, label = NAME),vjust=-2,hjust=1,size=3)+
  geom_point( aes(x=lng, y= lat,color=Hex),size=10)+
  geom_text( aes(x=lng, y= lat, label = scales::percent(recovery,accuracy=1)),color="white",size=3.5)+
  labs(title="CTA Ridership Recovery - Pink Line ",subtitle="Average Weekday Entries - Aug 2024 rides as a percent of Aug 2019 rides",fill="%")+
  scale_fill_identity()+
  scale_color_identity()+
  coord_sf(xlim=c(-87.78,-87.62),ylim=c(41.84,41.9),expand=FALSE)+
  theme_void()+
  theme(text=element_text(family="Arial"),legend.position="none")

pnkmap

ggsave(plot=pnkmap, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/pinkrecoverymapaug24.jpeg",height=8,width=10)

######################################################################################
# Most used stations

colback <- ggplot(head(arrange(ctastopkeymixed,-recovery),27))+
  geom_col(aes(x=base_weekday_rides,y=reorder(STATION,current_weekday_rides),fill=Hex),alpha=0.2,position="dodge")+
  geom_col(aes(x=current_weekday_rides,y=reorder(STATION,current_weekday_rides),fill=Hex),position="dodge")+
  geom_text(aes(x=current_weekday_rides,reorder(STATION,current_weekday_rides),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  labs(title="CTA Ridership Recovery",subtitle="Average Weekday Entries - Aug 2024 rides as a percent of Aug 2019 rides",y="Station",x="Average Weekday Riders")+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))

colback

ggsave(plot=colback, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/topstationsaug24.jpeg",height=10,width=10)

######################################################################################
# Most recovered stations

colrecover <- ggplot(head(arrange(ctastopkeymixed,-recovery),27))+
  geom_col(aes(x=base_weekday_rides,y=reorder(STATION,recovery),fill=Hex),alpha=0.2,position="dodge")+
  geom_col(aes(x=current_weekday_rides,y=reorder(STATION,recovery),fill=Hex),position="dodge")+
  geom_text(aes(x=current_weekday_rides,reorder(STATION,recovery),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  labs(title="CTA Ridership Recovery",subtitle="Average Weekday Entries - Aug 2024 rides as a percent of Aug 2019 rides",y="Station",x="Average Weekday Riders")+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))

colrecover

ggsave(plot=colrecover, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/colrecover.jpeg",height=10,width=10)

######################################################################################
##################################################################################################################################
#Loop

looprecovery <- ggplot(ctastopkeymixed %>% filter( Branch=="Loop"))+
  geom_col(aes(x=base_weekday_rides,y=reorder(STATION,-current_weekday_rides),fill=Hex),alpha=0.2,position="dodge")+
  geom_col(aes(x=current_weekday_rides,y=reorder(STATION,-current_weekday_rides),fill=Hex),position="dodge")+
  geom_text(aes(x=current_weekday_rides,reorder(STATION,-current_weekday_rides),y=,label=scales::percent(recovery,accuracy=1)),hjust=-0.1)+
  scale_fill_identity()+
  labs(x="Average Weekday Riders",title="CTA Ridership Recovery",subtitle="Average Weekday Entries - August 2024 rides as a percent of August 2019 rides",y="Branch")+
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
  labs(title="CTA Ridership Recovery - Loop ",subtitle="Average Weekday Entries - Aug 2024 rides as a percent of Aug 2019 rides",fill="%")+
  scale_fill_identity()+
  scale_color_identity()+
  coord_sf(xlim=c(-87.64,-87.62),ylim=c(41.87,41.891),expand=FALSE)+
  theme_void()+
  theme(text=element_text(family="Arial"),legend.position="none",panel.background = element_rect(fill = "#fafbec",colour = "#d5d492", size = 0.5, linetype = "solid"))

loopmap

ggsave(plot=loopmap, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/loopmapaug24.jpeg",height=8,width=10)

######################################################################################
##################################################################################################################################


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



ggplot(ctastopkeymixed)+
  geom_point(aes(x=base_weekday_rides,y=current_weekday_rides,color=Hex),alpha=0.7,position=position_jitter(h=15,w=15))+
  geom_text_repel(data=ctastopkey,aes(x=base_weekday_rides,y=current_weekday_rides,color=Hex,label=NAME),size=3)+
  scale_color_identity()+
  labs(title="CTA Ridership Recovery",subtitle="May 2024 and May 2019",x="May 2019",y="May 2024")+
  scale_x_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(labels=scales::comma, breaks = scales::pretty_breaks(n = 10))+
  xlim(0,25000)+
  ylim(0,25000)+
  theme_classic()+
  theme(text=element_text(family="Tahoma"))





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
