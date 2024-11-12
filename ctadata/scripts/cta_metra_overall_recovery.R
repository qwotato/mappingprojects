library(tidyverse)
library(sf)
library(ggrepel)
library(extrafont)


#Upload and summarise train data
ctatrain <- read.csv("https://data.cityofchicago.org/resource/t2rn-p8d7.csv?$limit=45000")
ctatrain$date <-  as.Date(ctatrain$month_beginning, format="%Y-%m-%d")
ctatrain <- filter(ctatrain,date > as.Date(17140))
ctatrainmonthly <- ctatrain %>% group_by(date) %>% summarise(ctatrain=sum(monthtotal))

#Upload and summarise bus data
ctabus <- read.csv("https://data.cityofchicago.org/resource/bynn-gwxy.csv?$limit=450000")
ctabus$date <-  as.Date(ctabus$month_beginning, format="%Y-%m-%d")
ctabus <- filter(ctabus,date > as.Date(17140))
ctabusmonthly <- ctabus %>% group_by(date) %>% summarise(ctabus=sum(monthtotal))

#formats data to plot total monthly rides
ctarides <- right_join(ctatrainmonthly, ctabusmonthly, by = "date")
ctarides <- ctarides %>% pivot_longer(!date,names_to = "mode",values_to="rides")

#month-by-month seasonal adjustments
ctarides$month <- format(as.Date(ctarides$date, format="%Y-%m-%d"),"%m")
ctarides$year <- format(as.Date(ctarides$date, format="%Y-%m-%d"),"%Y")



#Bringing in Metra Data. Latest available here:
#https://rtams.org/media/resources/metra-monthly-ridership-line 

metrarides <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/sources/Metra_Monthly_Ridership_by_Line_2002_2024.csv")
metrarides$date <- paste(metrarides$YEAR,"-",metrarides$MONTH,"-","01",sep="")
metrarides$date <-  as.Date(metrarides$date, format="%Y-%m-%d")
metrarides <- metrarides %>% group_by(date, MONTH, YEAR) %>% summarise(rides=sum(RIDES))
names(metrarides)[names(metrarides)== 'YEAR'] <- "year"
names(metrarides)[names(metrarides)== 'MONTH'] <- "month"
metrarides$mode <- "metra"


#Combining Metra and CTA
transitrides <- rbind(ctarides,metrarides)
transitrides <- filter(transitrides,year>2016)



#Plotting rides using SMOOTH
allrecover<- ggplot(transitrides)+
  geom_smooth(data=filter(transitrides,year>2020),aes(x=date,y=rides,color=mode,fill=mode),alpha=0.05)+
  geom_smooth(data=filter(transitrides,year<2020),aes(x=date,y=rides,color=mode,fill=mode),alpha=0.05)+
  geom_line(aes(x=date,y=rides,color=mode),linetype=2,linewidth=1)+
  scale_color_manual(values=c("#41B6E6","#E4002B","navy"),label=c("CTA Bus","CTA Train", "Metra"))+
  scale_fill_manual(values=c("#41B6E6","#E4002B","navy"))+
  guides(fill="none")+
  labs(x="Monthly Ridership",y="Rides","Ridership",title="Chicago Transit Recovery",color="Mode:")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),label=scales::comma)+
  scale_x_date(date_breaks = "year", date_labels = "%Y",expand=c(0,0))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(), legend.position="top",
        panel.grid.major.x = element_line(color="lightgrey"),panel.grid.major.y = element_line(color="lightgrey"))+
  theme(text=element_text(family="Arial"))

allrecover

ggsave(plot=allrecover, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/allrecover.jpeg",height=7,width=8)

#########################################################################
#Rides compared to 2017-2019 monthly avg

transitwider <- pivot_wider(as_tibble(transitrides[,-1]),names_from=year,values_from=rides)

transitwider$preavg <- ((transitwider$"2017" + transitwider$"2018" + transitwider$"2019") / 3)
transitwider$"2017" <- transitwider$"2017" / transitwider$preavg
transitwider$"2018" <- transitwider$"2018" / transitwider$preavg
transitwider$"2019" <- transitwider$"2019" / transitwider$preavg
transitwider$"2020" <- transitwider$"2020" / transitwider$preavg
transitwider$"2021" <- transitwider$"2021" / transitwider$preavg
transitwider$"2022" <- transitwider$"2022" / transitwider$preavg
transitwider$"2023" <- transitwider$"2023" / transitwider$preavg
transitwider$"2024" <- transitwider$"2024" / transitwider$preavg

transitwider$preavg <- NULL

transitavg <- transitwider %>% pivot_longer(cols="2017":"2024",names_to="year",values_to="rideavg")

transitavg$date <- paste(transitavg$year,"-",transitavg$month,"-","01",sep="")
transitavg$date <-  as.Date(transitavg$date, format="%Y-%m-%d")

riderecovery <- transitavg %>% group_by(mode) %>% drop_na(rideavg) %>%filter(date == max(date))

allrecoverpercent <- ggplot(transitavg)+
  geom_line(aes(x=date,y=rideavg,color=mode),linetype=1,linewidth=1)+
  geom_point(data=riderecovery,aes(x=date,y=rideavg,color=mode),show.legend = FALSE)+
  geom_text(data=riderecovery,aes(x=date,y=rideavg,label=scales::percent(rideavg),color=mode),size=3.25,hjust=0.2,vjust=-.45,show.legend = FALSE)+
  scale_color_manual(values=c("#41B6E6","#E4002B","navy"),label=c("CTA Bus","CTA Train", "Metra"))+
  labs(x="Monthly Ridership",y="Ridership Percentage","Ridership",title="Chicago Transit Recovery",color="Mode:",subtitle="Share of pre-2020 average")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),label=scales::percent)+
  scale_x_date(date_breaks = "year", date_labels = "%Y",expand=c(0,0))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(), legend.position="top",
        panel.grid.major.x = element_line(color="lightgrey"),panel.grid.major.y = element_line(color="lightgrey"))+
  theme(text=element_text(family="Arial"))

allrecoverpercent

ggsave(plot=allrecoverpercent, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/allrecoverpercent.jpeg",height=6,width=8)
