library(tidyverse)
library(sf)
library(ggrepel)
library(extrafont)


#Upload and summarise train data
ctatrain <- read.csv("https://data.cityofchicago.org/resource/t2rn-p8d7.csv?$limit=45000")
ctatrain$date <-  as.Date(ctatrain$month_beginning, format="%Y-%m-%d")
ctatrain <- filter(ctatrain,date > as.Date(17140))
ctatrainmonthly <- ctatrain %>% group_by(date) %>% summarise(trainrides=sum(monthtotal))

#Upload and summarise bus data
ctabus <- read.csv("https://data.cityofchicago.org/resource/bynn-gwxy.csv?$limit=450000")
ctabus$date <-  as.Date(ctabus$month_beginning, format="%Y-%m-%d")
ctabus <- filter(ctabus,date > as.Date(17140))
ctabusmonthly <- ctabus %>% group_by(date) %>% summarise(busrides=sum(monthtotal))

#formats data to plot total monthly rides
ctarides <- right_join(ctatrainmonthly, ctabusmonthly, by = "date")
ctarides <- ctarides %>% pivot_longer(!date,names_to = "mode",values_to="rides")

#month-by-month seasonal adjustments
ctarides$month <- format(as.Date(ctarides$date, format="%Y-%m-%d"),"%m")
ctarides$year <- format(as.Date(ctarides$date, format="%Y-%m-%d"),"%Y")

#Plotting rides using SMOOTH
overallrecover<- ggplot(ctarides)+
  geom_smooth(data=filter(ctarides,year>2020),aes(x=date,y=rides,color=mode,fill=mode),alpha=0.05)+
  geom_smooth(data=filter(ctarides,year<2020),aes(x=date,y=rides,color=mode,fill=mode),alpha=0.05)+
  geom_line(aes(x=date,y=rides,color=mode),linetype=2,linewidth=1)+
  scale_color_manual(values=c("#41B6E6","#E4002B"),label=c("Bus","Train"))+
  scale_fill_manual(values=c("#41B6E6","#E4002B"))+
  guides(fill="none")+
  labs(x="Monthly Ridership",title="CTA Ridership Recovery",color="Mode:")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),label=scales::comma)+
  scale_x_date(date_breaks = "year", date_labels = "%Y",expand=c(0,0))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(), legend.position="top",
        panel.grid.major.x = element_line(color="lightgrey"),panel.grid.major.y = element_line(color="lightgrey"))+
  theme(text=element_text(family="Arial"))

overallrecover

ggsave(plot=overallrecover, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/ctadata/plots/overallrecover.jpeg",height=10,width=10)

























#month-by-month seasonal adjustments
ctarides$month <- format(as.Date(ctarides$date, format="%Y-%m-%d"),"%m")
ctarides$year <- format(as.Date(ctarides$date, format="%Y-%m-%d"),"%Y")


ctaseasonal <- ctarides %>% filter(year < 2020)
ctaseasonal <- ctaseasonal %>% group_by(mode,year) %>% mutate(mode_year = sum(rides))
ctaseasonal$monthshare <- ctaseasonal$rides / ctaseasonal$mode_year
ctaseasonal <- ctaseasonal %>% group_by(month,mode) %>% mutate(monthadj = mean(monthshare))
ctaseasonal$monthadj <- ctaseasonal$monthadj / (1/12)
ctaseasonal$monthadj <- (1/ctaseasonal$monthadj)

monthadj <- ctaseasonal %>% group_by(mode,month) %>% summarise(mean(monthadj),.groups = 'keep')




ggplot(ctarides)+
  geom_col(aes(x=date,y=rides,fill=mode),width=25)+
  scale_fill_manual(values=c("#41B6E6","#E4002B"),label=c("Bus","Train"))+
  labs(x="Month",title="CTA Ridership Recovery",fill="Mode:")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),label=scales::comma,expand=c(0,0))+
  scale_x_date(date_breaks = "year", date_labels = "%Y",expand=c(0,0))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color="lightgrey"))+
  theme(text=element_text(family="Arial"))



ggplot(ctaseasonal)+
  geom_line(aes(x=date,y=adjrides,color=mode),linewidth=2)+
  scale_color_manual(values=c("#41B6E6","#E4002B"),label=c("Bus","Train"))+
  labs(x="Month",title="CTA Ridership Recovery",fill="Mode:")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),label=scales::comma)+
  scale_x_date(date_breaks = "year", date_labels = "%Y",expand=c(0,0))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color="lightgrey"))+
  theme(text=element_text(family="Arial"))

