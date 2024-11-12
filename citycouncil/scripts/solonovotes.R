library(tidyverse)
library(ggimage)
library(extrafont)
library(stringr)


wardcalls <- read.csv("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/citycouncil/wardcalls.csv")



solono <- ggplot(data=filter(wardcalls,solono>0), aes(x=fct_rev(fct_reorder(alder,solono)),y=solono))+
  geom_col(fill="#41B6E6")+
  geom_image(aes(image=image),size=.1,nudge_y=0.58)+
  geom_text(aes(label=solono,y=0.5),color="white",vjust=0.5,size=10)+
  labs(title="Chicago City Council: 49-1",, subtitle="Alders who cast the single 'No' vote on a piece of legislation. 2023 term - Present.",hjust=0.5)+
  labs(x="",y="Number of times voting 'No' alone", caption= "Vis: @alexcannon7.bsky.social, data & headshots: chicago.councilmatic.org/")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8),expand=c(0,0))+
  scale_y_discrete(expand=c(0,0))+
  coord_cartesian(ylim=c(0, 11.5))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), axis.text.y=element_blank(), axis.text.x=element_text(color="black"),
        panel.grid = element_blank(), panel.border = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(family="Arial"))

solono

ggsave(plot=solono, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/citycouncil/plots/solono.jpeg")






truantalder <- ggplot(data=filter(wardcalls,attendance<80), aes(x=fct_reorder(alder,ward),y=attendance))+
  geom_col(fill="#E4002B")+
  geom_image(aes(image=image),size=.1,nudge_y=-0.45)+
  geom_text(aes(label=attendance,y=attendance),color="black",vjust=-.15,size=5)+
  labs(title="Chicago City Council Meeting Attendance",, subtitle="Alder Attendance Below 80% In Current Legislative Session",hjust=0.5)+
  labs(x="",y="Meeting Attendance", caption= "Vis: @alexcannon7.bsky.social, data & headshots: chicago.councilmatic.org/")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8),expand=c(0,0))+
  scale_y_discrete(expand=c(0,0))+
  coord_cartesian(ylim=c(0, 5.5))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), axis.text.y=element_blank(), axis.text.x=element_text(color="black"),
        panel.grid = element_blank(), panel.border = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(family="Arial"))

truantalder

ggsave(plot=truantalder, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/citycouncil/plots/truantalder.jpeg")




attendalder <- ggplot(data=filter(wardcalls,attendance>90), aes(x=fct_reorder(alder,ward),y=attendance))+
  geom_col(fill="#41B6E6")+
  geom_image(aes(image=image),size=.06,nudge_y=-0.45)+
  geom_text(aes(label=attendance,y=attendance),color="black",vjust=-.15,size=5)+
  labs(title="Chicago City Council Meeting Attendance",, subtitle="Alder Attendance Above 90% In Current Legislative Session",hjust=0.5)+
  labs(x="",y="Meeting Attendance", caption= "Vis: @alexcannon7.bsky.social, data & headshots: chicago.councilmatic.org/")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8),expand=c(0,0))+
  scale_y_discrete(expand=c(0,0))+
  coord_cartesian(ylim=c(0, 7.5))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), axis.text.y=element_blank(), axis.text.x=element_text(color="black"),
        panel.grid = element_blank(), panel.border = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text=element_text(family="Arial"))

attendalder

ggsave(plot=attendalder, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/citycouncil/plots/attendalder.jpeg",height=8,width=14)
