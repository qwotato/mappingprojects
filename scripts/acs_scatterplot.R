library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)
library(sf)
library(extrafont)
library(ggrepel)
library(mapboxapi)
library(mapview)
library(plotly)

tmap_mode("plot")

# https://walker-data.com/census-r/mapping-census-data-with-r.html


chicago <- read_sf("https://data.cityofchicago.org/resource/qqq8-j68g.geojson")

dlsd <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/dlsd.geojson")

ctalines <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/ctalines.geojson")

metralines <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/metra_lines.geojson")

lakemich <- read_sf("C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/maps/lakeblock.geojson")

#####################################################################################################################
#Looking at number of kids in tracts

cook_county_scatter <- get_acs(
  geography = "tract",
  state = "IL",
  county = "Cook",
  variables = c(
    car_commuters = "B08006_003",
    no_vehicle_available = "B08141_002",
    median_earnings = "B08121_001"
  ),
  summary_var = "B08014_001",
  year = 2022,
  geometry = TRUE
) %>% select(-moe, -summary_moe) %>% 
  pivot_wider(names_from=variable,values_from = estimate) %>%
  mutate(vehicle_available = (summary_est - no_vehicle_available)) %>%
  mutate(carcommuteshare = (car_commuters/ summary_est)) %>%
  mutate(novehicleshare = (no_vehicle_available/ summary_est)) %>%
  mutate(vehicle_not_commute = ((vehicle_available - car_commuters)/summary_est))


#####################################################################################################################

censusscatter <- ggplot(cook_county_scatter)+
  geom_point(aes(x=vehicle_not_commute,y=median_earnings,color=median_earnings))+
  geom_smooth(aes(x=vehicle_not_commute,y=median_earnings),method="lm")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),label=scales::percent)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6),label=scales::comma)+
  scale_color_stepsn(colours = c("black","red4","red3","red2","red"))+
  labs( title="Cook County vehicle access, non car commuters",
      caption="Source: U.S. Census Bureau American Community Survery Data 2022")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"), plot.subtitle = element_text(hjust = 0.5),
        axis.text.y=element_text(color="black"), axis.text.x=element_text(color="black"),plot.caption = element_text( color="#404040"),
        panel.border = element_blank(),panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color="lightgrey"),panel.grid.major.y = element_line(color="lightgrey"))+
  theme(text=element_text(family="Arial"))

censusscatter


ggsave(plot=censusscatter, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/censusscatter.jpeg",height=6,width=6)


ggplotly(censusscatter)






car_without_commute <- ggplot(cook_county_scatter)+
  geom_sf(data = lakemich,fill="#6fc1df",color="#6fc1df")+
  geom_sf(aes(fill=vehicle_not_commute),color=NA)+
  scale_fill_stepsn(name="car access, \nnot a car \ncommuter:",
                    colours = c("#fafbec","#eaeccf","lightgreen","lightgreen","green3","green4","darkgreen"),
                    breaks = c(0,.1,.2,.3,.4,.5,.6),
                    labels = scales::percent)+
  geom_sf(data=ctalines,fill=NA,aes(color=Hex),linewidth=0.8)+
  scale_color_identity()+
  geom_sf(data=chicago,fill=NA,color="black",linewidth=0.5)+
  geom_sf(data=metralines,fill=NA,color="darkgrey",linewidth=0.5)+
  #geom_sf(data=dlsd,fill=NA,color="#F87F85",linewidth=2)+
  #geom_text(aes(x=-87.59,y=41.92,label="NDLSD"),color="#F87F85",size=10,face="bold")+
  labs(title="Cook County vehicle access,\nnon car commuters",subtitle="Workers 16+",caption="Source: U.S. Census Bureau American Community Survery Data 2022")+
  theme_void()+
  theme(text=element_text(family="Tahoma"))+
  coord_sf(xlim=c(-87.96,-87.515),ylim=c(41.625,42.05),expand=FALSE)+
  theme(plot.title = element_text(face="bold",size=35),
        plot.subtitle = element_text(),
        plot.caption = element_text(size=10),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.position = c(0.9,0.8),
        text=element_text(family="Arial"),
        panel.background = element_rect(fill = "#fafbec",colour = "#eaeccf", size = 0.5, linetype = "solid"))

car_without_commute

ggsave(plot=car_without_commute, "C:/Users/arcannon/OneDrive - Burns & McDonnell/FE PE School/Rstudio2022/ILVIS/chicagohousing/plots/car_without_commute.jpeg",height=14.5,width=11)



options(viewer = NULL)
tmap_mode("view")
mapview(cook_county_scatter,zcol="vehicle_not_commute",alpha=0.05,col.regions =mapviewPalette("mapviewTopoColors"), legend = TRUE)
