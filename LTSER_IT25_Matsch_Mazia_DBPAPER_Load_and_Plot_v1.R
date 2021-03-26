if (!require("ggplot2")) install.packages("ggplot2");library ("ggplot2")
if (!require("ggpubr")) install.packages("ggpubr");library ("ggpubr")
if (!require("dplyr")) install.packages("dplyr");library ("dplyr")
if (!require("plotly")) install.packages("plotly");library ("plotly")
if (!require("tidyverse")) install.packages("tidyverse");library ("tidyverse")
if (!require("lubridate")) install.packages("lubridate");library ("lubridate")
if (!require("ggridges")) install.packages("ggridges");library ("ggridges")
if (!require("knitr")) install.packages("knitr");library ("knitr")
if (!require("rmarkdown")) install.packages("rmarkdown");library ("rmarkdown")

## LOAD PLOT IT25_DATA

rm(list = ls())

## Some Pre-processing

## In the case you influxdb_query are to large save yearly queries in separates csv.
#write.csv(IT25_Data,"IT25_Data_2019.csv", row.names = T)
## CONCATENATE CSV OF DIFFERENT YEARS
df1<-read.csv("IT25_Data_2017.csv")
df2<-read.csv("IT25_Data_2018.csv")
df3<-read.csv("IT25_Data_2019.csv")
df4<-read.csv("IT25_Data_2020.csv")
IT25_Data<-dplyr::bind_rows(df1,df2,df3,df4)

# Create month, hour, yers new variables inside the IT25_Data dataframe.
IT25_Data$month<-month(IT25_Data$time)
IT25_Data$hour<-hour(IT25_Data$time)
IT25_Data$year<-year(IT25_Data$time)

#Create factors useful to rank the plots by months, hours and station(ranked by elevation)
# IT25_Data$monthfctr <- as.Date(IT25_Data$time, format = "%d-%m-%y") %>%
#     months() %>%
#     as.factor() %>%
#     factor(., levels = monthstr, labels = labelstr)

IT25_Data$monthfctr <- factor(IT25_Data$month, labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
IT25_Data$hourfctr <- factor(IT25_Data$hour)
IT25_Data$stationfctr <- factor(IT25_Data$station, 
                                levels = c("b1", "b2" , "p2", "s2", "m5", "m1", "b3",
                                           "m4","m4 snow","m2", "m3", "s4", "s3"))

IT25_Data$yearfctr <- factor(IT25_Data$year)

#class(IT25_Data$time)
#test<-as.Date(IT25_Data$time)
#pippo<-as.Date(month(test))
#class(pippo)




# modified by giulio
#AirT_month_2017<-IT25_Data%>%select(time,month,year,hour,air_t_h)%>% 
#  group_by(month,year) %>% summarise(AirT_Month = mean(air_t_h, na.rm = T))

# modified by giulio
#AirT_month_2017<-IT25_Data%>%select(time,month,year,hour,air_t_h)%>% 
 # group_by(month,year) %>% summarise(AirT_Month = mean(air_t_h, na.rm = T))


p1 <- ggplot(data = IT25_Data,aes(x=month,y=air_t_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")
ggsave("./Images/Valley_Monthly_AirT.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p1

p11 <- ggplot(data = IT25_Data,aes(x=month,y=air_t_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  facet_wrap(. ~ stationfctr, ncol = 4)
  
ggsave("./Images/Station_Monthly_AirT.tiff", units="cm", width=35, height=20, dpi=3000, compression = 'lzw')
p11


p1

p2 <- ggplot(data = IT25_Data,aes(x=month,y=air_rh_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+

  ylim(0, 100)+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")
ggsave("./Images/Valley_Monthly_RH.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p2

p21 <- ggplot(data = IT25_Data,aes(x=month,y=air_rh_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  ylim(0, 100)+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  facet_wrap(. ~ stationfctr, ncol = 4)
ggsave("./Images/Station_Monthly_RH.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p21

p3 <- ggplot(data = IT25_Data,aes(x=month,y=wind_speed_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
    scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")
ggsave("./Images/Valley_Monthly_Wsp.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p3

p31 <- ggplot(data = IT25_Data,aes(x=month,y=wind_speed_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  facet_wrap(. ~ stationfctr, ncol = 4)
ggsave("./Images/Station_Monthly_Wsp.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p31

p4 <- ggplot(data = IT25_Data,aes(x=month,y=wind_speed_max_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")
ggsave("./Images/Valley_Monthly_WspMax.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p4


p41 <- ggplot(data = IT25_Data,aes(x=month,y=wind_speed_max_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  facet_wrap(. ~ stationfctr, ncol = 4)

ggsave("./Images/Station_Monthly_WspMax.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p41

p44 <- ggplot(data = IT25_Data,aes(x=hour,y=wind_dir_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(0,24,2))+
  labs(x="Months")+
  ylim(50, 300)
ggsave("./Images/Valley_Monthly_WDir.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')

p44


p441 <- ggplot(data = IT25_Data,aes(x=hour,y=wind_dir_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(0,24,2))+
  labs(x="Months")+
  ylim(50, 300)+
  facet_wrap(. ~ stationfctr, ncol = 4)
ggsave("./Images/Station_Monthly_WDir.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')

p441




p5 <- ggplot(data = IT25_Data,aes(x=month,y=swc_st_02_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  ylim(-5, 25)
ggsave("./Images/Valley_Monthly_swc_st_02.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p5

p51 <- ggplot(data = IT25_Data,aes(x=month,y=swc_st_02_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  ylim(-5, 25)+
  facet_wrap(. ~ stationfctr, ncol = 4)
ggsave("./Images/Station_Monthly_swc_st_02.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p51

p6 <- ggplot(data = IT25_Data,aes(x=month,y=swc_st_05_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  ylim(-5, 25)
ggsave("./Images/Valley_Monthly_swc_st_05.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p6

p61 <- ggplot(data = IT25_Data,aes(x=month,y=swc_st_05_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  ylim(-5, 25)+
  facet_wrap(. ~ stationfctr, ncol = 4)
ggsave("./Images/Station_Monthly_swc_st_05.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p61

p7 <- ggplot(data = IT25_Data,aes(x=month,y=swc_st_20_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  ylim(-5, 25)
ggsave("./Images/Valley_Monthly_swc_st_20.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p7

p71 <- ggplot(data = IT25_Data,aes(x=month,y=swc_st_20_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  ylim(-5, 25)+
  facet_wrap(. ~ stationfctr, ncol = 4)
ggsave("./Images/Station_Monthly_swc_st_20.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p71
#gg1<-ggarrange(p5, p6,p7, ncol = 3, nrow = 1)
#gg1

p8 <- ggplot(data = IT25_Data,aes(x=month,y=swc_wc_02_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  ylim(0, 0.5)
ggsave("./Images/Valley_Monthly_swc_wc_02.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p8

p81 <- ggplot(data = IT25_Data,aes(x=month,y=swc_wc_02_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  ylim(0, 0.5)+
  facet_wrap(. ~ stationfctr, ncol = 4)
ggsave("./Images/Station_Monthly_swc_wc_02.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p81


SWC2cm<-IT25_Data %>% select(time,station,stationfctr, swc_wc_02_h,elevation,monthfctr,month,hourfctr,hour,landuse, year,yearfctr)
p81<-ggplot(data = SWC2cm)+
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  facet_wrap(. ~ stationfctr, ncol = 4)
p81


p9 <- ggplot(data = IT25_Data,aes(x=month,y=swc_wc_05_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  ylim(0, 0.5)
ggsave("./Images/Valley_Monthly_swc_wc_05.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p9

p91 <- ggplot(data = IT25_Data,aes(x=month,y=swc_wc_05_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  ylim(0, 0.5)+
  facet_wrap(. ~ stationfctr, ncol = 4)
ggsave("./Images/Station_Monthly_swc_wc_05.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p91



p10 <- ggplot(data = IT25_Data,aes(x=month,y=swc_wc_20_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  ylim(0, 0.5)
ggsave("./Images/Valley_Monthly_swc_wc_20.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p10

p101 <- ggplot(data = IT25_Data,aes(x=month,y=swc_wc_20_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  ylim(0, 0.5)+
  facet_wrap(. ~ stationfctr, ncol = 4)
ggsave("./Images/Station_Monthly_swc_wc_20.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p101





p11 <- ggplot(data = IT25_Data,aes(x=month,y=swp_wp_05_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")
ggsave("./Images/Valley_Monthly_swp_wp_05.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p11


p111 <- ggplot(data = IT25_Data,aes(x=month,y=swp_wp_05_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  ylim(-5000, 0)+
  facet_wrap(. ~ stationfctr, ncol = 4)
ggsave("./Images/Station_Monthly_swp_wp_05.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p111


p12 <- ggplot(data = IT25_Data,aes(x=month,y=swp_wp_20_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")
ggsave("./Images/Valley_Monthly_swp_wp_20.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p12


p121 <- ggplot(data = IT25_Data,aes(x=month,y=swp_wp_20_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  facet_wrap(. ~ stationfctr, ncol = 4)
ggsave("./Images/Station_Monthly_swp_wp_20.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p121


Tb1<-IT25_Data %>% dplyr::filter(station== "b1")
Ts3<-IT25_Data %>% dplyr::filter(station== "s3")

Tb1<-IT25_Data %>% select(time,station, air_t_h,elevation,monthfctr,month,hourfctr)%>%
  dplyr::filter(station== "b1")
Ts3<-IT25_Data %>% select(time,station, air_t_h,elevation,monthfctr,month,hourfctr)%>%
  dplyr::filter(station== "s3")

Ts3b1<-inner_join(Tb1, Ts3, by = "time")
laps<-Ts3b1%>% dplyr::mutate(laps = (Ts3b1$air_t_h.x-Ts3b1$air_t_h.y)/(Ts3b1$elevation.y/1000-Ts3b1$elevation.x/1000))

p13<-ggplot(data = laps)+
  geom_boxplot(aes(x =hourfctr.x, y= laps))+
  ylab("Hourly Air Temp lapsrate from 1000-2700 m a.s.l. ")+
  theme_minimal()+
  ylim(-2.5, 12.5)+
  facet_wrap(. ~ reorder(monthfctr.x,month.x), ncol = 4)+
  xlab("Hours of the day")
ggsave("./Images/Station_Hourly_AirT_Lapsrate.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p13   


library(ggridges)
library(ggplot2)

# Diamonds dataset is provided by R natively
#head(diamonds)

##SWC
p14<-ggplot(IT25_Data, aes(x = swc_wc_02_h, y = stationfctr, fill = stationfctr)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ggsave("./Images/Station_Density_Distribution_swc_wc_02.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p14

p141<-ggplot(IT25_Data, aes(x = swc_wc_02_h, y = stationfctr, fill = stationfctr)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  facet_wrap(. ~ yearfctr, ncol = 4)
ggsave("./Images/Station_Density_Distribution_Yearly_swc_wc_02.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p141



p15<-ggplot(IT25_Data, aes(x = swc_wc_05_h, y = stationfctr, fill = stationfctr)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ggsave("./Images/Station_Density_Distribution_swc_wc_05.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p15

p151<-ggplot(IT25_Data, aes(x = swc_wc_05_h, y = stationfctr, fill = stationfctr)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  facet_wrap(. ~ yearfctr, ncol = 4)
ggsave("./Images/Station_Density_Distribution_Yearly_swc_wc_05.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p151

p16<-ggplot(IT25_Data, aes(x = swc_wc_20_h, y = stationfctr, fill = stationfctr)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ggsave("./Images/Station_Density_Distribution_swc_wc_20.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p16

p161<-ggplot(IT25_Data, aes(x = swc_wc_20_h, y = stationfctr, fill = stationfctr)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  facet_wrap(. ~ yearfctr, ncol = 4)
ggsave("./Images/Station_Density_Distribution_Yearly_swc_wc_20.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p161

##SWP
p17<-ggplot(IT25_Data, aes(x = swp_wp_05_h, y = stationfctr, fill = stationfctr)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  xlim(-100, 0)
ggsave("./Images/Station_Density_Distribution_swp_wp_05.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p17

p171<-ggplot(IT25_Data, aes(x = swp_wp_05_h, y = stationfctr, fill = stationfctr)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  xlim(-100, 0)+
  facet_wrap(. ~ yearfctr, ncol = 4)
ggsave("./Images/Station_Density_Distribution_Yearly_swp_wp_05.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p171

p17log<-IT25_Data %>%
  ggplot(aes(x=-1*swp_wp_05_h, linetype=yearfctr))+
  geom_density(alpha=0.1)+
  theme(legend.position="top")+
  scale_x_log10()+
  scale_y_log10()+
  labs(x="xaxis log SWP 5 ; yaxis log density ")+
  facet_wrap(. ~ stationfctr, ncol = 4)
ggsave("./Images/Station_Density_Distribution_Yearly_log-log_swp_wp_5.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p17log


p18<-ggplot(IT25_Data, aes(x = swp_wp_20_h, y = stationfctr, fill = stationfctr)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  xlim(-100, 0)
ggsave("./Images/Station_Density_Distribution_swp_wp_5tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p18



p181<-ggplot(IT25_Data, aes(x = swp_wp_20_h, y = stationfctr, fill = stationfctr)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  xlim(-100, 0)+
  facet_wrap(. ~ yearfctr, ncol = 4)
ggsave("./Images/Station_Density_Distribution_Yearly_swp_wp_20.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p181

p181log<-IT25_Data %>%
  ggplot(aes(x=-1*swp_wp_20_h, linetype=yearfctr))+
  geom_density(alpha=0.1)+
  theme(legend.position="top")+
  scale_x_log10()+
  scale_y_log10()+
  labs(x="xaxis log SWP 20 ; yaxis log density ")+
    facet_wrap(. ~ stationfctr, ncol = 4)
ggsave("./Images/Station_Density_Distribution_Yearly_log-log_swp_wp_20.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p181log

<<<<<<< HEAD


p19<-ggplot(data = IT25_Data)+
  geom_boxplot(aes(x = monthfctr, y= sr_h))+
  ylab(" Global Radiaiton %")+
  xlab(" Months")+
  theme_minimal()+
  xlab("Months")+
  ylim(0, 1000)
p19




p12 <- ggplot(data = IT25_Data,aes(x=month,y=nr_up_sw_avg_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  scale_x_continuous(breaks=seq(1,12,1),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(x="Months")+
  facet_wrap(. ~ stationfctr, ncol = 4)
ggsave("./Images/Valley_Monthly_sr.tiff", units="cm", width=35, height=20, dpi=300, compression = 'lzw')
p12
=======
>>>>>>> 59c5a6ea4142a392c0d93f2a0084e0f45bb42bf6
