# https://cran.r-project.org/web/packages/influxdbr/index.html
#library(influxdbr)
if (!require("ggplot2")) install.packages("ggplot2");library ("ggplot2")
if (!require("ggpubr")) install.packages("ggpubr");library ("ggpubr")
if (!require("dplyr")) install.packages("dplyr");library ("dplyr")
if (!require("plotly")) install.packages("plotly");library ("plotly")
if (!require("tidyverse")) install.packages("tidyverse");library ("tidyverse")
if (!require("lubridate")) install.packages("lubridate");library ("lubridate")
# Create a connection to InfluxDB.
#
# INFO: For security reasons we cannot include username and password here.
#       Please create a ticket at https://support.scientificnet.org with the following
#       information: 
#
#       Subject: InfluxDB: Access to LTER "lter" Database
#       Text: Please create a username and password for accessing the LTER "lter" database.

rm(list = ls())

## Some Pre-processing

## In the case you influxdb_query are to large save yearly queries in separates csv.
#write.csv(IT25_Data,"IT25_Data_2019.csv", row.names = T)
## CONCATENATE CSV OF DIFFERENT YEARS
df1<-read.csv("IT25_Data_2017.csv")
df2<-read.csv("IT25_Data_2018.csv")
IT25_Data<-dplyr::bind_rows(df1,df2)


# to determine the data tpe of a variable or column of dataframe
#class(IT25_Data$monthfctr)

#class(AirT_Month_Full$month)


# Create strings of the months and hours
#monthstr <-c("January","February","March","April","May","June","July","August","September","October","November","December")
#labelstr <-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#hourstr <-c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23") 

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



#---- Box plot Monthly distribution
## MONTHY DISTRIBUTIONS


# modified by giulio
AirT_month_2017<-IT25_Data%>%select(time,month,year,hour,air_t_h)%>% 
    group_by(month,year) %>% summarise(AirT_Month = mean(air_t_h, na.rm = T))


# modified by giulio
# you can use lubridate::yday(time) to plot the day of the year
p1 <- ggplot(data = IT25_Data,aes(x=month,y=air_t_h))+
      geom_smooth(stat = 'summary', linetype=0,
           fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                             y = mean(y), ymax = quantile(y, .9)))+
      geom_smooth(aes(color = as.character(year)), stat = 'summary',
      fun.data = function(y) data.frame(y = mean(y)))+
      ylim(-15, 25)
p1

# added by giulio
ggplot(data = IT25_Data,aes(x=round_date(ymd_hms(time),"month"),y=air_t_h))+
    
    geom_smooth(stat = 'summary', linetype=0,
           fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                             y = mean(y), ymax = quantile(y, .9)))+
    
    geom_smooth(aes(color = as.character(year)), stat = 'summary',
    fun.data = function(y) data.frame(y = mean(y)))+
    ylim(-15, 25)+
facet_wrap(vars(station))

# added by giulio
IT25_Data%>%
ggplot(aes(x=ymd_hms(time),y=air_t_h))+
geom_point(size=0.4,alpha=0.3)+
geom_smooth()

# added by giulio
month_airT = IT25_Data%>%
mutate(month = round_date(ymd_hms(time),"month"))%>%
select(month,air_t_h)%>% 
group_by(month) %>% 
summarise(air_t_h_avg = mean(air_t_h, na.rm = TRUE),
air_t_h_q1 = quantile(air_t_h,.1,na.rm=TRUE),
air_t_h_q9 = quantile(air_t_h,.9,na.rm=TRUE)) 

p1<-ggplot(data = IT25_Data,aes(x=month,y=air_t_h))+
    geom_smooth(stat = 'summary', linetype=0,
                fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                  y = mean(y), ymax = quantile(y, .9)))+
    ylim(-15, 25)+
    geom_line(data=AirT_month_2017, aes(x=month, y=AirT_Month_2017, color="red"))+
        geom_line(data=AirT_month_2018, aes(x=month, y=AirT_Month_2018, color="blue"))

p1
## boxplot monthly air_t_h distributions Mazia (all stations)
p1<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= air_t_h, color = yearfctr))+
    ylab("Air Temp ?C")+
    theme_minimal()+
    xlab("Months")+
    ylim(-20, 30)
p1


## boxplot monthly air_t_h distributions Mazia (all stations)
p1<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x=monthfctr, y= air_t_h))+
    ylab("Air Temp ?C")+
    theme_minimal()+
    xlab("Months")+
    ylim(-20, 30)
p1

p1<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = hourfctr, y= air_t_h, color =yearfctr))+
    ylab("Air Temp ?C")+
    xlab("Hour of the day")+
    theme_minimal()+
    ylim(-20, 30)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p1
## boxplot monthly air_rh_h distributions Mazia (all stations)
p2<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= air_rh_h))+
    ylab("RH %")+
    theme_minimal()+
    xlab("Months")+
    ylim(0, 110)
p2
## boxplot monthly wind_dir_h distributions Mazia (all stations)

p3<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= wind_dir_h))+
    ylab("Wind Dir ?Degree")+
    theme_minimal()+
    xlab("Months")+
    ylim(0, 360)
p3

## boxplot monthly wind_speed_h distributions Mazia (all stations)
p4<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= wind_speed_h))+
    ylab("Wind Speed %")+
    theme_minimal()+
    xlab("Months")+
    ylim(0, 20)
p4
## boxplot monthly wind_speed_max_h distributions Mazia (all stations)
p5<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= wind_speed_max_h))+
    ylab("Wind Speed Max %")+
    theme_minimal()+
    xlab("Months")+
    ylim(0, 20)
p5

## boxplot monthly swc_st_02_h distributions Mazia (all stations)
p6<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= swc_st_02_h))+
    ylab("Soil Temp 2 cm ?C")+
    theme_minimal()+
    xlab("Months")+
    ylim(-5, 35)
p6

## boxplot monthly swc_st_05_h distributions Mazia (all stations)
p7<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= swc_st_05_h))+
    ylab("Soil Temp 5 cm ?C")+
    theme_minimal()+
    xlab("Months")+
    ylim(-5, 35)
p7
## boxplot monthly swc_st_20_h distributions Mazia (all stations)
p8<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= swc_st_20_h))+
    ylab("Soil Temp 20 cm ?C")+
    theme_minimal()+
    xlab("Months")+
    ylim(-5, 35)
p8

## boxplot monthly swc_wc_02_h distributions Mazia (all stations)
p9<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= swc_wc_02_h))+
    ylab("Soil Moisture 2 cm vol/vol")+
    theme_minimal()+
    xlab("Months")+
    ylim(0, 0.5)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p9


## boxplot monthly swc_wc_05_h distributions Mazia (all stations)
p10<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= swc_wc_05_h))+
    ylab("Soil Moisture 5 cm vol/vol")+
    theme_minimal()+
    xlab("Months")+
    ylim(0, 0.5)
p10

## boxplot monthly swc_wc_05_h distributions Mazia (all stations)
p10<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= swc_wc_05_h))+
    ylab("Soil Moisture 5 cm vol/vol")+
    theme_minimal()+
    xlab("Months")+
    ylim(0, 0.5)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p10



## boxplot monthly swc_wc_20_h distributions Mazia (all stations)
p11<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= swc_wc_20_h))+
    ylab("Soil Moisture 20 cm vol/vol")+
    theme_minimal()+
    xlab("Months")+
    ylim(0, 0.5)
p11

## boxplot monthly swc_wc_20_h distributions Mazia (all stations)
p11<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= swc_wc_20_h))+
    ylab("Soil Moisture 20 cm vol/vol")+
    theme_minimal()+
    xlab("Months")+
    ylim(0, 0.5)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p11

## boxplot monthly swc_wp_05_h distributions Mazia (all stations)
p12<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= swp_wp_05_h))+
    ylab("Soil Water Potential 5cm - kPa")+
    theme_minimal()+
    xlab("Months")+
    ylim(0, -750)
p12

## boxplot monthly swc_wp_05_h distributions Mazia (all stations)
p12<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= swp_wp_05_h))+
    ylab("Soil Water Potential 5cm - kPa")+
    theme_minimal()+
    xlab("Months")+
    ylim(0, -750)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p12

## boxplot monthly swc_wp_20_h distributions Mazia (all stations)
p13<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= swp_wp_20_h))+
    ylab("Soil Water Potential 20 cm - kPa")+
    theme_minimal()+
    xlab("Months")+
    ylim(0, -750)
p13
## boxplot monthly swc_wp_20_h distributions Mazia (all stations)
p13<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= swp_wp_20_h))+
    ylab("Soil Water Potential 20 cm - kPa")+
    theme_minimal()+
    xlab("Months")+
    ylim(0, -750)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p13


IT25_Data$fpar <- (IT25_Data$par_soil_h / IT25_Data$par_h)*100

## boxplot monthly fpar distributions Mazia (all stations)
p14<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= fpar))+
    ylab(" Absorbed PAR %")+
    xlab(" Months")+
    theme_minimal()+
    xlab("Months")+
    ylim(0, 60)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p14

## boxplot monthly sr_h distributions Mazia (all stations)
p15<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= sr_h))+
    ylab(" Global Radiaiton %")+
    xlab(" Months")+
    theme_minimal()+
    xlab("Months")+
    ylim(0, 1000)
p15


#IT25_Data<-subset(IT25_Data, select = -c(precipt_cum_h ))
#IT25_Data$precip_cum_h <- (cumsum(IT25_Data$precip_h))



## boxplot monthly precip distributions Mazia (all stations)
p16<-ggplot(IT25_Data)+
    geom_col(aes(x=monthfctr,y=precip_h))+
    ylab(" Precipitation mm")+
    xlab(" Months")+
    theme_minimal()+
    xlab("Months")+
    facet_wrap(. ~ stationfctr, ncol = 4)
#ylim(0, 15)
p16

gg1<-ggarrange(p1, p2,p3,p4,p5, p6,p7,p8,p9,p10,p11,p12,p13,p14,p15, ncol = 4, nrow = 4)

## HOURLY DISTRIBUTION
## ## boxplot hourly wind_dir_h distributions Mazia (all stations)

## boxplot hourly air_t_h distributions Mazia (all stations)
p1<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = hourfctr, y= air_t_h, color =yearfctr))+
    ylab("Air Temp ?C")+
    xlab("Hour of the day")+
    theme_minimal()+
    ylim(-20, 30)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p1
p1<-ggplot(data = IT25_Data, aes(x= month,y=air_t_h, color = yearfctr))+
    geom_smooth(stat = 'summary', 
                fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                  y = mean(y), ymax = quantile(y, .9)))+
    ylim(-15, 25)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p1

ggplot(data = IT25_Data,aes(x=month,y=air_t_h))+
    geom
geom_smooth(stat = 'summary', 
            fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                              y = mean(y), ymax = quantile(y, .9)))+
    ylim(-15, 25)+
    facet_wrap(. ~ stationfctr, ncol = 4)


## boxplot hourly air_rh_h distributions Mazia (all stations)
p2<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = hourfctr, y= air_rh_h))+
    ylab("RH %")+
    xlab("Hour of the day")+
    theme_minimal()+
    ylim(0, 110)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p2
## boxplot hourly wind_dir_h distributions Mazia (all stations)
p3<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = hourfctr, y= wind_dir_h))+
    ylab("Wind Dir ?Degree")+
    xlab("Hour of the day")+
    theme_minimal()+
    ylim(0, 360)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p3

## boxplot hourly wind_speed_h distributions Mazia (all stations)
p4<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = hourfctr, y= wind_speed_h))+
    ylab("Wind Speed %")+
    xlab("Hour of the day")+
    theme_minimal()+
    ylim(0, 15)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p4

## boxplot hourly wind_speed_max_h distributions Mazia (all stations)
p5<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = hourfctr, y= wind_speed_max_h))+
    ylab("Wind Speed Max %")+
    xlab("Hour of the day")+
    theme_minimal()+
    ylim(0, 20)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p5

## boxplot hourly swc_st_02_h distributions Mazia (all stations)
p6<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = hourfctr, y= swc_st_02_h))+
    ylab("Soil Temp 2 cm ?C")+
    xlab("Hour of the day")+
    theme_minimal()+
    ylim(0, 20)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p6


## boxplot hourly  swc_st_05_h distributions Mazia (all stations)
p7<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = hourfctr, y= swc_st_05_h))+
    ylab("Soil Temp 5 cm ?C")+
    theme_minimal()+
    xlab("Hour of the day")+
    ylim(-5, 35)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p7
## boxplothourly  swc_st_20_h distributions Mazia (all stations)
p8<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = hourfctr, y= swc_st_20_h))+
    ylab("Soil Temp 20 cm ?C")+
    theme_minimal()+
    xlab("Hour of the day")+
    ylim(-5, 35)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p8

# boxplot monthly sr_h distributions Mazia (all stations)
p15<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x = monthfctr, y= sr_h))+
    ylab(" Global Radiation ")+
    xlab(" Months")+
    theme_minimal()+
    xlab("Months")+
    ylim(0, 1000)+
    facet_wrap(. ~ stationfctr, ncol = 4)
p15


#IT25_Data%>% aggregate(precip_h, by =list(~ monthfctr, sum)

## boxplot monthly precip distributions Mazia (all stations)
p16<-ggplot(IT25_Data,aes(month,precip_h))+
    geom_col()+
    ylab(" Precipitation mm")+
    xlab(" Months")+
    theme_minimal()+
    xlab("Months")#+
#facet_wrap(. ~ stationfctr, ncol = 4)
# #ylim(0, 1000)
# p16
#ggplotly(p16)

#gg2<-ggarrange(p1, p2,p3,p4,p5, p6,p7,p8, ncol = 2, nrow = 4)
# 
## boxplot hourly  swc_wc_02_h distributions Mazia (all stations)
# p9<-ggplot(data = IT25_Data)+
#     geom_boxplot(aes(x = hourfctr, y= swc_wc_02_h))+
#     ylab("Soil Moisture 2 cm vol/vol")+
#     theme_minimal()+
#     xlab("Hour of the day")+
#     ylim(0, 0.5)+
#     facet_wrap(. ~ stationfctr, ncol = 4)
# p9
# 
# ## boxplot hourly  swc_wc_05_h distributions Mazia (all stations)
# p10<-ggplot(data = IT25_Data)+
#     geom_boxplot(aes(x = hourfctr, y= swc_wc_05_h))+
#     ylab("Soil Moisture 5 cm vol/vol")+
#     theme_minimal()+
#     xlab("Hour of the day")+
#     ylim(0, 0.5)+
#     facet_wrap(. ~ stationfctr, ncol = 4)
# p10
# 
# ## boxplot monthly swc_wc_20_h distributions Mazia (all stations)
# p11<-ggplot(data = IT25_Data)+
#     geom_boxplot(aes(x = hourfctr, y= swc_wc_20_h))+
#     ylab("Soil Moisture 20 cm vol/vol")+
#     theme_minimal()+
#     xlab("Hour of the day")+
#     ylim(0, 0.5)+
#     facet_wrap(. ~ stationfctr, ncol = 4)
# p11
# 
# ## boxplot hourly  swc_wp_05_h distributions Mazia (all stations)
# p12<-ggplot(data = IT25_Data)+
#     geom_boxplot(aes(x = hourfctr, y= swp_wp_05_h))+
#     ylab("Soil Water Potential 5cm - kPa")+
#     theme_minimal()+
#     xlab("Hour of the day")+
#     ylim(0, -750)+
#     facet_wrap(. ~ stationfctr, ncol = 4)
# p12
# 
# ## boxplot hourly  swc_wp_20_h distributions Mazia (all stations)
# p13<-ggplot(data = IT25_Data)+
#     geom_boxplot(aes(x = hourfctr, y= swp_wp_20_h))+
#     ylab("Soil Water Potential 20 cm - kPa")+
#     theme_minimal()+
#     xlab("Hour of the day")+
#     ylim(0, -750)+
#     facet_wrap(. ~ stationfctr, ncol = 4)
# p13
# 
# 
# IT25_Data$fpar <- (IT25_Data$par_soil_h / IT25_Data$par_h)*100
# 
# ## boxplot hourly fpar distributions Mazia (all stations)
# p14<-ggplot(data = IT25_Data)+
#     geom_boxplot(aes(x = hourfctr, y= fpar))+
#     ylab(" Absorbed PAR %")+
#     xlab("Hour of the day")+
#     theme_minimal()+
#     ylim(0, 60)+
#     facet_wrap(. ~ stationfctr, ncol = 4)
# p14


#ggarrange(p1, p2,p3,p4,p5, p6,p7,p8,p9,p10,p11,p12,p13,p14, ncol = 4, nrow = 4)
# ----



# ----
## Exploratory plots

p1<-ggplot(data = IT25_Data)+
    geom_line(aes(x =time, y= air_rh_h, colour = station))+
    xlab("Date")+ylab("Air Temp")+
    theme_minimal()+
    ylim(-20, 30)
p1
ggplotly(p1)

p2<-ggplot(data = IT25_Data)+
    geom_boxplot(aes(x =reorder(station, elevation), y= air_t_h,colour = elevation))+
    ylab("Air Temp")+
    theme_minimal()+
    ylim(-20, 30)
p2
ggplotly(p2)



p3<-ggplot(data = IT25_Data,aes(x=air_t_h, color = station))+
    geom_density()+
    xlab("Air Temp")+
    theme_minimal()+
    xlim(-20, 35)
p3
ggplotly(p3)


ggplot(data = IT25_Data)+
    geom_line(aes(x = time, y= wind_dir_h))+
    ylab("Wind Dir %")+
    theme_minimal()+
    ylim(0, 360)
## boxplot monthly wind_speed_h distributions Mazia (all stations)
ggplot(data = IT25_Data)+
    geom_line(aes(x = time, y= wind_speed_h))+
    ylab("Wind Speed %")+
    theme_minimal()+
    ylim(0, 20)

## boxplot monthly wind_speed_max_h distributions Mazia (all stations)
ggplot(data = IT25_Data)+
    geom_line(aes(x = time, y= wind_speed_max_h))+
    ylab("Wind Speed Max %")+
    theme_minimal()+
    ylim(0, 20)



ggarrange(p1, p2, ncol = 1, nrow = 2)

ggarrange(ggplotly(p1), ggplotly(p2), ggplotly(p3), ncol = 1, nrow = 3)
summary(result)    

# ---
## MONTHY DISTRIBUTION 
if (!require("viridis")) install.packages("viridis")
if (!require("ggjoy")) install.packages("ggjoy")
if (!require("hrbrthemes")) install.packages("hrbrthemes")





# ----
## Plot Monthy distribution AirT
#scales
mins <- min(IT25_Data$air_t_h)
maxs <- max(IT25_Data$air_t_h)

##AirT
ggplot(IT25_Data, aes(x = air_t_h, y = monthfctr, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 1.) +
    scale_x_continuous(expand = c(0.01, 0)) +
    scale_y_discrete(expand = c(0.01, 0)) +
    scale_fill_viridis(name = "Temp. [?C]", option = "E") +
    labs(title = 'Temperatures',
         subtitle = 'Mean temperatures ?C by month for 2019-2020', 
         x = "Mean Temperature") +
    xlim(-20, 30)+
    theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())+
    facet_wrap(. ~ stationfctr, ncol = 4)

##RH
ggplot(IT25_Data, aes(x = air_rh_h, y = monthfctr, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01, gradient_lwd = 1.) +
    scale_x_continuous(expand = c(0.01, 0)) +
    scale_y_discrete(expand = c(0.01, 0)) +
    scale_fill_viridis(name = "RH. [%]", option = "E") +
    labs(title = 'Relative Humidity',
         subtitle = 'Mean RH % by month for 2019-2020', 
         x = "Mean RH") +
    xlim(0, 110)+
    theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())

## black and white
ggplot(IT25_Data,aes(x = air_t_h,y=station,height=..density..))+
    geom_joy(scale=2) +
    scale_x_continuous(limits = c(-15,30))+
    theme_ipsum(grid=F)+
    theme(axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          strip.text.y = element_text(angle = 180, hjust = 1))+
    labs(title='Temperatures',
         subtitle='Mean temperatures by month for 2019-2020', x = "Mean Tempterature [?C]")    


ggplot(IT25_Data, aes(x = air_t_h, y = monthfctr, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01, gradient_lwd = 1.) +
    scale_x_continuous(expand = c(0.01, 0)) +
    scale_y_discrete(expand = c(0.01, 0)) +
    scale_fill_viridis(name = "Temp. [?C]", option = "D") +
    labs(title = 'Temperatures',
         subtitle = 'Mean temperatures ?C by month for 2019-2020', 
         x = "Mean Temperature") +
    xlim(-20, 30)+
    theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())


ggplot(IT25_Data, aes(x = air_rh_h, y = monthfctr, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01, gradient_lwd = 1.) +
    scale_x_continuous(expand = c(0.01, 0)) +
    scale_y_discrete(expand = c(0.01, 0)) +
    scale_fill_viridis(name = "RH. [%]", option = "E") +
    labs(title = 'Relative Humidity',
         subtitle = 'Mean RH % by month for 2019-2020', 
         x = "Mean RH") +
    xlim(0, 110)+
    theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())




# ----

## SOME GROUPING AND SUMMARY
# Mean Temp



## LAPS RATE CALCULATION
Tb1<-IT25_Data %>% dplyr::filter(station== "b1")
Ts3<-IT25_Data %>% dplyr::filter(station== "s3")

Tb1<-IT25_Data %>% select(time,station, air_t_h,elevation,monthfctr,month,hourfctr)%>%
    dplyr::filter(station== "b1")
Ts3<-IT25_Data %>% select(time,station, air_t_h,elevation,monthfctr,month,hourfctr)%>%
    dplyr::filter(station== "s3")

Ts3b1<-inner_join(Tb1, Ts3, by = "time")
laps<-Ts3b1%>% dplyr::mutate(laps = (Ts3b1$air_t_h.x-Ts3b1$air_t_h.y)/(Ts3b1$elevation.y/1000-Ts3b1$elevation.x/1000))


## HOURLY LAPS RATE
g1<-ggplot(data = laps)+
    geom_boxplot(aes(x =hourfctr.x, y= laps))+
    ylab("Hourly Air Temp lapsrate from 1000-2700 m a.s.l. ")+
    theme_minimal()+
    ylim(-2.5, 12.5)+
    facet_wrap(. ~ reorder(monthfctr.x,month.x), ncol = 4)+
    xlab("Hours of the day")
g1    
## MONTHLY LAPS RATE
g2<-ggplot(data = laps)+
    geom_boxplot(aes(x =reorder(monthfctr.x,month.x), y= laps))+
    ylab("Monthly Air Temp lapsrate from 1000-2700 m a.s.l. ")+
    theme_minimal()+
    ylim(-2.5, 12.5)+
    xlab("Months")
g2

g3<-ggplot(data = laps)+
    geom_boxplot(aes(x =reorder(monthfctr.x,month.x), y= laps))+
    ylab("Monthly Air Temp lapsrate from 1000-2700 m a.s.l. ")+
    theme_minimal()+
    ylim(-2.5, 12.5)+
    xlab("Months")+
    facet_wrap(. ~ station.x, ncol = 4)

g3


## PLOT TIME SERIES WITH CONFIDENCE INTERVAL
ggplot(data = IT25_Data,aes(x=month,y=air_t_h))+
    geom_smooth(stat = 'summary', 
                fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                  y = mean(y), ymax = quantile(y, .9)))+
    ylim(-15, 25)+
    facet_wrap(. ~ stationfctr, ncol = 4)

#----
## PRECIPITATION SUMMARY AND PREPROCESSING

precp_sum_month<-IT25_Data %>% group_by(station, monthfctr,month) %>% 
    dplyr::summarise(precip_sum = sum(precip_h,na.rm = T))%>%
    filter(station %in% c("b3","m4 snow", "s4","b1","b2","p2")) # Filter stations with Pluviometer   


station_Pcum<-precp_sum_month%>%group_by(station)%>%
    mutate(precip_cumsum = cumsum(precip_sum))


prep_sum_tot <-precp_sum_month %>%
    filter(precip_sum > 0) %>% group_by(monthfctr) %>% 
    summarise(precip_tot_avg = mean(precip_sum, na.rm = T))

## PRECIPITATION BARS PER STATIONS
p1<-ggplot(station_Pcum)+
    geom_col(aes(x=monthfctr,y=precip_sum), limit(0,50))+
    geom_line(mapping = aes(x=monthfctr,y=precip_cumsum), col="red", lwd=4)+
    scale_y_continuous("Flow (m3/s)", sec.axis = sec_axis(~., name = "Rainfall (mm)"))+
    ylab(" Precipitation mm")+
    xlab(" Months")+
    theme_minimal()+
    xlab("Months")+
    facet_wrap(. ~ station, ncol = 1)
#ylim(0, 15)
p1

## PRECIPITATION BARS & CUMULATIVE PER STATIONS
p1<-ggplot(station_Pcum)+
    geom_col(aes(x=month,y=precip_sum),col="black")+
    geom_line(aes(x=month,y=precip_cumsum), col="blue", lwd=1)+
    scale_y_continuous("Precipitation (mm)", sec.axis = sec_axis(~., name = "Cumulative Precipitation (mm)"))+
    scale_x_continuous("Month", breaks = 1:12)+
    ylab(" Precipitation mm")+
    xlab(" Months")+
    theme_minimal()+
    facet_wrap(. ~ station, ncol = 1)

p1


p1<-ggplot(station_Pcum)+
    geom_col(aes(x=month,y=precip_sum),col="black")+
    geom_line(aes(x=month,y=precip_cumsum), col="blue", lwd=1)+
    scale_y_continuous("Precipitation (mm)", sec.axis = sec_axis(~., name = "Cumulative Precipitation (mm)"))+
    scale_x_continuous("Month", breaks = 1:12)+
    ylab(" Precipitation mm")+
    xlab(" Months")+
    theme_minimal()+
    facet_wrap(. ~ station, ncol = 1)

p1          


## PRECIPITATION SMOOTH BANDS WITH CONFIDENCE INTERVAL

p1<-ggplot(data=station_Pcum,aes(x=month,y=precip_cumsum))+
    geom_smooth(stat = 'summary', 
                fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                  y = mean(y), ymax = quantile(y, .9)))+
    ylab(" Precipitation mm")+
    xlab(" Months")+
    theme_minimal()+
    xlab("Months")


p1       




p1<-ggplot(precp_sum_month)+
    geom_col(aes(x=monthfctr,y=precip_sum))+
    ylab(" Precipitation mm")+
    xlab(" Months")+
    theme_minimal()+
    xlab("Months")+
    facet_wrap(. ~ stationfctr ncol = 4)
#ylim(0, 15)
p1          



