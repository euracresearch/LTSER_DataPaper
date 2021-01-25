## LOAD PLOT IT25_DATA

rm(list = ls())

## Some Pre-processing

## In the case you influxdb_query are to large save yearly queries in separates csv.
#write.csv(IT25_Data,"IT25_Data_2019.csv", row.names = T)
## CONCATENATE CSV OF DIFFERENT YEARS
df1<-read.csv("IT25_Data_2017.csv")
df2<-read.csv("IT25_Data_2018.csv")
IT25_Data<-dplyr::bind_rows(df1,df2)

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

# modified by giulio
AirT_month_2017<-IT25_Data%>%select(time,month,year,hour,air_t_h)%>% 
  group_by(month,year) %>% summarise(AirT_Month = mean(air_t_h, na.rm = T))

p1 <- ggplot(data = IT25_Data,aes(x=month,y=air_t_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  ylim(-15, 25)
p1

p2 <- ggplot(data = IT25_Data,aes(x=month,y=air_rh_h))+
  geom_smooth(stat = 'summary', linetype=0,
              fun.data = function(y) data.frame(ymin = quantile(y, .1),
                                                y = mean(y), ymax = quantile(y, .9)))+
  geom_smooth(aes(color = as.character(year)), stat = 'summary',
              fun.data = function(y) data.frame(y = mean(y)))+
  ylim(0, 100)
p2
