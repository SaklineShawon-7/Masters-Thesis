
library(pacman)
p_load(tidyverse,lubridate,suncalc,timetk, data.table,StreamMetabolism,ragg,oce)
library(tidyverse)
library(lubridate)
library(suncalc)
library(timetk)
library(ragg)
library(oce)
library(StreamMetabolism)
library(data.table)


# Sauerbach data set from 2020 to 2023

Sauerbach_TS1 <- read_csv('Data/2020 to 2023.10.14.csv')

Sauerbach_TS2 <- data.frame(Sauerbach_TS1, Wdep = rep(5.0, nrow(Sauerbach_TS1)))
Sauerbach_TS3<- Sauerbach_TS2 %>% select(2,4,5,7,10,11) %>% rename("Date"="Time")


# select study period from April 2021 to October 2023
Sauerbach_TS <- Sauerbach_TS3 %>% filter(between(
  Date,
  ymd_hms('2021-04-25 00:02:00'),
  ymd_hms('2023-08-30 23:52:00')
)) %>% mutate(Watemp = as.numeric(wt),
              DO = as.numeric(O2_konz),
              SatO2 = as.numeric(O2_proz),
getSunlightTimes(
                  date = as.Date(Date) ,
                  lat = 51.941437,
                  lon = 11.158431 ,
                  tz = 'UTC',
                  keep = c('sunrise', 'sunset')
                )) 


# first check data
Sauerbach_TS %>% ggplot()+
  geom_line(aes(x = Date, y= DO, color ='black'))+
  geom_line(aes(x = Date, y= SatO2, color ='red'))+
  scale_y_continuous(sec.axis = sec_axis(~.*10))+
  theme_bw()


# remove outlier and spikes
Sauerbach_TS <- Sauerbach_TS %>% mutate( 
  DO =  oce::despike(
    x = Sauerbach_TS$DO,
    reference = 'median',
    n = 3,
    k = 33,
    replace = "NA"
  ) ,
  DO = ifelse(DO >15,NA,DO), # remove O2 above 15 mg/L based on warmer water temperature has lower DO
  SatO2 =  oce::despike(
    x = Sauerbach_TS$SatO2,
    reference = 'median',
    n = 3,
    k = 13,
    replace = "NA"
  ) ,
  DeltaDO = lag(DO)-DO ##will further use to calculate NEP
####If the time interval between consecutive observations is constant and equal to 10, 
###then DO - lag(DO) divided by 10 will yield the same result as (DO - DO) / 10.
)
View(Sauerbach_TS)
summary(Sauerbach_TS$Wdep)

# visualization in ggplot
Sauerbach_TS  %>% ggplot()+
  # geom_point(aes(x = Date, y= DO), color ='black')+
  geom_line(aes(x = Date, y= DO, color ='black'))+
  # geom_line(aes(x = Date, y= SatO2/10, color ='red'))+
  # scale_y_continuous(sec.axis = sec_axis(~.*10))+
  theme_bw()

#####Stream metabolism calculation###

# calculate re-aeration rate
K <- Sauerbach_TS %>% filter(between(
  Date,
  ymd_hms('2021-04-25 00:00:00'),
  ymd_hms('2023-08-14 00:00:00')
)) %>%  mutate(Q= Q/1000,
  Cs = ((DO*100) /SatO2), # Corrected saturation O2 (DOsat) = (O2 concentration * 100%) / saturation O2
  vel = Q / (0.92 * 0.05), # Mean width = 0.92 m, slope = 0.016 m/m, mean water level = 5 cm, 5/100 = 0.05 m
  K1 = 15300,
  K2 = (K1 * 0.016 * vel) / 1440,## Convert to min
kre = K2 * 1.024 ^ (Watemp - 20)) # correction by temperature, min-1


###Night time NEP
### only the period of night time taken to calculate the Night time NEP,
###which is equivalent to night time ER

Night<- K %>% filter(between(
  Date,
  ymd_hms('2021-04-25 00:00:00'),
  ymd_hms('2023-08-14 00:00:00')
)) %>%
  mutate(getSunlightTimes(
    date = as.Date(Date) ,
    lat = 51.950573,
    lon = 11.163875 ,
    tz = 'UTC',
    keep = c('sunrise', 'sunset')
  ) ) %>%
  filter(!  between(Date,sunrise,sunset)  )  %>% # filter nighttime
  mutate(ER = (DeltaDO - kre * (Cs-DO)) / 10) # dt = 10, # Time interval in minutes dDO/dt = delta_DO / dt

###show the result only night NEP that is written as night time ER
### Because Night time ER = Night time NEP

#### mean of all night time
Rnight = mean(Night$ER, na.rm = T) 
##Required for the calculation of Daytime ER
Tempnight = mean(Night$wt, na.rm = T)

### daylight time GPP and respiration rate (ER)

###GPP for each daytime interval was the difference between the net metabolism flux and interpolated ER.
Day <-
  K %>% filter(between(
    Date,
    ymd_hms('2021-04-25 00:00:00'),
    ymd_hms('2023-08-14 00:00:00')
  )) %>%  mutate(
    getSunlightTimes(
      date = as.Date(Date) ,
      lat = 51.941437,
      lon = 11.158431 ,
      tz = 'UTC',
      keep = c('sunrise', 'sunset')
    )
  ) %>%
  filter(sunrise <= Date, Date <= sunset)  %>% # filter daylight time
  
  ###Daytime ER calculated from night time mean ER/NEP
  mutate(ER = Rnight*1.072^(Watemp-Tempnight), # negative value, mg L-1 min-1
         GPP = (DeltaDO - kre * (Cs-DO)) / 10 - ER,  #mg L-1 min-1 ### dt = 10, # Time interval in minutes dDO/dt = delta_DO / dt
         GPP_day = GPP*Wdep*10)  # mg L-1 min-1* m = mg m-2 min-1

##volumetric rates were converted to areal units by the mean water depth of the stream reach

hist(Day$GPP)
hist(Day$GPP_day)

# daily GPP during daylight time
GPP_daytime <- Day %>% 
  summarise_by_time(Date, "day", value = sum(GPP_day*10, na.rm = T))  # mg m-2 d-1

GPP_daytime<-GPP_daytime[-c(56:62,66:69,72,73,74,76,77,81,84,86,120,121,
                          124,125,127,128,156,180,194,234,333:340,
                          369:393,397:408,413:417,422,433,532,535,536,573,790),]


# daily ER
ER_day <- Night %>% dplyr::select(date,ER,Wdep) %>% bind_rows(Day %>% dplyr::select(date,ER,Wdep)) %>% 
  summarise_by_time(date, "day", value = sum(ER*Wdep*10*10, na.rm = T)) # # mg L-1 min-1=mg m-2 d-1
ER_day

ER_day<-ER_day[-c(213,234,238,241,247,248,258,262,263,269,272,273,274,
                  277,279,285,298,316,333:339,369:392,397:408,413:416,
                  587,589,604,620,638,639,640,657,658,645,290,267,265,232,246,243,237),]


# daily GPP and ER plot
Nien_GPP_ER <- GPP_daytime %>% full_join(ER_day, by = c('Date'='date')) %>%
  rename('GPP'='value.x','ER' = 'value.y' ) %>% 
  mutate(Date = as.Date(Date)) %>% 
  ggplot() +
  geom_point(aes(Date,GPP/1000,color='mediumblue'), shape = 1, size = 1)+   ####mg m-2 d-1/1000=g m-2 d-1
  geom_point(aes(Date,ER/1000,color='red'),shape = 1, size = 1)+
  geom_line(aes(Date,GPP/1000,color='mediumblue'),size = 0.4)+
  geom_line(aes(Date,ER/1000,color='red'),size = 0.4)+
  scale_color_manual(name ='',values = c('mediumblue','red'),labels =c('GPP','ER'))+
  ylab(expression('Rate (g O'[2]*'m'^-2*'d'^-1*')'))+
  scale_x_date( date_breaks = '3 months', date_labels = '%b-%y',expand = c(0,0))+
  theme_bw()+
  ylim(c(-0.25,0.20))+
  theme(legend.position = c(0.5,0.10))+
  xlab("Stream Metabolism (GPP and ER),2021-2023") +
  theme_bw()+theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 14),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
Nien_GPP_ER



ggsave("GPP&ER.jpg", Nien_GPP_ER, width = 10, height = 6, dpi = 300)



min_value <- min(ER_day$value, na.rm = TRUE)
min_value/1000

max_value <- max(ER_day$value, na.rm = TRUE)
max_value/1000


gg<-GPP_daytime$value

Ua= (gg/(4.57*8.7))/1000


GPP_daytime

uaaa<- GPP_daytime %>% mutate(UA=((value*0.5)/(2.28*8.7)))


