

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

Sauerbach <- read_csv('Data/2020 to 2023.10.14.csv')

# Assuming your time series dataset is called ts_data
Sauerbach_TS2 <- data.frame(Sauerbach, Wdep = rep(5.0, nrow(Sauerbach)))
Sauerbach_TS3<- Sauerbach_TS2 %>% select(2,4,5,7,10,11) %>% rename("Date"="Time")

#colnames(Nien_TS) <- c("Date","Airp","Wdep","PAR","Watemp","DO","SatO2")
#head(Nien_TS3)
# select 2022
Sauerbach_TS <- Sauerbach_TS3 %>% filter(between(
  Date,
  ymd_hms('2022-12-01 00:00:00'),
  ymd_hms('2023-02-28 00:00:00')
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
  )
)
View(Sauerbach_TS)
summary(Sauerbach_TS$SatO2)

# 
Sauerbach_TS  %>% ggplot()+
  # geom_point(aes(x = Date, y= DO), color ='black')+
  geom_line(aes(x = Date, y= DO, color ='black'))+
  # geom_line(aes(x = Date, y= SatO2/10, color ='red'))+
  # scale_y_continuous(sec.axis = sec_axis(~.*10))+
  theme_bw()


Sauerbach_TS_DO <- Sauerbach_TS %>% 
  ggplot(aes(x = Date, y = DO)) +
  geom_line() +
  theme_bw() +
  labs(x = "Sauerbach_2022-12-01 to 2023-02-28")+ylim(5, 15)+theme(text = element_text(size = 18))

print(Sauerbach_TS_DO)


Sauerbach_TS_sato <- Sauerbach_TS %>% 
  ggplot(aes(x = Date, y = SatO2)) +
  geom_line() +
  theme_bw() +
  labs(x = "Sauerbach_2022-12-01 to 2023-02-28")+ ylim(80, 110)+theme(text = element_text(size = 18))

print(Sauerbach_TS_sato)

###################################################################################

######Hausniendorf####


Hausniendorf <- read_csv("O_2 data_Hausniendorf/Wasserguetemessstation-Selke-_HAU_Pegel--_Hausneindorf__-HAU_Pegel--_Hausneindorf_-507.csv")
Hausniendorf
# Assuming your time series dataset is called ts_data

Hausniendorf_TS3<- Hausniendorf %>% rename("Date"="Timestamp", "O2_proz"="SurfaceWaterSaturationOxygen0mSingleValue (O2%lok) [%]", "O2_konz"="SurfaceWaterConcentrationO20mSingleValue (O2konz) [mg/l]" ) %>% 
  mutate(
    O2_proz = as.numeric(O2_proz),
    O2_konz = as.numeric(O2_konz)
  ) %>%
  na.omit()  # Remove rows with missing values in any column

Hausniendorf_TS3

Hausniendorf_TS3 <- Hausniendorf_TS3 %>% filter(between(
  Date,
  ymd_hms('2022-12-01 00:00:00'),
  ymd_hms('2023-02-28 00:00:00')
)) %>% mutate( DO = as.numeric(O2_konz),
              SatO2 = as.numeric(O2_proz),
              getSunlightTimes(
                date = as.Date(Date) ,
                lat = 51.941437,
                lon = 11.158431 ,
                tz = 'UTC',
                keep = c('sunrise', 'sunset')
              )) 


# first check data
Hausniendorf_TS3 %>% ggplot()+
  geom_line(aes(x = Date, y= DO, color ='black'))+
  geom_line(aes(x = Date, y= SatO2, color ='red'))+
  scale_y_continuous(sec.axis = sec_axis(~.*10))+
  theme_bw()


# remove outlier and spikes
Hausniendorf_t <- Hausniendorf_TS3 %>% mutate( 
  DO =  oce::despike(
    x = Hausniendorf_TS3$DO,
    reference = 'median',
    n = 3,
    k = 33,
    replace = "NA"
  ) , # remove O2 above 15 mg/L based on warmer water temperature has lower DO
  SatO2 =  oce::despike(
    x = Hausniendorf_TS3$SatO2,
    reference = 'median',
    n = 3,
    k = 13,
    replace = "NA"
  )
)
View(Hausniendorf_t)
summary(Hausniendorf_t$SatO2)

# 
Hausniendorf_t  %>% ggplot()+
  # geom_point(aes(x = Date, y= DO), color ='black')+
  geom_line(aes(x = Date, y= DO, color ='black'))+
  # geom_line(aes(x = Date, y= SatO2/10, color ='red'))+
  # scale_y_continuous(sec.axis = sec_axis(~.*10))+
  theme_bw()



Hausniendorf_TS_DO <- Hausniendorf_t %>% 
  ggplot(aes(x = Date, y = DO)) +
  geom_line() +
  theme_bw() +
  labs(x = "Hausniendorf__2022-12-01 to 2023-02-28")+ylim(4, 17)+theme(text = element_text(size = 18))

print(Hausniendorf_TS_DO)


Hausniendorf_TS_sato <- Hausniendorf_t %>% 
  ggplot(aes(x = Date, y = SatO2)) +
  geom_line() +
  theme_bw() +
  labs(x = "Hausniendorf__2022-12-01 to 2023-02-28")+ ylim(60, 160)+theme(text = element_text(size = 18))

print(Hausniendorf_TS_sato)


###################################################################################

######Meisdorf####


Meisdorf <- read_csv("O_2 data_Miesdorf/Wasserguetemessstation-Selke-_MEI_Pegel-_Meisdorf__-MEI_Pegel-_Meisdorf_-506.csv")
Meisdorf
# Assuming your time series dataset is called ts_data

Meisdorf_TS3<- Meisdorf %>% rename("Date"="Timestamp", "O2_proz"="SurfaceWaterSaturationOxygen0mSingleValue (O2%lok) [%]", "O2_konz"="SurfaceWaterConcentrationO20mSingleValue (O2konz) [mg/l]" ) %>% 
  mutate(
    O2_proz = as.numeric(O2_proz),
    O2_konz = as.numeric(O2_konz)
  ) %>%
  na.omit()  # Remove rows with missing values in any column

Meisdorf_TS3

Meisdorf_TS3 <- Meisdorf_TS3 %>% filter(between(
  Date,
  ymd_hms('2022-12-01 00:00:00'),
  ymd_hms('2023-02-28 00:00:00')
)) %>% mutate( DO = as.numeric(O2_konz),
               SatO2 = as.numeric(O2_proz),
               getSunlightTimes(
                 date = as.Date(Date) ,
                 lat = 51.941437,
                 lon = 11.158431 ,
                 tz = 'UTC',
                 keep = c('sunrise', 'sunset')
               )) 


# first check data
Meisdorf_TS3 %>% ggplot()+
  geom_line(aes(x = Date, y= DO, color ='black'))+
  geom_line(aes(x = Date, y= SatO2, color ='red'))+
  scale_y_continuous(sec.axis = sec_axis(~.*10))+
  theme_bw()


# remove outlier and spikes
Meisdorf_t <- Meisdorf_TS3 %>% mutate( 
  DO =  oce::despike(
    x = Meisdorf_TS3$DO,
    reference = 'median',
    n = 3,
    k = 33,
    replace = "NA"
  ) , # remove O2 above 15 mg/L based on warmer water temperature has lower DO
  SatO2 =  oce::despike(
    x = Meisdorf_TS3$SatO2,
    reference = 'median',
    n = 3,
    k = 13,
    replace = "NA"
  )
)
View(Meisdorf_t)
summary(Meisdorf_t$SatO2)

# 
Meisdorf_t  %>% ggplot()+
  # geom_point(aes(x = Date, y= DO), color ='black')+
  geom_line(aes(x = Date, y= DO, color ='black'))+
  # geom_line(aes(x = Date, y= SatO2/10, color ='red'))+
  # scale_y_continuous(sec.axis = sec_axis(~.*10))+
  theme_bw()

Meisdorf_TS_DO <- Meisdorf_t %>% 
  ggplot(aes(x = Date, y = DO)) +
  geom_line() +
  theme_bw() +
  labs(x = "Meisdorf_2022-12-01 to 2023-02-28")+ylim(6, 17)+theme(text = element_text(size = 18))

print(Meisdorf_TS_DO)


Meisdorf_TS_sato <- Meisdorf_t %>% 
  ggplot(aes(x = Date, y = SatO2)) +
  geom_line() +
  theme_bw() +
  labs(x = "Meisdorf_2022-12-01 to 2023-02-28")+ ylim(80, 110)+theme(text = element_text(size = 18))

print(Meisdorf_TS_sato)





library(ggplot2)
library(patchwork)

# Assume you have three ggplot objects named plot1, plot2, and plot3

Combine_DO<- Sauerbach_TS_DO + Hausniendorf_TS_DO + Meisdorf_TS_DO

# Arrange the plots as desired
combined_plot_DO <- Combine_DO + plot_layout(nrow = 3)

# Print the combined plot
print(combined_plot_DO)

ggsave(filename = 'C:/Users/shawon/Desktop/Thesis/plot_DOw.png',
       plot = combined_plot_DO,
       height = 12,
       width = 18,
       dpi = 300
)



# Assume you have three ggplot objects named plot1, plot2, and plot3

Combine_sato<- Sauerbach_TS_sato + Hausniendorf_TS_sato + Meisdorf_TS_sato

# Arrange the plots as desired
combined_plot_sato <- Combine_sato + plot_layout(nrow = 3)

# Print the combined plot
print(combined_plot_sato)


ggsave(filename = 'C:/Users/shawon/Desktop/Thesis/plot_satow.png',
       plot = combined_plot_sato,
       height = 12,
       width = 18,
       dpi = 300
)


