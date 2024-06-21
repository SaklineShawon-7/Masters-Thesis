

library(pacman)
library(tidyverse)
library(lubridate)
library(data.table)
library(oce)
library(suncalc)
library(ggpubr)
library(timetk)






# assimilation uptake rate ----------

file_2022<-read_csv("./Data/controlled_2022_u.csv")

file_2022


SB_data_clean<-file_2022 %>% select(3,4,7,11) %>% mutate(Q= Q*3600) %>% timetk::summarise_by_time(.date_var = Time, .by = 'hour',
                                                                     No3_hour= mean(No3, na.rm=T),
                                                                     Wlevl_hour=mean(wl, na.rm=T),
                                                                     Q_hour=mean(Q, na.rm=T))  # according to Jan, L/s = 3600 L/hour
#View(SB_data_clean)

####*****convert the Q from L/s to L/hour (60*60)####


# find first peak and second peak datetime to filter the valley between two peak values
twopk_NO3 <- SB_data_clean %>%
  mutate(yy = year(Time), mon = month(Time), day = day(Time), hh = hour(Time)) %>%
  group_by(yy, mon, day, hh) %>%
  summarise(max_No3_hh = mean(No3_hour, na.rm = TRUE)) %>%
  group_by(yy, mon, day) %>%
  filter(max_No3_hh == max(max_No3_hh, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    begin = ymd_h(paste(yy, mon, day, hh, sep = '-')),
    end = dplyr::lead(begin)
  )


#dplyr::lead(begin), which shifts the 'begin' column by one position. It represents the end of the time interval. 
#However, note that for the last row, there is no subsequent 'begin' value to use as 'end', so it appears as missing.
#Now, regarding the varying end times, it's because each 'begin' time corresponds to a specific hour, and 'end' is the next 'begin' time.
#Since hours vary, the duration of each interval is different.

twopk_NO3
#View(twopk_NO3)

# mutate maximum No3 during two peaks
NO3_uptk <- SB_data_clean %>% dplyr::select(Time, No3_hour, Q_hour) %>%
  rowwise() %>%
  filter(any(Time >= twopk_NO3$begin & Time <= twopk_NO3$end)) %>% 
  ungroup() %>% 
  mutate(mon = month(Time),
         Day = day(Time)) %>%
  group_by(mon, Day) %>%
  mutate(Max_no3 = max(No3_hour, na.rm = T))  # find maximum values of each day

#View(NO3_uptk)

# https://stackoverflow.com/questions/71134733/r-summarize-values-between-specific-date-range            
# time period interval
NO3_intv <- setDT(twopk_NO3)
#View(NO3_intv)

# sum rows between two peaks interval --------
## https://stackoverflow.com/questions/33049441/r-if-date-falls-within-range-then-sum
# according to Rode et. al., EST, 2016
# Q should be L/h, A = width*length
NO3_uptk_daily <-
  setDT(NO3_uptk)[NO3_intv,  on = .(Time >= begin, Time <= end), .(begin, end, No3_hour, Max_no3, Q_hour)][, Ua := sum((Max_no3 - No3_hour) *
                                                                                                                         Q_hour, na.rm = T) / (0.92 * 1090) , by = .(begin, end)]
View(NO3_uptk_daily)


NO3_uptk_daily




rows_to_del<- c(51:53,450:455,704:752,1176:1180,1350:1353,1676:1682,1951:1953,2101:2107,2226:2232,2476:2483,2526:2527,2776:2777,2851:2855,2976:2983,3126:3132,4551:4556,
                     4901:4902,4978:5033,5126:5133,5301:5305,5651:5659,6135:6174,6201:6209,6250:6259,6326:6327,
                     6537:6577,6686:6750,6825:6832,6913:6950,7069:7100,7192:7224,7375:7380,7410:7450,7478:7573,7590:7625,
                     7636:7675,7706:7742
)

NO3_uptkupdate <- NO3_uptk_daily[-rows_to_del, ]

write.csv(NO3_uptkupdate,"nitrate_uptake_2022.csv")


U_No32 <- NO3_uptkupdate %>% as.data.frame() %>%
  distinct(begin,Ua) %>% 
  #filter(!day(begin) %in% c(4,5,15))  %>% 
  ggplot()+
  geom_point(aes(x = begin , y= Ua ), color ='black')+
  geom_line(aes(x = begin , y= Ua ), color ='blue')+
  scale_x_datetime(date_breaks = '1 months', date_labels = '%d-%b')+
  labs(x = 'Nitrate Uptake,2022', y= expression('U'[a]*'NO'[3]*'(mg N m'^-2* 'd'^-1*')') )+
  theme_bw()+
  theme(
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 14),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
U_No32


ggsave("Nitrate_uptake2022.jpg", U_No32, width = 10, height = 6, dpi = 300)
