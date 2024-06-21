
library(readxl)
library(readr)
library(ggplot2)
library(tidyverse)

a<-read_csv("Nitrate Uptake/2021/NO3_uptk_daily_2021.csv")

b<-read_csv("Nitrate Uptake/2022/nitrate_uptake_2022.csv")

c<-read_csv("Nitrate Uptake/2023/nitrate_uptake_2023.csv")


my_data_no3<-bind_rows(a,b,c)

View(my_data_no3)


U_No331 <- my_data_no3 %>%
  filter(Ua > 1) %>% 
  as.data.frame() %>%
  distinct(begin,Ua) %>%
  filter(between(
    begin,
    ymd_hms('2021-03-20 00:02:00'),
    # must be full day time
    ymd_hms('2023-09-01 23:52:00'))) 

write.csv(U_No331,"nitrate_uptake_2021_2023.csv")


U_N<-U_No331 %>% as.data.frame() %>%
  distinct(begin,Ua) %>% 
  #filter(!day(begin) %in% c(4,5,15))  %>% 
  ggplot()+
  geom_point(aes(x = begin , y= Ua ), color ='black')+
  geom_line(aes(x = begin , y= Ua ), color ='blue')+
  scale_x_datetime(date_breaks = '3 month', date_labels = '%b-%y')+
  labs(x = 'Nitrate Uptake,(2021-2023)', y= expression('U'[a]*'NO'[3]*'(mg N m'^-2* 'd'^-1*')') )+
  theme_bw()+
  theme(
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 14),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

U_N

ggsave("Nitrate_uptakeall.jpg", U_N, width = 10, height = 6, dpi = 300)



############################################


####Winter month Uptake####


W_uptake<- U_No331 %>% filter(between(
  begin,
  ymd_hms('2022-11-01 00:02:00'),
  ymd_hms('2023-01-31 23:52:00'))) %>% 
  timetk::summarise_by_time(.date_var = begin, .by = 'day', Ua = mean(Ua, na.rm = TRUE))



W_uptake

dW_uptake_cleaned <- W_uptake[-18, ]


W_uptake$begin <- as.POSIXct(W_uptake$begin)



####Plot####
Wuptake_2023 <- ggplot(W_uptake) +
  geom_col(aes(begin, Ua), fill = 'blue4', size = 0.6) +
  ylab(expression('U'[a]*'NO'[3]*'(mg N m'^-2* 'd'^-1*')')) +
  scale_x_datetime(date_breaks = '15 days', date_labels = '%d-%b-%Y') +
  scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
  labs(x = 'Date') +
  theme_bw() +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 14),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )


Wuptake_2023


ggsave("NitrateWinter.jpg", Wuptake_2023, width = 10, height = 6, dpi = 300)


###########
##uptake statistics in winter

U_S<- my_data_no3 %>%
  filter(between(
    begin,
    ymd_hms('2023-01-01 00:02:00'),
    ymd_hms('2023-01-31 23:52:00')))
U_S


Mean<-U_S %>% mutate(m=mean(Ua))


#####################################################################

###min and max

Minimum <- U_No331 %>% filter(between(
  begin,
  ymd_hms('2022-11-01 00:02:00'),
  ymd_hms('2023-01-28 23:52:00'))) %>% 
  summarise(min_value = min(Ua)) 


Minimum



Maximum <- U_No331 %>% filter(between(
  begin,
  ymd_hms('2022-01-01 00:02:00'),
  ymd_hms('2022-12-31 23:52:00'))) %>% 
  summarise(max_value = max(Ua)) 


Maximum



mean <- U_No331 %>% filter(between(
  begin,
  ymd_hms('2021-11-01 00:02:00'),
  ymd_hms('2022-01-31 23:52:00'))) %>% 
  summarise(max_value = mean(Ua)) 


mean



###################
##uptake velocity

View(U_No331)

U_No331$Date<-as.Date(U_No331$begin)

V<- U_No331 %>% filter(between(
  begin,
  ymd_hms('2023-01-01 00:02:00'),
  ymd_hms('2023-09-01 23:52:00')))

tail(V)

data_2021<-read_csv("./Data/controlled_2023.csv")

data_2021

dataN_2021<-data_2021 %>% select(Time,No3) %>% 
  timetk::summarise_by_time(.date_var = Time, .by = 'day', NO3_d = mean(No3, na.rm = TRUE)) 

dataN_2021$Date<-as.Date(dataN_2021$Time)

Mdata<-V %>%
  inner_join(dataN_2021, by = "Date")

Mdata


Velocity<- Mdata %>% mutate(Vel= (Ua/NO3_d)*0.001) ###m^-2 d^-1/mgL^-1 = Ld^-1


Velocity

##M1<-mean(Velocity$Vel)

uptakeVel_2021 <-ggplot(Velocity) +
  geom_col(aes(Date, Vel), fill = 'blue4', size = 0.6) +
  ylab(expression(m*'d'^-1)) +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  scale_y_continuous(limits = c(0, 0.01), expand = c(0, 0)) +
  labs(x = 'Nitrate Uptake Velocity,2023') +
  theme_bw() +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 14),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )


uptakeVel_2021



ggsave("Nitrate Uptake velocity ,2023.jpg", uptakeVel_2021, width = 10, height = 6, dpi = 300)


##########################################################

###Nitrate Balance####

OUT_N <-data_2022 %>% select(Time,No3,Q) %>%
  timetk::summarise_by_time(.date_var = Time, .by = 'hour', NO3_hr = mean(No3, na.rm = TRUE),
                            Q_hr= mean(Q, na.rm= TRUE)) %>% 
  mutate(OUT= (NO3_hr*Q_hr))
  

OUT_N

OUT_D<-OUT_N %>% timetk::summarise_by_time(.date_var = Time, .by = 'day', OUT_day = sum(OUT, na.rm = TRUE)) %>% 
  rename("Date"="Time") 

  
IN_N<- V %>% 
  inner_join(OUT_D,by="Date") %>% 
  mutate(IN_d= (OUT_day+Ua)/1000)


IN_N



R<- IN_N %>% 
  mutate(r= ((Ua/IN_d))/1000)
R

MAX<- mean(R$r)

r_plot<-ggplot(R) +
  geom_col(aes(Date, r), fill = 'blue4', size = 0.6) +
  ylab(expression((Ua / IN) ~ '(mg N m'^-2* 'd'^-1*')')) +
  scale_x_datetime(date_breaks = '1 month', date_labels = '%b') +
  scale_y_continuous(limits = c(0, 0.15), expand = c(0, 0)) +
  labs(x = 'Nitrate Uptake (Ua)/Nitrate Load(IN),2022') +
  theme_bw() +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 14),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

r_plot


ggsave("Relative Nitrate Assimilation,2022.jpg", r_plot, width = 10, height = 6, dpi = 300)

########################################

R

IN_plot<-ggplot(R) +
  geom_col(aes(Date, IN_d), fill = 'blue4', size = 0.6) +
  ylab(expression(IN*'(g N m'^-2* 'd'^-1*')')) +
  scale_x_datetime(date_breaks = '1 month', date_labels = '%b') +
  scale_y_continuous(limits = c(0, 2), expand = c(0, 0)) +
  labs(x = 'Daily Nitrate Load IN,2022') +
  theme_bw() +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 14),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

IN_plot

ggsave("IN_2022.jpg",IN_plot, width = 10, height = 6, dpi = 300)


#######################################

U_No33Dt$month= month(U_No33Dt$begin)
U_No33Dt$year= year(U_No33Dt$begin)


monthly_mean <- U_No33Dt %>%
  group_by(year) %>%
  summarise(mean_value = mean(Ua, na.rm = TRUE))

monthly_mean


min_value <- min(U_No33Dt$Ua, na.rm = TRUE)
min_value

max_value <- max(U_No33Dt$Ua, na.rm = TRUE)
max_value





ggsave(filename = 'C:/Users/shawon/Desktop/Thesis/uptake12.jpeg',
       plot = U_No33,
       height = 12,
       width = 20,
       dpi = 300
)




g<-read_csv("controlled_2021.csv")
h<-read_csv("controlled_2022_u.csv")
i<-read_csv("controlled_2023.csv")


my_dataQ<-bind_rows(g,h,i)

my_data_no5<-my_dataQ %>% filter(between(
  Time,
  ymd_hms('2021-01-01 00:00:00'),
  ymd_hms('2023-09-09 00:00:00')
))

j<-ggplot(my_data_no5)+geom_line(aes(Time,wt),color="blue")+
labs(x = '2021 to 2023', y= "Q and No3")+
ylim(c(0,20))+scale_x_datetime(date_breaks = '3 months', date_labels = '%d-%b')

j



graph_final <- grid.arrange(U_No33, j, ncol = 1)

graph_final



ggsave(filename = 'C:/Users/shawon/Desktop/Thesis/graph_No3_2021_2023.jpeg',
       plot = graph_final,
       height = 12,
       width = 20,
       dpi = 300
)





##########################################################%

