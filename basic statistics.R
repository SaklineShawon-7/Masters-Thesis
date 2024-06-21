
######Further work in Cluster#######

#############################################
setwd("C:/Users/shawon/Desktop/Thesis final data analysis/Thesis final")

library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(readxl)
library(tidyverse)
library(timetk)
library(oce)
library(suncalc)
library(gridExtra)
library(ragg)

# data frames
df1 <- read_csv("Final cluster/M9.csv")

df1_day<-df1 %>%  mutate(meanD_value = rowMeans(select(., V1:V24), na.rm = TRUE)) %>% 
  select(variable,Date,meanD_value)

df1_day$date<-mdy(df1_day$Date)

df1_day

df2 <- read_csv("Final cluster/M5.csv")

# Filter df2 based on dates in df1
filtered_df2 <- df2 %>%
  inner_join(df1, by = "variable")

selected_subsets <- filtered_df2 %>%
  group_by(variable)%>%
  sample_n(1) %>%
  ungroup()



view(selected_subsets)

DC<- selected_subsets %>% select(5,6,4,3)

DC$Date <- mdy(DC$Date)

DC$Type <-as.factor(DC$Type)

DC1<-DC %>% filter(between(Date,ymd('2021-01-01'),
                          # must be full day time
                          ymd('2023-08-30')))



nup<-read_csv("Nitrate Uptake/nitrate_uptake_2021_2023.csv")

tail(nup)

DU<-nup %>% filter(between(begin,ymd('2020-01-01'),
                           ymd('2022-12-31'))) %>% mutate(Date= as.Date(begin)) %>% 
  group_by(Date)%>%
  ungroup()

mD <- DU %>%
  inner_join(DC, by = "Date") %>% mutate(month= month(Date))


#DF<- mD %>% filter(month== 1) %>% group_by(Type) %>% summarise(Total_value= sum(Ua))

# Summarize data by month and type
DF <- mD %>%
  group_by(month, Type) %>%
  summarise(Total_value = sum(Ua, na.rm = TRUE)) %>%
  ungroup()

# Print the summarized data to verify
print(DF)


# Assuming mD is your main data frame with 'month', 'Type', and 'Ua' columns
# mD <- data.frame(month = ..., Type = ..., Ua = ...)  # Your actual data

# Define all months
all_months <- 1:12

# Summarize data by month and type
DF <- mD %>%
  group_by(month, Type) %>%
  summarise(Total_value = sum(Ua, na.rm = TRUE)) %>%
  ungroup()
DF

write.csv(DF,"total_uptake_vlaue_clusterwise_2023.csv")

# Ensure all months are represented, even if some have no data
DF <- DF %>%
  complete(month = all_months, Type = unique(mD$Type), fill = list(Total_value = 0))



# Create the stacked bar plot
mygp<- ggplot(DF, aes(x = factor(month), y = Total_value, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_discrete(name = "Monthly nitrate uptake based on Clusters, 2023", labels = month.abb) +
  ylab(expression("Total Uptake,"*'U'[a]*'NO'[3]*'(mg N m'^-2* 'd'^-1*')')) +
  scale_y_continuous() +
  scale_fill_manual(name = "Type", values = c("C1" = "aquamarine4", "C2" = "dodgerblue4", "C3" = "coral")) +  # Set fixed colors
  labs(fill = "Cluster") +  # Label for the legend
  theme_minimal() +
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

mygp

ggsave("total uptake based on cluster, 2023.jpg", mygp, width = 10, height = 6, dpi = 300)


#########################
CU1<-read_csv("total_uptake_vlaue_clusterwise_2022.csv")

DC1

CU<-DC1 %>% select(Type,value) %>% group_by(Type) %>% summarise(Total_V= sum(value))
  
CU 

CC<-DC1 %>% select(Type,value) %>% count(Type)

CC 

###########################################
###Correlation###
Data<- read_csv("Data/2020 to 2023.10.14.csv")

Data1<-Data %>% select(Time,No3,wt,Q) %>% filter(between(
  Time,
  ymd_hms('2022-11-01 00:00:00'),
  ymd_hms('2023-01-31 00:00:00')
)) %>%
  timetk::summarise_by_time(.date_var = Time, .by = 'day',wt_d= mean(wt,na.rm=T),
                            No3_d=mean(No3,na.rm= T),
                            Q_d=mean(Q,na.rm=T)) %>% rename("Date"="Time") %>% na.omit()


Data1

DC1


DD <- Data1 %>%
  inner_join(DC1, by = "Date") %>% mutate(month= month(Date)) %>% 
  rename("Cluster_Value"= "value")



COR1 <- DD %>% filter(Type=="C3") %>% select(wt_d,No3_d,Q_d,Cluster_Value) %>% na.omit() 
ANOVA <- DD %>% filter(Type=="C3") %>% select(wt_d,No3_d,Q_d,Cluster_Value) %>% na.omit() 


######Pearson correlation plot###
cor_matrix <- cor(ANOVA)

cor_subset <- cor_matrix["Cluster_Value", c("Q_d", "No3_d", "wt_d")]

cor_df <- as.data.frame(as.table(cor_subset))

cor_df

Cp<-ggplot(cor_df, aes(x = Var1, y = "C1", fill = Freq)) +
  geom_tile(color = "grey") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "black", size = 5) +
  xlab("Discharge(Q),Nitrate(No3),Water Temperature(wt)")+
  ylab('Cluster C1')+
  theme_minimal() +
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
  ) +
  ggtitle("Correlation Heatmap")

Cp

ggsave("pcorelation_C1.jpg", Cp, width = 10, height = 6, dpi = 300)

####Spearman's Rank correlation####

Sp<-ggplot(CORU, aes(x = GR, y = Ua)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(x = expression("GR"), y = "Ua") +  
  ggtitle("Spearman's Rank Correlation") +  # Add title
  theme_minimal()+
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


Sp
ggsave("Scorelation_C3WT.jpg", Sp, width = 10, height = 6, dpi = 300)

####Spearman's rank corelation value###

wt_d_vector <- COR1$No3_d
value_vector <- COR1$Cluster_Value

spearman_corr <- cor(wt_d_vector, value_vector, method = "spearman")
spearman_corr


####ANOVA#####

model <- aov(Cluster_Value ~ wt_d + No3_d + Q_d, data = ANOVA)

anova_result <- anova(model)

print(anova_result)







###########basic statistics and Graph creation###########


### yearly, monthly avg###
##try different years (2020 to 2023) dataset for the result####

Data_2022<- read_csv("Data/2020 to 2023.10.14.csv")
View(Data_2022)

mean(Data_2022$Q, na.rm = T)

###Try different parameter to have the basic statistics####

Data_2022<-Data_2022 %>% filter(between(
    Time,
    ymd_hms('2022-01-01 00:00:00'),
    ymd_hms('2022-12-31 00:00:00')
  )) %>% timetk::summarise_by_time(.date_var = Time, .by = 'month',
                                   wt_month= mean(wt, na.rm=T)) 
  
Data_2022  



##Global Radiation########

GR_2021<-read_csv("Data/Global radiation data2021.csv")
GR_2021<- GR_2021 %>% select(3,4) %>% rename("Time"="Zeitstempel", "value"="Wert")

GRh_2021<- GR_2021 %>% timetk::summarise_by_time(.date_var = Time, .by = 'day',
                                                 value_hr= sum(value, na.rm=T)) 




GR_2023 <- read_csv("Data/Global Radiation data2023.csv")

GR_2023<- GR_2023 %>% select(3,4) %>% rename("Time"="Zeitstempel", "value"="Wert")

GRh_2023<- GR_2023 %>% timetk::summarise_by_time(.date_var = Time, .by = 'day',
                                                 value_hr= sum(value, na.rm=T)) 

GRh_2023

All_GR<- bind_rows(GRh_2021,GRh_2022,GRh_2023)

### create Plot of GR data ######
All_GR$Time <- as.Date(All_GR$Time) 

GR_2023<- ggplot(All_GR) +
  geom_line(aes(Time, value_hr/100), color = 'blue4', size = 0.6) +
  ylab(expression(GR ~ values ~ (J~m^-2~d^-1))) +
  scale_x_date(date_breaks = '6 month', date_labels = '%b-%y') +
  scale_y_continuous() +
  xlab("Global Radiation, 2021 to 2023") +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(colour = "black", size = 12),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

GR_2023

ggsave("GR_all.jpg", GR_2023, width = 10, height = 6, dpi = 300)

######Discharge (Q)######
##Try year 2021, 2022 and 2023####

Q_2021 <-read_csv("Data/controlled_2022_u.csv")

###hourly mean##
Q_2021<-Q_2021 %>% select(Time,Q)%>%
  timetk::summarise_by_time(.date_var = Time, .by = 'hour',Q_hr= mean(Q, na.rm=T)) 

Q_2021$Time <- as.Date(Q_2021$Time)  

#Custom_Months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


###Custom months as lebel###
#Custom_Months <- function(breaks) {
#  months <- month(breaks)
#  Custom_Months[months]}

###Plot###
Dis_2021 <- ggplot(Q_2021) +
  geom_line(aes(Time, Q_hr), color = 'blue4', size = 0.6) +
  ylab(expression(Q ~ (L ~ s^-1))) +
  scale_x_date(date_breaks = '1 month', date_labels = "%b", expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 3), expand = c(0, 0)) +
  xlab("Discharge, 2021") +
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(colour = "black", size = 12),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

Dis_2021

ggsave("Dis_2021.jpg", Dis_2021, width = 8, height = 6, dpi = 300)



###Combined graph for three years####

D_2021<-read_csv("Data/controlled_2021.csv")
D_2022<-read_csv("Data/controlled_2022.csv")
D_2023<-read_csv("Data/controlled_2023.csv")

D_all<- bind_rows(D_2021,D_2022,D_2023)

###hourly mean##
Discharge<-D_all %>% select(Time,Q)%>% filter(between(Time,ymd_hms("2021-01-01 00:02:00"),ymd_hms("2023-09-08 23:52:00"))) %>% 
  timetk::summarise_by_time(.date_var = Time, .by = 'day',Q_hr= mean(Q, na.rm=T)) 

Discharge$Time <- as.Date(Discharge$Time)  

HH<-mean(Discharge$Q_hr)

###Plot###
Dis_all <- ggplot(Discharge) +
  geom_line(aes(Time, Q_hr), color = 'blue4', size = 0.6) +
  ylab(expression(Q ~ (L ~ s^-1))) +
  scale_x_date(date_breaks = '3 month', date_labels = "%b-%Y", expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 3), expand = c(0, 0)) +
  xlab("Date") +
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(colour = "black", size = 12),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

Dis_all


ggsave("Dis_all_daily.jpg", Dis_all, width = 10, height = 6, dpi = 300)



###mean##
D_all1<-read_csv("Data/2020 to 2023.10.14.csv")

D_M<-D_all1 %>% select(Time,wt)%>% 
  filter(between(Time,ymd_hms("2023-01-01 00:02:00"),ymd_hms("2023-01-31 23:52:00"))) %>% 
  timetk::summarise_by_time(.date_var = Time, .by = 'month',WT_hr= mean(wt, na.rm=T)) 

D_M


####################################################################
######## Diurnal plot of Nitrate concentration#######

NO3_2022 <- read_csv("Data/2020 to 2023.10.14.csv")

NO3_2022

NO3_2022<-NO3_2022 %>% filter(between(Time,ymd_hms("2022-04-26 00:02:00"),ymd_hms("2022-05-04 23:52:00"))) %>% select(Time,No3)%>%
  timetk::summarise_by_time(.date_var = Time, .by = 'hour',
                            No3_hr= mean(No3, na.rm=T)) %>% na.omit()
NO3_2022

NO3_2022$Time <- as.POSIXct(NO3_2022$Time) 

###day and night seperation by suncalc package####

shade <- data.frame(
  getSunlightTimes(
    date = seq.Date(
      as.Date("2022-04-26", format = '%Y-%m-%d'),
      by = 'day',
      length.out = 09
    ),
    lat = 51.950573,
    lon = 11.163875,
    tz = 'UTC',
    keep = c('sunrise', 'sunset')
  )) %>%
  mutate(
    xmin = sunset,
    xmax = lead(sunrise),
    ymin = -Inf,
    ymax = Inf
  ) %>%
  na.omit()

View(shade)


####Plot 2022 (spring)###

NO3_22S<- ggplot(NO3_2022) +
  geom_rect(data = shade, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill='grey4', alpha = 0.5) +
  geom_line(aes(Time, No3_hr), color = 'blue4', size = 0.6) +
  ylab(expression(NO[3]-N ~ (mg ~ L^-1))) +
  scale_x_datetime(date_breaks = '1 day', date_labels = "%d-%b-%y", expand = c(0, 0)) +
  scale_y_continuous() +
  xlab("Date") +
  theme_bw()+
  theme(
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 14),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

NO3_22S

ggsave("graph_NO_diurnal_spring.jpg", NO3_22S, width = 10, height = 6, dpi = 300)

#########################
NO3_22 <- read_csv("Data/2020 to 2023.10.14.csv")

NO3_22<-NO3_22 %>% filter(between(Time,ymd_hms("2022-06-24 00:02:00"),ymd_hms("2022-06-30  23:52:00"))) %>% select(Time,No3)%>%
  timetk::summarise_by_time(.date_var = Time, .by = 'hour',
                            No3_hr= mean(No3, na.rm=T)) %>% na.omit()
NO3_22



library(suncalc)
shade1 <- data.frame(
  getSunlightTimes(
    date = seq.Date(
      as.Date("2022-06-24", format = '%Y-%m-%d'),
      by = 'day',
      length.out = 7
    ),
    lat = 51.950573,
    lon = 11.163875,
    tz = 'UTC',
    keep = c('sunrise', 'sunset')
  )) %>%
  mutate(
    xmin = sunset,
    xmax = lead(sunrise),
    ymin = -Inf,
    ymax = Inf
  ) %>%
  na.omit()

View(shade1)



###Plot 2022 (summer)####

NO3_22Su<- ggplot(NO3_22) +
  geom_rect(data = shade1, aes(xmin = xmin,xmax = xmax, ymin = ymin,ymax = ymax), fill='grey4', alpha = 0.5) +
  geom_line(aes(Time, No3_hr), color = 'blue4', size = 0.6) +
  ylab(expression(NO[3] ~ (mg ~ L^-1))) +
  scale_x_datetime(date_breaks = '1 day', date_labels = "%d-%b", expand = c(0, 0)) +
  scale_y_continuous() +
  xlab("Nitrate Concentration (Summer, 2022)") +
  theme_bw()+
  theme(
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 14),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

NO3_22Su


ggsave("graph_NO_diurnal.jpg", NO3_22Su, width = 10, height = 6, dpi = 300)


#############################
##General plot of nitrate concentration#####

NO3_23 <- read_csv("Data/controlled_2023.csv")

NO3_23

tail(NO3_23)
NO3_23<-NO3_23 %>% filter(between(Time,ymd_hms("2023-01-01 00:02:00"),ymd_hms("2023-09-08 23:52:00"))) %>% select(Time,No3)%>% 
  timetk::summarise_by_time(.date_var = Time, .by = 'day',
                            No3_hr= mean(No3, na.rm=T)) %>% na.omit()
NO3_23

m<- max(NO3_23$No3_hr)

m

NO3_23$Time <- as.POSIXct(NO3_23$Time)

# Plotting the data
NO3_203<- ggplot(NO3_23) +
  geom_line(aes(Time, No3_hr), color = 'blue4', size = 0.6) +
  ylab(expression(NO[3]-N ~ (mg ~ L^-1))) +
  scale_x_datetime(date_breaks = '3 month', date_labels = "%b-%y", expand = c(0, 0)) +
  scale_y_continuous(limits = c(6.5, 9.5), expand = c(0, 0)) +
  xlab("Date") +
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

NO3_203



####2022

NO3_2 <- read_csv("Data/controlled_2022_u.csv")

NO3_2

tail(NO3_2)
NO3_2<-NO3_2 %>% filter(between(Time,ymd_hms("2022-01-01 00:02:00"),ymd_hms("2022-12-31 23:52:00"))) %>% select(Time,No3)%>% 
  timetk::summarise_by_time(.date_var = Time, .by = 'day',
                            No3_hr= mean(No3, na.rm=T)) %>% na.omit()
NO3_2

NO3_2d <- NO3_2[-c(119,120),]

m<- mean(NO3_2d$No3_hr)

m
NO3_2d$Time <- as.POSIXct(NO3_2d$Time)
# Plotting the data
NO3_202<- ggplot(NO3_2d) +
  geom_line(aes(Time, No3_hr), color = 'blue4', size = 0.6) +
  ylab(expression(NO[3]-N ~ (mg ~ L^-1))) +
  scale_x_datetime(date_breaks = '3 month', date_labels = "%b-%y", expand = c(0, 0)) +
  scale_y_continuous(limits = c(6.5, 9.5), expand = c(0, 0)) +
  xlab("Date") +
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

NO3_202




####2021

NO3_1 <- read_csv("Data/controlled_2021.csv")

NO3_1

tail(NO3_1)
NO3_1<-NO3_1 %>% filter(between(Time,ymd_hms("2021-01-01 00:02:00"),ymd_hms("2021-12-31 23:52:00"))) %>% select(Time,No3)%>% 
  timetk::summarise_by_time(.date_var = Time, .by = 'day',
                            No3_hr= mean(No3, na.rm=T)) %>% na.omit()
NO3_1

NO3_1$Time <- as.POSIXct(NO3_1$Time)

m<- max(NO3_1$No3_hr)

m
# Plotting the data
NO3_201<- ggplot(NO3_1) +
  geom_line(aes(Time, No3_hr), color = 'blue4', size = 0.6) +
  ylab(expression(NO[3]-N ~ (mg ~ L^-1))) +
  scale_x_datetime(date_breaks = '3 month', date_labels = "%b-%y", expand = c(0, 0)) +
  scale_y_continuous(limits = c(6.5, 9.5), expand = c(0, 0)) +
  xlab("Date") +
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

NO3_201


graph_NO3xxx <- grid.arrange(NO3_201,NO3_202,NO3_203,ncol = 1)


ggsave("graph_NO40.jpg", graph_NO3xxx, width = 8, height = 12, dpi = 300)
















###NO3_2022####


NO3_2022 <- read_csv("Data/controlled_2022_u.csv")

NO3_2022


NO3_2022<-NO3_2022 %>% filter(between(Time,ymd_hms("2022-01-01 00:02:00"),ymd_hms("2022-12-31 23:52:00"))) %>% select(Time,No3)%>% 
 # timetk::summarise_by_time(.date_var = Time, .by = 'hour',No3_hr= mean(No3, na.rm=T)) %>% 
  na.omit()
NO3_2022

#m1<- mean(NO3_2022$No3)

NO3_2022d <- NO3_2022[-c(2858:2880,6471:6476,7352:7355),]

#NO3_2022d$Time <- as.Date(NO3_2022d$Time)  

# Plotting the data
NO3_22<- ggplot(NO3_2022d) +
  geom_line(aes(Time, No3_hr), color = 'blue4', size = 0.6) +
  ylab(expression(NO[3] ~ (mg ~ L^-1))) +
  scale_x_datetime() +
  scale_y_continuous(limits = c(6.5, 8.5), expand = c(0, 0)) +
  xlab("Nitrate Concentration, 2022") +
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

NO3_22

graph_NO3 <- grid.arrange(NO3_201,NO3_22,ncol = 1)


ggsave("graph_NO33.jpg", graph_NO3, width = 8, height = 8, dpi = 300)


########################NO3_2023########

NO3_2021S <- read_csv("Data/controlled_2021.csv")

NO3_2021S

NO3_2021S<-NO3_2021S %>% filter(between(Time,ymd_hms("2021-04-19 00:02:00"),ymd_hms("2021-04-28 23:52:00"))) %>% select(Time,No3)%>% timetk::summarise_by_time(.date_var = Time, .by = 'hour',
                                                                                                                                                             No3_hr= mean(No3, na.rm=T)) %>% na.omit()
NO3_2021S
NO3_2021S$Time <- as.POSIXct(NO3_2021S$Time)

# ###Plot####
NO3_21s<- ggplot(NO3_2021S) +
  geom_line(aes(Time, No3_hr), color = 'blue4', size = 0.6) +
  ylab(expression(NO[3]-N ~ (mg ~ L^-1))) +
  scale_x_datetime(date_breaks = '1 day', date_labels = "%d-%b-%y", expand = c(0, 0)) +
  scale_y_continuous() +
  xlab("Date") +
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

NO3_21s
################################
NO3_2021S1 <- read_csv("Data/2020 to 2023.10.14.csv")

NO3_2021S1

NO3_2021S1<-NO3_2021S1 %>% filter(between(Time,ymd_hms("2021-12-27 00:02:00"),ymd_hms("2022-01-02 23:52:00"))) %>% select(Time,No3)%>% timetk::summarise_by_time(.date_var = Time, .by = 'hour',
                                                                                                                                                               No3_hr= mean(No3, na.rm=T)) %>% na.omit()
NO3_2021S1
NO3_2021S1$Time <- as.POSIXct(NO3_2021S1$Time)

# ###Plot####
NO3_21s1<- ggplot(NO3_2021S1) +
  geom_line(aes(Time, No3_hr), color = 'blue4', size = 0.6) +
  ylab(expression(NO[3]-N ~ (mg ~ L^-1))) +
  scale_y_continuous() +
  scale_x_datetime(date_breaks = '1 day', date_labels = "%d-%b-%y", expand = c(0, 0)) +
  xlab("Date") +
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

NO3_21s1


graph_NO3spww <- grid.arrange(NO3_21s,NO3_21s1,ncol = 1)


ggsave("graph_NO3spww.jpg", graph_NO3spww, width = 12, height = 8, dpi = 300)





###########################################
#### Water Temperture plot####

T_2022 <- read_csv("Data/controlled_2022.csv")


T_2022<-T_2022 %>% filter(between(Time,ymd_hms("2022-11-01 00:02:00"),ymd_hms("2023-02-31 23:52:00"))) %>% select(Time,wt)%>% 
  timetk::summarise_by_time(.date_var = Time, .by = 'hour',wt_hr= mean(wt, na.rm=T)) %>%
  na.omit()

T_2022


#T_2022$Time <- as.Date(T_2022$Time)  

Tem_2022<- ggplot(T_2022) +
  geom_line(aes(Time, wt_hr), color = 'blue4', size = 0.6) +
  ylab(expression(WT ~ values ~ (degree*C))) +
  scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b-%Y') +
  scale_y_continuous(limits = c(0, 25), expand = c(0, 0)) +
  xlab("Water Temperature (2022)") +
  theme_bw()+theme(text = element_text(size = 14))

Tem_2022



####Temperature######
T_2023 <- read_csv("Data/controlled_2023.csv")

T_2023<-T_2023 %>% filter(between(Time,ymd_hms("2023-06-01 00:02:00"),ymd_hms("2023-08-31 23:52:00"))) %>% select(Time,wt)%>% 
  timetk::summarise_by_time(.date_var = Time, .by = 'day',wt_hr= mean(wt, na.rm=T)) %>%
  na.omit()

T_2023


jhjh<- mean(T_2023$wt_hr)



T_2023$Time <- as.Date(T_2023$Time)  

Tem_2023<- ggplot(T_2023) +
  geom_line(aes(Time, wt_hr), color = 'blue4', size = 0.6) +
  ylab(expression(WT ~ values ~ (degree*C))) +
  scale_x_date(date_breaks = '2 month', date_labels = '%d-%b') +
  scale_y_continuous(limits = c(0, 25), expand = c(0, 0)) +
  xlab("Water Temperature (2022)") +
  theme_bw()+theme(text = element_text(size = 14))

Tem_2023

graph_NO3 <- grid.arrange(NO3_21,NO3_201,ncol = 1)


ggsave("graph_NO39.jpg", graph_NO3, width = 10, height = 6, dpi = 300)



###########################################################
#####Winter season WT graph#######

D_all
library(ggplot2)
library(dplyr)
library(lubridate)

T_2021w<-D_all %>% filter(between(Time,ymd_hms("2022-11-01 00:02:00"),ymd_hms("2023-01-31 23:52:00"))) %>% select(Time,wt)%>% 
  timetk::summarise_by_time(.date_var = Time, .by = 'hour',wt_hr= mean(wt, na.rm=T)) 

T_2021w


# Generate complete sequence of time points for comparison
complete_time <- seq(from = min(T_2021w$Time), to = max(T_2021w$Time), by = "hour")

# Merge complete sequence with original data to introduce NA for missing times
complete_data <- data.frame(Time = complete_time) %>%
  left_join(T_2021w, by = "Time")

# Plot the data
TemW_2021 <- ggplot(complete_data, aes(x = Time, y = wt_hr)) +
  geom_line(color = 'blue4', size = 0.6) +
  #geom_point(data = subset(complete_data, is.na(wt_hr)), aes(x = Time, y = 0), color = 'red', size = 2) +
  ylab(expression(WT ~ values ~ (degree * C))) +
  scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b-%Y') +
  scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
  xlab("Date") +
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

TemW_2021


ggsave("T_Winter_2023.jpg", TemW_2021, width = 10, height = 6, dpi = 300)

#T_2022w$Time <- as.Date(T_2022w$Time)  

TemW_2021<- ggplot(T_2021w) +
  geom_line(aes(Time, wt_hr), color = 'blue4', size = 0.6) +
  ylab(expression(WT ~ values ~ (degree*C))) +
  scale_x_datetime(date_breaks = '3 month', date_labels = '%d-%b-%Y') +
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0)) +
  xlab("Water Temperature in Winter,2021") +
  theme_bw()+theme(
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

TemW_2021



ggsave("T_Winter_2021.jpg", TemW_2021, width = 10, height = 6, dpi = 300)


#########################################
########min,max and avg############


D_2023 <- read_csv("Data/controlled_2023.csv")

D_2023$Time <- as.Date(D_2023$Time) 

T_2021$months<- month(T_2021$Time)

D_all
D1_2023<-D_all %>% select (Time,wt)%>% filter(between(Time,ymd_hms("2022-11-01 00:02:00"),ymd_hms("2023-01-31 23:52:00"))) %>% 
  filter(wt>1) %>% 
  #filter(months %in% c(11,12,1)) %>% 
  #timetk::summarise_by_time(.date_var = Time, .by = 'hour',wt_hr= mean(wt, na.rm=T))%>% 
  mutate(Max_value=max(wt, na.rm=T),
         Min_value= min(wt, na.rm=T),
         Avg= mean(wt, na.rm=T))

D1_2023



################################################################################

####Amplitude####

NO3a<- read_csv("Data/2020 to 2023.10.14.csv")

NO3a$Time <- mdy_hm(NO3a$Time)

sb <-NO3a %>% filter(between(Time,ymd_hms("2023-01-01 00:02:00"),ymd_hms("2023-09-08 23:52:00")))
sb$date <- format(sb$Time, format = "%Y-%m-%d")
sb$time1 <- format(sb$Time, format = "%H:%M")
sb$time <- as.POSIXct(sb$Time, format = "%H:%M:%S")
sb$year <- year(sb$Time)
sb$month <- month(sb$Time)
sb$day <- day(sb$Time)
sb$hour <- hour(sb$Time)

# Drop NA values and convert columns to numeric
sb2 <- sb %>%
  drop_na() %>%
  mutate(
    No3 = as.numeric(No3)
  )

# Select relevant columns and group by year, month, and day
sb_2018_clean <- sb2 %>%
  drop_na(No3) %>%
  select(time, No3, year, month, day, hour) %>%
  group_by(year, month, day) %>%
  mutate(mean_d = mean(No3)) %>%
  ungroup()

# Summarize max and min NO3 values and calculate amplitude
result <- sb_2018_clean %>%
  group_by(year, month, day) %>%
  summarise(
    max_NO3 = max(No3, na.rm = TRUE),
    min_NO3 = min(No3, na.rm = TRUE)
  ) %>%
  mutate(amplitude = (max_NO3 - min_NO3) / 2) %>% filter(amplitude < 0.5)

# View the result
print(result)

View(result)


dl<- c(221,268,272)
#dl1<-c(10,12,175)

result1<-result[-dl,]

m777<-mean(result$amplitude)

# Plotting the data
amp <- result %>%
  ggplot(aes(x = as.Date(paste(year, month, day, sep = "-")), y = amplitude)) +
  geom_bar(stat = "identity", fill = 'blue') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  scale_y_continuous(breaks = seq(0, max(result$amplitude, na.rm = TRUE), by = 0.1)) +
  xlab("Daily Nitrate Amplitude, 2021") +
  ylab(expression(Amplitude ~ (mg~L^-1)))+
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 14),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

print(amp)



ggsave("amplitude2021.jpg", amp, width = 10, height = 6, dpi = 300)



###################################################################################

####DO and SatO plot#####

D_all2<-read_csv("Data/2020 to 2023.10.14.csv")

DO<-D_all2 %>% select(Time,O2_konz)%>%
  filter(between(Time,ymd_hms("2021-03-29 00:02:00"),ymd_hms("2021-04-04 23:52:00"))) %>% 
  timetk::summarise_by_time(.date_var = Time, .by = 'hour',DO_hr= mean(O2_konz, na.rm=T)) %>% 
  drop_na()
DO

#dl_do<- DO[-c(11520:11591,19281:19449),]

shade_DO <- data.frame(
  getSunlightTimes(
    date = seq.Date(
      as.Date("2021-03-29", format = '%Y-%m-%d'),
      by = 'day',
      length.out = 07
    ),
    lat = 51.950573,
    lon = 11.163875,
    tz = 'UTC',
    keep = c('sunrise', 'sunset')
  )) %>%
  mutate(
    xmin = sunset,
    xmax = lead(sunrise),
    ymin = -Inf,
    ymax = Inf
  ) %>%
  na.omit()

DO_all<- ggplot(DO) +
  geom_rect(data = shade_DO, aes(xmin = xmin,xmax = xmax, ymin = ymin,ymax = ymax), fill='grey', alpha = 0.5) +
  geom_line(aes(Time, DO_hr), color = 'purple4', size = 0.6) +
  ylab(expression(DO ~ values ~ (mg~L^-1))) +
  scale_x_datetime(date_breaks = '1 day', date_labels = '%d-%b-%y') +
  scale_y_continuous(limits = c(5, 15), expand = c(0, 0)) +
  xlab("Date") +
  theme_bw()+theme(
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

DO_all



ggsave("DO_Saturation.jpg", DO_all, width = 10, height = 6, dpi = 300)



####Saturation oxygen####

D_all2<-read_csv("Data/2020 to 2023.10.14.csv")

SO<-D_all2 %>% select(Time,O2_proz)%>%
  filter(between(Time,ymd_hms("2021-03-29 00:02:00"),ymd_hms("2021-04-04 23:52:00"))) %>% 
  timetk::summarise_by_time(.date_var = Time, .by = 'hour',SatO_hr= mean(O2_proz, na.rm=T)) %>% 
  drop_na()
SO

#dl_SO<- SO[-c(11472:11591,19233:19496),]


shade_SO <- data.frame(
  getSunlightTimes(
    date = seq.Date(
      as.Date("2021-03-29", format = '%Y-%m-%d'),
      by = 'day',
      length.out = 07
    ),
    lat = 51.950573,
    lon = 11.163875,
    tz = 'UTC',
    keep = c('sunrise', 'sunset')
  )) %>%
  mutate(
    xmin = sunset,
    xmax = lead(sunrise),
    ymin = -Inf,
    ymax = Inf
  ) %>%
  na.omit()

View(shade)


SO_all<- ggplot(SO) +
  geom_line(aes(Time, SatO_hr), color = 'blue1', size = 0.6) +
  geom_rect(data = shade_SO, aes(xmin = xmin,xmax = xmax, ymin = ymin,ymax = ymax), fill='grey', alpha = 0.5) +
  ylab(expression(SatO ~ values ~ (paste("%")))) +
  scale_x_datetime(date_breaks = '1 day', date_labels = '%d-%b-%y') +
  scale_y_continuous(limits = c(80, 120), expand = c(0, 0)) +
  xlab("Date") +
  theme_bw()+theme(
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

SO_all



ggsave("SatO_diurnal_pattern.jpg", SO_all, width = 10, height = 6, dpi = 300)


graph_O2 <- grid.arrange(DO_all,SO_all,ncol = 1)

ggsave("graph_O2.jpg", graph_O2, width =10, height = 8, dpi = 300)


############################################################

###########################################
###Correlation###
Data<- read_csv("Data/2020 to 2023.10.14.csv")

Data1<-Data %>% select(Time,No3,Q,wt,Ec,Ph) %>% filter(between(Time,ymd_hms("2021-04-01 00:02:00"),ymd_hms("2023-08-31 23:52:00"))) %>% 
  timetk::summarise_by_time(.date_var = Time, .by = 'day',WT= mean(wt,na.rm=T),
                            NO3=mean(No3,na.rm= T),
                            Q =mean(Q,na.rm=T),
                            EC= mean(Ec, na.rm=T),
                            pH= mean(Ph, na.rm=T)) %>% rename("Date"="Time") %>% na.omit()


Data1

All_GR1<-All_GR %>% rename("Date"="Time", "GR"= "value_hr")

All_GR1

NUP<- read_csv("nitrate_uptake_2021_2023.csv")
NUP

DUD <- NUP %>%
  inner_join(Data1, by = "Date") %>% mutate(month= month(Date)) 

DUD1<-DUD %>% inner_join(All_GR1, by="Date")

CORU <- DUD1 %>% select(Date,Ua,NO3,WT,Q,GR,pH) %>% na.omit() 
CORU

CORU1 <- DUD1 %>% select(Ua,NO3,WT,Q,GR,pH) %>% na.omit() 
CORU1
#ANOVA <- DD %>% filter(Type=="C3") %>% select(wt_d,No3_d,Q_d,Cluster_Value) %>% na.omit() 


######Pearson correlation plot###
cor_u <- cor(CORU1)

cor_us <- cor_u["NO3",c("GR")]

coru_df <- as.data.frame(as.table(cor_us))

coru_df

#coru_df <- coru_df %>%
#  mutate(Var1 = recode(Var1, "A" = "WT"))

Cpu<-ggplot(coru_df, aes(x = Var1, y = "Ua", fill = Freq)) +
  geom_tile(color = "grey") +
  scale_fill_gradient2(low = "blue", high = "red3", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "black", size = 5) +
  xlab("Global Radiation (GR), Water Temperature (WT)")+
  ylab('Nitrate Uptake (Ua)')+
  theme_minimal() +
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
  ) +
  ggtitle("Pearson Correlation ")

Cpu

ggsave("pcorelation_uptawtGR.jpg", Cpu, width = 10, height = 6, dpi = 300)



#####





###################################################################

data<-CORU
data

Date = seq.Date(from = as.Date("2022-01-01"), to = as.Date("2022-12-31"), by = "day")


# Function to calculate correlation for a given period
calculate_correlation <- function(data, start_date, end_date) {
  subset_data <- data %>% filter(Date >= start_date & Date <= end_date)
  cor_value <- cor(subset_data$Ua, subset_data$WT)
  return(cor_value)
}

# the time periods for water temperature
time_periods <- list(
  c("2022-03-22", "2022-04-03"),
  c("2022-04-04", "2022-04-14"),
  c("2022-05-01", "2022-05-31"),
  c("2022-06-29","2022-07-17"),
  c("2022-08-05","2022-08-17"),
  c("2022-10-01","2022-10-12"))

# the time periods for GR
#time_periods <- list(
#  c("2022-03-22", "2022-04-03"),
#  c("2022-04-04", "2022-04-09"),
#  c("2022-05-01", "2022-05-31"),
#  c("2022-06-29","2022-07-20"),
#  c("2022-08-05","2022-08-12"),
#  c("2022-10-09","2022-10-28"))

# Calculate correlations for each period
correlations <- sapply(time_periods, function(period) {
  start_date <- as.Date(period[1])
  end_date <- as.Date(period[2])
  calculate_correlation(data, start_date, end_date)
})

# Create a result table
result_table <- data.frame(
  Parameter = "Ua;NO3-N;dt",
  `19.03-30.03` = correlations[1],
  `04.04-04.20` = correlations[2],
  `01.05-31.05` = correlations[3],
  `29.06-20.07` = correlations[4],
  `05.08-12.08` = correlations[5],
  `09.10-31.10` = correlations[6]
)

# Print the result table
print(result_table)



