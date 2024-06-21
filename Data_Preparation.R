

library(readr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ragg)  

d <- read_csv("C:/Users/shawon/Desktop/data/T1/final datasets/2020 to 2023.10.14.csv")
View(d)


sb1<-d %>% filter(between(
    Time,
    ymd_hms('2021-01-01 00:02:00'),
    # must be full day time
    ymd_hms('2021-12-22 23:52:00')))

#sb1<-d %>% filter(between(
#  Time,
 # ymd_hms('2021-03-07 00:02:00'),
  # must be full day time
 # ymd_hms('2021-12-22 23:52:00')))

sb1
View(sb1)



# Assuming 'df' is your dataset and 'date_to_remove' contains the specific date you want to remove
date_to_remove <- c("2021-01-02","2021-01-04","2021-01-13","2021-01-14","2021-01-19","2021-01-20","2021-01-21","2021-01-22","2021-01-25",
                    "2021-02-12","2021-02-14","2021-02-02","2021-02-03","2021-02-07","2021-02-08","2021-02-10","2021-02-16","2021-02-17","2021-02-18","2021-02-19","2021-02-20",
                    "2021-03-03","2021-03-02","2021-03-05","2021-03-06","2021-03-24","2021-03-25","2021-02-26","2021-01-01",
                    "2021-04-18", "2021-04-10","2021-04-11","2021-04-12", "2021-05-07","2021-02-27","2021-03-29","2021-03-04",
                    "2021-07-06", "2021-06-12", "2021-05-23", "2021-05-25","2021-06-11","2021-06-21","2021-06-30","2021-07-09","2021-07-17","2021-08-02","2021-08-04",
                    "2021-10-21","2021-10-22", "2021-09-10","2021-09-11", "2021-08-06","2021-08-10","2021-08-27", "2021-08-19","2021-08-29","2021-08-30","2021-09-27","2021-10-05","2021-10-09","2021-10-12",
                    "2021-12-01","2021-12-02","2021-12-03", "2021-11-05", "2021-11-04","2021-11-26","2021-11-28","2021-12-28") # Example date to remove

# Convert 'DateTime' column to Date format
sb1$Date <- as.Date(sb1$Time)

# Remove rows corresponding to the specified date
df <- sb1[!sb1$Date %in% as.Date(date_to_remove), ]

# Remove the 'Date' column if no longer needed
df <- subset(df, select = -Date)

# Print the updated dataframe
print(df)


write.csv(df,"controlled_2021.csv")

df7<-df %>% filter(between(
  Time,
  ymd_hms('2021-03-01 00:02:00'),
  # must be full day time
  ymd_hms('2021-03-31 23:52:00')))


df7
############################################################
da<-df3

Dt<- d %>%
  filter(between(
    Time,
    ymd_hms('2021-01-01 00:02:00'),
    # must be full day time
    ymd_hms('2023-09-09 23:52:00'))) %>%
  timetk::summarise_by_time(.date_var = Time, .by = 'hour', WT = mean(wt, na.rm = TRUE))




l1<-ggplot(Dt, aes(Time,WT))+geom_line()+
  scale_x_datetime(date_breaks = '3 month', date_labels = '%d-%b')+
  labs(x = '2021-2023', y= "WT(°C)")+
  ggtitle("Hourly Mean Value by Month in 2021-2023")+
  theme(
    text = element_text(size = 20) # Set the font size to 14 (or any size you prefer)
  )
l1


ggsave(filename = 'C:/Users/shawon/Desktop/Thesis/temp1.jpeg',
       plot = l1,
       height = 12,
       width = 20,
       dpi = 300
)



min_value <- min(D2023$Q, na.rm = TRUE)
min_value

max_value <- max(D2023$Q, na.rm = TRUE)
max_value

######################################################################

l2<-ggplot(sb1, aes(Time,Q))+geom_line()+ylim(c( 3,12))+
  scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')+
  labs(x = '2021', y= "Q_before(l/s)")
l2


graph_Q_2021 <- grid.arrange(l2, l1, ncol = 1)

# Combine plots l2 and l1 into one column
graph_Q_2021 <- grid.arrange(l2, l1, ncol = 1)

graph_Q_2021

# Export the plot as a JPEG file
ggsave(filename = 'C:/Users/shawon/Desktop/Thesis/graph_Q_2021.jpeg',
       plot = graph_Q_2021,
       height = 12,
       width = 20,
       dpi = 300
)



#j<-ggplot(df)+geom_line(aes(Time,No3),color="blue")+geom_line(aes(Time,Q),color="red")+
  #labs(x = '2021', y= "Q and No3")+
  #ylim(c(0,12))+scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')

#j



###################################################################

###2022###

sb2<-d %>% filter(between(
  Time,
  ymd_hms('2022-01-01 00:02:00'),
  # must be full day time
  ymd_hms('2022-12-31 23:52:00')))



sb2


View(sb2)



# Assuming 'df' is your dataset and 'date_to_remove' contains the specific date you want to remove
date_to_remove_2022 <- c("2022-01-09","2022-02-16","2022-02-20","2022-02-17","2022-02-21","2022-02-06","2022-02-01","2022-02-18","2022-02-27",
                         "2022-03-03","2022-03-30","2022-04-07","2022-04-08","2022-04-12","2022-05-06",
                         "2022-05-12","2022-05-14","2022-05-18","2022-05-20","2022-05-26","2022-06-19",
                         "2022-06-02","2022-06-20","2022-06-30","2022-07-01","2022-07-04","2022-07-05","2022-07-07",
                         "2022-07-30","2022-07-31","2022-08-22","2022-08-24","2022-08-26","2022-08-28",
                         "2022-09-06","2022-09-12","2022-09-16","2022-09-25","2022-11-14","2022-11-15",
                         "2022-10-10","2022-10-12","2022-10-18","2022-10-16","2022-10-22",
                         "2022-03-22",
                         "2022-12-05", "2022-05-17","2022-05-18","2022-05-22","2022-05-23","2022-09-08","2022-09-17") 

                    
# Convert 'DateTime' column to Date format
sb2$Date <- as.Date(sb2$Time)

# Remove rows corresponding to the specified date
df_2022 <- sb2[!sb2$Date %in% as.Date(date_to_remove_2022), ]

# Remove the 'Date' column if no longer needed
df_2022 <- subset(df_2022, select = -Date)

# Print the updated dataframe
print(df_2022)


write.csv(df_2022,"controlled_2022_u.csv")



df7<-df_2022 %>% filter(between(
  Time,
  ymd_hms('2022-12-01 00:02:00'),
  # must be full day time
  ymd_hms('2022-12-31 23:52:00')))


df7

j<-ggplot(df_2022, aes(Time,No3))+geom_line()+ylim(c(4,10))+scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')

j


l3<-ggplot(df_2022, aes(Time,No3))+geom_line()+ylim(c(4,10))+
  scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')+
  labs(x = '2022', y= "No3_after(mg/l)")
l3



l4<-ggplot(sb2, aes(Time,No3))+geom_line()+ylim(c(4,12))+
  scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')+
  labs(x = '2022', y= "No3_before(mg/l)")
l4



graph_No3_2022 <- grid.arrange(l4, l3, ncol = 1)

graph_No3_2022

ggsave(filename = 'C:/Users/shawon/Desktop/Thesis/graph_No3_2022.jpeg',
       plot = graph_No3_2022,
       height = 12,
       width = 20,
       dpi = 300
)


#####################################


sb3<-d %>% filter(between(
  Time,
  ymd_hms('2023-01-01 00:02:00'),
  # must be full day time
  ymd_hms('2023-09-09 23:52:00')))



sb3



# Assuming 'df' is your dataset and 'date_to_remove' contains the specific date you want to remove
date_to_remove <- c("2023-01-05","2023-01-08","2023-01-09","2023-01-11","2023-01-15","2023-01-14","2023-01-31",
                    "2023-02-19","2023-02-01","2023-02-03","2023-02-18","2023-03-19","2023-03-26",
                    "2023-03-09", "2023-03-10","2023-03-11","2023-03-12","2023-03-13","2023-03-14",
                    "2023-04-12", "2023-05-24","2023-06-22","2023-06-23","2023-06-20","2023-06-24",
                    "2023-07-24","2023-08-01","2023-08-15","2023-08-16","2023-08-17","2023-08-25","2023-08-30","2023-08-31",
                    "2023-09-09","2023-02-08"
                    
                    
                    
                    ) # Example date to remove

# Convert 'DateTime' column to Date format
sb3$Date <- as.Date(sb3$Time)

# Remove rows corresponding to the specified date
df3 <- sb3[!sb3$Date %in% as.Date(date_to_remove), ]

# Remove the 'Date' column if no longer needed
df3 <- subset(df3, select = -Date)

# Print the updated dataframe
print(df3)


write.csv(df3,"controlled_2023.csv")

df9<-df3 %>% filter(between(
  Time,
  ymd_hms('2023-02-01 00:02:00'),
  # must be full day time
  ymd_hms('2023-02-28 23:52:00')))


df9

ggplot(df9, aes(Time,Q))+geom_line()+ylim(c(0,9))+scale_x_datetime(date_breaks = '1 day', date_labels = '%d-%b')





ggplot(df3, aes(Time,Q))+geom_line()+ylim(c(0,8))+scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')


l5<-ggplot(df3, aes(Time,Q))+geom_line()+ylim(c(0,8))+
  scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')+
  labs(x = '2023', y= "Q_after(l/s)")
l5



l6<-ggplot(sb3, aes(Time,Q))+geom_line()+ylim(c(0,8))+
  scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')+
  labs(x = '2023', y= "Q_before(l/s)")
l6



graph_Q_2023 <- grid.arrange(l5, l6, ncol = 1)

graph_Q_2023

ggsave(filename = 'C:/Users/shawon/Desktop/Thesis/graph_Q_2023.jpeg',
       plot = graph_Q_2023,
       height = 12,
       width = 20,
       dpi = 300
)





#j<-ggplot(df)+geom_line(aes(Time,No3),color="blue")+geom_line(aes(Time,Q),color="red")+
#labs(x = '2021', y= "Q and No3")+
#ylim(c(0,12))+scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')

#j

#############################################################################

#########2020#########


d2020 <- read_csv("C:/Users/shawon/Desktop/data/T1/final datasets/2020 to 2023.10.14.csv")
View(d2020)


sb4<-d2020 %>% filter(between(
  Time,
  ymd_hms('2020-01-01 00:02:00'),
  # must be full day time
  ymd_hms('2020-12-31 23:52:00')))

#sb1<-d %>% filter(between(
#  Time,
# ymd_hms('2021-03-07 00:02:00'),
# must be full day time
# ymd_hms('2021-12-22 23:52:00')))

sb4
View(sb4)



# Assuming 'df' is your dataset and 'date_to_remove' contains the specific date you want to remove
date_to_remove <- c("2020-01-14","2020-01-15","2020-02-01","2020-02-02","2020-02-29","2020-02-03","2020-02-04","2020-02-09","2020-02-10","2020-02-11","2020-02-23","2020-02-24",
                    "2020-03-06","2020-03-23","2020-03-24","2020-03-25","2020-03-26","2020-03-31","2020-03-27","2020-03-28","2020-03-29","2020-03-30","2020-03-11","2020-03-10","2020-03-12","2020-03-13","2020-04-06","2020-04-05","2020-04-01","2020-04-02","2020-04-03","2020-04-04",
                    "2020-05-03","2020-05-04","2020-05-05","2020-05-06","2020-05-07","2020-05-08","2020-05-09","2020-05-10","2020-05-11","2020-05-12","2020-06-08",
                    "2020-06-05", "2020-06-01", "2020-06-02", "2020-06-03", "2020-06-04", "2020-06-06", "2020-06-07","2020-06-13","2020-06-14","2020-07-15","2020-07-26","2020-07-31","2020-08-02","2020-08-15","2020-08-16","2020-08-17","2020-08-18",
                    "2020-10-12","2020-10-13","2020-08-24","2020-08-07","2020-08-09","2020-08-10", "2020-08-05","2020-08-06","2020-08-08","2020-07-08", "2020-09-02","2020-09-01","2020-09-03","2020-09-24","2020-09-25","2020-09-26","2020-10-14","2020-10-15","2020-10-23",
                    "2020-12-22","2020-12-23","2020-12-28"
                    
                    ) # Example date to remove

# Convert 'DateTime' column to Date format
sb4$Date <- as.Date(sb4$Time)

# Remove rows corresponding to the specified date
df11 <- sb4[!sb4$Date %in% as.Date(date_to_remove), ]

# Remove the 'Date' column if no longer needed
df11 <- subset(df11, select = -Date)

# Print the updated dataframe
print(df11)


write.csv(df11,"controlled_2020.csv")

df12<-df11 %>% filter(between(
  Time,
  ymd_hms('2020-10-01 00:02:00'),
  # must be full day time
  ymd_hms('2020-10-31 23:52:00')))


df12



ggplot(df12, aes(Time,No3))+geom_line()+ylim(c(5,10))+scale_x_datetime(date_breaks = '1 day', date_labels = '%d-%b')





ggplot(df11, aes(Time,No3))+geom_line()+ylim(c(5,10))+scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')











l8<-ggplot(sb4, aes(Time,No3))+geom_line()+
  scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')+
  labs(x = '2020', y= "No3_after(mg/l)")
l8



l9<-ggplot(df11, aes(Time,No3))+geom_line()+ylim(c( 3,12))+
  scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')+
  labs(x = '2020', y= "No3_before(mg/l)")
l9


graph_No3_2020 <- grid.arrange(l8, l9, ncol = 1)


graph_No3_2020

# Export the plot as a JPEG file
ggsave(filename = 'C:/Users/shawon/Desktop/Thesis/graph_Q_2021.jpeg',
       plot = graph_Q_2021,
       height = 12,
       width = 20,
       dpi = 300
)



#j<-ggplot(df)+geom_line(aes(Time,No3),color="blue")+geom_line(aes(Time,Q),color="red")+
#labs(x = '2021', y= "Q and No3")+
#ylim(c(0,12))+scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')

#j


#############################################################################

#########2018#########


d2019 <- read_csv("sb_data_2011 to 2019.csv")
View(d2019)
d2019

sb5<-d2019 %>% rename("Time"="original_sensor_timestamp","No3"="NO3") %>% filter(between(
  Time,
  ymd_hms('2018-01-01 00:02:00'),
  # must be full day time
  ymd_hms('2018-12-31 23:52:00')))

sb5

View(sb5)



# Assuming 'df' is your dataset and 'date_to_remove' contains the specific date you want to remove
date_to_remove <- c("2018-12-09","2018-10-13","2018-10-06","2018-12-10","2018-12-23","2018-12-24","2018-12-29","2018-01-31","2018-01-01","2018-03-18","2018-03-19","2018-01-02","2018-01-23","2018-01-25","2018-01-03","2018-01-16","2018-01-26","2018-03-06","2018-03-07","2018-03-08","2018-03-17","2018-04-22","2018-04-21","2018-04-20","2018-04-04","2018-04-05",
                    "2018-05-13", "2018-05-29","2018-05-31","2018-06-04","2018-06-05", "2018-06-07","2018-07-28", "2018-08-09","2018-09-06","2018-09-09","2018-09-13", "2018-09-17","2018-03-31","2018-04-29","2018-04-12","2018-04-11","2018-05-25","2018-05-24","2018-05-23","2018-05-21","2018-05-22","2018-05-30","2018-07-02","2018-07-03", "2018-07-01","2018-08-23","2018-08-13", "2018-09-18","2018-05-15","2018-12-22","2018-12-21","2018-12-04","2018-12-03","2018-12-02","2018-11-13","2018-09-23","2018-07-22","2018-06-01","2018-06-09","2018-06-21","2018-06-20","2018-06-19","2018-06-18","2018-06-17","2018-06-16","2018-06-15","2018-06-14","2018-06-13","2018-06-12","2018-06-11","2018-06-10","2018-06-21"
                    
) # Example date to remove

# Convert 'DateTime' column to Date format
sb5$Date <- as.Date(sb5$Time)

# Remove rows corresponding to the specified date
df13 <- sb5[!sb5$Date %in% as.Date(date_to_remove), ]

# Remove the 'Date' column if no longer needed
df13 <- subset(df13, select = -Date)

# Print the updated dataframe
print(df13)


write.csv(df13,"controlled_2018.csv")

df14<-df13 %>% filter(between(
  Time,
  ymd_hms('2018-05-01 00:02:00'),
  # must be full day time
  ymd_hms('2018-05-31 23:52:00')))


df14



ggplot(df14, aes(Time,No3))+geom_line()+ylim(c(7,12))+scale_x_datetime(date_breaks = '1 day', date_labels = '%d-%b')





ggplot(df13, aes(Time,No3))+geom_line()+ylim(c(8,12))+scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')











l8<-ggplot(sb4, aes(Time,No3))+geom_line()+
  scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')+
  labs(x = '2020', y= "No3_after(mg/l)")
l8



l9<-ggplot(df11, aes(Time,No3))+geom_line()+ylim(c( 3,12))+
  scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')+
  labs(x = '2020', y= "No3_before(mg/l)")
l9


graph_No3_2020 <- grid.arrange(l8, l9, ncol = 1)


graph_No3_2020

# Export the plot as a JPEG file
ggsave(filename = 'C:/Users/shawon/Desktop/Thesis/graph_Q_2021.jpeg',
       plot = graph_Q_2021,
       height = 12,
       width = 20,
       dpi = 300
)


#####################################################################
#########2017#########


d2019 <- read_csv("sb_data_2011 to 2019.csv")
View(d2019)
d2019

sb6<-d2019 %>% rename("Time"="original_sensor_timestamp","No3"="NO3") %>% filter(between(
  Time,
  ymd_hms('2017-02-14 00:02:00'),
  # must be full day time
  ymd_hms('2017-07-15 23:52:00')))

sb6

View(sb6)



# Assuming 'df' is your dataset and 'date_to_remove' contains the specific date you want to remove
date_to_remove <- c("2017-02-03","2017-02-24","2017-02-23","2017-02-22","2017-02-21","2017-02-20","2017-02-15","2017-02-16","2017-02-04",
                    "2017-07-19",  "2017-07-12",  "2017-06-27","2017-06-26", "2017-06-25",  "2017-06-24", "2017-06-23", "2017-06-22", "2017-06-21","2017-06-20", "2017-06-19", "2017-06-18", "2017-06-17","2017-06-04","2017-06-15","2017-06-16", "2017-05-30","2017-05-02", "2017-05-20","2017-05-19","2017-05-13", "2017-05-03", "2017-04-12", "2017-03-20", "2017-03-19","2017-03-08","2017-03-09"
  
  
) # Example date to remove

# Convert 'DateTime' column to Date format
sb6$Date <- as.Date(sb6$Time)

# Remove rows corresponding to the specified date
df14 <- sb6[!sb6$Date %in% as.Date(date_to_remove), ]

# Remove the 'Date' column if no longer needed
df14 <- subset(df14, select = -Date)

# Print the updated dataframe
print(df14)


write.csv(df14,"controlled_u2017.csv")

df15<-df14 %>% filter(between(
  Time,
  ymd_hms('2017-05-01 00:02:00'),
  # must be full day time
  ymd_hms('2017-05-31 23:52:00')))


df15



ggplot(df15, aes(Time,Q))+geom_line()+ylim(c(3,8))+scale_x_datetime(date_breaks = '1 day', date_labels = '%d-%b')





ggplot(df14, aes(Time,No3))+geom_line()+ylim(c(7,12))+scale_x_datetime(date_breaks = '1 month', date_labels = '%d-%b')


##########################################################


a<-read_csv("controlled_u2017.csv")
b<-read_csv("controlled_2018.csv")
c<-read_csv("controlled_2020.csv")


a1<- a%>%select(Time,No3,Q) 
b1<-b %>% select(Time,No3,Q)
c1<-c %>% select(Time,No3,Q)

myd<-bind_rows(a1,b1,c1,d51)

d<-read_csv("C:/Users/shawon/Desktop/data/T1/final datasets/2020 to 2023.10.14.csv")

d51<-d %>% select(Time,No3,Q) %>% filter(between(
  Time,
  ymd_hms('2021-01-01 00:02:00'),
  # must be full day time
  ymd_hms('2023-10-14 23:52:00')))


d51


write.csv(myd,"mydata2.csv")
