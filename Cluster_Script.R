library(corrplot)

data <- matrix(c(
  1.000, -0.02,  0.03, -0.01, -0.03, -0.11,
  -0.02,  0.600,  0.03, -0.01, -0.03, -0.11,
  0.03,   0.160, -0.01, -0.01, -0.04,  0.09,
  -0.01,  0.150, -0.15, -0.01, -0.08,  0.04,
  -0.03,  0.400, -0.16,  0.32, -0.46,  0.39,
  -0.11,  0.050,  0.11,  0.35,  0.04, -0.14
), nrow = 6, byrow = TRUE)

# Create correlation matrix
correlation_matrix <- cor(data)


# Create correlogram with correlation values and significance indicators
corrplot(correlation_matrix, method = "square", type = "full", order = "original",
         addCoef.col = "black",bg = "white" )


########################################################
###########################################################
############################################################ 



library(factoextra)
library(cluster)
library(reshape2)
library(lubridate)
library(tictoc)
library(PerformanceAnalytics)
library(plyr)
library(multcompView)
library(RColorBrewer)
library(wesanderson)
library(cowplot)
library(corrplot)
library(data.table)
library(dplyr)
library(mgcv)
library(ggplot2)
library(viridis)
library(hydroGOF)

library(readr)
library(tidyverse)
#functions-----
summarize_q <- function(q, date_subset){
  q_subset <- q[which(q$Date %in% date_subset),]
  q_subset_daily <- data.frame(date=date_subset,
                               mean=aggregate(q_subset$Q, list(q_subset$Date), mean)$x,
                               min=aggregate(q_subset$Q, list(q_subset$Date), min)$x,
                               max=aggregate(q_subset$Q, list(q_subset$Date), max)$x,
                               sd=aggregate(q_subset$Q, list(q_subset$Date), sd)$x)
  q_subset_daily$cv <- q_subset_daily$sd / q_subset_daily$mean
  q_subset_daily$range <- q_subset_daily$max - q_subset_daily$min
  q_subset_daily$permin <- q_subset_daily$range / q_subset_daily$min
  q_subset_daily$permean <- q_subset_daily$range / q_subset_daily$mean
  
  return(q_subset_daily)
}

normalize <- function(data_raw) {
  data_nor <- (data_raw-min(data_raw, na.rm = T))/
    (max(data_raw, na.rm = T)-min(data_raw, na.rm = T))
  return(data_nor)
}

which.out <- function(x){
  # x: numeric vector
  # r: numeric value. Used to calculate the limits for outliers. Limits are the 25% & 75% quantiles -/+ r times interquartile range.
  x <- na.exclude(x)
  qs <- quantile(x, probs = c(0.25, 0.75))
  iqr <- diff(qs)
  outliers <- which(x < (qs[1] - 1.5 * iqr) 
                    | x > (qs[2] + 1.5 * iqr))
  if (length(outliers) == 0) {
    out <- NULL
    normal <- x
  } else {
    out <- x[outliers]
    normal <- x[-outliers]
  }
  results <- list(out, normal)
  return(normal)
}

remove.out <- function(x) {
  range <- quantile(x, c(0.05,0.95), na.rm = T)
  index <- which(x <= range[2] & x >= range[1])
  result <- x[index]
  return(result)
}



#raw input----------------------------------------------------------------------
Q_STF2 <- read_csv("Data/mydata2.csv")

Q_STF1<- Q_STF2 %>% select(2,3,4) 

Q_STF1$Datetime <- as.POSIXct(Q_STF1$Time, "UTC", "%m/%d/%Y %H:%M")

Q_STF1$Date <- as.Date(Q_STF1$Time)

Q_STF1
Q_STF2<-Q_STF1 %>% select(4,5,3) 

Q_STF2 

#write.csv(Q_STF2,"Q_STF2.csv")

N_fluxd_15min <- read_csv("Data/mydata2.csv")
N_fluxd_15min
N_fluxd_15min1 <- N_fluxd_15min %>% select(Time,No3,Q)
View(N_fluxd_15min1)

N_fluxd_15min3<- N_fluxd_15min1 %>% filter(between(
  Time,
  ymd_hms('2017-02-17 00:02:00'),
  # must be full day time
  ymd_hms('2023-10-13 23:52:00'))) %>% mutate(No3=(No3*Q)/(1000))

N_fluxd_15min2<-N_fluxd_15min3 %>% select(Time,No3)

N_fluxd_15min2

sum(is.na(N_fluxd_15min2$No3))

N_fluxd_15min2$Time <- as.POSIXct(N_fluxd_15min2$Time, "UTC", "%Y-%m-%d %H:%M")

N_fluxd_15min2$Date <- as.Date(N_fluxd_15min2$Time)

N_fluxd_15min2$Datehour <- strftime(N_fluxd_15min2$Time, "%F %H", "UTC")
N_fluxd_15min2
class(N_fluxd_15min2$Datehour)

#N_fluxd_15min2<- N_fluxd_15min1 %>% drop_na() %>%mutate( No3 = as.numeric(No3))

#N_fluxd_15min2

###taging the existing time frame
N_fluxd_hourly <- aggregate(N_fluxd_15min2$No3, list(N_fluxd_15min2$Datehour), mean, na.rm = TRUE)

#View(N_fluxd_hourly)

###x is the default result created by aggregate

#trace(aggregate, edit=T), to see what's going on inside a function

N_fluxd_hourly$Group.1 <- as.POSIXct(N_fluxd_hourly$Group.1, "UTC", "%Y-%m-%d %H")

N_fluxd_hourly$Date <- as.Date(N_fluxd_hourly$Group.1)

View(N_fluxd_hourly)

# Assuming N_fluxd_hourly is your aggregated dataset
#N_fluxd_hourly <- na.omit(N_fluxd_hourly)

#sum(is.na(N_fluxd_hourly$x))
# Create a matrix without NaN values
# Assuming N_fluxd_hourly is your aggregated dataset
# Assuming N_fluxd_hourly is your aggregated dataset
N_fluxd <- matrix(N_fluxd_hourly$x[1:40942 ], ncol = 24, byrow = TRUE)
View(N_fluxd)
# Create a data frame with Date and matrix columns
N_fluxd <- data.frame(Date = unique(N_fluxd_hourly$Date[1:40942]),
                      as.data.frame(N_fluxd))

View(N_fluxd)

##calculate relative N_flux and keep the sign
N_mean_ab <- abs(as.vector(apply(N_fluxd[,-1], 1, mean)))
N_fluxd_re <- N_fluxd
N_fluxd_re[,-1] <- N_fluxd_re[,-1] / N_mean_ab
View(N_fluxd_re[,-1])

##calculate residuals of N_flux
N_mean <- abs(as.vector(apply(N_fluxd[,-1], 1, mean)))
N_fluxd_resi <- N_fluxd
N_fluxd_resi[,-1] <- N_fluxd_resi[,-1] - N_mean
View(N_fluxd_resi[,-1])

##calculate min-max normalized N_flux
N_fluxd_nor <- N_fluxd
N_fluxd_nor[,-1] <- t(apply(N_fluxd_nor[,-1], 1, function(x) normalize(x)))

View(N_fluxd_nor[,-1])


#choose days for analysis-------------------------------------------------------
##days with uptake (positive N_fluxd)-------------------------------------------
check <- as.data.frame(apply(N_fluxd[,-1], 2, function(x) ifelse(x > 0 | x ==0, 1, 0)))
check$all <- apply(check, 1, sum)
uptake_raw <- N_fluxd[which(check$all == 24),]
View(uptake_raw)

uptake_raw_re <- N_fluxd_re[which(check$all == 24),]

uptake_raw_nor <- N_fluxd_nor[which(check$all == 24),]
uptake_raw_resi <- N_fluxd_resi[which(check$all == 24),]
uptake_date <- uptake_raw$Date

View(uptake_date)
#Q_STF2<-Q_STF2 %>% select(2,1,3)

#Q_STF2<-Q_STF2 %>% mutate(Q= (Q/1000))
##summarize discharge in uptake days
uptake_q_daily <- summarize_q(Q_STF2, uptake_date)

uptake_q_daily

ggplot(uptake_q_daily) +
  geom_line(aes(date, max)) +
  geom_line(aes(date, min))+
  ylim(0,20)

##choose data according to q conditions 
hist(uptake_q_daily$range)
quantile(uptake_q_daily$cv, probs = c( 0.0,0.1,0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.75, 0.8, 0.9, 1.0), na.rm=TRUE)


quantile(uptake_q_daily$mean, probs = c(0.0,0.1,0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.75, 0.8, 0.9, 1.0), na.rm=TRUE)
tmpyr = as.data.frame(aggregate(Q_STF2$Q,list(Q_STF2$Date),mean, na.rm = T))
tmpyr

names(tmpyr) = c("Date","dis")
tmpyr
tmpyr$Y = year(tmpyr$Date)
tmpyr
aggregate(tmpyr$dis,list(tmpyr$Y), quantile,probs=c(0.25,0.5,0.75, 1.00))


tmpyr$tk = "n"
tmpyr


tmpyr$tk = ifelse(tmpyr$Y==2017 & tmpyr$dis<5.893958,"Y", tmpyr$tk)
#tmpyr$tk = ifelse(tmpyr$Y==2018 & tmpyr$dis<6.381505,"Y", tmpyr$tk)
tmpyr$tk = ifelse(tmpyr$Y==2020 & tmpyr$dis<6.748062,"Y", tmpyr$tk)
tmpyr$tk = ifelse(tmpyr$Y==2021 & tmpyr$dis<5.913983,"Y", tmpyr$tk)
tmpyr$tk = ifelse(tmpyr$Y==2022 & tmpyr$dis<4.805610,"Y", tmpyr$tk)
tmpyr$tk = ifelse(tmpyr$Y==2023 & tmpyr$dis<4.832497,"Y", tmpyr$tk)
#tmpyr$tk = ifelse(tmpyr$Y==2018 & tmpyr$dis<2.8, "Y", tmpyr$tk)
#tmpyr$tk = ifelse(tmpyr$Y==2019 & tmpyr$dis<2.4, "Y", tmpyr$tk)
#tmpyr$tk = ifelse(tmpyr$Y==2020 & tmpyr$dis<2.2, "Y", tmpyr$tk)
lowflowdate = subset(tmpyr, tk=="Y")$Date

lowflowdate

###choose within uptake days
dry_uptake_date <- uptake_q_daily[which(uptake_q_daily$cv < quantile(uptake_q_daily$cv, 0.5, na.rm=TRUE)),] 
#| uptake_q_daily$mean < 3.5),] 
dry_uptake <- uptake_raw_nor[which(uptake_raw_nor$Date %in% dry_uptake_date$date),]

lowf_uptake <- dry_uptake[which(dry_uptake$Date %in% lowflowdate),]
uptake_dates_new = lowf_uptake$Date

uptake_dates_new

#k-means cluster----------------------------------------------------------------
##Find the Optimal Number of Clusters
###create a plot of the number of clusters vs. the total within sum of squares
# Install and load the 'factoextra' package if not already installed
data_cluster <- lowf_uptake

data_cluster

rows_delete <- c(125,136,141,209,212,231,236,246,258,261,267,292,308,319,328,348,352,354,362,363,
                     371,375,384,388,389,397,403)

data_cluster <- data_cluster[-rows_delete, ]

data_cluster

write.csv(data_cluster,"M9.csv")

#View(data_cluster)
FS3a <- fviz_nbclust(data_cluster[-1], kmeans, method = "wss", k.max = 10) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"),
        panel.grid = element_blank())+
  theme(
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 14),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )


FS3a

ggsave("M81.jpg", FS3a, width = 10, height = 7, dpi = 300)

##try k-mean cluster
n <- 3
uptake.km <- kmeans(data_cluster[,-1], centers = n, nstart = 25)
FS3b <- fviz_cluster(uptake.km, data = data_cluster[,-1]) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"),
        panel.grid = element_blank())+
  theme(
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 14),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )



FS3b

ggsave("M71.jpg", FS3b, width = 10, height = 7, dpi = 300)

##deal with cluster
index <- list()
index.day <- list()
cluster.Nfluxd <- list()
cluster.Nfluxd_melt <- list()
for (i in 1:n) {
  index[[i]] <- as.vector(which(uptake.km$cluster == i)) 
  index.day[[i]] <- data.frame(Date = data_cluster$Date[index[[i]]],
                               Type = paste0("C",i))
  cluster.Nfluxd[[i]] <- as.data.frame(t(data_cluster[index[[i]],-1]))
}
cluster.Nfluxd_orgkm = cluster.Nfluxd

##improve cluster
##improve cluster
##for V1 --exclude when 24h is the max 
V1_max <- apply(cluster.Nfluxd[[1]], 2, function(x) {return(which(x==max(x)))})
#V1_24 <- as.numeric(cluster.Nfluxd[[1]][24,]) ###!!!!do this tomorrow!
index_V1 <- which(V1_max>=22)
#index_V1 <- which(V1_24 >= 0.9999999)
cluster.Nfluxd[[1]] <- cluster.Nfluxd[[1]][,-index_V1]
index.day[[1]] <- index.day[[1]][-index_V1,]

##for V2-exclue when 00h is the max
V2_max <- apply(cluster.Nfluxd[[2]], 2, function(x) {return(which(x==max(x)))})
index_V2 <- which(V2_max <=2)
cluster.Nfluxd[[2]] <- cluster.Nfluxd[[2]][,-index_V2]
index.day[[2]] <- index.day[[2]][-index_V2,]


for (i in 1:n) {
  cluster.Nfluxd[[i]]$Time <- seq(1:24)
  cluster.Nfluxd[[i]]$Type <- paste0("C",i)
  cluster.Nfluxd_melt[[i]] <- reshape2::melt(cluster.Nfluxd[[i]], id.var= c("Time", "Type"))
}

cluster.Nfluxd_all_new <- do.call(rbind.data.frame, cluster.Nfluxd_melt)

write.csv(cluster.Nfluxd_all_new,"M5.csv")

# #cluster.Nfluxd_all$Type <- as.factor(cluster.Nfluxd_all$Type)
index_date <- do.call(rbind.data.frame, index.day)
data_cluster_new <- data_cluster[which(data_cluster$Date %in% index_date$Date),]
data_cluster_new <- merge(data_cluster_new[,-26], index_date, by = "Date", all = T)

N_fluxd_diel = N_fluxd[which(N_fluxd$Date %in% index_date$Date),]
N_fluxd_diel <- merge(N_fluxd_diel, index_date, by = "Date", all = T)
# 
# index_date$Type = ifelse(index_date$Type=="V1", "C1", index_date$Type)
# index_date$Type = ifelse(index_date$Type=="V2", "C2", index_date$Type)
# index_date$Type= ifelse(index_date$Type=="V3", "C3", index_date$Type)
# index_date$Type = ifelse(index_date$Type=="V4", "C4", index_date$Type)


#calculate quantile of cluster for plot
quantile_cluster <- function(data4cluster, number){   #data4cluster
  uptake.quantile <- aggregate(data4cluster[,-c(1,ncol(data4cluster))], 
                               by=list(data4cluster$Type), 
                               function(x) quantile(x, number, na.rm = T))
  ncoln = uptake.quantile[,1]
  uptake.quantile <- as.data.frame(t(uptake.quantile))[-1,]
  names(uptake.quantile) = ncoln
  uptake.cluster <- data.frame(Time = seq(1:24), uptake.quantile)
  uptake.cluster_melt <- reshape2::melt(uptake.cluster, id.var="Time")
  colnames(uptake.cluster_melt) <- c("Time", "Type", "value")
  uptake.cluster_melt$value <- as.numeric(uptake.cluster_melt$value)
  results <- list(uptake.cluster, uptake.cluster_melt)
  return(results)
}

uptake.mid_melt <- quantile_cluster(data_cluster_new, 0.5)[[2]]
uptake.up_melt <- quantile_cluster(data_cluster_new, 0.9)[[2]]
uptake.low_melt <- quantile_cluster(data_cluster_new, 0.1)[[2]]


time_label <- c("00:00","06:00","12:00","18:00")

cluster <- ggplot() +
  geom_line(data=cluster.Nfluxd_all_new, aes(Time, value, group = as.factor(variable)),
            size = 0.3, alpha = 0.5, color="light gray") +
  geom_line(data=uptake.mid_melt, aes(Time, value), color="blue") +
  geom_line(data=uptake.up_melt, aes(Time, value), color="red", linetype="dashed") +
  geom_line(data=uptake.low_melt, aes(Time, value), color="red", linetype="dashed") +
  #ylab(expression("relative N flux")) +
  scale_x_continuous(breaks=c(1,6,12,18), expand = c(0,0), labels = time_label) +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~Type, nrow = 1, scales = "free") +
  ylab(expression("normalized Nflux")) +
  xlab("Time (UTC)") +
  #scale_color_manual(name = "Campaigns", values = c("#FD3F3F", "#00BA38","#0B5CF4","#AB81CD")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 8), axis.title = element_text(size = 9),
        plot.margin = margin(3,11,3,5), panel.grid = element_blank())
cluster

ggsave(filename = 'C:/Users/shawon/Desktop/Thesis/M4.jpeg',
       plot = cluster,
       height = 12,
       width = 15,
       dpi = 300
)





#################################################################################################


#geom_smoothing using gam
library(ggplot2)

# Assuming your data frame is named cluster.Nfluxd_all_new
cluster.Nfluxd_all_new$Date = as.Date("2017-02-17") +as.numeric(as.character(cluster.Nfluxd_all_new$variable))-1

# Generate time labels
time_label <- c("1", "6", "12", "18", "24")

# Filter data for only "Normal" climate
normal_data <- cluster.Nfluxd_all_new[year(cluster.Nfluxd_all_new$Date) > 2017, ]

cluster2 <- ggplot() +
  geom_line(data = normal_data, aes(Time, value, group = as.factor(variable)),
            size = 0.5, alpha = 0.5, color = "light gray") +
  geom_smooth(data = normal_data,
              aes(Time, value, color = Type, fill = Type),
              method = "gam") +
  scale_x_continuous(breaks = c(1, 6, 12, 18, 24), expand = c(0, 0), labels = time_label) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~Type, nrow = 1, scales = "free") +
  ylab(expression("Normalized Nitrate Uptake Values")) +
  xlab("Time (UTC)") +
  scale_color_manual(name = "Cluster", values = c("aquamarine4","dodgerblue4", "coral")) +
  scale_fill_manual(name = "Cluster", values = c("aquamarine4","dodgerblue4", "coral")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 14), axis.title = element_text(size = 14),
        plot.margin = margin(3, 11, 3, 5), panel.grid = element_blank())+
  theme(
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 14),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

  

cluster2
ggsave("M3.jpg", cluster2, width = 10, height = 6, dpi = 300)


##extract uptake day in 15 min and plot with uptake days------------------------
# Check for missing values and handle them appropriately
N_fluxd_daily <- na.omit(N_fluxd_daily)
Q_STF2 <- na.omit(Q_STF2)


N_uptake <- N_fluxd_15min[which(N_fluxd_15min$Time %in% uptake_dates_new),]

Q_uptake <- Q_STF2[which(Q_STF2$Date %in% uptake_dates_new),]

date_rect = data_cluster_new[,c(1,26)]
dry_uptake_time <- data.frame(date_rect, start = rep("00:00"), end = rep("23:45"))
dry_uptake_time$start <- as.POSIXct(paste0(dry_uptake_time$Date," ",dry_uptake_time$start), "UTC", "%Y-%m-%d %H:%M")
dry_uptake_time$end <- as.POSIXct(paste0(dry_uptake_time$Date," ",dry_uptake_time$end), "UTC", "%Y-%m-%d %H:%M")
dry_uptake_time$Type <- factor(dry_uptake_time$Type,levels = c("C1","C2","C3")) #messy


#daily flux
tmp = N_fluxd_hourly
tmp$x = tmp$x * 3600*0.001 # kg/h
tmpd = as.data.frame(aggregate(tmp$x,list(tmp$Date),sum)) # kg/d
N_fluxd_daily = tmpd
names(N_fluxd_daily) = c("Date","Uptk_daily")
N_fluxd_daily$Datetime = as.POSIXct(paste0(N_fluxd_daily$Date," 0:00"),tz = "UTC")

ylim.n <- c(floor(min(N_fluxd_daily$Uptk_daily, na.rm = T)), ceiling(max(N_fluxd_daily$Uptk_daily, na.rm = T)))
ylim.q <- c(0, ceiling(max(Q_STF2$Q)))

ylim.q

k <- diff(ylim.q)/diff(ylim.n) * 0.8
ymax <- 3*ceiling(max(N_fluxd_daily$Uptk_daily, na.rm = T))
ymin <- floor(min(N_fluxd_daily$Uptk_daily, na.rm = T))


# Assuming your data frames are named dry_uptake_time, N_fluxd_daily, and Q_STF2
start_date <- ymd('2020-01-01')
end_date <- ymd('2023-08-30')

# Filter the main dataset
dry_uptake_filtered <- dry_uptake_time %>% filter(between(Date, start_date, end_date))

# Filter the additional datasets to match the same date range
N_fluxd_daily_filtered <- N_fluxd_daily %>% filter(between(Datetime, start_date, end_date))
Q_STF2_filtered <- Q_STF2 %>% filter(between(Datetime, start_date, end_date))

N_fluxd_daily_filtered


N_fluxd_plot<-dry_uptake_time %>% filter(between(
  Date,
  ymd('2020-01-01'),
  ymd('2023-08-30'))) %>% ggplot() +
  geom_rect( 
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = Type), 
            color = NA) + 
  geom_line(data = N_fluxd_daily_filtered, aes(Datetime, Uptk_daily), size = 0.3) +
  geom_line(data = Q_STF2_filtered, aes(Datetime, -Q / k + ymax), size = 0.3) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50), 
                     limits = c(ymin, ymax), expand = c(0, 0),
                     sec.axis = sec_axis(~ ((. - ymax) * (-k)), 
                                         name = expression("Q (" *L* s^{-1} * ")"),
                                         breaks = c(0, 10, 20, 30, 40, 50))) +
  scale_x_datetime(expand = c(0, 0), date_breaks = "6 months", date_labels = "%Y-%m") +
  scale_fill_manual(name = "Cluster",
                    values = c("aquamarine4","dodgerblue4", "coral")) +
  guides(color = "none") +
  xlab("Date") +
  ylab(expression("Nitrate Flux" * '(' * NO[3] %*% "Q" * ')' ~ "[" * kg ~ d^{-1} * "]"))+
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 14),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )


N_fluxd_plot

ggsave("M2.jpg", N_fluxd_plot, width = 10, height = 6, dpi = 300)


####################################

count <- aggregate(dry_uptake_time$Type,
                   list(year(dry_uptake_time$Date),
                        month(dry_uptake_time$Date),
                        dry_uptake_time$Type),
                   length)
colnames(count) <- c("Year", "Month", "Type", "Count")
count$Date <- as.Date(paste0(count$Year,"-",count$Month,"-15"))

disdaily = as.data.frame(aggregate(Q_STF2$Q,list(Q_STF2$Date),mean,na.rm=T))
names(disdaily) = c("Date","Q")
disdaily$Year = year(disdaily$Date)
disdaily
# Filter out data for years other than 2018
count_filtered <- count %>%
  filter(!Year %in% c(2017, 2018)) %>% filter(between(
    Date,
    ymd('2020-01-01'),
    ymd('2023-08-30')))
count

disdaily_filtered <- disdaily %>%
  filter(!Year %in% c(2017, 2018)) %>% filter(between(
    Date,
    ymd('2020-01-01'),
    ymd('2023-08-30')))

disdaily_filtered

count_bar <- ggplot(count_filtered) + 
  geom_bar(aes(Date, Count, fill=Type), stat = "identity", alpha=0.8) +
  geom_line(data = disdaily_filtered, aes(Date, Q), color = "blue", size = 0.3, alpha = 0.7) +
  facet_wrap(~Year, ncol = 3, dir = "h", scales = "free_x") + 
  ylab(expression("Cluster Counts and " * Q ~ (L ~ s^-1))) +
  scale_x_date(name = "Month", expand = c(0, 0), date_labels = "%b", date_breaks = "1 month") +
  theme_bw() +
  scale_fill_manual(name = "Cluster",
                    values = c("aquamarine4","dodgerblue4", "coral"))+
  theme(
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(colour = "black", size = 12),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

count_bar

ggsave("M1.jpg", count_bar, width = 15, height = 8, dpi = 300)
