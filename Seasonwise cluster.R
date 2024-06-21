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
Q <- read_csv("Data/mydata2.csv")
View(Q)

Q$Date <- as.Date(Q$Time)

# Extract month from Date
Q$Month <- as.integer(format(Q$Time, "%m"))

##Winter months
months_to_keep <- c(12,1,2)  # keep December, January, February

# Filter the data for the specific months
Winter_Q<- Q[Q$Month %in% months_to_keep, ]

Winter_Q<- Winter_Q %>% select(2,3,4) 

Winter_Q$Datetime <- as.POSIXct(Winter_Q$Time, "UTC", "%m/%d/%Y %H:%M")

Winter_Q$Date <- as.Date(Winter_Q$Time)

Winter_Q<-Winter_Q %>% select(4,5,3) 

########################

N_flux <- read_csv("Data/mydata2.csv")
N_flux
N_flux <- N_flux %>% select(Time,No3)
View(N_flux)

N_flux<- N_flux %>% filter(between(
  Time,
  ymd_hms('2017-02-17 00:02:00'),
  # must be full day time
  ymd_hms('2023-10-13 23:52:00')))


sum(is.na(N_flux$No3))

N_flux$Time <- as.POSIXct(N_flux$Time, "UTC", "%Y-%m-%d %H:%M")

N_flux$Date <- as.Date(N_flux$Time)

# Extract month from Date
N_flux$Month <- as.integer(format(N_flux$Time, "%m"))

##Winter months
months_to_keep1 <- c(12,1,2)  # keep December, January, February

# Filter the data for the specific months
Winter_NO3<- N_flux[N_flux$Month %in% months_to_keep1, ]

Winter_NO3

Winter_NO3$Datehour <- strftime(Winter_NO3$Time, "%F %H", "UTC")
Winter_NO3
class(Winter_NO3$Datehour)

###taging the existing time frame
N_fluxd_hourly <- aggregate(Winter_NO3$No3, list(Winter_NO3$Datehour), mean, na.rm = TRUE)

N_fluxd_hourly

###x is the default result created by aggregate

#trace(aggregate, edit=T), to see what's going on inside a function

N_fluxd_hourly$Group.1 <- as.POSIXct(N_fluxd_hourly$Group.1, "UTC", "%Y-%m-%d %H")

N_fluxd_hourly$Date <- as.Date(N_fluxd_hourly$Group.1)

# Assuming N_fluxd_hourly is your aggregated dataset
#N_fluxd_hourly <- na.omit(N_fluxd_hourly)

#sum(is.na(N_fluxd_hourly$x))

View(N_fluxd_hourly)
# Create a matrix without NaN values
# Assuming N_fluxd_hourly is your aggregated dataset
# Assuming N_fluxd_hourly is your aggregated dataset
N_fluxd <- matrix(N_fluxd_hourly$x[1: 9456 ], ncol = 24, byrow = TRUE)
View(N_fluxd)
# Create a data frame with Date and matrix columns
N_fluxd <- data.frame(Date = unique(N_fluxd_hourly$Date[1: 9456]),
                      as.data.frame(N_fluxd))

View(N_fluxd)

##calculate relative N_flux and keep the sign
N_mean_ab <- abs(as.vector(apply(N_fluxd[,-1], 1, mean)))
N_fluxd_re <- N_fluxd
N_fluxd_re[,-1] <- N_fluxd_re[,-1] / N_mean_ab


##calculate residuals of N_flux
N_mean <- abs(as.vector(apply(N_fluxd[,-1], 1, mean)))
N_fluxd_resi <- N_fluxd
N_fluxd_resi[,-1] <- N_fluxd_resi[,-1] - N_mean


##calculate min-max normalized N_flux
N_fluxd_nor <- N_fluxd
N_fluxd_nor[,-1] <- t(apply(N_fluxd_nor[,-1], 1, function(x) normalize(x)))

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

Q_STF2<- Winter_Q
##summarize discharge in uptake days
uptake_q_daily <- summarize_q(Q_STF2, uptake_date)

uptake_q_daily

ggplot(uptake_q_daily) +
  geom_line(aes(date, max)) +
  geom_line(aes(date, min))

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


tmpyr$tk = ifelse(tmpyr$Y==2017 & tmpyr$dis<6.027535, "Y", tmpyr$tk)
#tmpyr$tk = ifelse(tmpyr$Y==2018 & tmpyr$dis<6.381505, "Y", tmpyr$tk)
tmpyr$tk = ifelse(tmpyr$Y==2020 & tmpyr$dis<7.047774, "Y", tmpyr$tk)
tmpyr$tk = ifelse(tmpyr$Y==2021 & tmpyr$dis<6.261479, "Y", tmpyr$tk)
tmpyr$tk = ifelse(tmpyr$Y==2022 & tmpyr$dis<4.989033, "Y", tmpyr$tk)
tmpyr$tk = ifelse(tmpyr$Y==2023 & tmpyr$dis<4.587149, "Y", tmpyr$tk)
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
#View(data_cluster)
FS3a <- fviz_nbclust(data_cluster[-1], kmeans, method = "silhouette", k.max = 10) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"),
        panel.grid = element_blank())

FS3a




##try k-mean cluster
n <- 2
uptake.km <- kmeans(data_cluster[,-1], centers = n, nstart = 25)
FS3b <- fviz_cluster(uptake.km, data = data_cluster[,-1]) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"),
        panel.grid = element_blank())


FS3b

ggsave(filename = 'C:/Users/shawon/Desktop/Thesis/cluster_graph_7.jpeg',
       plot = FS3b,
       height = 15,
       width = 15,
       dpi = 300
)

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

cluster.Nfluxd

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

# #plot cluster
# ##0.25
# cluster.Nfluxd_all$Type <- factor(cluster.Nfluxd_all$Type, levels = c("V1","V3","V4","V2"),
#                                   labels = c("C1","C2","C3","C4"))
# uptake.mid_melt$Type <- factor(uptake.mid_melt$Type, levels = c("V1","V3","V4","V2"),
#                                labels = c("C1","C2","C3","C4"))
# uptake.up_melt$Type <- factor(uptake.up_melt$Type, levels = c("V1","V3","V4","V2"),
#                               labels = c("C1","C2","C3","C4"))
# uptake.low_melt$Type <- factor(uptake.low_melt$Type, levels = c("V1","V3","V4","V2"),
#                                labels = c("C1","C2","C3","C4"))
# 
# ##0.5
# cluster.Nfluxd_all$Type <- factor(cluster.Nfluxd_all$Type, levels = c("V2","V3","V1","V4"),
#                                   labels = c("C1","C2","C3","C4"))
# uptake.mid_melt$Type <- factor(uptake.mid_melt$Type, levels = c("V2","V3","V1","V4"),
#                                labels = c("C1","C2","C3","C4"))
# uptake.up_melt$Type <- factor(uptake.up_melt$Type, levels = c("V2","V3","V1","V4"),
#                               labels = c("C1","C2","C3","C4"))
# uptake.low_melt$Type <- factor(uptake.low_melt$Type, levels = c("V2","V3","V1","V4"),
#                                labels = c("C1","C2","C3","C4"))

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
  ylab(expression("normalized"~Delta*"N_flux")) +
  xlab("Time (UTC)") +
  #scale_color_manual(name = "Campaigns", values = c("#FD3F3F", "#00BA38","#0B5CF4","#AB81CD")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 8), axis.title = element_text(size = 9),
        plot.margin = margin(3,11,3,5), panel.grid = element_blank())
cluster

ggsave(filename = 'C:/Users/shawon/Desktop/Thesis/cluster_graph_11.jpeg',
       plot = cluster,
       height = 15,
       width = 15,
       dpi = 300
)






#################################################################################################

Q_STF2
N_fluxd_15min2
##extract uptake day in 15 min and plot with uptake days------------------------
N_uptake <- N_fluxd_15min[which(N_fluxd_15min2$Date %in% uptake_dates_new),]
Q_uptake <- Q_STF[which(Q_STF$Date %in% uptake_dates_new),]

date_rect = data_cluster_new[,c(1,26)]
dry_uptake_time <- data.frame(date_rect, start = rep("00:00"), end = rep("23:45"))
dry_uptake_time$start <- as.POSIXct(paste0(dry_uptake_time$Date," ",dry_uptake_time$start), "UTC", "%Y-%m-%d %H:%M")
dry_uptake_time$end <- as.POSIXct(paste0(dry_uptake_time$Date," ",dry_uptake_time$end), "UTC", "%Y-%m-%d %H:%M")
dry_uptake_time$Type <- factor(dry_uptake_time$Type,levels = c("C1","C2","C3","C4")) #messy


#daily flux
tmp = N_fluxd_hourly
tmp$x = tmp$x * 3600*0.001 # kg/h
tmpd = as.data.frame(aggregate(tmp$x,list(tmp$Date),sum)) # kg/d
N_fluxd_daily = tmpd
names(N_fluxd_daily) = c("Date","Uptk_daily")
N_fluxd_daily$Datetime = as.POSIXct(paste0(N_fluxd_daily$Date," 0:00"),tz = "UTC")

ylim.n <- c(floor(min(N_fluxd_daily$Uptk_daily, na.rm = T)), ceiling(max(N_fluxd_daily$Uptk_daily, na.rm = T)))
ylim.q <- c(0, ceiling(max(Q_STF$Q)))
k <- diff(ylim.q)/diff(ylim.n) * 0.8
ymax <- 3*ceiling(max(N_fluxd_daily$Uptk_daily, na.rm = T))
ymin <- floor(min(N_fluxd_daily$Uptk_daily, na.rm = T))




# Check for missing values and handle them appropriately
N_fluxd_daily_clean <- na.omit(N_fluxd_daily)
Q_STF_clean <- na.omit(Q_STF)

# Calculate ymin and ymax based on cleaned datasets
ymax <- 3 * ceiling(max(N_fluxd_daily_clean$Uptk_daily))
ymin <- floor(min(N_fluxd_daily_clean$Uptk_daily))

# Calculate k based on cleaned Q_STF dataset
k <- diff(c(0, ymax)) / diff(range(Q_STF_clean$Q)) * 0.8

# Create the plot
N_fluxd_plot <- ggplot() +
  geom_rect(data = dry_uptake_time, 
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = Type), 
            color = NA) +
  geom_line(data = N_fluxd_daily_clean, aes(Datetime, Uptk_daily), size = 0.3) +
  geom_line(data = Q_STF_clean, aes(Datetime, -Q / k + ymax), size = 0.3) +
  scale_y_continuous(breaks = c(-1000, -500, 0, 500, 1000), limits = c(ymin, ymax), expand = c(0, 0),
                     sec.axis = sec_axis(~((.-ymax) * (-k)), 
                                         name = expression("Q (" * m^3 * s^{-1} * ")"),
                                         breaks = c(0, 10, 20, 30, 40, 50))) +
  scale_x_datetime(expand = c(0, 0), date_breaks = "6 months", date_labels = "%Y-%m") +
  scale_fill_manual(name = "Cluster", values = c("aquamarine", "dodgerblue4", "bisque", "coral")) +
  guides(color = "none") +
  xlab("Date") +
  ylab(expression(Delta * "N [" * kg * d^{-1} * "]")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"), 
        legend.text = element_text(size = 9), 
        legend.title = element_text(size = 10),
        panel.grid = element_blank())

# Print the plot
print(N_fluxd_plot)







#--selected dates locs all together plot-------------
N_fluxd_plot <- ggplot() +
  geom_rect(data = dry_uptake_time, 
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill=Type), 
            color = NA) + #"light gray"
  geom_line(data = N_fluxd_daily, aes(Datetime, Uptk_daily), size=0.3)+
  geom_line(data = Q_STF, aes(Datetime, -Q/k+ymax), size=0.3) +
  scale_y_continuous(breaks = c(-1000,-500,0,500,1000), limits = c(ymin,ymax), expand = c(0,0),
                     sec.axis = sec_axis(~((.-ymax)*(-k)), 
                                         name = c(expression("Q ("~m^3~s^{-1}~")")),
                                         breaks = c(0,10,20,30,40,50))) +
  scale_x_datetime(expand = c(0,0), date_breaks = "6 months", date_labels = "%Y-%m") +
  scale_fill_manual(name = "Cluster",
                    values = c("aquamarine","dodgerblue4","bisque","coral")) +
  guides(color = "none") +
  xlab("Date") +
  ylab(c(expression(Delta*"N ["~kg~d^-1~"]"))) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"), 
        legend.text = element_text(size=9), legend.title = element_text(size=10),
        panel.grid = element_blank())
#legend.position = c(0.75,0.5), legend.background = element_blank(),
#legend.box = "horizontal", legend.margin = margin(0,-5,0,0))
N_fluxd_plot

