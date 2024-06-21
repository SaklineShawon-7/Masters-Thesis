library(tidyverse)

data<- read_csv('./2020 to 2023.10.14.csv')


library(dplyr)

D <- data %>%
  filter(between(
    Time,
    ymd_hms('2022-12-01 00:00:00'),
    ymd_hms('2023-02-28 00:00:00')
  )) %>%
  group_by(month = lubridate::month(Time)) %>%  # Group by month
  timetk::summarise_by_time(.date_var = Time, .by = 'month', wt = mean(wt, na.rm = TRUE))
D
# Calculate mean for each month
monthly_mean <- D %>%
  group_by(month) %>%
  summarise(mean_value = mean(wt, na.rm = TRUE))



monthly_mean


