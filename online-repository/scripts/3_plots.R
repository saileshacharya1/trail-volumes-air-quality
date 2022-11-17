### Load the packages ##########################################################
################################################################################

library(tidyverse)
library(gridExtra)
library(scales)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Import data ################################################################
################################################################################

count <- readRDS(count, file = "data/intermediate/count.rds")
df <- readRDS(df, file = "data/intermediate/main_data.rds")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Plot of % of weekly counts #################################################
################################################################################

# preparing the data
count$Day <- weekdays(count$Date_time)
count$Hour <- strftime(count$Date_time, format = "%H")
bt_per_hour <- aggregate(bt_per ~ Hour + Day, count, FUN = mean, na.rm = T)
gc_per_hour <- aggregate(gc_per ~ Hour + Day, count, FUN = mean, na.rm = T)
count <- merge(bt_per_hour, gc_per_hour, all = TRUE)
rm(bt_per_hour)
rm(gc_per_hour)
count$Day <- factor(count$Day,
                         levels = c("Monday", "Tuesday", "Wednesday", 
                                    "Thursday", "Friday", "Saturday", 
                                    "Sunday"),
                         labels = c(1:7))
count$Date <- as.POSIXct(paste(2018, 1, count$Day, count$Hour),
                              format = "%Y %m %d %H")

# ggplot of % of weekly counts
gp_count <- ggplot(count, aes(x = Date), legend = TRUE) +
  geom_line(aes(y = bt_per, color = "Boulevard Trail"), size = 0.4) +
  geom_line(aes(y = gc_per, color = "Green Canyon"), size = 0.4) +
  scale_x_datetime(labels = date_format("%A"), 
                   breaks = "1 day", expand = c(0.05, 0)) +
  labs(y = "Proportion of weekly total", x = "Day", color = "legend") +
  scale_colour_manual(name = element_blank(), 
                      values = c("Boulevard Trail" = "darkorange", 
                                 "Green Canyon" = "dodgerblue")) +
  theme(axis.title = element_text(size = 9),
        legend.position = c(0.04, 0.96), 
        legend.direction = "vertical", 
        legend.justification = "left", 
        legend.key.size = unit(0.75, "lines"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_line(color = "gray90"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5), 
        axis.title.x = element_blank())

# save the plot
ggsave(file = "figures/plot_counts.png", plot = gp_count, 
       width = 175, height = 120, units = "mm")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Plot of of weather and air quality variables ###############################
################################################################################

# ggplot of counts
gp1 <- ggplot(df, aes(x = Date), legend = TRUE) +
  geom_line(aes(y = btcount, color = "Boulevard Trail"), size = 0.4) +
  geom_line(aes(y = gccount, color = "Green Canyon"), size = 0.4) +
  scale_x_date(labels = date_format("%Y-%m"), 
               breaks = "3 month", 
               limits = as.Date(c("2017-06-01", "2020-02-29")), 
               expand = c(0, 0)) +
  labs(y = "Counts", x = "Date", color = "legend") +
  scale_colour_manual(name = element_blank(), 
                      values = c("Boulevard Trail" = "darkorange", 
                                 "Green Canyon" = "dodgerblue")) +
  theme(axis.title = element_text(size = 9),
        legend.position = c(0, 0.88), 
        legend.direction = "vertical", 
        legend.justification = "left", 
        legend.key.size = unit(0.75, "lines"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_line(color = "gray90"),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

# ggplot off rainfall
gp2 <- ggplot(df, aes(x = Date), legend = TRUE) +
  geom_line(aes(y = Precipitation, color = "Rainfall"), size = 0.4) +
  scale_x_date(labels = date_format("%Y-%m"), 
               breaks = "3 month", 
               limits = as.Date(c("2017-06-01", "2020-02-29")), 
               expand = c(0, 0)) +
  labs(y = "Rainfall (in) ", x = "Date", color = "legend") +
  scale_colour_manual(name = element_blank(), 
                      values = c("Rainfall" = "black")) +
  theme(axis.title = element_text(size = 9),
        legend.position = c(0, 0.88), 
        legend.justification = "left", 
        legend.key.size = unit(0.75, "lines"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_line(color = "gray90"),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

# ggplot of snowfall and snow depth
gp3 <- ggplot(df, aes(x = Date), legend = TRUE) +
  geom_line(aes(y = Snow, color = "Snowfall"), size = 0.4) +
  geom_line(aes(y = Snowdepth_WE, color = "Snow Depth"), size = 0.4) +
  scale_x_date(labels = date_format("%Y-%m"), 
               breaks = "3 month", 
               limits = as.Date(c("2017-06-01", "2020-02-29")), 
               expand = c(0, 0)) +
  labs(y = "Snow (in) ", x = "Date", color = "legend") +
  scale_colour_manual(name = element_blank(), 
                      values = c("Snowfall" = "dodgerblue", 
                                 "Snow Depth" = "darkorange")) +
  theme(axis.title = element_text(size = 9),
        legend.position = c(0, 0.88), 
        legend.direction = "vertical", 
        legend.justification = "left", 
        legend.key.size = unit(0.75, "lines"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_line(color = "gray90"),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

# ggplot of wind speed
gp4 <- ggplot(df, aes(x = Date), legend = TRUE) +
  geom_line(aes(y = HourlyWindSpeed, color = "Wind Speed"), size = 0.4) +
  scale_x_date(labels = date_format("%Y-%m"), 
               breaks = "3 month", 
               limits = as.Date(c("2017-06-01", "2020-02-29")), 
               expand = c(0, 0)) +
  labs(y = "Wind Speed (mph) ", x = "Date", color = "legend") +
  scale_colour_manual(name = element_blank(), 
                      values = c("Wind Speed" = "black")) +
  theme(axis.title = element_text(size = 9),
        legend.position = c(0, 0.88), 
        legend.justification = "left", 
        legend.key.size = unit(0.75, "lines"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_line(color = "gray90"),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

# ggplot of maximum and minimum temperature
gp5 <- ggplot(df, aes(x = Date), legend = TRUE) +
  geom_line(aes(y = Temp_Max, color = "Maximum Temperature"), size = 0.4) +
  geom_line(aes(y = Temp_Min, color = "Minimum Temperature"), size = 0.4) +
  scale_x_date(labels = date_format("%Y-%m"), 
               breaks = "3 month", 
               limits = as.Date(c("2017-06-01", "2020-02-29")), 
               expand = c(0, 0)) +
  labs(y = "Temperature (F) ", x = "Date", color = "legend") +
  scale_colour_manual(name = element_blank(), 
                      values = c("Maximum Temperature" = "darkorange", 
                                 "Minimum Temperature" = "dodgerblue")) +
  theme(axis.title = element_text(size = 9),
        legend.position = c(0, 0.2), 
        legend.direction = "vertical", 
        legend.justification = "left", 
        legend.key.size = unit(0.75, "lines"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_line(color = "gray90"),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

# ggplot of overall AQI
gp6 <- ggplot(df, aes(x = Date), legend = TRUE) +
  geom_line(aes(y = AQI, color = "Overall AQI"), size = 0.4) +
  scale_x_date(labels = date_format("%Y-%m"), 
               breaks = "3 month", 
               limits = as.Date(c("2017-06-01", "2020-02-29")), 
               expand = c(0, 0)) +
  labs(y = "Air Quality Index (AQI) ", x = "Date", color = "legend") +
  scale_colour_manual(name = element_blank(), 
                      values = c("Overall AQI" = "black")) +
  theme(axis.title = element_text(size = 9),
        legend.position = c(0, 0.88), 
        legend.direction = "vertical", 
        legend.justification = "left", 
        legend.key.size = unit(0.75, "lines"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_line(color = "gray90"))

# stack all plots together
gp_ts <- grid.arrange(gp1, gp2, gp3, gp4, gp5, gp6, nrow = 6, ncol = 1, 
                      heights = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.25))

# remove intermediate plots
rm(gp1)
rm(gp2)
rm(gp3)
rm(gp4)
rm(gp5)
rm(gp6)

# save the plot
ggsave(file = "figures/plot_timeseries.png", plot = gp_ts, 
       width = 350, height = 240, units = "mm")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

