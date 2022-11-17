### Load the packages ##########################################################
################################################################################

library(tidyverse)
library(splitstackshape)
library(forecast)
library(lmtest)
library(imputeTS)
library(readr)
library(scales)
library(quantmod)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Prepare air quality data for analysis ######################################
################################################################################

# import raw data
aqi <- read.csv("data/raw/Air_Quality.csv", header = TRUE) # AQI
PM25 <- read.csv("data/raw/PM25.csv", header = TRUE) # PM2.5
Ozone <- read.csv("data/raw/Ozone.csv", header = TRUE) # Ozone
PM10 <- read.csv("data/raw/PM10.csv", header = TRUE) # PM10
NO2 <- read.csv("data/raw/NO2.csv", header = TRUE) # NO2

# join data frames
airq <- aqi %>%
  full_join(PM25, by = "Date") %>%
  full_join(Ozone, by = "Date") %>%
  full_join(PM10, by = "Date") %>%
  full_join(NO2, by = "Date")

# remove unnecessary column
airq$Main.Pollutant <- NULL

# change data format
airq$Date <- as.Date(airq$Date, "%m/%d/%Y")
airq <- airq %>%
  mutate_at(c(2:10), as.numeric)

# summarize daily values
airq <- airq %>%
  group_by(Date) %>%
  summarize_each(funs(mean(., na.rm = TRUE)))

# remove intermediate files
rm(aqi)
rm(PM25)
rm(Ozone)
rm(PM10)
rm(NO2)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Prepare weather data for analysis ##########################################
################################################################################

# import raw data
weather1 <- read.csv("data/raw/Weather1.csv", header = T)
weather2 <- read.csv("data/raw/Weather2.csv", header = T)

# bringing weather2 file into required shape
weather2 <- cSplit(weather2, "Date", "T")
colnames(weather2)[3] <- "Date"
weather2$Date_2 <- NULL

# converting date character into date format
weather1$Date <- as.Date(weather1$Date, "%m/%d/%Y")
weather2$Date <- as.Date(weather2$Date, "%Y-%m-%d")

# converting variables into numeric
weather2 <- weather2 %>%
  mutate_at(c(1:2), as.numeric)

# daily mean values of variables
weather2 <- weather2 %>%
  group_by(Date) %>%
  summarize_each(funs(mean(., na.rm = TRUE)))

# merging all weather data to a data frame
weather <- merge(weather1, weather2, all = TRUE)

# remove intermediate files
rm(weather1)
rm(weather2)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Prepare count data for analysis ############################################
################################################################################

# import raw data
btcount <- read.csv("data/raw/BT_count_hourly.csv", header = T)
gccount <- read.csv("data/raw/GC_count_hourly.csv", header = T)

# convert date character into date format
btcount$Date <- as.Date(btcount$Date, "%m/%d/%Y")
gccount$Date <- as.Date(gccount$Date, "%m/%d/%Y")

# create date-time column
btcount$Date_time <- as.POSIXct(paste(btcount$Date, btcount$Time), 
                                format = "%Y-%m-%d %H")
gccount$Date_time <- as.POSIXct(paste(gccount$Date, gccount$Time), 
                                format = "%Y-%m-%d %H")
# merge counts of two locations
count <- merge(btcount, gccount, all = TRUE)

# limit data from June 2017 to Feb 2020 only
count <- count[count$Date > "2017-05-31" & 
                 count$Date < "2020-03-01", ]

# remove unnecessary columns
count$Time <- NULL
count$Day <- NULL

# remove intermediate files
rm(btcount)
rm(gccount)

# calculates for % of count per week
count$Week <- as.Date(cut(count$Date_time, "week"))
count <- count %>%
  group_by(Week) %>%
  mutate(bt_per = btcount/sum(btcount, na.rm = TRUE),
         gc_per = gccount/sum(gccount, na.rm = TRUE))

# daily average value of counts
count_daily <- count %>%
  group_by(Date) %>%
  summarize(btcount = sum(btcount, na.rm = TRUE),
            gccount = sum(gccount, na.rm = TRUE))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Prepare data set for analysis ##############################################
################################################################################

# merge count, air quality, and weather data
df <- count_daily %>%
  full_join(airq) %>%
  full_join(weather) 

# limit data from June 2017 to Feb 2020 only
df <- df[df$Date > "2017-05-31" & df$Date < "2020-03-01", ]

# impute the missing values with na_kalman
df <- df[order(df$Date),]
df <- df %>%
  mutate_at(c(2:19), 
            function(x) na_kalman(x, model = "auto.arima"))

# add columns - year, month and day
df$Year <- as.integer(strftime(df$Date, format = "%Y"))
df$Year <- as.factor(df$Year)
df$Month <- as.integer(strftime(df$Date, format = "%m"))
df$Month <- factor(df$Month, labels = c("Jan", "Feb", "Mar", "Apr",
                                        "May", "Jun", "Jul", "Aug",
                                        "Sep", "Oct", "Nov", "Dec"))
df$Weekday <- strftime(df$Date, format = "%a")
df$Weekday <- factor(df$Weekday, levels = c("Mon", "Tue", "Wed", "Thu",
                                            "Fri", "Sat", "Sun"))

# add column - weekend
df <- df %>%
  mutate(Weekend = ifelse(df$Weekday == "Sat" | df$Weekday == "Sun", 
                          TRUE, FALSE)) 

# add column - season
df <- df %>%
  mutate(Season = ifelse(df$Month == "Dec" | df$Month == "Jan" | 
                           df$Month == "Feb", "Winter", 
                         ifelse(df$Month == "Mar" | df$Month == "Apr" |
                                  df$Month == "May", "Spring", 
                                ifelse(df$Month == "Jun" | df$Month == "Jul" |
                                         df$Month == "Aug", "Summer", "Fall"))))
df$Season <- factor(df$Season, levels = c("Winter", "Spring", "Fall", "Summer"))

# add column - holidays (manual entry)
df$Holiday <- as.logical(ifelse(df$Date == "2017-07-04" | 
                                  df$Date == "2017-07-24" |
                                  df$Date == "2017-09-04" | 
                                  df$Date == "2017-10-09" |
                                  df$Date == "2017-11-10" | 
                                  df$Date == "2017-11-23" |
                                  df$Date == "2017-12-25" | 
                                  df$Date == "2018-01-01" |
                                  df$Date == "2018-01-15" | 
                                  df$Date == "2018-02-19" |
                                  df$Date == "2018-05-28" | 
                                  df$Date == "2018-07-04" |
                                  df$Date == "2018-07-24" | 
                                  df$Date == "2018-09-03" |
                                  df$Date == "2018-10-08" | 
                                  df$Date == "2018-11-12" |
                                  df$Date == "2018-11-22" |
                                  df$Date == "2018-12-05" |
                                  df$Date == "2018-12-24" |
                                  df$Date == "2018-12-25" |
                                  df$Date == "2019-01-01" | 
                                  df$Date == "2019-01-15" |    
                                  df$Date == "2019-02-18" | 
                                  df$Date == "2019-05-27" |
                                  df$Date == "2019-07-04" | 
                                  df$Date == "2019-07-24" |
                                  df$Date == "2019-09-02" | 
                                  df$Date == "2019-10-14" |
                                  df$Date == "2019-11-11" | 
                                  df$Date == "2019-11-28" |
                                  df$Date == "2019-12-25" | 
                                  df$Date == "2019-12-25" |    
                                  df$Date == "2020-01-01" | 
                                  df$Date == "2020-01-20" |
                                  df$Date == "2020-02-17", TRUE, FALSE))

# remove intermediate files
rm(airq)
rm(count_daily)
rm(weather)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Save data frames ###########################################################
################################################################################

saveRDS(count, file = "data/intermediate/count.rds")
saveRDS(df, file = "data/intermediate/main_data.rds")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
