### Load the packages ##########################################################
################################################################################

library(forecast)
library(lmtest)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Import data ################################################################
################################################################################

df <- readRDS(df, file = "data/intermediate/main_data.rds")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Basic checks for time series modeling ######################################
################################################################################

# time series plots of counts
plot(ts(df$btcount, frequency = 7), 
     las = 1, 
     xlab = "Week", 
     ylab = "Daily count", 
     main = "Time series of BT")
plot(ts(df$gccount, frequency = 7), 
     las = 1, 
     xlab = "Week", 
     ylab = "Daily count", 
     main = "Time series of GC")

# time series seasonal decomposition
plot(decompose(ts(df$btcount, frequency = 7), 
               type = "multiplicative"))
plot(decompose(ts(df$gccount, frequency = 7), 
               type = "multiplicative"))

# plot autocorrelation coefficients
acf(df$btcount, ylim = c(-1, 1))
acf(df$gccount, ylim = c(-1, 1))

# plot partial autocorrelation coefficients
pacf(df$btcount, ylim = c(-1, 1))
pacf(df$gccount, ylim = c(-1, 1))

# correlation matrix
cor(df[c("Precipitation", "Snow", "Snowdepth_WE", "Temp_Max", "Temp_Min",
         "HourlyWindSpeed", "PM25_AQI", "Ozone_AQI", "PM10_AQI", "NO2_AQI",
         "AQI", "PM25", "Ozone", "PM10", "NO2")])

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Time series model for Boulevard trail ######################################
################################################################################

# regressors
xreg_bt <- cbind(Season = model.matrix(~ as.factor(df$Season)),
                 Weekend = model.matrix(~ as.factor(df$Weekend)),
                 Holiday = model.matrix(~ as.factor(df$Holiday)),
                 Precipitation = df$Precipitation,
                 Snow = df$Snow,
                 Temp_Max_86 = I(df$Temp_Max > 86),
                 Temp_Min_32 = I(df$Temp_Min < 32),
                 WindSpeed = df$HourlyWindSpeed,
                 AQI = df$AQI)

# remove intercepts from xreg_bt
xreg_bt <- xreg_bt[, -c(1, 5, 7)] 

# set names of regressors
colnames(xreg_bt) <- c("Season: Spring", "Season: Fall", "Season: Summer",
                       "Weekend: True", "Holiday: True", "Rainfall", 
                       "Snowfall", "Max Temp > 86", "Min Temp < 32", 
                       "Wind speed", "AQI")

# fit the model
fit_bt <- auto.arima(log(df$btcount), xreg = xreg_bt)
summary(fit_bt)
coeftest(fit_bt)

# plot residuals, ACF, PACF
ggtsdisplay(residuals(fit_bt, type = "innovation"))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Time series model for Green canyon #########################################
################################################################################

# regressors
xreg_gc <- cbind(Season = model.matrix(~ as.factor(df$Season)),
                 Weekend = model.matrix(~ as.factor(df$Weekend)),
                 Holiday = model.matrix(~ as.factor(df$Holiday)),
                 Precipitation = df$Precipitation,
                 Snow = df$Snowdepth_WE,
                 Temp_Max_86 = I(df$Temp_Max > 86),
                 Temp_Min_32 = I(df$Temp_Min < 32),
                 WindSpeed = df$HourlyWindSpeed,
                 AQI = df$AQI)

# remove intercepts from xreg_gc
xreg_gc <- xreg_gc[, -c(1, 5, 7)] 

# set names of regressors
colnames(xreg_gc) <- c("Season: Spring", "Season: Fall", "Season: Summer",
                       "Weekend: True", "Holiday: True", "Rainfall", 
                       "Snow_depth", "Max Temp > 86", "Min Temp < 32", 
                       "Wind speed", "AQI")

# fit the model
fit_gc <- auto.arima(log(df$gccount), xreg = xreg_gc)
summary(fit_gc)
coeftest(fit_gc)

# plot residuals, ACF, PACF
ggtsdisplay(residuals(fit_gc, type = "innovation"))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Marginal effect and elasticity - BT ########################################
################################################################################

# estimate coefficients
coeff <- c(fit_bt[["coef"]][["Season: Spring"]], 
           fit_bt[["coef"]][["Season: Fall"]],
           fit_bt[["coef"]][["Season: Summer"]], 
           fit_bt[["coef"]][["Weekend: True"]],
           fit_bt[["coef"]][["Holiday: True"]], 
           fit_bt[["coef"]][["Rainfall"]],
           fit_bt[["coef"]][["Snowfall"]], 
           fit_bt[["coef"]][["Max Temp > 86"]],
           fit_bt[["coef"]][["Min Temp < 32"]], 
           fit_bt[["coef"]][["Wind speed"]],
           fit_bt[["coef"]][["AQI"]])

# data frame for calculation
elasticity_bt <- data.frame(colnames(xreg_bt), coeff)

# mean of variables
elasticity_bt$mean <- c(NA, NA, NA, NA, NA, mean(df$Precipitation), 
                        mean(df$Snow),NA, NA, mean(df$HourlyWindSpeed), 
                        mean(df$AQI))

# marginal effect calculation
elasticity_bt$marginal_effect <- elasticity_bt$coeff * mean(df$btcount)

# elasticity calculation
elasticity_bt$elasticity <- elasticity_bt$coeff * elasticity_bt$mean

# view the results
elasticity_bt

# remove intermediate files
rm(xreg_bt)
rm(coeff)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Marginal effect and elasticity - GC ########################################
################################################################################

coeff <- c(fit_gc[["coef"]][["Season: Spring"]], 
           fit_gc[["coef"]][["Season: Fall"]],
           fit_gc[["coef"]][["Season: Summer"]], 
           fit_gc[["coef"]][["Weekend: True"]],
           fit_gc[["coef"]][["Holiday: True"]], 
           fit_gc[["coef"]][["Rainfall"]],
           fit_gc[["coef"]][["Snow_depth"]], 
           fit_gc[["coef"]][["Max Temp > 86"]],
           fit_gc[["coef"]][["Min Temp < 32"]], 
           fit_gc[["coef"]][["Wind speed"]],
           fit_gc[["coef"]][["AQI"]])

# data frame for calculation
elasticity_gc <- data.frame(colnames(xreg_gc), coeff)

# mean of variables
elasticity_gc$mean <- c(NA, NA, NA, NA, NA, mean(df$Precipitation), 
                        mean(df$Snow), NA, NA, mean(df$HourlyWindSpeed), 
                        mean(df$AQI))

# marginal effect calculation
elasticity_gc$marginal_effect <- elasticity_gc$coeff * mean(df$gccount)

# elasticity calculation
elasticity_gc$elasticity <- elasticity_gc$coeff * elasticity_gc$mean

# view the results
elasticity_gc

# remove intermediate files
rm(xreg_gc)
rm(coeff)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
