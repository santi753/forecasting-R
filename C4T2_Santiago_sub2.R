################
# Load packages
################

install.packages("RMariaDB")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("plotly")
install.packages("epiDisplay")
install.packages("dplyr")
install.packages("ggfortify")
install.packages("forecast")
library(tidyverse)
library(RMariaDB)
library(lubridate)
library(plotly)
library(epiDisplay)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(forecast)


################################
## Create a database connection 
################################

con <- dbConnect(MariaDB(), user='deepAnalytics',password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

###########################################
##List the tables contained in the database 
###########################################

dbListTables(con)

## Lists attributes contained in a table

dbListFields(con,'iris')

## Use asterisk to specify all attributes for download

irisALL <- dbGetQuery(con, "SELECT * FROM iris")

## Use attribute names to specify specific attributes for download

irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

## Lists attributes contained in table "yr_2006" 

dbListFields(con,"yr_2006")

##############################
## Downloading required tables
##############################

yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")

## Investigate each new data frame

# yr_2006

str(yr_2006)
summary(yr_2006)
head(yr_2006)
tail(yr_2006)

# yr_2007

str(yr_2007)
summary(yr_2007)
head(yr_2007)
tail(yr_2007)

# yr_2008

str(yr_2008)
summary(yr_2008)
head(yr_2008)
tail(yr_2008)

# yr_2009

str(yr_2009)
summary(yr_2009)
head(yr_2009)
tail(yr_2009)

# yr_2010

str(yr_2010)
summary(yr_2010)
head(yr_2010)
tail(yr_2010)

## Combine tables into one dataframe using dplyr

MultiYear <- bind_rows(yr_2006, yr_2007, yr_2008, yr_2009, yr_2010)

# MultiYear

str(MultiYear)
summary(MultiYear)
head(MultiYear)
tail(MultiYear)

###########
## DateTime
###########

## Combine Date and Time attribute values in a new attribute column

MultiYear1 <- cbind(MultiYear, paste(MultiYear$Date, MultiYear$Time), stringsAsFactors=FALSE)
colnames(MultiYear1)[6] <-"DateTime"
MultiYear1 <- MultiYear1[,c(ncol(MultiYear1), 1:(ncol(MultiYear1)-1))]
head(MultiYear1)

## Convert DateTime from character to POSIXct 

MultiYear1$DateTime <- as.POSIXct(MultiYear1$DateTime, '%Y-%m-%d %H:%M:%S', tz = tz("Europe/Paris"))

## Inspect the data types

str(MultiYear1)

## Create "year" attribute with lubridate

MultiYear1$year <- year(MultiYear1$DateTime)
MultiYear1$month <- month(MultiYear1$DateTime)
MultiYear1$week <- week(MultiYear1$DateTime)
MultiYear1$weekdays <- weekdays(MultiYear1$DateTime)
MultiYear1$day <- day(MultiYear1$DateTime)
MultiYear1$hour <- hour(MultiYear1$DateTime)
MultiYear1$minute <- minute(MultiYear1$DateTime)

#####################
## Summary statistics
#####################

dput(head(MultiYear1, 10))

summary(MultiYear1)

sum(MultiYear1$Sub_metering_1)
sum(MultiYear1$Sub_metering_2)
sum(MultiYear1$Sub_metering_3)

sd(MultiYear1$Sub_metering_1)
sd(MultiYear1$Sub_metering_2)
sd(MultiYear1$Sub_metering_3)

# Consumption per year 

aggregate(MultiYear1$Sub_metering_1,
          by = list(year = MultiYear1$year),
          sum)
aggregate(MultiYear1$Sub_metering_2,
          by = list(year = MultiYear1$year),
          sum)
aggregate(MultiYear1$Sub_metering_3,
          by = list(year = MultiYear1$year),
          sum)

MultiYear1.sum <- MultiYear1 %>%
  mutate(year = year(DateTime)) %>% 
  filter(year==2007 | year==2008 | year==2009 | year==2010)  %>%
  group_by(year) %>%
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3),
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))   

MultiYear1.sum

MultiYear1.mean <- MultiYear1.sum %>%
  summarize(SM1 = mean(SM1), 
            SM2 = mean(SM2), 
            SM3 = mean(SM3),
            DateTime = first(DateTime))   

MultiYear1.mean

# Consumption per month

aggregate(MultiYear1$Sub_metering_1,
          by = list(month = MultiYear1$month, year = MultiYear1$year),
          sum)
aggregate(MultiYear1$Sub_metering_2,
          by = list(month = MultiYear1$month, year = MultiYear1$year),
          sum)
aggregate(MultiYear1$Sub_metering_3,
          by = list(month = MultiYear1$month, year = MultiYear1$year),
          sum)

# Consumption per day by choosing the day we want to observe

with(subset(MultiYear1, day == 16 & month == 12 & year == 2006), sum(Sub_metering_1, na.rm = TRUE))
with(subset(MultiYear1, day == 16 & month == 12 & year == 2006), sum(Sub_metering_2, na.rm = TRUE))
with(subset(MultiYear1, day == 16 & month == 12 & year == 2006), sum(Sub_metering_3, na.rm = TRUE))

#################
## Visualizations
#################

MultiYear1

plot(MultiYear1$Sub_metering_1)
plot(MultiYear1$Sub_metering_2)
plot(MultiYear1$Sub_metering_3)

## Average Annual Consumption: 2007~2010

plot_ly(MultiYear1.mean, x = "", y = ~MultiYear1.mean$SM1, name = 'SM1-Kitchen', type = 'bar') %>%
  add_trace(y = ~MultiYear1.mean$SM2, name = 'SM2-Laundry Room') %>%
  add_trace(y = ~MultiYear1.mean$SM3, name = 'SM3-Water Heater & AC') %>%
  layout(title = "Average Annual Consumption: 2007~2010",
         xaxis = list(title = "Submeter"),
         yaxis = list (title = "Power (kWh)"))  


## Subset month to moth of  2008

houseMonth1 <- filter(MultiYear1, year == 2008 & month == c(1,2,3,4,5,6,7,8,9,10,11,12)) 

plot_ly(houseMonth1, x = ~houseMonth1$DateTime, y = ~houseMonth1$Sub_metering_1, type = 'scatter', mode = 'lines', line = list(color = "blue"))%>%
  layout(title = "kitchen Power Consumption 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(houseMonth1, x = ~houseMonth1$DateTime, y = ~houseMonth1$Sub_metering_2, type = 'scatter', mode = 'lines', line = list(color = "orange"))%>%
  layout(title = "Laundry Room Power Consumption 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(houseMonth1, x = ~houseMonth1$DateTime, y = ~houseMonth1$Sub_metering_3, type = 'scatter', mode = 'lines', line = list(color = "green"))%>%
  layout(title = "Water Heater & AC Power Consumption 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


## Subset the second week of 2008 - All Observations

houseWeek <- filter(MultiYear1, year == 2008 & week == 2)

## Plot subset houseWeek

plot(houseWeek$Sub_metering_1)

plot_ly(houseWeek, x = ~houseWeek$DateTime, y = ~houseWeek$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption second week of January, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset the 9th day of January 2008 - All observations

houseDay <- filter(MultiYear1, year == 2008 & month == 1 & day == 9)

## Plot sub-meter 1

plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset the 9th day of January 2008 - 10 Minute frequency

houseDay10 <- dplyr::filter(MultiYear1, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency

plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#########################
##Time Series data frames
#########################

## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009

house070809weekly <- filter(MultiYear1, weekdays == "lunes" & hour == 20 & minute == 1)

## Create TS object with SubMeter3

tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))

## Plot sub-meter 3 with autoplot

autoplot(tsSM3_070809weekly)

## Plot sub-meter 3 with autoplot - add labels, color

autoplot(tsSM3_070809weekly, ts.colour = 'green', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3 one observation per week on Mondays at 8:00pm for each year")

## Plot sub-meter 3 with plot.ts

plot.ts(tsSM3_070809weekly)

## Create TS object with SubMeter1 for Fridays at dinner time

housefridays <- filter(MultiYear1, weekdays == "viernes" & (hour == 19 | hour == 20 | hour == 21 | hour == 22) & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

tsSM1_housefridays <- ts(housefridays$Sub_metering_1, frequency= 52, start=c(2007,1), end = c(2011,1))
 
autoplot(tsSM1_housefridays, ts.colour = 'blue', xlab = "Time", ylab = "Watt Hours", main ="Sub-meter 1 for Fridays at dinner time for each year")

## Create TS object with SubMeter2 for weekends at specific times 

houseweekends <- filter(MultiYear1, (weekdays == "sábado" | weekdays == "domingo") & (hour == 9 | hour == 10 | hour == 11 | hour == 12 | hour == 15 | hour == 16 | hour == 17 | hour == 18) & minute == 1)

tsSM2_houseweekends <- ts(houseweekends$Sub_metering_2, frequency= 52, start=c(2007,1), end = c(2011,1))

autoplot(tsSM2_houseweekends, ts.colour = 'orange', xlab = "Time", ylab = "Watt Hours", main ="Sub-meter 2 for weekends at specific times each year")

###########################
##Forecasting a time series
###########################

## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built

fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 

summary(fitSM3)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 

forecastfitSM3 <- forecast(fitSM3, h=20)

## Plot the forecast for sub-meter 3. 

plot(forecastfitSM3)

## Create sub-meter 3 forecast with confidence levels 80 and 90

forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels

plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time", main = 'Forecast Sub-meter 3 selected period', col = 'green')

## For sub-meter 1 -----------------------------------
 
fitSM1 <- tslm(tsSM1_housefridays ~ trend + season) 

summary(fitSM1)

forecastfitSM1 <- forecast(fitSM1, h=20)

plot(forecastfitSM1)

forecastfitSM1c <- forecast(fitSM1, h=20, level=c(80,90))

plot(forecastfitSM1c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time", main = 'Forecast Sub-meter 1 selected period', col = 'blue')

## For sub-meter 2 -----------------------------------

fitSM2 <- tslm(tsSM2_houseweekends ~ trend + season) 

summary(fitSM2)

forecastfitSM2 <- forecast(fitSM2, h=20)

plot(forecastfitSM2)

forecastfitSM2c <- forecast(fitSM2, h=20, level=c(80,90))

plot(forecastfitSM2c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time", main = 'Forecast Sub-meter 2 selected period', col = 'orange')

####################################
##Decomposing a Seasonal Time Series
####################################

## Decompose Sub-meter 3 into trend, seasonal and remainder

components070809SM3weekly <- decompose(tsSM3_070809weekly)

## Plot decomposed sub-meter 3 

plot(components070809SM3weekly, col = 'green')

## Check summary statistics for decomposed sub-meter 3 

summary(components070809SM3weekly)

## For sub-meter 1 -----------------------------------

componentshousefridays <- decompose(tsSM1_housefridays)

plot(componentshousefridays, col = 'blue')

summary(componentshousefridays)

## For sub-meter 2 -----------------------------------

componentshouseweekends <- decompose(tsSM2_houseweekends)

plot(componentshouseweekends, col = 'orange')

summary(componentshouseweekends)

##########################
##Holt-Winters Forecasting
##########################

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot

tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal

autoplot(tsSM3_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal

plot(decompose(tsSM3_070809Adjusted), col = 'green')

## Holt Winters Exponential Smoothing & Plot

tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)

plot(tsSM3_HW070809, ylim = c(0, 25), col = 'green')

## Holt Winters forecast & plot

tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)

plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", col = 'green')

## Forecast HoltWinters with diminished confidence levels

tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))

## Plot only the forecasted area

plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))

## For sub-meter 1 -----------------------------------

tsSM1_housefridaysAdjusted <- tsSM1_housefridays - componentshousefridays$seasonal

autoplot(tsSM1_housefridaysAdjusted)

plot(decompose(tsSM1_housefridaysAdjusted))

tsSM1_HWhousefridays <- HoltWinters(tsSM1_housefridaysAdjusted, beta=FALSE, gamma=FALSE)

plot(tsSM1_HWhousefridays, ylim = c(0, 25))

tsSM1_HWhousefridaysfor <- forecast(tsSM1_HWhousefridays, h=25)

plot(tsSM1_HWhousefridaysfor, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", col = 'blue')

tsSM1_HWhousefridaysforC <- forecast(tsSM1_HWhousefridaysfor, h=25)

plot(tsSM1_HWhousefridaysforC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2010))

## For sub-meter 2 -----------------------------------

tsSM2_houseweekendsAdjusted <- tsSM2_houseweekends - componentshouseweekends$seasonal

autoplot(tsSM2_houseweekendsAdjusted)

plot(decompose(tsSM2_houseweekendsAdjusted))

tsSM2_HWhouseweekends <- HoltWinters(tsSM2_houseweekendsAdjusted, beta=FALSE, gamma=FALSE)

plot(tsSM2_HWhouseweekends, ylim = c(0, 25))

tsSM2_HWhouseweekendsfor <- forecast(tsSM2_HWhouseweekends, h=25)

plot(tsSM2_HWhouseweekendsfor, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", col = 'orange')

tsSM2_HWhouseweekendsforC <- forecast(tsSM2_HWhouseweekendsfor, h=25)

plot(tsSM2_HWhouseweekendsforC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))


#############################################################################################################











