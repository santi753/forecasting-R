################
# Load packages
################

install.packages("RMariaDB")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("plotly")
install.packages("epiDisplay")
install.packages("dplyr")
library(tidyverse)
library(RMariaDB)
library(lubridate)
library(plotly)
library(epiDisplay)
library(dplyr)

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

MultiYear1$DateTime <- as.POSIXct(MultiYear1$DateTime, "%Y/%m/%d %H:%M:%S", tz = "Europe/Paris")

## Inspect the data types

str(MultiYear1)

## Create "year" attribute with lubridate

MultiYear1$year <- year(MultiYear1$DateTime)
MultiYear1$month <- month(MultiYear1$DateTime)
MultiYear1$week <- week(MultiYear1$DateTime)
MultiYear1$day <- day(MultiYear1$DateTime)
MultiYear1$hour <- hour(MultiYear1$DateTime)


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


plot(MultiYear1$Sub_metering_1)
plot(MultiYear1$Sub_metering_2)
plot(MultiYear1$Sub_metering_3)


## Subset the second week of 2008 - All Observations
houseWeek <- filter(MultiYear1, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)


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






