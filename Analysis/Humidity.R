# Makaylah Cowan
# Humidity Analysis

# Read in packages
library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
library(lattice)


Airport <- readRDS("../Data/LA_airport.rds")

cleandata <- Airport %>% 
  na.omit() %>% 
  filter(!grepl(9, aa1_3) & !grepl(1, aa1_3) & !grepl(999.9, aa1_2) & !grepl(99, aa1_1), !grepl(9, aa1_4))

# Humidity vs time
ggplot(cleandata, aes(x = time, y = rh)) + geom_point()


# year for humidity
yeardata <- cleandata %>%
  mutate(month = month(time),
         year = as.numeric(as.character(year(time))),
         MonthYear = paste(year(time),formatC(month(time), width = 2, flag = "0"))) %>% 
  select("year", "month", "MonthYear", "rh")

avgrh <- aggregate(yeardata, by = list(yeardata$MonthYear), FUN = function(x) mean(x, na.rm=T))

avgrh <- avgrh %>% 
  mutate(Year = factor(year) ,
         #Plot1 = droplevels(Year, exclude = c(2000, 2001, 2002, 2004, 2003, 2005, 2006, 2007,
                                               #2008, 2009, 2010, 2011, 2013, 2014, 2015, 2012, 2016, 1996, 2017,  1999)),
         Plot1 = droplevels(Year, exclude = 1996),
         Month = as.numeric(month),
         rh = as.numeric(rh))

avgrh <- avgrh[!is.na(avgrh$Plot1), ]


ggplot(avgrh, aes(x = month, y = rh) ) + geom_line() + facet_wrap(~Plot1)


# Humidty across months
monthdata <- cleandata %>% 
  mutate(month = factor(months(time),levels = c("January", "February", "March", "April", "May", 
                                                "June", "July", "August", "September",
                                                "October", "November", "December")))

ggplot(monthdata, aes(x = month, y = rh )) + geom_point() + xlab("Month") + ylab("Temperature (C)")


