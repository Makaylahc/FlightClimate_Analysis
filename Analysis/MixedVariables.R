# Makaylah Cowan
# Analysing multiple variables

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


# HUMIDITY VS TEMPERATURE
model <- lm(temp ~ rh, cleandata)
print(model)

# Look at residuals
model$residuals

yeardata <- cleandata %>%
  mutate(month = month(time),
         year = as.numeric(as.character(year(time))),
         MonthYear = paste(year(time),formatC(month(time), width = 2, flag = "0"))) #%>% 
  #select("year", "month", "MonthYear", "rh", "temp")

segmentedyears <- yeardata %>% 
    mutate(Year = factor(year) ,
         Plot1 = droplevels(Year, exclude = c(2000, 2001, 2002, 2004, 2003, 2005, 2006, 2007,
         2008, 2009, 2010, 2011, 2013, 2014, 2015, 2012, 2016, 1996)),
         #Plot1 = droplevels(Year, exclude = 2016),
         Month = as.numeric(month),
         rh = as.numeric(rh))

segmentedyears<- segmentedyears[!is.na(segmentedyears$Plot1), ]

ggplot(segmentedyears, aes(x = temp, y = rh)) + geom_point() + geom_smooth(method = "loess") + facet_wrap(~Plot1)

ggplot(yeardata, aes(x = temp, y = rh)) + geom_point() + geom_smooth(method = "loess")


# Depth vs Temperature
ggplot(yeardata, aes(x = aa1_2, y = temp)) + geom_point() +
    xlab("Depth of Precipitation") + ylab("Temperature")


# Depth vs Temperature
ggplot(yeardata, aes(x = aa1_3, y = rh)) + geom_point() +
  xlab("Quantity of Time") + ylab("Temperature")
 