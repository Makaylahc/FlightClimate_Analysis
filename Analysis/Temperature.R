# Makaylah Cowan
# ES218
# Temperature Analysis

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


# Temperature across the years
ggplot(cleandata, aes(x = time, y = temp)) + geom_point() +
       ylab("Temperature") + xlab("Year")

# Temperature at different Months
monthdata <- cleandata %>% 
    mutate(month = factor(months(time),levels = c("January", "February", "March", "April", "May", 
                                                            "June", "July", "August", "September",
                                                            "October", "November", "December")))

ggplot(monthdata, aes(x = month, y = temp )) + geom_point() + xlab("Month") + ylab("Temperature (C)")


# Temperature of different years across months
yeardata <- cleandata %>%
           mutate(month = month(time),
                  year = as.numeric(as.character(year(time))),
                  MonthYear = paste(year(time),formatC(month(time), width = 2, flag = "0"))) %>% 
           select("year", "month", "MonthYear", "temp")

avgtemp <- aggregate(yeardata, by = list(yeardata$MonthYear), FUN = function(x) mean(x, na.rm=T))

avgtemp <- avgtemp %>% 
    mutate(Year = factor(year),
           Plot1 = droplevels(Year, exclude = c(2000, 2001, 2002, 2004, 2003, 2005, 2006, 2007,
                                              2008, 2009, 2010, 2011, 2013, 2014, 2015, 2012, 2016, 1996, 1998,2017, 2019, 1999)),
           Month = as.numeric(month),
           Temp = as.numeric(temp))

avgtemp <- avgtemp[!is.na(avgtemp$Plot1), ]
  

ggplot(avgtemp2, aes(x = Month, y = Temp, color = Plot1) ) + geom_line()


# look at residuals for temperature across years
# don't really need to do this
rfdata <- yeardata %>% 
  group_by(year) %>% 
  mutate(residuals = temp - mean(temp))

# normalized batches to common location.
ggplot(rfdata, aes(x = year, y = residuals)) + geom_jitter(width = 0.1, height = 0, alpha = 0.1) +
       ylab("Residuals") + xlab("Year")


# look at spreads of data, we want them to be the same
df <- rfdata %>%
  dplyr::group_by(year)  %>%
  dplyr::arrange(residuals)  %>% 
  dplyr::mutate(f.val    = (row_number(residuals) - 0.5) / n() )  %>%
  ungroup()  %>%
  mutate(Pooled.res = quantile(residuals, probs = f.val))  %>%
  select(year, residuals, Pooled.res)

# spreads essentially have the same spread so we can compare batches in residual-fit plot
ggplot(df, aes(y = residuals, x = Pooled.res)) + geom_point(alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~ year, nrow = 3) 

# residual - fit plot
# normalized data to global mean
# The spread of the fitted teperature (across each year) is not 
# insignificant compared to the spread of the combined residuals. 
# So temperature differences between singer groups cannot be explained 
# by random chance alone or, put another way, the voice-parts can explain a 
# good part of the variation in the data
# fitted values condensed but residuals spread with the one residual as 'outlier'

temp <- yeardata %>% 
    mutate(Year = factor(year),
      year2 = droplevels(Year, exclude = c(2000, 2001, 2002, 2004, 2003, 2005, 2006, 2007,
                                                2008, 2009, 2010, 2011, 2013, 2014, 2015, 2012, 2016, 
                                                1996)))
temp <- temp[!is.na(temp$year2), ]
print(levels(temp$year2))
rfs(oneway(temp ~ year, data = temp, spread = 1), 
    aspect = 1, 
    ylab = "Temperature (Celsius)")




