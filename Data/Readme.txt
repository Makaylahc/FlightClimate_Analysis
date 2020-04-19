###### Historical meteorological data for  ######

Note that the data file is in an RDS file format and not a CSV format. You will need to use the readRDS() 
function to load the data into your R session.

The data were pulled from ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/ and represent hourly meteorological data for LOS ANGELES INTERNATIONAL AIRPORT (-118.389, 33.938). Documentation can be found at https://www1.ncdc.noaa.gov/pub/data/ish/ish-format-document.pdf.

The data table consists of the following variables:

time: date and time in UTC
temp: Air temperature in degrees Celcius
rh: relative humidity
aa1_1: The quantity of time over which the LIQUID-PRECIPITATION was measured (hours).
aa1_2: The depth of LIQUID-PRECIPITATION that is measured at the time of an observation (mm).
aa1_3: The code that denotes whether a LIQUID-PRECIPITATION depth dimension was a trace value.
   1 = Measurement impossible or inaccurate
   2 = Trace
   3 = Begin accumulated period (precipitation amount missing until end of accumulated period)
   4 = End accumulated period
   5 = Begin deleted period (precipitation amount missing due to data problem)
   6 = End deleted period
   7 = Begin missing period
   8 = End missing period
   E = Estimated data value (eg, from nearby station)
   I = Incomplete precipitation amount, excludes one or more missing reports, such as one or more 15-minute reports not included in the 1-hour precipitation total
   J = Incomplete precipitation amount, excludes one or more erroneous reports, such as one or more 1-hour precipitation amounts excluded from the 24-hour total
   9 = Missing
aa1_4: The code that denotes a quality status of the reported LIQUID-PRECIPITATION data.
   0 = Passed gross limits check
   1 = Passed all quality control checks
   2 = Suspect
   3 = Erroneous
   4 = Passed gross limits check , data originate from an NCEI data source
   5 = Passed all quality control checks, data originate from an NCEI data source
   6 = Suspect, data originate from an NCEI data source
   7 = Erroneous, data originate from an NCEI data source
   9 = Passed gross limits check if element is present
   A = Data value flagged as suspect, but accepted as good value
   I = Data value not originally in data, but inserted by validator
   M = Manual change made to value based on information provided by NWS or FAA
   P = Data value not originally flagged as suspect, but replaced by validator
   R = Data value replaced with value computed by NCEI software
   U = Data value replaced with edited value 