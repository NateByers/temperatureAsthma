temperatureAsthma
=================

This repository contains code for a study of the effects of extreme temperatures on asthma exacerbations in Indiana.

# Data

The weather data was obtained from NOAA's [Integrated Surface Database](http://www.ncdc.noaa.gov/isd). Hourly temperature and dewpoint was downloaded for all Indiana stations between 2007 and 2014. Relative humidity for every hour was calculated using the `dewpoint.to.humidity()` function in the `weathermetrics` package. 

Daily minimum, maximum, and mean for each station were calculated for temperature, dewpoint, and relative humidity. The total number of hours with a reading for each day was also tallied. Apparent temperature for each day was calculated using the following formula from [Wilker et al. 2012](http://ehp.niehs.nih.gov/wp-content/uploads/120/8/ehp.1104380.pdf): apparent temperature = –2.653 + [0.994 × 24-hr mean air temperature (°C)] + [0.0153 × 24-hr mean dew point temperature (°C)^2].

Daily PM2.5 data were downloaded from the [US EPA](http://www.epa.gov/airdata/ad_data_daily.html).

# ZIP code estimates

Daily zip code level estimates were calculated by inverse distance weighted (IDW) averaging. A ZIP code shapefile was downloaded from the Census Bureau website for the 2010 Census. For each ZIP code centroid, daily IDW estimates were calculated for all weather and pollution variables. A minimum distance criteria was applied for the calculations--a station had to be within 200 km to the ZIP code centroid to contribute to the average. And a completeness criteria was applied for the weather variables--a daily value was only accepted if at least 75% of the hours for that day had a valid reading (i.e., there must be at least 18 hours of valid readings to have a daily value.)
