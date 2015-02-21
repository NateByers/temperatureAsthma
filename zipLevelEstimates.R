########################################################################
# Download zip code shapefile and create zip data frame
dir.create("ZCTA_2010")
temp <- tempfile()
download.file("ftp://ftp2.census.gov/geo/pvs/tiger2010st/18_Indiana/18/tl_2010_18_zcta500.zip",
              temp)
#unzip into ZCTA_2010 folder
unzip(temp, exdir = "ZCTA_2010")
file.remove(temp)

#create SpatialPolygonsDataFrame
library(maptools)
proj <- "+proj=longlat +datum=NAD83"
leaflet.proj <-"+init=epsg:3857"
zcta.spolydf <- readShapePoly("ZCTA_2010\\tl_2010_18_zcta500", 
                                       proj4string = CRS(proj))
# take a look
plot(zcta.spolydf)

# get centroids
coords <- coordinates(zcta.spolydf)

# make a regular data frame and remove the large SPDF
zcta.df <- data.frame("Longitude" = coords[, 1],
                      "Latitude" = coords[, 2],
                      "ZCTA" = as.data.frame(zcta.spolydf)[, "ZCTA5CE00"],
                      stringsAsFactors = FALSE)
write.csv(zcta.df, file = "zcta2010.csv")
##########################################################################
# Write the function for inverse distance weighting
library(dplyr)
library(tidyr)
library(stringr)
# get the zip code centroids
zcta.df <- read.csv("zcta2010.csv", stringsAsFactors = FALSE)

# get locations of weather stations
stations.df <- read.csv("tempStations.csv", stringsAsFactors = FALSE)


# # take a look at the coverage
# library(maps)
# map("state", "indiana")
# points(stations.df$LON, stations.df$LAT)
# library(sp)
# stations.spdf <- SpatialPointsDataFrame(cbind(stations.df$LON, stations.df$LAT),
#                                       data.frame(dummy = rnorm(dim(stations.df)[1])),
#                                       proj4string = CRS("+proj=longlat +datum=NAD83"))
# spplot(stations.spdf)

# get the weather data
met.df <- read.csv("INmet2007_2014.csv", stringsAsFactors = FALSE)

# subset down to stations that we have lat/longs for
met.df <- merge(met.df, data.frame(USAF_WBAN = stations.df$USAF_WBAN))

# make one date column
met.df$DATE <- paste0(met.df$YEAR,
                       str_pad(met.df$MONTH, 2, pad = "0"),
                       str_pad(met.df$DAY, 2, pad = "0"))

# make a temp.df object and subset down to days with >=75% completeness
temp.df <- met.df[, c("DATE", "USAF_WBAN", grep("TEMP", names(met.df), fixed = TRUE, value = TRUE))]
temp.df <- temp.df[temp.df$TEMP_n >= 18, ]

# order by USAF_WBAN
temp.df <- arrange(temp.df, USAF_WBAN)

# make a stations data frame that only has stations in the temp.df data frame
stations.temp.df <- merge(stations.df, data.frame(USAF_WBAN = unique(temp.df$USAF_WBAN)))

# order by USAF_WBAN
stations.temp.df <- arrange(stations.temp.df, USAF_WBAN)

# make a wide data frame for maximum temp
temp.max.df <- spread_(temp.df[, c("DATE", "USAF_WBAN", "TEMP_max")],
                       key_col = "USAF_WBAN", value_col = "TEMP_max")
                       
# function for distance in kilometers between two long/lat positions (from "fossil" package)
earth.dist <- function (long1, lat1, long2, lat2) 
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

# subset 

# function that calculates the inverse distance weighted average for a target point
weightedAverage <- function(target.lat, target.lon, target.name = NULL, # name for value column in returned data.frame
                            station.ids, # id vector in same order as station.lats and station.lons
                            station.lats, # vector corresponding to station.ids
                            station.lons, # vector corresponding to station.ids
                            station.values, # data.frame whose columns are station.ids (i.e., a wide format)
                            station.index = NULL, # an index vector you can supply; convenient if the rows are an hour, day, year, etc.
                            max.radius # maximum radius, in kilometers, beyond which a station no longer influences the weighted average
){
  
  print(target.name)
  #   target.lat = zcta.df$Latitude[1]; target.lon = zcta.df$Longitude[1]; target.name = zcta.df$ZCTA[1]; station.ids = stations.df$USAF_WBAN; station.lats = stations.df$LAT; station.lons = stations.df$LON; station.values = temp.max.df[, -1]; station.index = temp.max.df[, 1]; max.radius = 200
  
  if(length(station.ids) != length(station.lats) | 
       length(station.ids) != length(station.lons) |
       length(station.lats) != length(station.lons) 
  ) {
    stop("Dimensions for station IDs and lat/lons don't match up")
  }
  if(!identical(station.ids, colnames(station.values))) {
    stop("Station IDs and column names for station values data frame are not identical")
  }
  if(!is.null(station.index)){
    if(length(station.index) != dim(station.values)[1]){
      stop("Station index vector doesn't match number of rows in station values data frame")
    }
  } else {station.index <- 1:dim(station.values)[1]}
  if(max.radius < 0){
    stop("Maximum radius must be a non-negative numeric value in kilometers")
  }
  if(sum(apply(cbind(station.lons, station.lats, target.lon, target.lat), 1, function(x){
    x[1] == x[3] & x[2] == x[4]
  })) > 0){
    stop("Target location cannot match any station locations")
  }
  if(is.null(target.name)){target.name = "idw_average"}
  
  # get distances from stations to target point
  dists <- sapply(1:length(station.ids), function(i){
    earth.dist(target.lon, target.lat, station.lons[i], station.lats[i])
  })
  
  # make data frame with location information
  station.locations <- data.frame(id = station.ids, lon = station.lons,
                                  lat = station.lats, r = dists,
                                  stringsAsFactors = FALSE)
  
  # remove stations greater than maximum radius
  station.locations <- subset(station.locations, r <= max.radius)
  
  # remove columns from station values data frame 
  station.values <- station.values[, station.locations$id]
  
  # make matrix of weights with same dimension as value matrix 
  # (weights are inverse distances squared)
  weights <- matrix(1/(rep(station.locations$r, dim(station.values)[1])^2),
                    nrow = dim(station.values)[1], byrow = TRUE)
  
  # replace weights with 0 where there is an NA in the values matrix
  weights[is.na(station.values)] <- 0
  
  # find rows in staiont.values that are all NAs, for later...
  all.na <- apply(station.values, 1, function(x) sum(!is.na(x)) == 0)
  
  # replace values with 0 where there is an NA 
  station.values[is.na(station.values)] <- 0
  
  # multiply (element-wise) the values and weights matrices and get a 
  # vector of summed rows
  numerator <- rowSums(station.values * weights) 
  
  # calculate the sum of each row in the weight matrix
  denom <- rowSums(weights) 
  
  # calculate inverse distance squared weighted average for each day
  weighted.avg <- numerator/denom
  
  # replace rows with all NAs with NA
  weighted.avg[all.na] <- NA
  
  # make data.frame object with the station values index
  idw.df <- data.frame(index = station.index, weighted.avg, stringsAsFactors = FALSE)
  
  # rename the values column
  colnames(idw.df)[2] <- target.name
  
  idw.df
}

idw.temp.max.list <- lapply(1:dim(zcta.df)[1], function(i){
  weightedAverage(target.lat = zcta.df$Latitude[i], target.lon = zcta.df$Longitude[i],
                  target.name = zcta.df$ZCTA[i], station.ids = stations.temp.df$USAF_WBAN,
                  station.lats = stations.temp.df$LAT, station.lons = stations.temp.df$LON, 
                  station.values = temp.max.df[, -1], station.index = temp.max.df[, 1],
                  max.radius = 200)
})


temp.max.idw.df <- Reduce(function(...) merge(..., all = TRUE), idw.temp.max.list)
write.csv(temp.max.idw.df, file = "max_temp_idw_ZIP_2007_2014.csv", row.names = FALSE)




# make a wide data frame for minimum temp
temp.min.df <- spread_(temp.df[, c("DATE", "USAF_WBAN", "TEMP_min")],
                       key_col = "USAF_WBAN", value_col = "TEMP_min")

idw.temp.min.list <- lapply(1:dim(zcta.df)[1], function(i){
  weightedAverage(target.lat = zcta.df$Latitude[i], target.lon = zcta.df$Longitude[i],
                  target.name = zcta.df$ZCTA[i], station.ids = stations.temp.df$USAF_WBAN,
                  station.lats = stations.temp.df$LAT, station.lons = stations.temp.df$LON, 
                  station.values = temp.min.df[, -1], station.index = temp.min.df[, 1],
                  max.radius = 200)
})

temp.min.idw.df <- Reduce(function(...) merge(..., all = TRUE), idw.temp.min.list)
write.csv(temp.min.idw.df, file = "min_temp_idw_ZIP_2007_2014.csv", row.names = FALSE)

# make a wide data frame for mean temp
temp.mean.df <- spread_(temp.df[, c("DATE", "USAF_WBAN", "TEMP_mean")],
                       key_col = "USAF_WBAN", value_col = "TEMP_mean")

idw.temp.mean.list <- lapply(1:dim(zcta.df)[1], function(i){
  weightedAverage(target.lat = zcta.df$Latitude[i], target.lon = zcta.df$Longitude[i],
                  target.name = zcta.df$ZCTA[i], station.ids = stations.temp.df$USAF_WBAN,
                  station.lats = stations.temp.df$LAT, station.lons = stations.temp.df$LON, 
                  station.values = temp.mean.df[, -1], station.index = temp.mean.df[, 1],
                  max.radius = 200)
})

temp.mean.idw.df <- Reduce(function(...) merge(..., all = TRUE), idw.temp.mean.list)
write.csv(temp.mean.idw.df, file = "mean_temp_idw_ZIP_2007_2014.csv", row.names = FALSE)

# make a dewp.df object and subset down to days with >=75% completeness
dewp.df <- met.df[, c("DATE", "USAF_WBAN", grep("DEWP", names(met.df), fixed = TRUE, value = TRUE))]
dewp.df <- dewp.df[dewp.df$DEWP_n >= 18, ]

# order by USAF_WBAN
dewp.df <- arrange(dew.df, USAF_WBAN)

# make a stations data frame that only has stations in the dewp.df data frame
stations.dewp.df <- merge(stations.df, data.frame(USAF_WBAN = unique(dewp.df$USAF_WBAN)))

# order by USAF_WBAN
stations.dewp.df <- arrange(stations.dewp.df, USAF_WBAN)

# make a wide data frame for maximum dewpoint
dewp.max.df <- spread_(dewp.df[, c("DATE", "USAF_WBAN", "DEWP_max")],
                       key_col = "USAF_WBAN", value_col = "DEWP_max")

idw.dewp.max.list <- lapply(1:dim(zcta.df)[1], function(i){
  weightedAverage(target.lat = zcta.df$Latitude[i], target.lon = zcta.df$Longitude[i],
                  target.name = zcta.df$ZCTA[i], station.ids = stations.dewp.df$USAF_WBAN,
                  station.lats = stations.dewp.df$LAT, station.lons = stations.dewp.df$LON, 
                  station.values = dewp.max.df[, -1], station.index = dewp.max.df[, 1],
                  max.radius = 200)
})

dewp.max.idw.df <- Reduce(function(...) merge(..., all = TRUE), idw.dewp.max.list)
write.csv(dewp.max.idw.df, file = "max_dewp_idw_ZIP_2007_2014.csv", row.names = FALSE)

# make a wide data frame for minimum dewpoint
dewp.min.df <- spread_(dewp.df[, c("DATE", "USAF_WBAN", "DEWP_min")],
                       key_col = "USAF_WBAN", value_col = "DEWP_min")

idw.dewp.min.list <- lapply(1:dim(zcta.df)[1], function(i){
  weightedAverage(target.lat = zcta.df$Latitude[i], target.lon = zcta.df$Longitude[i],
                  target.name = zcta.df$ZCTA[i], station.ids = stations.dewp.df$USAF_WBAN,
                  station.lats = stations.dewp.df$LAT, station.lons = stations.dewp.df$LON, 
                  station.values = dewp.min.df[, -1], station.index = dewp.min.df[, 1],
                  max.radius = 200)
})

dewp.min.idw.df <- Reduce(function(...) merge(..., all = TRUE), idw.dewp.min.list)
write.csv(dewp.min.idw.df, file = "min_dewp_idw_ZIP_2007_2014.csv", row.names = FALSE)

# make a wide data frame for mean dewpoint
dewp.mean.df <- spread_(dewp.df[, c("DATE", "USAF_WBAN", "DEWP_mean")],
                       key_col = "USAF_WBAN", value_col = "DEWP_mean")

idw.dewp.mean.list <- lapply(1:dim(zcta.df)[1], function(i){
  weightedAverage(target.lat = zcta.df$Latitude[i], target.lon = zcta.df$Longitude[i],
                  target.name = zcta.df$ZCTA[i], station.ids = stations.dewp.df$USAF_WBAN,
                  station.lats = stations.dewp.df$LAT, station.lons = stations.dewp.df$LON, 
                  station.values = dewp.mean.df[, -1], station.index = dewp.mean.df[, 1],
                  max.radius = 200)
})

dewp.mean.idw.df <- Reduce(function(...) merge(..., all = TRUE), idw.dewp.mean.list)
write.csv(dewp.mean.idw.df, file = "mean_dewp_idw_ZIP_2007_2014.csv", row.names = FALSE)








# make a rh.df object and subset down to days with >=75% completeness
rh.df <- met.df[, c("DATE", "USAF_WBAN", grep("RH", names(met.df), fixed = TRUE, value = TRUE))]
rh.df <- rh.df[rh.df$RH_n >= 18, ]

# order by USAF_WBAN
rh.df <- arrange(rh.df, USAF_WBAN)

# make a stations data frame that only has stations in the rh.df data frame
stations.rh.df <- merge(stations.df, data.frame(USAF_WBAN = unique(rh.df$USAF_WBAN)))

# order by USAF_WBAN
stations.rh.df <- arrange(stations.rh.df, USAF_WBAN)

# make a wide data frame for maximum relative humidity
rh.max.df <- spread_(rh.df[, c("DATE", "USAF_WBAN", "RH_max")],
                       key_col = "USAF_WBAN", value_col = "RH_max")

idw.rh.max.list <- lapply(1:dim(zcta.df)[1], function(i){
  weightedAverage(target.lat = zcta.df$Latitude[i], target.lon = zcta.df$Longitude[i],
                  target.name = zcta.df$ZCTA[i], station.ids = stations.rh.df$USAF_WBAN,
                  station.lats = stations.rh.df$LAT, station.lons = stations.rh.df$LON, 
                  station.values = rh.max.df[, -1], station.index = rh.max.df[, 1],
                  max.radius = 200)
})

rh.max.idw.df <- Reduce(function(...) merge(..., all = TRUE), idw.rh.max.list)
write.csv(rh.max.idw.df, file = "max_rh_idw_ZIP_2007_2014.csv", row.names = FALSE)

# make a wide data frame for minimum relative humidity
rh.min.df <- spread_(rh.df[, c("DATE", "USAF_WBAN", "RH_min")],
                       key_col = "USAF_WBAN", value_col = "RH_min")

idw.rh.min.list <- lapply(1:dim(zcta.df)[1], function(i){
  weightedAverage(target.lat = zcta.df$Latitude[i], target.lon = zcta.df$Longitude[i],
                  target.name = zcta.df$ZCTA[i], station.ids = stations.rh.df$USAF_WBAN,
                  station.lats = stations.rh.df$LAT, station.lons = stations.rh.df$LON, 
                  station.values = rh.min.df[, -1], station.index = rh.min.df[, 1],
                  max.radius = 200)
})

rh.min.idw.df <- Reduce(function(...) merge(..., all = TRUE), idw.rh.min.list)
write.csv(rh.min.idw.df, file = "min_rh_idw_ZIP_2007_2014.csv", row.names = FALSE)

# make a wide data frame for mean relative humidity
rh.mean.df <- spread_(rh.df[, c("DATE", "USAF_WBAN", "RH_mean")],
                        key_col = "USAF_WBAN", value_col = "RH_mean")

idw.rh.mean.list <- lapply(1:dim(zcta.df)[1], function(i){
  weightedAverage(target.lat = zcta.df$Latitude[i], target.lon = zcta.df$Longitude[i],
                  target.name = zcta.df$ZCTA[i], station.ids = stations.rh.df$USAF_WBAN,
                  station.lats = stations.rh.df$LAT, station.lons = stations.rh.df$LON, 
                  station.values = rh.mean.df[, -1], station.index = rh.mean.df[, 1],
                  max.radius = 200)
})

rh.mean.idw.df <- Reduce(function(...) merge(..., all = TRUE), idw.rh.mean.list)
write.csv(rh.mean.idw.df, file = "mean_rh_idw_ZIP_2007_2014.csv", row.names = FALSE)





# make a aptemp.df object and subset down to days with both TEMP and DEWP >=75% completeness
aptemp.df <- met.df[met.df$TEMP_n >= 18 & met.df$DEWP_n >= 18, ]
aptemp.df <- aptemp.df[, c("DATE", "USAF_WBAN", "APTEMP")]


# order by USAF_WBAN
aptemp.df <- arrange(aptemp.df, USAF_WBAN)

# make a stations data frame that only has stations in the aptemp.df data frame
stations.aptemp.df <- merge(stations.df, data.frame(USAF_WBAN = unique(aptemp.df$USAF_WBAN)))

# order by USAF_WBAN
stations.aptemp.df <- arrange(stations.aptemp.df, USAF_WBAN)

# make a wide data frame 
aptemp.df <- spread_(aptemp.df, key_col = "USAF_WBAN", value_col = "APTEMP")

idw.aptemp.list <- lapply(1:dim(zcta.df)[1], function(i){
  weightedAverage(target.lat = zcta.df$Latitude[i], target.lon = zcta.df$Longitude[i],
                  target.name = zcta.df$ZCTA[i], station.ids = stations.aptemp.df$USAF_WBAN,
                  station.lats = stations.aptemp.df$LAT, station.lons = stations.aptemp.df$LON, 
                  station.values = aptemp.df[, -1], station.index = aptemp.df[, 1],
                  max.radius = 200)
})

aptemp.idw.df <- Reduce(function(...) merge(..., all = TRUE), idw.aptemp.list)
write.csv(aptemp.idw.df, file = "aptemp_idw_ZIP_2007_2014.csv", row.names = FALSE)



###################################################################################
# Get inverse distance weighted PM2.5 estimates
library(dplyr)
library(tidyr)
library(stringr)

# get the zip code centroids
zcta.df <- read.csv("zcta2010.csv", stringsAsFactors = FALSE)

# get the data frame of PM2.5 daily summary values (object is named "pm25.df"),
# make date column a Date column, rename Mean column, make it a numeric vector,
# and order
load("pm25_IN_2007_2014.rda")
pm25.df$Date <- as.Date(pm25.df$Date, format = "%m/%d/%Y")
names(pm25.df)[names(pm25.df) == "Daily Mean PM2.5 Concentration"] <- "Mean_Value"
pm25.df$Mean_Value <- as.numeric(pm25.df$Mean_Value)
pm25.df <- arrange(pm25.df, AQS_SITE_ID, Date)

# subset down to days with completeness >=75% 
pm25.df <- filter(pm25.df, as.numeric(PERCENT_COMPLETE) >= 75)

# get AQS station locations
stations.df <- unique(pm25.df[, c("AQS_SITE_ID", "SITE_LATITUDE", "SITE_LONGITUDE")]) 

# take the average of POCs
pm25.df <- group_by(pm25.df, Date, AQS_SITE_ID)
pm25.df <- summarise(pm25.df, Mean_Value = mean(Mean_Value))

# make a wide data frame 
pm25.wide.df <- spread_(pm25.df, key_col = "AQS_SITE_ID", value_col = "Mean_Value")

idw.pm25.mean.list <- lapply(1:dim(zcta.df)[1], function(i){
  weightedAverage(target.lat = zcta.df$Latitude[i], target.lon = zcta.df$Longitude[i],
                  target.name = zcta.df$ZCTA[i], station.ids = stations.df$AQS_SITE_ID,
                  station.lats = as.numeric(stations.df$SITE_LATITUDE), 
                  station.lons = as.numeric(stations.df$SITE_LONGITUDE), 
                  station.values = pm25.wide.df[, -1], station.index = pm25.wide.df$Date,
                  max.radius = 200)
})


pm25.mean.idw.df <- Reduce(function(...) merge(..., all = TRUE), idw.pm25.mean.list)
write.csv(pm25.mean.idw.df, file = "mean_pm25_idw_ZIP_2007_2014.csv")



