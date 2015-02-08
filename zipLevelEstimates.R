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

# get the zip code centroids
zcta.df <- read.csv("zcta2010.csv", stringsAsFactors = FALSE)

# get the temperature data
temp.df <- read.csv("INtemp2007_2014.csv")
# make one date column
library(stringr)
temp.df$DATE <- paste0(temp.df$YEAR,
                       str_pad(temp.df$MONTH, 2, pad = "0"),
                       str_pad(temp.df$DAY, 2, pad = "0"))
# make it a wide data frame
library(tidyr)
temp.max.df <- spread_(temp.df[, c("DATE", "USAF_WBAN", "TEMP_max")],
                       key_col = "USAF_WBAN", value_col = "TEMP_max")
                       

# get locations of weather stations
stations.df <- read.csv("tempStations.csv", stringsAsFactors = FALSE)

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


# function that calculates the inverse distance weighted average for a target point
weightedAverage(target.lat, target.lon, 
                station.ids, # id vector in same order as station.lats and station.lons
                station.lats, # vector corresponding to station.ids
                station.lons, # vector corresponding to station.ids
                station.values, # data.frame whose columns are station.ids (i.e., a wide format)
                station.index = NULL, # an index vector you can supply; convenient if the rows are an hour, day, year, etc.
                max.radius # maximum radius, in kilometers, beyond which a station no longer influences the weighted average
                ){
#   target.lat = zcta.df$Latitude[1]; target.lon = zcta.df$Longitude[1]; station.ids = stations.df$USAF_WBAN; station.lats = stations.df$LAT; station.lons = stations.df$LON; station.values = temp.max.df[, -1]; station.index = temp.max.df[, 1]; max.radius = 200
  
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
    "Maximum radius must be a non-negative numeric value"
  }
  
  # need to add 0.000001 degrees to station coordinates if idential to target point
  
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
  
  # replace values with 0 where there is an NA 
  station.values[is.na(station.values)] <- 0
  
  
  # make matrix of weights with same dimension as value matrix 
  # (weights are inverse distances squared)
  weights <- matrix(1/(rep(station.locations$r, dim(station.values)[1])^2),
                    nrow = dim(station.values)[1], byrow = TRUE)
  
  # replace weights with 0 where there is an NA in the values matrix
  weights[is.na(station.values)] <- 0
  
  # multiply (element-wise) the values and weights matrices and get a 
  # vector of summed rows
  numerator <- rowSums(station.values * weights) 
  
  # calculate the sum of each row in the weight matrix
  denom <- rowSums(weights) 
  
  # calculate inverse distance squared weighted average for each day
  weighted.avg <- summed/denom
  
  # replace zeros with NAs
  weighted.avg[weighted.avg == 0] <- NA


  
}
