# Download zip code shapefile
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
census.tracts.spolydf$GEOID10 <- as.numeric(as.character(census.tracts.spolydf$GEOID10))