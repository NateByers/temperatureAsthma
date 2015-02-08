# Went to http://www.ncdc.noaa.gov/isd/data-access and used the search tool. 
# Requested Indiana data one year at a time, simplified output
library(stringr)
library(XML)
library(RSQLite)
library(RCurl)
library(data.table)
library(bit64)
library(dplyr)


# create an SQLite database
con <- dbConnect(dbDriver("SQLite", max.con = 25), dbname="ISDdatabase.sqlite")
table <- readHTMLTable("http://www1.ncdc.noaa.gov/pub/orders/72026654809-2007-08_3271206607236dat.html",
                       stringsAsFactors = FALSE)[[2]][-1, ]
YEAR <- str_sub(table[, "YR--MODAHRMN"], 1, 4)
MONTH <- str_sub(table[, "YR--MODAHRMN"], 5, 6)
DAY <- str_sub(table[, "YR--MODAHRMN"], 7, 8)
USAF_WBAN <- paste(table$USAF, table$WBAN, sep = "_")
table <- data.frame(USAF_WBAN, YEAR, MONTH, DAY, TEMP = table$TEMP, DEWP = table$DEWP,
                    stringsAsFactors = FALSE)
# add an empty data frame just to get the table created (the table is named "Data")
dbWriteTable(conn = con, name = "Data", value = table, row.names = FALSE, header = TRUE)
dbSendQuery(conn = con,
            "DELETE FROM Data")
dbReadTable(con, "Data")
dbDisconnect(con)

# function for getting list of urls for reading in the data
getURLs <- function(station.url, year){
  # station.url <- "http://www1.ncdc.noaa.gov/pub/orders/3271206607236stn.txt"
  stations.df <- read.fwf(station.url, c(6, -1, 5, -1, 31, -83, 7, -2, 8, -3, 7), 
                          skip = 2, stringsAsFactors = FALSE, 
                          colClasses = c(NA, "character", rep(NA, 4)))
  stations.header <- c("USAF", "WBAN", "STATION_NAME", "LATITUDE", "LONGITUDE", "ELEVATION")
  names(stations.df) <- stations.header
  id <- paste0(stations.df$USAF, stations.df$WBAN)
  months <- 1:12
  months <- str_pad(months, 2, pad = "0")
  combos <- expand.grid(id, months , stringsAsFactors = FALSE)
  urlnumbers <- str_split(station.url, "/")[[1]][6]
  urlnumbers <- str_extract(urlnumbers, "[0-9]+")
  urls <- paste0("http://www1.ncdc.noaa.gov/pub/orders/", combos[, 1], "-", year, "-", 
                 combos[, 2], "_", urlnumbers, "dat.html")
  urls
}


# function that takes url of web table, reads the table (if the url exist), and loads into database
loadDatabase <- function(database.file, url){
  # database.file <- "ISDdatabase.sqlite"
  # url <- "http://www1.ncdc.noaa.gov/pub/orders/72026654809-2007-08_3271206607236dat.html"
  # url <- "http://www1.ncdc.noaa.gov/pub/orders/72026654809-2007-09_3271206607236dat.html"
  urlExists <- url.exists(url)
  if(urlExists){
    try(table <- readHTMLTable(url)[[2]][-1, ])
    YEAR <- str_sub(table[, "YR--MODAHRMN"], 1, 4)
    MONTH <- str_sub(table[, "YR--MODAHRMN"], 5, 6)
    DAY <- str_sub(table[, "YR--MODAHRMN"], 7, 8)
    USAF_WBAN <- paste(table$USAF, table$WBAN, sep = "_")
    table <- data.frame(USAF_WBAN, YEAR, MONTH, DAY, TEMP = table$TEMP, DEWP = table$DEWP,
                        stringsAsFactors = FALSE)
    # create a connection to ISDdatabase
    db <- dbConnect(dbDriver("SQLite", max.con = 25), 
                     dbname = database.file)
    # append the data to the Data table
    dbWriteTable(conn = db, name = "Data", value = table,
                 row.names = FALSE, header = TRUE, append = TRUE)
    dbDisconnect(db)
    rm(table)
    gc()
  }
}

# load 2007 data
urls2007 <- getURLs("http://www1.ncdc.noaa.gov/pub/orders/3271206607236stn.txt", 2007)
lapply(urls2007, loadDatabase, database.file = "ISDdatabase.sqlite")

# load 2008 data
urls2008 <- getURLs("http://www1.ncdc.noaa.gov/pub/orders/198006608814stn.txt", 2008)
lapply(urls2008, loadDatabase, database.file = "ISDdatabase.sqlite")

# load 2009 data
urls2009 <- getURLs("http://www1.ncdc.noaa.gov/pub/orders/4972336608968stn.txt", 2009)
lapply(urls2009, loadDatabase, database.file = "ISDdatabase.sqlite")

# load 2010 data
urls2010 <- getURLs("http://www1.ncdc.noaa.gov/pub/orders/3189576608986stn.txt", 2010)
lapply(urls2010, loadDatabase, database.file = "ISDdatabase.sqlite")

# load 2011 data
urls2011 <- getURLs("http://www1.ncdc.noaa.gov/pub/orders/803636609065stn.txt", 2011)
lapply(urls2011, loadDatabase, database.file = "ISDdatabase.sqlite")

# load 2012 data
urls2012 <- getURLs("http://www1.ncdc.noaa.gov/pub/orders/377196609234stn.txt", 2012)
lapply(urls2012, loadDatabase, database.file = "ISDdatabase.sqlite")

# load 2013 data
urls2013 <- getURLs("http://www1.ncdc.noaa.gov/pub/orders/5405396609241stn.txt", 2013)
for(i in 1:length(urls2013)){
  loadDatabase(url = urls2013[i], database.file = "ISDdatabase.sqlite")
}

# load 2014 data
urls2014 <- getURLs("http://www1.ncdc.noaa.gov/pub/orders/7652966609743stn.txt", 2014)
for(i in 1:length(urls2014)){
  loadDatabase(url = urls2014[i], database.file = "ISDdatabase.sqlite")
}

# pull out hourly table, delete duplicates, and make daily summary table
con <- dbConnect(dbDriver("SQLite", max.con = 25), dbname="ISDdatabase.sqlite")
isd.df <- dbReadTable(con, "Data")
dbDisconnect(con)
class(isd.df$TEMP) <- "numeric"
class(isd.df$DEWP) <- "numeric"
isd.df <- distinct(isd.df)
isd.df$TEMP_not_na <- !is.na(isd.df$TEMP)
isd.df$DEWP_not_na <- !is.na(isd.df$DEWP)
isd.df <- group_by(isd.df, USAF_WBAN, YEAR, MONTH, DAY)
isd.df <- summarise(isd.df, TEMP_mean = mean(TEMP, na.rm = TRUE), TEMP_min = min(TEMP, na.rm = TRUE),
                    TEMP_max = max(TEMP, na.rm = TRUE), TEMP_n = sum(TEMP_not_na),
                    DEWP_mean = mean(DEWP, na.rm = TRUE), DEWP_min  = min(DEWP, na.rm = TRUE),
                    DEWP_max = max(DEWP, na.rm = TRUE), DEWP_n = sum(DEWP_not_na))

write.csv(isd.df, file = "INtemp2007_2014.csv")

# make stations table with lat/longs
stations.df <- read.csv("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/isd-history.csv",
                        stringsAsFactors = FALSE)
stations.df$USAF_WBAN <- paste(stations.df$USAF, stations.df$WBAN, sep = "_")
stations.df <- merge(stations.df, distinct(isd.df[, "USAF_WBAN"]))
write.csv(stations.df, file = "tempStations.csv")
