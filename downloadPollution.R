# Downloaded PM2.5 data from EPA on 2/15/2015 http://www.epa.gov/airdata/ad_data_daily.html
library(data.table)

setwd("PM25")

# read in .csv files and rbind together
pm25.df <- as.data.frame(rbindlist(lapply(list.files(), fread)))

setwd("..")

save(pm25.df, file = "pm25_IN_2007_2014.rda")






# # Download PM2.5 data from EPA AQS site http://www.epa.gov/ttn/airs/airsaqs/detaildata/downloadaqsdata.htm
# # go to AQS file
# setwd("AQS")
# 
# # make vector of links
# links <- paste0("http://www.epa.gov/ttn/airs/airsaqs/detaildata/501files/",
#                 c("Rd", rep("RD", 6)), "_501_88101_", 2007:2013, 
#                 c(".Zip", rep(".zip", 6)))
# 
# downloadAQS <- function(url){
#   #   url = links[1]
#   # create a temporary file
#   temp <- tempfile()
#   
#   # download the .zip file to a temporary file
#   download.file(url, temp)
#   
#   # unzip temporary file to your working directory
#   unzip(temp)
#   
#   # delete the temporary file
#   unlink(temp)
# }
# 
# 
# lapply(links[3:7], downloadAQS)











