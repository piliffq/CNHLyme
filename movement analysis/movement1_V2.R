# ********************************************************************************
# Pallavi Kache
# Tick App: Movement Analysis (2018)
# Script 1: Percent Land Cover for each Individual & Figures

# Created: Oct. 28, 2019
# Updated: Nov. 17, 2019
# ********************************************************************************

# PART 1. DATA CONFIGURATION AND DOWNLOAD ----------------------------------------

# 1A. Install Packages
packages=c("rgdal","rgeos","ecodist","lubridate","ggplot2","sp","stringr","maptools","geoR","plyr","classInt","dplyr","SDMtools","spdep")
lapply(packages,library,character.only=TRUE)

# 1B. Read in Data Tables
GPS=read.csv("C:\\Users\\Pallavi Kache\\Documents\\Amazon Photos Downloads\\Diuk-Wasser\\Projects\\TickApp\\data\\clean\\01_GPStotal_database.csv")
# original 2018 movement data 

# PART 2. GET INDIVIDUAL MOVEMENT DATA ------------------------------------------------

# 2A. Select unique individual
user2=GPS[which((GPS$user_id == "2c7138d5-a8a2-5ecb-711f-a9b76a5f41b0")),]

# 2B. Get coordinates for all locations and convert to spatial points
coords=cbind(user2$longitude,user2$latitude)
user2_spdf=SpatialPointsDataFrame(coords = coords, data = user2,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

lon=as.vector(user2_spdf@data$longitude)
lat=as.vector(user2_spdf@data$latitude)

# 2C. Append date/time to spatial data frame
time=ymd_hms(user2_spdf@data$date)

# PART 3. CALCULATE DISTANCE DATA ------------------------------------------------

# 3A. Create "flight" vector, which has the distance between current and previous point
flight=vector()

# 3B. First element of flight vector set to zero, since no distance have been traveled
flight[1]=0

# 3C. We iterate from 2 to the number of observations.
for(i in 2:NROW(lon)){
  Vincenty=SDMTools::distance(lat1=lat[i-1],lon1=lon[i-1],lat2=lat[i],lon2=lon[i]) #package "SDMTools"
  DirectDistance=as.numeric(Vincenty[5]) # fifth element of output is the distance between the points
  {flight[i]=DirectDistance} 
}

# 3D. Create data frame with timestamp and distance traveled
ResultingFrame=data.frame(lon,lat,time,flight)

# 3E. Split time variable to extract only date, bind to "ResultingFrame"
date2=sapply(seq(from=1,to=19,by=10), function(i) substr(ResultingFrame$time, i, i+9))
date2=as.Date(date2[,1])
ResultingFrame=cbind(ResultingFrame,date2)

# 3F. Sum the total distance traveled per day (based on "date2")
totaldist=ResultingFrame %>%
  group_by(date2) %>% summarize(total_dist=sum(flight,na.rm=T))

# 3G. Calculate total distance
totaldist=as.data.frame(totaldist)
totaldist$day=weekdays(as.Date(totaldist$date2))
totaldist$weekend=ifelse(totaldist$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# 3H. Convert to Kilometers
summary(totaldist[which((totaldist$weekend == "Weekend")),"total_dist"])/1000

summary_total=data.frame(unclass(summary(totaldist$total_dist)/1000), check.names = FALSE, stringsAsFactors = FALSE)
summary_total$key=rownames(summary_total)
summary_total$key=paste("overall_",summary_total$key,sep = "")
names(summary_total)=c("dist","key")
summary_total=spread(summary_total, key, dist)

summary_weekday=summary(totaldist[which((totaldist$weekend == "Weekday")),"total_dist"])/1000
summary_total2=data.frame(unclass(summary_weekday), check.names = FALSE, stringsAsFactors = FALSE)
summary_total2$key=rownames(summary_total2)
summary_total2$key=paste("weekday_",summary_total2$key,sep = "")
names(summary_total2)=c("dist","key")
summary_total2=spread(summary_total2, key, dist)
summary_total=cbind(summary_total,summary_total2)
data_wide

write.csv(totaldist,"C:\\Users\\Pallavi Kache\\Documents\\Amazon Photos Downloads\\Diuk-Wasser\\Projects\\TickApp\\data\\clean\\totaldist_User15.csv")
hist(totaldist$total_dist)
