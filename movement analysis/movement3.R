##########################
# Pallavi Kache
# Tick App: Movement Analysis (2018)
# Script 3: Percent Land Cover for each Individual

# Created: Oct. 28, 2019
# Updated: Nov. 17, 2019
#

# PART 1. DATA CONFIGURATION AND DOWNLOAD ----------------------------------------

# 1A. Install Packages
packages=c("plyr","ggplot2","devtools","reshape2","dplyr","raster","sp","rgdal","stringr","reshape","classInt")
lapply(packages,library,character.only=TRUE)

# 1B. Read in Data
movement=read.csv("C:\\Users\\Pallavi Kache\\Documents\\Amazon Photos Downloads\\Diuk-Wasser\\Projects\\TickApp\\data\\clean\\mobilitystats_7days_V3.csv")
users=read.csv("C:\\Users\\Pallavi Kache\\Documents\\Amazon Photos Downloads\\Diuk-Wasser\\Projects\\TickApp\\data\\clean\\implementation_paper.csv")
states=readOGR("C:\\Users\\Pallavi Kache\\Documents\\Amazon Photos Downloads\\Diuk-Wasser\\Projects\\TickApp\\data\\shapefiles\\states",layer="states")
GPS=read.csv("C:\\Users\\Pallavi Kache\\Documents\\Amazon Photos Downloads\\Diuk-Wasser\\Projects\\TickApp\\data\\clean\\GPStotal_database.csv")
nlcd=raster("C:\\Users\\Pallavi Kache\\Documents\\Amazon Photos Downloads\\Diuk-Wasser\\Projects\\TickApp\\data\\shapefiles\\NLCD_2016_Land_Cover_L48_20190424 (1)\\NLCD_2016_Land_Cover_L48_20190424.img")

# 1C. Select baseline survey variables of interest
vars_users=c("user_id","County","State","Region","Town","Zipcode","Age","Gender","previousTBD","pastLD",
             "pastTick","work_outdoors","outdooractivity","peridomesticactivity","check.tick",
             "Housetype","pet","pet.tick","ndogs","Tickdogs","ncats","Tickcats")
users2=users[vars_users]

# 1D. Join summary movement data with new baseline survey data
movement2=join(movement,users2,by="user_id",type="left",match="all")

# PART 2. GENERATE PERCENT LAND COVER FOR EACH INDIVIDUAL ------------------
# *** NEED TO CONVERT TO FUNCTION/FOR LOOP*****

# 2A. Select individual user id
user_select=GPS[which((GPS$user_id == "c2ea2ba7-44b1-58b3-6a35-e8102a1f36cc")),]

# 2B. Pull coordinates and convert to shapefile (with projection matching NLCD raster)
coordinates(user_select)= ~longitude + latitude
proj4string(user_select)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
points=spTransform(user_select, projection(nlcd))

# 2C. Extract NLCD values for all points (with no buffer)
landcover1=extract(nlcd, points, buffer = NULL)

# 2D. Append day of the week for future analyses (did not use)
user_select@data$dayweek=weekdays(as.Date(user_select@data$date))

# 2D. Append land cover class for each point
user_select@data=cbind(user_select@data,landcover1)

# 2E. Generate table of all land cover classes for each day (and sum total number of points in the day)
user1=table(user_select@data$day,user_select@data$landcover1)
user1=cbind(user1,rowSums(user1))

# 2E. Determine proportion in each land cover class per day 
user1=user1[,1:((dim(user1)[2])-1)]/user1[,dim(user1)[2]]*100

# 2F. Remove missing days 
user1=user1[complete.cases(user1),]

# 2G. Label columns
user1=as.data.frame.matrix(cbind(as.data.frame(user_WI[19,]),user1))
names(user1)=c("user_id",paste("prop_",colnames(user1)[2:length(user1)],sep = ""))

# 2H. Take the mean of every column to determine average time spend per day
user1=colMeans(user1[2:length(user1)])
user1=round(user1,2)
user1

# Part 3. FIGURES --------------------------------------------------------------

# Change violin plot colors by groups
movement_landcover[is.na(movement_landcover)]=0
movement_landcover=movement_landcover[,c(1:2,5:8,10,12)]
movement_landcover2=melt(movement_landcover, id.vars=c("ID", "user_id"))
movement_landcover2=join(movement_landcover2,users2[,c("user_id","State","Age","Gender","pastLD","pastTick","Housetype")],by="user_id",type="left",match="all")
movement_landcover2_WINY=movement_landcover2[which(movement_landcover2$State == c("Wisconsin","New York")),]

p_state=ggplot(movement_landcover2_WINY, aes(x=variable, y=value,fill=State)) +
  geom_violin() + scale_y_log10() 
p_state

p_LD=ggplot(movement_landcover2_WINY, aes(x=variable, y=value,fill=pastLD)) +
  geom_violin()+scale_y_log10()
p_LD

p_house=ggplot(movement_landcover2_WINY, aes(x=variable, y=value,fill=Housetype)) +
  geom_violin()+scale_y_log10()
p_house

#################################################################################
# FINAL DATABASES. --------------------------------------------------------------

#LANDCOVER DATA
#movement_landcover=read.csv("C:\\Users\\Pallavi Kache\\Documents\\Amazon Photos Downloads\\Diuk-Wasser\\Projects\\TickApp\\data\\clean\\mobility_landcover.csv")

#FINAL DATABASE
movement3=join(movement2,movement_landcover,by="user_id",type="left",match="all")
write.csv(movement3,"C:\\Users\\Pallavi Kache\\Documents\\Amazon Photos Downloads\\Diuk-Wasser\\Projects\\TickApp\\data\\mobilitystats_V4.csv")


#################################################################################
# IN PROGRESS. FOR LOOP FOR PART 2 ---------------------------------------------

list.files(
  path=c("c:/program files", "c:/program files (x86)"), 
  pattern="git.exe", 
  full.names=TRUE,
  recursive=TRUE
)

users=unique(movement2$user_id)

flw=vector("list", length(users))

for(i in seq_along(users)){
  print(users[i])
  
  
  
}


movement4 = NULL
for (i in 1:nrow(movement2)[3:4])
{
  temp = grepl(x=movement2@data[,"user_id"],pattern=movement2@data[i],perl=TRUE)
}


for (i in movement2) {
  if(any(variable.names(GPS)== i)) {assign(i, movement4[,c(i)])}
  coordinates(movem)= ~longitude + latitude
  
}