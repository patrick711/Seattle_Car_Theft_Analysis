##--------------------------------------------
##
## Patrick Chen 
## Seattle Car Thefts
##
## Class: PCE Data Science Methods Class
##
## DS350 Final Project
##
##
##
##--------------------------------------------

##----Import Libraries-----
require(logging)
library(sp)
library(rgeos)
library(rgdal)
library(ggplot2)
library(gridExtra)
##-----Declare Functions Here-----
##----Define the get-weather-data function-----

get_weather_data = function(airport, dates, logger=NA, db_conn=NA){
  
  # Build HTML Link String
  site_prefix = 'http://www.wunderground.com/history/airport/'
  site_suffix = '/DailyHistory.html?format=1'
  weather_links = paste0(site_prefix,airport,'/',gsub('-','/',dates),site_suffix)
  
  # Initialize final data frame
  weather_frame = data.frame()
  
  # Get Data
  for (l in weather_links){
    print(paste('Getting link',l))
    # Log each attempt
    if (is.function(logger)){
      loginfo(paste('Getting link',l),logger=logger)
    }
    
    weather_info = tryCatch(readLines(l)[-1], # Get String Response
                            error = function(e){
                              
                              print(paste('Error getting',l)) # Output Error on Screen/Console
                              
                              if(is.function(logger)){loginfo(paste('Error getting',l)
                                                              ,logger=logger)} # Store Error in Log
                              
                            })
    weather_info = strsplit(weather_info,',') # Parse each line by  a comma
    headers = weather_info[[1]]               # Get Headers
    weather_info = weather_info[-1]           # Drop Headers
    
    # Now transform list into data frame
    weather_info = do.call(rbind.data.frame, weather_info)
    names(weather_info) = headers
    
    # Post Retrieval Data Cleanup
    weather_info <- data.frame(lapply(weather_info, as.character),
                               stringsAsFactors=FALSE)
    # Convert numeric columns to numbers
    numeric_cols = c(2,3,4,5,6,8,9,10,13)
    weather_info[numeric_cols] = lapply(weather_info[numeric_cols],as.numeric)
    # Fill in the 'dashes' to zero
    weather_info[is.na(weather_info)]=0
    # Rename the date column and drop the last html tag
    colnames(weather_info)[14]="Date"
    weather_info$Date = as.Date(substr(weather_info$Date,1,10))
    
    # Concatenate DFs together
    weather_frame = tryCatch(rbind(weather_frame, setNames(weather_info, names(weather_frame))),
                             error=function(e) {print(e);weather_frame})
    
    
  } # End loop through each day's weather csv link (l)
  
  # Log ending time
  if(is.function(logger)){
    loginfo('All done!',logger=logger)
  }
  

  
  names(weather_frame) = c('time','temp','dew_pt','humidity','pressure',
                           'visibility','wind_dir','wind_speed','gust_speed',
                           'precipitation','events','conditions',
                           'wind_dir_deg','date')
  
  return(weather_frame)
  
}



##----Run Main Function Here----
if(interactive()){
  
  ##----Setup Test Logger-----
  basicConfig()
  addHandler(writeToFile, file="FinalProject.log", level='DEBUG')
  
  # Set working directory and load data
  ####################################
  loginfo("Setting wd.", logger="data_logger")
  setwd('C:/Users/Patrick/Documents/DS350/project/')
  
  loginfo("Loading Car theft Report datasets", logger="data_logger")
  thefts = read.csv('Thefts2015.csv', stringsAsFactors = FALSE)

  loginfo("Loading Weather Data set - Start", logger="data_logger")
  weather_file_name = 'weather_data2015.csv'
  
  if (weather_file_name %in% list.files()){
    weather_data = read.csv(weather_file_name, stringsAsFactors = FALSE)

  }  else  {
    airport = 'KSEA'
    dates = seq(from=as.Date('2014-12-30'),
                to=as.Date('2015-12-30'),
                by=1)
    
    weather_data = get_weather_data(airport, dates)
    write.csv(weather_data,file="weather_data2015.csv")
  }
  names(weather) = c('time','temp','dew_pt','humidity','pressure',
                     'visibility','wind_dir','wind_speed','gust_speed',
                     'precipitation','events','conditions',
                     'wind_dir_deg','date')
  
  ####################################
  
  #Format and Merge Data
  ###################################
  loginfo("Formating Data", logger="data_logger")
  weather_data$DateFormat = weather_data$date

  weather_data$DateFormat = as.Date(weather_data$DateFormat, format="%Y-%m-%d")

  weather_data$datetime = paste(weather_data$date,weather_data$time)
  weather_data$datetime = strptime(weather_data$datetime, format="%Y-%m-%d %I:%M %p")
  weather_data$Hour = as.numeric(format(round(weather_data$datetime, units="hours"), format="%H"))
  weather_data = weather_data[!duplicated(weather_data[c("date", 'Hour')]),]
  weather_data$date = NULL
  
  thefts$datetime = strptime(thefts$Occurred.Date.or.Date.Range.Start, format="%m/%d/%Y %I:%M:%S %p")
  thefts$Hour = as.numeric(format(round(thefts$datetime, units="hours"), format="%H"))
  thefts$DateFormat = as.Date(thefts$datetime,format="%Y-%m-%d")
  thefts$enddate = strptime(thefts$Occurred.Date.Range.End, format="%m/%d/%Y %I:%M:%S %p")
  thefts$enddate = as.Date(thefts$enddate,format="%Y-%m-%d")
  #thefts$datetime = as.Date(thefts$datetime)
  
  ##----Merge Data-----
  loginfo("Merging datasets", logger="data_logger")
  weather_theft= merge(thefts, weather_data, all.x=TRUE, by=c("DateFormat","Hour"))
  
  census_info = read.csv("spd_beats_pop_.csv",stringsAsFactors = FALSE)
  census_info  = census_info[,c(2,4,5,6,7)]
  names(census_info) = c("Zone.Beat","Area","Pop","Precinct","Sector")
  myrankings = census_info
  myrankings = myrankings[order(myrankings$Area,decreasing = TRUE),]
  myrankings$AreaSizeRank = 1:51
  myrankings = myrankings[order(myrankings$Pop,decreasing = TRUE),]
  myrankings$PopSizeRank = 1:51
  
  weather_mult = merge(weather_theft,myrankings,all.x=TRUE,by="Zone.Beat")
  
  weather_theft = weather_mult[weather_mult$Zone.Beat != "",]
  
  #write.csv(weather_mult,file="weather_mult.csv")
  #write.csv(thefts,file="thefts.csv")
  loginfo("Cleanup DataSet", logger="data_logger")
  #let's get rid of big differences in start and end period
  weather_theft$oneday = weather_theft$enddate - weather_theft$DateFormat
  data = weather_theft[weather_theft$oneday %in% c(0,1),]
  #write.csv(weather_theft,file="weather_theft.csv")
  data$RMS.CDW.ID = NULL
  data$General.Offense.Number = NULL
  data$Offense.Code = NULL
  data$Offense.Code.Extension = NULL
  data$Summary.Offense.Code = NULL
  
  #clean up street name
  data$Hundred.Block.Location = sapply(data$Hundred.Block.Location, function(x) gsub("[0-9]{3}XX BLOCK OF ","",x))
  data$Hundred.Block.Location = sapply(data$Hundred.Block.Location, function(x) gsub("[0-9]{2}XX BLOCK OF ","",x))
  data$Hundred.Block.Location = sapply(data$Hundred.Block.Location, function(x) gsub("[0-9]{1}XX BLOCK OF ","",x))
  ##########################################
  
  
  #Basic slices of data
  #######################################
  loginfo("Data Analysis Start - Street", logger="data_logger")
  #Thefts by Street
  street_theft = table(data$Hundred.Block.Location)
  plot(street_theft,main="Thefts by Street", ylab="Thefts")
  street_df = data.frame(street_theft)
  street_df = street_df[street_df$Freq>17,]
  #street_df = street_df[street_df$Freq>20,]
  street_df = street_df[order(street_df$Freq),]
  street_df$Var1 = paste(c(1:9),rep(". ",9),street_df$Var1)
  barplot(street_df$Freq,names.arg = c(1:9),main="Streets with most Thefts",bty="L",beside = T,xlim=c(0,9),width = c(.45,.45,.45,.45,.45,.45))
  + legend("right",legend=street_df$Var1,xpd = TRUE)

  #theffts by time
  loginfo("Data Analysis - Time", logger="data_logger")
  date_theft = table(data$DateFormat)
  plot(date_theft,main="Thefts over 2014",ylab="Thefts")
  month_theft = table(data$Month)
  barplot(month_theft,main="Thefts over 2014 by month",ylab="Thefts")
  hour_theft = table(data$Hour)
  barplot(hour_theft,main="Thefts by the Hour",ylab="Thefts")
  #This is a useless graph since this is merely the beginning hour of the theft window
  
  
  loginfo("Data Analysis - Location/SPD", logger="data_logger")
  sector_theft = table(data$Zone.Beat)
  mysectors = data.frame(sector_theft)
  barplot(sort(sector_theft),main="Thefts by Police Sector",ylab="Thefts")
  
  zone_theft = table(data$Zone.Beat)
  barplot(sort(zone_theft),main="Thefts by Police Beat",ylab="Thefts")
  
  precinct_theft=table(data$Precinct)
  barplot(precinct_theft,main="Thefts by Precinct",ylab="Thefts")
  
  #by precinct and Zone
  precinct_zone = table(data$Zone.Beat,data$Precinct)
  barplot(precinct_zone,main="Thefts by Precinct and Beat",ylab="Thefts")
  
  test = precinct_zone[,"NORTH"]
  barplot(sort(test[test>0]),main="Thefts by Precinct",xlab="North Precinct",ylim = c(0,120),ylab="Thefts")
  test = precinct_zone[,"EAST"]
  barplot(sort(test[test>0]),main="Thefts by Precinct",xlab="East Precinct",ylim = c(0,120),ylab="Thefts")
  test = precinct_zone[,"SOUTH"]
  barplot(sort(test[test>0]),main="Thefts by Precinct",xlab="South Precinct",ylim = c(0,120),ylab="Thefts")
  test = precinct_zone[,"WEST"]
  barplot(sort(test[test>0]),main="Thefts by Precinct",xlab="West Precinct",ylim = c(0,120),ylab="Thefts")
  test = precinct_zone[,"SOUTHWEST"]
  barplot(sort(test[test>0]),main="Thefts by Precinct",xlab="Southwest Precinct",ylim = c(0,120),ylab="Thefts")

  ######################################
  
  
  ##Create Linear model against time and weather events
  ########################################
  loginfo("Data Analysis - find vars by Linear Regression", logger="data_logger")
  data$events[data$events == ""] = "None"
  data$events[is.na(data$events)] = "None"
  data$conditions[is.na(data$conditions)] = "None"
  data$DayOfWeek = data$datetime.x$wday
  day_theft = table(data$DayOfWeek)
  barplot(day_theft,main="Thefts by Day",ylab="Thefts",xlim=c(0,7),width = c(.6,.6,.6,.6,.6,.6,.6))
  leg = c("0. Mon","1. Tues","2. Wed","3. Thurs","4. Fri","5. Sat","6. Sun")
  + legend("right",legend=leg,xpd = TRUE)

  
  data$weekend = as.numeric(data$DayOfWeek %in% c(6,7,1))
  weekend_theft = table(data$weekend)
  barplot(weekend_theft,main="Thefts on the Weekend(1) or Not(0)",ylab="Thefts")
  
  data$DayNumber <- strftime(data$datetime.x,format="%j")
  sum_per_day <- table(data$DayNumber)
  #barplot(sum_per_day)
  sum_df = data.frame(sum_per_day,stringsAsFactors = false)
  names(sum_df) = c("DayNumber","Sum")
  data_with_sums= merge(data, sum_df, all.x=TRUE, by=c("DayNumber"))
  data_with_sums$datetime.x = NULL
  data_with_sums$datetime.y = NULL
  data_with_sums$DateFormat = NULL
  data_with_sums$enddate = NULL
  data_with_sums$Summarized.Offense.Description = NULL
  data_with_sums$Offense.Type = NULL
  data_with_sums$Date.Reported = NULL
  data_with_sums$Occurred.Date.Range.End = NULL
  data_with_sums$Occurred.Date.or.Date.Range.Start = NULL
  data_with_sums$District.Sector = NULL
  data_with_sums$Zone.Beat = NULL
  data_with_sums$Census.Tract.2000 = NULL
  data_with_sums$Longitude= NULL
  data_with_sums$Latitude = NULL
  data_with_sums$Location = NULL
  data_with_sums$X = NULL
  data_with_sums$Hundred.Block.Location = NULL
  data_with_sums$DayNumber = NULL
  data_with_sums$time = NULL
  data_model = lm(Sum ~ ., data = data_with_sums)
  summary(data_model)
  loginfo("Analyze by Temp", logger="data_logger")
  #temp is listed as significant, let's examine
  plot(x=data_with_sums$temp,y= data_with_sums$Sum,data=data_with_sums,main="Thefts by Temp, Stochaistic or not?",ylab="Number of thefts",xlab="Temperature(F)")
  data_with_sums = data_with_sums[order(data_with_sums$Sum),]
  data_top_days = tail(data_with_sums,50)
  #plot(x=data_top_days$temp,y=data_top_days$Sum,data=data_top_days,ylim=c(25,29),main="Highest 50 Thefts by Temp")
  
  plot(x=data_top_days$temp,y=data_top_days$Sum,data=data_top_days,ylim=c(25,29),main="Highest 50 Thefts by Temp",xlab="Temperature",ylab="Day's Theft Number")
  test = table(data_top_days$Sum,data_top_days$temp)
  #barplot(test)
  
  ########################################3
  
  
  #Thefts per capita and by area  
  ###########################################
  loginfo("Data Analysis by Area/Pop", logger="data_logger")
  sum_by_Zone <- table(data$Zone.Beat)
  sumzone <- data.frame(sum_by_Zone)
  names(sumzone)<-c("Zone.Beat","Thefts")
  data_with_zones= merge(census_info, sumzone, all.x=TRUE, by=c("Zone.Beat"))
  data_with_zones$Pop = data_with_zones$Pop/1000
  
  data_with_zones$percapita = data_with_zones$Thefts/data_with_zones$Pop
  data_with_zones$Area = data_with_zones$Area/1000
  data_with_zones$perarea = data_with_zones$Thefts/data_with_zones$Area
  
  data_with_zones = data_with_zones[order(data_with_zones$percapita),]
  barplot(data_with_zones$percapita, names.arg=data_with_zones$Zone.Beat,main="Per capita car theft per 1000 residents by SPD Beat")
  data_best5_percapita = tail(data_with_zones,5)
  barplot(data_best5_percapita$percapita,names.arg = data_best5_percapita$Zone.Beat,main="Highest 5 per capita theft per 1000 residents by SPD Beat")
  topPop = data_best5_percapita$Zone.Beat
  perpop = myrankings[myrankings$Zone.Beat %in% topPop,]
  ggplot()
  grid.table((perpop))
  
  data_with_zones = data_with_zones[order(data_with_zones$perarea),]
  barplot(data_with_zones$perarea,names.arg = data_with_zones$Zone.Beat,main="Car thefts per km by SPD Beat")
  data_best5_perarea = tail(data_with_zones,10)
  barplot(data_best5_perarea$perarea,names.arg = data_best5_perarea$Zone.Beat,main="Highest 10 car thefts concentration by km by SPD Beat")
  topZones = data_best5_perarea$Zone.Beat
  perarea = myrankings[myrankings$Zone.Beat %in% topZones,]
  ggplot()
  grid.table(perarea)
  #########################3
  

  #Let's plot these on a map
  #############################
  loginfo("Plot thefts on a map of Seattle.", logger="data_logger")
  library(ggmap)
  mapSeattle <- get_googlemap("Seattle",scale=2,zoom=11, maptype = 'roadmap')
  seattle <-ggmap(mapSeattle,extent="device")
  
  seattle + geom_point(aes(x=Longitude,y=Latitude),data=data,size=2)


  
  
}




