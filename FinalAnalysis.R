############################################################################################################
################################################ INITIALISING ############################################## 
############################################################################################################


#### Install Packages and Load Data ####
if(!"dplyr" %in% rownames(installed.packages())) {
  install.packages("dplyr") 
}
library(dplyr)

if(!"tidyr" %in% rownames(installed.packages())) {
  install.packages("tidyr") 
}
library(tidyr)

if(!"ggplot2" %in% rownames(installed.packages())) {
  install.packages("ggplot2") 
}
library(ggplot2)

if(!"scales" %in% rownames(installed.packages())) {
  install.packages("scales") 
}
library(scales)

if(!"chron" %in% rownames(installed.packages())) {
  install.packages("chron") 
}
library(chron)

if(!"corrplot" %in% rownames(installed.packages())) {
  install.packages("corrplot") 
}
library(corrplot) # Correlation plot


if(!"Amelia" %in% rownames(installed.packages())) {
  install.packages("Amelia") 
}
library(Amelia)  # Missing data package


if(!"reshape2" %in% rownames(installed.packages())) {
  install.packages("reshape2") 
}
library(reshape2)  # Reshaping data (melt function)


if(!"GGally" %in% rownames(installed.packages())) {
  install.packages("GGally") 
}
library(GGally)  # pairs plot

if(!"glmmTMB" %in% rownames(installed.packages())) {
  install.packages("glmmTMB") 
}
library(glmmTMB)  # Neg bin models


# Import data
raptors <- read.csv("~/Desktop/Edinburgh MSc/DISSERTATION/Raptors (1)/Data/HMworkingdatav2.csv",
                    header = TRUE) %>% 
  .[, colSums(is.na(.)) != nrow(.)] # Remove columns with only NAs

# Import bird acronym key (manually made)
BirdKey <- read.csv("~/Desktop/Edinburgh MSc/DISSERTATION/Raptors (1)/Data/BirdKey.csv", 
                    header=FALSE)









############################################################################################################
################################################ CLEANING: PT I  ############################################## 
############################################################################################################

str(raptors)

#### Change Date Format ####

# Change to date format
raptors$Date <- as.Date(raptors$Date,format="%d-%b-%Y")

# Change to time format
raptors$Start <- strptime(as.character(raptors$Start), format = '%H:%M') %>% format("%H:%M:%S") %>% times 
raptors$End <- strptime(as.character(raptors$End), format = '%H:%M') %>% format("%H:%M:%S") %>% times

# Change name of observer 2 column to be consistent
names(raptors)[grepl("Observer2.new", names(raptors))] <- "Observer.2"

# Order by date
raptors <- raptors %>%
  arrange(Date)



#### New Variables  ####
# Add in number of observers (based on IDs given. i.e. number of populated observer ID columns)
raptors$No.Observers <- 4 - rowSums(is.na(raptors[,grepl( "Observer." , names(raptors))])) 

# Remove white spaces in observers and counters IDs
raptors$Counter <- sub(' ', '', raptors$Counter)
raptors$Observer.1 <- sub(' ', '', raptors$Observer.1)
raptors$Observer.2 <- sub(' ', '', raptors$Observer.2)
raptors$Observer.3 <- sub(' ', '', raptors$Observer.3)
raptors$Observer.4 <- sub(' ', '', raptors$Observer.4)

# If wind direction is blank, change to NA
raptors[which(raptors$Wind.Dir ==""),]$Wind.Dir <- NA

# Add in season indicator
raptors$Season <- ifelse(raptors$Month %in% c(8,9,10,11,12,1), "Fall (South)", "Spring (North)")








############################################################################################################
################################################ MISSINGNESS ############################################## 
############################################################################################################




#### Quantifying missingness ####
missmap(raptors)


#### Imputation: Part I ####

# FLIGHT HEIGHT
table(is.na(raptors$Flight.HT))
table(is.na(raptors$Flight.HT), raptors$TOTAL) 
raptors$Flight.HT.impute <- FALSE # Imputation flag
raptors[which(is.na(raptors$Flight.HT)==FALSE & raptors$TOTAL==0),]$Flight.HT.impute <- TRUE
raptors[which(is.na(raptors$Flight.HT)==FALSE & raptors$TOTAL==0),]$Flight.HT <- NA




# FLIGHT DIRECTION
table(is.na(raptors$Flight.DIR))
table(is.na(raptors$Flight.DIR), raptors$TOTAL) 
raptors$Flight.DIR.impute <- FALSE # Imputation flag
raptors[which(is.na(raptors$Flight.DIR)==FALSE & raptors$TOTAL==0),]$Flight.DIR.impute <- TRUE
raptors[which(is.na(raptors$Flight.DIR)==FALSE & raptors$TOTAL==0),]$Flight.DIR <- NA

# But looking at other flight directions it seems like this can be inferred with a fairly accurate degree:
table(raptors[which(raptors$Month==2 | raptors$Month==3 | raptors$Month==4),]$Flight.DIR) # All feb, march, april are north/nne (mainly north)
table(raptors[which(raptors$Month==5 | raptors$Month==6 | raptors$Month==7),]$Flight.DIR) # Nothing for may, june, july
table(raptors[which(raptors$Month==8 | raptors$Month==9 | raptors$Month==10),]$Flight.DIR) # All south for august, september, october
table(raptors[which(raptors$Month==11),]$Flight.DIR) # mix of s (70), SSw (7), sw(1) and w(5) for november
table(raptors[which(raptors$Month==12),]$Flight.DIR) # All south for december (none for jan)
raptors[which(is.na(raptors$Flight.DIR) & (raptors$Month ==2 | raptors$Month ==3 | raptors$Month ==4)),]$Flight.DIR.impute <- TRUE
raptors[which(is.na(raptors$Flight.DIR) & (raptors$Month ==2 | raptors$Month ==3 | raptors$Month ==4)),]$Flight.DIR <- "N"
raptors[which(is.na(raptors$Flight.DIR) & (raptors$Month ==8 | raptors$Month ==9 | raptors$Month ==10 | raptors$Month ==11 | raptors$Month ==12)),]$Flight.DIR.impute <- TRUE
raptors[which(is.na(raptors$Flight.DIR) & (raptors$Month ==8 | raptors$Month ==9 | raptors$Month ==10 | raptors$Month ==11 | raptors$Month ==12)),]$Flight.DIR <- "S"

# Now only missing values are from records in May. However, they are the ONLY records in May... and no birds were spotted so no need to impute




# HUMIDITY

table(is.na(raptors$Humidity)) # No NA values
table(raptors$Humidity) # A humidity of 0 and 127 are not possible - assume these are missing (now 46 observations missing humidity)

# If there are other records available for that day, take the record closest to that time period to impute the missing data (if multiple, take average. e.g. hour before and after)
# If there are no other records, for now make NA
raptors[which(raptors$Humidity==0 | raptors$Humidity>100),]$Humidity <- NA # Change records to NA
raptors$Humidity.impute <- FALSE # Flag indicating imputation

min.time.diff <- numeric()
toimpute <- raptors %>% filter(is.na(Humidity)) # Records up for imputation
indices <- which(is.na(raptors$Humidity)) # Indices for records up for imputation
otherrecords <- raptors %>% filter((Date %in% toimpute$Date) & (is.na(Humidity)==FALSE)) # Other records on the same day as those being imputed, but with non-missing Humidity values
for(i in 1:length(indices)){ # Iterate through records needing imputation
  othervals <- otherrecords[which(otherrecords$Date == toimpute[i,]$Date), ] # Other records for that day
  if(nrow(othervals)==0){ # If no other records for that day with non-missing humidities, go to next iteration
    next
  }
  timediff <- (hours(othervals$Start) - hours(toimpute[i,]$Start)) %>% abs # Otherwise, find time difference between records and the missing value
  min.time.diff <- c(min.time.diff, min(timediff))
  raptors[indices[i],]$Humidity <- mean(othervals[which(timediff==min(timediff)),]$Humidity) # Take records for mean difference closest to the time, average and make these the new humidity
  raptors[indices[i],]$Humidity.impute <- TRUE # Change imputation indicator to TRUE
}

table(is.na(raptors$Humidity)) # Imputed 3 values, 43 remain NA.
min.time.diff




# PRESSURE
table(is.na(raptors$BARO)) # No NAs
table(raptors$BARO) # But zero in quite a few records (and 0.85 in one) and pressures close to 40 - assume all are incorrect

# Assume pressures >38 are just out by a unit of 10. Subtract 10 to correct these.
raptors[which(raptors$BARO>38),]$BARO <- raptors[which(raptors$BARO>38),]$BARO - 10

# For the other records, assume NA. If there are other records available for that day, take the record closest to that time period to impute the missing data (if multiple, take average. e.g. hour before and after)
# If there are no other records, for now make NA
raptors[which(raptors$BARO < 20),]$BARO <- NA # Change records to NA
raptors$BARO.impute <- FALSE # Flag indicating imputation

min.time.diff <- numeric()
toimpute <- raptors %>% filter(is.na(BARO)) # Records up for imputation
indices <- which(is.na(raptors$BARO)) # Indices for records up for imputation
otherrecords <- raptors %>% filter((Date %in% toimpute$Date) & (is.na(BARO)==FALSE)) # Other records on the same day as those being imputed, but with non-missing values
for(i in 1:length(indices)){ # Iterate through records needing imputation
  othervals <- otherrecords[which(otherrecords$Date == toimpute[i,]$Date), ] # Other records for that day
  if(nrow(othervals)==0){ # If no other records for that day with non-missing values, go to next iteration
    next
  }
  timediff <- (hours(othervals$Start) - hours(toimpute[i,]$Start)) %>% abs # Otherwise, find time difference between records and the missing value
  min.time.diff <- c(min.time.diff, min(timediff))
  raptors[indices[i],]$BARO <- mean(othervals[which(timediff==min(timediff)),]$BARO) # Take records for mean difference closest to the time, average and make these the new value
  raptors[indices[i],]$BARO.impute <- TRUE # Change imputation indicator to TRUE
}

table(is.na(raptors$BARO)) # 6 values imputed, 52 remain NA.
min.time.diff



# CLOUD COVER
table(raptors$Cloud.Cover); table(is.na(raptors$Cloud.Cover))

# Assume a cloud cover of 127 is incorrect
raptors[which(raptors$Date == "2018-08-31"),]
# Other values available for the same day. Take average of the cloud covers given in the hour preceding and following the missing value (85 and 55)
raptors$Cloud.Cover.impute <- FALSE
raptors[which(raptors$Cloud.Cover>100),]$Cloud.Cover.impute <- TRUE
raptors[which(raptors$Cloud.Cover>100),]$Cloud.Cover <- mean(c(55,85))




# VISIBILITY
table(is.na(raptors$Visibility)) # 2 missing values
table(raptors$Visibility)

# Mix of units - convert these
raptors$Visibility <- raptors$Visibility %>% as.character
raptors$Visibility <- ifelse(raptors$Visibility=="1/2", "0.5", raptors$Visibility)
raptors$Visibility <- ifelse(raptors$Visibility=="1/4", "0.25", raptors$Visibility)
raptors$Visibility <- raptors$Visibility %>% as.numeric

# Missing values:
# Take average of the visibilities given in the hour preceding and following the missing value to impute
raptors$Visibility.impute <- FALSE
raptors[is.na(raptors$Visibility) & raptors$Date=="2018-11-17",]$Visibility.impute <- TRUE
raptors[is.na(raptors$Visibility) & raptors$Date=="2018-11-17",]$Visibility <- mean(c(10, 15))
raptors[is.na(raptors$Visibility) & raptors$Date=="2019-03-16",]$Visibility.impute <- TRUE
raptors[is.na(raptors$Visibility) & raptors$Date=="2019-03-16",]$Visibility <- 25

lm(Visibility~ Precipitation+ Cloud.Cover, data=raptors) %>% summary # Captured in other variables


# WIND DIRECTION

# Wind direction
table(is.na(raptors$Wind.Dir)); table(raptors$Wind.Dir) # 8 missing values, 20 variable

# For missing variables where the hour either side of the missing value, the direcions agree, OR where all other records on that day have the same direction, use these to impute the missing value
# If else, keep as NA as no information available to infer
raptors$Wind.Dir.impute <- FALSE
raptors[is.na(raptors$Wind.Dir) & raptors$Date=="2018-08-16",]$Wind.Dir.impute <- TRUE
raptors[is.na(raptors$Wind.Dir) & raptors$Date=="2018-08-16",]$Wind.Dir <- "SW"
raptors[is.na(raptors$Wind.Dir) & raptors$Date=="2018-08-26",]$Wind.Dir.impute <- TRUE
raptors[is.na(raptors$Wind.Dir) & raptors$Date=="2018-08-26",]$Wind.Dir <- "W"
raptors[is.na(raptors$Wind.Dir) & raptors$Date=="2018-09-01",]$Wind.Dir.impute <- TRUE
raptors[is.na(raptors$Wind.Dir) & raptors$Date=="2018-09-01",]$Wind.Dir <- "SE"
raptors[is.na(raptors$Wind.Dir) & raptors$Date=="2018-10-30",]$Wind.Dir.impute <- TRUE
raptors[is.na(raptors$Wind.Dir) & raptors$Date=="2018-10-30",]$Wind.Dir <- "W"
raptors[is.na(raptors$Wind.Dir) & raptors$Date=="2018-11-03",]$Wind.Dir.impute <- TRUE
raptors[is.na(raptors$Wind.Dir) & raptors$Date=="2018-11-03",]$Wind.Dir <- "W"
raptors[is.na(raptors$Wind.Dir) & raptors$Date=="2019-03-02",]$Wind.Dir.impute <- TRUE
raptors[is.na(raptors$Wind.Dir) & raptors$Date=="2019-03-02",]$Wind.Dir <- "W"
# Now just 3 missing values




# PRECIPITATION
plot(raptors$Precipitation, raptors$TOTAL, xlab="Precipitation", ylab="Total per Watch")
table(is.na(raptors$Precipitation)) # 23 missing values



# COUNTER
raptors[is.na(raptors$Counter),]; table(is.na(raptors$Counter)) # 9 missing values
raptors[is.na(raptors$Counter),]$Counter <- "Missing"


#### Imputation ####



# CORRELATION BETWEEN WEATHER VARIABLES (NUMERIC ONLY)
cor.data <- raptors %>%
  mutate(Start = hours(Start)) %>%
  filter(Flight.HT.impute==FALSE,
         Flight.DIR.impute==FALSE,
         Humidity.impute==FALSE,
         BARO.impute==FALSE,
         Wind.Dir.impute==FALSE,
         Cloud.Cover.impute==FALSE,
         Visibility.impute==FALSE) %>%  # Remove rows with any imputation
  select(Wind.Spd2, Temp, Humidity, Visibility,
         BARO, Cloud.Cover) # Variables we're interested in

names(cor.data) <- c("Wind Speed", "Temp.", "Humidity", "Pressure", "Cloud Cover", "Visibility")

corrplot(cor(na.omit(cor.data)), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)




# REGRESSION IMPUTATION: BARO PRESSURE
ggplot(raptors, aes(x=Temp, y=BARO)) + geom_point() + labs(x="Temperature", y="Pressure")
imputation = lm(BARO~Temp+Month+Start, data = raptors) #Create the linear regression
summary(imputation)
raptors[is.na(raptors$BARO),]$BARO.impute <- TRUE
raptors[is.na(raptors$BARO),]$BARO <- predict(imputation, newdata = raptors[is.na(raptors$BARO),])




# REGRESSION IMPUTATION: HUMIDITY
ggplot(raptors, aes(x=Temp, y=Humidity)) + geom_point() + labs(x="Temperature", y="Humidity") + facet_grid(col=vars(Precipitation))
table(is.na(raptors$Humidity), is.na(raptors$Precipitation))
table(is.na(raptors$Humidity), is.na(raptors$Cloud.Cover))
table(is.na(raptors$Humidity), is.na(raptors$Temp))
imputation = lm(Humidity~Temp+Month+Start+Precipitation2+Cloud.Cover, data = raptors) #Create the linear regression
raptors[is.na(raptors$Humidity),]$Humidity.impute <- TRUE
raptors[is.na(raptors$Humidity),]$Humidity <- predict(imputation, newdata = raptors[is.na(raptors$Humidity),])






############################################################################################################
################################################ CLEANING: PT III ############################################## 
############################################################################################################


#### Discrepancies ####
# END TIMES
duration <- hours(raptors$End - raptors$Start)*60 + minutes(raptors$End - raptors$Start) 
indices <- which(raptors$Duration != duration); length(indices) # Duration by start and end times not equal to recorded duration
toadd <- strptime(raptors[indices,]$Duration, format = '%M') %>% format("%H:%M:%S") %>% times # Durations to add onto start times
raptors[indices,]$End <- raptors[indices,]$Start + toadd




# DOUBLE MARKED AS OBSERVER AND COUNTER
# Number of unique values in each counter/observer ID list
uniquevals <- apply(raptors[,c("Counter", names(raptors)[startsWith(names(raptors), "Observer.")])], 1, function(x) unique(x)) %>%
  lapply(., function(x) x[!is.na(x)]) %>%
  lapply(., function(x) length(x)) %>% unlist 
# Number of values (not unique values) in each counter/observer ID list
populatedvals <- raptors$No.Observers + 1
# Number of observations where total IDs different to unique IDs
length(which(uniquevals != populatedvals)) # 12 rows where someone has been double marked as observer/counter
# Correct this by: reduce number of observers by 1, replace observer ID with NA, correct Observer (time)
raptors[which(uniquevals != populatedvals),]$No.Observers <- raptors[which(uniquevals != populatedvals),]$No.Observers - 1
raptors[which(uniquevals != populatedvals & raptors$No.Observers==0),]$Observer.1 <- NA
raptors[which(uniquevals != populatedvals & raptors$No.Observers==0),]$Observer <- raptors[which(uniquevals != populatedvals & raptors$No.Observers==0),]$Duration
raptors[which(uniquevals != populatedvals & raptors$No.Observers==1),]$Observer.1 <- raptors[which(uniquevals != populatedvals & raptors$No.Observers==1),]$Observer.2
raptors[which(uniquevals != populatedvals & raptors$No.Observers==1),]$Observer.2 <- NA
raptors[which(uniquevals != populatedvals & raptors$No.Observers==1),]$Observer <- 2*raptors[which(uniquevals != populatedvals & raptors$No.Observers==1),]$Duration
raptors[which(uniquevals != populatedvals & raptors$No.Observers==3 & raptors$Month==9 ),]$Observer.3 <- raptors[which(uniquevals != populatedvals & raptors$No.Observers==3 & raptors$Month==9 ),]$Observer.4
raptors[which(uniquevals != populatedvals & raptors$No.Observers==3 & raptors$Month==9 ),]$Observer.4 <- NA
raptors[which(uniquevals != populatedvals & raptors$No.Observers==3 & raptors$Month==9 ),]$Observer <- 4*raptors[which(uniquevals != populatedvals & raptors$No.Observers==3 & raptors$Month==9 ),]$Duration 
raptors[which(uniquevals != populatedvals & raptors$No.Observers==3 & raptors$Month==10 ),]$Observer.2 <- raptors[which(uniquevals != populatedvals & raptors$No.Observers==3 & raptors$Month==10 ),]$Observer.3
raptors[which(uniquevals != populatedvals & raptors$No.Observers==3 & raptors$Month==10 ),]$Observer.3 <- raptors[which(uniquevals != populatedvals & raptors$No.Observers==3 & raptors$Month==10 ),]$Observer.4
raptors[which(uniquevals != populatedvals & raptors$No.Observers==3 & raptors$Month==10 ),]$Observer.4 <- NA
raptors[which(uniquevals != populatedvals & raptors$No.Observers==3 & raptors$Month==10 ),]$Observer <- 4*raptors[which(uniquevals != populatedvals & raptors$No.Observers==3 & raptors$Month==10 ),]$Duration 




# OBSERVERS NOT MAKING SENSE WITH NUMBER OF OBSERVER IDS GIVEN
nrow(raptors[which(raptors$No.Observers < (raptors$Observer/raptors$Duration) - 1),]) # too much time recorded for number of observer IDs
nrow(raptors[which(raptors$No.Observers > (raptors$Observer/raptors$Duration) - 1),]) # too little time recorded for number of observer IDs
raptors$Observers.impute <- FALSE

# We have the following cases:

# 1. Too much time reported, but always a multiple of duration. In these cases, assume the duration is correct again and mark the observers as missing
indices <- which(raptors$No.Observers < (raptors$Observer/raptors$Duration) - 1)
indices1 <- indices[1:(length(indices)-1)] # Indices where no. observers needs correcting to 1
indices2 <- indices[length(indices)] # Indices where no. observers needs correcting to 2
raptors[indices1,]$Observers.impute <- TRUE
raptors[indices1,]$No.Observers <- 1
raptors[indices1,]$Observer.1 <- "Missing"
raptors[indices2,]$Observers.impute <- TRUE
raptors[indices2,]$No.Observers <- 2
raptors[indices2,]$Observer.2 <- "Missing"

# 2. Too little time reported, and this is NOT a multiple of Duration (6 obs)
# Assume observers didn't stay for the whole duration. Do nothing to keep time accurate.

# 3. Too little time reported, and IS a multiple of Duration, as if an observer was mistakenly put on. (3 obs)
indices <- which((raptors$No.Observers > (raptors$Observer/raptors$Duration) - 1) & floor(raptors$Observer/raptors$Duration) == raptors$Observer/raptors$Duration) # All only one observer
raptors[indices,]$Observers.impute <- TRUE
raptors[indices,]$Observer.1 <- NA
raptors[indices,]$No.Observers <- 0


#### New Variables ####


# HOW MANY COUNTERS IN THE OBSERVER GROUP
counters <- unique(raptors$Counter)
str(raptors)
obs1 <- raptors$Observer.1 %in% counters %>% as.numeric
obs2 <- raptors$Observer.2 %in% counters %>% as.numeric
obs3 <- raptors$Observer.3 %in% counters %>% as.numeric
obs4 <- raptors$Observer.4 %in% counters %>% as.numeric
raptors$No.Counters <- obs1 + obs2 + obs3 + obs4
raptors$Counter.In.Oversvers <- ifelse(obs1 + obs2 + obs3 + obs4 >0, "Yes", "No")




# START TIME FROM NOON (absolute)
ggplot(raptors, aes(x=hours(Start), y=TOTAL)) + geom_point() + labs(x="Start Hour", y="Total")# Peak at 12
noon <- strptime("12:00", format = '%H:%M') %>% format("%H:%M:%S") %>% times
raptors$Start.Noon <- NA
raptors[which(raptors$Start >= noon),]$Start.Noon <- (minutes(raptors[which(raptors$Start >=noon),]$Start - noon) + 60*hours(raptors[which(raptors$Start >=noon),]$Start - noon))/60
raptors[which(raptors$Start < noon),]$Start.Noon <- -(minutes(noon - raptors[which(raptors$Start < noon),]$Start) + 60*hours(noon - raptors[which(raptors$Start < noon),]$Start))/60
raptors$Start.Noon <- abs(raptors$Start.Noon)


# PRECIPIATION INTO GROUPS
table(raptors$Precipitation)
raptors$Precipitation.Grouped <- "None"
raptors[which(raptors$Precipitation2 == 1),]$Precipitation.Grouped <- "Haze or Fog"
raptors[which(raptors$Precipitation2 == 2 | raptors$Precipitation2 == 3 | raptors$Precipitation2 == 4),]$Precipitation.Grouped <- "Drizzle or Rain or Thunderstorm"
raptors[which(raptors$Precipitation2 == 5),]$Precipitation.Grouped <- "Snow"
raptors[which(raptors$Precipitation2 == 6),]$Precipitation.Grouped <- "Wind-driven Dust/Sand/Snow"
raptors$Precipitation.Grouped <- raptors$Precipitation.Grouped %>% as.factor



# WIND DIRECTION

raptors$Wind.Dir.2 <- NA
raptors[which(raptors$Wind.Dir=="ENE" | raptors$Wind.Dir=="E" | raptors$Wind.Dir=="ESE"),]$Wind.Dir.2 <- "Easternly"
raptors[which(raptors$Wind.Dir=="SE" | raptors$Wind.Dir=="SSE" | raptors$Wind.Dir=="S"| raptors$Wind.Dir=="SSW"| raptors$Wind.Dir=="SW"),]$Wind.Dir.2 <- "Southernly"
raptors[which(raptors$Wind.Dir=="WSW" | raptors$Wind.Dir=="W" | raptors$Wind.Dir=="WNW"),]$Wind.Dir.2 <- "Westernly"
raptors[which(raptors$Wind.Dir=="NW" | raptors$Wind.Dir=="NNW" | raptors$Wind.Dir=="N"| raptors$Wind.Dir=="NNE"| raptors$Wind.Dir=="NE"),]$Wind.Dir.2 <- "Northernly"

# those with variable wind excluded


raptors$Wind.Dir.Season <- NA
raptors[which(raptors$Wind.Dir.2=="Westernly"),]$Wind.Dir.Season <- "Crosswind (West)"
raptors[which(raptors$Wind.Dir.2=="Easternly"),]$Wind.Dir.Season <- "Crosswind (East)"
raptors[which(raptors$Wind.Dir.2=="Northernly" & raptors$Season=="Spring (North)"),]$Wind.Dir.Season <- "Headwind"
raptors[which(raptors$Wind.Dir.2=="Southernly" & raptors$Season=="Spring (North)"),]$Wind.Dir.Season <- "Tailwind"
raptors[which(raptors$Wind.Dir.2=="Northernly" & raptors$Season=="Fall (South)"),]$Wind.Dir.Season <- "Tailwind"
raptors[which(raptors$Wind.Dir.2=="Southernly" & raptors$Season=="Fall (South)"),]$Wind.Dir.Season <- "Headwind"






#### Wide to Long Format ####
# Row per species per watch

# Columns for bird names
keycol <- "Bird.abbrev"
valuecol <- "Count"
gathercols <- c('BV', 'TV', 'OS', 'BE', 'NH', 'SS', 'CH', 'NG', 'RS', 'BW', 'RT', 'RL', 'GE', 'AK', 'ML', 'PG', 'UA', 'UB', 'UF', 'UE', 'UR')

raptors.long <- gather_(raptors, keycol, valuecol, gathercols)
raptors.long <- raptors.long %>%
  left_join(BirdKey, by = c("Bird.abbrev" = "V1")) # Add in full name
raptors.long <- raptors.long %>% mutate(Bird.full = V2) %>% select(-'V2') # Rename full bird name column

# Remove white space
raptors.long$Bird.full <- trimws(raptors.long$Bird.full, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")


# Rough type of bird

bird.types <- list(
  'Vulture' = c("Black vulture", "Turkey vulture"),
  'Osprey' = c("Osprey"),
  'Eagle' = c("Golden eagle", "Bald eagle", "Unkown eagle"),
  'Accipiter' = c("Sharp-shinned hawk", "Cooper's hawk", "Northern goshawk", "Unknown accipiter (hawk)"),
  'Buteo' = c("Red-shouldered hawk", "Broad-winged hawk", "Red-tailed hawk", "Rough-legged hawk", "Unknown buteo (buzzard)"),
  'Harrier' = c("Northern harrier"),
  'Falcon' = c("American kestrel", "Merlin", "Peregrine falcon", "Unknown falcon"),
  'Unknown raptor' = c("Unknown raptor")
)

raptors.long$Bird.type <- `levels<-`(as.factor(raptors.long$Bird.full), bird.types)




############################################################################################################
################################################ UNIVARIATE ############################################## 
############################################################################################################


#### Abundance (Outcome) ####

# ABUNDANCE 
# Overall abundance - per hour
ggplot(data=raptors, aes(x=Date, y=TOTAL)) + geom_line() + labs(y="Total Raptor Count")
# Overall abundance - per day
raptors %>% group_by(Date) %>% summarise(daily.total.birds=sum(TOTAL)) %>% 
  ggplot(aes(x=Date, y=daily.total.birds)) + geom_line() + labs(y="Daily Total")
# Large numbers driven by the broad winged hawk (>1000).
ggplot(data=raptors, aes(x=Date, y=BW)) +
  geom_line() # 1130 birds in 1 hour at maximum = 283 birds per person-hour


# BIRD TYPE
# Bird species total
birdsums <- data.frame(colnames(raptors[,8:28]), as.numeric(colSums(raptors[,8:28])))
colnames(birdsums) <- c("bird_short", "total")
birdsums <- birdsums %>%
  left_join(BirdKey, by = c("bird_short" = "V1")) # Add in full name
colnames(birdsums)[3] <- "bird"
ggplot(data=birdsums, aes(x=reorder(bird, -total), y=total)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=total), vjust=-1, color="black", size=3.5) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(x="Bird", y="Total")
birdsums

# Bird type total
raptors.long %>% group_by(Bird.type) %>% summarise(type.total = sum(Count)) %>%
  ggplot(aes(x=Bird.type, y=type.total)) + geom_bar(stat="identity") + 
  geom_text(aes(label=type.total), vjust=-1, color="black", size=3.5)



#### Weather ####
ggplot(raptors, aes(x=Temp)) + 
  geom_histogram(binwidth=1)

ggplot(raptors, aes(x=Humidity)) + 
  geom_histogram(binwidth=1)

ggplot(raptors, aes(x=BARO)) + 
  geom_histogram(binwidth=0.2)

ggplot(raptors, aes(x=Cloud.Cover)) + 
  geom_histogram(binwidth=3)

raptors[!is.na(raptors$Wind.Dir),] %>% ggplot(aes(x=Wind.Dir)) + 
  geom_bar()
# Still have 3 NA values, omitted here

raptors[!is.na(raptors$Wind.Dir.Angle.SEASON),] %>% ggplot(aes(x=Wind.Dir.Angle.SEASON)) + 
  geom_histogram(binwidth=22.5)
# Still have 23 NA values, omitted here

ggplot(raptors, aes(x=Wind.Spd)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

raptors[!is.na(raptors$Precipitation),] %>% ggplot(aes(x=Precipitation)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
# Still have 23 NA values, omitted here

raptors[!is.na(raptors$Precipitation.Grouped),] %>% ggplot(aes(x=Precipitation.Grouped)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
# Still have 23 NA values, omitted here

ggplot(raptors, aes(x=Visibility)) + 
  geom_histogram(binwidth=1)




#### Bird flights ####

ggplot(raptors, aes(x=Flight.DIR)) + 
  geom_bar() 

ggplot(raptors, aes(x=Flight.HT)) + 
  geom_bar()  +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

# Flying in groups?
table(raptors.long$Count)
table(raptors.long$Count, raptors.long$Bird.full) %>% as.data.frame %>% filter(Freq>0) %>%View
# Broad-winged hawk, red-tailed hawk, sharp-shinned hawk, turkey vultures the only ones who seem to 
#   travel in large kettles?



### Observing Factors (Other) ###

# Number of observers
ggplot(raptors, aes(x=No.Observers)) +
  geom_bar()

# Frequency of different IDs observing (observer NOT counter)
raptors[,startsWith(names(raptors), "Observer.")] %>% 
  unlist %>% as.numeric %>%
  table() %>% as.data.frame %>%
  ggplot(aes(x=reorder(., -Freq), y=Freq)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) # NAs introduced by coercion for non-filled in entries. It's fine

# Frequency of IDs counting (counter NOT observer)
raptors %>% 
  select(Counter) %>%
  table() %>% as.data.frame %>%
  ggplot(aes(x=reorder(., -Freq), y=Freq)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Frequency of ALL people watching (observer OR counter)
raptors[,c("Counter", names(raptors)[startsWith(names(raptors), "Observer.")])] %>% 
  unlist %>% as.numeric %>%
  table() %>% as.data.frame %>%
  ggplot(aes(x=reorder(., -Freq), y=Freq)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Time spent watching
ggplot(raptors, aes(x=Duration)) + 
  geom_bar()
ggplot(raptors, aes(x=Observer)) + 
  geom_bar()
# Hours per day
raptors %>% group_by(Date) %>% summarise(total.time=sum(Duration)/60) %>%
  ggplot(aes(x=Date, y=total.time)) + geom_point()


# Start times
ggplot(raptors, aes(x=hours(Start))) + geom_bar()


# How many different teams
test <- apply(unique(raptors[,c("Counter", names(raptors)[startsWith(names(raptors), "Observer.")])][1:4]),1, as.vector)
list <- vector("list", ncol(test))
for (i in 1:ncol(test)){
  vec <- test[,i]
  vec <- sort(vec)%>% na.omit
  list[[i]] <- vec
}
test <- unique(list)
length(test) #305 individual teams


############################################################################################################
################################################ MULTIVARIATE  ############################################## 
############################################################################################################

# PAIRS PLOTS
columns <- c("Wind.Spd2", "Temp", "Humidity", "BARO", "Cloud.Cover", "Visibility", "Precipitation2", "TOTAL")
ggpairs(raptors[,which(names(raptors) %in% columns)])+ theme_bw()


# RELATIONSHIP WITH ABUNDANCE

# Precipitation and abundance
# Frequency of counts for no precipitation vs haze and fog
tabhaze <- prop.table(table(model.data[which(model.data$Precipitation.Grouped=="Haze or Fog"),]$Count)) %>% as.data.frame 
tabnone <- prop.table(table(model.data[which(model.data$Precipitation.Grouped=="None"),]$Count))  %>% as.data.frame
names(tabhaze) <- c("Total", "Haze")
names(tabnone) <- c("Total", "None")
merge(tabhaze, tabnone, all = TRUE) %>% gather_(., "Weather", "Frequency", c("Haze", "None")) %>%
  ggplot(aes(x=Total, y=Frequency, fill=Weather)) + geom_bar(stat="identity", position="dodge")


# Wind and abundance
ggplot(raptors, aes(x=Wind.Dir, y=Wind.Dir.Angle.SEASON, color=Season)) + geom_jitter(width=0.25)
ggplot(raptors, aes(x=Wind.Dir.Angle.SEASON, y=TOTAL, colour=Wind.Spd)) + geom_jitter(width=0.25)
# not what you would expect 

############################################################################################################
################################################ FITTING MODELS ############################################## 
############################################################################################################



#### Omitting Observations from Analysis ####

missmap(raptors)

model.data <- 
  raptors.long %>% filter(!is.na(Wind.Dir.2), # Data to remove: missing wind angle, missing precipiation, zero duration, all data in month of may (4 watches)
                          !is.na(Precipitation),
                          Duration>0,
                          Month!=5) %>% 
  mutate(Bird.full = as.factor(Bird.full),
         Counter = as.factor(Counter),
         Month = as.factor(Month),
         Duration = as.numeric(Duration),
         No.Observers = as.factor(No.Observers),
         Precipitation.Grouped = factor(Precipitation.Grouped, levels = c("None", "Haze or Fog", "Drizzle or Rain or Thunderstorm", "Snow", "Wind-driven Dust/Sand/Snow")),
         No.Counters = as.factor(No.Counters),
         Counter.In.Observers = as.factor(Counter.In.Oversvers)) # Need as factor if using as a random effect

str(model.data)


#### Models With All Parameters ####

#### No Inflation Models ####

# POISSON
poisson.model <- glmmTMB(Count ~ offset(log(Duration)) + Month + Wind.Dir.Season*Wind.Spd2 +
                           Precipitation.Grouped + Cloud.Cover + Temp*Humidity +
                           BARO + Start.Noon  + (1|Bird.full) + (1|Counter), data = model.data, family = poisson)



# NEGATIVE BINOMIAL
negative.binomial.model <- glmmTMB(Count ~ offset(log(Duration)) + Month + Wind.Dir.Season*Wind.Spd2 +
                                     Precipitation.Grouped + Cloud.Cover + Temp*Humidity +
                                     BARO + Start.Noon  + (1|Bird.full) + (1|Counter), data = model.data, family = nbinom2)








#### Inflation Models: Intercept Only ####


# ZERO-INFLATED POISSON: INTERCEPT ONLY
z.i.poisson.model.intercept <- glmmTMB(Count ~ offset(log(Duration)) + Month + Wind.Dir.Season*Wind.Spd2 +
                                         Precipitation.Grouped + Cloud.Cover + Temp*Humidity +
                                         BARO + Start.Noon  + (1|Bird.full) + (1|Counter), data = model.data,
                                       ziformula=~1,
                                       family = poisson)


# ZERO-INFLATED NEG BIN: INTERCEPT ONLY
z.i.neg.binomial.model.intercept <- glmmTMB(Count ~ offset(log(Duration)) + Month  + Wind.Dir.Season*Wind.Spd2 +
                                              Precipitation.Grouped + Cloud.Cover + Temp*Humidity +
                                              BARO + Start.Noon  + (1|Bird.full) + (1|Counter), data = model.data,
                                            ziformula=~1,
                                            family = nbinom2())









#### Inflation Models: Coefficients (FE) ####

# POISSON
z.i.poisson.model.coefs <- glmmTMB(Count ~ offset(log(Duration)) + Month + Wind.Dir.Season*Wind.Spd2 +
                                     Precipitation.Grouped + Cloud.Cover + Temp*Humidity +
                                     BARO + Start.Noon  + (1|Bird.full) + (1|Counter), data = model.data,
                                   ziformula=~as.numeric(No.Counters) + Cloud.Cover,
                                   family = poisson)


# NEGATIVE BINOMIAL
z.i.neg.binomial.coefs <- glmmTMB(Count ~ offset(log(Duration)) + Month + Wind.Dir.Season*Wind.Spd2 +
                                    Precipitation.Grouped + Cloud.Cover + Temp*Humidity +
                                    BARO + Start.Noon  + (1|Bird.full) + (1|Counter), data = model.data,
                                  ziformula=~as.numeric(No.Counters) + Cloud.Cover,
                                  family = nbinom2)












#### Comparing Models: Number of Zeros ####



# Poisson - no inflation
nzeros.p<-numeric()
estimations <- predict(poisson.model,type="response")
for (i in 1:1000){
  simcounts<-rpois(n=length(estimations),lambda=estimations) # simulate from poisson distribution using fitted values
  nzeros.p<-c(nzeros.p, sum(simcounts==0))
}
hist(nzeros.p); mean(nzeros.p); sd(nzeros.p)
sum(model.data$Count==0)




# Negtive binomial - no zero inflation
theta.negative.binomial <- sigma(negative.binomial.model) ## extract final theta estimate
nb.nzeros<-numeric()
nb.means <- numeric()
nb.variances <- numeric()
estimations <- predict(negative.binomial.model,type="response")
for (i in 1:1000){
  simcounts<-rnbinom(size=theta.negative.binomial, n=length(estimations), mu=estimations) 
  nb.nzeros<-c(nb.nzeros, sum(simcounts==0))
  nb.means<-c(nb.nzeros, mean(simcounts))
  nb.variances<-c(nb.nzeros, sd(simcounts)^2)
}
mean(nb.nzeros)
sd(nb.nzeros)
hist(nb.nzeros)
abline(v=sum(model.data$Count==0),col=2)




# Poisson - intercept inflation
zip.nzeros<-numeric()
estimations <- predict(z.i.poisson.model.intercept,type="response")
prob.structural.zero <- predict(z.i.poisson.model.intercept,type="zprob")
for (i in 1:1000){
  simcounts<-VGAM::rzipois(n=length(estimations), lambda=estimations, pstr0 = prob.structural.zero) # simulate from poisson distribution using fitted values
  zip.nzeros<-c(zip.nzeros, sum(simcounts==0))
}
hist(zip.nzeros); mean(zip.nzeros); sd(zip.nzeros); abline(v=sum(model.data$Count==0),col=2)




# Negative binomial - intercept inflation
theta.z.i.neg.binomial.intercept <- sigma(z.i.neg.binomial.model.intercept) ## extract final theta estimate
zinb.nzeros<-numeric()
estimations <- predict(z.i.neg.binomial.model.intercept,type="response")
prob.structural.zero <- predict(z.i.neg.binomial.model.intercept,type="zprob")
for (i in 1:1000){
  simcounts<-VGAM::rzinegbin(size=theta.z.i.neg.binomial.intercept, n=length(estimations), munb = estimations, pstr0 = prob.structural.zero)
  zinb.nzeros<-c(zinb.nzeros, sum(simcounts==0))
}
hist(zinb.nzeros); mean(zinb.nzeros); sd(zinb.nzeros);sum(model.data$Count==0)
abline(v=sum(model.data$Count==0),col=2)



# Poisson - coefficient inflation
zi.p.c.nzeros<-numeric()
zi.p.c.means<-numeric()
zi.p.c.variances<-numeric()
estimations <- predict(z.i.poisson.model.coefs,type="response")
prob.structural.zero <- predict(z.i.poisson.model.coefs,type="zprob")
for (i in 1:1000){
  simcounts<-VGAM::rzipois(n=length(estimations),lambda=estimations, pstr0 = prob.structural.zero) # simulate from poisson distribution using fitted values
  zi.p.c.nzeros<-c(zi.p.c.nzeros, sum(simcounts==0))
  zi.p.c.means<-c(zi.p.c.nzeros, mean(simcounts))
  zi.p.c.variances<-c(zi.p.c.nzeros, sd(simcounts)^2)
}
mean(zi.p.c.nzeros); sum(model.data$Count==0); sd(zi.p.c.nzeros)
hist(zi.p.c.nzeros)
abline(v=sum(model.data$Count==0),col=2)


# Negative binomial - coefficient inflation
theta.z.i.neg.binomial.model.coefs <- sigma(z.i.neg.binomial.coefs) ## extract final theta estimate
z.i.nb.c.nzeros<-numeric()
mean.count<-numeric() # Average of those counts which are predicted zero
max.count<-numeric() # Max of those counts which are predicted zero
estimations <- predict(z.i.neg.binomial.coefs ,type="response")
prob.structural.zero <- predict(z.i.neg.binomial.coefs ,type="zprob")
no.zeros <- rep(0,nrow(model.data))
for (i in 1:1000){
  simcounts<-VGAM::rzinegbin(size=theta.z.i.neg.binomial.model.coefs, n=length(estimations), munb = estimations, pstr0 = prob.structural.zero)
  z.i.nb.c.nzeros<-c(z.i.nb.c.nzeros, sum(simcounts==0))
  mean.count <- c(mean.count, mean(model.data[which(simcounts==0),]$Count))
  max.count <- c(max.count, max(model.data[which(simcounts==0),]$Count))
  no.zeros <- no.zeros + as.numeric(simcounts==0)
}
hist(z.i.nb.c.nzeros); mean(z.i.nb.c.nzeros);sum(model.data$Count==0); sd(z.i.nb.c.nzeros)
abline(v=sum(model.data$Count==0),col=2)

# 95 confidence interval via simulation
nzeros.p[order(nzeros.p)][25]; nzeros.p[order(nzeros.p)][975]
nb.nzeros[order(nb.nzeros)][25]; nb.nzeros[order(nb.nzeros)][975]
zip.nzeros[order(zip.nzeros)][25]; zip.nzeros[order(zip.nzeros)][975]
zinb.nzeros[order(zinb.nzeros)][25]; zinb.nzeros[order(zinb.nzeros)][975]
zi.p.c.nzeros[order(zi.p.c.nzeros)][25]; zi.p.c.nzeros[order(zi.p.c.nzeros)][975]
z.i.nb.c.nzeros[order(z.i.nb.c.nzeros)][25]; z.i.nb.c.nzeros[order(z.i.nb.c.nzeros)][975]





#### Comparing Models: AIC ####
library(bbmle)
AICtab(poisson.model, negative.binomial.model,
       z.i.poisson.model.intercept, z.i.neg.binomial.model.intercept,
       z.i.poisson.model.coefs, z.i.neg.binomial.coefs)




#### Comparing Models: Residual Plots ####

# Residual Plots - all seem to show large residuals belonging to BW hawk

data.p  <- data.frame(resid = residuals(poisson.model, type="response"), fit = predict(poisson.model, type="response"), col=model.data$Bird.abbrev)
ggplot(data.p , aes(y=resid, x=fit, col=col))+geom_point()


data.nb  <- data.frame(resid = residuals(negative.binomial.model, type="response"), fit = predict(negative.binomial.model, type="response"), col=model.data$Bird.abbrev)
ggplot(data.nb , aes(y=resid, x=fit, col=col))+geom_point()
# You can see how the overdispersion is already catered for here - residuals are much better

data.zipi  <- data.frame(resid = residuals(z.i.poisson.model.intercept, type="response"), fit = predict(z.i.poisson.model.intercept, type="response"), col=model.data$Bird.abbrev)
ggplot(data.zipi , aes(y=resid, x=fit, col=col))+geom_point()


data.zinbi  <- data.frame(resid = residuals(z.i.neg.binomial.model.intercept, type="response"), fit = predict(z.i.neg.binomial.model.intercept, type="response"), col=model.data$Bird.abbrev)
ggplot(data.zinbi , aes(y=resid, x=fit, col=col))+geom_point()


data.zipc  <- data.frame(resid = residuals(z.i.poisson.model.coefs, type="response"), fit = predict(z.i.poisson.model.coefs, type="response"), col=model.data$Bird.abbrev)
ggplot(data.zipc , aes(y=resid, x=fit, col=col))+geom_point()


data.zinbc  <- data.frame(resid = residuals(z.i.neg.binomial.coefs, type="response"), fit = predict(z.i.neg.binomial.coefs, type="response"), col=model.data$Bird.abbrev)
ggplot(data.zinbc , aes(y=resid, x=fit, col=col))+geom_point()








#### APPLICATION TO INIDIVIDUAL SPECIES ####

model.data.BW <- model.data %>% filter(Bird.abbrev=="BW")
model.data.RT <- model.data %>% filter(Bird.abbrev=="RT")
model.data.SS <- model.data %>% filter(Bird.abbrev=="SS")

model.data.BW <- model.data.BW %>% mutate(Precipitation.Grouped.2 = ifelse(Precipitation2==0, "None", "Some")) # Because too few levels - will not converge
model.data.RT <- model.data.RT %>% mutate(Precipitation.Grouped.2 = ifelse(Precipitation2==0, "None", "Some")) # Because too few levels - will not converge
model.data.SS <- model.data.SS %>% mutate(Precipitation.Grouped.2 = ifelse(Precipitation2==0, "None", "Some")) # Because too few levels - will not converge



# Broad-winged hawk
neg.binomial.model.BW <- glmmTMB(Count ~ offset(log(Duration))  + Wind.Dir.Season*Wind.Spd2 +
                                   Precipitation.Grouped.2 + Cloud.Cover + Temp+Humidity +
                                   BARO + Start.Noon  + (1|Month),
                                 data = model.data.BW,
                                 family = nbinom2)


# Ret tailed hawk
neg.binomial.model.RT <- glmmTMB(Count ~ offset(log(Duration))  + Wind.Dir.Season*Wind.Spd2 +
                                   Precipitation.Grouped.2 + Cloud.Cover + Temp+Humidity +
                                   BARO + Start.Noon  + (1|Month),
                                 data = model.data.RT,
                                 family = nbinom2)





# Sharp shinned hawk
neg.binomial.model.SS <- glmmTMB(Count ~ offset(log(Duration))  + Wind.Dir.Season*Wind.Spd2 +
                                   Precipitation.Grouped.2 + Cloud.Cover + Temp+Humidity +
                                   BARO + Start.Noon  + (1|Month),
                                 data = model.data.SS,
                                 family = nbinom2)





#### Model Assessment: Number of Zeros ####

theta.negative.binomial.BW <- sigma(neg.binomial.model.BW) 
theta.negative.binomial.RT <- sigma(neg.binomial.model.RT) 
theta.negative.binomial.SS <- sigma(neg.binomial.model.SS) 
nb.nzeros.BW<-numeric()
nb.nzeros.RT<-numeric()
nb.nzeros.SS<-numeric()
nb.variances <- numeric()
estimations.BW <- predict(neg.binomial.model.BW,type="response")
estimations.RT <- predict(neg.binomial.model.RT,type="response")
estimations.SS <- predict(neg.binomial.model.SS,type="response")
for (i in 1:1000){
  simcounts.BW<-rnbinom(size=theta.negative.binomial.BW, n=length(estimations.BW), mu=estimations.BW) 
  simcounts.RT<-rnbinom(size=theta.negative.binomial.RT, n=length(estimations.RT), mu=estimations.RT) 
  simcounts.SS<-rnbinom(size=theta.negative.binomial.SS, n=length(estimations.SS), mu=estimations.SS) 
  nb.nzeros.BW<-c(nb.nzeros.BW, sum(simcounts.BW==0))
  nb.nzeros.RT<-c(nb.nzeros.RT, sum(simcounts.RT==0))
  nb.nzeros.SS<-c(nb.nzeros.SS, sum(simcounts.SS==0))
}
mean(nb.nzeros.BW); hist(nb.nzeros.BW); abline(v=sum(model.data.BW$Count==0),col=2)
#  1059.359 vs 1058
mean(nb.nzeros.RT); hist(nb.nzeros.RT); abline(v=sum(model.data.RT$Count==0),col=2)
#  842.899 vs 848
mean(nb.nzeros.SS); hist(nb.nzeros.SS); abline(v=sum(model.data.SS$Count==0),col=2)
# 999.936 vs 1001


