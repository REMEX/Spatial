#######################################
##      Electronic Monitoring Of     ##
##  Scottish Razor Clam (Ensis spp.) ##
##   Fishery Data Processing Script  ##
#######################################


## Created 27.03.19
## Last edited 27.03.19

# library(devtools)
# install_github("skgrange/threadr")
## Helen - this isn't available on CRAN, so I've tried to build it from github, but running into grief with dependencies
## can we do without it?

# clean workspace
rm(list=ls())

library(httr)
library(ggplot2)
library(dplyr)
library(threadr)
library(stringr)



# setwd("M:\\Anchorlab\\Anchorlab Razor Clam Fishery Data")
setwd("C:/Work/Razor Clams")

available.files <- list.files(include.dirs = FALSE)

# uk.coast <- read.table("C:/Work/europe_coast.txt", header=T)
# lets leave this here for the times when the MSS firewall blocks R accessing github

uk.coast <- read.table(file = "https://raw.githubusercontent.com/REMEX/Spatial/master/europe_coast.csv", sep = ",", header=T)

raw <- read.table(file=available.files[1], sep = ";", header = TRUE) 
      

raw <- raw[,1:13]
raw <- raw[raw$GPSTime.local. != "",]

raw$LogTime.local. <- as.POSIXct(paste(raw$LogTime.local.), format="%d-%m-%Y %H:%M:%S" )
raw$GPSTime.local. <- as.POSIXct(paste(raw$GPSTime.local.), format="%d-%m-%Y %H:%M:%S" )

write.csv(raw, "RAW.csv", row.names=FALSE)


raw.2 <- read.table(file="RAW.csv", sep = ",", header = TRUE, 
                  colClasses = c("numeric", "factor", "POSIXct", "POSIXct", "numeric",
                  "numeric", "numeric","numeric","numeric","numeric","factor","factor","factor"))

plot(x=raw.2$LogTime.local.[raw.2$HarbourNumber=="AD1"], y=raw.2$Current1[raw.2$HarbourNumber=="AD1"], pch=".")

v.das <- unique(format(raw.2$LogTime.local.[paste(raw.2$HarbourNumber) == "AD1" & raw.2$Speed>0.5], "%d-%m-%Y"))

plot(x=raw.2$Longitude[format(raw.2$LogTime.local., "%d-%m-%Y")==v.das[1] & raw.2$HarbourNumber == "AD1"], 
     y=raw.2$Latitude[format(raw.2$LogTime.local., "%d-%m-%Y")==v.das[1] & raw.2$HarbourNumber == "AD1"], pch=".", #type="l",
     xlab="Longitude", ylab="Latitude", las=1, asp=1.5)#, xlim=c(-4.75, -4.74), ylim=c(55.615, 55.625))

points(x=raw.2$Longitude[format(raw.2$LogTime.local., "%d-%m-%Y")==v.das[1] & raw.2$HarbourNumber == "AD1" & raw.2$Current1 >= 3], 
       y=raw.2$Latitude[format(raw.2$LogTime.local., "%d-%m-%Y")==v.das[1] & raw.2$HarbourNumber == "AD1" & raw.2$Current1 >=3], pch=16,
        col = 2)
 
plot(x=raw$LogTime.local.[format(raw.2$LogTime.local., "%d-%m-%Y")==v.das[1] & raw.2$HarbourNumber == "AD1"], 
      y=raw$Speed[format(raw.2$LogTime.local., "%d-%m-%Y")==v.das[1] & raw.2$HarbourNumber == "AD1"], type = "l")


plot(x=raw$LogTime.local.[format(raw.2$LogTime.local., "%d-%m-%Y")==v.das[1] & raw.2$HarbourNumber == "AD1"], 
     y=raw$Current1[format(raw.2$LogTime.local., "%d-%m-%Y")==v.das[1] & raw.2$HarbourNumber == "AD1"], type = "l")

plot(x=raw$Speed[format(raw.2$LogTime.local., "%d-%m-%Y")==v.das[1]], pch=16,
     y=raw$Current1[format(raw.2$LogTime.local., "%d-%m-%Y")==v.das[1] ])


ass <- raw[raw$HarbourNumber == "SY46",]
ass$newFishing  <-  as.numeric(ass$Current1 >= 4 & ass$Speed <= 3)

days.fished <- unique(format(ass$GPSTime.local.[ass$newFishing == 1], format = "%d-%m-%y"))

razor.data <- raw


elec.threshold <- as.numeric(4)
#minimum haul length to signify fishing activity
duration.threshold <- as.numeric(10)
#minimum speed achieved between elec events to signify new haul
speed.threshold <- as.numeric(3000)

#make df to populate
Fishing_events <- data.frame(vessel_pln=factor(),
                             start_time=factor(),
                             start_lat=factor(), 
                             start_lon=factor(),
                             end_time=factor(),
                             end_lat=factor(),
                             end_lon=factor(),
                             IsEvent=numeric(),
                             elec_num= numeric(),
                             haul_num=numeric())

for(i in unique(razor.data$HarbourNumber)){
  
  data <- razor.data[razor.data$HarbourNumber==i,]
  
  # find the start and end of fishing activities
  data$next_activity <- lead(data$Current1)
  data$prev_activity <- lag(data$Current1)
  
  start <- data$`GPSTime.local.`[data$Current1 >= elec.threshold & data$prev_activity < elec.threshold]
  end   <- data$`GPSTime.local.`[data$Current1 >= elec.threshold & data$next_activity < elec.threshold]
  
  # to obtain a df with start and end times/coordinates
  df_start <- filter(data, Current1 >= elec.threshold & lag(Current1) < elec.threshold) %>%
    select(start_time = `GPSTime.local.`,
           start_lat = Latitude,
           start_lon = Longitude)
  if(nrow(df_start)==0) next 
  df_end <- filter(data, Current1 >= elec.threshold & lead(Current1) < elec.threshold) %>%
    select(end_time = `GPSTime.local.`,
           end_lat = Latitude,
           end_lon = Longitude)
  
  result <- cbind(df_start, df_end)
  result$vessel_pln <- i
  
  result$elec_duration <- difftime(result$end_time,result$start_time,tz="GMT",units="mins")
  
  result <- result[result$elec_duration > duration.threshold,]
  if(nrow(result)==0) next 
  
  # create a column of the start of the next elec_activity
  shift <- function(x, n){
    c(x[-(seq(n))], rep(NA, n))
  }
  result$next_start_time <- shift(result$start_time, 1)
  
  # merge df1 and df2 to find speed points between end_time and next_start_time
  data$vessel_pln <- data$HarbourNumber
  data$Speed <- as.numeric(data$Speed)
  temp <- result %>% 
    left_join(data, by = 'vessel_pln') %>% 
    mutate(BETWEEN = (`GPSTime.local.` >= end_time & `GPSTime.local.` < next_start_time)) %>% 
    filter(BETWEEN == TRUE) %>%
    filter(Speed > speed.threshold)
  
  result$IsEvent <- result$start_time %in% temp$start_time
  result$elec_num <- seq.int(nrow(result))
  result[is.na(result$next_start_time),"IsEvent"] <- "TRUE"
  x <- as.logical(result$IsEvent)
  result$haul_num <- cumsum(x) + !x
  Fishing_events <- rbind(Fishing_events,result)
  
} 

#create new id columns
Fishing_events$haul_id <- paste(Fishing_events$vessel_pln,format(Fishing_events$start_time,"%m_%Y"),Fishing_events$haul_num,sep="_")
Fishing_events$elec_id <- paste(Fishing_events$vessel_pln,format(Fishing_events$start_time,"%m_%Y"),Fishing_events$haul_num,Fishing_events$elec_num,sep="_")
#reorder columns for data export 
export <- Fishing_events[c("haul_id","elec_id","vessel_pln","start_time","start_lat","start_lon","end_time","end_lat","end_lon","elec_duration")]

write.csv(export,file = paste("razorFishery_",as.Date(min(export$start_time)), "_to_", as.Date(max(export$start_time)),"_haul_info.csv",sep=""),row.names=FALSE)

#### 3. GET GPS POSITION DATA FOR ALL HAULS  ----
razor.data$vessel_pln <- razor.data$HarbourNumber

positions <- export %>% 
  left_join(razor.data, by = 'vessel_pln') %>% 
  mutate(BETWEEN = (`GPSTime.local.` >= start_time & `GPSTime.local.` < end_time)) %>% 
  filter(BETWEEN == TRUE) 

activity.table <- data.frame(ID = c(1:dim(Fishing_events)[1]), haul_id = Fishing_events$elec_id, vessel_pln = Fishing_events$vessel_pln,
                             start_time = Fishing_events$start_time, start_lat = Fishing_events$start_lat, start_lon = Fishing_events$start_lon,
                              end_time = Fishing_events$end_time, end_lat = Fishing_events$end_lat, end_lon = Fishing_events$end_lon,
                             duration = as.numeric(Fishing_events$elec_duration))
                             


write.csv(activity.table, "C:/Work/Razor Clams/Activity.csv", row.names=F)

library(DBI)

connect_to_access_dbi <- function(db_file_path)  {
  require(DBI)
  # make sure that the file exists before attempting to connect
  if (!file.exists(db_file_path)) {
    stop("DB file does not exist at ", db_file_path)
  }
  # Assemble connection strings
  dbq_string <- paste0("DBQ=", db_file_path)
  driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  db_connect_string <n- paste0(driver_string, dbq_string)
  
  myconn <- dbConnect(odbc::odbc(),
                      .connection_string = db_connect_string)
  return(myconn)
}


rzrdb <- connect_to_access_dbi("C:/Work/Razor Clams/Razor_Clam_Database.accdb")
  

dbAppendTable(con = rzrdb, name = "ACTIVITY", value = activity.table)


dbDisconnect((rzrdb))

  ## The interface can work at a higher level importing tables 
## as data.frames and exporting data.frames as DBMS tables.

dbListTables(con)
dbListFields(con, "quakes")
if(dbExistsTable(con, "new_results"))
  dbRemoveTable(con, "new_results")
dbWriteTable(con, "new_results", new.output)

## The interface allows lower-level interface to the DBMS
res <- dbSendQuery(con, paste(
  "SELECT g.id, g.mirror, g.diam, e.voltage",
  "FROM geom_table as g, elec_measures as e",
  "WHERE g.id = e.id and g.mirrortype = 'inside'",
  "ORDER BY g.diam"))
out <- NULL
while(!dbHasCompleted(res)){
  chunk <- fetch(res, n = 10000)
  out <- c(out, doit(chunk))
}

## Free up resources
dbClearResult(res)
dbDisconnect(con)
dbUnloadDriver(drv)  