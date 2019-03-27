#######################################
##      Electronic Monitoring Of     ##
##  Scottish Razor Clam (Ensis spp.) ##
##   Fishery Data Processing Script  ##
#######################################


## Created 27.03.19
## Last edited 27.03.19


# clean workspace
rm(list=ls())

library(httr)

# setwd("M:\\Anchorlab\\Anchorlab Razor Clam Fishery Data")
setwd("C:/Work/Razor Clams")

available.files <- list.files(include.dirs = FALSE)

# uk.coast <- read.table("C:/Work/europe_coast.txt", header=T)
# lets leave this here for the times when the MSS firewall blocks R accessing github

uk.coast <- read.table(text = GET("https://github.com/REMEX/Spatial/blob/master/europe_coast.txt"), header=T)

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

