# Load dplyr for data frame manipulations.
library(dplyr)

# Source and local zip files.
externalZipURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
localZipFile <- "___Fhousehold_power_consumption___.zip"

# Folder in current working directory where unzipped data will be placed.
rootFolder <- "UCI Power Dataset"

# Unzipped data file name in root folder.  Appears to be a text file.
dataFileName <- "household_power_consumption.txt"

# Date range we are using.
dateRange <- c("1/2/2007", "2/2/2007")

makePOSIX <- function(dt, tm) {
# Utility function for converting raw date and time strings into POSIXct.
# 
# Inputs: 
#   dt: a character string of the form "dd/mm/yy".    
#   tm: a character string of the form "hh:mm:ss".
#   
# Outputs:
#   A POSIXct date/time value
#   
    as.POSIXct(strptime(paste(dt, tm), format = "%d/%m/%Y %H:%M:%S"))    
}

# STEP 1: Download raw data if we need to --------------------------------------
# If a folder called "UCI Power Dataset" does not already exist in the current 
# working directory, download the data and unzip it into this directory.  If 
# the folder already exists it will be assumed that the data have already 
# been downloaded.

download <- FALSE
if (!file.exists(rootFolder)) {
    print("Dowloading source data")
    download.file(externalZipURL, localZipFile)
    unzip(localZipFile, exdir = rootFolder) 
    file.remove(localZipFile)
    download <- TRUE
}

# STEP 2: If we need to, read the data, subset, and create time stamp ----------
if (!exists("powerData") | download == TRUE) {
    # Full path from current working directory to the raw data file.
    dataPath <- paste0(rootFolder, "/", dataFileName) 
    
    # Read the raw data, subset to just the two days of interest.
    powerData <- read.table(dataPath, sep = ";", header = TRUE, na.strings = "?",
                            comment.char = "", nrows = 2075260,
                            colClasses = c(rep("character", 2), 
                                           rep("numeric", 7))) %>%
        tbl_df %>%
        filter(Date %in% dateRange) %>%
        mutate(timeStamp = makePOSIX(Date, Time)) %>%  # Using function above.
        select(timeStamp, everything(), -Date, -Time)      
}

# STEP 3: Build Plot #4  -------------------------------------------------------
png(width = 480, height = 480, filename = "plot4.png")

par(mfcol = c(2, 2), mar = c(4, 4, 5, 2), oma = c(0, 0, 0, 0), cex = 0.80)

# Row 1, Column 1
with(powerData, plot(timeStamp, Global_active_power,
                      type = "l",
                      main = "", 
                      xlab = "", 
                      ylab = "Global Active Power",
                      col = "black"))

# Row 2, Column 1
with(powerData, {plot(timeStamp, Sub_metering_1,
                     type = "l",
                     main = "", 
                     xlab = "", 
                     ylab = "Energy sub metering",
                     col = "black")

                lines(timeStamp, Sub_metering_2, col = "red")
                lines(timeStamp, Sub_metering_3, col = "blue")})

legend("topright", pch = "", lty = 1, bty = "n", 
        col = c("black", "red", "blue"), 
        legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

# Row 1, Column 2
with(powerData, plot(timeStamp, Voltage,
                      type = "l",
                      main = "", 
                      xlab = "datetime", 
                      ylab = "Voltage",
                      col = "black"))

# Row 2, Column 2
with(powerData, plot(timeStamp, Global_reactive_power,
                      type = "l",
                      main = "", 
                      xlab = "datetime", 
                      ylab = "Global_reactive_power",
                      col = "black"))

dev.off()





                      