# Plot 1 -----------------------------------------------------------------------



# STEP 1: Get packages and set pointers for various files and folders we need -- 

# Load dplyr for data frame manipulations.  Won't reload if already loaded.
library(dplyr)
library(lubridate)

# External and local zip files.
externalZipURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
localZipFile <- "___Fhousehold_power_consumption___.zip"

# Folder in current working directory where unzipped data will be placed.
rootFolder <- "UCI Power Dataset"

# Unzipped data file name in root folder.  Appears to be a text file.
dataFileName <- "household_power_consumption.txt"

# Date range we care about for this assignment.
dateRange <- c("1/2/2007", "2/2/2007")



# STEP 2: Download raw data if we need to --------------------------------------

# If a folder called "UCI Power Dataset" does not already exist in the current 
# working directory, download the data and unzip it into this folder.  If the 
# folder already exists we'll skip this step since it so time consuming and
# assume that the data have already been properly downloaded.

download <- FALSE
if (!file.exists(rootFolder)) {
    message("Dowloading source data")
    download.file(externalZipURL, localZipFile)
    unzip(localZipFile, exdir = rootFolder) 
    file.remove(localZipFile)
    download <- TRUE
}



# STEP 3: Read unzipped data if we need to -------------------------------------

# If the powerData df already exists, we'll skip this step since it is so
# time consuming and since we're using the same code to read the same data for
# multiple plots.  If the powerData df does not exist, or we just downloaded the 
# raw data file, then go ahead and rebuild this df.

if (!exists("powerData") | download == TRUE) {
    message("Building powerData data frame")
    
    # Full path from current working directory to the raw data file.
    dataPath <- paste0(rootFolder, "/", dataFileName) 
    
    # Read the raw data, subset to just the two days of interest.
    powerData <- read.table(dataPath, sep = ";", header = TRUE, na.strings = "?",
                            comment.char = "", nrows = 2075260,
                            colClasses = c(rep("character", 2), 
                                           rep("numeric", 7))) %>%
        tbl_df %>%
        filter(Date %in% dateRange) %>%
        mutate(timeStamp = dmy_hms(paste(Date, Time))) %>% 
        select(timeStamp, everything(), -Date, -Time)      
}



# STEP 4: Build Plot #1  -------------------------------------------------------

# Set dimensions of plot; specify file for output in current working directory.
png(width = 480, height = 480, filename = "plot1.png")

par(mar = c(4, 4, 4, 2), oma = c(0, 0, 0, 0), cex = 1.0)

with(powerData, hist(Global_active_power, 
                     main = "Global Active Power", 
                     xlab = "Global Active Power (kilowatts)", 
                     ylab = "Frequency",
                     col = "red"))

dev.off()








                      