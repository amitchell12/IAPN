## THIS CODE TRANSFORMS .TXT DATA DOWNLOADED FROM LAB.JS & COMPILES .CSV FILES FROM OSWEB AND LAB.JS
## AG.Mitchell 10.02.21

##### PREP #####
# libraries to install
library(tidyverse)
library(jsonlite)

# paths
dataPath <- getwd()

##### QUESTIONNAIRE DATA CONVERT & COMPILE #####
# get list of all .txt files in data folder (this might be both questionnaire and bisectiond data)
txtfilenames <- dir(dataPath, recursive = TRUE, full.names = FALSE, pattern = '.txt')

# prepare data-frames for individual questionnaire data - need column names for this!
# first, convert just one Q file to .csv (tmp) to extract information about column names
for (file in txtfilenames){
  if (isTRUE(substr(basename(file), 6, 6)=="Q")){
    read_file(file) %>%
      # ... split it into lines ...
      str_split('\n') %>% first() %>%
      # ... filter empty rows ...
      discard(function(x) x == '') %>%
      # ... parse JSON into a data.frame
      map_dfr(fromJSON, flatten=T) -> tmp
    
    # get ID for ID column
    tmp$ID <- substr(basename(file), 1, 4) 
  }
}

# get column name info from .tmp for each questionnaire
consentData <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), colnames(tmp)[c(1,3,4,86)])
demoData <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), colnames(tmp)[c(1,3,15:18,86)])
MHQData <- setNames(data.frame(matrix(ncol = 15, nrow = 0)), colnames(tmp)[c(1,3,19:30,86)])
MAIA2Data <- setNames(data.frame(matrix(ncol = 40, nrow = 0)), colnames(tmp)[c(1,3,31:67,86)])

## now extract all Q data in data path and convert to 4 .csv files for each Q component
# loop through files in filenames that have 'Q' as 5th char
for (file in txtfilenames){
  if (isTRUE(substr(basename(file), 6, 6)=="Q")){
    # read in each file and convert to .csv format
    read_file(file) %>%
      # ... split it into lines ...
      str_split('\n') %>% first() %>%
      # ... filter empty rows ...
      discard(function(x) x == '') %>%
      # ... parse JSON into a data.frame
      map_dfr(fromJSON, flatten=T) -> data
    
    # first, get ID of participant
    data$ID <- substr(basename(file), 1, 4) 
    
    # extract individual questionnaires from data
    # isolate consent
    consenttemp <- data[1, c(1,3,4,86)]
    consentData <- rbind(consentData, consenttemp)
    # isoalte demographics
    demotemp <- data[2, c(1,3,15:18,86)]
    demoData <- rbind(demoData, demotemp)
    # isolate MHQdata
    MHQtemp <- data[3, c(1,3,19:30,86)]
    MHQData <- rbind(MHQData, MHQtemp)
    # isolate MAIA
    MAIA2temp <- data[4, c(1,3,31:67,86)]
    MAIA2Data <- rbind(MAIA2Data, MAIA2temp)
  }
}

# raname demographics columns so they make more sense
names(demoData)[names(demoData) == "age:"] <- "age"
names(demoData)[names(demoData) == "date-of-birth:"] <- "DOB"
names(demoData)[names(demoData) == "i-am-predominantly"] <- "handedness"

# save all of these as .csv
write.csv(consentData, "IAPN_consent_data.csv", row.names = FALSE)
write.csv(demoData, "IAPN_demographic_data.csv", row.names = FALSE)
write.csv(MHQData, "IAPN_MHQ_data.csv", row.names = FALSE)
write.csv(MAIA2Data, "IAPN_MAIA2_data.csv", row.names = FALSE)

# compile all key questionnaire info into one data-frame (if needed)
QData <- merge(demoData, MHQData, by = 'ID')
QData <- merge(QData, MAIA2Data, by = 'ID')
write.csv(QData, "all_questionnaire_data.csv", row.names = FALSE)

##### BISECTION DATA COMPILE & TRANSFORM #####
# get list of all .csv files in data folder (this might be both questionnaire and bisectiond data)
csvfilenames <- dir(dataPath, recursive = TRUE, full.names = FALSE, pattern = '.csv')
# create empty .csv with key data
bisectData <- read.csv(text = 'bisect_,bisect_y,card_mm,card_pix,coordinates,count_line_bisection,left_mm,left_pix,line_y_pos,linelength_pix,mouse_loc,pix_per_mm,response_time,right_mm,right_pix,screen_centre_x,screen_centre_y,screen_left_x,screen_left_y,screen_right_x,screen_right_y,workerId,ID')

# extract key data from all bisection .csv files
for (file in csvfilenames){
  if (isTRUE(substr(basename(file), 6, 6)=="B")){
    tmp <- read.csv(file)[, c(9,10,13,14,23,45,90:93,95,100,114,125,126,129:134,191)]
    tmp$ID <- substr(basename(file), 1, 4) 
    # merge data together
    bisectData <- rbind(bisectData, tmp)
  }
}

# get demographic data for each ppt and merge with bisection data
bisectData <- merge(bisectData, demoData, by = 'ID')
# reorder data-frame to make more logical sense
bisectData <- bisectData[, c(1,28:31,25,2:24)]

## final step: calculate midpoint for each line
# first, identify line-length in mm
bisectData$linelength_mm <- bisectData$linelength_pix/bisectData$pix_permm

# use screen test coordinate measurements to identify how far in pixels measurements are away from centre
bisectData$left_screen_test <- (bisectData$screen_left_x - bisectData$screen_centre_x)
bisectData$right_screen_test <- (bisectData$screen_right_x - bisectData$screen_centre_x)
# make sure that the right is +ve and left is -ve, otherwise this will need changing

# calculate actual midpoint of line, in mm then pixels for each participant
bisectData$line_midpoint_mm <- NA
# for lines that begin & end at matched L&R coords, centre is the same
bisectData$line_midpoint_mm[bisectData$left_mm == -40 & bisectData$right_mm == 40] <- 0
bisectData$line_midpoint_mm[bisectData$left_mm == -80 & bisectData$right_mm == 80] <- 0
# for lines shifted left, mid point is -20mm
bisectData$line_midpoint_mm[bisectData$left_mm == -80 & bisectData$right_mm == 40] <- -20
# for lines shifted right, midpoint is 20mm
bisectData$line_midpoint_mm[bisectData$left_mm == -40 & bisectData$right_mm == 80] <- 20

# now calculate what they are in pixels - on the participants actual screen!
## note if right is NEGATIVE the above code will have to be reversed - can check coordinates for this
bisectData$line_midpoint_pix <- (bisectData$line_midpoint_mm*bisectData$pix_permm)+bisectData$screen_centre_x

## CALCULATE BISECTION ERROR
bisectData$bisect_err_pix <- bisectData$bisect_x - bisectData$line_midpoint_pix
bisectData$bisect_err_mm <- bisectData$bisect_err_pix/bisectData$pix_permm

##### BISECT ERROR SANITY CHECK - y coordinates #####
bisectData$y_err_mm <- (bisectData$bisect_y - bisectData$screen_centre_y)/bisectData$pix_permm
# remove anything over 20mm away from line!

# save data frame
write.csv(bisectData, 'IAPN_bisection_data.csv', row.names = FALSE)
