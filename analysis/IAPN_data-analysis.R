## IAPN DATA ANALYSIS
library(lme4) #<- YOU WILL NEED TO INSTALL THIS PACKAGE
library(reshape2)
library(ggpubr)

# import relevant files
MAIA2Data <- read.csv('IAPN_MAIA2_data.csv')
consentData <- read.csv('IAPN_consent_data.csv')
demoData <- read.csv('IAPN_demographic_data.csv')
bisectData <- read.csv('IAPN_bisection_data.csv')

##### EWB CALCULATE #####
# calculate P
bisectData$P <- (bisectData$bisect_x - bisectData$screen_centre_x)/bisectData$pix_permm

# calculate and extract end-point weightings for each participant using linear regression
# use large trial by trial data frame to do this
EWmodel <- lmList(P ~ left_mm + right_mm | ID, data = bisectData)
dPL <- coefficients(EWmodel)[2]
dPR <- coefficients(EWmodel)[3]
k <- coefficients(EWmodel)[1]
# converting to data-frame
EPW = data.frame(ID = row.names(dPL), dPL, dPR, k)
# rename columns
names(EPW)[2] <- 'dPL'
names(EPW)[3] <- 'dPR'
names(EPW)[4] <- 'k'

## do some plotting for sanity's sake
ggplot(EPW) +
  geom_point(aes(dPR, dPL)) +
  xlim(0, 1) + ylim(0, 1)

EPW <- EPW[EPW$dPR < 1 ,]

## summary stats
# summary P for each participant
EWdata <- aggregate(P ~ ID, mean, data = bisectData)
# add dPL and dPR
EWdata <- merge(EWdata, EPW, by = 'ID')
EWdata$EWB <- EWdata$dPR - EWdata$dPL
EWdata$EWS <- EWdata$dPR + EWdata$dPL

##### MAIA2 MEAN #####
MAIA2Data <- MAIA2Data[, c(3:40)]
# melt so response all in one column
INall <- melt(MAIA2Data)
INsum <- aggregate(value ~ ID, sum, data = INall)
names(INsum)[2] <- 'MAIAscore'
# merge with bisection data
EWdata <- merge(EWdata, INsum, by = 'ID')

ggplot(EWdata) +
  geom_point(aes(MAIAscore, EWB)) +
  ylim(-0.5,0.5)

ggplot(EWdata) +
  geom_point(aes(MAIAscore, EWS))


test <- cor(EWdata$EWB, EWdata$MAIAscore)


