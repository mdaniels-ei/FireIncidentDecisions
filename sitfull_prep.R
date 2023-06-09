# check packages to load
## car faraway ggplot2 MASS dplyr lattice
library(car)
library(faraway)
library(ggplot2)
library(MASS)
library(dplyr)
library(lattice)
library(corrplot)
library(openssl)
library(XML)
library(rlist)
library(rrefine)

# setting work directory
saveDir <- getwd() # get the current working directory
saveDir # show me the saved directory

swd <- "C:/Users/Molly/Desktop/sit_working/sit_0813_inb" 
# path to my project
setwd(swd) # set this path as my work directory 
# directory is set.

# load the data
sitor <- read.csv("C:/Users/Molly/Desktop/sit_working/sit_0813_inb/cleandata/0813_209_working_or.csv", header=TRUE, na.strings="") 
#swor is the name of this dataset, loaded from the working file 0813_209_working_or,
#this file contains data that has been cleaned in OpenRefine.

# view the data
View(sitor)

###

# load the 'sw' data
sw <- read.csv("C:/Users/Molly/Desktop/sit_working/sit_0813_inb/cleandata/sit_working_or.csv", header=TRUE, na.strings="") 
#sw is the name of this dataset, loaded from the working file sit_working_or,
#this file contains data that has been cleaned in OpenRefine.

# view the data
View(sw)

###

# remove incidents outside of the study boundary from the data frame
swb <- subset(sw, sw$Bound_Ind==1)

View(swb)

###

# Join "sitor" with the "swb" dataframe, because that has incidents within the study boundaries

sitfull <- semi_join(sitor, swb, by="INCIDENT_ID_KS", all=TRUE)
View(sitfull)

###

## first prep the columns: establishing either factor or numeric columns

#numeric variables
sitfull$START_DOY <- as.numeric(sitfull$START_DOY)
sitfull$REPORT_DOY <- as.numeric(sitfull$REPORT_DOY)
sitfull$AREA <- as.numeric(sitfull$AREA)
sitfull$INJURIES_TO_DATE <- as.numeric(sitfull$INJURIES_TO_DATE)
sitfull$INJURIES <- as.numeric(sitfull$INJURIES)
sitfull$FAT <- as.numeric(sitfull$FATALITIES)
sitfull$P_CONTAIN <- as.numeric(sitfull$P_CONTAIN)
sitfull$HOUR <- as.numeric(sitfull$HOUR)
sitfull$LINE_TO_BUILD <- as.numeric(sitfull$LINE_TO_BUILD)

#numeric variables (subset: current weather variables) which have been cleaned somewhat

sitfull$C_WIND_SPEED <- as.numeric(sitfull$C_WIND_SPEED)
# This has been cleaned in OpenRefine, but there is still data entry error

sitfull$C_TEMP <- as.numeric(sitfull$C_TEMP) 
# This has been cleaned in OpenRefine, but there is still data entry error

sitfull$C_RH <- as.numeric(sitfull$C_RH) 
# This has been cleaned in OpenRefine, but there is still data entry error

#numeric variables (subset: future weather variables) which have been cleaned somewhat

sitfull$F_WIND_SPEED <- as.numeric(sitfull$F_WIND_SPEED)
# This has been cleaned in OpenRefine, but there is still data entry error

sitfull$F_TEMP <- as.numeric(sitfull$F_TEMP) 
# This has been cleaned in OpenRefine, but there is still data entry error

sitfull$F_RH <- as.numeric(sitfull$F_RH) 
# This has been cleaned in OpenRefine, but there is still data entry error


#factor variables
sitfull$PRIMARY_FUEL_MODEL <- as.factor(sitfull$PRIMARY_FUEL_MODEL)
sitfull$GROWTH_POTENTIAL <- as.factor(sitfull$GROWTH_POTENTIAL)
sitfull$TERRAIN <- as.factor(sitfull$TERRAIN)
sitfull$SUPPRESSION_METHOD <- as.factor(sitfull$SUPPRESSION_METHOD)
sitfull$IMT_TYPE <- as.factor(sitfull$IMT_TYPE)
sitfull$IMT_TYPE_DESCRIPTION <- as.factor(sitfull$IMT_TYPE_DESCRIPTION)
sitfull$UNIT_TYPE <- as.factor(sitfull$UnitType)
sitfull$TYPE_INC <- as.factor(sitfull$TYPE_INC)
sitfull$CAUSE <- as.factor(sitfull$CAUSE)
sitfull$CAUSE_DESCR <- as.factor(sitfull$CAUSE_DESCR)
sitfull$STATUS <- as.factor(sitfull$STATUS)
sitfull$COMPLEX_FLAG <- as.factor(sitfull$COMPLEX_FLAG)
sitfull$ST_INC <- as.factor(sitfull$ST_INCNUM)
sitfull$ST_UNIT <- as.factor(sitfull$UN_USTATE)
sitfull$OWNERSHIP_STATE <- as.factor(sitfull$OWNERSHIP_STATE)
sitfull$LINE_MEASUREMENT <- as.factor(sitfull$LINE_MEASUREMENT)


###

# creating new variable for Fire Size Class:
sitfull$FSCLASS <- ifelse(sitfull$AREA < 0.26, "A",
                          ifelse(sitfull$AREA < 10, "B", 
                                 ifelse(sitfull$AREA < 100, "C", 
                                        ifelse(sitfull$AREA < 300, "D", 
                                               ifelse(sitfull$AREA < 1000, "E", 
                                                      ifelse(sitfull$AREA < 5000, "F", "G"))))))


# Setting FSCLASS as a categorical variable:
sitfull$FSCLASS <- as.factor(sitfull$FSCLASS)

###

sitfull <- as.data.frame(sitfull)

###

#Need to set incident fire class based on maximum fire class:

sitfull %>%
  group_by(INCIDENT_ID_KS) %>%
  mutate(FSCLASS_INC = last(na.omit(sitfull$FSCLASS), order_by = sitfull$AREA)) -> sitfull

#this isn't working, so I'm going to export this dataframe as an excel sheet, save it, and reupload it.
write.table(sitor, "C:/Users/Molly/Desktop/sit_working/sit_0813_inb/cleandata/sitor.txt", sep="\t")

# reload the 'sitor' data
sitor2 <- read.csv("C:/Users/Molly/Desktop/sit_working/sit_0813_inb/cleandata/sitor.csv", header=TRUE, na.strings="") 
#sitor2 is the name of this dataset, loaded from the working file sitor (csv, not txt),
#this file contains data that has been cleaned in OpenRefine.

# view the data
View(sitor2)



###

#reorder columns for easier viewing:
sitfullsubset <- select(sitfull, INCIDENT_ID_KS, AREA, FSCLASS, FSCLASS_INC)
View(sitfullsubset)

#How many NA's are in the Fire Size Class by Incident variable? (AKA, how many incidents do not report any area throughout the incident?)
sum(is.na(swbfull$FSCLASS_INC))


