
##############################################################################################################################
#This script builds upon the "sitfull_prep2" script, adding correlations. Correlations include ....
#
##############################################################################################################################

# check packages to load
## car faraway ggplot2 MASS dplyr lattice
#library("car", lib.loc="~/R/win-library/3.5")
#library("faraway", lib.loc="~/R/win-library/3.5")
library("ggplot2", lib.loc="~/R/win-library/3.5")
#library("ggpubr", lib.loc="~/R/win-library/3.5")
#library("gplots", lib.loc="~/R/win-library/3.5")
#library("MASS", lib.loc="~/R/win-library/3.5")
library("dplyr", lib.loc="~/R/win-library/3.5")
#library("lattice", lib.loc="~/R/win-library/3.5")
#library("multcomp", lib.loc="~/R/win-library/3.5")
library("corrplot", lib.loc="~/R/win-library/3.5")
#library("openssl", lib.loc="~/R/win-library/3.5")
#library("XML", lib.loc="~/R/win-library/3.5")
#library("rlist", lib.loc="~/R/win-library/3.5")
#library("rrefine", lib.loc="~/R/win-library/3.5")

##### directory and data #####
# setting work directory
saveDir <- getwd() # get the current working directory
saveDir # show me the saved directory

swd <- "C:/Users/Molly/Desktop/sit_working/sit_0813_inb" 
# path to my project
setwd(swd) # set this path as my work directory 
# directory is set.

# load the data
sitfull <- read.csv("C:/Users/Molly/Desktop/sit_working/sit_0813_inb/workingdata/sitfull.csv", 
                    header=TRUE, na.strings="NA") 
# sitfull is the name of this dataset, loaded from the working file sitfull (csv, not txt),
# this file contains data that has been cleaned in OpenRefine, and because it was opened in 
#   OpenRefine, the NA's are not blank cells, but contain the letters "NA".

View(sitfull)

##### factor and numeric variables #####

# numeric variables
sitfull$START_DOY <- as.numeric(sitfull$START_DOY)
sitfull$REPORT_DOY <- as.numeric(sitfull$REPORT_DOY)
sitfull$AREA <- as.numeric(sitfull$AREA)
sitfull$INJURIES_TO_DATE <- as.numeric(sitfull$INJURIES_TO_DATE)
sitfull$INJURIES <- as.numeric(sitfull$INJURIES)
sitfull$FATALITIES <- as.numeric(sitfull$FATALITIES)
sitfull$P_CONTAIN <- as.numeric(sitfull$P_CONTAIN)
sitfull$HOUR <- as.numeric(sitfull$HOUR)
sitfull$LINE_TO_BUILD <- as.numeric(sitfull$LINE_TO_BUILD)

# numeric variables (subset: current weather variables) which have been cleaned somewhat
#Wind speed:
sitfull$C_WIND_SPEED <- as.numeric(sitfull$C_WIND_SPEED)
# This has been cleaned in OpenRefine, but there is still data entry error
#Temp:
sitfull$C_TEMP <- as.numeric(sitfull$C_TEMP) 
# This has been cleaned in OpenRefine, but there is still data entry error
sitfull$C_TEMPL <- as.numeric(sitfull$C_TEMPL)
sitfull$C_TEMPH <- as.numeric(sitfull$C_TEMPH)
#RH:
sitfull$C_RH <- as.numeric(sitfull$C_RH) 
# This has been cleaned in OpenRefine, but there is still data entry error
sitfull$C_RHL <- as.numeric(sitfull$C_RHL) 
sitfull$C_RHH <- as.numeric(sitfull$C_RHH) 

#numeric variables (subset: future weather variables) which have been cleaned somewhat
#Wind speed:
sitfull$F_WIND_SPEED <- as.numeric(sitfull$F_WIND_SPEED)
# This has been cleaned in OpenRefine, but there is still data entry error
#Temp:
sitfull$F_TEMP <- as.numeric(sitfull$F_TEMP) 
# This has been cleaned in OpenRefine, but there is still data entry error
sitfull$F_TEMPL <- as.numeric(sitfull$F_TEMPL)
sitfull$F_TEMPH <- as.numeric(sitfull$F_TEMPH)
#RH:
sitfull$F_RH <- as.numeric(sitfull$F_RH) 
# This has been cleaned in OpenRefine, but there is still data entry error
sitfull$F_RHL <- as.numeric(sitfull$F_RHL) 
sitfull$F_RHH <- as.numeric(sitfull$F_RHH) 


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


##### maximum area per incident #####

# creating new variable for maximum area for an incident

sitfull %>%
  group_by(INCIDENT_ID_KS) %>%
  mutate(MAX_AREA_INC = ifelse( !all(is.na(AREA)), 
                                max(AREA, na.rm=TRUE), 
                                NA)) -> sitfull
sitfull$MAX_AREA_INC <- as.numeric(sitfull$MAX_AREA_INC)

##### fire size class #####

# creating new variable for Fire Size Class:
sitfull$FSCLASS <- ifelse(sitfull$AREA < 0.26, "A",
                          ifelse(sitfull$AREA < 10, "B", 
                                 ifelse(sitfull$AREA < 100, "C", 
                                        ifelse(sitfull$AREA < 300, "D", 
                                               ifelse(sitfull$AREA < 1000, "E", 
                                                      ifelse(sitfull$AREA < 5000, "F", "G"))))))
# Setting FSCLASS as a categorical variable:
sitfull$FSCLASS <- as.factor(sitfull$FSCLASS)

##### fire size class per incident #####

# Creating FSCLASS_INC, assigning the largest fire size class achieved 
#   for an incident to all rows within that incident.

sitfull$FSCLASS_INC <- ifelse(sitfull$MAX_AREA_INC < 0.26, "A",
                              ifelse(sitfull$MAX_AREA_INC < 10, "B", 
                                     ifelse(sitfull$MAX_AREA_INC < 100, "C", 
                                            ifelse(sitfull$MAX_AREA_INC < 300, "D", 
                                                   ifelse(sitfull$MAX_AREA_INC < 1000, "E", 
                                                          ifelse(sitfull$MAX_AREA_INC < 5000, "F", "G"))))))
sitfull$FSCLASS_INC <- as.factor(sitfull$FSCLASS_INC)

##### AREA transformation #####

# creating a new variable for the log of AREA

hist(sitfull$AREA)
hist(log1p(sitfull$AREA))
sitfull %>%
  mutate(AREA_Log = ifelse( !all(is.na(AREA)), 
                            log1p(AREA), 
                            NA)) -> sitfull
sitfull$AREA_Log <- as.numeric(sitfull$AREA_Log)

###

##### suppression_bi #####
# creating a new variable for the binary statement, Full Suppression v. Less-than-Full Suppression

sitfull %>%
  mutate(SUPPRESSION_BI = ifelse(is.na(SUPPRESSION_METHOD), NA,
                                 ifelse(SUPPRESSION_METHOD == "FS", "FS", "NFS"))) -> sitfull
sitfull <- filter(sitfull, !(is.na(SUPPRESSION_METHOD)))

sitfull$SUPPRESSION_BI <- factor(sitfull$SUPPRESSION_BI, levels = c("NFS", "FS", NA), ordered = TRUE, exclude = NULL)

summary(sitfull$SUPPRESSION_BI)
summary(sitfull$SUPPRESSION_METHOD)

###


##### changes in suppression #####
# creating subsets for each type of change in suppression
#FS -> NFS = 1,  NFS -> FS/NFS/mixed = 2
sitfull %>%
  mutate(SUPP_DECREASE = ifelse(INCIDENT_ID_KS == "2010_ID-SCF-10293_BANNER", 1, 
                                ifelse(INCIDENT_ID_KS == "2010_MT-LNF-005057_SIEGEL FIRE", 1, 
                                       ifelse(INCIDENT_ID_KS == "2012_WY-SHF-219_INDEX CREEK", 1, 
                                              ifelse(INCIDENT_ID_KS == "2013_ID-PAF-013040_THUNDER CITY", 1, 
                                                     ifelse(INCIDENT_ID_KS == "2013_WY-SHF-437_BURROUGHS", 1, 
                                                            ifelse(INCIDENT_ID_KS == "2012_ID-NPF-000368_BALLINGER", 1, 
                                                                   ifelse(INCIDENT_ID_KS == "2012_ID-SCF-012151_HALSTEAD", 1, 
                                                                          ifelse(INCIDENT_ID_KS == "2012_MT-BRF-005563_SAWTOOTH", 1, 
                                                                                 ifelse(INCIDENT_ID_KS == "2012_MT-CES-000515_PINE CREEK", 1,
                                                                                        ifelse(INCIDENT_ID_KS == "2012_MT-LCF-002089_GOBLIN GULCH", 1, 
                                                                                               ifelse(INCIDENT_ID_KS == "2012_WY-BTF-000005_BEAR CUB", 1, 
                                                                                                      ifelse(INCIDENT_ID_KS == "2013_MT-BDF-000246_BEAVER CREEK", 1, 
                                                                                                             0))))))))))))) -> sitfull
sitfull$SUPP_DECREASE <- as.factor(sitfull$SUPP_DECREASE)

sitfull %>%
  mutate(SUPP_MIX = ifelse(INCIDENT_ID_KS == "2009_MT-CRA-0131_LITTLE PEOPLE", 2,
                           ifelse(INCIDENT_ID_KS == "2010_WA-SPA-000001_LINE FIRE", 2, 
                                  ifelse(INCIDENT_ID_KS == "2012_ID-CWF-000625_ISABELLA COMPLEX", 2, 
                                         ifelse(INCIDENT_ID_KS == "2012_ID-CWF-000631_SKULL", 2, 
                                                ifelse(INCIDENT_ID_KS == "2012_ID-CWF-000651_LIGHTNING CREEK", 2, 
                                                       ifelse(INCIDENT_ID_KS == "2012_ID-SCF-12190_MUSTANG COMPLEX", 2, 
                                                              ifelse(INCIDENT_ID_KS == "2012_MT-BRF-005436_FIRE MOUNTAIN", 2, 
                                                                     ifelse(INCIDENT_ID_KS == "2012_MT-BRF-005453_ELK TRACK LAKE", 2, 
                                                                            ifelse(INCIDENT_ID_KS == "2012_MT-FNF-000020_PRISONER LAKE", 2, 
                                                                                   ifelse(INCIDENT_ID_KS == "2012_MT-GNF-000144_MILLIE", 2, 
                                                                                          ifelse(INCIDENT_ID_KS == "2012_MT-LCF-002085_ELBOW PASS COMPLEX", 2, 
                                                                                                 ifelse(INCIDENT_ID_KS == "2012_WY-YNP-G5SC_CYGNET COMPLEX", 2, 
                                                                                                        ifelse(INCIDENT_ID_KS == "2013_ID-STF-000389_LITTLE QUEENS", 2,
                                                                                                               ifelse(INCIDENT_ID_KS == "2013_MT-BRF-13039_GOLD PAN COMPLEX", 2,
                                                                                                                      ifelse(INCIDENT_ID_KS == "2009_MT-BDF-000026_BIELENBURG", 2,
                                                                                                                             ifelse(INCIDENT_ID_KS == "2009_MT-BRF-005112_KOOTENAI CREEK", 2,
                                                                                                                                    ifelse(INCIDENT_ID_KS == "2009_MT-FNF-000050_NINKO CREEK", 2,
                                                                                                                                           ifelse(INCIDENT_ID_KS == "2009_WY-GTP-9049_BEARPAW BAY", 2,
                                                                                                                                                  ifelse(INCIDENT_ID_KS == "2010_ID-BOF-000814_WHITEHAWK COMPLEX", 2,
                                                                                                                                                         ifelse(INCIDENT_ID_KS == "2010_ID-SCF-010169_BIGHORN", 2,
                                                                                                                                                                ifelse(INCIDENT_ID_KS == "2010_MT-FNF-000007_CARDINAL CREEK", 2,
                                                                                                                                                                       ifelse(INCIDENT_ID_KS == "2010_MT-CES-0029_NORTH FORK", 2,
                                                                                                                                                                              ifelse(INCIDENT_ID_KS == "2011_ID-IPF-007007_BIRTHDAY", 2,
                                                                                                                                                                                     ifelse(INCIDENT_ID_KS == "2011_ID-NPF-000567_LOG", 2,
                                                                                                                                                                                            ifelse(INCIDENT_ID_KS == "2011_ID-SCF-011175_SADDLE COMPLEX", 2,
                                                                                                                                                                                                   ifelse(INCIDENT_ID_KS == "2011_MT-BDF-052_STEWART FIRE", 2,
                                                                                                                                                                                                          ifelse(INCIDENT_ID_KS == "2011_MT-BRF-005402_FORTY ONE COMPLEX", 2,
                                                                                                                                                                                                                 ifelse(INCIDENT_ID_KS == "2011_MT-GNF-000040_BULL", 2,
                                                                                                                                                                                                                        ifelse(INCIDENT_ID_KS == "2012_WY-WRA-490_ALPINE LAKE", 2,
                                                                                                                                                                                                                               ifelse(INCIDENT_ID_KS == "2013_MT-FHA-075_MISSION FALLS", 2,
                                                                                                                                                                                                                                      ifelse(INCIDENT_ID_KS == "2013_MT-GNF-000050_EMIGRANT", 2,
                                                                                                                                                                                                                                             ifelse(INCIDENT_ID_KS == "2013_MT-GNF-0111_MINER PARADISE COMPLEX", 2, 
                                                                                                                                                                                                                                                    ifelse(INCIDENT_ID_KS == "2013_WY-BTF-000020_KENDALL", 2,
                                                                                                                                                                                                                                                           0)))))))))))))))))))))))))))))))))) -> sitfull
sitfull$SUPP_MIX <- as.factor(sitfull$SUPP_MIX)

sitsuppdec <- subset(sitfull, SUPP_DECREASE == 1)
sitsuppdec$INCIDENT_ID_KS <- as.factor(sitsuppdec$INCIDENT_ID_KS)


sitsuppmix <- subset(sitfull, SUPP_MIX == 2)
sitsuppmix$INCIDENT_ID_KS <- as.factor(sitsuppmix$INCIDENT_ID_KS)

###

##### summarise by incident, include factors #####

###

sitfull %>%
  group_by(INCIDENT_ID_KS) %>%
  summarise(max_area = max(MAX_AREA_INC), fs_class = first(FSCLASS_INC), state = first(OWNERSHIP_STATE), 
            first_year = first(REPORT_YEAR), last_year = last(REPORT_YEAR),
            first_FS = first(SUPPRESSION_METHOD), last_FS = last(SUPPRESSION_METHOD),
            first_unit = first(UnitType), last_unit = last(UnitType), 
            first_imt = first(IMT_TYPE_DESCRIPTION), last_imt = last(IMT_TYPE_DESCRIPTION) 
  ) -> sitinc
summary(sitinc)
#View(sitinc)

#subset sitinc to select only fires over 100 acres:
sitinc_sub <- filter(sitinc, max_area>=100)
summary(sitinc_sub)

#subset sitinc with incidents that have more than one suppression method reported:
sitinc_suppchange <- filter(sitinc, distinct_supp>1)
summary(sitinc_suppchange)
View(sitinc_suppchange)


######################################################


hist(sitfull$INJURIES)
hist(log(sitfull$INJURIES))
hist(log(sitfull$FATALITIES))
summary(sitfull$FATALITIES)

# Setting scientific penalty to 10,000 
# (this will not display scientific abbreviations until >10,000)
options(scipen=10000)

# Using only numerical variables for correlations:

#Class A
classA <- subset(sitfull, sitfull$FSCLASS_INC=='A', select = c(REPORT_DOY, HOUR, START_DOY, AREA, P_CONTAIN,
                                                               INJURIES_TO_DATE, C_WIND_SPEED, C_TEMP, C_RH, 
                                                               F_WIND_SPEED, F_TEMP, F_RH, MAX_AREA_INC))

View(classA)
corA <- cor(classA, use ="complete.obs")
View(corA)

corrplot(corA, method = c("number"), type = c("full"), title = title)
title <- "Correlations for Class A"

#
#
#

#Class B
classB <- subset(sitfull, sitfull$FSCLASS_INC=='B', select = c(REPORT_DOY, HOUR, START_DOY, AREA, P_CONTAIN,
                                                               INJURIES_TO_DATE, C_WIND_SPEED, C_TEMP, C_RH,
                                                               F_WIND_SPEED, F_TEMP, F_RH, MAX_AREA_INC))
corB <- cor(classB, use ="complete.obs")
View(corB)

corrplot(corB, method = c("number"), type = c("full"))


#
#
#

#Class C
classC <- subset(sitfull, sitfull$FSCLASS_INC=='C', select = c(REPORT_DOY, HOUR, START_DOY, AREA, P_CONTAIN,
                                                               INJURIES_TO_DATE, C_WIND_SPEED, C_TEMP, C_RH, 
                                                               F_WIND_SPEED, F_TEMP, F_RH, MAX_AREA_INC))
corC <- cor(classC, use ="complete.obs")
View(corC)

corrplot(corC, method = c("number"), type = c("full"))

#
#
#

#Class D
classD <- subset(sitfull, sitfull$FSCLASS_INC=='D', select = c(REPORT_DOY, HOUR, START_DOY, AREA, P_CONTAIN,
                                                               INJURIES_TO_DATE, C_WIND_SPEED, C_TEMP, C_RH, 
                                                               F_WIND_SPEED, F_TEMP, F_RH, MAX_AREA_INC))
corD <- cor(classD, use ="complete.obs")

corrplot(corD, method = c("number"), type = c("full"))


#
#
#

#Class E
classE <- subset(sitfull, sitfull$FSCLASS_INC=='E', select = c(REPORT_DOY, HOUR, START_DOY, AREA, P_CONTAIN,
                                                               INJURIES_TO_DATE, C_WIND_SPEED, C_TEMP, C_RH, 
                                                               F_WIND_SPEED, F_TEMP, F_RH, MAX_AREA_INC))
corE <- cor(classE, use ="complete.obs")

corrplot(corE, method = c("number"), type = c("full"))


#
#
#

#Class F
classF <- subset(sitfull, sitfull$FSCLASS_INC=='F', select = c(REPORT_DOY, HOUR, START_DOY, AREA, P_CONTAIN,
                                                               INJURIES_TO_DATE, C_WIND_SPEED, C_TEMP, C_RH, 
                                                               F_WIND_SPEED, F_TEMP, F_RH, MAX_AREA_INC))
corF <- cor(classF, use ="complete.obs")

corrplot(corF, method = c("number"), type = c("full"))


#
#
#

#Class G
classG <- subset(sitfull, sitfull$FSCLASS_INC=='G', select = c(REPORT_DOY, HOUR, START_DOY, AREA, P_CONTAIN,
                                                               INJURIES_TO_DATE, C_WIND_SPEED, C_TEMP, C_RH, 
                                                               F_WIND_SPEED, F_TEMP, F_RH, MAX_AREA_INC))
corG <- cor(classG, use ="complete.obs")

corrplot(corG, method = c("number"), type = c("full"))



###

# 

classAthruF <- filter(sitfull, FSCLASS_INC != "G")
View(classAthruF)


############################################################################################################
