##############################################################################################################################
#This script builds upon the "sitfull_prep2" script, adding box plots. Box plots include those grouped by fire size class, 
#   by complex flag (binary), suppression method, & IMT type.
##############################################################################################################################
##### packages #####
# check packages to load
# check packages to load
## car faraway ggplot2 MASS dplyr lattice
#library("car", lib.loc="~/R/win-library/3.5")
#library("faraway", lib.loc="~/R/win-library/3.5")
library("ggplot2", lib.loc="~/R/win-library/3.5")
library(ggplot2)
library("ggpubr", lib.loc="~/R/win-library/3.5")
library("gplots", lib.loc="~/R/win-library/3.5")
library("MASS", lib.loc="~/R/win-library/3.5")
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
#sitfull is the name of this dataset, loaded from the working file sitfull (csv, not txt),
#this file contains data that has been cleaned in OpenRefine.
View(sitfull)
##### factor and numeric variables #####

#numeric variables
sitfull$START_DOY <- as.numeric(sitfull$START_DOY)
sitfull$REPORT_DOY <- as.numeric(sitfull$REPORT_DOY)
sitfull$AREA <- as.numeric(sitfull$AREA)
sitfull$INJURIES_TO_DATE <- as.numeric(sitfull$INJURIES_TO_DATE)
sitfull$INJURIES <- as.numeric(sitfull$INJURIES)
sitfull$FATALITIES <- as.numeric(sitfull$FATALITIES)
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


##### max area per incident #####

#creating new variable for maximum area for an incident

sitfull %>%
  group_by(INCIDENT_ID_KS) %>%
  mutate(MAX_AREA_INC = max(AREA, na.rm = TRUE)) -> sitfull

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

#Creating FSCLASS_INC, assigning the largest fire size class achieved 
#for an incident to all rows within that incident.

sitfull$FSCLASS_INC <- ifelse(sitfull$MAX_AREA_INC < 0.26, "A",
                              ifelse(sitfull$MAX_AREA_INC < 10, "B", 
                                     ifelse(sitfull$MAX_AREA_INC < 100, "C", 
                                            ifelse(sitfull$MAX_AREA_INC < 300, "D", 
                                                   ifelse(sitfull$MAX_AREA_INC < 1000, "E", 
                                                          ifelse(sitfull$MAX_AREA_INC < 5000, "F", "G"))))))
sitfull$FSCLASS_INC <- as.factor(sitfull$FSCLASS_INC)
#subset for easier viewing:
sitfullsubset <- select(sitfull, INCIDENT_ID_KS, AREA, FSCLASS, FSCLASS_INC)

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


##### boxplot settings #####

# Setting scientific penalty to 10,000 
# (this will not display scientific abbreviations until >10,000)
options(scipen=10000)

##### classes A-G + boxplots #####

# Combining variables for smaller boxplots:

#Class A
classA <- subset(sitfull, sitfull$FSCLASS_INC=='A')
bpA1 <- ggplot(classA, aes(x=SUPPRESSION_METHOD, y=MAX_AREA_INC, fill=SUPPRESSION_METHOD)) +
  geom_boxplot() + labs(title="Maximum Fire Area per Suppression Method, Class A",  
                        subtitle="Per Day of Fire Incidents Reported, 2008-2013",
                        x="Suppression Method", y="Maximum Fire Area in Acres")
bpA1 + theme_classic()


bpA2 <- ggplot(classA, aes(x=IMT_TYPE_DESCRIPTION, y=MAX_AREA_INC, fill=IMT_TYPE_DESCRIPTION)) +
  geom_boxplot() + labs(title="Maximum Fire Area per Incident Management 
                        Team (IMT) Type, Class A",  
                        subtitle="Per Day of Fire Incidents Reported, 2008-2013",
                        x="IMT Type", y="Maximum Fire Area in Acres")
bpA2 + theme_classic()

# box plot for multiplots:
bpA3 <- ggplot(classA, aes(x=SUPPRESSION_METHOD, y=MAX_AREA_INC, fill=SUPPRESSION_METHOD)) +
  geom_boxplot(show.legend = FALSE) + labs(title="Class A", 
                                           x="Suppression Method", y="Max Fire Area (acres)") +
  theme_light()

#
#
#

#Class B
classB <- subset(sitfull,sitfull$FSCLASS_INC=='B')
bpB1 <- ggplot(classB, aes(x=SUPPRESSION_METHOD, y=MAX_AREA_INC, fill=SUPPRESSION_METHOD)) +
  geom_boxplot() + labs(title="Maximum Fire Area per Suppression Method, Class B",  
                        subtitle="Per Day of Fire Incidents Reported, 2008-2013",
                        x="Suppression Method", y="Maximum Fire Area in Acres")
bpB1 + theme_classic()


bpB2 <- ggplot(classB, aes(x=IMT_TYPE_DESCRIPTION, y=AREA, fill=IMT_TYPE_DESCRIPTION)) +
  geom_boxplot() + labs(title="Maximum Fire Area per Incident Management 
                        Team (IMT) Type, Class B",  
                        subtitle="Per Day of Fire Incidents Reported, 2008-2013",
                        x="IMT Type", y="Maximum Fire Area in Acres")
bpB2 + theme_classic()

# box plot for multiplots:
bpB3 <- ggplot(classB, aes(x=SUPPRESSION_METHOD, y=MAX_AREA_INC, fill=SUPPRESSION_METHOD)) +
  geom_boxplot(show.legend = FALSE) + labs(title="Class B", 
                                           x="Suppression Method", y="Max Fire Area (acres)") +
  theme_light()

#bpB3

#
#
#

#Class C
classC <- subset(sitfull,sitfull$FSCLASS_INC=='C')
bpC1 <- ggplot(classC, aes(x=SUPPRESSION_METHOD, y=AREA, fill=SUPPRESSION_METHOD)) +
  geom_boxplot() + labs(title="Maximum Fire Area per Suppression Method, Class C", 
                        subtitle="Per Day of Fire Incidents Reported, 2008-2013",
                        x="Suppression Method", y="Maximum Fire Area in Acres")
bpC1 + theme_classic()


bpC2 <- ggplot(classC, aes(x=IMT_TYPE_DESCRIPTION, y=AREA, fill=IMT_TYPE_DESCRIPTION)) +
  geom_boxplot() + labs(title="Maximum Fire Area per Incident Management 
                        Team (IMT) Type, Class C",  
                        subtitle="Per Day of Fire Incidents Reported, 2008-2013",
                        x="IMT Type", y="Maximum Fire Area in Acres")
bpC2 + theme_classic()

# box plot for multiplots:
bpC3 <- ggplot(classC, aes(x=SUPPRESSION_METHOD, y=MAX_AREA_INC, fill=SUPPRESSION_METHOD)) +
  geom_boxplot(show.legend = FALSE) + labs(title="Class C", 
                                           x="Suppression Method", y="Max Fire Area (acres)") +
  theme_light()

#
#
#

#Class D
classD <- subset(sitfull,sitfull$FSCLASS_INC=='D')
bpD1 <- ggplot(classD, aes(x=SUPPRESSION_METHOD, y=AREA, fill=SUPPRESSION_METHOD)) +
  geom_boxplot() + labs(title="Maximum Fire Area per Suppression Method, Class D",  
                        subtitle="Per Day of Fire Incidents Reported, 2008-2013",
                        x="Suppression Method", y="Maximum Fire Area in Acres")
bpD1 + theme_classic()


bpD2 <- ggplot(classD, aes(x=IMT_TYPE_DESCRIPTION, y=AREA, fill=IMT_TYPE_DESCRIPTION)) +
  geom_boxplot() + labs(title="Maximum Fire Area per Incident Management 
                        Team (IMT) Type, Class D",  
                        subtitle="Per Day of Fire Incidents Reported, 2008-2013",
                        x="IMT Type", y="Maximum Fire Area in Acres")
bpD2 + theme_classic()

# box plot for multiplots:
bpD3 <- ggplot(classD, aes(x=SUPPRESSION_METHOD, y=MAX_AREA_INC, fill=SUPPRESSION_METHOD)) +
  geom_boxplot(show.legend = FALSE) + labs(title="Class D", 
                                           x="Suppression Method", y="Max Fire Area (acres)") +
  theme_light()

#
#
#

#Class E
classE <- subset(sitfull,sitfull$FSCLASS_INC=='E')
bpE1 <- ggplot(classE, aes(x=SUPPRESSION_METHOD, y=AREA, fill=SUPPRESSION_METHOD)) +
  geom_boxplot() + labs(title="Maximum Fire Area per Suppression Method, Class E",  
                        subtitle="Per Day of Fire Incidents Reported, 2008-2013",
                        x="Suppression Method", y="Maximum Fire Area in Acres")
bpE1 + theme_classic()


bpE2 <- ggplot(classE, aes(x=IMT_TYPE_DESCRIPTION, y=AREA, fill=IMT_TYPE_DESCRIPTION)) +
  geom_boxplot() + labs(title="Maximum Fire Area per Incident Management 
                        Team (IMT) Type, Class E",  
                        subtitle="Per Day of Fire Incidents Reported, 2008-2013",
                        x="IMT Type", y="Maximum Fire Area in Acres")
bpE2 + theme_classic()

# box plot for multiplots:
bpE3 <- ggplot(classE, aes(x=SUPPRESSION_METHOD, y=MAX_AREA_INC, fill=SUPPRESSION_METHOD)) +
  geom_boxplot(show.legend = FALSE) + labs(title="Class E", 
                                           x="Suppression Method", y="Max Fire Area (acres)") +
  theme_light()

#
#
#

#Class F
classF <- subset(sitfull,sitfull$FSCLASS_INC=='F')
bpF1 <- ggplot(classF, aes(x=SUPPRESSION_METHOD, y=AREA, fill=SUPPRESSION_METHOD)) +
  geom_boxplot() + labs(title="Maximum Fire Area per Suppression Method, Class F",  
                        subtitle="Per Day of Fire Incidents Reported, 2008-2013",
                        x="Suppression Method", y="Maximum Fire Area in Acres")
bpF1 + theme_classic()


bpF2 <- ggplot(classF, aes(x=IMT_TYPE_DESCRIPTION, y=AREA, fill=IMT_TYPE_DESCRIPTION)) +
  geom_boxplot() + labs(title="Maximum Fire Area per Incident Management 
                        Team (IMT) Type, Class F",  
                        subtitle="Per Day of Fire Incidents Reported, 2008-2013",
                        x="IMT Type", y="Maximum Fire Area in Acres")
bpF2 + theme_classic()

# box plot for multiplots:
bpF3 <- ggplot(classF, aes(x=SUPPRESSION_METHOD, y=MAX_AREA_INC, fill=SUPPRESSION_METHOD)) +
  geom_boxplot(show.legend = FALSE) + labs(title="Class F", 
                                           x="Suppression Method", y="Max Fire Area (acres)") +
  theme_light()

#
#
#

#Class G
classG <- subset(sitfull,sitfull$FSCLASS_INC=='G')
bpG1 <- ggplot(classG, aes(x=SUPPRESSION_METHOD, y=AREA, fill=SUPPRESSION_METHOD)) +
  geom_boxplot() + labs(title="Maximum Fire Area per Suppression Method, Class G",  
                        subtitle="Per Day of Fire Incidents Reported, 2008-2013",
                        x="Suppression Method", y="Maximum Fire Area in Acres")
bpG1 + theme_classic()


bpG2 <- ggplot(classG, aes(x=IMT_TYPE_DESCRIPTION, y=AREA, fill=IMT_TYPE_DESCRIPTION)) +
  geom_boxplot() + labs(title="Maximum Fire Area per Incident Management 
                        Team (IMT) Type, Class G",  
                        subtitle="Per Day of Fire Incidents Reported, 2008-2013",
                        x="IMT Type", y="Maximum Fire Area in Acres")
bpG2 + theme_classic()

# box plot for multiplots:
bpG3 <- ggplot(classG, aes(x=SUPPRESSION_METHOD, y=MAX_AREA_INC, fill=SUPPRESSION_METHOD)) +
  geom_boxplot(show.legend = FALSE) + labs(title="Class G", 
                                           x="Suppression Method", y="Max Fire Area (acres)") +
  theme_light()

###

##### multiplots #####

# multiplot of box plots, maximum fire area per suppression method, per fire size class
multiplot(bpA3, bpB3, bpC3, bpD3, bpE3, bpF3, bpG3, cols = 3)
#

##### all classes, classes A-F #####

# More boxplots...
#Strategies:
#     remove outliers:  geom_boxplot(position=position_dodge(1), outlier.shape=NA)
#     limit y axis:   + scale_y_continuous(limits = c(0, #highest you want it to go))


classALL <- (sitfull)
bpALL_complex <- ggplot(classALL, aes(x=SUPPRESSION_METHOD, y=MAX_AREA_INC, fill=COMPLEX_FLAG)) +
  geom_boxplot(position=position_dodge(1), outlier.shape=NA) + 
  labs(title="Maximum Fire Area per Suppression Method, All Classes",
       subtitle="Per Day of Fire Incidents Reported, 2008-2013",
       x="Suppression Method", y="Maximum Fire Area in Acres")
bpALL_complex + theme_classic() + scale_y_continuous(limits = c(0, 150000))

#
#
#

bpALL_complex <- ggplot(classALL, aes(x=SUPPRESSION_METHOD, y=MAX_AREA_INC, fill=COMPLEX_FLAG)) +
  geom_boxplot(position=position_dodge(1)) + 
  labs(title="Maximum Fire Area per Suppression Method, All Classes",
       subtitle="Per Day of Fire Incidents Reported, 2008-2013",
       x="Suppression Method", y="Maximum Fire Area in Acres")
bpALL_complex + theme_classic()

# Adena wants a boxplot of suppression methods using all fire size classes:
#bpareasuppall <- ggplot(swbfull, aes(x=SUPP, y=AREA, fill=SUPP)) +
#  geom_boxplot() = labs(title="Fire Area per Suppression Method, All Fire Size Classes",  
#                        subtitle="Per Day of Fire Incidents Reported, 2008-2013",
#                        x="Suppression Method", y="Area in Acres")

classAthruF <- filter(sitfull, FSCLASS_INC<'G')
View(classAthruF)

bpAthruF_SUPP <- ggplot(classAthruF, aes(x=SUPPRESSION_METHOD, y=MAX_AREA_INC, fill=SUPPRESSION_METHOD)) +
  geom_boxplot(position=position_dodge(1)) + 
  labs(title="Maximum Fire Area per Suppression Method, Classes A-F",
       subtitle="Per Day of Fire Incidents Reported, 2008-2013",
       x="Suppression Method", y="Maximum Fire Area in Acres")
bpAthruF_SUPP + theme_classic()

##### FS v NFS boxplots #####

bp_fsnfs <- ggplot(sitfull, aes(x=SUPPRESSION_BI, y=AREA_Log, fill=SUPPRESSION_BI)) +
  geom_boxplot() +
  labs(x = "Suppression Method (binary)", y = "Log of Area")
bp_fsnfs + theme_light() + theme(legend.position = "none")

##### changes in suppression #####

#Plotting fires with changes in suppression during an incident

##### boxplots - suppression methods decreasing and suppression methods mixing ##### 

bp_suppdec <- ggplot(sitsuppdec, aes(x=INCIDENT_ID_KS, y=AREA, fill=SUPPRESSION_METHOD)) + geom_boxplot() 
bp_suppdec + labs(title=" Area v. Suppression Methods among Incidents 
                  with Decrease in Suppression")

bp_suppmix <- ggplot(sitsuppmix, aes(x=INCIDENT_ID_KS, y=AREA, fill=SUPPRESSION_METHOD)) + geom_boxplot() 
bp_suppmix + labs(title=" Area v. Suppression Methods among Incidents 
                  with Mixed Suppression")
###

# multiplots of line graphs, fire size area per day of incident, per year
#Decreasing Suppression
multiplot(lp_sitsuppdec2010, lp_sitsuppdec2012, lp_sitsuppdec2013, cols = 1)

#Mixed Suppression


##### sawtooth #####
#subsetting sitfull to select just the Sawtooth fire (MT, 2012):
sit_2012sawtooth <- subset(sitfull, INCIDENT_ID_KS == "2012_MT-BRF-005563_SAWTOOTH")
summary(sit_2012sawtooth)
#line plot of sawtooth:
lp_sit_2012sawtooth <- ggplot(sit_2012sawtooth,
                              aes(x=REPORT_DOY, y=AREA,
                                   group=SUPPRESSION_METHOD, color=SUPPRESSION_METHOD)) +
  geom_line() +
  geom_point()+ labs(title = "          Sawtooth Fire, MT", 
                     subtitle = "      Per day of incident, Aug-Oct 2012",
                     xlab = "Report Day of Year (# out of 365)",
                     ylab = "Fire Size Area (acres)")
lp_sit_2012sawtooth

##### pine creek ##### 
#subsetting sitfull to select just the Pine Creek fire (MT, 2012):
sit_2012pinecreek <- subset(sitfull, INCIDENT_ID_KS == "2012_MT-CES-000515_PINE CREEK")
summary(sit_2012pinecreek)
#line plot of pine creek:
lp_sit_2012pinecreek <- ggplot(sit_2012pinecreek, 
                               aes(x=REPORT_DOY, y=AREA,
                                   group=SUPPRESSION_METHOD, color=SUPPRESSION_METHOD)) +
  geom_line() +
  geom_point() + labs(title = "           Pine Creek Fire, MT", 
                      subtitle = "     Per day of incident, Aug-Nov 2012",
                      xlab = "Report Day of Year (# out of 365)",
                      ylab = "Fire Size Area (acres)")
lp_sit_2012pinecreek
##### ballinger ##### 

#subsetting sitfull to select just the Ballinger fire (ID, 2012):
sit_2012ballinger <- subset(sitfull, INCIDENT_ID_KS == "2012_ID-NPF-000368_BALLINGER")
summary(sit_2012ballinger)
#line plot of ballinger:
lp_sit_2012ballinger <- ggplot(sit_2012ballinger, 
                               aes(x=REPORT_DOY, y=AREA,
                                   group=SUPPRESSION_METHOD, color=SUPPRESSION_METHOD)) +
  geom_line() +
  geom_point() + 
  labs(title = "           Ballinger Fire, ID", 
                      subtitle = "     Per day of incident, Aug-Oct 2012",
                      xlab = "Report Day of Year (# out of 365)",
                      ylab = "Fire Size Area (acres)")
lp_sit_2012ballinger

##### banner #####
#subsetting sitfull to select just the banner fire (ID, 2010):
sit_2012banner <- subset(sitfull, INCIDENT_ID_KS == "2010_ID-SCF-10293_BANNER")
summary(sit_2012banner)
#line plot of banner:
lp_sit_2012banner <- ggplot(sit_2012banner, 
                               aes(x=REPORT_DOY, y=AREA,
                                   group=INCIDENT_ID_KS, color=SUPPRESSION_METHOD)) +
  geom_line() +
  geom_point() + labs(title = "           Banner Fire, ID", 
                      subtitle = "     Per day of incident, Aug-Sep 2010",
                      xlab = "Report Day of Year (# out of 365)",
                      ylab = "Fire Size Area (acres)")
lp_sit_2012banner

#mixed plot: banner
mp_sit_2012banner <- ggplot(sit_2012banner, aes(x=REPORT_DOY, y=AREA)) +
  geom_line(aes(x=REPORT_DOY, y=AREA,
                group=SUPPRESSION_METHOD, color=SUPPRESSION_METHOD)) + 
  geom_point(aes(x=REPORT_DOY, y=AREA, color=sit_2012banner$STATUS)) +
  geom_col(aes(x=sit_2012banner$REPORT_DOY, y=sit_2012banner$C_TEMP)) +
  labs(title = "           Banner Fire, ID", 
       subtitle = "     Per day of incident, Aug-Sep 2010",
       x = "Report Day of Year (# out of 365)",
       y = "Fire Size Area (acres)")
mp_sit_2012banner + theme_light() + scale_y_continuous(sec.axis = sec_axis(~./3, name = "Current Temperature (deg F)"))


##### bear cub ##### 

#subsetting sitfull to select just the Bear Cub fire (WY, 2012):
sit_2012bearcub <- subset(sitfull, INCIDENT_ID_KS == "2012_WY-BTF-000005_BEAR CUB")
summary(sit_2012bearcub)
#line plot of bear cub:
lp_sit_2012bearcub <- ggplot(sit_2012bearcub, 
                               aes(x=REPORT_DOY, y=AREA,
                                   group=SUPPRESSION_METHOD, color=SUPPRESSION_METHOD
                                   )) +
  geom_line() +
  geom_point() + labs(title = "           Bear Cub Fire, WY", 
                      subtitle = "     Per day of incident, Jul-Sep 2012",
                      xlab = "Report Day of Year (# out of 365)",
                      ylab = "Fire Size Area (acres)")
lp_sit_2012bearcub

##### goblin gulch #####
#subsetting sitfull to select just the Goblin Gulch fire (MT, 2012):
sit_2012goblin <- subset(sitfull, INCIDENT_ID_KS == "2012_MT-LCF-002089_GOBLIN GULCH")
summary(sit_2012goblin)
#line plot of goblin gulch:
lp_sit_2012goblin <- ggplot(sit_2012goblin, 
                            aes(x=REPORT_DOY, y=AREA, 
                            group=INCIDENT_ID_KS, 
                            color=SUPPRESSION_METHOD)) +
  geom_line() +
  geom_point() + 
  labs(title = "           Goblin Gulch Fire, MT", 
                      subtitle = "     Per day of incident, Jul-Aug 2012",
                      x = "Report Day of Year (# out of 365)",
                      y = "Fire Size Area (acres)")
lp_sit_2012goblin

##### birthday #####
sit_2011birthday <- subset(sitfull, INCIDENT_ID_KS == "2011_ID-IPF-007007_BIRTHDAY")
summary(sit_2011birthday)
#line plot of birthday:
lp_sit_2011birthday <- ggplot(sit_2011birthday, 
                            aes(x=REPORT_DOY, y=AREA,
                                group=INCIDENT_ID_KS, color=SUPPRESSION_METHOD)) +
  geom_line() +
  geom_point() + labs(title = "           Birthday Fire, ID", 
                      subtitle = "     Per day of incident, Sep-Oct 2011",
                      xlab = "Report Day of Year (# out of 365)",
                      ylab = "Fire Size Area (acres)")
lp_sit_2011birthday

#mixed plot: birthday
mp_sit_2011birthday <- ggplot(sit_2011birthday, aes(x=REPORT_DOY, y=AREA)) +
  geom_line(aes(x=REPORT_DOY, y=AREA,
                group=SUPPRESSION_METHOD, color=SUPPRESSION_METHOD)) + 
  geom_point(aes(x=REPORT_DOY, y=AREA, color=sit_2011birthday$STATUS)) +
  geom_col(aes(x=sit_2011birthday$REPORT_DOY, y=sit_2011birthday$C_TEMP)) +
  labs(title = "           Birthday Fire, ID", 
       subtitle = "     Per day of incident, Sep-Oct 2011",
       x = "Report Day of Year (# out of 365)",
       y = "Fire Size Area (acres)")
mp_sit_2011birthday + theme_light() + scale_y_continuous(sec.axis = sec_axis(~./3, name = "Current Temperature (deg F)"))




##### all incidents with decreasing suppression #####
#2010
sitsuppdec2010 <- subset(sitsuppdec, REPORT_YEAR == 2010)

lp_sitsuppdec2010 <- ggplot(sitsuppdec2010, 
                            aes(x=REPORT_DOY, y=AREA, 
                                group=INCIDENT_ID_KS)) +
  geom_line(aes(color=INCIDENT_ID_KS)) +
  geom_point(aes(color=SUPPRESSION_METHOD))  +
  labs(title = "  Decreasing Suppression Methods, 2010", 
       x = "Report Day of Year",
       y = "Fire Size Area (acres)") +
  theme_light()
lp_sitsuppdec2010
#none in 2011
# 2012
sitsuppdec2012 <- subset(sitsuppdec, REPORT_YEAR == 2012)

lp_sitsuppdec2012 <- ggplot(sitsuppdec2012, 
                            aes(x=REPORT_DOY, y=AREA, 
                                group=INCIDENT_ID_KS)) +
  geom_line(aes(color=INCIDENT_ID_KS)) +
  geom_point(aes(color=SUPPRESSION_METHOD)) + 
  labs(title = "  Incidents with Decreasing Suppression Methods", 
       subtitle = "     Per day of incident, 2012",
       x = "Report Day of Year (# out of 365)",
       y = "Fire Size Area (acres)") +
  theme_light()
lp_sitsuppdec2012

#2013
sitsuppdec2013 <- subset(sitsuppdec, REPORT_YEAR == 2013)

lp_sitsuppdec2013 <- ggplot(sitsuppdec2013, 
                            aes(x=REPORT_DOY, y=AREA, 
                                group=INCIDENT_ID_KS)) +
  geom_line(aes(color=INCIDENT_ID_KS)) +
  geom_point(aes(color=SUPPRESSION_METHOD)) + 
  labs(title = "  Decreasing Suppression Methods, 2013", 
       x = "Report Day of Year",
       y = "Fire Size Area (acres)") +
  theme_light()
lp_sitsuppdec2013


##### all incidents with mixed suppression (not decreasing suppression) #####
summary(sitsuppmix)

# 2009
sitsuppmix2009 <- subset(sitsuppmix, REPORT_YEAR == 2009) 
#
lp_sitsuppmix2009 <- ggplot(sitsuppmix2009, 
                            aes(x=REPORT_DOY, y=AREA, 
                                group=INCIDENT_ID_KS)) +
  geom_line(aes(color=INCIDENT_ID_KS)) +
  geom_point(aes(color=SUPPRESSION_METHOD)) +
  labs(title = "  Mixed Suppression Methods, 2009",
       x = "Report Day of Year (# out of 365)",
       y = "Fire Size Area (acres)") + 
  theme_light()
lp_sitsuppmix2009 

# 2010
sitsuppmix2010 <- subset(sitsuppmix, REPORT_YEAR == 2010) 
#
lp_sitsuppmix2010 <- ggplot(sitsuppmix2010, 
                            aes(x=REPORT_DOY, y=AREA, 
                                group=INCIDENT_ID_KS)) +
  geom_line(aes(color=INCIDENT_ID_KS)) +
  geom_point(aes(color=SUPPRESSION_METHOD)) +
  labs(title = "  Incidents with Mixed Suppression Methods", 
       subtitle = "     Per day of incident, 2010",
       x = "Report Day of Year (# out of 365)",
       y = "Fire Size Area (acres)") + 
  theme_light()
lp_sitsuppmix2010

# 2011
sitsuppmix2011 <- subset(sitsuppmix, REPORT_YEAR == 2011) 
#
lp_sitsuppmix2011 <- ggplot(sitsuppmix2011, 
                            aes(x=REPORT_DOY, y=AREA, 
                                group=INCIDENT_ID_KS)) +
  geom_line(aes(color=INCIDENT_ID_KS)) +
  geom_point(aes(color=SUPPRESSION_METHOD)) +
  labs(title = "  Incidents with Mixed Suppression Methods", 
       subtitle = "     Per day of incident, 2011",
       x = "Report Day of Year (# out of 365)",
       y = "Fire Size Area (acres)") + 
  theme_light()
lp_sitsuppmix2011

# 2012
sitsuppmix2012 <- subset(sitsuppmix, REPORT_YEAR == 2012) 
#
lp_sitsuppmix2012 <- ggplot(sitsuppmix2012, 
                            aes(x=REPORT_DOY, y=AREA, 
                                group=INCIDENT_ID_KS)) +
  geom_line(aes(color=INCIDENT_ID_KS)) +
  geom_point(aes(color=SUPPRESSION_METHOD)) +
  labs(title = "  Mixed Suppression Methods, 2012",
       x = "Report Day of Year (# out of 365)",
       y = "Fire Size Area (acres)") + 
  theme_light()
lp_sitsuppmix2012 + scale_y_continuous(limits = c(0, 50000))

# 2013
sitsuppmix2013 <- subset(sitsuppmix, REPORT_YEAR == 2013) 
#
lp_sitsuppmix2013 <- ggplot(sitsuppmix2013, 
                            aes(x=REPORT_DOY, y=AREA, 
                                group=INCIDENT_ID_KS)) +
  geom_line(aes(color=INCIDENT_ID_KS)) +
  geom_point(aes(color=SUPPRESSION_METHOD)) +
  labs(title = "  Mixed Suppression Methods, 2013",
       x = "Report Day of Year (# out of 365)",
       y = "Fire Size Area (acres)") + 
  theme_light() 
lp_sitsuppmix2013

# multiplots for mixed suppression
multiplot(lp_sitsuppmix2009, lp_sitsuppmix2010, lp_sitsuppmix2011, lp_sitsuppmix2012, lp_sitsuppmix2013, cols = 2)


##### specific mixed suppression plots #####
sit_2012elbow <- subset(sitfull, INCIDENT_ID_KS == "2012_MT-LCF-002085_ELBOW PASS COMPLEX")
summary(sit_2012elbow)
mp_sit_2012elbow <- ggplot(sit_2012elbow, 
                            aes(x=REPORT_DOY, y=AREA)) +
  geom_line() +
  geom_point(aes(color=SUPPRESSION_METHOD)) + 
  geom_area(aes(x=REPORT_DOY, y=C_TEMP)) +
  labs(title = "  Elbow Pass Complex, MT", 
       subtitle = "     Per day of incident, 2012",
       x = "Report Day of Year (# out of 365)",
       y = "Fire Size Area (acres)") 

mp_sit_2012elbow + theme_light() + 
  scale_y_continuous(sec.axis = sec_axis(~./70, name = "Current Temperature (deg F)")) +
  theme(legend.key.width = unit(3, "mm")) 
  
# Stewart Fire
sit_2011stewart <- subset(sitfull, INCIDENT_ID_KS == "2011_MT-BDF-052_STEWART FIRE")
summary(sit_2011stewart)

mp_sit_2011stewart <- ggplot(sit_2011stewart, 
                           aes(x=REPORT_DOY, y=AREA)) +
  geom_line() +
  geom_point(aes(color=SUPPRESSION_METHOD)) + 
  geom_col(aes(x=REPORT_DOY, y=(C_TEMP*10)), color = "red", fill = "white") +
  geom_text(aes(x = REPORT_DOY, y = C_TEMP, label = C_TEMP), vjust = -15,
    size = 3.5, inherit.aes = TRUE) + 
  labs(title = "  Stewart Fire, MT", 
       subtitle = "     Per day of incident, 2011",
       x = "Report Day of Year (# out of 365)",
       y = "Fire Size Area (acres)")

mp_sit_2011stewart + theme_light() + 
  scale_y_continuous(sec.axis = sec_axis(~./9, name = "Current Temperature (deg F)")) +
  theme(legend.key.width = unit(3, "mm")) 


# (breaks = round(seq(min(dat$y), max(dat$y), by = 0.5),1))
 
##########################################################################################################
