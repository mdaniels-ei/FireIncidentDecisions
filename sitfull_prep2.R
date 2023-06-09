##############################################################################################################################
#This script opens the file, "sitfull.csv" which is a file created from another script. "sitfull.csv" is a joined dataset that
#   includes all variables from the original, raw data, but the rows are limited by the study boundary (determined in ArcGIS).
#   This script then sets the variable classes (*only those relevant to analysis*) and creates new variables and subsets of
#   the data for a variety of purposes. See headers for specific sections.
##############################################################################################################################
##### packages #####
# check packages to load
## car faraway ggplot2 MASS dplyr lattice
library("car")
#library("faraway")
require("ggplot2")
require("ggpubr")
require("gplots")
require("MASS")
library("dplyr")
#library("lattice")
#library("multcomp")
library("corrplot")
#library("openssl")
#library("XML")
#library("rlist")
#library("rrefine")

##### directory and data #####
# setting work directory
saveDir <- getwd() # get the current working directory
saveDir # show me the saved directory

#swdG <- "C:/Users/mcdaniels2/Documents/GitHub/sit_working"
# path to my project linked to GitHub
swdG <- "C:/Users/Amber/Documents/GitHub/sit_working"
# path to my project linked to GitHub
#swdG <- "C:/Users/mcdaniels2/Documents/GitHub/sit_working"
# path to my project linked to GitHub

setwd(swdG) # set this path as my work directory 
# directory is set.

# OLD loading text:
#swd <- "C:/Users/Molly/Desktop/sit_working/sit_0813_inb" 
# path to my project on the desktop - outdated, should not use this.
# load the data
#sitfull <- read.csv("C:/Users/Molly/Desktop/sit_working/sit_0813_inb/workingdata/sitfull.csv", 
#                    header=TRUE, na.strings="NA") 
# sitfull is the name of this dataset, loaded from the working file sitfull (csv, not txt),
# this file contains data that has been cleaned in OpenRefine, and because it was opened in 
#   OpenRefine, the NA's are not blank cells, but contain the letters "NA".


#loading data from GitHub, home laptop:
#sitfull <- read.csv("C:/Users/mcdaniels2/Documents/GitHub/sit_working/workingdata/sitfull.csv", 
#                    header=TRUE, na.strings="NA")
# sitfull is the name of this dataset, loaded from the working file sitfull (csv, not txt),
# this file contains data that has been cleaned in OpenRefine, and because it was opened in 
# OpenRefine, the NA's are not blank cells, but contain the letters "NA".

#loading data from GitHub, Lab laptop:
sitfull <- read.csv("/Users/Amber/Documents/GitHub/sit_working/z_pics_summaries_extracted/sitfull.csv", 
                    header=TRUE, na.strings="NA")
# sitfull is the name of this dataset, loaded from the working file sitfull (csv, not txt),
# this file contains data that has been cleaned in OpenRefine, and because it was opened in 
# OpenRefine, the NA's are not blank cells, but contain the letters "NA".

#loading data from GitHub, Russell Computer:
#sitfull <- read.csv("C:/Users/mcdaniels2/Documents/GitHub/sit_working/workingdata/sitfull.csv", 
#                    header=TRUE, na.strings="NA")
# sitfull is the name of this dataset, loaded from the working file sitfull (csv, not txt),
# this file contains data that has been cleaned in OpenRefine, and because it was opened in 
# OpenRefine, the NA's are not blank cells, but contain the letters "NA".

#View(sitfull)

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
sitfull$SUPPRESSION_METHOD <- ordered(sitfull$SUPPRESSION_METHOD, levels = c("FS", "CC", "PZ", "MM"))
sitfull$SUPPNUM <- as.numeric(sitfull$SUPPRESSION_METHOD)
sitfull$IMT_TYPE <- as.factor(sitfull$IMT_TYPE)
sitfull$IMT_TYPE <- ordered(sitfull$IMT_TYPE, levels = 
                                          c("C", "D", "1", "7", "2", "8", "3", "4", "F", "E", "B", "5", "6"))
sitfull$IMT_TYPE_DESCRIPTION <- as.factor(sitfull$IMT_TYPE_DESCRIPTION)
sitfull$IMT_TYPE_DESCRIPTION <- ordered(sitfull$IMT_TYPE_DESCRIPTION, levels = 
                                          c("Area Command", "Unified Command", "Type 1 Team", "Type 1 IC", "Type 2 Team",
                                            "Type 2 IC", "Type 3 Team", "Type 3 IC", "FUMT", "SOPL", "FUM2", "Type 4 IC",
                                            "Type 5 IC"))
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

# creating INC_ID, which assigns each incident ID a number
sitfull <- transform(sitfull,INC_ID=as.numeric(interaction(INCIDENT_ID_KS)))
sitfull <- arrange(sitfull, INCIDENT_ID_KS, REPORT_DOY)
#View(sitfull)

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

# creating a new variable for the log of AREA to normalize data

hist(sitfull$AREA)
hist(log1p(sitfull$AREA))
sitfull %>%
  mutate(LOG_AREA = ifelse( !all(is.na(AREA)), 
                            log1p(AREA), 
                            NA)) -> sitfull
sitfull$LOG_AREA <- as.numeric(sitfull$LOG_AREA)

###

##### assigning national preparedness levels #####

# creating variable npl in sitfull, using natpreplev dataframe as a guide:
sitfull %>%
  mutate(npl = ifelse(REPORT_YEAR <2009 & REPORT_DOY < 123 | 
                        REPORT_YEAR <2009 & REPORT_DOY > 278, 1, 
                      
                      ifelse(REPORT_YEAR <2009 & REPORT_DOY < 173 | 
                               REPORT_YEAR <2009 & REPORT_DOY > 245, 2, 
                             
                             ifelse(REPORT_YEAR <2009 & REPORT_DOY < 176 | 
                                      REPORT_YEAR <2009 & REPORT_DOY > 233, 3, 
                                    
                                    ifelse(REPORT_YEAR <2009 & REPORT_DOY < 182 | 
                                             REPORT_YEAR <2009 & REPORT_DOY > 204, 4,
                                           
                                           ifelse(REPORT_YEAR <2009 & REPORT_DOY < 204, 5,
                                                  
                                                  ifelse(REPORT_YEAR <2010 & REPORT_DOY < 190 | 
                                                           REPORT_YEAR <2010 & REPORT_DOY > 279, 1, 
                                                         
                                                         ifelse(REPORT_YEAR <2010 & REPORT_DOY < 210 | 
                                                                  REPORT_YEAR <2010 & REPORT_DOY > 250, 2, 
                                                                
                                                                ifelse(REPORT_YEAR <2010 & REPORT_DOY < 230 | 
                                                                         REPORT_YEAR <2010 & REPORT_DOY > 243, 3,
                                                                       
                                                                       ifelse(REPORT_YEAR <2011 & REPORT_DOY < 152 | 
                                                                                REPORT_YEAR <2011 & REPORT_DOY > 285, 1, 
                                                                              
                                                                              ifelse(REPORT_YEAR <2012 & REPORT_DOY < 129 | 
                                                                                       REPORT_YEAR <2012 & REPORT_DOY > 286, 1,
                                                                                     
                                                                                     ifelse(REPORT_YEAR <2012 & REPORT_DOY < 128 & REPORT_DOY < 154 | 
                                                                                              REPORT_YEAR <2012 & REPORT_DOY > 195 & REPORT_DOY < 238 |
                                                                                              REPORT_YEAR <2012 & REPORT_DOY > 260 & REPORT_DOY < 287, 2, 
                                                                                            
                                                                                            ifelse(REPORT_YEAR <2012 & REPORT_DOY > 152 & REPORT_DOY < 195 | 
                                                                                                     REPORT_YEAR <2012 & REPORT_DOY > 236 & REPORT_DOY < 251 |
                                                                                                     REPORT_YEAR <2012 & REPORT_DOY > 257 & REPORT_DOY <262, 3, 
                                                                                                   
                                                                                                   ifelse(REPORT_YEAR <2012 & REPORT_DOY > 250 & REPORT_DOY < 259, 4,
                                                                                                          
                                                                                                          ifelse(REPORT_YEAR <2013 & REPORT_DOY < 134 |
                                                                                                                   REPORT_YEAR <2013 & REPORT_DOY > 288, 1,
                                                                                                                 
                                                                                                                 ifelse(REPORT_YEAR <2013 & REPORT_DOY > 133 & REPORT_DOY < 162 |
                                                                                                                          REPORT_YEAR <2013 & REPORT_DOY > 206 & REPORT_DOY < 215 |
                                                                                                                          REPORT_YEAR <2013 & REPORT_DOY > 273 & REPORT_DOY < 289, 2,
                                                                                                                        
                                                                                                                        ifelse(REPORT_YEAR <2013 & REPORT_DOY > 161 & REPORT_DOY < 179 |
                                                                                                                                 REPORT_YEAR <2013 & REPORT_DOY > 196 & REPORT_DOY < 208 |
                                                                                                                                 REPORT_YEAR <2013 & REPORT_DOY > 213 & REPORT_DOY < 225 |
                                                                                                                                 REPORT_YEAR <2013 & REPORT_DOY > 245 & REPORT_DOY < 275, 3,
                                                                                                                               
                                                                                                                               ifelse(REPORT_YEAR <2013 & REPORT_DOY > 177 & REPORT_DOY < 198 |
                                                                                                                                        REPORT_YEAR <2013 & REPORT_DOY > 219 & REPORT_DOY < 246, 4,
                                                                                                                                      
                                                                                                                                      ifelse(REPORT_YEAR <2014 & REPORT_DOY < 154 |
                                                                                                                                               REPORT_YEAR <2014 & REPORT_DOY > 264, 1, 
                                                                                                                                             
                                                                                                                                             ifelse(REPORT_YEAR <2014 & REPORT_DOY > 153 & REPORT_DOY < 172 |
                                                                                                                                                      REPORT_YEAR <2014 & REPORT_DOY > 246 & REPORT_DOY < 266 |
                                                                                                                                                      REPORT_YEAR <2014 & REPORT_DOY > 191 & REPORT_DOY < 203, 2,
                                                                                                                                                    
                                                                                                                                                    ifelse(REPORT_YEAR <2014 & REPORT_DOY > 170 & REPORT_DOY < 193 |
                                                                                                                                                             REPORT_YEAR <2014 & REPORT_DOY > 201 & REPORT_DOY < 221 |
                                                                                                                                                             REPORT_YEAR <2014 & REPORT_DOY > 243 & REPORT_DOY < 248, 3,
                                                                                                                                                           
                                                                                                                                                           ifelse(REPORT_YEAR <2014 & REPORT_DOY > 219 & REPORT_DOY < 231 |
                                                                                                                                                                    REPORT_YEAR <2014 & REPORT_DOY > 236 & REPORT_DOY < 245, 4, 
                                                                                                                                                                  
                                                                                                                                                                  ifelse(REPORT_YEAR <2014 & REPORT_DOY > 230 & REPORT_DOY < 238, 5, 0
                                                                                                                                                                  ))))))))))))))))))))))) -> sitfull
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

# for fires with mixed suppression, not just a decrease:
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
                                                                                                                                                                            ifelse(INCIDENT_ID_KS == "2010_WY-SHF-310_NORTH FORK", 2,
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
#View(sitsuppmix)
###


##### summarise by incident, include factors #####

# 4-1-2019: trying a new summarizing method to separate IMT types within incidents
head(sitfull$IMT_TYPE)

# summarizing by incident: 
sitfull %>%
  group_by(INCIDENT_ID_KS) %>%
  summarise(lat = first(LATITUDE), long = first(LONGITUDE), max_area = max(MAX_AREA_INC), 
            fs_class = first(FSCLASS_INC), state = first(OWNERSHIP_STATE), 
            first_year = first(REPORT_YEAR), last_year = last(REPORT_YEAR),
            first_supp = first(SUPPRESSION_METHOD), last_supp = last(SUPPRESSION_METHOD), 
            distinct_supp = n_distinct(SUPPRESSION_METHOD),
            first_suppbi = first(SUPPRESSION_BI), last_suppbi = last(SUPPRESSION_BI),
            first_unit = first(UnitType), last_unit = last(UnitType), 
            first_imt = first(IMT_TYPE_DESCRIPTION), last_imt = last(IMT_TYPE_DESCRIPTION), 
            all_imt = paste(IMT_TYPE, collapse = ", ")) -> sitinc2

#View(sitinc2) # success!

summary(sitinc2)

#subset sitinc2 to select only fires over 100 acres:
sitinc2.100 <- filter(sitinc2, max_area>=100)
summary(sitinc2.100)

# RENAME | RENAMING | NEW NAME
# renaming sitinc2.100 for easier coding:
sitinc375 <- sitinc2.100 # because there are 375 incidents with maximum areas >=100
#View(sitinc375)

###

#subset sitinc with incidents that have more than one suppression method reported:
sitinc_suppchange <- filter(sitinc375, distinct_supp>1)
summary(sitinc_suppchange)
#View(sitinc_suppchange)


##### sitfull100: fires with maximum area >= 100 #####

# this will limit sitfull100, a subset of sitfull, to only fires with maximum 
# fire sizes of >=100 acres

sitfull100 <- filter(sitfull, MAX_AREA_INC>=100)
#nrow(sitfull100) = 4815
sitfull100 <- arrange(sitfull100, INCIDENT_ID_KS, REPORT_DOY)
###

##### creating variable: duration of incident #####

sitfull100 %>%
  group_by(INCIDENT_ID_KS) %>%
  mutate(DURATION = ifelse( !all(is.na(REPORT_DOY)), 
                                (max(REPORT_DOY, na.rm=TRUE)-min(REPORT_DOY, na.rm=TRUE)), 
                                NA)) -> sitfull100

sitfull100$DURATION <- as.numeric(sitfull100$DURATION)
#View(sitfull100) # it worked, but it's hard to see

verify.duration <- select(sitfull100,c(2,10,11,19,89))
#View(verify.duration) # it worked! This is easier to see that it worked

###

##### creating variable: local-national ranking ####
# grouping based on IMT_TYPE, which is described in the excel file with Variable Descriptions
# [also, all of this info is on the sticky notes in the office]
sitfull100$IMT_RANK <- ifelse(sitfull100$IMT_TYPE == "C", "A",
                          ifelse(sitfull100$IMT_TYPE == "D", "A",  
                                        ifelse(sitfull100$IMT_TYPE == "1", "B", 
                                               ifelse(sitfull100$IMT_TYPE == "7", "B",
                                                      ifelse(sitfull100$IMT_TYPE == "2", "C",
                                                             ifelse(sitfull100$IMT_TYPE == "8", "C",
                                                                    ifelse(sitfull100$IMT_TYPE == "3", "D",
                                                                           ifelse(sitfull100$IMT_TYPE == "4", "D",
                                                                                  ifelse(sitfull100$IMT_TYPE == "F", "D",
                                                                                         ifelse(sitfull100$IMT_TYPE == "E", "D",
                                                                                                ifelse(sitfull100$IMT_TYPE == "B", "D",
                                                                                                       ifelse(sitfull100$IMT_TYPE == "5", "E",
                                                                                                              ifelse(sitfull100$IMT_TYPE == "6", "F", "")))))))))))))
                                                      
# Setting IMT_RANK as a categorical variable:
sitfull100$IMT_RANK <- as.factor(sitfull100$IMT_RANK)

# viewing sitfull100 with select columns to see IMT ranking
summary(sitfull100)
#verify.imtrank <- select(sitfull100, c("INCIDENT_ID_KS", "STATUS", "REPORT_DATE","REPORT_DOY", "DURATION", 
#                                       "IC_NAME", "IMT_RANK", "IMT_TYPE_DESCRIPTION", "AREA", "FSCLASS",
#                                       "SUPPRESSION_METHOD"))
#verify.imtrank <- arrange(verify.imtrank, INCIDENT_ID_KS, REPORT_DOY)
#View(verify.imtrank)

###

##### creating sitsuppchange dataframe #####
# all days for each incident with changes in suppression
#summary(sitinc_suppchange)
#View(sitinc_suppchange)
sitfull_suppdec <- subset(sitfull100, sitfull100$SUPP_DECREASE == "1", select = INCIDENT_ID_KS:IMT_RANK)
#View(sitfull_suppdec)
sitfull_suppmix <- subset(sitfull100, sitfull100$SUPP_MIX == "2", select = INCIDENT_ID_KS:IMT_RANK)
#View(sitfull_suppmix)
#
sitsuppchange <- bind_rows(sitfull_suppmix, sitfull_suppdec)
sitsuppchange <- arrange(sitsuppchange, INCIDENT_ID_KS, REPORT_DOY)
#View(sitsuppchange)
summary(sitsuppchange$SUPPRESSION_METHOD) # order of levels stayed the same
summary(sitsuppchange$IMT_TYPE_DESCRIPTION) # order of levels stayed the same
###

##### sitsuppchange variables #####

# creating variables that indicate a change in suppression or IMT/IC type
sitsuppchange$SUPPNUM <- as.numeric(sitsuppchange$SUPPRESSION_METHOD)
sitsuppchange$IMT_RANKNUM <- as.numeric(sitsuppchange$IMT_RANK)

# variable that shows change in suppression method

sitsuppchange %>%
  group_by(INCIDENT_ID_KS) %>%
  mutate(DIFF_SUPP = SUPPNUM - lag(SUPPNUM)) -> sitsuppchange

# old attempt
#sitsuppchange %>%
#  group_by(INCIDENT_ID_KS) %>%
#  mutate(DIFF_SUPP = c(0, diff(SUPPNUM))) -> sitsuppchange

# viewing subset of columns to verify new variable:
sitsuppchange.suppdiff <-select(sitsuppchange, c("INCIDENT_ID_KS", "STATUS", "SUPPRESSION_METHOD", "SUPPNUM",
                                                 "DIFF_SUPP", "SUPPRESSION_BI", "SUPP_DECREASE", "SUPP_MIX",
                                                 "REPORT_DOY"))
sitsuppchange.suppdiff<- arrange(sitsuppchange.suppdiff, INCIDENT_ID_KS, REPORT_DOY)
#View(sitsuppchange.suppdiff)

# variable that shows change in IMT type
summary(sitsuppchange$IMT_RANK)
sitsuppchange %>%
  group_by(INCIDENT_ID_KS) %>%
  mutate(DIFF_IMT = c(0, diff(IMT_RANKNUM))) -> sitsuppchange

sitsuppchange.imtdiff <-select(sitsuppchange, c("INCIDENT_ID_KS", "STATUS", "IMT_TYPE_DESCRIPTION", "IMT_RANK",
                                                 "DIFF_IMT", "SUPPRESSION_METHOD","DIFF_SUPP", "REPORT_DOY"))
sitsuppchange.imtdiff<- arrange(sitsuppchange.imtdiff, INCIDENT_ID_KS, REPORT_DOY)
#View(sitsuppchange.imtdiff)

# variable that shows difference in preparendess level:
summary(sitsuppchange)

sitsuppchange %>%
  group_by(INCIDENT_ID_KS) %>%
  mutate(DIFF_NPL = c(0, diff(npl))) -> sitsuppchange
#View(sitsuppchange)
###

##### adding variable showing from-to in suppression method to sitsuppchange #####

sitsuppchange %>%
  mutate(SUPPCHANGE = paste(lag(SUPPRESSION_METHOD), "->", SUPPRESSION_METHOD)) -> sitsuppchange
#View(sitsuppchange)

###

##### adding variable showing from-to in IMT type to sitsuppchange #####

sitsuppchange %>%
  mutate(IMTCHANGE = paste(lag(IMT_TYPE_DESCRIPTION), "->", IMT_TYPE_DESCRIPTION)) -> sitsuppchange
#View(sitsuppchange)

###

##### adding variable showing from-to in IMT Ranking to sitsuppchange #####

sitsuppchange %>%
  mutate(IMTRCHANGE = paste(lag(IMT_RANK), "->", IMT_RANK)) -> sitsuppchange
#View(sitsuppchange)

###

##### adding variable showing from-to in npl changes to sitsuppchange #####

sitsuppchange %>%
  mutate(NPLCHANGE = paste(lag(npl), "->", npl)) -> sitsuppchange
#View(sitsuppchange)

###

##### insert more here #####

###

#
# insert more changes to sitfull_prep2 here
#

###

##### summary table #####

#sumtbl_sitfull <- summary(sitfull)
#View(sumtbl_sitfull)

#write.csv(sitsuppchange, file = 
#            "C:/Users/mcdaniels2/Documents/GitHub/sit_working/z_pics_summaries_extracted/sitsuppchange_061419.csv")

###

##### export sitfull #####

#write.csv(sitinc375, file = 
#            "C:/Users/mcdaniels2/Documents/GitHub/sit_working/z_pics_summaries_extracted/sitinc375.csv")

#write.csv(sitjoin100, file = 
#            "C:/Users/mcdaniels2/Documents/GitHub/sit_working/z_pics_summaries_extracted/sitincjoin100.csv")

###
############################################################################################################
# FYI: Use rm() to remove unwanted variables/tables/products.


#summary(ClassA)
#summary(sitfull$FSCLASS_INC)

##### notes #####

# 4-1-2019: deleted a long string of seemingly the same stuff as above, but repeated?
