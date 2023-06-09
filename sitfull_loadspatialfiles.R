##############################################################################################################################
#
# This script opens the files from ArcGIS. This includes all variables from the original, raw data, but the rows are limited 
#   by the study boundary (determined in ArcGIS). This script then sets the variable classes (*only those relevant to the
#   analysis*) and creates new variables and subsets of the data for a variety of purposes. See headers for specific sections.
#
##############################################################################################################################

##### packages #####
#library("car")
#library("faraway")
require("ggplot2")
require("ggpubr")
require("gplots")
#require("MASS")
library("dplyr")
#library("lattice")
#library("multcomp")
library("corrplot")
#library("openssl")
#library("XML")
#library("rlist")
#library("rrefine")


###

##### load the spatial files #####

# sitfull100_b5km_ele_slo_asp
sitspat5kmESA <- read.csv("/Users/Amber/Documents/GitHub/sit_working/originaldata/Layers_from_ArcGIS_Boundary_062819/sitfull100_b5km_ele_slo_asp.csv", 
                    header=TRUE, na.strings="NA")

# sitfull100_b5km_landcover
sitspat5kmLC <- read.csv("/Users/Amber/Documents/GitHub/sit_working/originaldata/Layers_from_ArcGIS_Boundary_062819/sitfull100_b5km_landcover.csv", 
                          header=TRUE, na.strings="NA")

# sitfull100_spj_mgmtplan
sitspatspjMGMT<- read.csv("/Users/Amber/Documents/GitHub/sit_working/originaldata/Layers_from_ArcGIS_Boundary_062819/sitfull100_spj_mgmtplan.csv", 
                          header=TRUE, na.strings="NA")

# sitfull100_spj_roadraildist
sitspatdistRR <- read.csv("/Users/Amber/Documents/GitHub/sit_working/originaldata/Layers_from_ArcGIS_Boundary_062819/sitfull100_spj_roadraildist.csv", 
                          header=TRUE, na.strings="NA")

#
colnames(sitspat5kmESA)
colnames(sitspat5kmLC)
colnames(sitspatdistRR)
colnames(sitspatspjMGMT)
#
#View(sitspat5kmESA)
#View(sitspat5kmLC)
#View(sitspatdistRR)
#View(sitspatspjMGMT)
#
#nrow(sitspat5kmESA) # 4857
#nrow(sitspat5kmLC) # 374
#nrow(sitspatdistRR) # 4857
#nrow(sitspatspjMGMT) # 4857

# Rename FID columns to streamline merging

# from sitspatdistRR:
colnames(sitspatdistRR)[colnames(sitspatdistRR)=="OBJECTID"] <- "OBJECTID_RR"
colnames(sitspatdistRR)[colnames(sitspatdistRR)=="IN_FID"] <- "FID"
colnames(sitspatdistRR)[colnames(sitspatdistRR)=="NEAR_FID"] <- "RR_FID"
colnames(sitspatdistRR)[colnames(sitspatdistRR)=="NEAR_DIST"] <- "RR_DIST"
colnames(sitspatdistRR)[colnames(sitspatdistRR)=="FROM_X"] <- "SIT_X"
colnames(sitspatdistRR)[colnames(sitspatdistRR)=="FROM_Y"] <- "SIT_Y"
colnames(sitspatdistRR)[colnames(sitspatdistRR)=="NEAR_X"] <- "RR_X"
colnames(sitspatdistRR)[colnames(sitspatdistRR)=="NEAR_Y"] <- "RR_Y"
colnames(sitspatdistRR)[colnames(sitspatdistRR)=="NEAR_ANGLE"] <- "RR_ANGLE"
colnames(sitspatdistRR)[colnames(sitspatdistRR)=="NEAR_FC"] <- "RR_FC"
colnames(sitspatdistRR)[colnames(sitspatdistRR)=="near_dist_km"] <- "RR_DIST_KM"
#

# land cover classes
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_0"] <- "VALUE_0"
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_11"] <- "OPEN WATER"
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_12"] <- "PERENNIAL ICE/SNOW"
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_21"] <- "DEVELOPED, OPEN SPACE"
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_22"] <- "DEVELOPED, LOW INTENSITY"
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_23"] <- "DEVELOPED, MEDIUM INTENSITY"
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_24"] <- "DEVELOPED, HIGH INTENSITY"
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_31"] <- "BARREN LAND (ROCK/SAND/CLAY)"
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_41"] <- "DECIDUOUS FOREST"
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_42"] <- "EVERGREEN FOREST"
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_43"] <- "MIXED FOREST"
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_52"] <- "SHRUB/SCRUB"
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_71"] <- "GRASSLAND/HERBACEOUS"
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_81"] <- "PASTURE/HAY"
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_82"] <- "CULTIVATED CROPS"
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_90"] <- "WOODY WETLANDS"
colnames(sitspat5kmLC)[colnames(sitspat5kmLC)=="VALUE_95"] <- "EMERGENT HERBACEOUS WETLANDS"

#

# >>>>>>>>>>>>>>>>>> should do the renaming for each data frame before or after merging (i.e. Z, Z_Max, etc.)

# Select columsn from sitspatspjMGMT to streamline merging
sitspatspjMGMT2 <- select(sitspatspjMGMT, 1, 7, 136:149)

###

###### merge files #####
# INCIDENT_ID will work to merge these first two because the land cover is at an incident level
sitspat <- merge(sitspat5kmESA, sitspat5kmLC, by = c("INCIDENT_I"))
#summary(sitspat)
# removing columns with NAs ("CONTROLLED" column has 1 cell entered, 4/14/12)
sitspat <- dplyr::select(sitspat, -LTFS_FIRE, -Rowid_, -CONTROLLED)
#colnames(sitspat)
#View(sitspat)
#

sitspat <- merge(sitspat, sitspatdistRR, by = "FID")
#View(sitspat)
#colnames(sitspat)
#

sitspat <- merge(sitspat, sitspatspjMGMT2, by = "FID")
#View(sitspat)
colnames(sitspat)

# I'm not sure what the land cover class 0 is, but it is a type of land cover in the study area 
# summary(sitspat$VALUE_0)

###

##### cleaning up columns in sitspat #####

colnames(sitspat)

# Removing duplicates
sitspat <- dplyr::select(sitspat, -c(13:17, 20, 32, 33, 34, 52, 102))
# columns w/ all NAs: -PERCENT_MM, -EFFECTIVED, -EXPIRATION, -HQ_LOCATIO, -COMMENTS

# Renaming columns we're keeping
colnames(sitspat)[colnames(sitspat)=="INCIDENT_I.x"] <- "INCIDENT_ID_KS"
colnames(sitspat)[colnames(sitspat)=="OWNERSHIP_"] <- "OWNERSHIP_ST"
colnames(sitspat)[colnames(sitspat)=="OWNERSHIP1"] <- "OWNERSHIP_UNIT"
colnames(sitspat)[colnames(sitspat)=="Join_Count"] <- "JOIN_COUNT"
colnames(sitspat)[colnames(sitspat)=="Department"] <- "DEPARTMENT"
colnames(sitspat)[colnames(sitspat)=="Agency"] <- "AGENCY"
colnames(sitspat)[colnames(sitspat)=="Elevation"] <- "ELEVATION"
colnames(sitspat)[colnames(sitspat)=="AREATYPE"] <- "AREA_TYPE"
colnames(sitspat)[colnames(sitspat)=="COMPLEX_FL"] <- "COMPLEX_FLAG"
colnames(sitspat)[colnames(sitspat)=="REPORT_YEA"] <- "REPORT_YEAR"
colnames(sitspat)[colnames(sitspat)=="INCIDENT_N"] <- "INC_NUMBER"
colnames(sitspat)[colnames(sitspat)=="REPORT_DAT"] <- "REPORT_DATE"
colnames(sitspat)[colnames(sitspat)=="INCIDENT_3"] <- "INCIDENT_NAME"
colnames(sitspat)[colnames(sitspat)=="START_DOY_"] <- "START_DOY"
colnames(sitspat)[colnames(sitspat)=="SUPPRESSIO"] <- "SUPPRESSION"
colnames(sitspat)[colnames(sitspat)=="LINE_TO_BU"] <- "LINE_TO_BUILD"
colnames(sitspat)[colnames(sitspat)=="LINE_MEASU"] <- "LINE_MEASURE"
colnames(sitspat)[colnames(sitspat)=="EST_CONTRO"] <- "EST_CONTROL"
colnames(sitspat)[colnames(sitspat)=="DEMOBE_STA"] <- "DEMOBE_START"
colnames(sitspat)[colnames(sitspat)=="INJURIES_T"] <- "INJURIES_TO_DATE"
colnames(sitspat)[colnames(sitspat)=="NO_EVACUAT"] <- "NO_EVAC"
colnames(sitspat)[colnames(sitspat)=="C_WIND_SPE"] <- "C_WIND_SPEED"
colnames(sitspat)[colnames(sitspat)=="F_WIND_SPE"] <- "F_WIND_SPEED"
colnames(sitspat)[colnames(sitspat)=="PRIMARY_FU"] <- "PRIMARY_FUEL_MODEL"
colnames(sitspat)[colnames(sitspat)=="GROWTH_POT"] <- "GROWTH_POTENTIAL"
colnames(sitspat)[colnames(sitspat)=="MAX_AREA_I"] <- "MAX_AREA_INC"
colnames(sitspat)[colnames(sitspat)=="FSCLASS_IN"] <- "FSCLASS_INC"
colnames(sitspat)[colnames(sitspat)=="AREA_Log"] <- "LOG_AREA"
colnames(sitspat)[colnames(sitspat)=="npl"] <- "NPL"
colnames(sitspat)[colnames(sitspat)=="SUPP_DECRE"] <- "SUPP_DECREASE"
#


colnames(sitspat)

###

###### setting factor & numeric variables #####
# Let's look at what R defaulted them to
str(sitspat)

# Factor variables
sitspat$INCIDENT_ID_KS <- as.factor(sitspat$INCIDENT_ID_KS)
sitspat$INC_ID <- as.factor(sitspat$INC_ID)
sitspat$ST_INCNUM <- as.factor(sitspat$ST_INCNUM)
sitspat$INCIDENT_NAME <- as.factor(sitspat$INCIDENT_NAME)
sitspat$STATUS <- as.factor(sitspat$STATUS)
sitspat$AREA_MEASU <- as.factor(sitspat$AREA_MEASU)
sitspat$TYPE_INC <- as.factor(sitspat$TYPE_INC)
sitspat$SUPPRESSION <- as.factor(sitspat$SUPPRESSION)
sitspat$IMT_TYPE <- as.factor(sitspat$IMT_TYPE)
sitspat$IMT_TYPE_D <- as.factor(sitspat$IMT_TYPE_D)
sitspat$CAUSE <- as.factor(sitspat$CAUSE)
sitspat$CAUSE_DESC <- as.factor(sitspat$CAUSE_DESC)
sitspat$UN_USTATE <- as.factor(sitspat$UN_USTATE)
sitspat$UNIT_TYPE <- as.factor(sitspat$UNIT_TYPE)
sitspat$DEPARTMENT <- as.factor(sitspat$DEPARTMENT)
sitspat$AGENCY <- as.factor(sitspat$AGENCY)
sitspat$OWNERSHIP_ST <- as.factor(sitspat$OWNERSHIP_ST)
sitspat$OWNERSHIP_UNIT <- as.factor(sitspat$OWNERSHIP_UNIT)
sitspat$PRIMARY_FUEL_MODEL <- as.factor(sitspat$PRIMARY_FUEL_MODEL)
sitspat$GROWTH_POTENTIAL <- as.factor(sitspat$GROWTH_POTENTIAL)
sitspat$TERRAIN <- as.factor(sitspat$TERRAIN)
sitspat$FSCLASS <- as.factor(sitspat$FSCLASS)
sitspat$IMT_RANK <- as.factor(sitspat$IMT_RANK)
sitspat$FSCLASS_INC <- as.factor(sitspat$FSCLASS_INC)
sitspat$NPL <- as.factor(sitspat$NPL)
sitspat$SUPP_DECREASE <- as.factor(sitspat$SUPP_DECREASE)
sitspat$SUPP_MIX <- as.factor(sitspat$SUPP_MIX)


# Numeric variables
sitspat$REPORT_DOY <- as.numeric(sitspat$REPORT_DOY)
sitspat$HOUR <- as.numeric(sitspat$HOUR)
sitspat$REPORT_YEAR <- as.numeric(sitspat$REPORT_YEAR)
sitspat$START_DOY <- as.numeric(sitspat$START_DOY)
sitspat$AREA <- as.numeric(sitspat$AREA)
sitspat$P_CONTAIN <- as.numeric(sitspat$P_CONTAIN)
sitspat$INJURIES_TO_DATE <- as.numeric(sitspat$INJURIES_TO_DATE)
sitspat$INJURIES <- as.numeric(sitspat$INJURIES)
sitspat$FATALITIES <- as.numeric(sitspat$FATALITIES)
sitspat$C_WIND_SPEED <- as.numeric(sitspat$C_WIND_SPEED)
sitspat$C_TEMP <- as.numeric(sitspat$C_TEMP)
sitspat$C_TEMPH <- as.numeric(sitspat$C_TEMPH)
sitspat$C_TEMPL <- as.numeric(sitspat$C_TEMPL)
sitspat$C_RH <- as.numeric(sitspat$C_RH)
sitspat$C_RHL <- as.numeric(sitspat$C_RHL)
sitspat$C_RHH <- as.numeric(sitspat$C_RHH)
sitspat$F_WIND_SPEED <- as.numeric(sitspat$F_WIND_SPEED)
sitspat$F_TEMP <- as.numeric(sitspat$F_TEMP)
sitspat$F_TEMPL <- as.numeric(sitspat$F_TEMPL)
sitspat$F_TEMPH <- as.numeric(sitspat$F_TEMPH)
sitspat$F_RH <- as.numeric(sitspat$F_RH)
sitspat$F_RHL <- as.numeric(sitspat$F_RHL)
sitspat$F_RHH <- as.numeric(sitspat$F_RHH)
sitspat$SUPPNUM <- as.numeric(sitspat$SUPPNUM)
sitspat$MAX_AREA_INC <- as.numeric(sitspat$MAX_AREA_INC)
sitspat$DURATION <- as.numeric(sitspat$DURATION)
sitspat$ELEVATION <- as.numeric(sitspat$ELEVATION)

str(sitspat)
colnames(sitspat)

sitspatcor <- dplyr::select(sitspat, REPORT_DOY, HOUR, START_DOY, LOG_AREA, P_CONTAIN, INJURIES_TO_DATE, 
                     C_TEMPH, C_RHL, F_TEMPH, F_RHL, MAX_AREA_INC, SUPPNUM, DURATION, ELEVATION, 
                     Z, Z_Mean, RR_DIST_KM)

cor1 <- cor(sitspatcor)
#View(cor1)
corrplot(cor1, method = c("number"), type = c("full"))
#

# again w/o NAs
cor2 <- cor(sitspatcor, use = "complete")
corrplot(cor2, method = c("number"), type = c("full"))
#


###

##### exporting #####

# export sitspat:
#write.csv(sitspat, 
#          "/Users/Amber/Documents/GitHub/sit_working/z_pics_summaries_extracted/sitspat.csv")
#

