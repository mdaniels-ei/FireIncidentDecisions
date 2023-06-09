##############################################################################################################################
#This script builds upon the "sitfull_prep2" script, adding regression.
#
##############################################################################################################################
##### packages #####
# check packages to load
## car faraway ggplot2 MASS dplyr lattice
library("car", lib.loc="~/R/win-library/3.5")
#library("faraway", lib.loc="~/R/win-library/3.5")
library("ggplot2", lib.loc="~/R/win-library/3.5")
library("ggpubr", lib.loc="~/R/win-library/3.5")
library("gplots", lib.loc="~/R/win-library/3.5")
#library("MASS", lib.loc="~/R/win-library/3.5")
library("dplyr", lib.loc="~/R/win-library/3.5")
#library("lattice", lib.loc="~/R/win-library/3.5")
library("multcomp", lib.loc="~/R/win-library/3.5")
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

#sumtbl_sitfull <- summary(sitfull)
#View(sumtbl_sitfull)

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


########################################################
##### Analyses of FS v NFS #####
# Separate dataframe for analyses:
sitsupp_ANOVA <- select(sitfull, INCIDENT_ID_KS, SUPPRESSION_METHOD, SUPPRESSION_BI, AREA, AREA_Log, FSCLASS, FSCLASS_INC, 
                        IMT_TYPE, OWNERSHIP_STATE, PRIMARY_FUEL_MODEL, TERRAIN)

# Log Reg: fire size & suppression, FS v. NFS ("SUPPRESSION_BI")

glm.fit <- glm(sitfull$SUPPRESSION_BI ~ sitfull$AREA_Log, family = binomial, na.action = na.omit)
summary(glm.fit)

step(glm.fit, test="LRT")

#glmfit <-summary.glm(glm.fit)$coefficients
#write.table(glmfit, "C:/Users/Molly/Desktop/sit_working/sit_0813_inb/z_pics_summaries_extracted/glmfit.txt", sep="\t") 

###

# More analyses with FS v. NFS ("SUPPRESSION_BI")

# Average fire size for NFS:
group_by(sitsupp_ANOVA, SUPPRESSION_BI) %>%
  summarise(
    count = n(),
    mean = mean(AREA_Log, na.rm = TRUE),
    sd = sd(AREA_Log, na.rm = TRUE)
  )

bp_sitsuppbi_arealog <- ggplot(sitsupp_ANOVA, aes(x=SUPPRESSION_BI, y=AREA_Log, fill=SUPPRESSION_BI)) + 
  geom_boxplot() + labs(title = "  Full Suppression v. Non-Full Suppression (no NAs) 
                                v. 
                           Log of Area", 
                        subtitle = "    Per Day of Fire Incidents Reported, 2008-2013", 
                        x = "Full Suppression v. All Other Methods (CC, MM, PZ)", 
                        y = "Log of Area")
bp_sitsuppbi_arealog + theme_classic()

# independent 2-group t-test for areas of FS v. NFS:
t.test(AREA_Log ~ SUPPRESSION_BI, data = sitsupp_ANOVA)


##### Suppression Methods, ANOVA and ANOVA2 subset #####

# One-Way ANOVA: suppression methods ("SUPPRESSION_METHOD"; not the binary "SUPPRESSION_BI")

group_by(sitsupp_ANOVA, SUPPRESSION_METHOD) %>%
  summarise(
    count = n(),
    mean = mean(AREA_Log, na.rm = TRUE),
    sd = sd(AREA_Log, na.rm = TRUE)
  )

bp_sitsupp_arealog <- ggplot(sitsupp_ANOVA, aes(x=SUPPRESSION_METHOD, y=AREA_Log, fill=SUPPRESSION_METHOD)) + 
  geom_boxplot(show.legend = FALSE) + labs(x = "Suppression Methods", 
                        y = "Log of Area")
bp_sitsupp_arealog + theme_classic()

# Mean plots

# Plot area by suppression method
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)

sitsupp_ANOVA2 <- na.omit(select(sitfull, INCIDENT_ID_KS, SUPPRESSION_METHOD, SUPPRESSION_BI, 
                                  AREA, AREA_Log, FSCLASS, FSCLASS_INC, IMT_TYPE, 
                                  OWNERSHIP_STATE, PRIMARY_FUEL_MODEL, TERRAIN))
summary(sitsupp_ANOVA2)
#plotting ANOVA2
ggline(sitsupp_ANOVA2, x = "SUPPRESSION_METHOD", y = "AREA_Log", 
       add = c(mean_se, jitter), 
       ylab = "Log of Area", xlab = "Suppression Method",
       error.plot = "errorbar")
# plotmeans: plotting the mean log of area per suppression method
plotmeans(AREA ~ SUPPRESSION_METHOD, data = sitsupp_ANOVA2, frame = FALSE,
          xlab = "Suppression Method", ylab = "Area",
          main="Mean Plot with 95% CI ((2) NAs removed", mean.labels = TRUE, n.label = TRUE,
          use.t = TRUE)

#using ANOVA3 for analyses (different variables)
sitsupp_ANOVA3 <- na.omit(select(sitfull, INCIDENT_ID_KS, REPORT_DOY, SUPPRESSION_METHOD, SUPPRESSION_BI, 
                                 AREA, AREA_Log, FSCLASS, FSCLASS_INC, OWNERSHIP_STATE, 
                                 NWCG_UNITID, UNIT_TYPE, Department, TERRAIN))
summary(sitsupp_ANOVA3)

#Plotting ANOVA3; grouped by suppression methods
ggline(sitsupp_ANOVA3, x = "SUPPRESSION_METHOD", y = "AREA_Log", 
       add = c(mean_se, jitter), 
       ylab = "Log of Area", xlab = "Suppression Method",
       error.plot = "errorbar")
# 
plotmeans(AREA_Log ~ SUPPRESSION_METHOD, data = sitsupp_ANOVA3, barwidth=2,
          xlab = "Suppression Method", ylab = "Log of Area", frame = FALSE, 
          main="Mean Plot with 95% CI (NAs removed)", mean.labels = TRUE, n.label = TRUE, 
          use.t = TRUE)


##### aov - Log of Area #####
# Compute the analysis of variance
supp.area.aov2b <- aov(AREA_Log ~ SUPPRESSION_METHOD, data = sitsupp_ANOVA2)
# Summary of the analysis
summary(supp.area.aov2b)
# Using Tukey's HSD for multiple pairwise-comparison between Suppression Method means
TukeyHSD(supp.area.aov2b)
# Multiple Comparisons procedure for ANOVA
summary(glht(supp.area.aov2b, linfct = mcp(SUPPRESSION_METHOD = "Tukey")))
# 1. Homogeneity of variances
plot(supp.area.aov2b, 1)
title("Variances, Log of Area & Suppression Methods")
# 2. Homogeneity of variables - Levene's Test
leveneTest(AREA_Log ~ SUPPRESSION_METHOD, data = sitsupp_ANOVA2)

###### aov 2 - Area #####
# Compute the analysis of variance
supp.area.aov2 <- aov(AREA ~ SUPPRESSION_METHOD, data = sitsupp_ANOVA2)
# Summary of the analysis
summary(supp.area.aov2)
# Using Tukey's HSD for multiple pairwise-comparison between Suppression Method means
TukeyHSD(supp.area.aov2)
# Multiple Comparisons procedure for ANOVA
summary(glht(supp.area.aov2, linfct = mcp(SUPPRESSION_METHOD = "Tukey")))
# 1. Homogeneity of variances
plot(supp.area.aov2, 1)
title("Variances, Area & Suppression Methods")
# 2. Homogeneity of variables - Levene's Test
leveneTest(AREA ~ SUPPRESSION_METHOD, data = sitsupp_ANOVA)

##### glm (FS & NFS, +variables) #####
# https://www.ssc.wisc.edu/sscc/pubs/RFR/RFR_RegressionGLM.html 
#Using the subset sitsupp_ANOVA2, which has selected variables and NAs removed
summary(sitsupp_ANOVA2)
str(sitsupp_ANOVA2)

# glm, FS/NFS with independent variables
# using SUPPRESSION_BI as dependent variable, running regression:
sitg1 <- glm(SUPPRESSION_BI ~ AREA_Log+ IMT_TYPE + OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL, 
             family=binomial, data=sitsupp_ANOVA2)
summary(sitg1)
step(sitg1, test="LRT")

#as suggested by step, the glm again:
sitg2 <- glm(formula = SUPPRESSION_BI ~ AREA_Log + IMT_TYPE + OWNERSHIP_STATE + 
               TERRAIN + PRIMARY_FUEL_MODEL, family = binomial, data = sitsupp_ANOVA2)
summary(sitg2)
###

# regression with fewer variables
# based on "sitg1" and sitg2" but without IMT_TYPE and PRIMARY_FUEL_MODEL
sitg3 <- glm(formula = SUPPRESSION_BI ~ AREA_Log + OWNERSHIP_STATE + 
               TERRAIN, family = binomial, data = sitsupp_ANOVA3)
summary(sitg3)
plot(sitg3)
#
##### lms, FS v NFS #####
sitg3a <- lm(AREA_Log ~ SUPPRESSION_BI, data=sitsupp_ANOVA3)
summary(sitg3a)
sitg3b <- lm(AREA_Log ~ TERRAIN, data=sitsupp_ANOVA3)
summary(sitg3b)
sitg3c <- lm(AREA_Log ~ OWNERSHIP_STATE, data=sitsupp_ANOVA3)
summary(sitg3c)
#plots of LMs
plot(sitg3a)
#
plot(sitg3b)
#
plot(sitg3c)
#
plot(sitg3)
#
##### residual analysis and autocorrelation #####
# first residuals:
par(mfrow=c(1,1))
plot(residuals(sitg3a))
plot(residuals(sitg3b))
plot(residuals(sitg3c))

#autocorrelation plots
acf(residuals(sitg3a))
acf(residuals(sitg3b))
acf(residuals(sitg3c))

#load nlme for the gls() function to account for autocorrelation
sitg3b.ac <- gls(AREA_Log ~ TERRAIN, data=sitsupp_ANOVA3, 
                 correlation=corAR1(form=~1|REPORT_DOY), 
                 na.action = na.omit)
summary(sitg3b.ac)
#
sitg3c.ac <- gls(AREA_Log ~ OWNERSHIP_STATE, data=sitsupp_ANOVA3, 
                 correlation=corAR1(form=~1|REPORT_DOY), 
                 na.action = na.omit)
summary(sitg3c.ac)
#


##### quasi-poisson - SUPPRESSION_BI + variables, var v. mean #####
#Using the subset sitsupp_ANOVA2, which has selected variables and NAs removed
summary(sitsupp_ANOVA2)
str(sitsupp_ANOVA2)
# running quiasipoisson glm regression:
sitp1 <- glm(SUPPRESSION_BI ~ AREA_Log + IMT_TYPE + OWNERSHIP_STATE + TERRAIN + 
               PRIMARY_FUEL_MODEL, family = "quasipoisson", data = sitsupp_ANOVA2)
#Error message received: "negative values not allowed for the 'quasiPoisson' family"; and
#                         "In Ops.factor(y,0): '<' not meaningful for factors
##### negative binomial model #####
#Lots of errors... needs work

##### OLS #####
#confidence intervals
confint(sitsupp_ANOVA2)
#also not working, because factors?

##### glm (FS & NFS, subset of sitinc, which is grouped by incident) #####

# ...

#############################################################################################################################