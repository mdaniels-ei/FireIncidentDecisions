##############################################################################################################################
# This script opens the file, "0813_IMSR_209_INCIDENTSTAB_working_052419.csv" which is a file created from another script. 
#   "0813_IMSR_209_INCIDENTSTAB_working_052419.csv" is a joined dataset that includes all variables from the original, raw 
#   data, but the rows are limited by the study boundary (determined in ArcGIS).
#
# None it really works. 5/27/19
#
##############################################################################################################################
##### packages #####
# check packages to load
## car faraway ggplot2 MASS dplyr lattice
#library("car", lib.loc="~/R/win-library/3.5")
#library("faraway", lib.loc="~/R/win-library/3.5")
library("ggplot2", lib.loc="~/R/win-library/3.5")
library("ggpubr", lib.loc="~/R/win-library/3.5")
library("gplots", lib.loc="~/R/win-library/3.5")
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
#sitqual <- read.csv("C:/Users/Molly/Desktop/sit_working/sit_0813_inb/cleandata/0813_access_working_052419_INCIDENT_ID_KS_created.csv", 
#header=TRUE, na.strings="" 
# sitqual is the name of this dataset, loaded from the working file sitqual (csv, not txt),
# this file contains data that has been cleaned in OpenRefine, and because it was opened in 
#   OpenRefine, the NA's are not blank cells, but contain the letters "".


#loading data from GitHub, home laptop:
#sitqual <- read.csv("C:/Users/mcdaniels2/Documents/GitHub/sit_working/cleandata/0813_access_working_052419_INCIDENT_ID_KS_created.csv", 
#header=TRUE, na.strings="")
# sitqual is the name of this dataset, loaded from the working file sitqual (csv, not txt),
# this file contains data that has been cleaned in OpenRefine, and because it was opened in 
# OpenRefine, the NA's are not blank cells, but contain the letters "".

#loading data from GitHub, Lab laptop:
sitqual <- read.csv("/Users/Amber/Documents/GitHub/sit_working/cleandata/0813_access_working_052419_INCIDENT_ID_KS_created.csv", 
                    header=TRUE, na.strings="")
# sitqual is the name of this dataset, loaded from the working file sitqual (csv, not txt),
# this file contains data that has been cleaned in OpenRefine, and because it was opened in 
# OpenRefine, the NA's are not blank cells, but contain the letters "".

#loading data from GitHub, Russell Computer:
#sitqual <- read.csv("C:/Users/mcdaniels2/Documents/GitHub/sit_working/cleandata/0813_access_working_052419_INCIDENT_ID_KS_created.csv", 
#header=TRUE, na.strings="")
# sitqual is the name of this dataset, loaded from the working file sitqual (csv, not txt),
# this file contains data that has been cleaned in OpenRefine, and because it was opened in 
# OpenRefine, the NA's are not blank cells, but contain the letters "".

###### exploring sitqual #####

View(sitqual)

##### creating variable in sitqual for suppression changes #####
# incidents with decreasing suppression methods
sitqual %>%
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
                                                                                                             0))))))))))))) -> sitqual
#

# for fires with mixed suppression, not a decrease:
sitqual %>%
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
                                                                                                                                                                                                                                                           0)))))))))))))))))))))))))))))))))) -> sitqual
# it worked :)

###

##### sitqualsuppchange dataframe #####

sitqual_suppdec <- subset(sitqual, sitqual$SUPP_DECREASE == "1", select = INCIDENT_ID_KS:SUPP_MIX)
#View(sitqual_suppdec)
nrow(sitqual_suppdec)
sitqual_suppmix <- subset(sitqual, sitqual$SUPP_MIX == "2", select = INCIDENT_ID_KS:SUPP_MIX)
nrow(sitqual_suppmix)
#
sitsuppchange.q <- bind_rows(sitqual_suppdec, sitqual_suppmix)
#summary(sitsuppchange.q)
#View(sitsuppchange.q)
nrow(sitsuppchange.q)
nrow(sitsuppchange)
# it's not right - not enough rows
###

##### selecting columns from sitqual to merge with sitfull #####
colnames(sitqual)
sitqual %>%
  select(c(1,4,5,2,25,31,32,37,38,39,40,46,47,48,53,54,55,56,59,61,65,66,67,68,73,74,75,76,83,84,85,86,
           87,88,89)) -> sitquals
View(sitquals)
# success!

# WITHOUT INCIDENT_ID_KS AND INCIDENT_NUMBER
sitqual %>%
  select(c(5,25,31,32,37,38,39,40,46,47,48,53,54,55,56,59,61,65,66,67,68,73,74,75,76,83,84,85,86,
           87,88,89)) -> sitquals2
View(sitquals2)
# success; 10,048 rows, 32 columns

###

##### left_join, sitfull and sitquals #####
# FYI: sitfull has 6,246 rows, 88 columns

lj_sqs <- left_join(sitfull, sitquals, by = "INCIDENT_NAME")
View(lj_sqs)

# 134k rows :( 

lj_sqs2 <- left_join(sitfull, sitquals)
#View(lj_sqs2)
# all sitquals are empty :(

lj_sqs3 <- left_join(sitfull, sitquals2)
View(lj_sqs3)
# 94k rows??? 

##### inner_join, sitfull and sitquals #####

ij_sqs <- inner_join(sitfull, sitquals2, by = "INCIDENT_NAME")
View(ij_sqs)
# nope; 134k rows

##### semi_join, sitqual and sitfull100 #####
# to keep only incidents from sitqual that are in sitfull100, for easier merging

sitqual1 <- semi_join(sitqual, sitfull, by = "INCIDENT_ID_KS")
View(sitqual1)
# why are there 6,150 incidents, instead of 4,815?

##### sitsuppchange and sitsuppchange.q #####
#colnames(sitsuppchange.q)
sitsuppchange.q %>%
  select(c(5,25,31,32,37,38,39,40,47,48,53,54,55,56,59,61,65,66,67,68,73,74,75,76,83,84,85,86,
           87,88,89, 93,94)) -> sitsuppchange.qs
colnames(sitsuppchange.q)
colnames(sitsuppchange)
#
#View(sitsuppchange.qs)

###

lj_sitsuppchange <- left_join(sitsuppchange, sitquals2, by = "INCIDENT_NAME")
View(lj_sitsuppchange)

###

lj_sitsuppchange <- left_join(sitsuppchange, sitsuppchange.qs, by = "INCIDENT_NAME")
View(lj_sitsuppchange)
# 34k rows :( 
###



##############################################################################################################################

### below = old

##### trying left join, sitqual and sitfull #####

lj_sit <- left_join(sitfull, sitqual, by = "INCIDENT_ID_KS")
View(lj_sit)
# maybe worked? 24k rows; I don't think so


