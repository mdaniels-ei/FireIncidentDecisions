##########################################################################################################################
#
# This script opens files pulled from ArcGIS, "sitfull100_5km_pophu2010.csv" and "sitfull100_5km_homevalues.csv".
#  The file was imported to ArcGIS as a national-scale shapefile, then scaled to the region, spatially joined to the 
#  sitfull100 data points then exported from ArcGIS as a csv. 
#
##########################################################################################################################

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
#library(GSODR)
#library(grid)
#library(gridExtra)
#library(raster)
#library(readr)
#library(rgeos)
#library(sp)

##### load the census data files #####

# population
sitpop <- read.csv("/Users/Amber/Documents/GitHub/sit_working_clone/sit_working2/workingdata/sitfull100_5km_pophu2010.csv", 
                   header=TRUE, na.strings="NA")
#View(sitpop)

# home values
sithomes <- read.csv("/Users/Amber/Documents/GitHub/sit_working_clone/sit_working2/workingdata/sitfull100_5km_homevalues.csv", 
                     header=TRUE, na.strings="NA")
#View(sithomes)  

sitpop2 <- dplyr::select(sitpop, c(7, 11:18))
sithomes2 <- dplyr::select(sithomes, c(7:106))
sithomes2a <- dplyr::select(sithomes2, c(1:9,13:16, 18, 20:100))
#View(sithomes2a)

### 

##### summarizing by incident, checking unique values in columns per incident #####

# for population - need to deal with the two incidents with more than one unique row

sitpop2 %>%
  group_by(INCIDENT_I) %>%
  summarise(statefp = first(STATEFP10), st_count = n_distinct(STATEFP10), countyfp = first(COUNTYFP10), 
            ct_count = n_distinct(COUNTYFP10), tractce = first(TRACTCE10), tr_count = n_distinct(TRACTCE10), 
            blockce = first(BLOCKCE), bl_count = n_distinct(BLOCKCE), blockid = first(BLOCKID10), 
            blid_count = n_distinct(BLOCKID10), housing = first(HOUSING10), ho_count = n_distinct(HOUSING10),
            pop = first(POP10), pop_count = n_distinct(POP10)) -> sitincpop
# View(sitincpop)
#summary(sitincpop)
# multi <- filter(sitincpop, blid_count > 1)
# View(multi) # OK 7 incidents with more than one row of unique information:

# 2008_ID-MCS-00042014_CHURCH CANYON
# 2010_MT-FNF-000007_CARDINAL CREEK
# 2011_ID-SCF-011175_SADDLE COMPLEX
# 2011_MT-BRF-005402_FORTY ONE COMPLEX
# 2011_MT-BRF-5399_HELLS HALF COMPLEX
# 2011_WY-YNP-000010_HEART COMPLEX
# 2012_ID-CTS-000396_STEEP CORNER

# for home values - need to deal with the four incidents with more than one unique row

sithomes2a %>%
  group_by(INCIDENT_I) %>%
  summarise_all(funs(mean)) -> sitinchomes
# View(sitinchomes)
# summary(sitinchomes)
# multi <- filter(sitinchomes, HD01_VD03 > 1)
# View(multi) # 4 incidents:

# 2010_MT-FNF-000007_CARDINAL CREEK
# 2011_MT-BRF-005402_FORTY ONE COMPLEX
# 2011_WY-YNP-000010_HEART COMPLEX	
# 2012_ID-CTS-000396_STEEP CORNER
# ^^^ those are four of the seven listed above. I'll remove the seven incidents from both summary dataframes,
#    and add them back in later with their appropriate information.

sitincpop <- filter(sitincpop, !(INCIDENT_I == "2008_ID-MCS-00042014_CHURCH CANYON" | 
                                   INCIDENT_I == "2010_MT-FNF-000007_CARDINAL CREEK" | 
                                   INCIDENT_I == "2011_ID-SCF-011175_SADDLE COMPLEX" | 
                                   INCIDENT_I == "2011_MT-BRF-005402_FORTY ONE COMPLEX" | 
                                   INCIDENT_I == "2011_MT-BRF-5399_HELLS HALF COMPLEX" | 
                                   INCIDENT_I == "2011_WY-YNP-000010_HEART COMPLEX" | 
                                   INCIDENT_I == "2012_ID-CTS-000396_STEEP CORNER"))
#
sithomes3 <- filter(sithomes2a, !(INCIDENT_I == "2008_ID-MCS-00042014_CHURCH CANYON" | 
                                        INCIDENT_I == "2010_MT-FNF-000007_CARDINAL CREEK" | 
                                        INCIDENT_I == "2011_ID-SCF-011175_SADDLE COMPLEX" | 
                                        INCIDENT_I == "2011_MT-BRF-005402_FORTY ONE COMPLEX" | 
                                        INCIDENT_I == "2011_MT-BRF-5399_HELLS HALF COMPLEX" | 
                                        INCIDENT_I == "2011_WY-YNP-000010_HEART COMPLEX" | 
                                        INCIDENT_I == "2012_ID-CTS-000396_STEEP CORNER"))
#

#summary(sithomes3)


###

##### creating useful census info within the dataframes #####

sitpop3 <- select(sitincpop, c(1, 4, 6, 8, 12, 14))
#View(sitpop3)
#

summary(sitinchomes)
colnames(sitinchomes)

sithomes3 %>%
  group_by(INCIDENT_I) %>%
  summarise_all(funs(mean)) -> sitinchomes

# finding the columns with the minimum and median home values
#sitinchomes %>%
#  group_by(INCIDENT_I) %>%
#  mutate(max_val = (colnames(sitinchomes)[apply(sitinchomes,1,which.max)])) -> sitinchomes 
# the problem with this is the sum function - it's still summarizing the full column


# I can do it in excel: export sitinchomes & create a new column, median_home_value, based on 
# weighted medians of each home value range, averaged
  
###

# computed median home values for each census tract occurring in the sitinc dataframe (375 incidents)
# importing "medhomevalue_pertractce" csv:

medhomeval_tractce <- read.csv("/Users/Amber/Documents/GitHub/sit_working_clone/sit_working2/workingdata/medhomevalues+pertractce.csv", 
                   header=TRUE, na.strings="")
#View(medhomeval_tractce)
medhomeval_tractce <- dplyr::select(medhomeval_tractce, -c(5:7))

# importing "sitincmedhomevalues" csv:

sitincmedhomeval <- read.csv("/Users/Amber/Documents/GitHub/sit_working_clone/sit_working2/workingdata/sitincmedhomevalues.csv", 
                               header=TRUE, na.strings="")
#View(sitincmedhomeval)

# joining two imported dataframes:

sitincmhv <- dplyr::left_join(sitincmedhomeval, medhomeval_tractce, by = c("TRACTCE" = "TRACTCE_NODUP"))
#View(sitincmhv)

# success! now, to export and fix the incidents which had the same census tract ID but in different states:
write.csv(sitincmhv, 
          "/Users/Amber/Documents/GitHub/sit_working_clone/sit_working2/z_pics_summaries_extracted/sitincmhv.csv")
# confirmed correct median home values for each incident. importing "sitincmhv_final.csv":

sitincmhv <- read.csv("/Users/Amber/Documents/GitHub/sit_working_clone/sit_working2/workingdata/sitincmhv_final.csv", 
                             header=TRUE, na.strings="")
#View(sitincmhv)
# numbers in the median_home_value column refer to the following:
# Estimate; Total:	Estimate; Total: - Less than $10,000	Estimate; Total: - $10,000 to $14,999	Estimate; Total: - $15,000 to $19,999	Estimate; Total: - $20,000 to $24,999	Estimate; Total: - $25,000 to $29,999	Estimate; Total: - $30,000 to $34,999	Estimate; Total: - $35,000 to $39,999	Estimate; Total: - $40,000 to $49,999	Estimate; Total: - $50,000 to $59,999	Estimate; Total: - $60,000 to $69,999	Estimate; Total: - $70,000 to $79,999	Estimate; Total: - $80,000 to $89,999	Estimate; Total: - $90,000 to $99,999	Estimate; Total: - $100,000 to $124,999	Estimate; Total: - $125,000 to $149,999	Estimate; Total: - $150,000 to $174,999	Estimate; Total: - $175,000 to $199,999	Estimate; Total: - $200,000 to $249,999	Estimate; Total: - $250,000 to $299,999	Estimate; Total: - $300,000 to $399,999	Estimate; Total: - $400,000 to $499,999	Estimate; Total: - $500,000 to $749,999	Estimate; Total: - $750,000 to $999,999	Estimate; Total: - $1,000,000 to $1,499,999	Estimate; Total: - $1,500,000 to $1,999,999	Estimate; Total: - $2,000,000 or more
#HD01_VD01	HD01_VD02	HD01_VD03	HD01_VD04	HD01_VD05	HD01_VD06	HD01_VD07	HD01_VD08	HD01_VD09	HD01_VD10	HD01_VD11	HD01_VD12	HD01_VD13	HD01_VD14	HD01_VD15	HD01_VD16	HD01_VD17	HD01_VD18	HD01_VD19	HD01_VD20	HD01_VD21	HD01_VD22	HD01_VD23	HD01_VD24	HD01_VD25	HD01_VD26	HD01_VD27
#1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18	19	20	21	22	23	24	25	26	27

# ^ all of that reads more easily when copied and pasted into a spreadsheet

###

sitinchomesub <- dplyr::select(sitinchomes, c(1,15:68))

sitinchomesub %>%
  dplyr::mutate(max_val = 
                  colnames(sitinchomesub)[max.col(sitinchomesub,ties.method="first")], 
                na.rm = TRUE) -> sitinchomesub

colnames(sitinchomesub)
# 

#sithomes3 <- select()

#View(sithomes3)
###

##### merging them into sitcensus #####


# 
sitcensus <- left_join(sitpop3, sithomes3, by = "INCIDENT_I")
#View(sitcensus)
summary(sitcensus)

###

##### wui #####

# load wui dataframe that was created in ArcGIS (spatially joining sitfull100_b5km & WUI_merge)
wui <- read.csv("/Users/Amber/Documents/GitHub/sit_working_clone/sit_working2/spatial_data/sitfull100_5km_wui_19902010.csv", 
                header=TRUE, na.strings="NA")
#
#View(wui)
# Summarize by incident, averaging numeric variables
summary(wui)
colnames(wui)
#
wui2 <- filter(wui, wui$WATER10 == 0)
wui_sum <- select(wui, c(8,10:14, 16, 17, 20, 22, 33:48))
wui_sum_noveg <- select(wui_sum, 1:6, 9:26)
#

wui_sum %>%
  group_by(INCIDENT_I) %>%
  summarise_all(funs(n_distinct)) -> wui_ndistinct
#
#View(wui_ndistinct)
#

wui_sum %>%
  group_by(INCIDENT_I) %>%
  summarise_all(funs(mean)) -> wui_avg
#View(wui_avg)

#

wui_sum %>%
  filter(!(is.na(wui_sum))) %>%
  group_by(INCIDENT_I) %>%
  summarise_all(funs(mean)) -> wui_avg2
#
#View(wui_avg2)

#
wui_sum_noveg %>%
  group_by(INCIDENT_I) %>%
  summarise_all(funs(mean)) -> wui_avg_noveg
#View(wui_avg_noveg)
#

###

# dividing by wui column sections, then summarizing
colnames(wui_sum)
wui_veg <- select(wui_sum, 1:3, 7,8)
#

wui_veg %>%
  filter(!(is.na(VEG01PC))) %>%
  group_by(INCIDENT_I) %>%
  summarise_all(funs(mean)) -> wui_veg_avg
#View(wui_veg_avg)
# looks to be the same numbers as the VEG01PC and VEG11PC in wui_avg

wui_pop <- select(wui_sum, 1:3, 14, 22, 18, 26)

#
wui_pop %>%
  filter(!is.na(POPDEN2000)) %>%
  group_by(INCIDENT_I) %>%
  summarise_all(funs(mean)) -> wui_pop_avg
#View(wui_pop_avg)
#
# again, looks to be the same numbers as the POP2000 AND POPDEN2010 in wui_avg

###

##### exporting #####

# export sitinchomes:
#write.csv(sitinchomes, 
#          "/Users/Amber/Documents/GitHub/sit_working_clone/sit_working2/z_pics_summaries_extracted/sitinchomes.csv")

# export sitpop3:
#write.csv(sitpop3, 
#          "/Users/Amber/Documents/GitHub/sit_working_clone/sit_working2/z_pics_summaries_extracted/sitpop3.csv")

# export wui_avg:
#write.csv(wui_avg, 
#          "/Users/Amber/Documents/GitHub/sit_working_clone/sit_working2/z_pics_summaries_extracted/sit_5km_wui_avg.csv")

# export sitincmhv:
#write.csv(sitincmhv,
#          "/Users/Amber/Documents/GitHub/sit_working_clone/sit_working2/z_pics_summaries_extracted/sitmhv.csv")




