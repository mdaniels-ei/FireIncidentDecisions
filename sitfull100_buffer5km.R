################################################################################################
#
# opening & editing sitfull100_b5km, a file created in ArcMap from sitfull100 that is a 5km 
# buffer around points. Points are daily data so this script summarizes by incident and exports 
# it to go back into Arcmap.
#
################################################################################################

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

sitfull100_b5km <- read.csv("C:/Users/mcdaniels2/Documents/GitHub/sit_working/workingdata/sitfull100_b5km.csv", 
                    header=TRUE, na.strings="NA")
View(sitfull100_b5km)
colnames(sitfull100_b5km)

sitfull100_b5km %>%
  group_by(INCIDENT_I) %>%
  summarise(lat = first(LATITUDE), long = first(LONGITUDE), max_area = max(MAX_AREA_I), 
            fs_class = first(FSCLASS_IN), state = first(OWNERSHIP_), 
            year = first(REPORT_YEA), startdoy = first(START_DOY),
            first_supp = first(SUPPRESSIO), last_supp = last(SUPPRESSIO), 
            first_unit = first(UnitType), last_unit = last(UnitType), 
            first_imt = first(IMT_TYPE_D), last_imt = last(IMT_TYPE_D), 
            all_imt = paste(IMT_TYPE, collapse = ", "), elevation = first(Elevation),
            duration = first(DURATION)) -> sitinc100_b5km
summary(sitinc100_b5km)
#nrow(sitinc100_b5km)   rows = 375 
  
# export
write.csv(sitinc100_b5km, file = 
            "C:/Users/mcdaniels2/Documents/GitHub/sit_working/z_pics_summaries_extracted/sitinc375_b5km.csv")
            
  
  
  
  
  
  
  




