##############################################################################################################################
#This script builds upon the "sitfull_prep2" script, adding box plots for sitfull100 data. Box plots include those grouped by 
#   duration, log of area, complex flag (binary), suppression method, & IMT type.
##############################################################################################################################

##### packages #####
# check packages to load


##### boxplot settings #####

# Setting scientific penalty to 10,000 
# (this will not display scientific abbreviations until >10,000)
options(scipen=10000)

##### boxplot: duration #####

bp.dur.size1 <- ggplot(sitfull100, aes(sitfull100$DURATION, sitfull100$LOG_AREA, colour=sitfull100$STATUS)) + 
  geom_jitter() + labs(title="Duration v. Log of Area, sitfull100") + 
  xlab("Duration of Incident") + ylab("Log of Area of Incident")
bp.dur.size1 + theme_light()

bp.dur.size2 <- ggplot(sitfull100, aes(sitfull100$DURATION, sitfull100$MAX_AREA_INC, colour=sitfull100$MAX_AREA_INC)) + 
  geom_smooth() + labs(title="Duration v. Max Area, sitfull100") + 
  xlab("Duration of Incident") + ylab("Max Area of Incident")
bp.dur.size2 + theme_light()
# cool

bp.dur.imt1 <- ggplot(sitfull100, aes(sitfull100$IMT_TYPE_DESCRIPTION, sitfull100$DURATION, fill=sitfull100$IMT_TYPE_DESCRIPTION)) + 
  geom_boxplot() + labs(title="Duration v. IMT Type, sitfull100") + 
  xlab("IMT Type") + ylab("Duration of Incident")
bp.dur.imt1 + theme_light()

hist(sitfull100$DURATION)
hist(log1p(sitfull100$DURATION))


