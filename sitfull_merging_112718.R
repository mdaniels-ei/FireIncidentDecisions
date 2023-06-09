##############################################################################################################################
#This script joins sit_working_or and 0813_209_working_or, data cleaned in openrefine and data with all of the variables and
# study boundary indicated.
##############################################################################################################################

# setting work directory
saveDir <- getwd() # get the current working directory
saveDir # show me the saved directory

swd <- "C:/Users/Molly/Desktop/sit_working/sit_0813_inb" 
# path to my project
setwd(swd) # set this path as my work directory 
# directory is set.

# load the data
sitfull <- read.csv("C:/Users/Molly/Desktop/sit_working/sit_0813_inb/workingdata/sit_working_or.csv", 
                    header=TRUE, na.strings="") 
#sit_working_or is the name of this dataset, loaded from the working file sitfull (csv, not txt),
#this file contains data that has been cleaned in OpenRefine.
View(sitfull)
# remove incidents outside of the study boundary from the data frame
sitfull2 <- subset(sitfull, sitfull$Bound_Ind==1)
View(sitfull2)

###

# load the data
sitor <- read.csv("C:/Users/Molly/Desktop/sit_working/sit_0813_inb/workingdata/0813_209_working_or.csv", header=TRUE, na.strings="") 
#swor is the name of this dataset, loaded from the working file 0813_209_working_or,
#this file contains data that has been cleaned in OpenRefine.

# view the data
View(sitor)

###

sitfull3 <- semi_join(sitor, sitfull2, by="INCIDENT_ID_KS", all=TRUE)
View(sitfull3)

write.csv(sitfull3, file = "C:/Users/Molly/Desktop/sit_working/sit_0813_inb/workingdata/sitfull1.csv")
