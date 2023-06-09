#############################################################################################################################
#This script attempts to identify spatial autocorrelation in the dataset sitfull.
#
#
#############################################################################################################################

##### prep: distance matrix -> inverse distance matrix ####
#Creating a inverse distance matrix
sitfull.dists <- as.matrix(dist(cbind(sitfull$LATITUDE, sitfull$LONGITUDE)))

sitfull.dists.inv <- 1/sitfull.dists
diag(sitfull.dists.inv) <- 0

sitfull.dists.inv[1:5, 1:5]

###

##### calculating moran's I ####

Moran.I(sitfull$AREA, sitfull.dists.inv, na.rm = TRUE)



###

#####
#


###