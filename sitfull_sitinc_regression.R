


#sitinc100 & sitfull100
#summary(sitfull100)
#summary(sitinc100)
#summary(sitjoin100)
#

##### plots of sitinc's variables #####

#first imt reported
bp_sitinc_first_imt <- ggplot(sitinc, aes(x=sitinc$first_imt, y=sitinc$INCIDENT_ID_KS, colour = sitinc$first_imt)) +
  geom_jitter() + labs(title="jitter plot of first reported imt for incidents 08-13",  
                        x="IMT types", y="Incidents") #subtitle="",
bp_sitinc_first_imt + theme_classic()

#last imt reported
bp_sitinc_last_imt <- ggplot(sitinc, aes(x=sitinc$last_imt, y=sitinc$INCIDENT_ID_KS, colour = sitinc$last_imt)) +
  geom_jitter() + labs(title="jitter plot of last reported imt for incidents 08-13",  
                       x="IMT types", y="Incidents") #subtitle="",
bp_sitinc_last_imt + theme_classic()


#first imt reported based on first report year reported (not start_year)
bp_sitinc_first_imt_yr <- ggplot(sitinc, aes(x=sitinc$first_imt, y=sitinc$first_year, colour = sitinc$last_year)) +
  geom_jitter() + labs(title="jitter plot of first report imt for each year 08-13",  
                       x="IMT types", y="years") #subtitle="",
bp_sitinc_first_imt_yr + theme_classic()

#last imt reported based on last report year reported (not start_year)
bp_sitinc_last_imt_yr <- ggplot(sitinc, aes(x=sitinc$last_imt, y=sitinc$last_year, colour = sitinc$first_year)) +
  geom_jitter() + labs(title="jitter plot of last report imt for each year 08-13",  
                       x="IMT types", y="years") #subtitle="",
bp_sitinc_last_imt_yr + theme_classic()


##### regression of sitfull100 #####

# Log Reg: fire size & suppression, FS v. NFS ("SUPPRESSION_BI")

glm.fit.f <- glm(sitfull100$SUPPRESSION_BI ~ sitfull100$AREA_Log, family = binomial, na.action = na.omit)
summary(glm.fit.f)
step(glm.fit.f, test="LRT")
str(sitfull100)

g1sitfull100 <- glm(SUPPRESSION_BI ~ AREA_Log + IMT_TYPE + OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL, 
             family=binomial, data=sitfull100)
summary(g1sitfull100)
step(g1sitfull100, test="LRT")

g2sitfull100 <- glm(SUPPRESSION_BI ~ AREA_Log + OWNERSHIP_STATE + TERRAIN + GROWTH_POTENTIAL, 
                    family = binomial, data = sitfull100)
summary(g2sitfull100)

##### regression of sitinc100 #####
g1sitinc100 <- glm(first_suppbi ~ max_area + first_imt + last_imt + first_unit + last_unit + state, 
                    family=binomial, data=sitinc100)
summary(g1sitinc100)
step(g1sitinc100, test="LRT")

g2sitinc100 <- glm(last_suppbi ~ max_area + last_imt + last_unit + state, 
                   family = binomial, data = sitinc100)
summary(g2sitinc100)

##### regression of sitjoin100 #####
g1sitjoin100 <- glm(first_suppbi ~ max_area + mode_imt + state, 
                   family=binomial, data=sitjoin100)
summary(g1sitjoin100)
step(g1sitjoin100, test="LRT")
