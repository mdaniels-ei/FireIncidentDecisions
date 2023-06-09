###################################################################################################
#
# sitfull100 regression
#
###################################################################################################
##### libraries #####
library(MASS)
library(lme4)
library(lattice)
library(car)
library(ggplot2)
library(pbkrtest)
library(RLRsim)
library(pbnm)
library(gridExtra)
require(GGally)
require(reshape2)
require(compiler)
require(parallel)
require(boot)
require(ordinal)
require(leaps)
require(standardize)
#

# after running sitfull_prep2...

sitfull100$IMTm <- as.factor(sitfull100$IMTm)
sitfull100$IMTm <- ordered(sitfull100$IMTm, 
                           levels = c(1,2,3,4))

sitfull100$UnitType <- as.factor(sitfull100$UnitType)
sitfull100$UnitType <- ordered(sitfull100$UnitType,
                               levels = c("County & Local", "State", "Federal", "Interagency"))

sitfull100$TERRAIN <- as.factor(sitfull100$TERRAIN)
sitfull100$TERRAIN <- ordered(sitfull100$TERRAIN, 
                              levels = c("Low", "Medium", "High", "Extreme"))

sitfull100$SUPPRESSION_METHOD <- as.factor(sitfull100$SUPPRESSION_METHOD)
sitfull$SUPPRESSION_METHOD <- ordered(sitfull$SUPPRESSION_METHOD, 
                                      levels = c("FS", "CC", "PZ", "MM"))
sitfull100$SUPPNUM <- as.numeric(sitfull100$SUPPRESSION_METHOD)

##### IMTm (grouping IMT ranks A & B, E & F) #####

sitfull100 %>%
  mutate(IMTm = ifelse(IMT_RANK == "A" | IMT_RANK == "B", 1, 
                       ifelse(IMT_RANK == "C", 2, 
                              ifelse(IMT_RANK == "D", 3,
                                     ifelse(IMT_RANK == "E" | IMT_RANK == "F", 4, 0
                                            ))))) -> sitfull100
sitfull100$IMTm <- as.factor(sitfull100$IMTm)

##### [polrsupp] OLR suppression ~ area, imt, npl, unit, state, terrain #####

### Build ordered logistic regression model, with IMTm
options(contrasts = c("contr.treatment", "contr.poly"))
polrsupp <- polr(SUPPRESSION_METHOD ~ AREA_Log + IMTm + npl + UnitType + 
                   OWNERSHIP_STATE + TERRAIN, data=sitfull100)
summary(polrsupp)
polrsupp

###

##### ANOVA of polrsupp #####

# ANOVA of OLR, polrsupp
Anova(polrsupp)

##### Odds ratios #####

# finding odds ratios
summary(polrsupp)$coefficients + 
  qnorm(c(0.025,0.5,0.975)) * summary(polrsupp)$coefficients

cbind("Odds ratio" = coef(polrsupp), confint.default(polrsupp, level = 0.95))

###


##### OLR, variables (except state) as numeric #####

sitfull100$SUPPRESSION_METHOD <- as.factor(sitfull100$SUPPRESSION_METHOD)
sitfull100$SUPPRESSION_METHOD <- ordered(sitfull100$SUPPRESSION_METHOD, 
                                      levels = c("FS", "CC", "PZ", "MM"))
sitfull100$SUPPNUM <- as.numeric(sitfull100$SUPPRESSION_METHOD)

sitfull100$FUELn <- as.numeric(sitfull100$PRIMARY_FUEL_MODEL)

summary(sitfull100)

###

##### [polrsuppn] OLR suppression ~ area, imt, npl, unit, state, terrain #####

# Use:  AREA_Log, npl, IMTn, UNITn, STATEn, TERRAINn, FUELn

### Build ordered logistic regression model, with numeric
options(contrasts = c("contr.treatment", "contr.poly"))

polrsuppn <- polr(SUPPRESSION_METHOD ~ AREA_Log + IMTn + npl + UNITn + 
                   STATEn + TERRAINn + FUELn, data=sitfull100)
summary(polrsuppn)

# ANOVA of OLR, polrsuppn
Anova(polrsuppn)
###


##### polrsuppn2, OLR w/ suppression, numeric variables, state non-numeric #####
sitfull100$OWNERSHIP_STATE <- as.factor(sitfull100$OWNERSHIP_STATE)
polrsuppn2 <- polr(SUPPRESSION_METHOD ~ AREA_Log + IMTn + npl + UNITn + 
                    OWNERSHIP_STATE + TERRAINn + FUELn, data=sitfull100)
summary(polrsuppn2)
# Call:
# polr(formula = SUPPRESSION_METHOD ~ AREA_Log + IMTn + npl + UNITn + 
#        OWNERSHIP_STATE + TERRAINn + FUELn, data = sitfull100)

# Coefficients:
#                     Value  Std. Error   t value
#AREA_Log          -0.19781     0.01544  -12.8100
#IMTn               0.38787     0.04096    9.4707
#npl               -0.08937     0.03454   -2.5875
#UNITn             -0.93713     0.09456   -9.9100
#OWNERSHIP_STATEMT -0.05755     0.07287   -0.7897
#OWNERSHIP_STATEWA -1.63686     0.49723   -3.2919
#OWNERSHIP_STATEWY  0.37520     0.08813    4.2573
#TERRAINn          -0.25126     0.03424   -7.3375
#FUELn              0.19776     0.01668   11.8537

# Intercepts:
#          Value    Std. Error t value 
# FS|CC  -1.0869   0.3136    -3.4661
# CC|PZ   0.0559   0.3135     0.1782
# PZ|MM   2.5512   0.3220     7.9228

# Residual Deviance: 8344.181 
# AIC: 8368.181 
# (791 observations deleted due to missingness)

## store table 
(psn2table <- coef(summary(polrsuppn2)))
## calculate and store p values
pso2 <- pnorm(abs(psn2table[, "t value"]), lower.tail = FALSE)

## combined table
(so1table <- cbind(psn2table, "p value" = pso2))
#
(cipso1 <- confint(polrsuppn2)) # default method gives profiled CIs
confint.default(pso2) # CIs assuming normality
#


# ANOVA of OLR, polrsuppn
Anova(polrsuppn2)
# Analysis of Deviance Table (Type II tests)

#     Response: SUPPRESSION_METHOD
#                  LR Chisq Df             Pr(>Chisq)    
#  AREA_Log         169.717  1  < 0.00000000000000022 ***
#  IMTn              92.056  1  < 0.00000000000000022 ***
#  npl                6.706  1               0.009609 ** 
#  UNITn            136.456  1  < 0.00000000000000022 ***
#  OWNERSHIP_STATE   42.192  3     0.0000000036521612 ***
#  TERRAINn          54.785  1     0.0000000000001344 ***
#  FUELn            158.099  1  < 0.00000000000000022 ***

###
predn <- select(sitfull100, c(AREA_Log, IMTn, npl, UNITn, TERRAINn, FUELn))
colnames(predn)
predn$INCIDENT_ID_KS <- as.numeric(predn$INCIDENT_ID_KS)
cpr <- cor(predn, use = "complete.obs")
cpcpr <- corrplot(cpr, method="number")


##### standardized coefficients #####

stand.polrsuppn <- standardize(polrsuppn, data=sitfull100, scale = 1, family = "quasibinomial")
summary(stand.polrsuppn)
stand.polrsuppn$formula
head(stand.polrsuppn$data, 25)

###

##### model fit #####

require(broom)
#
polrsuppn3 <- polr(SUPPRESSION_METHOD ~ AREA_Log + IMTn + npl + UNITn + 
                     OWNERSHIP_STATE + TERRAINn, data=sitfull100)
summary(polrsuppn3)

rsupp_sum <- bind_rows(glance(polrsuppn), glance(polrsuppn2),
                       glance(polrsuppn3), glance(polrsupp)) %>% round(., 3)
rsupp_sum$names <- c("intercept", "... + OWNERSHIP_STATE", "... - FUELn", "non-numeric")
rsupp_sum <- rsupp_sum %>%
  select(names, logLik, AIC, BIC, deviance, df.residual)
rsupp_sum

# polrsuppn2 is the winner

###

##### Odds ratios #####

# finding odds ratios
summary(polrsuppn)$coefficients + 
  qnorm(c(0.025,0.5,0.975)) * summary(polrsuppn)$coefficients

cbind("Odds ratio" = coef(polrsuppn), confint.default(polrsuppn, level = 0.95))

#           Odds ratio      2.5 %      97.5 %
# AREA_Log -0.19925566 -0.2295061 -0.16900521
# IMTn      0.39767725  0.3174482  0.47790634
# npl      -0.08540122 -0.1526426 -0.01815984
# UNITn    -0.97883537 -1.1636671 -0.79400368
# STATEn    0.11317669  0.0558220  0.17053137
# TERRAINn -0.23597555 -0.3022534 -0.16969772
# FUELn     0.20600052  0.1737216  0.23827943

###

##### stan_polr, but same as polrsupp2 #####
require(rstanarm)
sitfull100$OWNERSHIP_STATE <- as.character(sitfull100$OWNERSHIP_STATE)

spolrsuppn <- stan_polr(SUPPRESSION_METHOD ~ AREA_Log + IMTn + npl + UNITn + 
            OWNERSHIP_STATE + TERRAINn + FUELn, data=sitfull100, 
            prior = R2(0.2, "mean"), prior_counts = dirichlet(1))
summary(spolrsuppn)
#
print(spolrsuppn)
#
plot(spolrsuppn)

###

##### effect plots #####
#
plot(Effect(focal.predictors = c("UNITn", "AREA_Log"), mod = polrsuppn2, 
            rug = FALSE, latent = TRUE))
plot(Effect(focal.predictors = c("IMTn", "UNITn"), mod = polrsuppn2, 
            ylim = c(0,19)))
plot(Effect(focal.predictors = c("UNITn", "IMTn"), mod = polrsuppn2, 
            rug = FALSE, latent = TRUE))
plot(Effect(focal.predictors = c("OWNERSHIP_STATE", "TERRAINn"), mod = polrsuppn2, 
            rug = FALSE, latent = TRUE))
plot(Effect(focal.predictors = c("OWNERSHIP_STATE", "npl"), mod = polrsuppn2, 
            rug = FALSE, latent = TRUE))
plot(Effect(focal.predictors = c("IMTn", "OWNERSHIP_STATE"), mod = polrsuppn2, 
            rug = FALSE, latent = TRUE))
#
#Effect(focal.predictors = c("UnitType", "IMTm"), polrsupp)
plot(Effect(focal.predictors = c("UNITn", "IMTn"), polrsuppn2), rug = TRUE) # THIS ONE
#
#Effect(focal.predictors = c("UnitType", "AREA_Log"), polrsupp)
plot(Effect(focal.predictors = c("UNITn", "AREA_Log"), polrsuppn2), rug = TRUE) # THIS ONE
#
#Effect(focal.predictors = c("npl", "AREA_Log"), polrsupp)
plot(Effect(focal.predictors = c("npl", "AREA_Log"), polrsuppn2), rug = TRUE) # THIS ONE
#
#Effect(focal.predictors = c("OWNERSHIP_STATE", "TERRAIN"), polrsupp)
plot(Effect(focal.predictors = c("OWNERSHIP_STATE", "TERRAINn"), polrsuppn2), rug = TRUE) # THIS ONE
#
#Effect(focal.predictors = c("AREA_Log", "TERRAIN"), polrsupp)
plot(Effect(focal.predictors = c("TERRAINn", "AREA_Log"), polrsuppn2), rug = TRUE)
#
#Effect(focal.predictors = c("AREA_Log", "IMT_RANK"), polrsupp)
plot(Effect(focal.predictors = c("AREA_Log", "IMT_RANK"), polrsuppn2), rug = TRUE)
#
#Effect(focal.predictors = c("TERRAIN", "IMT_RANK"), polrsupp)
plot(Effect(focal.predictors = c("TERRAINn", "IMTn"), polrsuppn2), rug = TRUE)
#
#Effect(focal.predictors = c("AREA_Log", "npl"), polrsupp)
plot(Effect(focal.predictors = c("AREA_Log", "npl"), polrsuppn2), rug = TRUE)
#

###

submod <- glm(SUPPNUM ~ AREA_Log + UnitType + IMTm, data=sitfull100)
plot(allEffects(submod))
plot(allEffects(polrsupp), style = "stacked")

plotmeans(polrsuppn2, data=sitfull100)

plotmeans(polrsuppn2$coefficients)

plotmeans(SUPPNUM ~ TERRAINn, data=sitfull100)
plotmeans(SUPPNUM ~ FUELn, data=sitfull100)
plotmeans(SUPPNUM ~ PRIMARY_FUEL_MODEL, data=sitfull100)
plotmeans(FUELn ~ SUPPRESSION_METHOD, data=sitfull100)
plotmeans(TERRAINn ~ SUPPRESSION_METHOD, data=sitfull100)

###







###
