###################################################################################################
#
# sitfull100 regression
#
###################################################################################################
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
#

# after running sitfull_prep2...

sitfull100$SUPPRESSION_METHOD <- as.factor(sitfull100$SUPPRESSION_METHOD)
sitfull$SUPPRESSION_METHOD <- ordered(sitfull$SUPPRESSION_METHOD, levels = c("FS", "CC", "PZ", "MM"))
sitfull100$SUPPNUM <- as.numeric(sitfull100$SUPPRESSION_METHOD)

###

##### subset of sitfull100 for final regression, 5/30/19 #####
summary(sitfull100)
ss1 <- select(sitfull100, INC_ID, REPORT_DOY, DURATION, LATITUDE, LONGITUDE, OWNERSHIP_STATE, LOG_AREA, 
              SUPPRESSION_METHOD, SUPPNUM, SUPPRESSION_BI, IMT_TYPE_DESCRIPTION, IMT_RANK, npl, 
              Department, UnitType, TERRAIN, PRIMARY_FUEL_MODEL)
ss1 <- filter(ss1, PRIMARY_FUEL_MODEL == 2 | PRIMARY_FUEL_MODEL == 8 | PRIMARY_FUEL_MODEL == 10, 
              UnitType == "County & Local" | UnitType == "State" | UnitType == "Federal")
# ss1: n = 4,461
#
ss1$Department <- as.factor(ss1$Department)
ss1$UnitType <- as.factor(ss1$UnitType)
ss1$IMT_RANK <- as.factor(ss1$IMT_RANK)
ss1$TERRAIN <- as.factor(ss1$TERRAIN)
ss1$PRIMARY_FUEL_MODEL <- as.factor(ss1$PRIMARY_FUEL_MODEL)
ss1 %>%
  mutate(IMTm = ifelse(IMT_RANK == "A" | IMT_RANK == "B", 1, 
                       ifelse(IMT_RANK == "C", 2, 
                              ifelse(IMT_RANK == "D", 3,
                                     ifelse(IMT_RANK == "E" | IMT_RANK == "F", 4, 0))))) -> ss1
ss1$IMTm <- as.factor(ss1$IMTm)
summary(ss1)
#
ss2 <- na.omit(ss1)
summary(ss2)
nrow(ss2) # n = 3,749
###


##### Ordinal Logistic Regression with SUPPRESSION_METHOD, ss1 #####

# Ordinal Logistic Regression

## fit ordered logit model 
so1 <- polr(SUPPRESSION_METHOD ~  LOG_AREA + DURATION + IMT_RANK + npl + UnitType + Department + OWNERSHIP_STATE + 
              TERRAIN, 
            data = ss1, Hess=TRUE)
## view a summary of the model
summary(so1) # Doesn't have  p-values in output, so storing the table and finding p-values to then rejoin with the table
## store table 
(so1table <- coef(summary(so1)))
## calculate and store p values
pso1 <- pnorm(abs(so1table[, "t value"]), lower.tail = FALSE)

## combined table
(so1table <- cbind(so1table, "p value" = pso1))
#
(ciso1 <- confint(so1)) # default method gives profiled CIs
confint.default(so1) # CIs assuming normality
#

# Looking at the confidence intervals as odds ratios:
## odds ratios
exp(coef(so1))

## OR and CI
exp(cbind(OR = coef(so1), confint(so1)))
#
###

##### Ordinal Logistic Regression with SUPPRESSION_METHOD, ss2 #####

# Ordinal Logistic Regression

## fit ordered logit model 
so2 <- polr(SUPPRESSION_METHOD ~  LOG_AREA + DURATION + IMTm + npl + UnitType + Department + OWNERSHIP_STATE + 
              TERRAIN, 
            data = ss2, Hess=TRUE)
## view a summary of the model
summary(so2) # Doesn't have  p-values in output, so storing the table and finding p-values to then rejoin with the table
## store table 
(so2table <- coef(summary(so2)))
## calculate and store p values
pso2 <- pnorm(abs(so2table[, "t value"]), lower.tail = FALSE)

## combined table
(so2table <- cbind(so2table, "p value" = pso2))
#
(ciso2 <- confint(so2)) # default method gives profiled CIs
confint.default(so2) # CIs assuming normality
#
# Looking at the confidence intervals as odds ratios:
## odds ratios
exp(coef(so2))

## OR and CI
exp(cbind(OR = coef(so2), confint(so2)))
#
###

logLik(so2)
# 'log Lik.' -3837.331 (df=21)


# looking at deviance of each variable
deviance(so2) - deviance(update(so2, . ~ . + LOG_AREA:DURATION))
# [1] 94.864
deviance(so2) - deviance(update(so2, . ~ . + LOG_AREA:IMTm))
# [1] 50.71719
deviance(so2) - deviance(update(so2, . ~ . + LOG_AREA:npl))
# [1] 32.74108
deviance(so2) - deviance(update(so2, . ~ . + LOG_AREA:UnitType))
# ERROR !!!!!!!!!!!!!!
deviance(so2) - deviance(update(so2, . ~ . + LOG_AREA:Department))
# ERROR !!!!!!!!!!!!!!
deviance(so2) - deviance(update(so2, . ~ . + LOG_AREA:OWNERSHIP_STATE))
# [1] 8.177811
deviance(so2) - deviance(update(so2, . ~ . + LOG_AREA:TERRAIN))
# [1] 25.42996
deviance(so2) - deviance(update(so2, . ~ . + DURATION:IMTm))
# [1] 86.08489
deviance(so2) - deviance(update(so2, . ~ . + DURATION:npl))
# [1] 27.98341
deviance(so2) - deviance(update(so2, . ~ . + DURATION:UnitType))
# [1] 40.72305
deviance(so2) - deviance(update(so2, . ~ . + DURATION:Department))
# [1] 106.3817
deviance(so2) - deviance(update(so2, . ~ . + DURATION:OWNERSHIP_STATE))
# [1] 22.13368
deviance(so2) - deviance(update(so2, . ~ . + DURATION:TERRAIN))
# [1] 20.44671
deviance(so2) - deviance(update(so2, . ~ . + IMTm:npl))
# [1] 10.76948
deviance(so2) - deviance(update(so2, . ~ . + IMTm:UnitType))
# [1] 25.08012
deviance(so2) - deviance(update(so2, . ~ . + IMTm:Department))
# ERROR !!!!!!!!
deviance(so2) - deviance(update(so2, . ~ . + IMTm:OWNERSHIP_STATE))
# [1] 31.09393
deviance(so2) - deviance(update(so2, . ~ . + IMTm:TERRAIN))
# [1] 88.45812
deviance(so2) - deviance(update(so2, . ~ . + npl:UnitType))
# [1] 20.65929
deviance(so2) - deviance(update(so2, . ~ . + npl:Department))
# [1] 24.56171
deviance(so2) - deviance(update(so2, . ~ . + npl:OWNERSHIP_STATE))
# [1] 29.40725
deviance(so2) - deviance(update(so2, . ~ . + npl:TERRAIN))
# [1] 1.903476
deviance(so2) - deviance(update(so2, . ~ . + UnitType:Department))
# ERROR !!!!!!!!!!! 
deviance(so2) - deviance(update(so2, . ~ . + UnitType:OWNERSHIP_STATE))
# ERROR !!!!!!!!!!! 
deviance(so2) - deviance(update(so2, . ~ . + UnitType:TERRAIN))
# ERROR !!!!!!!!!!! 
deviance(so2) - deviance(update(so2, . ~ . + Department:OWNERSHIP_STATE))
# [1] 48.17635
deviance(so2) - deviance(update(so2, . ~ . + Department:TERRAIN))
# [1] 44.61586
deviance(so2) - deviance(update(so2, . ~ . + TERRAIN:OWNERSHIP_STATE))
# [1] 111.9282
###

so2int <- update(so2, .~. + TERRAIN:OWNERSHIP_STATE + LOG_AREA:DURATION + IMTm:TERRAIN)
summary(so2int)

deviance(so2) - deviance(so2int)

# comparing odds
c <- coef(so2int)
# npl
exp(c["npl"])
# 0.9544688 < so the odds of moving from FS to less decrease as npl increases

# TERRAIN # Extreme -> High
exp(c["TERRAINHigh"])
# 0.07985246
# TERRAIN Extreme -> Low
exp(c["TERRAINLow"])
# 0.03964496 
# TERRAIN # Extreme -> Medium
exp(c["TERRAINMedium"])
# 0.6875439

# IMTm1 -> 2
exp(c["IMTm2"])
# 0.2262009 
# IMTm1 -> 3
exp(c["IMTm3"])
# 0.4924785 
# IMTm1 -> 4
exp(c["IMTm4"])
# 0.4902936 

# LOG_AREA
exp(c["LOG_AREA"])
# 0.4686973

# UnitType County & Local -> Federal
exp(c["UnitTypeFederal"])
# 27673338 <<<<<<<<<<<<<<<<<<<<<<<<<<<< hmm.
# UnitType County & Local -> State
exp(c["UnitTypeState"])
# 0.9809802 

# Department USDI -> MT
exp(c["DepartmentMT"])
# 2556341 <<<<<<<<<<<<<<<<<<<<<<<<<<<< hmm.
# Department USDI -> USDA
exp(c["DepartmentUSDA"])
# 0.3520733
# Department USDI -> WA
exp(c["DepartmentWA"])
# 18.87706  <<<<<<<<<<<<<<<<<<<<<<<<<<<< hmm.

# OWNERSHIP_STATE ID -> MT
exp(c["OWNERSHIP_STATEMT"])
# 0.8710621
# OWNERSHIP_STATE ID -> WA
exp(c["OWNERSHIP_STATEWA"])
# 1.028164 <<<<<<<<<<<<<<<<<<<<<<<<<<<< hmm.
# OWNERSHIP_STATE ID -> WY
exp(c["OWNERSHIP_STATEWY"])
# 10.92009 <<<<<<<<<<<<<<<<<<<<<<<<<<<< hmm. 
#
summary(ss2)
exp(coef(so2))

###

##### [same as above, without UnitType] Ordinal Logistic Regression with SUPPRESSION_METHOD, ss2 #####

# Ordinal Logistic Regression

## fit ordered logit model 
so2 <- polr(SUPPRESSION_METHOD ~  LOG_AREA + DURATION + IMTm + npl + Department + OWNERSHIP_STATE + 
              TERRAIN, 
            data = ss2, Hess=TRUE)
## view a summary of the model
summary(so2) # Doesn't have  p-values in output, so storing the table and finding p-values to then rejoin with the table
## store table 
(so2table <- coef(summary(so2)))
## calculate and store p values
pso2 <- pnorm(abs(so2table[, "t value"]), lower.tail = FALSE)

## combined table
(so2table <- cbind(so2table, "p value" = pso2))
#

# Looking at the confidence intervals as odds ratios:
## odds ratios
exp(coef(so2))

## OR and CI
exp(cbind(OR = coef(so2), confint(so2)))
#


logLik(so2)
# 'log Lik.' -3837.331 (df=20)


# looking at deviance of each variable
deviance(so2) - deviance(update(so2, . ~ . + LOG_AREA:DURATION))
# [1] 94.864
deviance(so2) - deviance(update(so2, . ~ . + LOG_AREA:IMTm))
# [1] 50.71719
deviance(so2) - deviance(update(so2, . ~ . + LOG_AREA:npl))
# [1] 32.74108
deviance(so2) - deviance(update(so2, . ~ . + LOG_AREA:Department))
# ERROR !!!!!!!!!!!!!!
deviance(so2) - deviance(update(so2, . ~ . + LOG_AREA:OWNERSHIP_STATE))
# [1] 8.177811
deviance(so2) - deviance(update(so2, . ~ . + LOG_AREA:TERRAIN))
# [1] 25.42996
deviance(so2) - deviance(update(so2, . ~ . + DURATION:IMTm))
# [1] 86.08489
deviance(so2) - deviance(update(so2, . ~ . + DURATION:npl))
# [1] 27.98341
deviance(so2) - deviance(update(so2, . ~ . + DURATION:Department))
# [1] 106.3817
deviance(so2) - deviance(update(so2, . ~ . + DURATION:OWNERSHIP_STATE))
# [1] 22.13368
deviance(so2) - deviance(update(so2, . ~ . + DURATION:TERRAIN))
# [1] 20.44671
deviance(so2) - deviance(update(so2, . ~ . + IMTm:npl))
# [1] 10.76948
deviance(so2) - deviance(update(so2, . ~ . + IMTm:Department))
# ERROR !!!!!!!!
deviance(so2) - deviance(update(so2, . ~ . + IMTm:OWNERSHIP_STATE))
# [1] 31.09393
deviance(so2) - deviance(update(so2, . ~ . + IMTm:TERRAIN))
# [1] 88.45812
deviance(so2) - deviance(update(so2, . ~ . + npl:UnitType))
# [1] 20.65929
deviance(so2) - deviance(update(so2, . ~ . + npl:Department))
# [1] 24.56171
deviance(so2) - deviance(update(so2, . ~ . + npl:OWNERSHIP_STATE))
# [1] 29.40725
deviance(so2) - deviance(update(so2, . ~ . + npl:TERRAIN))
# [1] 1.903476
deviance(so2) - deviance(update(so2, . ~ . + Department:OWNERSHIP_STATE))
# [1] 48.17635
deviance(so2) - deviance(update(so2, . ~ . + Department:TERRAIN))
# [1] 44.61586
deviance(so2) - deviance(update(so2, . ~ . + TERRAIN:OWNERSHIP_STATE))
# [1] 111.9282
###

so2int <- update(so2, .~. + TERRAIN:OWNERSHIP_STATE + LOG_AREA:DURATION + IMTm:TERRAIN)
summary(so2int)

deviance(so2) - deviance(so2int) # = 339.0377

# comparing odds
c <- coef(so2int)
# npl
exp(c["npl"])
# 0.9544651 < so the odds of moving from FS to less decrease as npl increases

# TERRAIN # Extreme -> High
exp(c["TERRAINHigh"])
# 0.07987093
# TERRAIN Extreme -> Low
exp(c["TERRAINLow"])
# 0.03963002
# TERRAIN # Extreme -> Medium
exp(c["TERRAINMedium"])
# 0.6855249

# IMTm1 -> 2
exp(c["IMTm2"])
# 0.2262453
# IMTm1 -> 3
exp(c["IMTm3"])
# 0.4925563  
# IMTm1 -> 4
exp(c["IMTm4"])
# 0.4903802 

# LOG_AREA
exp(c["LOG_AREA"])
# 0.4687039

# Department USDI -> MT
exp(c["DepartmentMT"])
# 2929570 <<<<<<<<<<<<<<<<<<<<<<<<<<<< hmm.
# Department USDI -> USDA
exp(c["DepartmentUSDA"])
# 11381189  <<<<<<<<<<<<<<<<<<<<<<<<<<<< hmm.
# Department USDI -> WA
exp(c["DepartmentWA"])
# 18.88091  <<<<<<<<<<<<<<<<<<<<<<<<<<<< hmm.

# OWNERSHIP_STATE ID -> MT
exp(c["OWNERSHIP_STATEMT"])
# 0.871019
# OWNERSHIP_STATE ID -> WA
exp(c["OWNERSHIP_STATEWA"])
# 1.028184 <<<<<<<<<<<<<<<<<<<<<<<<<<<< hmm.
# OWNERSHIP_STATE ID -> WY
exp(c["OWNERSHIP_STATEWY"])
# 10.92116 <<<<<<<<<<<<<<<<<<<<<<<<<<<< hmm. 
#
summary(ss2)
exp(coef(so2))

###

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)))
}

(s <- with(ss2, summary(as.numeric(SUPPRESSION_METHOD) ~ LOG_AREA + DURATION + IMTm + 
                          npl + Department + OWNERSHIP_STATE + TERRAIN + 
                          TERRAIN:OWNERSHIP_STATE + LOG_AREA:DURATION + IMTm:TERRAIN, fun=sf)))

glm(I(as.numeric(SUPPRESSION_METHOD) >= 2) ~ IMTm, family="binomial", data = ss2)
# Call:  glm(formula = I(as.numeric(SUPPRESSION_METHOD) >= 2) ~ IMTm, 
#           family = "binomial", data = ss2)

# Coefficients:
#  (Intercept)        IMTm2        IMTm3        IMTm4  
#      -0.4005      -0.7327       0.9223       1.3837  

# Degrees of Freedom: 3748  Total (i.e. Null);  3745 Residual
# Null Deviance:	    5161 
# Residual Deviance:  4646 	  AIC: 4654
#


###

##### ordinal logistic regression, ss1 suppression_method ~ ordered/filtered independents, INC_ID as random effect #####

sclm <- clm2(SUPPRESSION_METHOD ~ LOG_AREA + IMT_RANK + npl + UnitType + Department + TERRAIN + PRIMARY_FUEL_MODEL + 
               (1 | INC_ID), 
             data = ss1, Hess = TRUE)
summary(sclm)
step(sclm)
#
regsub.sclm <- regsubsets(SUPPRESSION_METHOD ~ LOG_AREA + IMT_RANK + npl + UnitType + Department + TERRAIN + 
                            PRIMARY_FUEL_MODEL + (1 | INC_ID),
                          data = ss1,
                          nbest = 1,       # 1 best model for each number of predictors
                          nvmax = NULL,    # NULL for no limit on number of variables
                          force.in = NULL, force.out = NULL,
                          method = "exhaustive")
sumregsub.sclm <- summary(regsub.sclm)
exhaustivesearch.sclm <- cbind(sumregsub.sclm$which, sumregsub.sclm$rsq, sumregsub.sclm$adjr2, sumregsub.sclm$bic)
View(exhaustivesearch.sclm)
exp(coef(sclm))
sclm$coefficients
#
#
#sclmm <- clmm(SUPPRESSION_METHOD ~ LOG_AREA + IMT_RANK + npl + UnitType + Department + TERRAIN + PRIMARY_FUEL_MODEL + 
#                 (1 | INC_ID), data = ss1, Hess = TRUE)
#summary(sclmm)
#
# didn't work, provided NA's for all columns except Estimate

# Dropping TERRAIN and PRIMARY_FUEL_MODEL, adding DURATION
#sclmm2 <- clmm(SUPPRESSION_METHOD ~ LOG_AREA + DURATION + IMT_RANK + npl + UnitType + Department +
#               (1 | INC_ID), data = ss1, Hess = TRUE)
#summary(sclmm2)
#
# didn't work, provided NA's for all columns except Estimate

###


##### linear regression with SUPPNUM #####
s100g4 <- lm(SUPPNUM ~ LOG_AREA + IMT_RANK + npl + UnitType + OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL +
               C_TEMPH + C_RHL + F_TEMPH + F_RHL, data=sitfull100)
summary(s100g4)

regsub.s100g4 <- regsubsets(SUPPNUM ~ LOG_AREA + IMT_RANK + npl + UnitType + OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL +
                              C_TEMPH + C_RHL + F_TEMPH + F_RHL,
                            data = sitfull100,
                            nbest = 1,       # 1 best model for each number of predictors
                            nvmax = NULL,    # NULL for no limit on number of variables
                            force.in = NULL, force.out = NULL,
                            method = "exhaustive")
sumregsub.s100g4 <- summary(regsub.s100g4)

exhaustivesearch.s100g4 <- cbind(sumregsub.s100g4$which, sumregsub.s100g4$rsq, sumregsub.s100g4$adjr2, sumregsub.s100g4$bic)

View(exhaustivesearch.s100g4)

###

##### linear regression with SUPPNUM, INC_ID as random effect #####

slmer <- lmer(SUPPNUM ~ LOG_AREA + IMT_RANK + npl + UnitType + OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL +
                (1 | INC_ID) + C_TEMPH + C_RHL + F_TEMPH + F_RHL, data=sitfull100)
summary(slmer)
print(slmer, correlation = TRUE)
Anova(slmer)
###

slmer2 <- lmer(SUPPNUM ~ LOG_AREA + IMT_RANK + npl + UnitType + OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL +
                 (1 | INC_ID), data=sitfull100)
summary(slmer2)
print(slmer2, correlation = TRUE)
Anova(slmer2)

###

##### ordinal logistic regression of SUPPRESSION_METHOD with mixed methods (INC_ID) as random effect #####

hist(sitfull100$INC_ID) # not very normally distributed
hist(sitfull100$REPORT_DOY) # normal distribution, although pretty narrow tails
qqnorm(sitfull100$INC_ID) # looks ok

###

sclm1 <- clm2(SUPPRESSION_METHOD ~ LOG_AREA + IMT_RANK + (1 | INC_ID), data = sitfull100, link = "logistic")
summary(sclm1)
#
sclm2 <- clm2(SUPPRESSION_METHOD ~ LOG_AREA + IMT_RANK + npl + UnitType + TERRAIN + (1 | INC_ID), 
              data = sitfull100, link = "logistic")
summary(sclm2)
#
sclm3 <- clm2(SUPPRESSION_METHOD ~ LOG_AREA + IMT_RANK + npl + UnitType + OWNERSHIP_STATE + TERRAIN + 
                PRIMARY_FUEL_MODEL + (1 | INC_ID),
              data = sitfull100, link = "logistic", Hess = TRUE)
summary(sclm3)
#
regsub.sclm3 <- regsubsets(SUPPRESSION_METHOD ~ LOG_AREA + IMT_RANK + npl + UnitType + OWNERSHIP_STATE + TERRAIN + 
                             PRIMARY_FUEL_MODEL + (1 | INC_ID),
                           data = sitfull100,
                           nbest = 1,       # 1 best model for each number of predictors
                           nvmax = NULL,    # NULL for no limit on number of variables
                           force.in = NULL, force.out = NULL,
                           method = "exhaustive")
sumregsub.sclm3 <- summary(regsub.sclm3)

exhaustivesearch.sclm3 <- cbind(sumregsub.sclm3$which, sumregsub.sclm3$rsq, sumregsub.sclm3$adjr2, sumregsub.sclm3$bic)

View(exhaustivesearch.sclm3)

###


##### graphing proportional odds assumption #####
# estimate values
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4'=qlogis(mean(y >= 4)))
}
# summary table of linear predicted values
(stable <- with(sitfull100, summary(as.numeric(SUPPNUM) ~ LOG_AREA + IMT_RANK + npl + UnitType + 
                                 OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL, fun=sf)))
#
#  +------------------+--------------+----+----+-----------+-----------+---------+
#  |                  |              |N   |Y>=1|Y>=2       |Y>=3       |Y>=4     |
#  +------------------+--------------+----+----+-----------+-----------+---------+
#  |LOG_AREA          |[0.00, 4.29)  |1237|Inf | 2.75469306|-0.07278879|-1.700455|
#  |                  |[4.29, 5.34)  |1180|Inf | 0.13580154|-0.72895663|-2.733646|
#  |                  |[5.34, 6.55)  |1206|Inf |-0.38615416|-1.25276297|-4.247655|
#  |                  |[6.55,11.29]  |1182|Inf |-0.20032887|-0.67417128|-5.278115|
#  |                  |Missing       |  10|Inf |        Inf| 0.84729786|     -Inf|
#  +------------------+--------------+----+----+-----------+-----------+---------+
#  |IMT_RANK          |A             |  59|Inf |-0.82320031|-0.82320031|     -Inf|
#  |                  |B             | 294|Inf |-0.38566248|-0.48550782|     -Inf|
#  |                  |C             | 894|Inf |-1.21223129|-1.53863509|-5.404927|
#  |                  |D             |2045|Inf | 0.48560656|-0.70785333|-2.587817|
#  |                  |E             |1152|Inf | 1.20811516|-0.33647224|-2.444236|
#  |                  |F             | 120|Inf | 1.67068154|-0.73088751|-1.670682|
#  |                  |Missing       | 251|Inf | 2.24689619| 0.77804662|-2.074967|
#  +------------------+--------------+----+----+-----------+-----------+---------+
#  |npl               |1             | 364|Inf | 1.45286885|-0.24294618|-2.149311|
#  |                  |2             |1349|Inf | 0.76291675|-0.81119792|-2.777324|
#  |                  |3             |1677|Inf | 0.22878039|-0.58241081|-3.243336|
#  |                  |4             |1198|Inf | 0.01669488|-0.57275658|-2.650743|
#  |                  |5             | 227|Inf |-0.80775056|-1.36985563|-2.002481|
#  +------------------+--------------+----+----+-----------+-----------+---------+
#  |UnitType          |County & Local|  43|Inf |       -Inf|       -Inf|     -Inf|
#  |                  |Federal       |4330|Inf | 0.60536257|-0.49288241|-2.648998|
#  |                  |Interagency   |   7|Inf |       -Inf|       -Inf|     -Inf|
#  |                  |State         | 430|Inf |-2.92022472|-3.55057478|     -Inf|
#  |                  |Missing       |   5|Inf |       -Inf|       -Inf|     -Inf|
#  +------------------+--------------+----+----+-----------+-----------+---------+
#  |OWNERSHIP_STATE   |ID            |2114|Inf | 0.40467678|-0.57134231|-2.157902|
#  |                  |MT            |1753|Inf | 0.09476567|-0.68630955|-4.365412|
#  |                  |WA            |  93|Inf |-2.86789890|-4.52178858|     -Inf|
#  |                  |WY            | 855|Inf | 1.04171762|-0.59981524|-2.890372|
#  +------------------+--------------+----+----+-----------+-----------+---------+
#  |TERRAIN           |Extreme       |1511|Inf | 0.17116452|-0.50694486|-2.870981|
#  |                  |High          |2137|Inf | 0.19056053|-0.71923255|-3.124321|
#  |                  |Low           |  25|Inf |-0.08004271|       -Inf|     -Inf|
#|                  |Medium        | 630|Inf | 0.18464985|-1.42646040|-4.030210|
#  |                  |Missing       | 512|Inf | 2.61216843| 0.06252036|-1.366876|
#  +------------------+--------------+----+----+-----------+-----------+---------+
#  |PRIMARY_FUEL_MODEL|1             |  49|Inf |       -Inf|       -Inf|     -Inf|
#  |                  |2             | 603|Inf |-1.16838186|-2.22141180|-3.540106|
#  |                  |3             |  48|Inf |-1.94591015|-2.39789527|-2.397895|
#  |                  |4             |   1|Inf |        Inf|        Inf|     -Inf|
#  |                  |5             |  76|Inf |-0.59598343|-0.71294981|     -Inf|
#  |                  |7             |  13|Inf |       -Inf|       -Inf|     -Inf|
#  |                  |8             | 443|Inf | 1.09560477|-0.85699099|-2.622250|
#  |                  |9             |  56|Inf | 1.19625076|-0.21511138|-3.295837|
#  |                  |10            |3415|Inf | 0.65690659|-0.35809742|-2.614227|
#  |                  |11            |   7|Inf |       -Inf|       -Inf|     -Inf|
#  |                  |12            |   5|Inf |       -Inf|       -Inf|     -Inf|
#  |                  |13            |  39|Inf |-1.35454566|-2.91777073|     -Inf|
#  |                  |Missing       |  60|Inf |-1.18958407|-4.07753744|     -Inf|
#  +------------------+--------------+----+----+-----------+-----------+---------+
#  |Overall           |              |4815|Inf | 0.34866099|-0.64769184|-2.762265|
#  +------------------+--------------+----+----+-----------+-----------+---------+
#
glm(I(as.numeric(SUPPNUM) >= 2) ~ IMT_RANK, family="binomial", data = sitfull100)
glm(I(as.numeric(SUPPNUM) >= 3) ~ IMT_RANK, family="binomial", data = sitfull100)


##### Ordinal Logistic Regression, SUPPRESSION_METHOD #####

### Build ordered logistic regression model
options(contrasts = c("contr.treatment", "contr.poly"))
polrsupp <- polr(SUPPRESSION_METHOD ~ LOG_AREA + IMTm + npl + UnitType + 
                   OWNERSHIP_STATE + TERRAIN, data=sitfull100)
summary(polrsupp)
polrsupp
Effect(focal.predictors = c("IMTm"), polrsupp)
Effect(focal.predictors = c("TERRAIN"), polrsupp)


# ANOVA of OLR, polrsupp
Anova(polrsupp)
#

(ctable <- coef(summary(polrsupp)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))


# finding odds ratios
summary(polrsupp)$coefficients + qnorm(c(0.025,0.5,0.975)) * summary(polrsupp)$coefficients

cbind("Odds ratio" = coef(polrsupp), confint.default(polrsupp, level = 0.95))


###

submod <- glm(SUPPNUM ~ LOG_AREA + UnitType + IMTm, data=sitfull100)
plot(allEffects(submod))
plot(allEffects(polrsupp), style = "stacked")

##### plotting OLR 1 #####

plot(Effect(focal.predictors = c("UnitType", "LOG_AREA"), mod = polrsupp, rug = FALSE, latent = TRUE), 
     ylim = c(0,19))
plot(Effect(focal.predictors = c("IMTm", "UnitType"), mod = polrsupp, rug = FALSE, latent = TRUE), 
     ylim = c(0,19))
plot(Effect(focal.predictors = c("UnitType", "IMTm"), mod = polrsupp, rug = FALSE, latent = TRUE), 
     ylim = c(0,19))
plot(Effect(focal.predictors = c("OWNERSHIP_STATE", "TERRAIN"), mod = polrsupp, rug = FALSE, latent = TRUE), 
     ylim = c(0,19))
plot(Effect(focal.predictors = c("OWNERSHIP_STATE", "npl"), mod = polrsupp, rug = FALSE, latent = TRUE), 
     ylim = c(0,19))
plot(Effect(focal.predictors = c("IMTm", "OWNERSHIP_STATE"), mod = polrsupp, rug = FALSE, latent = TRUE), 
     ylim = c(0,19))

#Effect(focal.predictors = c("UnitType", "IMTm"), polrsupp)
plot(Effect(focal.predictors = c("UnitType", "IMTm"), polrsupp), rug = TRUE) # THIS ONE
#
#Effect(focal.predictors = c("UnitType", "LOG_AREA"), polrsupp)
plot(Effect(focal.predictors = c("UnitType", "LOG_AREA"), polrsupp), rug = TRUE) # THIS ONE
#
#Effect(focal.predictors = c("npl", "LOG_AREA"), polrsupp)
plot(Effect(focal.predictors = c("npl", "LOG_AREA"), polrsupp), rug = TRUE) # THIS ONE
#
#Effect(focal.predictors = c("OWNERSHIP_STATE", "TERRAIN"), polrsupp)
plot(Effect(focal.predictors = c("OWNERSHIP_STATE", "TERRAIN"), polrsupp), rug = TRUE) # THIS ONE
#
#Effect(focal.predictors = c("LOG_AREA", "TERRAIN"), polrsupp)
plot(Effect(focal.predictors = c("TERRAIN", "LOG_AREA"), polrsupp), rug = TRUE)
#
#Effect(focal.predictors = c("LOG_AREA", "IMT_RANK"), polrsupp)
plot(Effect(focal.predictors = c("LOG_AREA", "IMT_RANK"), polrsupp), rug = TRUE)
#
#Effect(focal.predictors = c("TERRAIN", "IMT_RANK"), polrsupp)
plot(Effect(focal.predictors = c("TERRAIN", "IMT_RANK"), polrsupp), rug = TRUE)
#
#Effect(focal.predictors = c("LOG_AREA", "npl"), polrsupp)
plot(Effect(focal.predictors = c("LOG_AREA", "npl"), polrsupp), rug = TRUE)
#


###



sitfull100 %>%
  mutate(IMTm = ifelse(IMT_RANK == "A" | IMT_RANK == "B", 1, 
                       ifelse(IMT_RANK == "C", 2, 
                              ifelse(IMT_RANK == "D", 3,
                                     ifelse(IMT_RANK == "E" | IMT_RANK == "F", 4, 0))))) -> sitfull100
sitfull100$IMTm <- as.factor(sitfull100$IMTm)

### Build ordered logistic regression model again, with IMTm
options(contrasts = c("contr.treatment", "contr.poly"))
polrsupp <- polr(SUPPRESSION_METHOD ~ LOG_AREA + IMTm + npl + UnitType + 
                   OWNERSHIP_STATE + TERRAIN, data=sitfull100)
summary(polrsupp)
polrsupp
class(sitfull100$IMTm)

# predict on test data
### Predict
predictsupp <- predict(polrsupp, sitfull100)  # predict the supp methods directly
head(predictsupp)
predictedsupp <- predict(polrsupp, sitfull100, type="p")  # predict the probabilites
head(predictedsupp)
## Confusion matrix and misclassification error
table(sitfull100$SUPPRESSION_METHOD, predictsupp)  # confusion matrix
mean(as.character(sitfull100$SUPPRESSION_METHOD) != as.character(predictsupp))  # misclassification error
# NA
###

# ANOVA of OLR, polrsupp
Anova(polrsupp)

#
  # finding odds ratios
  summary(polrsupp)$coefficients + qnorm(c(0.025,0.5,0.975)) * summary(polrsupp)$coefficients

cbind("Odds ratio" = coef(polrsupp), confint.default(polrsupp, level = 0.95))

###

##### second OLR with lat / long #####
### Build ordered logistic regression model
options(contrasts = c("contr.treatment", "contr.poly"))
polrs2 <- polr(SUPPRESSION_METHOD ~ LOG_AREA + IMT_RANK + npl + UnitType + 
                   OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL + LATITUDE + LONGITUDE, 
               data=sitfull100)
summary(polrs2)
#

# predict on test data
### Predict
predicts2 <- predict(polrs2, sitfull100)  # predict the supp methods directly
head(predicts2)
predicteds2 <- predict(polrs2, sitfull100, type="p")  # predict the probabilites
head(predicteds2)
## Confusion matrix and misclassification error
table(sitfull100$SUPPRESSION_METHOD, predicts2)  # confusion matrix
mean(as.character(sitfull100$SUPPRESSION_METHOD) != as.character(predicts2))  # misclassification error
# NA
###

# ANOVA of OLR, polrsupp
Anova(polrs2)
# Analysis of Deviance Table (Type II tests)
# Response: SUPPRESSION_METHOD
#                     LR Chisq  Df  Pr(>Chisq)    
#  LOG_AREA              3.725   1     0.05360 .  
#  IMT_RANK            155.102   5   < 2.2e-16 ***
#  npl                   5.765   1     0.01635 *  
#  UnitType            210.620   3   < 2.2e-16 ***
#  OWNERSHIP_STATE     218.215   3   < 2.2e-16 ***
#  TERRAIN              40.732   3   7.453e-09 ***
#  PRIMARY_FUEL_MODEL  185.567  11   < 2.2e-16 ***
#  LATITUDE            277.997   1   < 2.2e-16 ***
#  LONGITUDE             2.317   1     0.12793    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
Effect(focal.predictors = c("IMT_RANK","OWNERSHIP_STATE"), polrs2)
plot(Effect(focal.predictors = c("IMT_RANK","OWNERSHIP_STATE"), polrs2), rug = TRUE)
#
# latitude & longitude as fixed effects
e.out <- Effect(focal.predictors = c("IMT_RANK","OWNERSHIP_STATE"), polrs2)
e.out$model.matrix[1,c("LATITUDE","LONGITUDE")]
# LATITUDE  LONGITUDE 
# 45.47421  113.37844
e.out <- Effect(focal.predictors = c("IMT_RANK","OWNERSHIP_STATE"), mod = polrs2, 
                given.values = c(LATITUDE = 45.47421, LONGITUDE = 113.37844))
plot(e.out, rug = FALSE)


###

##### OLR with INC_ID as fixed effect #####
### Build ordered logistic regression model
options(contrasts = c("contr.treatment", "contr.poly"))
polrs3 <- polr(SUPPRESSION_METHOD ~ LOG_AREA + IMT_RANK + npl + UnitType + 
                 OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL + (1 | INC_ID), 
               data=sitfull100, Hess = TRUE)
summary(polrs3)
#Call:
#polr(formula = SUPPRESSION_METHOD ~ LOG_AREA + IMT_RANK + npl + 
#       UnitType + OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL + 
#       (1 | INC_ID), data = sitfull100, Hess = TRUE)

#Coefficients:
#                        Value Std. Error    t value
#LOG_AREA             -0.18066  1.593e-02 -1.134e+01
#IMT_RANKB            -0.78789  3.480e-01 -2.264e+00
#IMT_RANKC            -1.46276  3.374e-01 -4.335e+00
#IMT_RANKD            -0.34870  3.312e-01 -1.053e+00
#IMT_RANKE            -0.17703  3.355e-01 -5.277e-01
#IMT_RANKF            -0.15344  3.849e-01 -3.986e-01
#npl                  -0.09368  3.510e-02 -2.669e+00
#UnitTypeFederal      15.93082  1.625e-01  9.804e+01
#UnitTypeInteragency  -1.99646  4.864e-08 -4.105e+07
#UnitTypeState        13.76708  1.987e-01  6.927e+01
#OWNERSHIP_STATEMT     0.01951  7.565e-02  2.579e-01
#OWNERSHIP_STATEWA    -1.58906  5.092e-01 -3.121e+00
#OWNERSHIP_STATEWY     0.50337  9.441e-02  5.332e+00
#TERRAINHigh          -0.37379  7.461e-02 -5.010e+00
#TERRAINLow           -0.77971  4.281e-01 -1.822e+00
#TERRAINMedium        -0.77802  1.079e-01 -7.214e+00
#PRIMARY_FUEL_MODEL2  15.39582  2.549e-01  6.041e+01
#PRIMARY_FUEL_MODEL3  15.35693  7.205e-01  2.131e+01
#PRIMARY_FUEL_MODEL4  18.29108  1.514e+00  1.208e+01
#PRIMARY_FUEL_MODEL5  16.92586  3.337e-01  5.072e+01
#PRIMARY_FUEL_MODEL7  -1.15045  3.280e-07 -3.508e+06
#PRIMARY_FUEL_MODEL8  16.29208  2.461e-01  6.620e+01
#PRIMARY_FUEL_MODEL9  16.45535  3.434e-01  4.792e+01
#PRIMARY_FUEL_MODEL10 16.55440  2.324e-01  7.124e+01
#PRIMARY_FUEL_MODEL11  1.81084  1.131e-07  1.601e+07
#PRIMARY_FUEL_MODEL12  0.10283  6.731e-08  1.528e+06
#PRIMARY_FUEL_MODEL13 15.17121  4.362e-01  3.478e+01

#Intercepts:
#           Value       Std. Error      t value      
#FS|CC  3.010990e+01   2.765000e-01   1.089012e+02
#CC|PZ  3.129890e+01   2.763000e-01   1.132846e+02
#PZ|MM  3.381650e+01   2.840000e-01   1.190873e+02

#Residual Deviance: 8110.835 
#AIC: 8170.835 
#(791 observations deleted due to missingness)

# seemingly, this provides the regression results of an ordinal logistic regression
# with the incident id (INC_ID) as a random effect.

# predict on test data
### Predict
predicts3 <- predict(polrs3, sitfull100)  # predict the supp methods directly
head(predicts2)
predicteds3 <- predict(polrs3, sitfull100, type="p")  # predict the probabilites
head(predicteds3)
## Confusion matrix and misclassification error
table(sitfull100$SUPPRESSION_METHOD, predicts3)  # confusion matrix
mean(as.character(sitfull100$SUPPRESSION_METHOD) != as.character(predicts3))  # misclassification error
# NA
###

# ANOVA of OLR, polrsupp
Anova(polrs3)
#Analysis of Deviance Table (Type II tests)
#Response: SUPPRESSION_METHOD
#                     LR Chisq  Df   Pr(>Chisq)    
#  LOG_AREA            132.208   1   < 2.2e-16 ***
#  IMT_RANK            165.553   5   < 2.2e-16 ***
#  npl                   7.136   1    0.007556 ** 
#  UnitType            164.095   3   < 2.2e-16 ***
#  OWNERSHIP_STATE      47.604   3   2.586e-10 ***
#  TERRAIN              56.168   3   3.869e-12 ***
#  PRIMARY_FUEL_MODEL  165.666  11   < 2.2e-16 ***
#  1 | INC_ID            0.000   1    1.000000    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#

Effect(c("IMT_RANK","OWNERSHIP_STATE"), polrs3)
plot(Effect(c("IMT_RANK","OWNERSHIP_STATE"), polrs3), rug = TRUE, residuals = TRUE)

###

##### same as polrs3, removing INC_ID as random effect #####
polrs3a <- polr(formula = SUPPRESSION_METHOD ~ LOG_AREA + IMT_RANK + npl + 
                  UnitType + OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL, 
                data = sitfull100, Hess = TRUE)
summary(polrs3a)

###

##### OLR with DOY as random effect #####
options(contrasts = c("contr.treatment", "contr.poly"))
polrs4 <- polr(SUPPRESSION_METHOD ~ LOG_AREA + IMT_RANK + npl + UnitType + 
                 OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL + (1 | REPORT_DOY), 
               data=sitfull100, Hess = TRUE)
summary(polrs4)
# ANOVA of OLR, polrs4
Anova(polrs4)

summary(sitfull100$SUPPNUM)
#
testlm <- lm(SUPPNUM ~ LOG_AREA, data=sitfull100)
testlmr <- lmer(SUPPNUM ~ LOG_AREA + (1|INC_ID), data=sitfull100)
summary(testlm)
summary(testlmr)
#
polrs4 <- lmer(SUPPNUM ~ LOG_AREA + IMT_RANK + npl + UnitType + 
                 OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL + (1 | REPORT_DOY), 
               data=sitfull100)
summary(polrs4)
###
#
polrs5 <- lmer(SUPPNUM ~ LOG_AREA + IMT_RANK + npl + UnitType + 
                 OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL + (1 | INC_ID), 
               data=sitfull100)
summary(polrs5)
plot(polrs4)
plot(polrs5)
plot(Effect(c("IMT_RANK","OWNERSHIP_STATE"), polrs5), rug = TRUE, residuals = TRUE)
plot(Effect(c("IMT_RANK","LOG_AREA"), polrs5), rug = TRUE, residuals = TRUE)
plot(Effect(c("IMT_RANK","UnitType"), polrs5), rug = TRUE, residuals = TRUE)
plot(Effect(c("IMT_RANK","npl"), polrs5), rug = TRUE, residuals = TRUE)
plot(Effect(c("TERRAIN","UnitType"), polrs5), rug = TRUE, residuals = TRUE)
plot(Effect(c("TERRAIN","OWNERSHIP_STATE"), polrs5), rug = TRUE, residuals = TRUE)
plot(Effect(c("TERRAIN","npl"), polrs5), rug = TRUE, residuals = TRUE)
plot(Effect(c("TERRAIN","IMT_RANK"), polrs5), rug = TRUE, residuals = TRUE)
plot(Effect(c("TERRAIN","LOG_AREA"), polrs5), rug = TRUE, residuals = TRUE)

###
#
polrs6 <- lm(SUPPNUM ~ LOG_AREA + IMT_RANK + npl + UnitType + 
                 OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL, 
               data=sitfull100)
summary(polrs6)
plot(polrs6)

###


##### binomial logistic regression with random effects #####
# this is going about it as if we are predicting the suppression method used, using the independent variables

sitfull100$SUPPRESSION_BIn <- ifelse(sitfull100$SUPPRESSION_BI == "FS", 0,1)
summary(sitfull100$SUPPRESSION_BIn)
# variables to include:
 # LOG_AREA + IMT_RANK + npl + UnitType + COUNTY + Department + OWNERSHIP_STATE + 
 # TERRAIN + PRIMARY_FUEL_MODEL
# visualizing independence of predictors
ggpairs(sitfull100[, c("LOG_AREA", "IMT_RANK", "npl", "UnitType", "Department", "OWNERSHIP_STATE", "TERRAIN", "PRIMARY_FUEL_MODEL")])
#

# running mixed effects logistic regression with INCIDENT_ID_KS as random effect
# estimate the model and store results in sm1
sm1 <- glmer(SUPPRESSION_BIn ~ LOG_AREA + IMT_RANK + npl + UnitType + COUNTY + Department + OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL +
             (1 | INCIDENT_ID_KS), data = sitfull100, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)
# error message: fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients
# print the mod results without correlations among fixed effects
print(sm1, corr = FALSE)
# The estimates represent the regression coefficients, which are unstandardized and are on the logit scale. The estimates are followed by 
# their standard errors (SEs). As is common in GLMs, the SEs are obtained by inverting the observed information matrix (negative second 
# derivative matrix). However, for GLMMs, this is an approximation. The approximations of the coefficient estimates likely stabilize 
# faster than do those for the SEs. Thus, if using fewer integration points, the estimates may be reasonable, but the approximation of the
# SEs may be less accurate. The Wald tests, (frac{Estimate}{SE}), rely on asymptotic theory, here referring to as the highest level unit
# size converges to infinity, these tests will be normally distributed, and from that, p values (the probability of obtaining the 
# observed estimate or more extreme, given the true estimate is 0).
#
# Generalized linear mixed model fit by maximum likelihood (Adaptive Gauss-Hermite Quadrature, nAGQ = 10) ['glmerMod']
#Family: binomial  ( logit )
#Formula: SUPPRESSION_BIn ~ LOG_AREA + IMT_RANK + npl + UnitType + COUNTY +      Department + OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL +      (1 | INCIDENT_ID_KS)
#Data: sitfull100
#AIC      BIC   logLik deviance df.resid 
#976.958 1745.319 -366.479  732.958     3894 
#Random effects:
#  Groups         Name        Std.Dev.
#INCIDENT_ID_KS (Intercept) 11.71   
#Number of obs: 4016, groups:  INCIDENT_ID_KS, 294
#Fixed Effects:
#  (Intercept)                    LOG_AREA                   IMT_RANKB                   IMT_RANKC                   IMT_RANKD                   IMT_RANKE                   IMT_RANKF  
#-40.33189                    -2.03222                    -5.61175                    -3.76636                    -1.96163                     0.14458                     0.61745  
#npl             UnitTypeFederal               UnitTypeState             COUNTYBear Lake            COUNTYBeaverhead            COUNTYBEAVERHEAD              COUNTYBig Horn  
#0.27144                    26.20233                    -3.90233                    24.26510                    19.32734                    33.11470                     9.16866  
#COUNTYBighorn                COUNTYBlaine                COUNTYBLAINE                 COUNTYBoise                 COUNTYBOISE            COUNTYBonneville              COUNTYBoundary  
#24.61776                   -31.04263                   -26.24356                    -2.83462                   -10.79380                    -6.45163                    -3.68093  
#COUNTYBroadwater            COUNTYBROADWATER                 COUNTYButte                 COUNTYCamas                COUNTYCarbon               COUNTYCascade            COUNTYClearwater  
#-21.05959                   -23.95340                   -29.39990                   -28.03955                   -29.45987                   -11.28810                     0.87036  
#COUNTYCrook                COUNTYCuster                COUNTYElmore                COUNTYELMORE                 COUNTYFerry                 COUNTYFERRY        COUNTYFerry/Okanogan  
#1.46869                    -3.43903                    -7.28667                   -29.33711                   -26.54304                    18.99315                   -19.31131  
#COUNTYFlathead               COUNTYFremont               COUNTYFREMONT               COUNTYGalitan              COUNTYGallatin              COUNTYGALLATIN                COUNTYGranit  
#-5.32315                   -11.75696                   -13.08495                   -29.19011                   -24.12485                    -3.95555                    15.54226  
#COUNTYGranite               COUNTYGRANITE                 COUNTYidaho                 COUNTYIdaho                 COUNTYIDAHO      COUNTYIdaho and Valley             COUNTYJefferson  
#0.03067                    46.48990                    19.90892                     4.08852                    -3.50073                    14.93040                   -20.09146  
#COUNTYJEFFERSON      COUNTYJefferson County               COUNTYJOHNSON          COUNTYJudith Basin                  COUNTYLake                  COUNTYLAKE       COUNTYLake & Missoula  
#-26.71854                   -27.77335                    14.52631                   -30.32565                     4.20866                    -1.20280                   -29.10453  
#COUNTYLatah                 COUNTYLemhi                 COUNTYLewis        COUNTYLewis  & Clark         COUNTYLewis & Clark         COUNTYLEWIS & CLARK       COUNTYLewis and Clark  
#-0.62070                    -5.57033                    11.23554                    16.22680                    -8.86123                   -23.27910                     3.82489  
#COUNTYLincoln    COUNTYLincoln and Sublet  COUNTYLincoln and Sublette              COUNTYMaddison               COUNTYMadison               COUNTYMADISON               COUNTYMeagher  
#-8.75794                   -25.67085                   -29.53352                   -29.70925                   -30.84756                   -27.30862                    16.73730  
#COUNTYMineral              COUNTYMissoula              COUNTYMISSOULA             COUNTYNez Perce             COUNTYNEZ PERCE              COUNTYOkanogan              COUNTYOKANOGAN  
#-6.75132                   -14.97528                    23.59205                    11.19147                    12.66450                     0.27811                    11.75902  
#COUNTYOkanogan/Ferry                  COUNTYPark                  COUNTYPARK                COUNTYPowell                COUNTYPOWELL  COUNTYPowell/Flathead/Miss  COUNTYPowell/Lewis & Clark  
#-21.16487                     4.25805                   -17.44790                     1.75333                    19.89433                    14.95163                    16.10464  
#COUNTYRavalli               COUNTYSanders               COUNTYSANDERS             COUNTYShoeshone              COUNTYshoshone              COUNTYShoshone               COUNTYSpokane  
#3.12953                    -7.03657                   -24.89375                    17.41254                    19.45304                    -5.10981                    -1.09507  
#COUNTYStevens         COUNTYStevens/Ferry              COUNTYSublette           COUNTYSweet Grass                 COUNTYTeton                 COUNTYTETON                COUNTYValley  
#1.67499                    -1.23340                    -8.08011                   -28.97710                    -0.96803                    13.74454                     4.82234  
#COUNTYVALLEY            COUNTYValley, ID                COUNTYWeston                DepartmentMT              DepartmentUSDA                DepartmentWA                DepartmentWY  
#-5.78924                    -1.30706                    -3.19523                    19.86626                     7.09546                     1.22945                    -5.46189  
#OWNERSHIP_STATEMT           OWNERSHIP_STATEWY                 TERRAINHigh                  TERRAINLow               TERRAINMedium         PRIMARY_FUEL_MODEL2         PRIMARY_FUEL_MODEL3  
#2.24615                    11.88249                    -1.45536                    -2.69378                    -3.35190                    24.43898                    14.61552  
#PRIMARY_FUEL_MODEL4         PRIMARY_FUEL_MODEL5         PRIMARY_FUEL_MODEL7         PRIMARY_FUEL_MODEL8         PRIMARY_FUEL_MODEL9        PRIMARY_FUEL_MODEL10        PRIMARY_FUEL_MODEL11  
#38.47294                    31.64521                    -3.46750                    26.71893                    18.90633                    24.36849                    25.42401  
#PRIMARY_FUEL_MODEL12        PRIMARY_FUEL_MODEL13  
#3.86002                    -3.80130  
#fit warnings:
##  fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients
#convergence code 1; 2 optimizer warnings; 1 lme4 warnings 
#We can get rough estimates of CIs using the SEs.
se <- sqrt(diag(vcov(sm1)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(sm1), LL = fixef(sm1) - 1.96 * se, UL = fixef(sm1) + 1.96 *
                se))

###

# again, without county
# estimate the model and store results in sm2
sm2 <- glmer(SUPPRESSION_BIn ~ LOG_AREA + IMT_RANK + npl + UnitType + OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL +
               (1 | INCIDENT_ID_KS), data = sitfull100, family = binomial, nAGQ = 10)
# print the mod results without correlations among fixed effects
summary(sm2)
#Generalized linear mixed model fit by maximum likelihood (Adaptive Gauss-Hermite Quadrature, nAGQ = 10) ['glmerMod']
#Family: binomial  ( logit )
#Formula: SUPPRESSION_BIn ~ LOG_AREA + IMT_RANK + npl + UnitType + OWNERSHIP_STATE +      TERRAIN + PRIMARY_FUEL_MODEL + (1 | INCIDENT_ID_KS)
#Data: sitfull100

#AIC      BIC   logLik deviance df.resid 
#910.9   1093.6   -426.5    852.9     3995 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-9.3633 -0.0181  0.0002  0.0278  2.4501 

#Random effects:
#  Groups         Name        Variance Std.Dev.
#INCIDENT_ID_KS (Intercept) 127.3    11.28   
#Number of obs: 4024, groups:  INCIDENT_ID_KS, 296

#Fixed effects:
#                       Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          -4.225e+01  9.216e+05   0.000 0.999963    
#LOG_AREA             -2.519e+00  6.569e-01  -3.835 0.000126 *** Area
#IMT_RANKB            -2.550e+00  1.444e+01  -0.177 0.859837    
#IMT_RANKC            -8.174e-01  1.444e+01  -0.057 0.954845    
#IMT_RANKD             1.020e+00  1.443e+01   0.071 0.943689    
#IMT_RANKE             2.914e+00  1.444e+01   0.202 0.840059    
#IMT_RANKF             2.118e+00  1.486e+01   0.143 0.886611    
#npl                   1.941e-01  1.724e-01   1.126 0.260048    
#UnitTypeFederal       2.684e+01  6.545e+04   0.000 0.999673    
#UnitTypeInteragency  -3.721e+00  2.245e+05   0.000 0.999987    
#UnitTypeState         5.815e+00  6.545e+04   0.000 0.999929    
#OWNERSHIP_STATEMT     8.750e-01  2.100e+00   0.417 0.676956    
#OWNERSHIP_STATEWA    -1.067e+01  7.829e+00  -1.363 0.172743    
#OWNERSHIP_STATEWY     8.247e+00  3.297e+00   2.501 0.012383 *  State
#TERRAINHigh          -1.453e+00  7.419e-01  -1.959 0.050106 .  
#TERRAINLow           -4.150e+00  7.321e+00  -0.567 0.570819    
#TERRAINMedium        -3.148e+00  1.158e+00  -2.718 0.006559 **  Terrain
#PRIMARY_FUEL_MODEL2   2.766e+01  9.191e+05   0.000 0.999976    
#PRIMARY_FUEL_MODEL3   3.111e+01  9.191e+05   0.000 0.999973    
#PRIMARY_FUEL_MODEL4   2.587e+01  9.191e+05   0.000 0.999978    
#PRIMARY_FUEL_MODEL5   3.282e+01  9.191e+05   0.000 0.999972    
#PRIMARY_FUEL_MODEL7  -2.624e+00  9.626e+05   0.000 0.999998    
#PRIMARY_FUEL_MODEL8   3.098e+01  9.191e+05   0.000 0.999973    
#PRIMARY_FUEL_MODEL9   2.647e+01  9.191e+05   0.000 0.999977    
#PRIMARY_FUEL_MODEL10  2.965e+01  9.191e+05   0.000 0.999974    
#PRIMARY_FUEL_MODEL11  2.230e+01  9.885e+05   0.000 0.999982    
#PRIMARY_FUEL_MODEL12 -8.556e+00  3.357e+07   0.000 1.000000    
#PRIMARY_FUEL_MODEL13  2.372e+01  9.191e+05   0.000 0.999979    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
print(sm2, corr = FALSE)
#Generalized linear mixed model fit by maximum likelihood (Adaptive Gauss-Hermite Quadrature, nAGQ = 10) ['glmerMod']
#Family: binomial  ( logit )
#Formula: SUPPRESSION_BIn ~ LOG_AREA + IMT_RANK + npl + UnitType + OWNERSHIP_STATE +      TERRAIN + PRIMARY_FUEL_MODEL + (1 | INCIDENT_ID_KS)
#   Data: sitfull100
#    AIC       BIC    logLik   deviance  df.resid 
# 910.910  1093.611  -426.455   852.910      3995 
# Random effects:
#  Groups          Name         Std.Dev.
#  INCIDENT_ID_KS  (Intercept)  11.28   
# Number of obs: 4024, groups:  INCIDENT_ID_KS, 296
# Fixed Effects:
#         (Intercept)               LOG_AREA             IMT_RANKB             IMT_RANKC             IMT_RANKD             IMT_RANKE  
#            -42.2456                -2.5192               -2.5503               -0.8174                1.0196                2.9142  
#            IMT_RANKF                   npl       UnitTypeFederal   UnitTypeInteragency         UnitTypeState     OWNERSHIP_STATEMT
#               2.1183                0.1941               26.8416               -3.7208                5.8154                0.8750
#    OWNERSHIP_STATEWA     OWNERSHIP_STATEWY           TERRAINHigh            TERRAINLow         TERRAINMedium   PRIMARY_FUEL_MODEL2
#             -10.6737                8.2470               -1.4534               -4.1496               -3.1482               27.6588  
#  PRIMARY_FUEL_MODEL3   PRIMARY_FUEL_MODEL4   PRIMARY_FUEL_MODEL5   PRIMARY_FUEL_MODEL7   PRIMARY_FUEL_MODEL8   PRIMARY_FUEL_MODEL9
#              31.1063               25.8714               32.8204               -2.6237               30.9757               26.4665
# PRIMARY_FUEL_MODEL10  PRIMARY_FUEL_MODEL11  PRIMARY_FUEL_MODEL12  PRIMARY_FUEL_MODEL13  
#              29.6485               22.2985               -8.5565               23.7203  
#convergence code 0; 2 optimizer warnings; 1 lme4 warnings
#
# We can get rough estimates of CIs using the SEs.
se2 <- sqrt(diag(vcov(sm2)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(sm2), LL = fixef(sm2) - 1.96 * se, UL = fixef(sm2) + 1.96 *
                se2))
plot(sm2, main = "binomial logistic regression with Incident ID as random effect")

###
colnames(sitfull100)
# again, with log of area, imt rank, npl, unit type, ownership state, terrain
# estimate the model and store results in sm3
sm3 <- glmer(SUPPRESSION_BIn ~ LOG_AREA + IMT_RANK + IMT_TYPE + npl + UnitType + OWNERSHIP_STATE + TERRAIN +
               (1 | INCIDENT_ID_KS), data = sitfull100, family = binomial, nAGQ = 10)
#fixed-effect model matrix is rank deficient so dropping 5 columns / coefficients
#Error in length(value <- as.numeric(value)) == 1L : 
#  Downdated VtV is not positive definite
# summarize the results
#summary(sm3)
# print results without correlations
#print(sm2, corr = FALSE)


###

##### binomial regression with glm #####
# using SUPPRESSION_BI as dependent variable, running regression:

s100g1 <- glm(SUPPRESSION_BI ~ LOG_AREA + IMT_RANK + npl + UnitType + OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL +
                C_TEMPL + C_TEMPH + C_RHL + C_RHH + F_TEMPL + F_TEMPH + F_RHL + F_RHH, 
              family=binomial, data=sitfull100)
summary(s100g1)

regsub.s100g1 <- regsubsets(SUPPRESSION_BI ~ LOG_AREA + IMT_RANK + npl + UnitType + OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL +
                              C_TEMPL + C_TEMPH + C_RHL + C_RHH + F_TEMPL + F_TEMPH + F_RHL + F_RHH,
                            data = sitfull100,
                            nbest = 1,       # 1 best model for each number of predictors
                            nvmax = NULL,    # NULL for no limit on number of variables
                            force.in = NULL, force.out = NULL,
                            method = "exhaustive")
sumregsub.s100g1 <- summary(regsub.s100g1)

exhaustivesearch.s100g1 <- cbind(sumregsub.s100g1$which, sumregsub.s100g1$rsq, sumregsub.s100g1$adjr2, sumregsub.s100g1$bic)

View(exhaustivesearch.s100g1)
#
# is.numeric(sitfull100$SUPPRESSION_BI) # FALSE
sitfull100$SUPPRESSION_BIn <- ifelse(sitfull100$SUPPRESSION_BI == "FS", 0,1)
summary(sitfull100$SUPPRESSION_BIn)
# log regression (without landscape/weather variables, + TERRAIN + PRIMARY_FUEL_MODEL + C_TEMPH + C_RHL + F_TEMPH + F_RHL)
s100g2 <- glm(SUPPRESSION_BIn ~ LOG_AREA + IMT_RANK + npl + UnitType + OWNERSHIP_STATE, data=sitfull100, family="binomial")
summary(s100g2)
# Call:
# glm(formula = SUPPRESSION_BIn ~ LOG_AREA + IMT_RANK + npl + UnitType + 
#      OWNERSHIP_STATE, family = "binomial", data = sitfull100)

# Deviance Residuals: 
#  Min         1Q     Median       3Q       Max  
#-2.1226   -0.8430    0.4842    0.7911    2.4396  

# Coefficients:
#                       Estimate   Std. Error  z value  Pr(>|z|)    
#  (Intercept)         -13.530806  205.933304   -0.066  0.947613    
#  LOG_AREA             -0.199451    0.016220  -12.297   < 2e-16 ***
#  IMT_RANKB            -0.539676    0.383577   -1.407  0.159440    
#  IMT_RANKC            -1.329120    0.374077   -3.553  0.000381 ***
#  IMT_RANKD            -0.002663    0.368377   -0.007  0.994232    
#  IMT_RANKE             0.394105    0.374394    1.053  0.292503    
#  IMT_RANKF             0.443810    0.462978    0.959  0.337761    
#  npl                  -0.248719    0.039180   -6.348  2.18e-10 ***
#  UnitTypeFederal      15.901424  205.932959    0.077  0.938451    
#  UnitTypeInteragency   0.084810  583.257746    0.000  0.999884    
#  UnitTypeState        12.962370  205.933078    0.063  0.949811    
#  OWNERSHIP_STATEMT     0.092688    0.080226    1.155  0.247956    
#  OWNERSHIP_STATEWA    -2.270411    0.502998   -4.514  6.37e-06 ***
#  OWNERSHIP_STATEWY     0.766521    0.107557    7.127  1.03e-12 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 6220.7  on 4548  degrees of freedom
#Residual deviance: 4654.6  on 4535  degrees of freedom
#(266 observations deleted due to missingness)
#AIC: 4682.6

#Number of Fisher Scoring iterations: 14

# pull the coefficients themselves by using the coef() command.
# Store coefficients in another object
coefs <- coef(s100g2)
# Show the coefficients, just for fun
#coefs
#(Intercept)            LOG_AREA           IMT_RANKB           IMT_RANKC           IMT_RANKD           IMT_RANKE           IMT_RANKF                 npl     UnitTypeFederal UnitTypeInteragency 
#-13.530806380        -0.199450847        -0.539676022        -1.329119946        -0.002663118         0.394104887         0.443809695        -0.248719422        15.901423816         0.084809715 
#UnitTypeState   OWNERSHIP_STATEMT   OWNERSHIP_STATEWA   OWNERSHIP_STATEWY 
#12.962369614         0.092687541        -2.270411242         0.766520900 
# Raise e to the coefficients
exp(coefs)
#(Intercept)            LOG_AREA           IMT_RANKB           IMT_RANKC           IMT_RANKD           IMT_RANKE           IMT_RANKF                 npl     UnitTypeFederal UnitTypeInteragency 
#1.329369e-06        8.191805e-01        5.829371e-01        2.647101e-01        9.973404e-01        1.483056e+00        1.558634e+00        7.797987e-01        8.051942e+06        1.088510e+00 
#UnitTypeState   OWNERSHIP_STATEMT   OWNERSHIP_STATEWA   OWNERSHIP_STATEWY 
#4.260746e+05        1.097119e+00        1.032697e-01        2.152265e+00 
#take the odds ratio, subtract 1, and multiply by 100 to get the percent change in the odds for a one unit increase in the independent variable.
(exp(coefs)-1)*100


# exhaustive search (variables to include)
regsub.s100g2 <- regsubsets(SUPPRESSION_BIn ~ LOG_AREA + IMT_RANK + npl + UnitType + OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL +
                              C_TEMPH + C_RHL + F_TEMPH + F_RHL,
                            data = sitfull100,
                            nbest = 1,       # 1 best model for each number of predictors
                            nvmax = NULL,    # NULL for no limit on number of variables
                            force.in = NULL, force.out = NULL,
                            method = "exhaustive")
sumregsub.s100g2 <- summary(regsub.s100g2)

exhaustivesearch.s100g2 <- cbind(sumregsub.s100g2$which, sumregsub.s100g2$rsq, sumregsub.s100g2$adjr2, sumregsub.s100g2$bic)

View(exhaustivesearch.s100g2)
#
plot(s100g2)
psg2 <- plot(allEffects(s100g2))
psg1 <- plot(allEffects(s100g1))

###

##### other regression (glm) with SUPPNUM #####
# nrow(sitfull100) = 4815
regsub.s100g3 <- regsubsets(SUPPNUM ~ LOG_AREA + IMT_RANK + npl + UnitType + OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL +
                              C_TEMPH + C_RHL + F_TEMPH + F_RHL,
                            data = sitfull100,
                            nbest = 1,       # 1 best model for each number of predictors
                            nvmax = NULL,    # NULL for no limit on number of variables
                            force.in = NULL, force.out = NULL,
                            method = "exhaustive")
sumregsub.s100g3 <- summary(regsub.s100g3)
exhaustivesearch.s100g3 <- cbind(sumregsub.s100g3$which, sumregsub.s100g3$rsq, sumregsub.s100g3$adjr2, sumregsub.s100g3$bic)
View(exhaustivesearch.s100g3)
#s100g3 <- glm(SUPPNUM ~ LOG_AREA + IMT_RANK + npl + UnitType + OWNERSHIP_STATE + TERRAIN + PRIMARY_FUEL_MODEL +
#                C_TEMPH + C_RHL + F_TEMPH + F_RHL, 
#              family= ?, data=sitfull100)
#psg3 <- plot(allEffects(s100g3))
#summary(s100g3)
#View(s100g3)

###

##### export #####

#write.csv(exhaustivesearch.s100g2, file = 
#            "/Users/Amber/Documents/GitHub/sit_working/z_pics_summaries_extracted/exhaustivesearch.s100g2.csv")


##### sources #####
# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
# https://data.library.virginia.edu/visualizing-the-effects-of-proportional-odds-logistic-regression/
