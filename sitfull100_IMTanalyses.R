##############################################################################################################################
###### IMT analyses and coding #####
#
# This script features analyses of the Incident Management Team types reported in the daily reports of incidents. It should be
# used after/with sitfull_prep2 because sitfull_prep2 characterizes the variables, creates new variables needed, and filters
# incidents to only those with a max fire size >=100 acres.
#
##############################################################################################################################

# After running base code in sitfull_prep2...

##### characterizing IMT types #####
#
# Type 5:       small fire, initial attack; few resources (2-6 people); little incident complexity; local
#               team (village or township)
#
# Type 4:       small fire, initial attack; few resources; IC performs all functions of command  
#               (operations, planning, logistics, and finance); generally a local team (City, County or 
#               Fire District Level)
#
# Type 3:       Extended initial attack; IC goes between manager and "doer"; single team or several  
#               teams; everal command positions (but not all) filled; maybe more than one operational 
#               period (12 hrs); state or regional team or multiple agencies (standing team of trained 
#               personnel from different departments, organizations, agencies, and jurisdictions within 
#               a state); all states and metropolitan areas were required (at least at the time of this
#               study period) to have at minimum a Type 3 IMT.
#
# Type 2:       IC is manager of incident, not a "doer"; most command positions filled; large amount of 
#               resources (200-500 people); multiple operational periods; base camp(s) established; 
#               significant logistical support; nationally- or state-certified team but less training 
#               and/or resources than Type 1; typically used on smaller scale national or state 
#               incidents; *major fires*
#
# Type 1:       Large, complex incident; multiple agencies; national resources; generally 500-1000 people;
#               the most robust IMT with the most experience; is fully equipped and self-contained
#
# Area Command: Incident is so large it needs to be managed as multiple incidents; multi-jurisdictional
#
# Unified Command is just multiple agencies, and is not necessarily a linear jump from/to the categories
# listed above.
#
# FUMT:         Fire Use Management Team; provide unit and agency managers with skilled & mobile 
#               personnel to assist with management of Wildland Fire Use (WFU) & prescribed fires; 
#               offers full range of appropriate management responses to wildland fire occurrences &
#               large, complex prescribed fire applications with priority given to wildland fire use &
#               long-duration suppression activities; supervised by an IC, 7-10 positions
#
# FUM2:         Fire Use Manager Type 2
#
# SOPL:         Strategic Operational Planner (SOPL); responsible for developing courses of action on 
#               long-term wildfire events (may include both protection and resource benefit objectives); 
#               may be ordered by & work forthe host unit, the Geographic Area Coordination Center (GACC), 
#               or the IMT assigned to the fire.

###

##### looking at sitfull100, IMT types and log of area #####
require(car)
View(sitfull100)
summary(sitfull100)
summary(sitfull100$IMT_TYPE_DESCRIPTION)
is.factor(sitfull100$IMT_TYPE_DESCRIPTION) # TRUE (if sitfull_prep2 is run prior to this script)

###

##### ANOVA for IMT_TYPE_DESCRIPTION, Log of Area #####

options(scipen=10000)

bp.imt.test <- ggplot(sitfull100, aes(sitfull100$IMT_TYPE_DESCRIPTION, sitfull100$AREA_Log)) +
  geom_boxplot(aes(fill=sitfull100$IMT_TYPE_DESCRIPTION)) + 
  labs(title="IMT Types v Log of Area")

bp.imt.test + theme(legend.position = "none")

##### aov, IMT types #####
# fitting the model, log of area:

a1 <- aov(sitfull100$AREA_Log~sitfull100$IMT_TYPE_DESCRIPTION)
summary(a1)

# results from a1:
#                                    Df  Sum Sq Mean Sq  F value              Pr(>F)    
#  sitfull100$IMT_TYPE_DESCRIPTION   12   6531   544.2   118.4    <0.0000000000000002 ***
#  Residuals                       4541  20879     4.6                                
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  261 observations deleted due to missingness

##### tukey's HSD, IMT types #####

# Finding which IMT types differ with Tukey's HSD:
TukeyHSD(a1, conf.level = 0.99)

# results:
#Tukey multiple comparisons of means
#99% family-wise confidence level

#Fit: aov(formula = sitfull100$AREA_Log ~ sitfull100$IMT_TYPE_DESCRIPTION)
#
#$`sitfull100$IMT_TYPE_DESCRIPTION`
#                                    diff          lwr        upr     p adj
#FUM2-Area Command             -0.39285486  -5.52488084  4.7391711 1.0000000
#FUMT-Area Command              6.93280436   0.30738731 13.5582214 0.0050914
#SOPL-Area Command             -2.00570213  -6.72841219  2.7170079 0.9281255
#Type 1 IC-Area Command        -3.25162384 -12.62137850  6.1181308 0.9849410
#Type 1 Team-Area Command       2.96800977  -1.74079049  7.6768100 0.4546144
#Type 2 IC-Area Command         2.09447735  -3.19077707  7.3797318 0.9564912
#Type 2 Team-Area Command       2.54868865  -2.14414039  7.2415177 0.6957027
#Type 3 IC-Area Command         1.15022620  -3.54150873  5.8419611 0.9994173
#Type 3 Team-Area Command       2.00983597  -2.68368771  6.7033596 0.9238867
#Type 4 IC-Area Command         0.51304363  -4.17793512  5.2040224 0.9999999
#Type 5 IC-Area Command        -1.09101731  -5.83557701  3.6535424 0.9996973
#Unified Command-Area Command   3.83271447  -0.97601357  8.6414425 0.1182372
#FUMT-FUM2                      7.32565922   2.19363323 12.4576852 0.0000054
#SOPL-FUM2                     -1.61284728  -3.79127099  0.5655764 0.2015056
#Type 1 IC-FUM2                -2.85876899 -11.23933232  5.5217943 0.9869869
#Type 1 Team-FUM2               3.36086463   1.21276327  5.5089660 0.0000003
#Type 2 IC-FUM2                 2.48733221  -0.73376293  5.7084274 0.1507270
#Type 2 Team-FUM2               2.94154350   0.82868196  5.0544050 0.0000111
#Type 3 IC-FUM2                 1.54308105  -0.56734926  3.6535114 0.2181807
#Type 3 Team-FUM2               2.40269082   0.28828687  4.5170948 0.0012379
#Type 4 IC-FUM2                 0.90589849  -1.20285024  3.0146472 0.9221855
#Type 5 IC-FUM2                -0.69816245  -2.92355821  1.5272333 0.9937666
#Unified Command-FUM2           4.22556932   1.86645829  6.5846804 0.0000000
#SOPL-FUMT                     -8.93850649 -13.66121655 -4.2157964 0.0000000
#Type 1 IC-FUMT               -10.18442820 -19.55418286 -0.8146735 0.0027370
#Type 1 Team-FUMT              -3.96479459  -8.67359485  0.7440057 0.0735667
#Type 2 IC-FUMT                -4.83832701 -10.12358143  0.4469274 0.0309225
#Type 2 Team-FUMT              -4.38411571  -9.07694475  0.3087133 0.0243500
#Type 3 IC-FUMT                -5.78257816 -10.47431309 -1.0908432 0.0002361
#Type 3 Team-FUMT              -4.92296839  -9.61649207 -0.2294447 0.0049050
#Type 4 IC-FUMT                -6.41976073 -11.11073948 -1.7287820 0.0000178
#Type 5 IC-FUMT                -8.02382167 -12.76838137 -3.2792620 0.0000001
#Unified Command-FUMT          -3.10008989  -7.90881793  1.7086381 0.4159537
#Type 1 IC-SOPL                -1.24592171  -9.38226864  6.8904252 0.9999963
#Type 1 Team-SOPL               4.97371191   4.21171515  5.7357087 0.0000000
#Type 2 IC-SOPL                 4.10017949   1.58189564  6.6184633 0.0000001
#Type 2 Team-SOPL               4.55439078   3.89827836  5.2105032 0.0000000
#Type 3 IC-SOPL                 3.15592833   2.50768783  3.8041688 0.0000000
#Type 3 Team-SOPL               4.01553810   3.35447556  4.6766006 0.0000000
#Type 4 IC-SOPL                 2.51874577   1.87600099  3.1614905 0.0000000
#Type 5 IC-SOPL                 0.91468483  -0.04379622  1.8731659 0.0187153
#Unified Command-SOPL           5.83841660   4.60079610  7.0760371 0.0000000
#Type 1 Team-Type 1 IC          6.21963362  -1.90864732 14.3479146 0.1610172
#Type 2 IC-Type 1 IC            5.34610120  -3.12916078 13.8213632 0.4533119
#Type 2 Team-Type 1 IC          5.80031249  -2.31872658 13.9193516 0.2508482
#Type 3 IC-Type 1 IC            4.40185004  -3.71655668 12.5202568 0.6980525
#Type 3 Team-Type 1 IC          5.26145981  -2.85798079 13.3809004 0.4071710
#Type 4 IC-Type 1 IC            3.76466748  -4.35330227 11.8826372 0.8715783
#Type 5 IC-Type 1 IC            2.16060653  -5.98844236 10.3096554 0.9987274
#Unified Command-Type 1 IC      7.08433831  -1.10223707 15.2709137 0.0565144
#Type 2 IC-Type 1 Team         -0.87353242  -3.36563256  1.6185677 0.9836206
#Type 2 Team-Type 1 Team       -0.41932113  -0.96639799  0.1277557 0.1590856
#Type 3 IC-Type 1 Team         -1.81778358  -2.35539433 -1.2801728 0.0000000
#Type 3 Team-Type 1 Team       -0.95817381  -1.51117767 -0.4051699 0.0000000
#Type 4 IC-Type 1 Team         -2.45496614  -2.98593735 -1.9239949 0.0000000
#Type 5 IC-Type 1 Team         -4.05902708  -4.94643651 -3.1716177 0.0000000
#Unified Command-Type 1 Team    0.86470470  -0.31872848  2.0481379 0.2191015
#Type 2 Team-Type 2 IC          0.45421129  -2.00757821  2.9160008 0.9999709
#Type 3 IC-Type 2 IC           -0.94425116  -3.40395435  1.5154520 0.9658717
#Type 3 Team-Type 2 IC         -0.08464139  -2.54775481  2.3784720 1.0000000
#Type 4 IC-Type 2 IC           -1.58143372  -4.03969426  0.8768268 0.4195780
#Type 5 IC-Type 2 IC           -3.18549466  -5.74451986 -0.6264695 0.0001890
#Unified Command-Type 2 IC      1.73823712  -0.93788481  4.4143590 0.4031620
#Type 3 IC-Type 2 Team         -1.39846245  -1.77111481 -1.0258101 0.0000000
#Type 3 Team-Type 2 Team       -0.53885268  -0.93338750 -0.1443179 0.0000188
#Type 4 IC-Type 2 Team         -2.03564501  -2.39865311 -1.6726369 0.0000000
#Type 5 IC-Type 2 Team         -3.63970596  -4.43804169 -2.8413702 0.0000000
#Unified Command-Type 2 Team    1.28402582   0.16583150  2.4022201 0.0010155
#Type 3 Team-Type 3 IC          0.85960977   0.47830943  1.2409101 0.0000000
#Type 4 IC-Type 3 IC           -0.63718256  -0.98576126 -0.2886039 0.0000000
#Type 5 IC-Type 3 IC           -2.24124350  -3.03312239 -1.4493646 0.0000000
#Unified Command-Type 3 IC      2.68248827   1.56889465  3.7960819 0.0000000
#Type 4 IC-Type 3 Team         -1.49679233  -1.86867276 -1.1249119 0.0000000
#Type 5 IC-Type 3 Team         -3.10085327  -3.90326222 -2.2984443 0.0000000
#Unified Command-Type 3 Team    1.82287850   0.70177248  2.9439845 0.0000001
#Type 5 IC-Type 4 IC           -1.60406094  -2.39144730 -0.8166746 0.0000000
#Unified Command-Type 4 IC      3.31967084   2.20926736  4.4300743 0.0000000
#Unified Command-Type 5 IC      4.92373178   3.60518677  6.2422768 0.0000000

##### plotting Tukey's HSD, IMT types #####

# visualizing the Tukey's HSD results:
plot(TukeyHSD(a1, conf.level = 0.99),las=1, col = "red")
# another visualization, plotting the means:
plotmeans(sitfull100$AREA_Log~sitfull100$IMT_TYPE_DESCRIPTION, 
          main="Mean Plot with 95% Confidence Interval (Log of Area, IMT Description, from sitfull100)", 
          ylab = "Log of Area", xlab = "IMT type")

##### plotting assumptions, IMT types #####
# checking model assumptions, visually:
par(mfrow=c(2,2))
plot(a1)

##### plotting normality, IMT types #####
# checking normality of residuals of the model:
uhat<-resid(a1)

# conducting Shapiro-Wilk test for normality:
shapiro.test(uhat)
#results:
# 	Shapiro-Wilk normality test

#data:  uhat
#W = 0.99383, p-value = 0.000000000000433

##### plotting variances, IMT types #####
# testing homogeneity of variances across IMT types:
library(car)
leveneTest(sitfull100$AREA_Log~sitfull100$IMT_TYPE_DESCRIPTION)
#results:
# Levene's Test for Homogeneity of Variance (center = median)
#        Df  F value                Pr(>F)    
#group   12  10.734   < 0.00000000000000022  ***
#      4541                                  
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### 

# as a result of the tests conducted above, I come to the conclusion that the IMT 
# types are significantly different.

###

##### aov r1, IMT ranks ANOVA #####
# ANOVA testing if differences among ranks exist
r1 <- aov(sitfull100$AREA_Log~sitfull100$IMT_RANK)
summary(r1)
#                        Df  Sum Sq  Mean Sq   F value              Pr(>F)    
#  sitfull100$IMT_RANK    5    3875    775.0     149.8  <0.0000000000000002 ***
#  Residuals           4548   23535      5.2                                
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 261 observations deleted due to missingness

# Tukey's HSD to find differences
TukeyHSD(r1, conf.level = 0.99)

# Tukey multiple comparisons of means
#  99% family-wise confidence level

# Fit: aov(formula = sitfull100$AREA_Log ~ sitfull100$IMT_RANK)

# $`sitfull100$IMT_RANK`
#        diff        lwr         upr     p adj
#B-A -0.6909761 -1.7832444  0.40129212 0.2722523 << least different comparison is B-A; B is ICT/IMT 1 & A is Area/Unified Command
#C-A -1.0947308 -2.1239154 -0.06554609 0.0046731
#D-A -2.4344618 -3.4455982 -1.42332540 0.0000000
#E-A -3.1247870 -4.1468335 -2.10274058 0.0000000
#F-A -4.7288480 -5.9514329 -3.50626311 0.0000000
#C-B -0.4037546 -0.9185178  0.11100848 0.0880416 << second least different is C-B; C is ICT/IMT 2
#D-B -1.7434857 -2.2211433 -1.26582806 0.0000000
#E-B -2.4338109 -2.9341498 -1.93347208 0.0000000
#F-B -4.0378719 -4.8748150 -3.20092875 0.0000000
#D-C -1.3397310 -1.6468597 -1.03260243 0.0000000
#E-C -2.0300563 -2.3713921 -1.68872047 0.0000000
#F-C -3.6341172 -4.3868755 -2.88135900 0.0000000
#E-D -0.6903252 -0.9726121 -0.40803836 0.0000000
#F-D -2.2943862 -3.0222740 -1.56649835 0.0000000
#F-E -1.6040609 -2.3470299 -0.86109200 0.0000000

##### Plotting Tukey's HSD of IMT Ranks #####
plot(TukeyHSD(r1, conf.level = 0.99),las=1, col = "orange")

# Plotting the means:
plotmeans(sitfull100$AREA_Log~sitfull100$IMT_RANK, 
          main="Mean Plot, IMT Ranks with 95% Confidence Interval", 
          ylab = "Log of Area", xlab = "IMT RANK (A-F & NAs)")

##### plotting assumptions, IMT ranks #####

plot(r1)

##### plotting normality, IMT ranks #####

# checking normality of residuals of the model:
uhatr<-resid(r1)

# conducting Shapiro-Wilk test for normality:
shapiro.test(uhatr)

##### plotting variances, IMT ranks #####

leveneTest(sitfull100$AREA_Log~sitfull100$IMT_RANK)
# results: 
# Levene's Test for Homogeneity of Variance (center = median)
#          Df  F value                 Pr(>F)    
#  group    5   16.506  0.0000000000000003498 ***
#        4548                                  
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Kruskal-Wallis test (nonparametric):

kruskal.test(sitfull100$AREA_Log, sitfull100$IMT_RANK) 

# Kruskal-Wallis rank sum test

#  data:  sitfull100$AREA_Log and sitfull100$IMT_RANK
#  Kruskal-Wallis chi-squared = 598.82, df = 5, p-value < 0.00000000000000022

###

##### aov2, IMT types & duration #####
# fitting the model, duration:

a2 <- aov(sitfull100$DURATION~sitfull100$IMT_TYPE_DESCRIPTION)
summary(a2)

# results from a2:
#                                   Df  Sum Sq  Mean Sq  F value              Pr(>F)    
# sitfull100$IMT_TYPE_DESCRIPTION   12  273833    22819    23.17  <0.0000000000000002 ***
#  Residuals                       4551 4481717     985                                
#   ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  251 observations deleted due to missingness

##### tukey's HSD, IMT types & duration #####

# Finding which IMT types differ with Tukey's HSD:
TukeyHSD(a2, conf.level = 0.99)

# results:
# Tukey multiple comparisons of means
# 99% family-wise confidence level

# Fit: aov(formula = sitfull100$DURATION ~ sitfull100$IMT_TYPE_DESCRIPTION)

#$`sitfull100$IMT_TYPE_DESCRIPTION`
#                                 diff         lwr         upr     p adj
#FUM2-Area Command            -25.133333 -100.239084  49.9724169 0.9889667
#FUMT-Area Command            -61.333333 -158.294440  35.6277733 0.4485052
#SOPL-Area Command            -19.830631  -88.946158  49.2848967 0.9972732
#Type 1 IC-Area Command       -52.333333 -189.457045  84.7903787 0.9673893
#Type 1 Team-Area Command     -10.210466  -79.122428  58.7014950 0.9999975
#Type 2 IC-Area Command       -37.333333 -114.681539  40.0148719 0.8365793
#Type 2 Team-Area Command     -32.145338 -100.823565  36.5328891 0.8640344
#Type 3 IC-Area Command       -28.261489  -96.923119  40.4001421 0.9424505
#Type 3 Team-Area Command     -17.296388  -85.984780  51.3920054 0.9992371
#Type 4 IC-Area Command       -14.614583  -83.265655  54.0364879 0.9998648
#Type 5 IC-Area Command       -22.116667  -91.530255  47.2969221 0.9928361
#Unified Command-Area Command -35.654762 -106.029138  34.7196143 0.7854952
#FUMT-FUM2                    -36.200000 -111.305750  38.9057503 0.8379186
#SOPL-FUM2                      5.302703  -26.577913  37.1833182 0.9999907
#Type 1 IC-FUM2               -27.200000 -149.847177  95.4471766 0.9997922
#Type 1 Team-FUM2              14.922867  -16.513990  46.3597233 0.8519217
#Type 2 IC-FUM2               -12.200000  -59.339817  34.9398173 0.9989974
#Type 2 Team-FUM2              -7.012005  -37.933136  23.9091271 0.9997380
#Type 3 IC-FUM2                -3.128155  -34.012408  27.7560973 1.0000000
#Type 3 Team-FUM2               7.836946  -23.106759  38.7806502 0.9991918
#Type 4 IC-FUM2                10.518750  -20.342020  41.3795196 0.9870761
#Type 5 IC-FUM2                 3.016667  -29.505077  35.5384105 1.0000000
#Unified Command-FUM2         -10.521429  -45.046352  24.0034949 0.9952145
#SOPL-FUMT                     41.502703  -27.612825 110.6182301 0.5375137
#Type 1 IC-FUMT                 9.000000 -128.123712 146.1237120 1.0000000
#Type 1 Team-FUMT              51.122867  -17.789095 120.0348283 0.1989026 <<<<<<
#Type 2 IC-FUMT                24.000000  -53.348205 101.3482053 0.9943592
#Type 2 Team-FUMT              29.187995  -39.490232  97.8662225 0.9277680
#Type 3 IC-FUMT                33.071845  -35.589786 101.7334754 0.8385537
#Type 3 Team-FUMT              44.036946  -24.651447 112.7253387 0.4254485
#Type 4 IC-FUMT                46.718750  -21.932321 115.3698212 0.3256630
#Type 5 IC-FUMT                39.216667  -30.196922 108.6302554 0.6368147
#Unified Command-FUMT          25.678571  -44.695805  96.0529476 0.9771904
#Type 1 IC-SOPL               -32.502703 -151.575841  86.5704359 0.9983095
#Type 1 Team-SOPL               9.620164   -1.531443  20.7717715 0.0582906 <<<<<<
#Type 2 IC-SOPL               -17.502703  -54.357075  19.3516699 0.8515063
#Type 2 Team-SOPL             -12.314707  -21.916727  -2.7126873 0.0000942 <<<<<<
#Type 3 IC-SOPL                -8.430858  -17.913445   1.0517294 0.0427384
#Type 3 Team-SOPL               2.534243   -7.140220  12.2087067 0.9988701
#Type 4 IC-SOPL                 5.216047   -4.189776  14.6218701 0.6652099
#Type 5 IC-SOPL                -2.286036  -16.205311  11.6332393 0.9999919
#Unified Command-SOPL         -15.824131  -33.936358   2.2880951 0.0513134 <<<<<<
#Type 1 Team-Type 1 IC         42.122867  -76.832228 161.0779619 0.9821688
#Type 2 IC-Type 1 IC           15.000000 -109.033064 139.0330644 0.9999997
#Type 2 Team-Type 1 IC         20.187995  -98.631847 139.0078384 0.9999882
#Type 3 IC-Type 1 IC           24.071845  -94.738406 142.8820957 0.9999201
#Type 3 Team-Type 1 IC         35.036946  -83.788773 153.8626649 0.9964746
#Type 4 IC-Type 1 IC           37.718750  -81.085399 156.5228988 0.9930613
#Type 5 IC-Type 1 IC           30.216667  -89.029727 149.4630608 0.9991875
#Unified Command-Type 1 IC     16.678571 -103.129646 136.4867891 0.9999987
#Type 2 IC-Type 1 Team        -27.122867  -63.594048   9.3483146 0.1957377 <<<<<<
#Type 2 Team-Type 1 Team      -21.934871  -29.941187 -13.9285561 0.0000000 <<<<<<
#Type 3 IC-Type 1 Team        -18.051022  -25.913704 -10.1883406 0.0000000 <<<<<<
#Type 3 Team-Type 1 Team       -7.085921  -15.178976   1.0071342 0.0501923
#Type 4 IC-Type 1 Team         -4.404117  -12.174046   3.3658124 0.6317334
#Type 5 IC-Type 1 Team        -11.906200  -24.776652   0.9642514 0.0273755
#Unified Command-Type 1 Team  -25.444295  -42.763506  -8.1250853 0.0000022 <<<<<<
#Type 2 Team-Type 2 IC          5.187995  -30.839598  41.2155893 0.9999982
#Type 3 IC-Type 2 IC            9.071845  -26.924102  45.0677916 0.9992305
#Type 3 Team-Type 2 IC         20.036946  -16.010023  56.0839149 0.6616927
#Type 4 IC-Type 2 IC           22.718750  -13.257051  58.6945507 0.4513749
#Type 5 IC-Type 2 IC           15.216667  -22.193692  52.6270256 0.9472090
#Unified Command-Type 2 IC      1.678571  -37.485717  40.8428597 1.0000000
#Type 3 IC-Type 2 Team          3.883849   -1.562453   9.3301511 0.2534905
#Type 3 Team-Type 2 Team       14.848950    9.075045  20.6228559 0.0000000 <<<<<<
#Type 4 IC-Type 2 Team         17.530755   12.219235  22.8422744 0.0000000 <<<<<<
#Type 5 IC-Type 2 Team         10.028671   -1.525071  21.5824137 0.0548152 <<<<<<
#Unified Command-Type 2 Team   -3.509424  -19.873882  12.8550339 0.9998539
#Type 3 Team-Type 3 IC         10.965101    5.392071  16.5381311 0.0000000 <<<<<<
#Type 4 IC-Type 3 IC           13.646905    8.554468  18.7393429 0.0000000 <<<<<<
#Type 5 IC-Type 3 IC            6.144822   -5.309856  17.5994998 0.7130018
#Unified Command-Type 3 IC     -7.393273  -23.687940   8.9013934 0.8881146
#Type 4 IC-Type 3 Team          2.681804   -2.759584   8.1231920 0.8157551
#Type 5 IC-Type 3 Team         -4.820279  -16.434298   6.7937393 0.9389057
#Unified Command-Type 3 Team  -18.358374  -34.765444  -1.9513045 0.0016450 <<<<<<
#Type 5 IC-Type 4 IC           -7.502083  -18.893294   3.8891274 0.3797550
#Unified Command-Type 4 IC    -21.040179  -37.290292  -4.7900648 0.0000748 <<<<<<
#Unified Command-Type 5 IC    -13.538095  -32.756392   5.6802013 0.2716761

##### plotting Tukey's HSD, IMT types & duration #####

# visualizing the Tukey's HSD results:
plot(TukeyHSD(a2, conf.level = 0.99),las=1, col = "blue")

# another visualization, plotting the means:
plotmeans(sitfull100$DURATION~sitfull100$IMT_TYPE_DESCRIPTION, 
          main="Mean Plot with 95% Confidence Interval (Duration, IMT Description, from sitfull100)", 
          ylab = "Duration of Incident", xlab = "IMT type")

##### plotting assumptions, IMT types & duration #####
# checking model assumptions, visually:
par(mfrow=c(2,2))
plot(a2)

##### plotting normality, IMT types & duration #####
# checking normality of residuals of the model:
uhat2<-resid(a2)

# conducting Shapiro-Wilk test for normality:
shapiro.test(uhat2)

#results:
#	Shapiro-Wilk normality test

# data:  uhat2
# W = 0.97754, p-value < 0.00000000000000022


##### checking variances, IMT types & duration #####
# testing homogeneity of variances across IMT types:

leveneTest(sitfull100$DURATION~sitfull100$IMT_TYPE_DESCRIPTION)
#results:
# Levene's Test for Homogeneity of Variance (center = median)
#        Df  F value                 Pr(>F)    
#group   12   23.325  < 0.00000000000000022 ***
#      4551                                  
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### 

# as a result of the tests conducted above, I come to the conclusion that the IMT 
# types are significantly different.

###

##### grouping by rank for correlation tests #####

sitfull100 %>%
  group_by(IMT_RANK) %>%
  summarise(area.avg = mean(AREA, na.rm=TRUE),  area.min = min(AREA, na.rm=TRUE),  area.max = max(AREA, na.rm=TRUE), 
            log.area.avg = mean(AREA_Log, na.rm=TRUE), log.area.min = min(AREA_Log, na.rm=TRUE),  log.area.max = max(AREA_Log, na.rm=TRUE), 
            dur.avg = mean(DURATION, na.rm=TRUE),  dur.min = min(DURATION, na.rm=TRUE),  dur.max = max(DURATION, na.rm=TRUE),
            ctemp.avg = mean(C_TEMP, na.rm=TRUE), ctemp.min = min(C_TEMP, na.rm=TRUE), ctemp.max = max(C_TEMP, na.rm=TRUE), 
            ftemp.avg = mean(F_TEMP, na.rm=TRUE), ftemp.min = min(F_TEMP, na.rm=TRUE), ftemp.max = max(F_TEMP, na.rm=TRUE), 
            unit.count = n_distinct(UNIT_TYPE, na.rm=TRUE), inc.count = n_distinct(INCIDENT_ID_KS, na.rm=TRUE)) -> imtrank100
summary(imtrank100)
View(imtrank100)

##### correlations of IMT Ranks #####

ggscatter(imtrank100, x = "log.area.avg", y = "dur.avg", 
          color = "IMT_RANK",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "log of area", ylab = "duration")

# Shapiro-Wilk normality test for log of area, average (Null hypothesis: the data are normally distributed)
shapiro.test(imtrank100$log.area.avg) # p-value = 0.7775
# Shapiro-Wilk normality test for duration, average (Null hypothesis: the data are normally distributed)
shapiro.test(imtrank100$dur.avg) # p-value = 0.3031

# Shapiro-Wilk normality test for duration, average (Null hypothesis: the data are normally distributed)
shapiro.test(imtrank100$ctemp.avg) # p-value = 0.6475

               
colnames(imtrank100)
ggscatter(imtrank100, x = "log.area.avg", y = "ctemp.avg", shape = "IMT_RANK",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "average log of area", ylab = "average current temp")

ggscatter(imtrank100, x = "log.area.avg", y = "ctemp.avg", shape = "IMT_RANK",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "average log of area", ylab = "average current temp")



###

##### aov3, IMT Ranks and national preparedness levels #####

a3 <- aov(sitfull100$npl~sitfull100$IMT_RANK)
summary(a3)

TukeyHSD(a3, conf.level = 0.99)

plot(TukeyHSD(a3, conf.level = 0.99),las=1, col = "purple")

# another visualization, plotting the means:
plotmeans(sitfull100$npl~sitfull100$IMT_RANK, 
          main="Mean Plot with 95% Confidence Interval 
          National Preparedness Levels, IMT Ranks", 
          ylab = "National Preparedness Level", xlab = "IMT Rank")

par(mfrow=c(2,2))
plot(a3)

# checking normality of residuals of the model:
uhat3<-resid(a3)

# conducting Shapiro-Wilk test for normality:
shapiro.test(uhat3)

# testing homogeneity of variances across IMT types:
leveneTest(sitfull100$npl~sitfull100$IMT_RANK)

###

##### more #####

# mean plots
colnames(sitfull100)
plotmeans(sitfull100$SUPPNUM~sitfull100$npl, 
          main="Mean Plot with 95% Confidence Interval 
          Suppression Types v. National Preparedness Levels", 
          ylab = "Suppression Type", xlab = "National Preparedness Level")
plotmeans(sitfull100$SUPPNUM~sitfull100$IMT_RANK, 
          main="Mean Plot with 95% Confidence Interval 
          Suppression Types v. IMT Ranks", 
          ylab = "Suppression Type", xlab = "IMT Rank")
plotmeans(sitfull100$SUPPNUM~sitfull100$IMT_TYPE_DESCRIPTION, 
          main="Mean Plot with 95% Confidence Interval 
          Suppression Types v. IMT Types", 
          ylab = "Suppression Type", xlab = "IMT Types")

##### sources #####
# ANOVA codes and steps from: https://datascienceplus.com/one-way-anova-in-r/
# nonparametric testing: https://www.sheffield.ac.uk/polopoly_fs/1.579191!/file/stcp-karadimitriou-normalR.pdf





