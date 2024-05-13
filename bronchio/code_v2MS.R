#-package list----

require(here)
require(readxl)
require(tidyverse)
require(ggcorrplot)
require(rcompanion)
#require(car)
#require(glmnet)
require(bestglm)
require(rms)
require(lmtest)

#-data loading----

reaped <- read.csv2("/Users/damianocerasuolo/Desktop/UBRC/UBRS_CONSULT_MAC/Brossier_David/StatistiquesReaPed/BronchioOHD.csv")

names(reaped)
dim(reaped)

#-var selection for prediction----

#BronchioOHD$pHCAT=cut(BronchioOHD$pH,c(7.15, 7.3, 7.38,7.5))
str(reaped$pH)

reaped <- reaped %>%
  mutate(pHCAT = case_when(
    pH <= 7.3 ~ "1",
    pH > 7.3 ~ "2",
  ))
table(reaped$pHCAT, useNA = "always")

#-variable transformation
# str(rp)
#'data.frame':	383 obs. of  15 variables:
#$ TransfertREA    : chr  "non" "oui" "non" "non" ...
#$ Terme           : chr  ">37" ">37" ">37" ">37" ...
#$ Ageb            : chr  "1-3mois" "1-3mois" "1-3mois" "1-3mois" ...
#$ Poids           : num  6.58 6.13 5.74 6.08 6.67 ...
#$ PremiereBronchio: chr  "oui" "oui" "oui" "oui" ...
#$ Dureesymptomes  : int  5 3 3 4 5 3 2 3 4 4 ...
#$ virus           : chr  "non identifie" "VRS" "non identifie" "VRS" ...
#$ Coinfection     : chr  "non" "non" "non" "non" ...
#$ FC              : chr  "normocarde" "normocarde" "normocarde" "tachycarde>160/min" ...
#$ Spo2            : chr  ">92%" ">92%" ">92%" ">92%" ...
#$ ApneesMaison    : chr  "non" "non" "non" "non" ...
#$ MalaiseHOP      : chr  "non" "non" "non" "non" ...
#$ pHCAT           : chr  NA NA NA NA ...
#$ RP              : chr  "syndrome bronchique" "syndrome bronchique" "syndrome bronchique" "syndrome bronchique" ...
#$ TRe             : Factor w/ 2 levels "0","1": 1 2 1 1 1 1 1 1 1 1 ...

# transfert
reaped$TRe = as.factor(if_else(reaped$TransfertREA == "oui", "2", "1"))

# Terme
reaped <- reaped %>%
  mutate(Terme.F = case_when(
    Terme == "<28" | Terme == "28-32" | Terme == "32-37" ~ "1",
    Terme == ">37" ~ "2"
  ))
reaped$Terme.F = as.factor(reaped$Terme.F)
table(reaped$Terme.F)

# Ageb
reaped <- reaped %>%
  mutate(Age.F = case_when(
    Ageb == "<1mois" ~ "1",
    Ageb == ">3mois" ~ "2",
    Ageb == "1-3mois" ~ "3"
  ))
reaped$Age.F = as.factor(reaped$Age.F)

# PremiereBronchio
reaped$PremiereBronchio.F = as.factor(if_else(reaped$PremiereBronchio == "oui", "1", "0"))

# virus
reaped <- reaped %>%
  mutate(virus.F = case_when(
    virus == "Autre" ~ "1", 
    virus == "ERV" ~ "2",
    virus == "non identifie" ~ "3",
    virus == "VRS" ~ "4"
  ))
reaped$virus.F = as.factor(reaped$virus.F)
table(reaped$virus.F)

reaped <- reaped %>%
  mutate(virus.Fb = case_when(
    virus == "Autre" | virus == "ERV" | virus == "non identifie" ~ "0", # autre que VRS
    virus == "VRS" ~ "1"
  ))
table(reaped$virus.Fb, reaped$virus)
table(reaped$virus.Fb, useNA = "always")

# Coinfection
reaped$Coinfection.F = as.factor(if_else(reaped$Coinfection == "oui", "1", "0"))

# FC
reaped <- reaped %>%
  mutate(FC.F = case_when(
    FC == "normocarde" ~ "1",
    FC == "tachycarde>160/min" ~ "2",
    FC == "tachycarde>180min" ~ "3"
  ))
reaped$FC.F = as.factor(reaped$FC.F)

# Spo2
reaped <- reaped %>%
  mutate(Spo2.F = case_when(
    Spo2 == "<88%" | Spo2 == "88-92%" ~ "1", # < 92
    Spo2 == ">92%" ~ "2", # > 92
  ))
reaped$Spo2.F = as.factor(reaped$Spo2.F)

# ApneesMaison
reaped$ApneesMaison.F = as.factor(if_else(reaped$ApneesMaison == "oui", "1", "0"))

# MalaiseHOP
reaped$MalaiseHOP.F = as.factor(if_else(reaped$MalaiseHOP == "oui", "1", "0"))

# RP
reaped <- reaped %>%
  mutate(RP.F = case_when(
    RP == "normale" ~ "1",
    RP == "atelectasie" ~ "2", 
    RP == "distension thoracique" ~ "3",
    RP == "pneumopathie" ~ "4",
    RP == "syndrome bronchique" ~ "5"
  ))
reaped$RP.F = as.factor(reaped$RP.F)

# subset dataset----

rp = subset(reaped, select = c("TRe", "Terme.F", "Age.F", "Poids", "PremiereBronchio.F",
                              "Dureesymptomes", "virus.Fb", "Coinfection.F", "FC.F", "Spo2.F",
                              "ApneesMaison.F", "MalaiseHOP.F", "pHCAT"))

# TRe
rp$TRe = as.numeric(as.factor(rp$TRe))

#-reg log test----
glm.full = glm(TRe ~ Terme.F + Age.F + Poids + PremiereBronchio.F
                 + Dureesymptomes + virus.Fb + Coinfection.F
                 + FC.F + Spo2.F + ApneesMaison.F + MalaiseHOP.F
                 + pHCAT, data = rp)
glm.null = glm(TRe ~ 1, data = rp)

# LRT SELECTION----
# manual likelihood-ratio-test-based backward selection
drop1(glm.full, test = "LRT")

#TRe ~ Terme.F + Age.F + Poids + PremiereBronchio.F + Dureesymptomes + 
#    virus.Fb + Coinfection.F + FC.F + Spo2.F + ApneesMaison.F + 
#    MalaiseHOP.F + pHCAT
#                   Df Deviance    AIC scaled dev. Pr(>Chi)  
#<none>                  14.065 120.34                       
#Terme.F             1   14.790 122.66      4.3224  0.03761 *
#Age.F               2   15.257 123.33      6.9929  0.03031 *
#Poids               1   14.432 120.56      2.2148  0.13669  
#PremiereBronchio.F  1   14.709 122.19      3.8491  0.04977 *
#Dureesymptomes      1   14.175 119.01      0.6704  0.41292  
#virus.Fb            1   14.069 118.36      0.0233  0.87879  
#Coinfection.F       1   14.065 118.34      0.0002  0.98885  
#FC.F                2   14.495 118.93      2.5869  0.27433  
#Spo2.F              1   15.191 124.96      6.6210  0.01008 *
#ApneesMaison.F      1   14.101 118.56      0.2200  0.63902  
#MalaiseHOP.F        1   14.235 119.37      1.0301  0.31013  
#pHCAT               1   14.224 119.31      0.9666  0.32552  

# drop 1
drop1(update(glm.full, ~ . -Coinfection.F), test = "LRT")

#TRe ~ Terme.F + Age.F + Poids + PremiereBronchio.F + Dureesymptomes + 
#    virus.Fb + FC.F + Spo2.F + ApneesMaison.F + MalaiseHOP.F + 
#    pHCAT
#                   Df Deviance    AIC scaled dev. Pr(>Chi)   
#<none>                  14.065 118.34                        
#Terme.F             1   14.817 120.82      4.4755 0.034383 * 
#Age.F               2   15.263 121.37      7.0272 0.029790 * 
#Poids               1   14.432 118.56      2.2156 0.136626   
#PremiereBronchio.F  1   14.710 120.19      3.8526 0.049670 * 
#Dureesymptomes      1   14.175 117.01      0.6720 0.412369   
#virus.Fb            1   14.069 116.37      0.0236 0.877818   
#FC.F                2   14.495 116.93      2.5878 0.274195   
#Spo2.F              1   15.218 123.12      6.7746 0.009246 **
#ApneesMaison.F      1   14.102 116.57      0.2268 0.633910   
#MalaiseHOP.F        1   14.236 117.38      1.0404 0.307723   
#pHCAT               1   14.226 117.32      0.9781 0.322657        

# drop 2
drop1(update(glm.full, ~ . -Coinfection.F -virus.Fb), test = "LRT")

#TRe ~ Terme.F + Age.F + Poids + PremiereBronchio.F + Dureesymptomes + 
#    FC.F + Spo2.F + ApneesMaison.F + MalaiseHOP.F + pHCAT
#                   Df Deviance    AIC scaled dev. Pr(>Chi)   
#<none>                  14.069 116.37                        
#Terme.F             1   14.831 118.90      4.5325 0.033256 * 
#Age.F               2   15.270 119.41      7.0442 0.029537 * 
#Poids               1   14.444 116.63      2.2636 0.132445   
#PremiereBronchio.F  1   14.727 118.30      3.9312 0.047398 * 
#Dureesymptomes      1   14.182 115.06      0.6896 0.406285   
#FC.F                2   14.503 114.98      2.6142 0.270603   
#Spo2.F              1   15.228 121.17      6.8084 0.009073 **
#ApneesMaison.F      1   14.109 114.61      0.2449 0.620659   
#MalaiseHOP.F        1   14.237 115.39      1.0228 0.311851   
#pHCAT               1   14.239 115.39      1.0292 0.310346  

# drop 3
drop1(update(glm.full, ~ . -Coinfection.F -virus.Fb -ApneesMaison.F), test = "LRT")

#TRe ~ Terme.F + Age.F + Poids + PremiereBronchio.F + Dureesymptomes + 
#    FC.F + Spo2.F + MalaiseHOP.F + pHCAT
#                   Df Deviance    AIC scaled dev. Pr(>Chi)  
#<none>                  14.109 114.61                       
#Terme.F             1   14.889 117.23      4.6233  0.03154 *
#Age.F               2   15.463 118.49      7.8798  0.01945 *
#Poids               1   14.483 114.86      2.2478  0.13380  
#PremiereBronchio.F  1   14.863 117.09      4.4774  0.03435 *
#Dureesymptomes      1   14.225 113.31      0.7007  0.40255  
#FC.F                2   14.512 113.03      2.4186  0.29840  
#Spo2.F              1   15.240 119.24      6.6319  0.01002 *
#MalaiseHOP.F        1   14.311 113.83      1.2194  0.26948  
#pHCAT               1   14.295 113.73      1.1242  0.28901   

# drop 4
drop1(update(glm.full, ~ . -Coinfection.F -virus.Fb -ApneesMaison.F -Dureesymptomes), test = "LRT")

#TRe ~ Terme.F + Age.F + Poids + PremiereBronchio.F + FC.F + Spo2.F + 
#    MalaiseHOP.F + pHCAT
#                   Df Deviance    AIC scaled dev. Pr(>Chi)  
#<none>                  14.550 115.31                       
#Terme.F             1   15.356 118.00      4.6932  0.03028 *
#Age.F               2   15.777 118.36      7.0458  0.02951 *
#Poids               1   14.900 115.38      2.0682  0.15039  
#PremiereBronchio.F  1   15.061 116.31      3.0045  0.08303 .
#FC.F                2   15.041 114.20      2.8910  0.23563  
#Spo2.F              1   15.650 119.65      6.3433  0.01178 *
#MalaiseHOP.F        1   14.794 114.76      1.4512  0.22833  
#pHCAT               1   14.738 114.43      1.1210  0.28969  

# drop 5
drop1(update(glm.full, ~ . -Coinfection.F -virus.Fb -ApneesMaison.F -Dureesymptomes -pHCAT), test = "LRT")

#TRe ~ Terme.F + Age.F + Poids + PremiereBronchio.F + FC.F + Spo2.F + 
#    MalaiseHOP.F
#                   Df Deviance    AIC scaled dev.  Pr(>Chi)    
#<none>                  50.279 331.44                          
#Terme.F             1   51.287 337.02      7.5810 0.0058986 ** 
#Age.F               2   52.266 342.24     14.8054 0.0006096 ***
#Poids               1   50.314 329.70      0.2680 0.6046464    
#PremiereBronchio.F  1   50.329 329.82      0.3833 0.5358567    
#FC.F                2   50.677 330.45      3.0152 0.2214438    
#Spo2.F              1   52.262 344.22     14.7799 0.0001208 ***
#MalaiseHOP.F        1   50.349 329.97      0.5347 0.4646570   

# drop 6
drop1(update(glm.full, ~ . -Coinfection.F -virus.Fb -ApneesMaison.F -Dureesymptomes -pHCAT -Poids), test = "LRT")

#TRe ~ Terme.F + Age.F + PremiereBronchio.F + FC.F + Spo2.F + 
#    MalaiseHOP.F
#                   Df Deviance    AIC scaled dev.  Pr(>Chi)    
#<none>                  50.406 330.21                          
#Terme.F             1   51.810 338.73      10.517 0.0011830 ** 
#Age.F               2   55.377 362.23      36.019 1.509e-08 ***
#PremiereBronchio.F  1   50.453 328.57       0.353 0.5522888    
#FC.F                2   50.803 329.22       3.002 0.2229295    
#Spo2.F              1   52.297 342.32      14.107 0.0001727 ***
#MalaiseHOP.F        1   50.490 328.85       0.633 0.4262501  

# drop 7
drop1(update(glm.full, ~ . -Coinfection.F -virus.Fb -ApneesMaison.F -Dureesymptomes -pHCAT -Poids -PremiereBronchio.F), test = "LRT")

#TRe ~ Terme.F + Age.F + FC.F + Spo2.F + MalaiseHOP.F
#             Df Deviance    AIC scaled dev.  Pr(>Chi)    
#<none>            50.453 328.57                          
#Terme.F       1   51.865 337.14      10.575 0.0011465 ** 
#Age.F         2   55.907 363.88      39.316 2.901e-09 ***
#FC.F          2   50.903 327.97       3.404 0.1823120    
#Spo2.F        1   52.340 340.63      14.067 0.0001764 ***
#MalaiseHOP.F  1   50.536 327.20       0.634 0.4257141    

# drop 8
drop1(update(glm.full, ~ . -Coinfection.F -virus.Fb -ApneesMaison.F -Dureesymptomes -pHCAT -Poids -PremiereBronchio.F -MalaiseHOP.F), test = "LRT")

#Model:
#TRe ~ Terme.F + Age.F + FC.F + Spo2.F
#        Df Deviance    AIC scaled dev.  Pr(>Chi)    
#<none>       50.536 327.20                          
#Terme.F  1   51.986 336.03      10.834 0.0009967 ***
#Age.F    2   55.982 362.40      39.198 3.079e-09 ***
#FC.F     2   51.012 326.79       3.587 0.1663777    
#Spo2.F   1   52.882 342.58      17.377 3.066e-05 ***

# final model 
# modf = TRe ~ Terme.F + Age.F + FC.F + Spo2.F

# AUTOMATED SELECTION----

lrm.full <- rms::lrm(TRe ~ Terme.F + Age.F + Poids + PremiereBronchio.F + Dureesymptomes + 
                     virus.Fb + Coinfection.F + FC.F + Spo2.F + ApneesMaison.F + 
                     MalaiseHOP.F + pHCAT
                     , 
                     data = rp)

fastbw(lrm.full, rule = "p", sls = 0.5)

# Deleted            Chi-Sq d.f. P      Residual d.f. P      AIC   
# virus.Fb           0.01   1    0.9128 0.01      1   0.9128  -1.99
# Coinfection.F      0.02   1    0.8811 0.03      2   0.9830  -3.97
# ApneesMaison.F     0.09   1    0.7641 0.12      3   0.9887  -5.88
# Dureesymptomes     0.55   1    0.4565 0.68      4   0.9539  -7.32
# pHCAT              1.04   1    0.3069 1.72      5   0.8860  -8.28
# FC.F               2.25   2    0.3244 3.97      7   0.7827 -10.03
# MalaiseHOP.F       0.77   1    0.3799 4.75      8   0.7844 -11.25
# Poids              1.03   1    0.3092 5.78      9   0.7617 -12.22
# PremiereBronchio.F 1.56   1    0.2120 7.34     10   0.6932 -12.66
#
#Approximate Estimates after Deleting Factors
#
#            Coef   S.E. Wald Z        P
#Intercept  2.528 0.9288  2.722 0.006498
#Terme.F=2 -1.213 0.6914 -1.755 0.079338
#Age.F=2   -1.912 0.8373 -2.283 0.022427
#Age.F=3   -1.038 0.6094 -1.704 0.088389
#Spo2.F=2  -1.677 0.6068 -2.764 0.005705
#
#Factors in Final Model
#
#[1] Terme.F Age.F   Spo2.F   


# MODEL DIAGNOSIS----

rp$TRe01 = as.factor(if_else(rp$TRe == "2", "1", "0"))

mod1 = glm(TRe01 ~ Terme.F + Age.F + FC.F + Spo2.F, 
           data = rp, 
           family = binomial)
summary(mod1)
#Call:
#glm(formula = TRe01 ~ Terme.F + Age.F + FC.F + Spo2.F, family = binomial, 
#    data = rp)
#
#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.7160     0.5759   2.980 0.002886 ** 
#Terme.F2     -1.2341     0.3897  -3.167 0.001539 ** 
#Age.F2       -2.4215     0.4402  -5.501 3.78e-08 ***
#Age.F3       -1.1697     0.3208  -3.646 0.000267 ***
#FC.F2         0.5111     0.3074   1.663 0.096406 .  
#FC.F3         0.6648     0.4701   1.414 0.157277    
#Spo2.F2      -1.4266     0.3910  -3.649 0.000263 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#    Null deviance: 381.64  on 382  degrees of freedom
#Residual deviance: 317.29  on 376  degrees of freedom
#AIC: 331.29
#
#Number of Fisher Scoring iterations: 5
#
exp(cbind(OR = coef(mod1), confint(mod1)))

modage = glm(TRe01 ~ Terme.F + FC.F + Spo2.F, 
             data = rp, 
             family = binomial)
lrtest(mod1, modage)

modfre = glm(TRe01 ~ Terme.F + Age.F + Spo2.F, 
             data = rp, 
             family = binomial)
lrtest(mod1, modfre)


# Correlation----
performance::check_collinearity(mod1)
#Low Correlation
#
#    Term  VIF   VIF 95% CI Increased SE Tolerance Tolerance 95% CI
# Terme.F 1.07 [1.02, 1.34]         1.04      0.93     [0.75, 0.98]
#   Age.F 1.12 [1.04, 1.33]         1.06      0.89     [0.75, 0.96]
#    FC.F 1.07 [1.02, 1.34]         1.04      0.93     [0.75, 0.98]
#  Spo2.F 1.07 [1.02, 1.34]         1.04      0.93     [0.75, 0.98]

# Hosmer-Lemeshow----
glmtoolbox::hltest(mod1)

#   The Hosmer-Lemeshow goodness-of-fit test
#
# Group Size Observed  Expected
#     1   54        3  1.801773
#     2   40        1  2.261287
#     3   13        1  1.378138
#     4   79       12  8.507918
#     5   54        8  8.917400
#     6   26        4  5.063601
#     7   33        5  9.237993
#     8   45       18 16.255549
#     9   39       24 22.576342
#
#         Statistic =  6.88921 
#degrees of freedom =  7 
#           p-value =  0.44051 

# pseudo-R2----

mod.null = glm(TRe01 ~ 1, data = rp, family = binomial)

1-logLik(mod1)/logLik(mod.null)
# 'log Lik.' 0.1686141 (df=7)

# Accuracy of the full mod by bootstrapping 


performance::performance_accuracy(
  mod1, 
  method = c(#cv, CROSS VALIDATION
    "boot"),
  k = 5,
  n = 1000,
  verbose = TRUE)

# Accuracy of Model Predictions
#
#Accuracy (95% CI): 82.69% [76.97%, 87.81%]
#Method: Area under Curve

# MACHINE LEARNING----

names(rp)

xtabs(~ Terme.F + Terme.F + Age.F + FC.F + Spo2.F, data = rp)
# no zero for nominal variables 
# zero are present for quantitative variables 

# Data partition----
set.seed(21051986)
ind <- sample(2, nrow(rp), replace= T, prob = c(0.8, 0.2))
train <- rp[ind==1,]
test <- rp[ind==2,]

# Prediction----

p1 <- predict(mod1, train, type = "response")
head(p1, n = 30)

# Missclassification error on train data----

pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$TRe01)
tab1

#         Actual
#Predicted   0   1
#        0 236  44
#        1   3  12

# Missclassification error rate----
1-sum(diag(tab1))/sum(tab1)
# [1] 0.159322 -> 15.9 error rate

# Misclassification error on test data-----

p2 <- predict(mod1, test, type = "response")
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$TRe01)
tab2

#         Actual
#Predicted  0  1
#        0 66 17
#        1  2  3

# 17+2 = 19

# GOODNESS-OF-FIT of the model----
#    Null deviance: 381.64  on 381  degrees of freedom
#Residual deviance: 317.29  on 375  degrees of freedom
pvalue = 1-pchisq(381.64-317.29, df=(382-375)#, lower.tail = F #specify lower tail to 
                                                              #get extreme values or substract -1 
                                      )
# pvalue
#[1]  2.031297e-11
# => model is valid

### SHRINKED MODEL-----------------------------------------------------------------------------------

install.packages("glmnet")
require("glmnet")

rp.la <- rp

table(rp.la$Terme.F)
rp.la <- rp.la %>%
  mutate(Terme.F2 = case_when(
    Terme.F == "1" | Terme.F == "2" ~ "1",
    Terme.F == "3" | Terme.F == "4" ~ "2"
  ))
table(rp.la$Terme.F2)
rp.la$Terme.F2 = as.factor(rp.la$Terme.F2)

rp.la$pHCAT = as.factor(rp.la$pHCAT)
rp.la <- rp.la %>%
  mutate(pHCAT2 = case_when(
    pHCAT == "1" ~ "1" | pHCAT == "2" ~ "1",
    pHCAT == "3" ~ "2"
  ))
rp.la$pHCAT2 = as.factor(rp.la$pHCAT2)

rp.la2 = subset(rp.la, select = c("Age.F", "Poids", "PremiereBronchio.F", "Dureesymptomes", 
                           "virus.F", "Coinfection.F", "FC.F", "Spo2.F", "ApneesMaison.F", 
                           "MalaiseHOP.F", #"pHCAT2", 
                           "TRe01", "Terme.F2"))

# more here https://eight2late.wordpress.com/2017/07/11/a-gentle-introduction-to-logistic-regression-and-lasso-regularisation-using-r/
rp.la2[,"train"] <- ifelse(runif(nrow(rp.la2))<0.8,1,0)
trainset <- rp.la2[rp.la2$train==1,]
testset <- rp.la2[rp.la2$train==0,]

# get column index of the train flag 
trainColNum <- grep("train", names(trainset))
#remove train flag column from train and test sets
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]

#get column index of predicted variable in dataset
typeColNum <- grep("TRe01",names(rp.la2))

# build the model
glm_model <- glm(TRe01 ~., data = trainset, family = binomial)
summary(glm_model)

#predict probabilities on testset
#type="response" gives probabilities, type="class" gives class
glm_prob <- predict.glm(glm_model,testset[,-typeColNum],type= "response")

#which classes do these probabilities refer to? What are 1 and 0?
contrasts(rp.la2$TRe01)

#make predictions
## first create vector to hold predictions (we know 0 refers to neg now)
glm_predict <- rep("0",nrow(testset))
glm_predict[glm_prob>.5] <- "1"

#confusion matrix
table(pred=glm_predict,true=testset$TRe01)

#accuracy
mean(glm_predict==testset$TRe01)
# 0.7901235

# library(glmnet)

#convert training data to matrix format
trainset = na.omit(trainset)
x <- model.matrix(TRe01~.,trainset)

#convert class to numerical variable
y <- ifelse(trainset$TRe01 == "1",1,0)

#perform grid search to find optimal value of lambda
#family= binomial => logistic regression, alpha=1 => lasso
# check docs to explore other type.measure options

cv.out <- cv.glmnet(x,y,alpha = 1,family= "binomial", 
                    type.measure = "mse" )
#plot result
plot(cv.out)

#min value of lambda
lambda_min <- cv.out$lambda.min
# 0.01471419
#best value of lambda
lambda_1se <- cv.out$lambda.1se
# 0.1038057
#regression coefficients
coef(cv.out,s=lambda_1se)

#16 x 1 sparse Matrix of class "dgCMatrix"
#                           s1
#(Intercept)         -1.449269
#(Intercept)          .       
#Age.F2               .       
#Age.F3               .       
#Poids                .       
#PremiereBronchio.F1  .       
#Dureesymptomes       .       
#virus.F3             .       
#virus.F4             .       
#Coinfection.F1       .       
#FC.F2                .       
#Spo2.F2              .       
#Spo2.F3              .       
#ApneesMaison.F1      .       
#MalaiseHOP.F1        .       
#Terme.F22            .       

x_test <- model.matrix(TRe01 ~ ., testset)

# predict class, type = "class"
lasso_prob <- predict(cv.out, newx = x_test, s = lambda_1se, 
                      type = "response")
lasso_predict <- rep("neg", nrow(testset))
lasso_predict[lasso_prob>.5] <- "pos"
table(pred = lasso_predict, true = testset$TRe01)
#     true
#pred   0  1
#  neg 62 19

# accuracy 
mean(lasso_predict==testset$TRe01)
