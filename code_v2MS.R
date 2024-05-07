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

#-data loading----

reaped <- read.csv2("/Users/damianocerasuolo/Desktop/UBRC/UBRS_CONSULT_MAC/Brossier_David/StatistiquesReaPed/BronchioOHD.csv")

names(reaped)
dim(reaped)

#-var selection for prediction----

#BronchioOHD$pHCAT=cut(BronchioOHD$pH,c(7.15, 7.3, 7.38,7.5))
str(reaped$pH)

reaped <- reaped %>%
  mutate(pHCAT = case_when(
    pH <= 7.15 ~ "1",
    pH > 7.15 & pH <= 7.3 ~ "2",
    pH > 7.38 & pH <= 7.5 ~ "3",
    pH > 7.5 ~ "4"
  ))
table(reaped$pHCAT)

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
    Terme == "<28" ~ "1",
    Terme == ">37" ~ "2",
    Terme == "28-32" ~ "3",
    Terme == "32-37" ~ "4",
  ))
reaped$Terme.F = as.factor(reaped$Terme.F)

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
    virus == "autre" ~ "1", 
    virus == "ERV" ~ "2",
    virus == "non identifie" ~ "3",
    virus == "VRS" ~ "4"
  ))
reaped$virus.F = as.factor(reaped$virus.F)

# Coinfection
reaped$Coinfection.F = as.factor(if_else(reaped$Coinfection == "oui", "1", "0"))

# FC
reaped <- reaped %>%
  mutate(FC.F = case_when(
    FC == "normocarde" ~ "1",
    FC == "tachycarde>160/min" ~ "2",
    FC == "tachycarde>180/min" ~ "3"
  ))
reaped$FC.F = as.factor(reaped$FC.F)

# Spo2
reaped <- reaped %>%
  mutate(Spo2.F = case_when(
    Spo2 == "<88%" ~ "1",
    Spo2 == ">92%" ~ "2",
    Spo2 == "88-92%" ~ "3"
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
                              "Dureesymptomes", "virus.F", "Coinfection.F", "FC.F", "Spo2.F",
                              "ApneesMaison.F", "MalaiseHOP.F", "pHCAT"))

# TRe
rp$TRe = as.numeric(as.factor(rp$TRe))

#-reg log test----
glm.full = glm(TRe ~ Terme.F + Age.F + Poids + PremiereBronchio.F
                 + Dureesymptomes + virus.F + Coinfection.F
                 + FC.F + Spo2.F + ApneesMaison.F + MalaiseHOP.F
                 + pHCAT, data = rp)
glm.null = glm(TRe ~ 1, data = rp)

# LRT SELECTION----
# manual likelihood-ratio-test-based backward selection
drop1(glm.full, test = "LRT")

# Single term deletions
#
#Model:
#  TRe ~ Terme.F + Age.F + Poids + PremiereBronchio.F + Dureesymptomes + 
#  virus.F + Coinfection.F + FC.F + Spo2.F + ApneesMaison.F + 
#  MalaiseHOP.F + pHCAT
#Df Deviance    AIC scaled dev. Pr(>Chi)   
#<none>                  3.4884 52.137                        
#Terme.F             1   3.6983 52.240      2.1033 0.146978   
#Age.F               2   4.1227 54.151      6.0143 0.049432 * 
#Poids               1   3.6026 51.296      1.1592 0.281622   
#PremiereBronchio.F  1   3.6438 51.706      1.5686 0.210414   
#Dureesymptomes      1   4.3106 57.756      7.6186 0.005777 **
#virus.F             2   3.5356 48.621      0.4837 0.785174   
#Coinfection.F       1   3.7654 52.888      2.7505 0.097227 . 
#FC.F                1   3.7512 52.752      2.6147 0.105878   
#Spo2.F              2   4.0455 53.471      5.3334 0.069483 . 
#ApneesMaison.F      1   3.6562 51.828      1.6912 0.193440   
#MalaiseHOP.F        1   3.4892 50.145      0.0083 0.927505   
#pHCAT               1   3.5010 50.267      0.1297 0.718713  

# Coinfection.F est le moins significatif dans le model (p-value)

# drop Coinfection.F
drop1(update(glm.full, ~ . -Coinfection.F), test = "LRT")

#                  Df Deviance    AIC scaled dev. Pr(>Chi)   
#<none>                  3.7654 52.888                        
#Terme.F             1   4.2443 55.198      4.3102 0.037884 * 
#Age.F               2   4.8322 57.868      8.9804 0.011218 * 
#Poids               1   3.8470 51.659      0.7717 0.379702   
#PremiereBronchio.F  1   3.8826 51.991      1.1038 0.293424   
#Dureesymptomes      1   4.5301 57.543      6.6558 0.009884 **
#virus.F             2   3.8112 49.323      0.4351 0.804470   
#FC.F                1   4.0215 53.257      2.3692 0.123753   
#Spo2.F              2   4.2601 53.331      4.4437 0.108407   
#ApneesMaison.F      1   4.0225 53.266      2.3779 0.123060   
#MalaiseHOP.F        1   3.7730 50.961      0.0731 0.786903   
#pHCAT               1   3.7701 50.933      0.0449 0.832106   

# drop pHCAT
drop1(update(glm.full, ~ . -pHCAT -Coinfection.F), test = "LRT")

#                   Df Deviance    AIC scaled dev.  Pr(>Chi)    
#<none>                  41.032 281.78                          
#Terme.F             3   42.237 285.42      9.6335 0.0219529 *  
#Age.F               2   42.225 287.32      9.5368 0.0084938 ** 
#Poids               1   41.057 279.98      0.1995 0.6550886    
#PremiereBronchio.F  1   41.132 280.59      0.8042 0.3698548    
#Dureesymptomes      1   42.211 289.21      9.4310 0.0021334 ** 
#virus.F             2   41.738 283.46      5.6781 0.0584799 .  
#FC.F                1   41.337 282.25      2.4662 0.1163219    
#Spo2.F              2   43.009 293.45     15.6701 0.0003956 ***
#ApneesMaison.F      1   41.108 280.39      0.6093 0.4350636    
#MalaiseHOP.F        1   41.094 280.28      0.4992 0.4798443    

# drop Poids
drop1(update(glm.full, ~ . -pHCAT -Coinfection.F - Poids), test = "LRT")

#                   Df Deviance    AIC scaled dev.  Pr(>Chi)    
#<none>                  41.151 280.49                          
#Terme.F             3   42.664 286.55     12.0602 0.0071799 ** 
#Age.F               2   44.051 299.24     22.7467  1.15e-05 ***
#PremiereBronchio.F  1   41.244 279.25      0.7559 0.3846031    
#Dureesymptomes      1   42.311 287.78      9.2848 0.0023106 ** 
#virus.F             2   41.835 282.00      5.5090 0.0636421 .  
#FC.F                1   41.456 280.96      2.4679 0.1161940    
#Spo2.F              2   43.076 291.76     15.2690 0.0004835 ***
#ApneesMaison.F      1   41.231 279.14      0.6497 0.4202163    
#MalaiseHOP.F        1   41.217 279.03      0.5362 0.4640127 

# drop ApneesMaison.F
drop1(update(glm.full, ~ . -pHCAT -Coinfection.F - Poids -ApneesMaison.F), test = "LRT")

#                   Df Deviance    AIC scaled dev.  Pr(>Chi)    
#<none>                  41.231 279.14                          
#Terme.F             3   42.772 285.40     12.2575 0.0065513 ** 
#Age.F               2   44.413 299.97     24.8308 4.056e-06 ***
#PremiereBronchio.F  1   41.312 277.80      0.6550 0.4183377    
#Dureesymptomes      1   42.392 286.42      9.2770 0.0023205 ** 
#virus.F             2   41.950 280.91      5.7722 0.0557945 .  
#FC.F                1   41.527 279.53      2.3853 0.1224809    
#Spo2.F              2   43.098 289.93     14.7915 0.0006138 ***
#MalaiseHOP.F        1   41.310 277.78      0.6382 0.4243622    

# drop MalaiseHOP.F 
drop1(update(glm.full, ~ . -pHCAT -Coinfection.F - Poids -ApneesMaison.F -MalaiseHOP.F), test = "LRT")

#                   Df Deviance    AIC scaled dev.  Pr(>Chi)    
#<none>                  41.310 277.78                          
#Terme.F             3   42.919 284.54     12.7601  0.005185 ** 
#Age.F               2   44.489 298.54     24.7651 4.191e-06 ***
#PremiereBronchio.F  1   41.392 276.44      0.6628  0.415559    
#Dureesymptomes      1   42.483 285.13      9.3483  0.002232 ** 
#virus.F             2   42.026 279.52      5.7374  0.056774 .  
#FC.F                1   41.614 278.23      2.4524  0.117348    
#Spo2.F              2   43.726 292.76     18.9849 7.542e-05 ***  

# drop PremiereBronchio.F
drop1(update(glm.full, ~ . -pHCAT -Coinfection.F - Poids -ApneesMaison.F -MalaiseHOP.F -PremiereBronchio.F), test = "LRT")

#               Df Deviance    AIC scaled dev.  Pr(>Chi)    
#<none>              41.392 276.44                          
#Terme.F         3   43.020 283.32     12.8811  0.004901 ** 
#Age.F           2   45.098 301.08     28.6372 6.047e-07 ***
#Dureesymptomes  1   42.526 283.47      9.0242  0.002664 ** 
#virus.F         2   42.126 278.31      5.8711  0.053101 .  
#FC.F            1   41.753 277.35      2.9028  0.088424 .  
#Spo2.F          2   43.821 291.49     19.0433 7.325e-05 ***

# final model 
# modf = TRe ~ Terme.F + Age.F + Dureesymptomes + virus.F + FC.F + Spo2.F

# AUTOMATED SELECTION----

lrm.full <- rms::lrm(TRe ~ Terme.F + Age.F + Poids + PremiereBronchio.F + Dureesymptomes + 
                     virus.F + Coinfection.F + FC.F + Spo2.F + ApneesMaison.F + 
                     MalaiseHOP.F #+ pHCAT
                     , 
                     data = rp)

fastbw(lrm.full, rule = "p", sls = 0.1)

# Deleted            Chi-Sq d.f. P      Residual d.f. P      AIC  
# Poids              0.10   1    0.7524  0.10    1    0.7524 -1.90
# MalaiseHOP.F       0.37   1    0.5406  0.47    2    0.7890 -3.53
# ApneesMaison.F     0.59   1    0.4430  1.06    3    0.7862 -4.94
# PremiereBronchio.F 0.66   1    0.4161  1.72    4    0.7864 -6.28
# Coinfection.F      2.27   1    0.1316  4.00    5    0.5498 -6.00
# FC.F               2.43   1    0.1187  6.43    6    0.3767 -5.57
# virus.F            5.48   2    0.0645 11.91    8    0.1552 -4.09
#
#Approximate Estimates after Deleting Factors
#
#                  Coef    S.E.  Wald Z         P
#Intercept       4.9285  1.8124  2.7194 6.540e-03
#Terme.F=2      -1.1550  1.1715 -0.9859 3.242e-01
#Terme.F=3      -6.1640 27.1502 -0.2270 8.204e-01
#Terme.F=4       0.1930  1.2233  0.1578 8.747e-01
#Age.F=2        -2.1594  0.5132 -4.2074 2.583e-05
#Age.F=3        -0.9953  0.3599 -2.7651 5.690e-03
#Dureesymptomes -0.2693  0.1059 -2.5432 1.098e-02
#Spo2.F=2       -3.5995  1.2196 -2.9514 3.163e-03
#Spo2.F=3       -2.3544  1.2986 -1.8131 6.982e-02

#Factors in Final Model

#[1] Terme.F        Age.F          Dureesymptomes Spo2.F   


# MODEL DIAGNOSIS----

rp$TRe01 = as.factor(if_else(rp$TRe == "2", "1", "0"))

mod1 = glm(TRe01 ~ Terme.F + Age.F + Dureesymptomes + virus.F + FC.F + Spo2.F, 
           data = rp, 
           family = binomial)
summary(mod1)
#Call:
#glm(formula = TRe01 ~ Terme.F + Age.F + Dureesymptomes + virus.F + 
#    FC.F + Spo2.F, family = binomial, data = rp)
#
#Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      5.3730     1.9075   2.817  0.00485 ** 
#Terme.F2        -2.0740     1.2047  -1.722  0.08514 .  
#Terme.F3       -14.1562   831.5752  -0.017  0.98642    
#Terme.F4        -0.4896     1.2437  -0.394  0.69384    
#Age.F2          -2.4227     0.5203  -4.656 3.22e-06 ***
#Age.F3          -1.1386     0.3705  -3.073  0.00212 ** 
#Dureesymptomes  -0.3086     0.1053  -2.930  0.00339 ** 
#virus.F3        -1.0070     0.8747  -1.151  0.24958    
#virus.F4         0.4919     0.6399   0.769  0.44207    
#FC.F2            0.5240     0.3256   1.610  0.10749    
#Spo2.F2         -3.5718     1.2401  -2.880  0.00397 ** 
#Spo2.F3         -2.4130     1.3180  -1.831  0.06713 .  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#    Null deviance: 332.04  on 333  degrees of freedom
#Residual deviance: 255.15  on 322  degrees of freedom
#  (49 observations deleted due to missingness)
#AIC: 279.15
#
#Number of Fisher Scoring iterations: 14


# Correlation----
performance::check_collinearity(mod1)
#Low Correlation
#
#           Term  VIF   VIF 95% CI Increased SE Tolerance Tolerance 95% CI
#        Terme.F 1.27 [1.15, 1.49]         1.13      0.78     [0.67, 0.87]
#          Age.F 1.27 [1.15, 1.48]         1.13      0.79     [0.68, 0.87]
# Dureesymptomes 1.04 [1.00, 1.62]         1.02      0.96     [0.62, 1.00]
#        virus.F 1.23 [1.12, 1.44]         1.11      0.81     [0.69, 0.89]
#           FC.F 1.06 [1.01, 1.40]         1.03      0.94     [0.71, 0.99]
#         Spo2.F 1.15 [1.06, 1.36]         1.07      0.87     [0.73, 0.94]

# Hosmer-Lemeshow----
glmtoolbox::hltest(mod1)

#   The Hosmer-Lemeshow goodness-of-fit test
#
# Group Size Observed   Expected
#     1   32        0  0.3517837
#     2   30        1  0.9139926
#     3   34        2  1.6328151
#     4   35        1  2.7572585
#     5   26        4  2.9842098
#     6   31        7  4.3162760
#     7   27        4  4.8484436
#     8   33        8  7.2836648
#     9   33        9 10.6241061
#    10   33       15 15.3554907
#    11   20       15 14.9319596
#
#         Statistic =  4.6498 
#degrees of freedom =  9 
#           p-value =  0.86371 

# pseudo-R2----

mod.null = glm(TRe01 ~ 1, data = rp, family = binomial)

1-logLik(mod1)/logLik(mod.null)
# 'log Lik.' 0.3314434 (df=12)

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

xtabs(~ Terme.F + Terme.F + Age.F + Dureesymptomes + virus.F + FC.F + Spo2.F, data = rp)
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

#          1           2           3           4           5           7           8          11          12          15 
#0.018679428 0.136425661 0.034083555 0.163845309 0.038323547 0.056210015          NA 0.189808686          NA 0.361350399 
#         16          17          18          19          20          21          22          23          24          25 
#         NA 0.045319638 0.190529086 0.009262727 0.210604772          NA 0.049527162 0.565402534 0.175889081          NA 
#         27          28          29          30          32          33          34          35          37          38 
#0.068791726          NA 0.099239306          NA 0.021972316 0.750519792          NA 0.210604772 0.151130453          NA 

# Missclassification error on train data----

pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$TRe01)
tab1

#         Actual
#Predicted   0   1
#        0 199  35
#        1   6  13

# Missclassification error rate----
1-sum(diag(tab1))/sum(tab1)
# [1] 0.1620553 -> 16.2% error rate

# Misclassification error on test data-----

p2 <- predict(mod1, test, type = "response")
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$TRe01)
tab2

#         Actual
#Predicted  0  1
#        0 59 12
#        1  4  6

# 4+12 = 16

# GOODNESS-OF-FIT of the model----
#    Null deviance: 332.04  on 341  degrees of freedom
#Residual deviance: 255.15  on 331  degrees of freedom
pvalue = 1-pchisq(332.04-255.15, df=(333-322)#, lower.tail = F #specify lower tail to 
                                                              #get extreme values or substract -1 
                                      )
# pvalue
#[1] 5.87419e-12
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
