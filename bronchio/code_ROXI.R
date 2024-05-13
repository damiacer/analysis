# packages-----

require("MASS")

# var recoding----

table(reaped$OHD3L, useNA = "always")

reaped <- reaped %>%
  mutate(OHb = case_when(
    OHD3L == "non" ~ "0",
    OHD3L == "OUI" ~ "1"
  ))
table(reaped$OHb, useNA = "always")

#-DATA----

re2 = subset(reaped, select = c("OHb", "FR.1", "Spo2b", "SDLb", "WangB", 
                                "Woodb", "apneeb", "AEGb", "malaiseb", "ROXI"))

#-VARIABLE RECODING---

#OHb
re2$OHb = as.factor(re2$OHb)

#FR.1
table(re2$FR.1)
re2 <- re2 %>%
  mutate(FR = case_when(
    FR.1 == "eupneique" ~ "1", #30-60 ?
    FR.1 == "bradypnee" ~ "2",
    FR.1 == "<60/min" ~ "3",
    FR.1 == "60-70/min" ~ "4",
    FR.1 == ">70/min" ~ "5",
    FR.1 == "60-70/min" ~ "6"
  ))
table(re2$FR)
#  1   2   3   4   5 
#114   8 147  78  36 
re2$FR = as.factor(re2$FR)

#Spo2b 

table(re2$Spo2b, useNA = "always")
re2 <- re2 %>%
  mutate(SpO2 = case_when(
    Spo2b == "<88%" ~ "1", #<88
    Spo2b == "88-92%" ~ "1", #88-92
    Spo2b == ">92%" ~ "2" #>92
  ))
table(re2$SpO2)
(prop.table(table(re2$SpO2)))*100
re2$SpO2 = as.factor(re2$SpO2)

#SDLb
table(re2$SDLb)
re2 <- re2 %>%
  mutate(SDL = case_when(
    SDLb == "absent" ~ "0",
    SDLb == "leger" ~ "1",
    SDLb == "modere" ~ "1",
    SDLb == "intense" ~ "2"
  ))
table(re2$SDL)
re2$SDL = as.factor(re2$SDL)

#WangB
table(re2$WangB)
re2 <- re2 %>%
  mutate(Wang = case_when(
    WangB == "0-3 sans gravite" ~ "0",
    WangB == "4-7 moderee" ~ "1",
    WangB == "8-12 severe" ~ "2"
  ))
table(re2$Wang) 
re2$Wang = as.factor(re2$Wang)

#Woodb
table(re2$Woodb)
re2 <- re2 %>%
  mutate(Wood = case_when(
    Woodb == ">=3" ~ "2",
    Woodb == "0-2,5" ~ "1"
  ))
table(re2$Wood, useNA = "always")
re2$Wood = as.factor(re2$Wood)

#apneeb 
table(re2$apneeb, useNA = "always")
re2$apnee <- as.factor(if_else(re2$apneeb == "oui", "1", "2"))

#AEGb
table(re2$AEGb)
re2$AEG <- as.factor(if_else(re2$AEGb == "oui", "1", "0"))

#malaiseb
table(re2$malaiseb)
re2$malaise = as.factor(if_else(re2$malaiseb == "oui", "1", "2"))

#ROXI
table(re2$ROXI)
str(re2$ROXI)

# new dataset----
re2s = subset(re2, select = c("OHb", "FR", "SpO2", "SDL", 
                                "Wang", "Wood", "apnee", "AEG", "malaise",
                              "ROXI"))

# reg log test----
glm.fulloh = glm(as.numeric(OHb) ~ FR + SpO2 + SDL + Wang + Wood + apnee + AEG + malaise
               + ROXI,
               data = re2s)
glm.nulloh = glm(as.numeric(OHb) ~ 1, data = re2s)

# step function----

sglm1 <- step(glm.fulloh)
summary(sglm1)

# LRT SELECTION----
# manual likelihood-ratio-test-based backward selection
drop1(glm.fulloh, test = "LRT")

#        Df Deviance     AIC scaled dev. Pr(>Chi)
#<none>       5.5216 -306.57                     
#FR       4   5.5600 -312.52     2.04711   0.7271
#SpO2     1   5.5232 -308.48     0.08536   0.7702
#SDL      2   5.5785 -307.53     3.03404   0.2194
#Wang     2   5.5804 -307.44     3.13134   0.2089
#Wood     1   5.5355 -307.82     0.74298   0.3887
#apnee    1   5.5401 -307.58     0.98772   0.3203
#AEG      1   5.5261 -308.33     0.23982   0.6243
#malaise  1   5.5387 -307.65     0.91349   0.3392
#ROXI     1   5.5465 -307.24     1.32865   0.2490

# drop1 
drop1(update(glm.fulloh, ~ . -SpO2), test = "LRT")
#        Df Deviance     AIC scaled dev. Pr(>Chi)
#<none>       5.5232 -308.48                     
#FR       4   5.5600 -314.52     1.96181   0.7428
#SDL      2   5.5792 -309.50     2.98259   0.2251
#Wang     2   5.5805 -309.43     3.05526   0.2170
#Wood     1   5.5386 -309.66     0.82310   0.3643
#apnee    1   5.5418 -309.49     0.99475   0.3186
#AEG      1   5.5278 -310.24     0.24446   0.6210
#malaise  1   5.5399 -309.59     0.89293   0.3447
#ROXI     1   5.5504 -309.03     1.45250   0.2281

# drop2
drop1(update(glm.fulloh, ~ . -FR -SpO2), test = "LRT")
#        Df Deviance     AIC scaled dev. Pr(>Chi)  
#<none>       5.5600 -314.52                       
#SDL      2   5.6282 -314.91      3.6105  0.16444  
#Wang     2   5.6202 -315.33      3.1887  0.20304  
#Wood     1   5.5746 -315.74      0.7783  0.37766  
#apnee    1   5.5752 -315.71      0.8124  0.36741  
#AEG      1   5.5648 -316.26      0.2588  0.61093  
#malaise  1   5.5806 -315.42      1.0951  0.29535  
#ROXI     1   5.6445 -312.05      4.4673  0.03455 *

# drop3
drop1(update(glm.fulloh, ~ . -FR -SpO2 -AEG), test = "LRT")
#        Df Deviance     AIC scaled dev. Pr(>Chi)  
#<none>       5.5648 -316.26                       
#SDL      2   5.6337 -316.62      3.6395  0.16207  
#Wang     2   5.6225 -317.21      3.0534  0.21725  
#Wood     1   5.5787 -317.53      0.7350  0.39126  
#apnee    1   5.5828 -317.31      0.9546  0.32855  
#malaise  1   5.5827 -317.31      0.9501  0.32969  
#ROXI     1   5.6491 -313.81      4.4512  0.03488 *

# drop4
drop1(update(glm.fulloh, ~ . -FR -SpO2 -AEG -Wood), test = "LRT")
#        Df Deviance     AIC scaled dev. Pr(>Chi)  
#<none>       5.5787 -317.53                       
#SDL      2   5.6363 -318.48      3.0451  0.21815  
#Wang     2   5.6285 -318.89      2.6321  0.26819  
#apnee    1   5.5952 -318.65      0.8788  0.34853  
#malaise  1   5.5954 -318.64      0.8847  0.34693  
#ROXI     1   5.6652 -314.97      4.5579  0.03277 *

# drop5
drop1(update(glm.fulloh, ~ . -FR -SpO2 -AEG -Wood -apnee), test = "LRT")
#        Df Deviance     AIC scaled dev. Pr(>Chi)  
#<none>       5.5952 -318.65                       
#SDL      2   5.6420 -320.18      2.4622  0.29197  
#Wang     2   5.6523 -319.64      3.0045  0.22263  
#malaise  1   5.6065 -320.05      0.5922  0.44155  
#ROXI     1   5.6676 -316.85      3.8008  0.05123 .

# drop6
drop1(update(glm.fulloh, ~ . -FR -SpO2 -AEG -Wood -apnee -malaise), test = "LRT")
#       Df Deviance     AIC scaled dev. Pr(>Chi)  
#<none>      5.6065 -320.05                       
#SDL     2   5.6639 -321.04      3.0150  0.22147  
#Wang    2   5.6819 -320.10      3.9565  0.13831  
#ROXI    1   5.6831 -318.03      4.0206  0.04495 *

# AUTOMATED SELECTION----

lrm.fulloh <- rms::lrm(OHb ~ FR + SpO2 + SDL + Wang + Wood + apnee + AEG + malaise
                     + ROXI, data = re2s)
fastbw(lrm.fulloh, rule = "p", sls = 0.1)

# MODEL AND MODEL PERFORMANCE----
# MODEL----

table(re2s$OHb)

modoh1 = glm(OHb ~ SDL + Wang + ROXI, family = binomial, data = re2s)
summary(modoh1)
exp(cbind(OR = coef(modoh1), confint(modoh1)))

modoh1SDL = glm(OHb ~ Wang + ROXI, family = binomial, data = re2s)
lrtest(modoh1, modoh1SDL)
modoh1W = glm(OHb ~ SDL + ROXI, family = binomial, data = re2s)
lrtest(modoh1, modoh1W)


#Call:
#glm(formula = OHb ~ SDL + Wang + ROXI, family = binomial, data = re2s)
#
#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)   
#(Intercept)  -4.4591     1.6696  -2.671  0.00757 **
#SDL1         -1.4446     1.3476  -1.072  0.28374   
#SDL2          0.4461     1.9363   0.230  0.81779   
#Wang1        -1.8445     1.8008  -1.024  0.30572   
#Wang2         0.3462     2.0172   0.172  0.86372   
#ROXI          0.1401     0.0861   1.628  0.10362   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#    Null deviance: 58.661  on 295  degrees of freedom
#Residual deviance: 47.843  on 290  degrees of freedom
#  (87 observations deleted due to missingness)
#AIC: 59.843
#
#Number of Fisher Scoring iterations: 8
#
#
#                    OR        2.5 %      97.5 %
#(Intercept) 0.01157297 0.0003177449   0.2424934
#SDL1        0.23584414 0.0094876905   3.2172178
#SDL2        1.56221978 0.0278244133  55.8239252
#Wang1       0.15810953 0.0033325281   3.7631966
#Wang2       1.41373867 0.0376294035 111.0583767
#ROXI        1.15042432 0.9561932138   1.3579739

# Correlation----
performance::check_collinearity(modoh1)

#Low Correlation
#
# Term  VIF   VIF 95% CI Increased SE Tolerance Tolerance 95% CI
#  SDL 4.70 [3.88, 5.75]         2.17      0.21     [0.17, 0.26]
# Wang 4.65 [3.84, 5.69]         2.16      0.22     [0.18, 0.26]
# ROXI 1.78 [1.54, 2.12]         1.33      0.56     [0.47, 0.65]

# Hosmer-Lemeshow----
glmtoolbox::hltest(modoh1)

#   The Hosmer-Lemeshow goodness-of-fit test
#
# Group Size Observed   Expected
#     1   30        0 0.03140408
#     2   30        0 0.03951214
#     3   31        0 0.05514915
#     4   30        0 0.15446050
#     5   31        0 0.23382698
#     6   31        1 0.28759740
#     7   31        0 0.37019827
#     8   30        1 0.68538961
#     9   31        2 1.68159338
#    10   21        2 2.46086849

#         Statistic =  2.9823 
#degrees of freedom =  8 
#           p-value =  0.93546 

# pseudo-R2----

mod.null = glm(OHb ~ 1, data = re2s, family = binomial)

1-logLik(modoh1)/logLik(mod.null)
# 'log Lik.' 0.3838226 (df=6)

# Accuracy of the full mod by bootstrapping 


performance::performance_accuracy(
  modoh1, 
  method = c(#cv, CROSS VALIDATION
    "boot"),
  k = 5,
  n = 1000,
  verbose = TRUE)

#Accuracy (95% CI): 90.18% [81.30%, 98.46%]
#          Method: Area under Curve

# MACHINE LEARNING----

xtabs(~ SDL + Wang + ROXI, data = re2s)
# no zero for nominal variables 
# zero are present for quantitative variables 

# Data partition----
set.seed(21051986)
ind <- sample(2, nrow(re2s), replace= T, prob = c(0.8, 0.2))
train <- re2s[ind==1,]
test <- re2s[ind==2,]

# Prediction----

p1 <- predict(modoh1, train, type = "response")
head(p1, n = 30)

# Missclassification error on train data----

pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$OHb)
tab1

#         Actual
#Predicted   0   1
#        0 225   6

# Missclassification error rate----
1-sum(diag(tab1))/sum(tab1)
# [1] 0.02597403 -> 2.6 error rate

# Misclassification error on test data-----

p2 <- predict(modoh1, test, type = "response")
pred2 <- ifelse(p2>0.1, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$OHb)
tab2

#         Actual
#Predicted  0  1
#        0 63  0
#        1  2  0

# 2 misclassifications

# GOODNESS-OF-FIT of the model----
#    Null deviance: 58.661  on 295  degrees of freedom
#Residual deviance: 47.843  on 375  degrees of freedom
pvalue = 1-pchisq(58.661-47.843, df=(295-290)#, lower.tail = F #specify lower tail to 
                  #get extreme values or substract -1 
)
# pvalue
#[1] 0.05511033
# => model is ALMOST valid
