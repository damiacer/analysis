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

re2 = subset(reaped, select = c("VNI", "FR.1", "Spo2b", "SDLb", "WangB", 
                                "Woodb", "apneeb", "AEGb", "malaiseb", "ROXI"))

#-VARIABLE RECODING---

#VNI
re2$VNI = as.factor(if_else(re2$VNI == "Oui", "1", "0"))

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
re2s = subset(re2, select = c("VNI", "FR", "SpO2", "SDL", 
                                "Wang", "Wood", "apnee", "AEG", "malaise",
                              "ROXI"))

# reg log test----
glm.fullv = glm(as.numeric(VNI) ~ FR + SpO2 + SDL + Wang + Wood + apnee + AEG + malaise
               + ROXI,
               data = re2s)
glm.nullv = glm(as.numeric(VNI) ~ 1, data = re2s)

# step function----
#
#sglm1 <- step(glm.fulloh)
#summary(sglm1)
#
# LRT SELECTION----
# manual likelihood-ratio-test-based backward selection
drop1(glm.fullv, test = "LRT")

#        Df Deviance    AIC scaled dev. Pr(>Chi)   
#<none>       39.768 277.85                        
#FR       4   40.672 276.51      6.6561 0.155215   
#SpO2     1   39.889 276.75      0.9007 0.342586   
#SDL      2   40.122 276.47      2.6224 0.269502   
#Wang     2   39.887 274.73      0.8816 0.643525   
#Wood     1   40.171 278.84      2.9859 0.083994 . 
#apnee    1   40.136 278.58      2.7281 0.098600 . 
#AEG      1   39.900 276.83      0.9806 0.322054   
#malaise  1   40.691 282.64      6.7942 0.009146 **
#ROXI     1   40.568 281.74      5.8931 0.015201 * 

# drop1 
drop1(update(glm.fullv, ~ . -Wang), test = "LRT")
#         Df Deviance    AIC scaled dev. Pr(>Chi)   
#<none>       39.887 274.73                        
#FR       4   40.763 273.17      6.4349 0.168938   
#SpO2     1   40.017 273.70      0.9671 0.325407   
#SDL      2   40.149 272.68      1.9432 0.378474   
#Wood     1   40.219 275.19      2.4564 0.117043   
#apnee    1   40.238 275.33      2.5997 0.106885   
#AEG      1   40.006 273.62      0.8850 0.346833   
#malaise  1   40.868 279.93      7.1939 0.007315 **
#ROXI     1   40.636 278.24      5.5106 0.018901 * 

# drop2
drop1(update(glm.fullv, ~ . -Wang -AEG), test = "LRT")
#        Df Deviance    AIC scaled dev. Pr(>Chi)   
#<none>       40.006 273.62                        
#FR       4   40.856 271.84      6.2245 0.182997   
#SpO2     1   40.130 272.53      0.9140 0.339059   
#SDL      2   40.271 271.57      1.9555 0.376148   
#Wood     1   40.327 273.99      2.3677 0.123872   
#apnee    1   40.414 274.62      3.0027 0.083125 . 
#malaise  1   41.174 280.13      8.5173 0.003518 **
#ROXI     1   40.740 277.00      5.3840 0.020322 * 

# drop3
drop1(update(glm.fullv, ~ . -Wang -AEG -SDL), test = "LRT")
#        Df Deviance    AIC scaled dev. Pr(>Chi)   
#<none>       40.339 271.92                        
#FR       4   41.084 269.35      5.4333 0.245658   
#SpO2     1   40.520 271.24      1.3264 0.249450   
#Wood     1   40.898 274.00      4.0838 0.043296 * 
#apnee    1   40.647 272.17      2.2568 0.133028   
#malaise  1   41.474 278.15      8.2398 0.004098 **
#ROXI     1   41.051 275.11      5.1928 0.022681 * 

# drop4
drop1(update(glm.fullv, ~ . -Wang -AEG -SDL -SpO2), test = "LRT")
#        Df Deviance    AIC scaled dev. Pr(>Chi)   
#<none>       40.520 271.24                        
#FR       4   41.127 267.66      4.4216 0.351949   
#Wood     1   41.000 272.74      3.5018 0.061304 . 
#apnee    1   40.823 271.45      2.2135 0.136807   
#malaise  1   41.588 276.97      7.7299 0.005431 **
#ROXI     1   41.145 273.79      4.5499 0.032920 * 

# drop5
drop1(update(glm.fullv, ~ . -Wang -AEG -SDL -SpO2 -FR), test = "LRT")
#        Df Deviance    AIC scaled dev. Pr(>Chi)   
#<none>       41.127 267.66                        
#Wood     1   41.638 269.33      3.6651 0.055562 . 
#apnee    1   41.785 270.38      4.7142 0.029915 * 
#malaise  1   42.627 276.30     10.6392 0.001107 **
#ROXI     1   41.574 268.87      3.2062 0.073359 . 


# AUTOMATED SELECTION----

lrm.fullv <- rms::lrm(VNI ~ FR + SpO2 + SDL + Wang + Wood + apnee + AEG + malaise
                     + ROXI, data = re2s)
fastbw(lrm.fullv, rule = "p", sls = 0.5)

# MODEL AND MODEL PERFORMANCE----
# MODEL----

table(re2s$OHb)

modv1 = glm(VNI ~ Wood + apnee + malaise + ROXI, family = binomial, data = re2s)
summary(modv1)
exp(cbind(OR = coef(modv1), confint(modv1)))

#Call:
#glm(formula = VNI ~ Wood + apnee + malaise + ROXI, family = binomial, 
#    data = re2s)
#
#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)   
#(Intercept)  0.76252    0.76646   0.995  0.31980   
#Wood2        0.63603    0.33596   1.893  0.05833 . 
#apnee2      -0.97886    0.46607  -2.100  0.03571 * 
#malaise2    -1.17437    0.41728  -2.814  0.00489 **
#ROXI        -0.07511    0.04683  -1.604  0.10872   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#    Null deviance: 287.57  on 296  degrees of freedom
#Residual deviance: 262.53  on 292  degrees of freedom
#  (86 observations deleted due to missingness)
#AIC: 272.53
#
#Number of Fisher Scoring iterations: 4
#
#                   OR     2.5 %    97.5 %
#(Intercept) 2.1436821 0.4771929 9.8076622
#Wood2       1.8889759 0.9753864 3.6617169
#apnee2      0.3757393 0.1521984 0.9617403
#malaise2    0.3090132 0.1365927 0.7084148
#ROXI        0.9276375 0.8415192 1.0136974


# Correlation----
performance::check_collinearity(modv1)

#Low Correlation
#
#    Term  VIF   VIF 95% CI Increased SE Tolerance Tolerance 95% CI
#    Wood 1.16 [1.06, 1.40]         1.08      0.86     [0.72, 0.94]
#   apnee 1.16 [1.06, 1.40]         1.07      0.87     [0.72, 0.94]
# malaise 1.07 [1.01, 1.42]         1.04      0.93     [0.70, 0.99]
#    ROXI 1.23 [1.11, 1.46]         1.11      0.81     [0.68, 0.90]

# Hosmer-Lemeshow----
glmtoolbox::hltest(modv1)

#   The Hosmer-Lemeshow goodness-of-fit test
#
# Group Size Observed  Expected
#     1   32        3  2.659418
#     2   30        5  3.025879
#     3   30        2  3.299188
#     4   30        2  3.540322
#     5   30        3  3.877641
#     6   30        3  4.957344
#     7   30        6  6.338880
#     8   30       10  6.834480
#     9   30       11  8.379605
#    10   25       11 13.087242
#
#         Statistic =  7.7259 
#degrees of freedom =  8 
#           p-value =  0.46069 


# pseudo-R2----

mod.null = glm(VNI ~ 1, data = re2s, family = binomial)

1-logLik(modv1)/logLik(mod.null)
# 'log Lik.' 0.2963833 (df=5)

# Accuracy of the full mod by bootstrapping 


performance::performance_accuracy(
  modv1, 
  method = c(#cv, CROSS VALIDATION
    "boot"),
  k = 5,
  n = 1000,
  verbose = TRUE)

#Accuracy (95% CI): 71.95% [64.36%, 79.55%]
#Method: Area under Curve

# MACHINE LEARNING----

xtabs(~ Wood + apnee + malaise + ROXI, data = re2s)
# no zero for nominal variables 
# zero are present for quantitative variables 

# Data partition----
set.seed(21051986)
ind <- sample(2, nrow(re2s), replace= T, prob = c(0.8, 0.2))
train <- re2s[ind==1,]
test <- re2s[ind==2,]

# Prediction----

p1 <- predict(modv1, train, type = "response")
head(p1, n = 30)

# Missclassification error on train data----

pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$VNI)
tab1

#         Actual
#Predicted   0   1
#        0 185  36
#        1   5   6

# Missclassification error rate----
1-sum(diag(tab1))/sum(tab1)
# [1] 0.1767241 -> 17.67 misclassification rate

# Misclassification error on test data-----

p2 <- predict(modv1, test, type = "response")
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$VNI)
tab2

#         Actual
#Predicted  0  1
#        0 50 11
#        1  1  3

# 2 misclassifications

# GOODNESS-OF-FIT of the model----
#    Null deviance: 287.57  on 296 degrees of freedom
#Residual deviance: 262.53  on 292  degrees of freedom
pvalue = 1-pchisq(287.57-262.53, df=(296-292)#, lower.tail = F #specify lower tail to 
                  #get extreme values or substract -1 
)
# pvalue
#[1] 4.938667e-05
# => model is valid
