### FULL MODEL
fullmodel <- glm(compl.s ~ atcd_ethylisme.2 + ttt_betabloquant + ttt_diuretique + creatinine_micromolL + albumine_gL +
              duree_CEC + min_clampageAortique + EUROSCORE_II + degree_urgenceChir3, 
                 data = lg, family = binomial)

performance::check_collinearity(fit)
# Check for Multicollinearity
#
#Low Correlation
#
#                 Term  VIF Increased SE Tolerance
#     atcd_ethylisme.2 1.03         1.02      0.97
#     ttt_betabloquant 1.18         1.09      0.85
#       ttt_diuretique 1.14         1.07      0.87
# creatinine_micromolL 1.12         1.06      0.89
#          albumine_gL 1.20         1.10      0.83
#         EUROSCORE_II 1.20         1.09      0.83
#  degree_urgenceChir3 1.34         1.16      0.75
#
#Moderate Correlation
#
#                 Term  VIF Increased SE Tolerance
#            duree_CEC 7.80         2.79      0.13
# min_clampageAortique 7.66         2.77      0.13

glmtoolbox::hltest(fit)

#   The Hosmer-Lemeshow goodness-of-fit test
#
# Group Size Observed Expected
#     1   20        5    0.498
#     2   20       10    0.857
#     3   20       11    1.100
#     4   20       12    1.332
#     5   20       13    1.464
#     6   20       17    1.599
#     7   20       19    1.743
#     8   20       19    1.852
#     9   20       19    1.935
#    10   20       20    1.977
#    11    2        2    0.020
#
#         Statistic =  1499.959 
#degrees of freedom =  9 
#           p-value =  < 2.22e-16 

### MACHINE LEARNING

xtabs(~ ttt_diuretique + ttt_diuretique, data = lg)
# there is no zero for nominal variables 
# zeros are present for the quantitative variables

### DATA PARTITION
set.seed(21051986)
ind <- sample(2, nrow(lg), replace = T, prob = c(0.8, 0.2))
train <- lg[ind==1,]
test <- lg[ind==2,]

mod.lg <- glm(compl.s ~ atcd_ethylisme.2 + ttt_betabloquant + ttt_diuretique + creatinine_micromolL + albumine_gL +
              duree_CEC + min_clampageAortique + EUROSCORE_II + degree_urgenceChir3, 
              data = train, family = 'binomial')
summary(mod.lg)

#Call:
#glm(formula = compl.s ~ atcd_ethylisme.2 + ttt_betabloquant + 
#    ttt_diuretique + creatinine_micromolL + albumine_gL + duree_CEC + 
#    min_clampageAortique + EUROSCORE_II + degree_urgenceChir3, 
#    family = "binomial", data = train)

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-2.6415  -0.4936   0.2460   0.6938   1.7782  
#
#Coefficients:
#                       Estimate Std. Error z value Pr(>|z|)   
#(Intercept)           1.521e+01  1.158e+03   0.013  0.98952   
#atcd_ethylisme.21     9.245e-01  6.114e-01   1.512  0.13050   
#ttt_betabloquant1     2.234e-01  6.057e-01   0.369  0.71226   
#ttt_diuretique1       2.175e-01  6.265e-01   0.347  0.72842   
#creatinine_micromolL  1.042e-02  8.154e-03   1.278  0.20114   
#albumine_gL          -1.267e-01  4.852e-02  -2.611  0.00902 **
#duree_CEC             4.048e-02  2.166e-02   1.869  0.06158 . 
#min_clampageAortique -1.866e-02  2.472e-02  -0.755  0.45036   
#EUROSCORE_II          8.997e-02  8.300e-02   1.084  0.27839   
#degree_urgenceChir31 -1.515e+01  1.158e+03  -0.013  0.98956   
#degree_urgenceChir32 -1.534e+01  1.158e+03  -0.013  0.98943   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#    Null deviance: 172.52  on 150  degrees of freedom
#Residual deviance: 118.57  on 140  degrees of freedom
#  (68 observations deleted due to missingness)
#AIC: 140.57
#
#Number of Fisher Scoring iterations: 16


### PREDICTION
p1 <- predict(mod.lg, train, type = 'response')
head(p1, n=30)

#       1         2         3         4         5         6         7         8 
#       NA        NA        NA        NA        NA        NA 0.4876013        NA 
#        9        10        11        12        13        14        15        16 
#0.1092869        NA 0.5276585 0.7629518 0.1869220        NA        NA        NA 
#       17        18        19        20        21        22        23        24 
#       NA 0.1202365        NA 0.1277580        NA        NA 0.4956047 0.8073018 
#       25        26        27        28        29        30 
#0.3972732 0.6198268 0.1818924 0.5842666 0.5998957        NA 


### MISCLASSIFICATION ERROR ON TRAIN DATA
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$compl.s)
tab1
#         Actual
#Predicted   0   1
#        0  20   7
#        1  19 105

### MISCLASSIFICATION ERROR RATE 
1 - sum(diag(tab1))/sum(tab1)
# 0.1721854 ==> 17%

### MISCLASSIFICATION ON TEST DATA
p2 <- predict(mod.lg, test, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$compl.s)
tab2
#         Actual
#Predicted  0  1
#        0  5  6
#        1 11 29
# 5+29 = 34% of misclassification in test data

### GOODNESS-OF-FIT 
pvalue = 1-pchisq(172 - 118, df=(150-140)#, lower.tail = F #specify lower tail to get extreme values or substract -1
)
# p-value = 4.852262e-08 => MODEL IS VALID

table(lg$compl.s)
str(lg$compl.s)

#library("tidyverse")
#train %>%
#  mutate(prob = ifelse(compl.s == 1, 1, 0)) %>%
#  ggplot(aes(creatinine_micromolL, prob)) +
#  geom_point(alpha = 0.2) +
#  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
#  labs(
#    title = "Logistic Regression Model", 
#    x = "Creat micromol/L",
#    y = "Probability of having a major compl/death"
#    )


### COMPLETE MODEL
mod.lg.c <- glm(compl.s ~ atcd_ethylisme.2 + ttt_betabloquant + ttt_diuretique + creatinine_micromolL + albumine_gL +
              duree_CEC + min_clampageAortique + EUROSCORE_II + degree_urgenceChir3, 
              data = lg, family = 'binomial')
              
# PSEUDO-R2
mod <- glm(compl.s~atcd_ethylisme.2 + ttt_betabloquant + 
    ttt_diuretique + creatinine_micromolL + albumine_gL + duree_CEC + 
    min_clampageAortique + EUROSCORE_II + degree_urgenceChir3, data = lg, family="binomial")
nullmod <- glm(compl.s~1, data = lg, family="binomial")
1-logLik(mod)/logLik(nullmod)

# 'log Lik.' 0.5329728 (df=11)

lg$mod.pred <- predict(mod.lg.c, newdata = lg, type = "response")
lg
