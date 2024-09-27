require("readxl")
require("tidyverse")
#require("plyr")
require("lubridate")
require("here")
require("tableone")

#-DATASET-----------------------------------------------------------------------

setwd("P:/CONSULTATION/Guaberti_M/Lepont_Lucas")

lp = read_excel("lepont_data.xlsx", na = "")
#lp = read.csv2("lepont_data.csv", header = T, na.strings = "NA")

names(lp)
#View(lp)

lp = as.data.frame(lp)
typeof(lp)

#-TABLEONE----------------------------------------------------------------------

dput(names(lp))

str(lp$Nbre_passage)

lp <- lp %>%
  mutate(Nbre_passageCL = case_when(
    Nbre_passage == 1 ~ "1",
    Nbre_passage == 2 ~ "2",
    Nbre_passage == 3 ~ "3",
    Nbre_passage >= 4 ~ "4"
  ))
table(lp$Nbre_passageCL)

str(lp$Vasospasme)
lp <- lp %>%
  mutate(Vasospasme2 = case_when(
    Vasospasme == "0" ~ "0",
    Vasospasme == "1" ~ "1",
    Vasospasme == "2" ~ "2"
  ))
table(lp$Vasospasme2)

vars <- c("Age", "Sexe", "NIHSS_preT", "Thrombolyse", "DeltaNIHSS",
          "KT_Ballon", "Nbre_passageCL", "Vasospasme2", 
          "Hemorragie_parehymateusePH", "Rapport_CI_KT")

fvars <- c("Sexe", "Thrombolyse", "Nbre_passageCL",
          "KT_Ballon", "Vasospasme2", "Hemorragie_parehymateusePH")
          

descriptive = CreateTableOne(vars = vars, factorVars = fvars, data = lp)

print(descriptive, showAllLevels = TRUE,
                  quote = TRUE,
                  noSpaces = TRUE)

bivariate = CreateTableOne(vars = vars, factorVars = fvars, data = lp,
                           test = TRUE, strata = "Vasospasme2CL")
print(bivariate, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#-VASOSPASME-LOG----------------------------------------------------------------

lp <- lp %>%
  mutate(Vasospasme2CL = case_when(
    Vasospasme == "0" ~ "0",
    Vasospasme == "1" ~ "1",
    Vasospasme == "2" ~ "1"
  ))
table(lp$Vasospasme2CL)

lp$Vasospasme2CL = as.factor(lp$Vasospasme2CL)

vs.log = glm(Vasospasme2CL ~ Age + Sexe + KT_Ballon + Nbre_passageCL + Rapport_CI_KT,
          data = lp, family = binomial)
summary(vs.log)

vs.log.e = glm(Vasospasme2CL ~ Age + Sexe + KT_Ballon + Rapport_CI_KT,
             data = lp, family = binomial)
library(lmtest)
lrtest(vs.log, vs.log.e)

#here:https://www.rdocumentation.org/packages/mosaic/versions/1.8.4.2/topics/confint
#install.packages("mosaic")
#library("mosaic")
#
#if (require(mosaic)){
#  bootstrap <- do(500) * diffmean(Vasospasme2CL ~ Age + Sexe + KT_Ballon + 
#                                    Nbre_passageCL + Rapport_CI_KT,
#                                  data = resample(lp))
#  confint(bootstrap, method = "boot"))
#}
#

exp(cbind(OR = coef(vs.log), confint(vs.log)))

#-NEW-ANALYSIS------------------------------------------------------------------

table(lp$TICI)
str(lp$TICI)

lp <- lp %>%
  mutate(TICI2 = case_when(
    TICI == "0" ~ "mauvaise",
    TICI == "1" ~ "mauvaise",
    TICI == "2a" ~ "mauvaise",
    TICI == "2b" ~ "bonne",
    TICI == "2c" ~ "bonne",
    TICI == "3" ~ "bonne",
    TICI == "3(M1)/1(A2)" ~ "bonne"
  ))

lp <- lp %>%
  mutate(Nbre_passageCL = case_when(
    Nbre_passage == 1 ~ "1",
    Nbre_passage == 2 ~ "2",
    Nbre_passage == 3 ~ "3",
    Nbre_passage >= 4 ~ "4"
  ))

lp <- lp %>%
  mutate(Vasospasme2CL = case_when(
    Vasospasme == "0" ~ "0",
    Vasospasme == "1" ~ "1",
    Vasospasme == "2" ~ "1"
  ))

lp$Vasospasme2CL = as.factor(lp$Vasospasme2CL)

vs.log2 = glm(Vasospasme2CL ~ Age + Sexe + KT_Ballon + Nbre_passageCL + Rapport_CI_KT + TICI2 + DT + HTA,
             data = lp, family = binomial)

summary(vs.log2)

library(lmtest)

# full model collinearity
performance::check_collinearity(vs.log2)

#   The Hosmer-Lemeshow goodness-of-fit test
glmtoolbox::hltest(fit)

# MacFadden PSEUDO-R

vs.log2e = glm(Vasospasme2CL ~ 1, data = lp, family = "binomial")
1-logLik(vs.log2)/logLik(vs.log2e)

# ACCURACY OF THE COMPLETE MODEL BY BOOTSTRAPPING

performance::performance_accuracy(
  vs.log2,
   method = #c("cv", #USE CROSSVALIDATION
   ("boot" #USE BOOTSTRAP
   	),
   k = 5,
   n = 1000,
   verbose = TRUE
   )


#-MACHINE LEARNING--------------------------------------------------------------

table(lp$Sexe)
lp$Sexe = if_else(lp$Sexe == "M", "1", "0")

table(lp$KT_Ballon)
lp$KT_Ballon = if_else(lp$KT_Ballon == "Oui", "1", "0")

table(lp$TICI2)
lp$TICI2 = if_else(lp$TICI2 == "bonne", "1", "0")

table(lp$DT)
lp$DT = if_else(lp$DT == "Oui", "1", "0")

table(lp$HTA)
lp$HTA = if_else(lp$HTA == "Oui", "1", "0")

xtabs(~ Sexe + KT_Ballon + TICI2 + DT + HTA + Nbre_passageCL, data = lp)
# there is no zero for nominal variables 
# zeros are present for the quantitative variables

### DATA PARTITION
set.seed(21051986)
ind <- sample(2, nrow(lp), replace = T, prob = c(0.8, 0.2))
train <- lp[ind==1,]
test <- lp[ind==2,]

mod.lpt <- glm(Vasospasme2CL ~ Age + Sexe + KT_Ballon + Nbre_passageCL + Rapport_CI_KT + TICI2 + DT + HTA,
              data = train, family = binomial)
summary(mod.lpt)

# prediction

p1 <- predict(mod.lpt, train, type = 'response')
head(p1, n=30)

pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$Vasospasme2CL)
tab1

# MISCLASSIFICATION ERROR RATE 
1 - sum(diag(tab1))/sum(tab1)
# 0.1445783 ==> 14%

#-ANALYSE SEPTEMBRE------

# deltaNIHSS (24h - pret)

mod.delta = ln(delta ~ Age + Sexe + Vasospasme2CL + TICI2 + Thrombolyse + Diabete + HTA, data = )
summary(mod.delta)
exp(cbind(OR = coef(mod.delta), confint(mod.delta)))

# descriptive for the delta 

vars <- c("Age", "Sexe", "NIHSS_preT", "Thrombolyse", "DeltaNIHSS",
          "KT_Ballon", "Nbre_passageCL", "Vasospasme2CL", 
          "Hemorragie_parehymateusePH", "Rapport_CI_KT", 
         # verify
         "Diab", "HTA")

fvars <- c("Sexe", "Thrombolyse", "Nbre_passageCL",
          "KT_Ballon", "Vasospasme2CL", "Hemorragie_parehymateusePH"
          "Diab", "HTA")
          

descriptiveS = CreateTableOne(vars = vars, factorVars = fvars, data = lp)

print(descriptiveS, showAllLevels = TRUE,
                  quote = TRUE,
                  noSpaces = TRUE)

bivariateS = CreateTableOne(vars = vars, factorVars = fvars, data = lp,
                           test = TRUE, strata = "delta")
print(bivariateS, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
