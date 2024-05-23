# PACKAGES----

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

# DATA-----

reaped <- read.csv2("/Users/damianocerasuolo/Desktop/UBRC/UBRS_CONSULT_MAC/Brossier_David/StatistiquesReaPed/BronchioOHD.csv")
dim(reaped)
# 383 175

# VAR TRANSFORMATION----

str(reaped$pH)

reaped <- reaped %>%
  mutate(pHCAT = case_when(
    pH <= 7.3 ~ "1",
    pH > 7.3 ~ "2",
  ))
table(reaped$pHCAT, useNA = "always")

# transfert
reaped$TRe = as.factor(if_else(reaped$TransfertREA == "oui", "2", "1"))

# Terme
reaped <- reaped %>%
  mutate(Terme.F = case_when(
    Terme == "<28" | Terme == "28-32" | Terme == "32-37" ~ "1",
    Terme == ">37" ~ "0" #ref 
  ))
reaped$Terme.F = as.factor(reaped$Terme.F)
table(reaped$Terme.F)

# Ageb
reaped <- reaped %>%
  mutate(Age.F = case_when(
    Ageb == "<1mois" ~ "2",
    Ageb == ">3mois" ~ "0", #reference
    Ageb == "1-3mois" ~ "1"
  ))
reaped$Age.F = as.factor(reaped$Age.F)

# PremiereBronchio
reaped$PremiereBronchio.F = as.factor(if_else(reaped$PremiereBronchio == "oui", "1", "0"))

# virus
#reaped <- reaped %>%
#  mutate(virus.F = case_when(
#    virus == "Autre" ~ "1", 
#    virus == "ERV" ~ "2",
#    virus == "non identifie" ~ "3",
#    virus == "VRS" ~ "4"
#  ))
#reaped$virus.F = as.factor(reaped$virus.F)
#table(reaped$virus.F)

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
    FC == "normocarde" ~ "0", # ref
    FC == "tachycarde>160/min" ~ "0", # ref
    FC == "tachycarde>180min" ~ "1"
  ))
reaped$FC.F = as.factor(reaped$FC.F)
table(reaped$FC.F, reaped$FC, useNA = "always")

# Spo2
reaped <- reaped %>%
  mutate(Spo2.F = case_when(
    Spo2 == "<88%" | Spo2 == "88-92%" ~ "1", # < 92
    Spo2 == ">92%" ~ "0", # > 92 # reference
  ))
reaped$Spo2.F = as.factor(reaped$Spo2.F)
table(reaped$Spo2.F, reaped$Spo2, useNA = "always")

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
table(reaped$RP.F, useNA = "always")

# Woodb
table(reaped$Woodb)
reaped <- reaped %>%
  mutate(Wood = case_when(
    Woodb == ">=3" ~ "1",
    Woodb == "0-2,5" ~ "0"
  ))
table(reaped$Wood, useNA = "always")
reaped$Wood = as.factor(reaped$Wood)

# ROXI
str(reaped$ROXI)


# model TRe01----

table(reaped$TRe)
reaped$TRe01 = as.factor(if_else(reaped$TRe == "2", "1", "0"))

mod.transf = glm(TRe01 ~ Terme.F + Age.F + FC.F + Spo2.F + Wood + 
             ApneesMaison.F + MalaiseHOP.F + ROXI, 
           data = reaped, 
           family = binomial)
summary(mod.transf)
exp(cbind(OR = coef(mod.transf), confint(mod.transf)))

mod.age = glm(TRe01 ~ Terme.F + FC.F + Spo2.F + Wood + 
                             ApneesMaison.F + MalaiseHOP.F + ROXI, 
                           data = reaped, 
                           family = binomial)
lrtest(mod.transf, mod.age)

# Correlation----
performance::check_collinearity(mod.transf)

# Hosmer-Lemeshow----
glmtoolbox::hltest(mod.transf)

# pseudo-R2----

mod.null = glm(TRe01 ~ 1, data = reaped, family = binomial)

1-logLik(mod.transf)/logLik(mod.null)
# 'log Lik.' 0.3633187 (df=10)

# Accuracy of the full mod by bootstrapping 

performance::performance_accuracy(
  mod.transf, 
  method = c(#cv, CROSS VALIDATION
    "boot"),
  k = 5,
  n = 1000,
  verbose = TRUE)

#----------

# model VNI

str(reaped$VNI)
reaped$VNI = as.factor(if_else(reaped$VNI == "Oui", "1", "0"))


mod.vni = glm(VNI ~ Terme.F + Age.F + FC.F + Spo2.F + Wood + 
                   ApneesMaison.F + MalaiseHOP.F + ROXI, 
                 data = reaped, 
                 family = binomial)
summary(mod.vni)
exp(cbind(OR = coef(mod.vni), confint(mod.vni)))

mod.vage = glm(VNI ~ Terme.F + FC.F + Spo2.F + Wood + 
                ApneesMaison.F + MalaiseHOP.F + ROXI, 
              data = reaped, 
              family = binomial)
lrtest(mod.transf, mod.vage)


# Correlation----
performance::check_collinearity(mod.vni)

# Hosmer-Lemeshow----
glmtoolbox::hltest(mod.vni)

# pseudo-R2----

mod.null = glm(VNI ~ 1, data = reaped, family = binomial)

1-logLik(mod.vni)/logLik(mod.null)
# 'log Lik.' 0.3633187 (df=10)

# Accuracy of the full mod by bootstrapping 

mod.vni2 = glm(VNI ~ Terme.F + FC.F + Spo2.F 
               + Wood
               + ApneesMaison.F
               + MalaiseHOP.F
               + ROXI, 
              data = reaped, 
              family = binomial)

performance::performance_accuracy(
  mod.vni, 
  method = c(#"cv"
             #, #CROSS VALIDATION
    #"boot"
    ),
  k = 5,
  n = 1000,
  verbose = TRUE)

