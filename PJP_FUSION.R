getwd()
setwd("P:/CONSULTATION/Pelage_JeanPierre") # ON PC
setwd("/Users/damianocerasuolo/Desktop/PhD/M2/DATABASES_REIN/csv_data") # ON MAC

################################################################################

# PACKAGES

#install.packages("readxl")
library("readxl")
#install.packages("tidyverse")
library("tidyverse")

################################################################################

# DATA

fus <- read_excel("FUSION CaenNantesVannesAngersRennes final 020721.xlsx", na="")

names(fus)

fus <- as_tibble(fus)
fus <- fus %>% rename(
  # new name = old name,
  "age" = "âge",
  "inclusion" = "Date diagnostic...5",
  "taillelesion_m" = "TAILLE LESION (mm)",
  "dategeste" = "DATE.GESTE...17",
  "Primitifpoumon" = "Primitif poumon",
  "Factfavor" = "Fact favor",
  "lesionarterepulm" = "Lésion artère pulm",
  "prox_distal" = "prox/distal",
  "bronchique_ETpump" = "bronchique même temps", # embolisation des artères bronchiques et pulmunaires
  "TECHembolPULM1" = "TECH EMBOL PULM1", # voir en fonction de la variable prox_distal
  "datedeces" = "date décès",
  "daterecidive" = "Date RECIDIVE j",
  "recidive"= "Récidive oui/non",
  "delaissurvgeste" = "DELAI.SURVIE.GESTE jours"
  )

################################################################################

# CCOVARIATES

# age + SEXE + Primitifpoumon + ETIOLOGIE + 
# HEMOPTYSIE.VOLUME = en trois modalités
# HEMOPTYSIE.IMPORTANTE = variable volume en deux catégories
# Hemodynamique + Factfavor + IMAGERIE.EXCAVE + LESION.NECROTIQUE
# lesionarterepulm + taillelesion_m + prox_distal + bronchique_ETpump
# TECHembolPULM1

# OUTCOMES
# Arrêt immediat hemoptysie
# Récidive oui/non = Date RECIDIVE j < 30 jours
# DATE.DERN.NOUV = date des dernières nouvlles
# DECES
# DATE.GESTE

#------------------------------------------------------------------------------#

# FUP

str(fus$dategeste)
fus$dategeste = as.Date(fus$dategeste)

str(fus$DATE.DERN.NOUV)
fus$DATE.DERN.NOUV = as.Date(fus$DATE.DERN.NOUV)

str(fus$daterecidive)
table(fus$daterecidive)

mean(fus$daterecidive, na.rm = TRUE)
min(fus$daterecidive, na.rm = TRUE)
max(fus$daterecidive, na.rm = TRUE)

# FUP OVERALL 
# DECES COMME DICHOTOMOUS VARIABLE
fus$DECES01[fus$DECES=="Oui"] = "1"
fus$DECES01[fus$DECES=="Non" | fus$DECES=="ND"] = "0"
table(fus$DECES01)

# FOLLOW UP POUR LA POPULATION TOTALE 
a = as.Date(fus$DATE.DERN.NOUV) - as.Date(fus$dategeste)
b = as.Date(fus$datedeces) - as.Date(fus$dategeste)
fus$fup = ifelse(fus$DECES01 == "1", b, a)
table(fus$fup)

# NEW DATABASE WITH ONLY COMPLETE DATA
# more on drop: https://blog.rstudio.com/2016/08/15/tidyr-0-6-0/
fusna <- fus %>% drop_na(fup)
count(fusna)

mean(fus$fup, na.rm = TRUE)
is.na(fus$fup)
is.na(a)
is.na(b)

# FUP AFTER TREATMENT

################################################################################

# RECIDIVE

str(fus$recidive)
fus$recidive[fus$recidive=="non"] = "0"
fus$recidive[fus$recidive=="oui"] = "1"
fus$recidive[fus$recidive=="ND"] = "."
fus$recidive

# TO MAKE THE MISSING VALUES "INVISIBLE", TRANSFORM THE VARIABLE IN NUMERIC
fus$relapse = as.numeric(as.character(fus$recidive))
fus$taillelesion_m = as.numeric(as.character(fus$taillelesion_m))
fus$delaissurvgeste = as.numeric(as.character(fus$delaissurvgeste))

# age + SEXE + Primitifpoumon + ETIOLOGIE + HEMOPTYSIE.VOLUME + HEMOPTYSIE.IMPORTANTE + 
# Hemodynamique + Factfavor + IMAGERIE.EXCAVE + LESION.NECROTIQUE + lesionarterepulm +
# taillelesion_m + prox_distal + TECHembolPULM1 + delaissurvgeste

relapse1 <- glm(relapse ~ age + SEXE + Primitifpoumon + ETIOLOGIE + HEMOPTYSIE.VOLUME + HEMOPTYSIE.IMPORTANTE + 
                Hemodynamique + Factfavor + IMAGERIE.EXCAVE + LESION.NECROTIQUE + lesionarterepulm +
                taillelesion_m + prox_distal + TECHembolPULM1 + delaissurvgeste,
                data=fus, family=binomial)

summary(relapse1)
exp(cbind(coef(relapse1), confint(relapse1)))

#-------------------------------------------------------------------------------

# MODEL SELECTION

require(leaps)
fus$relapse2 = as.factor(fus$relapse) # leaps works with characters or factors as y
str(fus$relapse2)

regfit.full = regsubsets(relapse2 ~ age + SEXE + Primitifpoumon + ETIOLOGIE + HEMOPTYSIE.VOLUME + HEMOPTYSIE.IMPORTANTE + 
                                    Hemodynamique + Factfavor + IMAGERIE.EXCAVE + LESION.NECROTIQUE + lesionarterepulm +
                                    taillelesion_m + prox_distal + TECHembolPULM1 + delaissurvgeste, fus)

summary(regfit.full)
plot(regfit.full, scale = "adjr2", main = "Adjusted R^2")

relapse2 = glm(relapse2 ~ age + SEXE + ETIOLOGIE + HEMOPTYSIE.IMPORTANTE + 
                 IMAGERIE.EXCAVE + LESION.NECROTIQUE + prox_distal, data = fus, family = binomial)
summary(relapse2)
exp(cbind(coef(relapse2), confint(relapse2)))

# fus$ETIOLOGIE
# Adenocarcinome 21 
# Carcinome peu différencié 2 
# CBNPC 2 
# Epidermoide 50 
# indetermine 6 
# lymphangiome kystique 1 
# lymphome 2 
# mesotheliome 1 
# metastase 6 
# Petite cellule 1

fus$ETIOLOGIE2[fus$ETIOLOGIE != "Adenocarcinome" | fus$ETIOLOGIE != "Epidermoide"] <- "Autre"
fus$ETIOLOGIE2[fus$ETIOLOGIE == "Adenocarcinome"] <- "Adenocarcinome"
fus$ETIOLOGIE2[fus$ETIOLOGIE == "Epidermoide"] <- "Epidermoide"
table(fus$ETIOLOGIE2)

table(fus$IMAGERIE.EXCAVE)
fus$IMAGERIE.EXCAVE_class[fus$IMAGERIE.EXCAVE == "oui"] <- "1"
fus$IMAGERIE.EXCAVE_class[fus$IMAGERIE.EXCAVE == "Non"] <- "0"
fus$IMAGERIE.EXCAVE_class[fus$IMAGERIE.EXCAVE == "Pas de TDM"] <- "."
str(fus$IMAGERIE.EXCAVE_class)
fus$IMAGERIE.EXCAVE_class = as.numeric(as.character(fus$IMAGERIE.EXCAVE_class))
fus$IMAGERIE.EXCAVE_class = as.factor(fus$IMAGERIE.EXCAVE_class)
table(fus$IMAGERIE.EXCAVE_class)

table(fus$LESION.NECROTIQUE)
fus$LESION.NECROTIQUE_class[fus$LESION.NECROTIQUE == "Oui"] <- "1"
fus$LESION.NECROTIQUE_class[fus$LESION.NECROTIQUE == "Non"] <- "0"
fus$LESION.NECROTIQUE_class[fus$LESION.NECROTIQUE == "Pas de TDM"] <- "."
fus$LESION.NECROTIQUE_class = as.numeric(as.character(fus$LESION.NECROTIQUE_class))
fus$LESION.NECROTIQUE_class = as.factor(fus$LESION.NECROTIQUE_class)
table(fus$LESION.NECROTIQUE_class)

table(fus$prox_distal)
fus$prox_distal_class[fus$prox_distal == "distal"] <- "0" #DISTAL
fus$prox_distal_class[fus$prox_distal == "lobe"] <- "1" #LOBE
fus$prox_distal_class[fus$prox_distal == "NA"] <- "." #MISSING
fus$prox_distal_class
fus$prox_distal_class = as.numeric(as.character(fus$prox_distal_class))
fus$prox_distal_class = as.factor(fus$prox_distal_class)

relapse3 = glm(relapse2 ~ age + SEXE + ETIOLOGIE2 + HEMOPTYSIE.IMPORTANTE + 
                 IMAGERIE.EXCAVE_class + LESION.NECROTIQUE_class + prox_distal_class, data = fus, family = binomial)

summary(relapse3)
exp(cbind(coef(relapse3), confint(relapse3)))

################################################################################

# SURVIVAL 
# https://www.r-bloggers.com/2017/09/survival-analysis-with-r-3/
install.packages("survival")
library("survival")

# create the database with only patients with follow-up not missing
fusna <- fus %>% drop_na(fup)
names(fusna)

fusna$status = fusna$relapse2
fusna$time = fusna$fup
mean(fusna$time)
median(fusna$time)
max(fusna$time)
min(fusna$time)

km <- with(fusna, Surv(time, status))
plot(km, xscale = 365.25)

km_fit <- survfit(Surv(time, status) ~ 1, data = fusna)
summary(km_fit, times = c(1,100,200,500*(1:10)))

# install.packages("ggfortify")
library("ggfortify")

autoplot(km_fit, conf.int = TRUE, censor.shape = "*")
autoplot(aareg(Surv(time, status) ~ 1, data = fusna))
