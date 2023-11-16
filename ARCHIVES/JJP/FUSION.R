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

################################################################################

# VARIABLES RENAMING

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
  "delaissurvgeste" = "DELAI.SURVIE.GESTE jours",
  "premiererecidive" = "1er/recidive",
  "tailleparticules" = "Taille particules",
  "arrethem" = "Arrêt immediat hemoptysie"
  )

#fusna <- as_tibble(fusna)
#fusna <- fusna %>% rename(
#  "arrethem" = "Arrêt immediat hemoptysie"
#)

################################################################################

# VARIABLE RECODING 

# ONE PREVIOUS PATHOLOGY / TRACK RECORD
# "ATCD.CARDIOVASC", "ATCD.RESPI", "ATCD.GASTRO", 
# "ATCD.HEMATO", "ATCD.METAB.ENDOC", "ATCD.CANCER.NONPULM", "ATCD.NEPHRO"
# fus$track[fus$ATCD.CARDIOVASC == "Oui" | fus$ATCD.RESPI == "Oui" | fus$ATCD.GASTRO == "Oui"
#          | fus$ATCD.HEMATO == "Oui " | fus$ATCD.METAB.ENDOC == "Oui"
#          | fus$ATCD.CANCER.NONPULM == "Oui" | fus$ATCD.NEPHRO == "Oui"
#          | fus$ATCD embo bronchique == "Oui"] <- "1"
# fus$track[fus$ATCD.CARDIOVASC == "Non" & fus$ATCD.RESPI == "Non" & fus$ATCD.GASTRO == "Non"
#          & fus$ATCD.HEMATO == "Non " & fus$ATCD.METAB.ENDOC == "Non"
#          & fus$ATCD.CANCER.NONPULM == "Non" & fus$ATCD.NEPHRO == "Non"
#          & fus$ATCD embo bronchique == "Non] <- "0"
# fus$track[fus$ATCD.CARDIOVASC == "NA" & fus$ATCD.RESPI == "" & fus$ATCD.GASTRO == ""
#          & fus$ATCD.HEMATO == " " & fus$ATCD.METAB.ENDOC == ""
#          & fus$ATCD.CANCER.NONPULM == "" & fus$ATCD.NEPHRO == ""
#          & fus$ATCD embo bronchique == ""] <- "."
# table(fus$track)

# fusna$Primitifpoumon
fus$Primitifpoumon2[fus$Primitifpoumon == "Primitif"] <- "1"
fus$Primitifpoumon2[fus$Primitifpoumon == "Secondaire"] <- "2"
table(fus$Primitifpoumon2)

# ATCD.RESPI
fus$ATCD.RESPI[fus$ATCD.RESPI == "Non"] <- "0"
fus$ATCD.RESPI[fus$ATCD.RESPI == "Oui"] <- "1"
fus$ATCD.RESPI[fus$ATCD.RESPI == "NA"] <- "."
fus$ATCD.RESPI = as.numeric(as.character(fus$ATCD.RESPI))
fus$ATCD.RESPI = as.factor(fus$ATCD.RESPI)

# TABAC.PA
fus$TABAC.PA[fus$TABAC.PA == "0"] <- "0"
fus$TABAC.PA[fus$TABAC.PA == ""] <- "."
fus$TABAC.PA[fus$TABAC.PA != "ZERO"] <- fus$TABAC.PA

# ETIOLOGIE
fus$ETIOLOGIE2[fus$ETIOLOGIE != "Adenocarcinome" | fus$ETIOLOGIE != "Epidermoide"] <- "Autre"
fus$ETIOLOGIE2[fus$ETIOLOGIE == "Adenocarcinome"] <- "Adenocarcinome"
fus$ETIOLOGIE2[fus$ETIOLOGIE == "Epidermoide"] <- "Epidermoide"
table(fus$ETIOLOGIE2)

# EXCAV
table(fus$IMAGERIE.EXCAVE)
fus$IMAGERIE.EXCAVE_class[fus$IMAGERIE.EXCAVE == "oui"] <- "1"
fus$IMAGERIE.EXCAVE_class[fus$IMAGERIE.EXCAVE == "Non"] <- "0"
fus$IMAGERIE.EXCAVE_class[fus$IMAGERIE.EXCAVE == "Pas de TDM"] <- "."
str(fus$IMAGERIE.EXCAVE_class)
fus$IMAGERIE.EXCAVE_class = as.numeric(as.character(fus$IMAGERIE.EXCAVE_class))
fus$IMAGERIE.EXCAVE_class = as.factor(fus$IMAGERIE.EXCAVE_class)
table(fus$IMAGERIE.EXCAVE_class)

# NECROSIS
table(fus$LESION.NECROTIQUE)
fus$LESION.NECROTIQUE_class[fus$LESION.NECROTIQUE == "Oui"] <- "1"
fus$LESION.NECROTIQUE_class[fus$LESION.NECROTIQUE == "Non"] <- "0"
fus$LESION.NECROTIQUE_class[fus$LESION.NECROTIQUE == "Pas de TDM"] <- "."
fus$LESION.NECROTIQUE_class = as.numeric(as.character(fus$LESION.NECROTIQUE_class))
fus$LESION.NECROTIQUE_class = as.factor(fus$LESION.NECROTIQUE_class)
table(fus$LESION.NECROTIQUE_class)

# PROX/DISTAL
table(fus$prox_distal)
fus$prox_distal_class[fus$prox_distal == "distal"] <- "0" #DISTAL
fus$prox_distal_class[fus$prox_distal == "lobe"] <- "1" #LOBE
fus$prox_distal_class[fus$prox_distal == "NA"] <- "." #MISSING
fus$prox_distal_class
fus$prox_distal_class = as.numeric(as.character(fus$prox_distal_class))
fus$prox_distal_class = as.factor(fus$prox_distal_class)

table(fus$HEMOPTYSIE.IMPORTANTE)

table(fus$Hemodynamique)

# FAV FACTORS
table(fus$Factfavor)
fus$Factfavor2[fus$Factfavor == "Aucun"] <- "0"
fus$Factfavor2[fus$Factfavor != "Aucun" ] <- "1"

# LESION
table(fus$lesionarterepulm)
fus$lesionarterepulm_class[fus$lesionarterepulm == "Irrégularité"] <- "1"
fus$lesionarterepulm_class[fus$lesionarterepulm == "normale"] <- "0"
fus$lesionarterepulm_class[fus$lesionarterepulm == "occlusion"] <- "2"
fus$lesionarterepulm_class[fus$lesionarterepulm == "Pseudoanévrisme"] <- "3"
fus$lesionarterepulm_class[fus$lesionarterepulm == ""] <- "."
fus$lesionarterepulm_class = as.numeric(as.character(fus$lesionarterepulm_class))
fus$lesionarterepulm_class = as.factor(fus$lesionarterepulm_class)

# RECIDIVE
str(fus$recidive)
fus$recidive[fus$recidive=="non"] = "0"
fus$recidive[fus$recidive=="oui"] = "1"
fus$recidive[fus$recidive=="ND"] = "."
fus$recidive
fus$relapse = as.numeric(as.character(fus$recidive))

# TECHembolPULM1
table(fus$TECHembolPULM1)
fus$TECHembolPULM2[fus$TECHembolPULM1 == "Coils"] <- "1"
fus$TECHembolPULM2[fus$TECHembolPULM1 == "Colle"] <- "2"
fus$TECHembolPULM2[fus$TECHembolPULM1 == "Gélatine"] <- "3"
fus$TECHembolPULM2[fus$TECHembolPULM1 == "Plug"] <- "4"
fus$TECHembolPULM2[fus$TECHembolPULM1 == "Stent couvert"] <- "5"
fus$TECHembolPULM2[fus$TECHembolPULM1 == ""] <- "."
str(fus$TECHembolPULM2)
fus$TECHembolPULM2 = as.numeric(as.character(fus$TECHembolPULM2))
fus$TECHembolPULM2 = as.factor(fus$TECHembolPULM2)

# SIZE
fus$taillelesion_m = as.numeric(as.character(fus$taillelesion_m))

# DELAI SURGERY
fus$delaissurvgeste = as.numeric(as.character(fus$delaissurvgeste))

# DECES

fus$DECES01[fus$DECES=="Oui"] = "1"
fus$DECES01[fus$DECES=="Non" | fus$DECES=="ND"] = "0"
table(fus$DECES01)

#-------------------------------------------------------------------------------

fus$relapse = as.numeric(as.character(fus$recidive))
fus$taillelesion_m = as.numeric(as.character(fus$taillelesion_m))
fus$delaissurvgeste = as.numeric(as.character(fus$delaissurvgeste))

fus$relapsec = as.factor(fus$relapse)

################################################################################

# TABLEONE

require("tableone")

dput(names(fusna))

variables = c("age",
              "SEXE",
              "Primitifpoumon",
              "ETIOLOGIE2",
              "HEMOPTYSIE.VOLUME",
              "HEMOPTYSIE.IMPORTANTE",
              "Hemodynamique",
              "Factfavor2",
              "IMAGERIE.EXCAVE_class",
              "LESION.NECROTIQUE_class",
              "lesionarterepulm_class",
              "taillelesion_m",
              "prox_distal_class",
              "TECHembolPULM2",
              "delaissurvgeste",
              "relapsec",
              "arrethem",
              "DECES01")

categorical = c("SEXE",
                "Primitifpoumon",
                "ETIOLOGIE2",
                "HEMOPTYSIE.VOLUME",
                "HEMOPTYSIE.IMPORTANTE",
                "Hemodynamique",
                "Factfavor2",
                "IMAGERIE.EXCAVE_class",
                "LESION.NECROTIQUE_class",
                "lesionarterepulm_class",
                "prox_distal_class",
                "TECHembolPULM2",
                "relapsec",
                "arrethem",
                "DECES01")

# CREATE THE DESCRIPTIVE TABLE TABLE 
tab1 = CreateTableOne(vars = variables, data = fus, factorVars = categorical)
print(tab1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

# CREATE THE UNIVARIATE TABLE 
tab2 = CreateTableOne(vars = variables, data = fusnad, 
                      factorVars = categorical, test = TRUE, 
                      includeNA = FALSE, strata = "DECES01")
print(tab2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

str(fusna$relapsec)
tab3 = CreateTableOne(vars = variables, data = fusna, factorVars = categorical, test = TRUE, 
                      includeNA = FALSE, strata = "relapsec")
print(tab3, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#-------------------------------------------------------------------------------

tabarr = table(fus$arrethem)
prop.table(tabarr)
0.08695652*100
0.91304348*100

################################################################################

# TEST FOR UNIVARIATE ANALYSIS: RELAPSE ANALYSIS

# AGE
str(fus$age)
shapiro.test(fus$age)
hist(fus$age)
str(fus$relapsec) 
  # T-test in R works with numeric variables (1 quantitative and 1 quantitative assuming 2 values)

t.test(fus$age, fus$relapse, alternative = "two.sided", conf.int = 0.95, var.equal = FALSE)
wilcox.test(fus$age~fus$relapse, paired = FALSE, exact = FALSE, correct = FALSE)

# SEXE
chisq.test(fus$SEXE, fus$relapse, correct = FALSE, simulate.p.value = TRUE)

# Primitifpoumon
chisq.test(fus$Primitifpoumon, fus$relapse, correct = FALSE, simulate.p.value = TRUE)
fisher.test(fus$Primitifpoumon, fus$relapse)

# ETIOLOGIE2
chisq.test(fus$ETIOLOGIE2, fus$relapse, correct = FALSE, simulate.p.value = TRUE)

# HEMOPTYSIE.VOLUME
chisq.test(fus$HEMOPTYSIE.VOLUME, fus$relapse, correct = FALSE, simulate.p.value = TRUE)

# HEMOPTYSIE.IMPORTANTE
chisq.test(fus$HEMOPTYSIE.IMPORTANTE, fus$relapse, correct = FALSE, simulate.p.value = TRUE)
fisher.test(fus$HEMOPTYSIE.IMPORTANTE, fus$relapse)

# Hemodynamique
chisq.test(fus$Hemodynamique, fus$relapse, correct = FALSE, simulate.p.value = TRUE)

# Factfavor2
chisq.test(fus$Factfavor2, fus$relapse, correct = FALSE, simulate.p.value = TRUE)

# IMAGERIE.EXCAVE_class 
chisq.test(fus$IMAGERIE.EXCAVE_class, fus$relapse, correct = FALSE, simulate.p.value = TRUE)

# LESION.NECROTIQUE_class 
chisq.test(fus$LESION.NECROTIQUE_class, fus$relapse, correct = FALSE, simulate.p.value = TRUE)

# lesionarterepulm_class
chisq.test(fus$lesionarterepulm_class, fus$relapse, correct = FALSE, simulate.p.value = TRUE)

# taillelesion_m
shapiro.test(fus$taillelesion_m)
t.test(fus$taillelesion_m, fus$relapse, alternative = "two.sided", conf.int = 0.95, var.equal = FALSE)

# prox_distal_class
chisq.test(fus$prox_distal_class, fus$relapse, correct = FALSE, simulate.p.value = TRUE)

# TECHembolPULM2
chisq.test(fus$TECHembolPULM2, fus$relapse, correct = FALSE, simulate.p.value = TRUE)

# delaissurvgeste
shapiro.test(fus$delaissurvgeste)
wilcox.test(fus$delaissurvgeste~fus$relapse, paired = FALSE, exact = FALSE, correct = FALSE)

# DECES01
chisq.test(fus$DECES01, fus$relapse, correct = FALSE, simulate.p.value = TRUE)

# arrethem
hemrel = table(fus$arrethem, fus$relapse)
prop.table(hemrel, margin = 2)
0.11363636*100
0.07894737*100
0.88636364*100
0.92105263*100
chisq.test(fus$arrethem, fus$relapse, correct = FALSE, simulate.p.value = TRUE)

#-------------------------------------------------------------------------------

fus$DECES01 = as.numeric(as.character(fus$DECES01))

# AGE
t.test(fus$age, fus$DECES01, alternative = "two.sided", conf.int = 0.95, var.equal = FALSE)
wilcox.test(fus$age~fus$relapse, paired = FALSE, exact = FALSE, correct = FALSE)

# SEXE
chisq.test(fus$SEXE, fus$DECES01, correct = FALSE, simulate.p.value = TRUE)

# Primitifpoumon
chisq.test(fus$Primitifpoumon, fus$DECES01, correct = FALSE, simulate.p.value = TRUE)
fisher.test(fus$Primitifpoumon, fus$DECES01)

# ETIOLOGIE2
chisq.test(fus$ETIOLOGIE2, fus$DECES01, correct = FALSE, simulate.p.value = TRUE)

# HEMOPTYSIE.VOLUME(
chisq.test(fus$HEMOPTYSIE.VOLUME, fus$DECES01, correct = FALSE, simulate.p.value = TRUE)

# HEMOPTYSIE.IMPORTANTE
chisq.test(fus$HEMOPTYSIE.IMPORTANTE, fus$DECES01, correct = FALSE, simulate.p.value = TRUE)
fisher.test(fus$HEMOPTYSIE.IMPORTANTE, fus$DECES01)

# Hemodynamique
chisq.test(fus$Hemodynamique, fus$DECES01, correct = FALSE, simulate.p.value = TRUE)

# Factfavor2
chisq.test(fus$Factfavor2, fus$DECES01, correct = FALSE, simulate.p.value = TRUE)

# IMAGERIE.EXCAVE_class 
chisq.test(fus$IMAGERIE.EXCAVE_class, fus$DECES01, correct = FALSE, simulate.p.value = TRUE)

# LESION.NECROTIQUE_class 
chisq.test(fus$LESION.NECROTIQUE_class, fus$DECES01, correct = FALSE, simulate.p.value = TRUE)

# lesionarterepulm_class
chisq.test(fus$lesionarterepulm_class, fus$DECES01, correct = FALSE, simulate.p.value = TRUE)

# taillelesion_m
shapiro.test(fus$taillelesion_m)
t.test(fus$taillelesion_m, fus$DECES01, alternative = "two.sided", conf.int = 0.95, var.equal = FALSE)

# prox_distal_class
chisq.test(fus$prox_distal_class, fus$DECES01, correct = FALSE, simulate.p.value = TRUE)

# TECHembolPULM2
chisq.test(fus$TECHembolPULM2, fus$relapse, correct = FALSE, simulate.p.value = TRUE)

# delaissurvgeste
shapiro.test(fus$delaissurvgeste)
wilcox.test(fus$delaissurvgeste~fus$relapse, paired = FALSE, exact = FALSE, correct = FALSE)

# DECES01
chisq.test(fus$DECES01, fus$relapse, correct = FALSE, simulate.p.value = TRUE)

# arrethem
hemdec = table(fus$arrethem, fus$DECES01)
prop.table(hemdec, margin = 2)
0.0000000*100
0.1025641*100
1.0000000*100
0.8974359*100
chisq.test(fus$arrethem, fus$DECES01, correct = FALSE, simulate.p.value = TRUE)

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

#########################################
############## FOLLOW UP ################
#########################################

# FUP OVERALL (FOLLOW UP AFTER TREATMENT)
# DEATHS ARE CENSORED - NO CONCURRENT EVENT
# DECES COMME DICHOTOMOUS VARIABLE
fus$DECES01[fus$DECES=="Oui"] = "1"
fus$DECES01[fus$DECES=="Non" | fus$DECES=="ND"] = "0"
table(fus$DECES01)

# FOLLOW UP POUR LA POPULATION TOTALE 
a = as.Date(fus$DATE.DERN.NOUV) - as.Date(fus$dategeste)
b = as.Date(fus$datedeces) - as.Date(fus$dategeste)
fus$fup = ifelse(fus$DECES01 == "1", b, a)
table(fus$fup)

# FUP FOR DEATH DATA
fus$fupd = a

#########################################
############## ONLY FU ##################
#########################################

require(dplyr)

# NEW DATABASE WITH ONLY COMPLETE DATA
# more on drop: https://blog.rstudio.com/2016/08/15/tidyr-0-6-0/
fusna <- fus %>% drop_na(fup)
count(fusna)

fusnad <- fus %>% drop_na(fupd)
count(fusnad)

mean(fus$fup, na.rm = TRUE)
is.na(fus$fup)
is.na(a)
is.na(b)

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


################################################################################

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
fus$relapse2 = as.factor(fus$relapse) # leaps works with characters or factors as y
str(fus$relapse2)

library("dplyr")

fusna <- fus %>% drop_na(fup)
names(fusna)

fusna$status = fusna$relapse
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

autoplot(km_fit, surv.linetype = "dashed", surv.colour = "orange", 
         censor.colour = "red", conf.int = "TRUE", censor.shape = "*")

################################################################################

# COX MODEL AND MODEL SELECTION

install.packages("My.stepwise")
library("My.stepwise")

names(fusna)

str(fusna$time)
str(fusna$status)
str(fusna$Primitifpoumon)

var.list = c("fusna$age", "fusna$SEXE", #"fusna$Primitifpoumon", 
             "fusna$ETIOLOGIE2", 
            "fusna$HEMOPTYSIE.VOLUME", "fusna$HEMOPTYSIE.IMPORTANTE",  
            "fusna$Hemodynamique", "fusna$Factfavor2", "fusna$IMAGERIE.EXCAVE_class",
            "fusna$LESION.NECROTIQUE_class", "fusna$lesionarterepulm", 
            "fusna$taillelesion_m", "fusna$prox_distal_class", "fusna$TECHembolPULM1", 
            "fusna$delaissurvgeste")

My.stepwise.coxph(Time = "time", Status = "status", variable.list = var.list, 
                  data = fusna, sle = 0.15, sls = 0.15)

My.stepwise.coxph(Time = "time", Status = "status", variable.list = c("age"), 
                  data = fusna, sle = 0.15, sls = 0.15)

#-------------------------------------------------------------------------------

library("survival")
install.packages("rms")

install.packages("riskRegression")
library("riskRegression")

selectcox1 <- selectCox(Surv(time, status) ~ age + SEXE + Primitifpoumon + ETIOLOGIE2 
                        + HEMOPTYSIE.VOLUME + HEMOPTYSIE.IMPORTANTE + Hemodynamique + 
                          Factfavor2 + IMAGERIE.EXCAVE_class + LESION.NECROTIQUE_class + 
                          lesionarterepulm + taillelesion_m + prox_distal_class + 
                          TECHembolPULM1 + delaissurvgeste, data = fusna)

################################################################################

#################################################################
# GET THE COX MODEL BY USING FACTOR VARIABLES WHERE APPROPRIATE #
#################################################################

# SEXE
fusna$SEXE2 = as.factor(fusna$SEXE2)

# Primitifpoumon
fusna$Primitifpoumon2 = as.factor(fusna$Primitifpoumon2)

# ETIOLOGIE2
fusna$ETIOLOGIE2f = as.factor(fusna$ETIOLOGIE2f)

# HEMOPTYSIE.VOLUME
fusna$HEMOPTYSIE.VOLUME2 = as.factor(fusna$HEMOPTYSIE.VOLUME2)

# HEMOPTYSIE.IMPORTANTE
fusna$HEMOPTYSIE.IMPORTANTE2 = as.factor(fusna$HEMOPTYSIE.IMPORTANTE2)

# Hemodynamique
fusna$Hemodynamique2 = as.factor(fusna$Hemodynamique2)

# Factfavor2
fusna$Factfavor2 = as.factor(fusna$Factfavor2)

# IMAGERIE.EXCAVE_class
fusna$IMAGERIE.EXCAVE_class = as.factor(fusna$IMAGERIE.EXCAVE_class)

# LESION.NECROTIQUE_class
fusna$LESION.NECROTIQUE_class = as.factor(fusna$LESION.NECROTIQUE_class)

# lesionarterepulm
fusna$lesionarterepulm2 = as.factor(fusna$lesionarterepulm2)

# taillelesion_m
fusna$taillelesion_m = fusna$taillelesion_m

# prox_distal_class
fusna$prox_distal_class = as.factor(fusna$prox_distal_class)

# TECHembolPULM1
fusna$TECHembolPULM12 = as.factor(fusna$TECHembolPULM12)

# fusna$delaissurvgeste

#-------------------------------------------------------------------------------

# UNIVARIATE MODEL VARIABLES

coxdatafusna <- fusna[,c("age", "SEXE2", "Primitifpoumon", "ETIOLOGIE2", "HEMOPTYSIE.VOLUME2",
                         "HEMOPTYSIE.IMPORTANTE", "Hemodynamique2", "Factfavor2", "IMAGERIE.EXCAVE_class",
                         "LESION.NECROTIQUE_class", "LESION.NECROTIQUE_class", "lesionarterepulm2", "taillelesion_m",
                         "TECHembolPULM12", "arrethem", "relapsec", "DECES01", "fup")]


time = fusna$fupd

# RECODE DECES01 AND relapse FOR THE COX STATEMENT
# 1st UNIVARIATE
valetudo = fusna$DECES01
valetudo = as.numeric(as.character(valetudo))
valetudo = ifelse(valetudo == 0, 1, 2)

table(valetudo)
str(valetudo)
status = as.numeric(as.character(valetudo))

# 2nd UNIVARIATE
valetudo = fusna$relapsec
valetudo = as.numeric(as.character(valetudo))
valetudo = ifelse(valetudo == 0, 1, 2)

table(valetudo)
str(valetudo)
status = as.numeric(as.character(valetudo))


covariates <- c("age", "SEXE2", "Primitifpoumon", "ETIOLOGIE2", "HEMOPTYSIE.VOLUME2",
"HEMOPTYSIE.IMPORTANTE", "Hemodynamique2", "Factfavor2", "IMAGERIE.EXCAVE_class",
"LESION.NECROTIQUE_class", "lesionarterepulm2", "taillelesion_m",
"TECHembolPULM12", "arrethem", "relapsec"
 #, #DECES01"
)

univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, valetudo)~', x)))

library("survival")

univ_models <- lapply(univ_formulas, function(x){coxph(x, data = coxdatafusna)})

# EXTRACT DATA
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2); #coeficient beta
                         HR <-signif(x$coef[2], digits=2); #exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })

res <- t(as.data.frame(univ_results, check.names = F))
as.data.frame(res)

#-------------------------------------------------------------------------------

# COX UNIVARIATES

# lesionarterepulm_class

time = fusna$fup
status = valetudo

coxfusnad = coxph(formula = Surv(time, status) ~ arrethem, data = fusna)

summary(coxfusnad)
confint(coxfusnad)  #coefficient CIs
exp(confint(coxfusnad))  #Also HR CIs

################################################################################1

install.packages("survminer")
library("survminer")

fusnad$status <- status
fusnad$time <- fusnad$fup/(365.25/12)

linelistsurv.by = survfit(Surv(time, status) ~ relapsec, data = fusnad)

survminer::ggsurvplot(
  linelistsurv.by, 
  data = apkd.dathap,          
  conf.int = FALSE,              # do not show confidence interval of KM estimates
  surv.scale = "percent",        # present probabilities in the y axis in %
  break.time.by = 10,            # present the time axis with an increment of 10 days
  xlab = "Follow-up days",
  ylab = "Survival Probability",
  pval = T,                      # print p-value of Log-rank test 
  pval.coord = c(40,.91),        # print p-value at these plot coordinates
  risk.table = T,                # print the risk table at bottom 
  legend.title = "overall",     # legend characteristics, "class1" and "class2" most of the time
  #legend.labs = c("class1","class2"),
  font.legend = 10, 
  palette = "Dark2",             # color palette 
  surv.median.line = "hv",       # draw horizontal and vertical lines to the median survivals
  ggtheme = theme_light()        # simplify plot background
)

#-------------------------------------------------------------------------------

# STRATIFY 
library("ggplot2")

survminer::ggsurvplot(
  linelistsurv.by, 
  data = fusnad,          
  conf.int = FALSE,              # do not show confidence interval of KM estimates
  surv.scale = "percent",        # present probabilities in the y axis in %
  break.time.by = 10,            # present the time axis with an increment of 10 days
  xlab = "Follow-up days",
  ylab = "Survival Probability",
  pval = T,                      # print p-value of Log-rank test 
  pval.coord = c(40,.91),        # print p-value at these plot coordinates
  risk.table = T,                # print the risk table at bottom 
  legend.title = "overall",     # legend characteristics, "class1" and "class2" most of the time
  legend.labs = c("class1","class2"),
  font.legend = 10, 
  palette = "Dark2",             # color palette 
  surv.median.line = "hv",       # draw horizontal and vertical lines to the median survivals
  ggtheme = theme_light()        # simplify plot background
)
