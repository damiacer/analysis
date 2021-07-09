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

fus <- read_excel("FUSION CaenNantesVannesAngersRennes final 020721.xlsx", na="NA")

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
  "daterecidive" = "Date RECIDIVE j"
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
mean(fus$fup, na.rm = TRUE)
is.na(fus$fup)
is.na(a)
is.na(b)

fus$fupna[fus$fup=="NA"] = "1"
fus$fupna[fus$fup!=""] = "0"
fus$fupna

# NEW DATABASE WITH ONLY COMPLETE DATA
apkd<-rein_mone[!(rein_mone$apkd01=="0"),]
cofus = fus[!fus$fupna=="1"]
### FUP 