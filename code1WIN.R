# packages 

install.packages("readxl")
install.packages("tidyverse")
install.packages("expss")
install.packages("tableone")
install.packages("lmtest")
install.packages("farver")
install.packages("sqldf")
install.packages("bit")

require("readxl")
library("tidyverse")
require("expss")
require("lmtest")
require("broom")
require("farver")
require("sqldf")
require("bit")

#setwd("P:/R_PROJECTS/baddisuarez")
setwd("/Users/damianocerasuolo/Desktop/UBRC/UBRS_CONSULT_MAC/Baddi_Suarez")
bs = read_xlsx("baddisuarez.xlsx")

names(bs)
View(bs)

# var labels----
# it doesn't work under this version

# --> variables incluses dans l'analyse multivariée

bs = apply_labels(bs,
                  DDN	= "Date de naissance",
                  #  Sexe =	"0 = femme ; 1 = homme",
                  Diag = "Date de diagnostic", 
                  #  Type = "Type de melanome (SSM : melanome supeficiel extensif, NM : melanome nodulaire, ALM : melanome acral lentigineux, LM : lentigo malin, MLM : melanome des muqueuses, CM : melanome choroidien)",
                  BRAF = "Mutation BRAF (0 = BRAF sauvage, 1 = BRAF mute)",
                  Breslow = "Indice de Breslow (Epaisseur de la tumeur en millimetre)",
                  #  StadeJ0 = "Stadification du cancer a l inclusion",
                  Ipilumab = "Traitement par Ipilumab (0 = patient naif de traitement, 1 = patient ayant reçu ≥ 1 perfusion)",
                  C1Ipi = "Date de la premiere cure d Ipilumab",
                  CxIpi = "Date de la derniere cure d Ipilumab ",
                  Ipi1A =	"Nombre de cure d Ipilumab a 1 an de la premiere cure",
                  Ipi2A = "Nombre de cure d Ipilumab entre 1 et 2 ans apres la premiere cure",
                  IpiTot = "Nombre total de cure d Ipilumab",
                  Nivolumab = "Traitement par Nivolumab (0 = patient naif de traitement, 1 = patient ayant reçu ≥ 1 perfusion)",
                  C1Nivo = "Date de la premiere cure de Nivolumab",
                  CxNivo = "Date de la derniere cure de Nivolumab", 
                  Nivo1A = "Nombre de cure de Nivolumab a 1 an de la premiere cure",
                  Nivo2A = "Nombre de cure de Nivolumab entre 1 et 2 ans apres la premiere cure",
                  NivoTot	= "Nombre total de cure de Nivolumab",
                  Pembrolizumab = "Traitement par Pembrolizumab (0 = patient naif de traitement, 1 = patient ayant reçu ≥ 1 perfusion)",
                  C1Pembro = "Date de la premiere cure de Pembrolizumab",
                  CxPembro	= "Date de la derniere cure de Pembrolizumab",
                  Pembro1A	= "Nombre de cure de Pembrolizumab a 1 an de la premiere cure",
                  Pembro2A	= "Nombre de cure de Pembrolizumab entre 1 et 2 ans apres la premiere cure",
                  PembroTot	= "Nombre total de cure de Pembrolizumab",
                  Taille	= "Taille en centimetres",
                  Poids0 	= "Poids a l inclusion en kilogrammes",
                  #  IMC0	= "IMC a l inclusion en kg/m2",
                  Poids1	= "Poids a 1 an de la premiere cure en kilogrammes",
                  IMC1	= "IMC a 1 an de la premiere cure en kg/m2",
                  Poids2	= "Poids a 2 ans de la premiere cure en kilogrammes",
                  IMC2	= "IMC a 2 ans de la premiere cure en kg/m2",
                  #  Tabac 	= "Statu tabagique (0 = absence de tabagisme, 1 = tabagisme actif, 2 = tabagisme sevre)",
                  #  Alcool	= "Statu alcoolique (0 = absence de consommation chronique d alcool, 1 = presence d une intoxication alcoolique chronique)",
                  #  Diabete	= "Antecedent de diabete a l inclusion",
                  #  CTC	= "Corticotherapie a partir de l inclusion",
                  IRC	= "Antecedent d insuffisance renale chronique a l inclusion definie par un DFG < 60mL/min",
                  #  Hyperthryoidie = "Antecedent d hyperthyroidie a l inclusion",
                  #  IPP	= "Traitement par Inhibiteur de la pompe a proton a l inclusion",
                  Chimiotherapie 	= "Antecedent de traitement par chimiotherapie a l inclusion",
                  Menopause = "precoce 	Antecedent de menopause precoce a l inclusion",
                  PAL0	= "Phosphatases alcalines a la premiere cure en U/L",
                  PAL1	= "Phosphatases alcalines a 1 an de la premiere cure en U/L",
                  PAL2	= "Phosphatases alcalines a 2 ans de la premiere cure en U/L",
                  LDH0	= "LDH a la premiere cure en U/L",
                  LDH1	= "LDH a 1 an de la premiere cure en U/L",
                  LDH2	= "LDH a 2 ans de la premiere cure en U/L",
                  #  BOR1	= "Best overall response a 1 an de la premiere cure",
                  BOR2	= "Best overall response a 2 ans de la premiere cure",
                  Deces	= "Deces du patient (0 = patient considere vivant en l absence de preuve du deces, 1 = deces prouve et notifie dans le dossier medical du CHU)",
                  DDC	= "Date du deces",
                  DN	= "Date des dernieres nouvelles dans le dossier medical du CHU",
                  TEPTDM	= "TEPTDM utilise pour calcule la densite minerale osseuse (0 = l examen utilise est un scanner, 1 = l examen utilise est un TEP-TDM)",
                  Scanner0	= "Date du scanner de reference",
                  ROI0	= "Densite minerale osseuse de reference par Soukayna",
                  ROI0B	= "Densite minerale osseuse de reference par Dimitri",
                  Scanner1	= "Date du scanner a 1 an de la premiere cure",
                  ROI1	= "Densite minerale osseuse a 1 an de la premiere cure par Soukayna",
                  ROI1B	= "Densite minerale osseuse a 1 an de la premiere cure par Dimitri",
                  Fracture1	= "Fracture sur le scanner a 1 an de la premere cure par Soukayna (0 = absence de fracture ; 1 = fracture sur le compte-rendu du radiologue ou visualise sur les coupes sagittales)",
                  Fracture1B	= "Fracture sur le scanner a 1 an de la premere cure par Dimitri (0 = absence de fracture ; 1 = fracture sur le compte-rendu du radiologue ou visualise sur les coupes sagittales)",
                  Scanner2	= "Date du scanner a 2 ans de la premiere cure",
                  ROI2	= "Densite minerale osseuse a 2 ans de la premiere cure par Soukayna",
                  ROI2B	= "Densite minerale osseuse a 2 ans de la premiere cure par Dimitri",
                  Fracture2	= "Fracture sur le scanner a 2 ans de la premere cure par Soukayna (0 = absence de fracture ; 1 = fracture sur le compte-rendu du radiologue ou visualise sur les coupes sagittales)",
                  Fracture2B	= "Fracture sur le scanner a 2 ans de la premere cure par Dimitri (0 = absence de fracture ; 1 = fracture sur le compte-rendu du radiologue ou visualise sur les coupes sagittales)",
                  IRAESDiabete	= "Diabete apparu au cours de l immunotherapie",
                  IRAESColite	= "Colite apparue au cours de l immunotherapie",
                  IRAESHepatite	= "Hepatite apparue au cours de l immunotherapie",
                  IRAESI.surrenale = "Insuffisance surrrenalienne apparue au cours de l immunotherapie",
                  IRAESRhumatisme	= "Rhumatisme apparu au cours de l immunotherapie",
                  IRAESHypophysite	= "Hypophysite apparue au cours de l immunotherapie",
                  IRAESPNP = "Pneumopathie apparue au cours de l immunotherapie",
                  IRAESHyperthyroidie	= "Hyperthyroidie apparue au cours de l immunotherapie",
                  IRAESAutre	= "Autre evenement indesirable apparu au cours de l immunotherapie",
                  Zometa	= "Cure de zometa (0 = absence de cure realisee ; 1 = antecedent d au moins une cure de Zometa au cours du suivi)",
                  Zometa1	= "Date de la premiere cure de Zometa",
                  Zometax	= "Date de la derniere cure de Zometa",
                  Zometaz	= "Nombre de cure de Zometa au cours du suivi"
)

# tableone----

require("tableone")
dput(names(bs))

bs$Breslow = as.numeric(as.character(bs$Breslow))

variables <- c("Sexe", "Type", "BRAF", "Breslow", "StadeJ0", 
               "Ipilimumab", "Ipi1A", "Ipi2A", "IpiTot", "Nivolumab", 
               "Nivo1A", "Nivo2A", "NivoTot", "Pembrolizumab", 
               "Pembro_1A", "Pembro2A", "PembroTot", 
               "Taille", "Poids0", "IMC0", "Poids1", "IMC1", "Poids2", "IMC2", 
               "Tabac", "Alcool", "Diabète", "CTC", "IRC", "Hyperthyroidie", 
               "IPP", "Chimiotherapie", "Menaupose_precoce", "PAL0", "PAL1", 
               "PAL2", "LDH0", "LDH1", "LDH2", "BOR1", "BOR2", "Decès",  
               "TEPTDM", "ROI0", "ROI0B", "ROI1", 
               "ROI1B", "Fracture1", "Fracture1B", "ROI2", "ROI2B", 
               "Fracture2", "Fracture2B", "IRAESDiabete", "IRAESColite", "IRAESHepatite", 
               "IRAESI.surrenale", "IRAESRhumatisme", "IRAESHypophysite", "IRAESPNP", 
               "IRAESHyperthyroidie", "IRAESAutre", "Zometa", 
               "Zometaz")

cvariables <- c("Sexe", "Type", "BRAF", "StadeJ0", 
                "Ipilimumab", "Ipi1A", "Ipi2A", "IpiTot", "Nivolumab", 
                "Pembrolizumab", 
                "Tabac", "Alcool", "Diabète", "CTC", "IRC", "Hyperthyroidie", 
                "IPP", "Chimiotherapie", "Menaupose_precoce", "Decès",  
                "TEPTDM", "Fracture1", "Fracture1B", 
                "Fracture2", "Fracture2B", "IRAESDiabete", "IRAESColite", "IRAESHepatite", 
                "IRAESI.surrenale", "IRAESRhumatisme", "IRAESHypophysite", "IRAESPNP", 
                "IRAESHyperthyroidie", "IRAESAutre", "Zometa", 
                "Zometaz")

bs.des = CreateTableOne(vars = variables, data = bs, factorVars = cvariables)
print(bs.des, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

# y = ROI0-----

bs$y = bs$ROI0
bs$y = bs$ROI1 - bs$ROI0
empty = lm(y ~ 1, data = bs)

# Sexe 
sex.m = lm(ROI0 ~ Sexe, data = bs)
summary(sex.m)
coef(sex.m)
confint(sex.m)
cbind(coef(sex.m), confint.default(sex.m))

# Type
table(bs$Type)
#ALM LMM MLM  MN SSM 
#8   2   4  36  90 

type.m = lm(y ~ Type, data = bs)
summary(type.m)
#lrtest(type.m, empty)
cbind(coef(type.m), confint.default(type.m))

# BRAF
table(bs$BRAF)

BRAF.m = lm(y ~ BRAF, data = bs)
summary(BRAF.m)
cbind(coef(BRAF.m), confint.default(BRAF.m))

# Breslow
bs$Breslow = as.numeric(as.character(bs$Breslow))

Breslow.m = lm(y ~ Breslow, data = bs)
summary(Breslow.m)
cbind(coef(Breslow.m), confint.default(Breslow.m))

# StadeJ0
table(bs$StadeJ0)

StadeJ0.m = lm(y ~ StadeJ0, data = bs)
summary(StadeJ0.m)
cbind(coef(Breslow.m), confint.default(Breslow.m))


# Ipilimumab
table(bs$Ipilimumab)

Ipilimumab.m = lm(y ~ Ipilimumab, data = bs)
summary(Ipilimumab.m)
cbind(coef(Ipilimumab.m), confint.default(Ipilimumab.m))

# IpiTot
table(bs$IpiTot)
bs$IpiTot = as.factor(bs$IpiTot)

IpiTot.m = lm(y ~ IpiTot, data = bs)
summary(IpiTot.m)
cbind(coef(IpiTot.m), confint.default(IpiTot.m))


# Nivolumab
table(bs$Nivolumab)

Nivolumab.m = lm(y ~ Nivolumab, data = bs)
summary(Nivolumab.m)
cbind(coef(Nivolumab.m), confint.default(Nivolumab.m))


# NivoTot
bs$NivoTot = as.numeric(as.factor(bs$NivoTot))

NivoTot.m = lm(y ~ NivoTot, data = bs)
summary(NivoTot.m)
cbind(coef(NivoTot.m), confint.default(NivoTot.m))

# Pembrolizumab
table(bs$Pembrolizumab)

Pembrolizumab.m = lm(y ~ Pembrolizumab, data = bs)
summary(Pembrolizumab.m)
cbind(coef(Pembrolizumab.m), confint.default(Pembrolizumab.m))

# PembroTot
table(bs$PembroTot)

PembroTot.m = lm(y ~ PembroTot, data = bs)
summary(PembroTot.m)
cbind(coef(PembroTot.m), confint.default(PembroTot.m))

# IMC0
IMC0.m = lm(y ~ IMC0, data = bs)
summary(IMC0.m)
cbind(coef(IMC0.m), confint.default(IMC0.m))

# Tabac
bs$Tabac = as.factor(bs$Tabac)

Tabac.m = lm(y ~ Tabac, data = bs)
summary(Tabac.m)
cbind(coef(Tabac.m), confint.default(Tabac.m))

# Alcool
table(bs$Alcool)

Alcool.m = lm(y ~ Alcool, data = bs)
summary(Alcool.m)
cbind(coef(Alcool.m), confint.default(Alcool.m))

# Diabète
table(bs$Diabète)

Diabète.m = lm(y ~ Diabète, data = bs)
summary(Diabète.m)
cbind(coef(Diabète.m), confint.default(Diabète.m))

# CTC
table(bs$CTC)

CTC.m = lm(y ~ CTC, data = bs)
summary(CTC.m)
cbind(coef(CTC.m), confint.default(CTC.m))

# IRC
table(bs$IRC)

IRC.m = lm(y ~ IRC, data = bs)
summary(IRC.m)
cbind(coef(IRC.m), confint.default(IRC.m))

# Hyperthyroidie
table(bs$Hyperthyroidie)

Hyperthyroidie.m = lm(y ~ Hyperthyroidie, data = bs)
summary(Hyperthyroidie.m)
cbind(coef(Hyperthyroidie.m), confint.default(Hyperthyroidie.m))

# IPP
table(bs$IPP)

IPP.m = lm(y ~ IPP, data = bs)
summary(IPP.m)
cbind(coef(IPP.m), confint.default(IPP.m))

# Chimiotherapie
table(bs$Chimiotherapie)

Chimiotherapie.m = lm(y ~ Chimiotherapie, data = bs)
summary(Chimiotherapie.m)
cbind(coef(Chimiotherapie.m), confint.default(Chimiotherapie.m))

# Menaupose_precoce
table(bs$Menaupose_precoce)

Menoprec.m = lm(y ~ Menaupose_precoce, data = bs)
summary(Menoprec.m)
cbind(coef(Menoprec.m), confint.default(Menoprec.m))

# PAL0
table(bs$PAL0)

PAL0.m = lm(y ~ PAL0, data = bs)
summary(PAL0.m)
cbind(coef(PAL0.m), confint.default(PAL0.m))

# LDH0
LDH0.m = lm(y ~ LDH0, data = bs)
summary(LDH0.m)
cbind(coef(LDH0.m), confint.default(LDH0.m))

# BOR1
BOR1.m = lm(y ~ BOR1, data = bs)
summary(BOR1.m)
cbind(coef(BOR1.m), confint.default(BOR1.m))

# BOR2
BOR2.m = lm(y ~ BOR2, data = bs)
summary(bs)

# Decès
table(bs$Deces)

bs <- as_tibble(bs)
bs <- bs %>% rename(
  #new name = old name
  "Deces" = "Decès")

Deces.m = lm(y ~ Deces, data = bs)
summary(Deces.m)
cbind(coef(Deces.m), confint.default(Deces.m))

#TEPTDM
table(bs$TEPTDM)

TEPTDM.m = lm(y ~ TEPTDM, data = bs)
summary(TEPTDM.m)
cbind(coef(TEPTDM.m), confint.default(TEPTDM.m))

# ROI0
sd(bs$ROI0)

ROI0.m = lm(y ~ ROI0, data = bs)
summary(ROI0.m)
cbind(coef(ROI0.m), confint.default(ROI0.m))

# ROI0B
table(bs$ROI0B)

ROI0B.m = lm(y ~ ROI0B, data = bs)
summary(ROI0B.m)

# ROI1
ROI1.m = lm(y ~ ROI1, data = bs)
summary(ROI1.m)

# ROI1B
ROI1B.m = lm(y ~ ROI1B, data = bs)
summary(ROI1B.m)

# Fracture1
Fracture1.m = lm(y ~ Fracture1, data = bs)
summary(Fracture1.m)

# Fracture1B
Fracture1B.m = lm(y ~ Fracture1B, data = bs)
summary(Fracture1B.m
)
# ROI2
ROI2.m = lm(y ~ ROI2, data = bs)
summary(ROI2.m)

# ROI2B 
ROI2B.m = lm(y ~ ROI2B, data = bs)
summary(ROI2B.m)

# Fracture2
Fracture2.m = lm(y ~ Fracture2, data = bs)
summary(Fracture2.m)

# Fracture2B
Fracture2B.m = lm(y ~ Fracture2B, data = bs)
summary(Fracture2B.m)

# IRAESDiabete
IRAESDiabete.m = lm(y ~ IRAESDiabete, data = bs)
summary(IRAESDiabete.m)

# IRAESColite
IRAESColite.m = lm(y ~ IRAESColite, data = bs)
summary(IRAESColite.m)

# IRAESHepatite
IRAESHepatite.m = lm(y ~ IRAESHepatite, data = bs)
summary(IRAESHepatite.m)

# IRAESI.surrenale
#table(bs$IRAESI.surrenale)

#IRAESRhumatisme
table(bs$IRAESRhumatisme)

# IRAESHypophysite
table(bs$IRAESHypophysite)

# IRAESPNP
table(bs$IRAESPNP)

# IRAESHyperthyroidie
table(bs$IRAESHyperthyroidie)

# IRAESAutre
table(bs$IRAESAutre)

# table
table(bs$Zometa)

bs$Zometa.f = as.factor(as.character(bs$Zometa))

Zometa.m = lm(y ~ Zometa.f, data = bs)
summary(Zometa.m)
cbind(coef(Zometa.m), confint.default(Zometa.m))


# Zometaz
table(bs$Zometaz)

Zometaz.m = lm(y ~ Zometaz, data = bs)
summary(Zometaz.m)

# linear regression

str(bs$ROI0)
str(bs$ROI1)

table(bs$ROI0, bs$ROI1)

bs$ROId = bs$ROI1 - bs$ROI0
str(bs$ROId)

# variable traitement----

#Ipilimumab = "Traitement par Ipilumab (0 = patient naif de traitement, 1 = patient ayant reçu ≥ 1 perfusion)",
#C1Ipi = "Date de la premiere cure d Ipilumab",
#CxIpi = "Date de la derniere cure d Ipilumab ",
#Ipi1A =	"Nombre de cure d Ipilumab a 1 an de la premiere cure",
#Ipi2A = "Nombre de cure d Ipilumab entre 1 et 2 ans apres la premiere cure",
#IpiTot = "Nombre total de cure d Ipilumab",
#Nivolumab = "Traitement par Nivolumab (0 = patient naif de traitement, 1 = patient ayant reçu ≥ 1 perfusion)",
#C1Nivo = "Date de la premiere cure de Nivolumab",
#CxNivo = "Date de la derniere cure de Nivolumab", 
#Nivo1A = "Nombre de cure de Nivolumab a 1 an de la premiere cure",
#Nivo2A = "Nombre de cure de Nivolumab entre 1 et 2 ans apres la premiere cure",
#NivoTot	= "Nombre total de cure de Nivolumab",
#Pembrolizumab = "Traitement par Pembrolizumab (0 = patient naif de traitement, 1 = patient ayant reçu ≥ 1 perfusion)",
#C1Pembro = "Date de la premiere cure de Pembrolizumab",
#CxPembro	= "Date de la derniere cure de Pembrolizumab",
#Pembro1A	= "Nombre de cure de Pembrolizumab a 1 an de la premiere cure",
#Pembro2A	= "Nombre de cure de Pembrolizumab entre 1 et 2 ans apres la premiere cure",
#PembroTot	= "Nombre total de cure de Pembrolizumab"

table(bs$Ipilimumab, useNA = "always")
table(bs$Nivolumab, useNA = "always")
table(bs$Pembrolizumab, useNA = "always")

table(bs$Ipilimumab, bs$Nivolumab, useNA = "always")

bs <- bs %>%
  mutate(ttt = case_when(
    Ipilimumab == 1 & Nivolumab == 0 & Pembrolizumab == 0 ~ "ipi",
    Ipilimumab == 1 & Nivolumab == 1 & Pembrolizumab == 0 ~ "ipiniv",
    Ipilimumab == 1 & Nivolumab == 1 & Pembrolizumab == 1 ~ "ipinivpem",
    Ipilimumab == 0 & Nivolumab == 1 & Pembrolizumab == 0 ~ "niv",
    Ipilimumab == 0 & Nivolumab == 1 & Pembrolizumab == 1 ~ "nivpem",
    Ipilimumab == 0 & Nivolumab == 0 & Pembrolizumab == 1 ~ "pem",
    Ipilimumab == 1 & Nivolumab == 0 & Pembrolizumab == 1 ~ "ipipem",
  ))
table(bs$ttt, useNA = "always")

# nombre de cure total 

# ipilimumab
### warning: il y a des discordances dans le calcul 
bs$IpiTOT = bs$Ipi1A + bs$Ipi2A
bs$IpiTot
table(bs$IpiTot, bs$IpiTOT, useNA = "always")

# nombre de cures en fonction du traitement 

bs <- bs %>%
  mutate(cures = case_when(
    ttt == "ipi" ~ IpiTot,
    ttt == "ipiniv" ~ (IpiTot + NivoTot),
    ttt == "ipinivpem" ~ (IpiTot + NivoTot + PembroTot),
    ttt == "niv" ~ NivoTot, 
    ttt == "nivpem" ~ (NivoTot + PembroTot),
    ttt == "pem" ~ PembroTot, 
    ttt == "ipipem" ~ (IpiTot + PembroTot)
  ))
table(bs$cures, useNA = "always")

# linear regression with ROI delta----

# variables----

str(bs$ROId)

# Sexe =	"0 = femme ; 1 = homme",
# Type = "Type de melanome (SSM : melanome supeficiel extensif, NM : melanome nodulaire, ALM : melanome acral lentigineux, LM : lentigo malin, MLM : melanome des muqueuses, CM : melanome choroidien)",
# StadeJ0 = "Stadification du cancer a l inclusion",
# IMC0	= "IMC a l inclusion en kg/m2",
# IMC1	= "IMC a 1 an de la premiere cure en kg/m2",
# Tabac 	= "Statu tabagique (0 = absence de tabagisme, 1 = tabagisme actif, 2 = tabagisme sevre)",
# Alcool	= "Statu alcoolique (0 = absence de consommation chronique d alcool, 1 = presence d une intoxication alcoolique chronique)",
# Diabete	= "Antecedent de diabete a l inclusion",
# CTC	= "Corticotherapie a partir de l inclusion",
# IRC	= "Antecedent d insuffisance renale chronique a l inclusion definie par un DFG < 60mL/min",
# Hyperthryoidie = "Antecedent d hyperthyroidie a l inclusion",
# IPP	= "Traitement par Inhibiteur de la pompe a proton a l inclusion",
# PAL1	= "Phosphatases alcalines a 1 an de la premiere cure en U/L",
# PAL2	= "Phosphatases alcalines a 2 ans de la premiere cure en U/L",
# BOR1	= "Best overall response a 1 an de la premiere cure",
# BOR2	= "Best overall response a 2 ans de la premiere cure",
# Zometa	= "Cure de zometa (0 = absence de cure realisee ; 1 = antecedent d au moins une cure de Zometa au cours du suivi)",

bs$Zometa01 = if_else(bs$Zometa == 0, "0", "1")

lm.d = lm(ROId ~ Sexe + Type + StadeJ0 + IMC0 + Tabac + Alcool + #Diabète +
            CTC + IRC + Hyperthyroidie + IPP + PAL1 + BOR1 + Zometa01, data = bs)
summary(lm.d)
cbind(coef(lm.d), confint.default(lm.d))

# model diagnostic----

model.diag.metrics <- augment(lm.d)
head(model.diag.metrics)

ggplot(model.diag.metrics, aes(Sexe, ROId)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = ROId, yend = .fitted), color = "red", size = 0.3)

# diagnostic plot----

par(mfrow = c(1,1))
plot(lm.d)

# residuals vs fitted = linear relationship assumptions. Line should be horizontal 
# normal qq = residuals are normally distributed 
# scale-location (or spread-location) = check homogeneity of variance of residuals homoscendasticity 
# residulas vs leverage = identify influential cases

# cook's distance 
plot(lm.d, 4)

#---------------

# create a new dataset with exact 

bsnew <- data.frame(
  ID = c(1:165)
)
View(bsnew)

# duplicating dataset

rep_bs <- cbind(bsnew, rep(row.names(bsnew), each = 2))
View(rep_bs)
names(rep_bs)

rep_bs <- as_tibble(rep_bs)
rep_bs <- rep_bs %>% rename(
  #new name = old name
  "ID2" = "rep(row.names(bsnew), each = 2)")

rep_bs = rep_bs[, -c(1)]
names(rep_bs)

# new dataset-----
names(rep_bs)
dim(rep_bs)

rep_bs <- as_tibble(rep_bs)
rep_bs <- rep_bs %>% rename(
  #new name = old name
  "ID" = "ID2")

bs <- as_tibble(bs)
bs <- bs %>% rename(
  #new name = old name
  "ID" = "Num")

mubs <- merge(rep_bs, bs, by.x = "ID", by.y = "ID",
              all.x = TRUE, all.y = FALSE)

str(mubs$ID)
mubs$ID = as.numeric(as.character(mubs$ID))

# create a count variable 
library("sqldf")

mubs_oneclass <- sqldf("SELECT a.*, COUNT(*) count
              FROM mubs a, mubs b
              WHERE a.ID = b.ID AND b.ROWID <= a.ROWID
              GROUP BY a.ROWID"
)
View(mubs_oneclass)
