#-data----

library(prettyR)
library(questionr)
library(ggplot2)

require("here")
require("readxl")
require("tidyverse")

library(openxlsx)
library("lubridate")
library(survival)
library(survminer)

library("epitools")
require("tibble")


setwd("C:/Users/cerasuolo-d.CHU-CAEN/Desktop/thèse_Cousin Juliette_AVC_DC/personnes-temps et incidence")
base <- read_excel("base_263com_2020.xlsx")


dim(base)
names(base)

sum(is.na(base$SEXE1_AGEPYR1006))





str(base$SEXE1_AGEPYR1000) # num

base$totline = base$SEXE1_AGEPYR1000 + base$SEXE1_AGEPYR1003 + base$SEXE1_AGEPYR1006 + base$SEXE1_AGEPYR1011 + base$SEXE1_AGEPYR1018 + base$SEXE1_AGEPYR1025 + 
  base$SEXE1_AGEPYR1040 + base$SEXE1_AGEPYR1055 + base$SEXE1_AGEPYR1065 + base$SEXE1_AGEPYR1080 + base$SEXE2_AGEPYR1000 + base$SEXE2_AGEPYR1003 + base$SEXE2_AGEPYR1006 + base$SEXE2_AGEPYR1011 +
  base$SEXE2_AGEPYR1018 + base$SEXE2_AGEPYR1025 + base$SEXE2_AGEPYR1040 + base$SEXE2_AGEPYR1055 + base$SEXE2_AGEPYR1065 + base$SEXE2_AGEPYR1080


# addition des valeurs d'une variable
total <- sum(base$totline, na.rm = T)
total
# 411 797



#-Creation de base des evenements pour chaque annee----

##bdd de Juliette Cousin
jc <- read_excel("C:/Users/cerasuolo-d.CHU-CAEN/Desktop/thèse_Cousin Juliette_AVC_DC/bdd_HSA.xlsx")

## selection des anevrismes rompus
jc2 <- subset(jc, !(jc$Etio_crypto_typique == 1) & !(Etio_crypto_atypique == 1)
              & !(Etio_malform == 1)) 

eve2017 <- jc2 %>% filter(year(DATE_EVE) == 2017)

eve2018 <- jc2 %>% filter(year(DATE_EVE) == 2018)

eve2019 <- jc2 %>% filter(year(DATE_EVE) == 2019)

eve2020 <- jc2 %>% filter(year(DATE_EVE) == 2020)

eve2021 <- jc2 %>% filter(year(DATE_EVE) == 2021)


#-create a new dataset----
require(tibble)

data <- data.frame(num_inclusion = 1:411797, start = as.Date("01/01/2020", "%d/%m/%Y"),
                   DATE_EVE = as.Date("31/12/2020", "%d/%m/%Y"))
View(data)

alldatanormandy20 = merge(data, eve2020, by.x = "num_inclusion", by.y = "num_inclusion", all.x = TRUE, 
                          all.y = TRUE)

View(alldatanormandy20)



names(alldatanormandy20)

str(alldatanormandy20)



# creation date 2020 pour les patients ayant eu un evenement 

eve2020$jour = rep("01", times = 23)
eve2020$mois = rep("01", times = 23)
eve2020$year = rep("2020", times = 23)

eve2020$startdate = as.numeric(paste(eve2020$year, eve2020$mois, eve2020$jour, sep = ""))
require(lubridate)
eve2020startdate = ymd(eve2020$startdate)

eve2020$DATE_EVE = as.Date(eve2020$DATE_EVE) # YYYY-MM-DD

data <- data.frame(num_inclusion = 1:411797, start = as.Date("01/01/2020", "%d/%m/%Y"),
                   DATE_EVE = as.Date("31/12/2020", "%d/%m/%Y"))

str(eve2020startdate )
# eve2020$startdate = as.Date(eve2020$startdate) 
# pb : 57275-12-31

eve2020$startdate <- eve2020(startdate= as.Date("01/01/2020", "%d/%m/%Y")) 
# pb

eve2020$startdate <- data.frame(startdate= as.Date("01/01/2020", "%d/%m/%Y"))
# startdate$startdate

eve2020$startdate <- (as.Date("01/01/2020", "%d/%m/%Y"))
# date YYYY-MM-DD


eve2020$fup = as.Date(eve2020$DATE_EVE, "%d/%m/%Y") - as.Date(eve2020startdate, "%d/%m/%Y") 
eve2020$fup = eve2020$DATE_EVE - eve2020startdate 

sum(eve2020$fup)
# Time difference of 3 597 days

## personnes-jour patients = 3 597 
## personnes-jours des sujets sains = (411797-23)*365 = 411774 *365 = 150 297 510

# incidence en 2020
(23/(3597+150297510))*1000000 # incidence par million de personnes sur 365 jours

3597/12 # = 299.75 personnes-mois parmi les patients
411774 * 12 # = 4 941 288  personnes-mois parmi les sujets sains


(23/(299.75+ 4941288))*1000000 = # 4.654374 par million de personnes sur 12 mois
  (23/(299.75+4941288))*10000 # =  0.04654374 par 10000 personnes sur 12 mois


## incidence entre 2017 et 2021
(nb evenements de période étude/(4,63*(personnes-mois parmi les patients en 2020 +personnes-mois parmi les sujets sains en 2020))*1000000
  
  Pour les PT, si tu utilises le PM comme dénominateur, il suffit - toujours pour le dénominateur - de multiplier le nombre de sujets * le nombre des mois du suivi (3 ans = 36 mois + 6.5 mois en 2017 = 42.5 PM). 
  
  4 ans = 48 mois + 7,5 mois en 2017 =55,5 PM
  
  
  
  
