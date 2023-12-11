#-packages----
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

#-data----
setwd("C:/Users/cerasuolo-d.CHU-CAEN/Desktop/thèse_Cousin Juliette_AVC_DC/personnes-temps et incidence")
base <- read_excel("base_263com_2020.xlsx")

dim(base)
names(base)

#sum(is.na(base$SEXE1_AGEPYR1006))
#str(base$SEXE1_AGEPYR1000) 

base$totline = base$SEXE1_AGEPYR1000 + base$SEXE1_AGEPYR1003 + base$SEXE1_AGEPYR1006 + base$SEXE1_AGEPYR1011 + base$SEXE1_AGEPYR1018 + base$SEXE1_AGEPYR1025 + 
  base$SEXE1_AGEPYR1040 + base$SEXE1_AGEPYR1055 + base$SEXE1_AGEPYR1065 + base$SEXE1_AGEPYR1080 + base$SEXE2_AGEPYR1000 + base$SEXE2_AGEPYR1003 + base$SEXE2_AGEPYR1006 + base$SEXE2_AGEPYR1011 +
  base$SEXE2_AGEPYR1018 + base$SEXE2_AGEPYR1025 + base$SEXE2_AGEPYR1040 + base$SEXE2_AGEPYR1055 + base$SEXE2_AGEPYR1065 + base$SEXE2_AGEPYR1080

# addition des valeurs d'une variable
total <- sum(base$totline, na.rm = T)
total
# 411 797

#-patient's data----
jc <- read_excel("C:/Users/cerasuolo-d.CHU-CAEN/Desktop/thèse_Cousin Juliette_AVC_DC/bdd_HSA.xlsx")
# 98 (events) 33 (variables / characteristics of the patients) 

## selection des anevrismes rompus
jc2 <- subset(jc, !(jc$Etio_crypto_typique == 1) & !(Etio_crypto_atypique == 1)
              & !(Etio_malform == 1)) 

#-datasets by year of the event----
eve2017 <- jc2 %>% filter(year(DATE_EVE) == 2017)
eve2018 <- jc2 %>% filter(year(DATE_EVE) == 2018)
eve2019 <- jc2 %>% filter(year(DATE_EVE) == 2019)
eve2020 <- jc2 %>% filter(year(DATE_EVE) == 2020)
dim(eve2020)
names(eve2020)
# 23 patients with an event 
eve2021 <- jc2 %>% filter(year(DATE_EVE) == 2021)


#-create a new dataset with Caen Metropoly population----
require(tibble)
data <- data.frame(num_inclusion = 1:411797, start = as.Date("01/01/2020", "%d/%m/%Y"), DATE_EVE = as.Date("31/12/2020", "%d/%m/%Y"))
# where 411797 == population in the Caen Metropole
# start == FUP start date
# DATE_EVE == end of the follow up (for this population, there is no event) 

#-merging data with the events from 2020----
all20 = merge(data, eve2020, by.x = "num_inclusion", by.y = "num_inclusion", all.x = TRUE, all.y = TRUE)

names(all20)

#-create the event---

all20$DATE_EVE.yMISS <- as.numeric(all20$DATE_EVE.y)
all20$DATE_EVE.yMISS[is.na(all20$DATE_EVE.yMISS)] <- 999999
table(all20$DATE_EVE.yMISS)

all20 <- all20 %>%
  mutate(EVENT = case_when(
    DATE_EVE.yMISS == 999999 ~ "no",
    DATE_EVE.yMISS > 999999 ~ "yes"
                                    ))

table(all20$EVENT) # event and no event

all20 <- all20 %>%
  mutate(fup_end = case_when(
    EVENT == "yes" ~ as.Date(DATE_EVE.y),
    EVENT == "no" ~ as.Date(DATE_EVE.x)
  ))

all20 <- as_tibble(all20)
all20 <- all20 %>% rename(
  # new name = old name,
  "DATE_EVE" = "DATE_EVE.y")

all20 = subset(all20, select = -c(DATE_EVE.x))




#-event date is missing 
all20$missED <- as.numeric(all20$DATE_EVE)
all20$missED[is.na(all20$DATE_EVE)] <- 999999
table(all20$missED) # 411774 missing

all20 <- all20 %>%
  mutate(missEDb = case_when(
    missED == 999999 ~ "yes", 
    missED > 999999 ~ "no"
  ))
table(all20$missEDb)


all20$jour = rep("01", times = 411797)
all20$mois = rep("01", times = 411797)
all20$year = rep("2020", times = 411797)

require(lubridate)
all20$DATE_END = as.numeric(paste(all20$year, all20$mois, all20$jour, sep = ""))
all20$DATE_END = ymd(all20$DATE_END)

all20DATE_EVEall = if_else(all20$missEDb == "yes", as.Date(all20$DATE_EVE), as.Date(all20$DATE_END))
