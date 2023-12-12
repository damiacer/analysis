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

library("ggsurvfit")
library("gtsummary")
library("tidycmprsk")

install.packages("devtools")
library("devtools")
# devtools::install_github("zabore/condsurv")
library(condsurv)

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
dim(eve2017)
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

#all20 <- as_tibble(all20)
#all20 <- all20 %>% rename(
  # new name = old name,
#  "DATE_EVE" = "DATE_EVE.y")

#all20 = subset(all20, select = -c(DATE_EVE.x))

all20$fup20 = as.Date(all20$fup_end) - as.Date(all20$start)
str(all20$fup20)
table(all20$fup20)

#-events in 2020: 23
#-incidence in 2020----

all20$fup20 = as.numeric(all20$fup20)

all20$pt20 = sum((all20$fup20))
(all20$pt20/365.25)*12

i20 = (23/4938024)*100000
i20 # 0.4657734

#-KM curve---

all20$EVENTf = as.factor(if_else(all20$EVENT == "no", "0", "1"))
all20$fup20M = as.numeric((all20$fup20/365.25)*12)
km20_fit <- survfit(Surv(fup20M, EVENTf) ~ 1, data = all20)
summary(km20_fit)

plot(km20_fit) # 
plot(km20_fit, fun = function(x) 1-x) # survival function

################################################################################

#-all events 

data1721 <- data.frame(num_inclusion = 1:411797, start = as.Date("15/05/2017", "%d/%m/%Y"), DATE_EVE = as.Date("31/12/2021", "%d/%m/%Y"))

all = merge(data1721, jc2, by.x = "num_inclusion", by.y = "num_inclusion", all.x = TRUE, all.y = TRUE)


#-create the event 2017-2021---

all$DATE_EVE.yMISS <- as.numeric(all$DATE_EVE.y)
all$DATE_EVE.yMISS[is.na(all$DATE_EVE.yMISS)] <- 999999
table(all$DATE_EVE.yMISS)

all <- all %>%
  mutate(EVENT = case_when(
    DATE_EVE.yMISS == 999999 ~ "no",
    DATE_EVE.yMISS > 999999 ~ "yes"
  ))

table(all$EVENT) # event and no event

all <- all %>%
  mutate(fup_end = case_when(
    EVENT == "yes" ~ as.Date(DATE_EVE.y),
    EVENT == "no" ~ as.Date(DATE_EVE.x)
  ))

all$fup1721 = as.Date(all$fup_end) - as.Date(all$start)
str(all$fup1721)
table(all$fup1721)


all$pt1721 = sum((all$fup1721))
(all$pt1721/365.25)*12

i1721 = (98/22875464)*100000
i1721 # 0.4284066

#-KM curve---

all$EVENTf1721 = as.factor(if_else(all$EVENT == "no", "0", "1"))
all$fup1721M = as.numeric((all$fup1721/365.25)*12)
km1721_fit <- survfit(Surv(fup1721M, EVENTf1721) ~ 1, data = all)
summary(km1721_fit)

plot(km20_fit) 
plot(km20_fit, fun = function(x) 1-x) # survival function
