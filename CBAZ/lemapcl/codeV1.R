getwd()
#setwd("/Users/damianocerasuolo/Desktop/UBRC/UBRS_CONSULT_MAC/LeMapihan_Clarisse") 
setwd("P:/CONSULTATION/Bazille_C/KREIN")

#-PACKAGES-----------------------------------------------------------------------

require("readxl")
require("tableone")
require("tidyverse")
require ("survival")
require("cmprsk")
require("tidycmprsk")
require("lmtest")

#-DATABASE "REIN"----------------------------------------------------------------
rec <- read_excel("clar_db.xlsx", na = "NA")
summary(rec)
dim(rec)

names(rec)

#-TYPE HISTO---------------------------------------------------------------------

table(rec$type_histo, useNA = "always")

cecl <- rec[(rec$type_histo=="cellule claire"),]
dim(cecl)
# [1] 120  26 (120 tumeurs cellules claires)

#-CELL CLAIR, VAR TRANSF--------------------------------------------------------

cecl$SSRT2a_Hscore = as.numeric(as.character(cecl$SSRT2a_Hscore))
cecl$PSMA_Hscore = as.numeric(as.character(cecl$PSMA_Hscore))
cecl$R = as.factor(cecl$R)
cecl$nbre_spotanalysables = as.factor(cecl$nbre_spotanalysables)

mean(cecl$SSRT2a_Hscore, na.rm = "TRUE")
median(cecl$SSRT2a_Hscore)
min(cecl$SSRT2a_Hscore)
max(cecl$SSRT2a_Hscore)

cecl <- cecl %>%
  mutate(SSRT2a_c = case_when(
    SSRT2a_Hscore >= 0 & SSRT2a_Hscore < 20 ~ "1", 
    SSRT2a_Hscore >= 20 & SSRT2a_Hscore <= 100 ~ "2",
    SSRT2a_Hscore > 100 ~ "3"
  ))

table(cecl$SSRT2a_c, useNA = "always")

cecl <- cecl %>%
  mutate(PSMA_c = case_when(
    PSMA_Hscore >= 0 & PSMA_Hscore < 20 ~ "1",
    PSMA_Hscore >= 20 & PSMA_Hscore <= 100 ~ "2",
    PSMA_Hscore > 100 ~ "3"
  ))

table(cecl$PSMA_c, useNA = "always")

str(cecl$SSRT2a_c)
cecl <- cecl %>%
  mutate(SSRT2a_c2 = case_when(
    SSRT2a_c == "1" ~ "1",
    SSRT2a_c == "2" ~ "1",
    SSRT2a_c == "3" ~ "2"
  ))
cecl$SSRT2a_c2 = as.factor(cecl$SSRT2a_c2)

str(cecl$ISUP)
cecl <- cecl %>%
  mutate(ISUP2 = case_when(
    ISUP == "1" | ISUP == "2" ~ "1",
    ISUP == "3" ~ "2",
    ISUP == "4" ~ "3"
  ))
cecl$ISUP2 = as.factor(cecl$ISUP2)

cecl <- cecl %>%
  mutate(ISUP3 = case_when(
    ISUP == "1" | ISUP == "2" ~ "1",
    ISUP == "3" | ISUP == "4" ~ "2"
  ))
cecl$ISUP3 = as.factor(cecl$ISUP3)

cecl <- cecl %>%
  mutate(R2 = case_when(
    R == 0 ~ "0",
    R == 1 | R == 2 ~ "1"
  ))
table(cecl$R2)

#-DESCRIPTIVE CELL CLAIRES------------------------------------------------------

dput(names(cecl))

variables <- c("sexe", "recidive", 
               "metastasis",  
               "deces", "deces_ccr", 
               "TNM", "tnm_short", "ISUP", "R", "nbre_spotanalysables", 
               "SSRT2a_c", "PSMA_c")

cvariables <- c("sexe", "recidive", 
                "metastasis",  
                "deces", "deces_ccr", 
                "TNM", "tnm_short", "ISUP", "R", "nbre_spotanalysables",
                "SSRT2a_c", "PSMA_c")

# CREATE THE DESCRIPTIVE TABLE
tab1 = CreateTableOne(vars = variables, data = cecl, factorVars = cvariables)
print(tab1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)


#-FOLLOW UP TIMES-------------------------------------------------------------

#-FUP TO DEATH------

str(cecl$date_chirurgie)
str(cecl$date_deces)
str(cecl$ddnouvelels)

cecl <- cecl %>%
  mutate(fup_death = case_when(
    deces == 1 ~ (as.Date(date_deces) - as.Date(date_chirurgie)),
    deces == 0 ~ (as.Date(ddnouvelels) - as.Date(date_chirurgie))
  ))
table(cecl$fup_death, useNA = "always")
str(cecl$fup_death)
cecl$fup_deathN = as.numeric(cecl$fup_death)

table(cecl$fup_death)
mean(cecl$fup_death, na.rm = T)
min(cecl$fup_death, na.rm = T)
max(cecl$fup_death, na.rm = T)
quantile(cecl$fup_death, na.rm = T)


# notes 
# 2 values are missing -> dates are missing for 2 patients 

#-ONLY RELAPSE

cecl <- cecl %>% 
  mutate(fup_onlyrelapse = case_when(
    recidive == 1 & (deces == 0 | deces == 1) ~ (as.Date(date_recidiveORmeta) - as.Date(date_chirurgie))
  ))
table(cecl$fup_onlyrelapse)
mean(cecl$fup_onlyrelapse, na.rm = T)
min(cecl$fup_onlyrelapse, na.rm = T)
max(cecl$fup_onlyrelapse, na.rm = T)
quantile(cecl$fup_onlyrelapse, na.rm = T)

#-FUP TO RELAPSE < FUP TO DEATH
#
#cecl <- cecl %>% 
#  mutate(fup_reldeath = case_when(
#    as.numeric(fup_onlyrelapse) < as.numeric(fup_death) ~ "1",
#  ))
#table(cecl$fup_reldeath, useNA = "always")

#-FUP TO RELAPSE-----

str(cecl$date_recidiveORmeta)

cecl <- cecl %>%
  mutate(fup_relapse = case_when(
    recidive == 1 & deces == 0 ~ (as.Date(date_recidiveORmeta) - as.Date(date_chirurgie)),
    recidive == 0 & deces == 0 ~ (as.Date(ddnouvelels) - as.Date(date_chirurgie)),
    deces == 1 ~ (as.Date(date_deces) - as.Date(date_chirurgie))
  ))
table(cecl$fup_relapse, useNA = "always")

#-UNIVARIATE ANALYSIS FOR DEATH-----------------------------------------------

# sexe
table(cecl$sexe, cecl$deces, useNA = "always")
(prop.table(table(cecl$sexe, cecl$deces), margin = 2))*100
cox_sex = coxph(Surv(fup_death, deces==1) ~ sexe, data = cecl)
summary(cox_sex)

# recidive
table(cecl$recidive, cecl$deces, useNA = "always")
(prop.table(table(cecl$recidive, cecl$deces), margin = 2))*100
cox_recidive = coxph(Surv(fup_death, deces==1) ~ recidive, data = cecl)
summary(cox_recidive)

# metastasis
table(cecl$metastasis, cecl$deces, useNA = "always")
prop.table(table(cecl$metastasis, cecl$deces), margin = 2)*100
cox_metastasis = coxph(Surv(fup_death, deces ==1) ~ metastasis, data = cecl)
summary(cox_metastasis)

# TNM
cox_TNM = coxph(Surv(fup_death, deces == 1) ~ TNM, data = cecl)
summary(cox_TNM)

# tnm_short
table(cecl$tnm_short, cecl$deces, useNA = "always")
(prop.table(table(cecl$tnm_short, cecl$deces), margin = 2))*100
cox_tnms = coxph(Surv(fup_death, deces==1) ~ tnm_short, data = cecl)
summary(cox_tnms)

# ISUP
cox_ISUP = coxph(Surv(fup_death, deces==1) ~ ISUP, data = cecl)
summary(cox_ISUP)

table(cecl$ISUP2, cecl$deces, useNA = "always")
(prop.table(table(cecl$ISUP2, cecl$deces), margin = 2))*100
cox_ISUP2 = coxph(Surv(fup_death, deces==1) ~ ISUP2, data = cecl)
summary(cox_ISUP2)

table(cecl$ISUP3, cecl$deces, useNA = "always")
(prop.table(table(cecl$ISUP3, cecl$deces), margin = 2))*100
cox_ISUP3 = coxph(Surv(fup_death, deces==1) ~ ISUP3, data = cecl)
summary(cox_ISUP3)

# R
cox_R = coxph(Surv(fup_death, deces==1) ~ R, data = cecl)
summary(cox_R)

table(cecl$R2, cecl$deces, useNA = "always")
(prop.table(table(cecl$R2, cecl$deces), margin = 2))*100
cox_R2 = coxph(Surv(fup_death, deces==1) ~ R2, data = cecl)
summary(cox_R2)

# nbre_spotanalysables
table(cecl$nbre_spotanalysables, cecl$deces, useNA = "always")
(prop.table(table(cecl$nbre_spotanalysables, cecl$deces), margin = 2))*100
cox_span = coxph(Surv(fup_death, deces==1) ~ nbre_spotanalysables, data = cecl)
summary(cox_span)

cox_E = coxph(Surv(fup_death, deces==1) ~ 1, data = cecl)
lrtest(cox_span, cox_E)

# SSRT2a_c
cox_ssrt2a = coxph(Surv(fup_death, deces==1) ~ SSRT2a_c, data = cecl)
summary(cox_ssrt2a)

table(cecl$SSRT2a_c2, cecl$deces, useNA = "always")
(prop.table(table(cecl$SSRT2a_c2, cecl$deces), margin = 2))*100
cox_SSRT2ac2 = coxph(Surv(fup_death, deces==1) ~ SSRT2a_c2, data = cecl)
summary(cox_SSRT2ac2)

# PSMA_c
table(cecl$PSMA_c, cecl$deces, useNA = "always")
(prop.table(table(cecl$PSMA_c, cecl$deces), margin = 2))*100
cox_psma = coxph(Surv(fup_death, deces==1) ~ PSMA_c, data = cecl)
summary(cox_psma)

lrtest(cox_psma, cox_E)

#-MULTIVARIATE ANALYSIS FOR DEATH---------------------------------------------

cox_death = coxph(Surv(fup_death, deces==1) ~ sexe + recidive + metastasis +
                    tnm_short + ISUP2 + R + SSRT2a_c2 + PSMA_c, data = cecl)
summary(cox_death)

#-MULTIVARIATE COMPETITIVE----------------------------------------------------

# new event 

table(cecl$recidive, useNA = "always")
#   0    1 <NA> 
# 107   13    0 
table(cecl$deces, useNA = "always")
#   0    1 <NA> 
#  71   49    0 

table(cecl$recidive, cecl$deces)
#   0  1
#0 67 40
#1  4  9

cecl <- cecl %>% 
  mutate(decrec = case_when(
    deces == 1 & recidive == 0 ~ "2", #deces
    deces == 1 & recidive == 1 ~ "1", #recidive
    deces == 0 & recidive == 1 ~ "1", #recidive
    deces == 0 & recidive == 0 ~ "0"  #pas d evenement
  ))
table(cecl$decrec, useNA = "always")

cecl <- cecl %>% 
  mutate(decrec2 = case_when(
    deces == 1 & recidive == 0 ~ "2", #deces
    deces == 1 & recidive == 1 ~ "3", #recidive et deces
    deces == 0 & recidive == 1 ~ "1", #recidive
    deces == 0 & recidive == 0 ~ "0"  #pas d evenement
  ))
table(cecl$decrec2, useNA = "always")

#-CHECKS FOR COMPETITIVE RISKS--------------------------------------------

names(cecl)

quantile(cecl$fup_death, na.rm = TRUE)
quantile(cecl$fup_onlyrelapse, na.rm = TRUE)
# 2280 max

table(cecl$deces, cecl$recidive, useNA = "always")

# time to dernier nouvelles et time to death

cecl$timedn = (as.Date(cecl$ddnouvelels) - as.Date(cecl$date_chirurgie))
max(cecl$timedn, na.rm = T)
cecl$timedeath = (as.Date(cecl$date_deces) - as.Date(cecl$date_chirurgie))
max(cecl$timedeath, na.rm = T)

# time between relapse and death 
# cette variable prend en compte la variable "decrec2"

cecl <- cecl %>% 
  mutate(relde = case_when(
    decrec2 == "3" ~ (as.Date(date_deces) - as.Date(date_recidiveORmeta))
  ))
quantile(cecl$relde, na.rm = TRUE)

cecl <- cecl %>%
  mutate(ttrd = case_when(
    decrec == "2" ~ (as.Date(date_deces) - as.Date(date_chirurgie)), 
    decrec == "1" ~ (as.Date(date_recidiveORmeta) - as.Date(date_chirurgie)),
    decrec == "0" ~ (as.Date(ddnouvelels) - as.Date(date_chirurgie))
  ))
table(cecl$ttrd, useNA = "always")
str(cecl$ttrd)
cecl$ttrdn = as.numeric(cecl$ttrd)

cecl <- cecl %>%
  mutate(fup3 = case_when(
    ttrdn <= 2280.0 ~ "1",
    ttrdn > 2280.0 & ttrdn <= 3210 ~ "2",
    ttrdn > 3210 ~ "3"
  ))
table(cecl$fup3, useNA = "always")

# time entre chirurgie et relapse 
# time entre chirurgie et death 
# time entre chirurgie et date des dernieres nouvelles 

cecl <- cecl %>%
  mutate(chirrel = case_when(
    decrec == "1" ~ (as.Date(date_recidiveORmeta) - as.Date(date_chirurgie))
  ))

cecl <- cecl %>% 
  mutate(chirdeath = case_when(
    decrec2 == "2" | decrec2 == "3" ~ (as.Date(date_deces) - as.Date(date_chirurgie))
  ))

cecl <- cecl %>%
  mutate(chirddn = case_when(
    decrec == 0 ~ (as.Date(ddnouvelels) - as.Date(date_chirurgie))
  ))

quantile(cecl$chirrel, na.rm = TRUE)
quantile(cecl$chirdeath, na.rm = TRUE)
quantile(cecl$chirddn, na.rm = TRUE)

# dernier suivi à 2280 (delai de derniere recidive)

table(cecl$fup3)
str(cecl$fup3)

cecl2 = cecl[(cecl$fup3 == "1"),]
dim(cecl2)

cecl2 <- cecl2 %>% 
  mutate(decrec = case_when(
    deces == 1 & recidive == 0 ~ "2", #deces
    deces == 1 & recidive == 1 ~ "1", #recidive
    deces == 0 & recidive == 1 ~ "1", #recidive
    deces == 0 & recidive == 0 ~ "0"  #pas d evenement
  ))

# fup deces 

cecl2 <- cecl2 %>%
  mutate(chirdec = case_when(
    decrec == "2" ~ (as.Date(date_deces) - as.Date(date_chirurgie))
  ))
quantile(cecl2$chirdec, na.rm = T)
mean(cecl2$chirdec, na.rm = T)
# Time difference of 875 days

cecl2 <- cecl2 %>%
  mutate(chirrec = case_when(
    decrec == "1" ~ (as.Date(date_recidiveORmeta) - as.Date(date_chirurgie))
  ))
quantile(cecl2$chirrec, na.rm = T)
mean(cecl2$chirrec, na.rm = T)
# Time difference of 355.3846 days

#-UNIVARIATE ANALYSIS FOR RELAPSE---------------------------------------------

# sexe
cox_sex = coxph(Surv(fup_relapse, recidive==1) ~ sexe, data = cecl)
summary(cox_sex)

# metastasis
cox_metastasis = coxph(Surv(fup_relapse, recidive ==1) ~ metastasis, data = cecl)
summary(cox_metastasis)

# TNM
cox_TNM = coxph(Surv(fup_relapse, recidive == 1) ~ TNM, data = cecl)
summary(cox_TNM)

# tnm_short
cox_tnms = coxph(Surv(fup_relapse, recidive==1) ~ tnm_short, data = cecl)
summary(cox_tnms)

# ISUP
cox_ISUP = coxph(Surv(fup_relapse, recidive==1) ~ ISUP, data = cecl)
summary(cox_ISUP)

cox_ISUP2 = coxph(Surv(fup_relapse, recidive==1) ~ ISUP2, data = cecl)
summary(cox_ISUP2)

# R
cox_R = coxph(Surv(fup_relapse, recidive==1) ~ R, data = cecl)
summary(cox_R)

# nbre_spotanalysables
cox_span = coxph(Surv(fup_relapse, recidive==1) ~ nbre_spotanalysables, data = cecl)
summary(cox_span)

# SSRT2a_c
cox_ssrt2a = coxph(Surv(fup_relapse, recidive==1) ~ SSRT2a_c, data = cecl)
summary(cox_ssrt2a)

cox_SSRT2ac2 = coxph(Surv(fup_relapse, recidive==1) ~ SSRT2a_c2, data = cecl)
summary(cox_SSRT2ac2)

# PSMA_c
cox_psma = coxph(Surv(fup_relapse, recidive==1) ~ PSMA_c, data = cecl)
summary(cox_psma)

#-MULTIVARIATE ANALYSIS FOR RELAPSE-------------------------------------------

cox_recidive = coxph(Surv(fup_relapse, recidive==1) ~ sexe + metastasis +
                       tnm_short + ISUP2 + R + SSRT2a_c2 + PSMA_c, data = cecl)
summary(cox_recidive)
