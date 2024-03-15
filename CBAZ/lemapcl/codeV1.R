getwd()
setwd("/Users/damianocerasuolo/Desktop/UBRC/UBRS_CONSULT_MAC/LeMapihan_Clarisse") 
#setwd("P:/CONSULTATION/Bazille_C/KREIN")

#-PACKAGES-----------------------------------------------------------------------

require("readxl")
require("tableone")
require("tidyverse")
require ("survival")
require("cmprsk")
require("tidycmprsk")
require("lmtest")
require("nnet")
require("gtsummary")
require("stargazer")

#-DATABASE ---------------------------------------------------------------------
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
table(cecl$sexe, cecl$recidive, useNA = "always")
(prop.table(table(cecl$sexe, cecl$recidive), margin = 2))*100
cox_sexR = coxph(Surv(fup_relapse, recidive==1) ~ sexe, data = cecl)
summary(cox_sexR)

# metastasis
table(cecl$metastasis, cecl$recidive, useNA = "always")
prop.table(table(cecl$metastasis, cecl$recidive), margin = 2)*100
cox_metastasisR = coxph(Surv(fup_relapse, recidive ==1) ~ metastasis, data = cecl)
summary(cox_metastasisR)

# TNM
cox_TNM = coxph(Surv(fup_death, deces == 1) ~ TNM, data = cecl)
summary(cox_TNM)

# tnm_short
table(cecl$tnm_short, cecl$recidive, useNA = "always")
(prop.table(table(cecl$tnm_short, cecl$recidive), margin = 2))*100
cox_tnms = coxph(Surv(fup_relapse, recidive==1) ~ tnm_short, data = cecl)
summary(cox_tnms)

cox_E = coxph(Surv(fup_relapse, recidive == 1) ~ 1, data = cecl)
lrtest(cox_tnms, cox_E)

# ISUP
cox_ISUP = coxph(Surv(fup_death, deces==1) ~ ISUP, data = cecl)
summary(cox_ISUP)

table(cecl$ISUP2, cecl$deces, useNA = "always")
(prop.table(table(cecl$ISUP2, cecl$deces), margin = 2))*100
cox_ISUP2 = coxph(Surv(fup_death, deces==1) ~ ISUP2, data = cecl)
summary(cox_ISUP2)

table(cecl$ISUP3, cecl$recidive, useNA = "always")
(prop.table(table(cecl$ISUP3, cecl$recidive), margin = 2))*100
cox_ISUP3R = coxph(Surv(fup_relapse, recidive==1) ~ ISUP3, data = cecl)
summary(cox_ISUP3R)

# R
cox_R = coxph(Surv(fup_death, deces==1) ~ R, data = cecl)
summary(cox_R)

table(cecl$R2, cecl$recidive, useNA = "always")
(prop.table(table(cecl$R2, cecl$recidive), margin = 2))*100
cox_R2R = coxph(Surv(fup_relapse, recidive==1) ~ R2, data = cecl)
summary(cox_R2R)

# nbre_spotanalysables
table(cecl$nbre_spotanalysables, cecl$recidive, useNA = "always")
(prop.table(table(cecl$nbre_spotanalysables, cecl$recidive), margin = 2))*100
cox_spanR = coxph(Surv(fup_relapse, recidive==1) ~ nbre_spotanalysables, data = cecl)
summary(cox_spanR)

cox_E = coxph(Surv(fup_relapse, recidive==1) ~ 1, data = cecl)
lrtest(cox_span, cox_E)

# SSRT2a_c
cox_ssrt2a = coxph(Surv(fup_death, deces==1) ~ SSRT2a_c, data = cecl)
summary(cox_ssrt2a)

table(cecl$SSRT2a_c2, cecl$recidive, useNA = "always")
(prop.table(table(cecl$SSRT2a_c2, cecl$recidive), margin = 2))*100
cox_SSRT2ac2R = coxph(Surv(fup_relapse, recidive==1) ~ SSRT2a_c2, data = cecl)
summary(cox_SSRT2ac2R)

# PSMA_c
table(cecl$PSMA_c, cecl$recidive, useNA = "always")
(prop.table(table(cecl$PSMA_c, cecl$recidive), margin = 2))*100
cox_psmaR = coxph(Surv(fup_relapse, recidive==1) ~ PSMA_c, data = cecl)
summary(cox_psmaR)

lrtest(cox_psmaR, cox_E)

#-MULTIVARIATE ANALYSIS FOR RELAPSE-------------------------------------------

cox_recidive = coxph(Surv(fup_relapse, recidive==1) ~ sexe + metastasis +
                       tnm_short + ISUP2 + R + SSRT2a_c2 + PSMA_c, data = cecl)
summary(cox_recidive)

#-UNIVARIATE ANALYSIS FOR FREE FROM RELAPSE-----------------------------------

table(cecl$recidive)

cecl$recidiveFREE = if_else(cecl$recidive == 0, "1", "0")
table(cecl$recidive, cecl$recidiveFREE, useNA = "always")


# sexe
table(cecl$sexe, cecl$recidive, useNA = "always")
(prop.table(table(cecl$sexe, cecl$recidive), margin = 2))*100
cox_sexR = coxph(Surv(fup_relapse, recidiveFREE==1) ~ sexe, data = cecl)
summary(cox_sexR)

# metastasis
table(cecl$metastasis, cecl$recidive, useNA = "always")
prop.table(table(cecl$metastasis, cecl$recidive), margin = 2)*100
cox_metastasisR = coxph(Surv(fup_relapse, recidiveFREE ==1) ~ metastasis, data = cecl)
summary(cox_metastasisR)

# TNM
cox_TNM = coxph(Surv(fup_death, deces == 1) ~ TNM, data = cecl)
summary(cox_TNM)

# tnm_short
table(cecl$tnm_short, cecl$recidive, useNA = "always")
(prop.table(table(cecl$tnm_short, cecl$recidive), margin = 2))*100
cox_tnms = coxph(Surv(fup_relapse, recidiveFREE==1) ~ tnm_short, data = cecl)
summary(cox_tnms)

cox_E = coxph(Surv(fup_relapse, recidiveFREE == 1) ~ 1, data = cecl)
lrtest(cox_tnms, cox_E)

# ISUP
cox_ISUP = coxph(Surv(fup_death, deces==1) ~ ISUP, data = cecl)
summary(cox_ISUP)

table(cecl$ISUP2, cecl$deces, useNA = "always")
(prop.table(table(cecl$ISUP2, cecl$deces), margin = 2))*100
cox_ISUP2 = coxph(Surv(fup_death, recidiveFREE==1) ~ ISUP2, data = cecl)
summary(cox_ISUP2)

table(cecl$ISUP3, cecl$recidive, useNA = "always")
(prop.table(table(cecl$ISUP3, cecl$recidive), margin = 2))*100
cox_ISUP3R = coxph(Surv(fup_relapse, recidiveFREE==1) ~ ISUP3, data = cecl)
summary(cox_ISUP3R)

# R
cox_R = coxph(Surv(fup_death, deces==1) ~ R, data = cecl)
summary(cox_R)

table(cecl$R2, cecl$recidive, useNA = "always")
(prop.table(table(cecl$R2, cecl$recidive), margin = 2))*100
cox_R2R = coxph(Surv(fup_relapse, recidiveFREE==1) ~ R2, data = cecl)
summary(cox_R2R)

# nbre_spotanalysables
table(cecl$nbre_spotanalysables, cecl$recidive, useNA = "always")
(prop.table(table(cecl$nbre_spotanalysables, cecl$recidive), margin = 2))*100
cox_spanR = coxph(Surv(fup_relapse, recidiveFREE==1) ~ nbre_spotanalysables, data = cecl)
summary(cox_spanR)

cox_E = coxph(Surv(fup_relapse, recidiveFREE==1) ~ 1, data = cecl)
lrtest(cox_spanR, cox_E)

# SSRT2a_c
cox_ssrt2a = coxph(Surv(fup_death, recidiveFREE==1) ~ SSRT2a_c, data = cecl)
summary(cox_ssrt2a)

table(cecl$SSRT2a_c2, cecl$recidive, useNA = "always")
(prop.table(table(cecl$SSRT2a_c2, cecl$recidive), margin = 2))*100
cox_SSRT2ac2R = coxph(Surv(fup_relapse, recidiveFREE==1) ~ SSRT2a_c2, data = cecl)
summary(cox_SSRT2ac2R)

# PSMA_c
table(cecl$PSMA_c, cecl$recidive, useNA = "always")
(prop.table(table(cecl$PSMA_c, cecl$recidive), margin = 2))*100
cox_psmaR = coxph(Surv(fup_relapse, recidiveFREE==1) ~ PSMA_c, data = cecl)
summary(cox_psmaR)

lrtest(cox_psmaR, cox_E)

#-REC DATABASE------------------------------------------------------------

dim(rec)
names(rec)

table(rec$type_histo, useNA = "always")
# cellule claire    chromophobe     oncocytome     papillaire           <NA> 
# 120             20             10             20              0 

rec$SSRT2a_Hscore = as.numeric(as.character(rec$SSRT2a_Hscore))
rec <- rec %>%
  mutate(SSRT2a = case_when(
    SSRT2a_Hscore >= 0 & SSRT2a_Hscore < 20 ~ "1", 
    SSRT2a_Hscore >= 20 & SSRT2a_Hscore <= 100 ~ "2",
    SSRT2a_Hscore > 100 ~ "3"))
table(rec$SSRT2a, useNA = "always")

chisq.test(rec$SSRT2a, rec$type_histo, simulate.p.value = T)

rec$PSMA_Hscore = as.numeric(as.character(rec$PSMA_Hscore))
rec <- rec %>%
  mutate(PSMA_c = case_when(
    PSMA_Hscore >= 0 & PSMA_Hscore < 20 ~ "1",
    PSMA_Hscore >= 20 & PSMA_Hscore <= 100 ~ "2",
    PSMA_Hscore > 100 ~ "3"))
table(rec$PSMA_c, useNA = "always")

chisq.test(rec$PSMA_c, rec$type_histo, simulate.p.value = T)

str(rec$type_histo)
rec$type_histo = as.factor(rec$type_histo)
rec$type_histo2 <- relevel(rec$type_histo, ref = "cellule claire")
fit <- multinom(SSRT2a ~ type_histo, data = rec)
summary(fit)

str(rec$type_histo)
rec$type_histo2 <- relevel(rec$type_histo, ref = "cellule claire")
fit2 <- multinom(PSMA_c ~ type_histo, data = rec)

#z <- summary(fit)$coefficients/summary(fit)$standard.errors
#z
#
#p <- (1- pnorm(abs(z), 0, 1)) *2
#p
#
#exp(coef(fit))
#
#fit %>%
#tbl_regression(exponenitate = T)

#-fit (stargazer package)-------------------------------------------------
# avec fit = SSRT2a ~ type_histo
stargazer(fit, type = "text")
#==================================================
#  Dependent variable:     
#  ----------------------------
#  2              3      
#(1)            (2)     
#--------------------------------------------------
#  type_histochromophobe    -2.591**      -5.621***  
#  (1.156)        (1.505)   
#
#type_histooncocytome      5.398          4.671    
#(42.942)      (42.943)   
#
#type_histopapillaire    -3.912***      -4.235***  
#  (1.164)        (1.161)   
#
#Constant                 3.913***      4.235***   
#  (1.010)        (1.007)   
#
#--------------------------------------------------
#  Akaike Inf. Crit.        269.998        269.998   
#==================================================
#  Note:                  *p<0.1; **p<0.05; ***p<0.01


# avec fit2 = PSMA_c ~ type_histo
stargazer(fit2, type = "text")

#==================================================
#  Dependent variable:     
#  ----------------------------
#  2              3      
#(1)            (2)     
#--------------------------------------------------
#  type_histochromophobe   -1.552***       -2.013*   
#  (0.522)        (1.102)   
#
#type_histooncocytome    -2.701***       -9.240    
#(0.840)       (44.312)   
#
#type_histopapillaire    -1.898***       -9.068    
#(0.544)       (32.440)   
#
#Constant                 1.447***       -0.288    
#(0.249)        (0.342)   
#
#--------------------------------------------------
#  Akaike Inf. Crit.        276.499        276.499   
#==================================================
#  Note:                  *p<0.1; **p<0.05; ***p<0.

#-analyse metastasis------------------------------------------------------

table(rec$metastasis)
str(rec$metastasis)

rec$metastasisF = as.factor(rec$metastasis)
#                 0  1
# cellule claire 63 57
# chromophobe     0  0
# oncocytome      0  0
# papillaire      0  0

table(rec$type_histo, rec$metastasis)

met_reglog = glm(metastasis ~ sexe + recidive + SSRT2a + PSMA_c, 
                 data = rec, family = "binomial")
summary(met_reglog)
#Coefficients:
#        Estimate Std. Error z value Pr(>|z|)   
#(Intercept)  -16.1371  1455.3976  -0.011  0.99115   
#sexeM          0.5711     0.4602   1.241  0.21463   
#recidive       2.9452     1.1082   2.658  0.00787 **
#SSRT2a2       14.8793  1455.3977   0.010  0.99184   
#SSRT2a3       13.8768  1455.3977   0.010  0.99239   
#PSMA_c2        1.2684     0.6563   1.933  0.05329 . 
#PSMA_c3        1.7922     0.8528   2.102  0.03559 * 

#Null deviance: 166.06  on 119  degrees of freedom
#Residual deviance: 137.58  on 113  degrees of freedom
#(50 observations deleted due to missingness)
#AIC: 151.58
#Number of Fisher Scoring iterations: 14

exp(cbind(OR = coef(met_reglog), confint(met_reglog)))
#OR         2.5 %        97.5 %
#  (Intercept) 9.811535e-08            NA 5.146789e+121
#sexeM       1.770131e+00  7.290390e-01  4.483498e+00
#recidive    1.901541e+01  3.130480e+00  3.716250e+02
#SSRT2a2     2.897391e+06 7.561455e-123            NA
#SSRT2a3     1.063241e+06 2.859709e-123            NA
#PSMA_c2     3.555077e+00  1.067215e+00  1.476047e+01
#PSMA_c3     6.002538e+00  1.204574e+00  3.569262e+01
