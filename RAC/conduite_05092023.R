require("readxl")
require("tidyverse")
#require("plyr")
require("lubridate")
require("here")
require("lme4")
require("nlme")
require("merTools")

#-ON PC-------------------------------------------------------------------------

setwd("P:/CONSULTATION/Rat_AnneChristine/Conduite/DATA")

c0 <- read_excel("conduite0.xlsx", na = "NA")
c3 <- read_excel("conduite3.xlsx", na = "NA")
c5 <- read_excel("conduite5.xlsx", na = "NA")
c7 <- read_excel("conduite7.xlsx", na = "NA")
a0 <- read_excel("DMO_Annee0.xls", na = "NA")
a3 <- read_excel("DMO_Annee3.xls", na = "NA")
a4 <- read_excel("DMO_Annee4.xls", na = "NA")
a5 <- read_excel("DMO_Annee5.xls", na = "NA")
a6 <- read_excel("DMO_Annee6.xls", na = "NA")
a7 <- read_excel("DMO_Annee7.xls", na = "NA")
a8 <- read_excel("DMO_Annee8.xls", na = "NA")
a9 <- read_excel("DMO_Annee9.xls", na = "NA")
a10 <- read_excel("DMO_Annee10.xls", na = "NA")
pro <- read_excel("DMO_Protheses.xls", na = "NA")

a0c <- read_excel("Items_OAKQOL_A0.xlsx",  na = "")
a3c <- read_excel("Items_OAKHQOL_A3.xls", na = "")
a5c <- read_excel("Items_OAKHQOL_A5.xlsx", na = "")
a7c <- read_excel("Items_OAKHQOL_A7.xlsx", na = "")

s0 <- read_excel("Sedentarite_A0.xlsx",  na = "")
s3 <- read_excel("Sedentarite_A3.xlsx",  na = "")
s5 <- read_excel("Sedentarite_A5.xlsx",  na = "")
s7 <- read_excel("Sedentarite_A7.xlsx",  na = "")

ai0 <- read_excel("Articulations_A0.xlsx",  na = "")
ai3 <- read_excel("Articulation_A3.xlsx",  na = "")
ai5 <- read_excel("Articulation_A5.xlsx",  na = "")
ai7 <- read_excel("Articulation_A7.xlsx",  na = "")

co0 <- read_excel("Comorbidites_A0.xls",  na = "")
co3 <- read_excel("Comorbidites_A3.xlsx",  na = "")
co5 <- read_excel("Comorbidites_A5.xlsx",  na = "")
co7 <- read_excel("Comorbidites_A7.xlsx",  na = "")


#-ON MACBOOK--------------------------------------------------------------------

c0 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "conduite0.xlsx"))
c3 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "conduite3.xlsx"))
c5 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "conduite5.xlsx"))
c7 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "conduite7.xlsx"))

a0 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "DMO_Annee0.xls"))
a3 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "DMO_Annee3.xls"))
a4 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "DMO_Annee4.xls"))
a5 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "DMO_Annee5.xls"))
a6 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "DMO_Annee6.xls"))
a7 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "DMO_Annee7.xls"))
a8 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "DMO_Annee8.xls"))
a9 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "DMO_Annee9.xls"))
a10 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "DMO_Annee10.xls"))
pro <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "DMO_Protheses.xls"))

a0c <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "Items_OAKQOL_A0.xlsx"))
a3c <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "Items_OAKHQOL_A3.xls"))
a5c <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "Items_OAKHQOL_A5.xlsx"))
a7c <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "Items_OAKHQOL_A7.xlsx"))

s0 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "Sedentarite_A0.xlsx"))
s3 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "Sedentarite_A3.xlsx"))
s5 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "Sedentarite_A5.xlsx"))
s7 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "Sedentarite_A7.xlsx"))

ai0 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "Articulations_A0.xlsx"))
ai3 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "Articulation_A3.xlsx"))
ai5 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "Articulation_A5.xlsx"))
ai7 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "Articulation_A7.xlsx"))

co0 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "Comorbidites_A0.xls"))
co3 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "Comorbidites_A3.xlsx"))
co5 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "Comorbidites_A5.xlsx"))
co7 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", "Comorbidites_A7.xlsx"))

#-MERGING-----------------------------------------------------------------------

C0M <- merge(c0, a0c, by.x = "IdCohorte", by.y = "IdCohorte")
C3M <- merge(c3, a3c, by.x = "IdCohorte", by.y = "IdCohorte")
C5M <- merge(c5, a5c, by.x = "IdCohorte", by.y = "IDCOHORTE")
C7M <- merge(c7, a7c, by.x = "IdCohorte", by.y = "IDCOHORTE")

#-MERGING TO CALCULATE THE NUMBER OF THE TOTAL PARTICIPANTS 

n0 = merge(c0, ai0, by.x = "IdCohorte", by.y = "IdCohorte")
table(n0$ArticIncl)

n3 = merge(c3, ai0, by.x = "IdCohorte", by.y = "IdCohorte")
table(n3$ArticIncl)

n5 = merge(c5, ai0, by.x = "IdCohorte", by.y = "IdCohorte")
table(n5$ArticIncl)

n7 = merge(c7, ai0, by.x = "IdCohorte", by.y = "IdCohorte")
table(n7$ArticIncl)

#-MERGING NEW DATA--------------------------------------------------------------

sai0 <- merge(s0, ai0, by.x = "IdCohorte", by.y = "IdCohorte")
saico0 <- merge(sai0, co0, by.x = "IdCohorte", by.y = "IdCohorte")

sai3 <- merge(s3, ai3, by.x = "IdCohorte", by.y = "IdCohorte")
saico3 <- merge(sai3, co3, by.x = "IdCohorte", by.y = "IdCohorte")

sai5 <- merge(s5, ai5, by.x = "IDCOHORTE", by.y = "IDCOHORTE")
saico5 <- merge(sai5, co5, by.x = "IDCOHORTE", by.y = "IDCOHORTE")

sai7 <- merge(s7, ai7, by.x = "IDCOHORTE", by.y = "IDCOHORTE")
saico7 <- merge(sai7, co7, by.x = "IDCOHORTE", by.y = "IDCOHORTE")

#-FINAL DATASETS----------------------------------------------------------------

dim(C0M) # 878 247
dim(saico0) # 878  25

C0M <- merge(C0M, saico0, by.x = "IdCohorte", by.y = "IdCohorte") # 878 271

dim(C3M) # 747 289
dim(saico3) # 747  76

C3M <- merge(C3M, saico3, by.x = "IdCohorte", by.y = "IdCohorte") # 747 364

dim(C5M) # 675 322
dim(saico5) # 675  76

C5M <- merge(C5M, saico5, by.x = "IdCohorte", by.y = "IDCOHORTE") # 675 397

dim(C7M) # 545 289
dim(saico7) # 545  76

C7M <- merge(C7M, saico7, by.x = "IdCohorte", by.y = "IDCOHORTE") # 545 364

#-FINAL-DATASETS----------------------------------------------------------------

dim(C0M) # 878 271
dim(C3M)
dim(C5M)
dim(C7M)
dim(c0)
dim(a0c)

#-VARIABLE RECODING BEFORE MERGING----------------------------------------------

C0M <- C0M %>%
  mutate(comorb_neuro = case_when(
    FCI08 == "1" & FCI09 == "1" ~ "1",
    FCI08 == "0" & FCI09 == "1" ~ "1",
    FCI08 == "1" & FCI09 == "0" ~ "1",
    FCI08 == "0" & FCI09 == "0" ~ "0",
  ))
table(C0M$comorb_neuro)

C0M <- C0M %>%
  mutate(comorb_psy = case_when(
    FCI13 == "1" & FCI14 == "1" ~ "1",
    FCI13 == "1" & FCI14 == "0" ~ "1",
    FCI13 == "0" & FCI14 == "1" ~ "1",
    FCI13 == "0" & FCI14 == "0" ~ "0"
  ))
table(C0M$comorb_psy)

C0M <- C0M %>%
  mutate(comorb_sens = case_when(
    FCI15 == "1" & FCI16 == "1" ~ "1",
    FCI15 == "1" & FCI16 == "0" ~ "1",
    FCI15 == "0" & FCI16 == "1" ~ "1",
    FCI15 == "0" & FCI16 == "0" ~ "0"
  ))

table(C0M$comorb_sens)

C0M$comorb_diab = C0M$FCI11
C0M$comorb_dos = C0M$FCI17

str(C0M$Nbre_Art_doul)

C0M <- C0M %>%
  mutate(Nbre_Art_doulCL = case_when(
    Nbre_Art_doul == 0 ~ "0",
    Nbre_Art_doul == 1 ~ "1",
    Nbre_Art_doul == 2 ~ "2",
    Nbre_Art_doul == 3 | Nbre_Art_doul == 4 ~ "3ouplus"
  ))
table(C0M$Nbre_Art_doulCL)


#-BUILDING-DATASETS-FROM-VARS-TO-INCLUDE----------------------------------------

co0 = subset(C0M, select = c("IdCohorte", "AMIQUAL_Q09", "AMIQUAL_Q10", "AMIQUAL_Q11", "AMIQUAL_Q24", 
                             "MAQ_L_MET", "scorfoncNorm", "scordoulNorm", "ScoGlob",
                             "MAQ2_NBHREGT", "MAQ2_NBHREGPC"))

# fixed variables
C0Mses = subset(C0M, select = c("IdCohorte", "ArticIncl", "KL", "Nbre_Art_doul", 
                                "comorb_neuro",
                                "comorb_psy",
                                "comorb_sens",
                                "comorb_diab",
                                "comorb_dos",
                                "Nbre_Art_doulCL",
                                "Groll",
                                "EDUCATION", "SEXE", "AGE", "BMI", "PROFESSION", "MARITAL", "RETRAITE"))


co3 = subset(C3M, select = c("IdCohorte", "AMIQUAL_Q09", "AMIQUAL_Q10", "AMIQUAL_Q11", "AMIQUAL_Q24",
                             "MAQ_L_MET",
                             "scorfoncNorm", "scordoulNorm", "ScoGlob",
                             "MAQ2_NBHREGT", "MAQ2_NBHREGPC"))

co5 = subset(C5M, select = c("IdCohorte", "AMIQUAL_Q09", "AMIQUAL_Q10", "AMIQUAL_Q11", "AMIQUAL_Q24",
                             "MAQ_L_MET", 
                             "scorfoncNorm", "scordoulNorm", "ScoGlob",
                             "MAQ2_NBHREGT", "MAQ2_NBHREGPC"))

co7 = subset(C7M, select = c("IdCohorte", "AMIQUAL_Q09", "AMIQUAL_Q10", "AMIQUAL_Q11", "AMIQUAL_Q24",
                             "MAQ_L_MET", 
                             "scorfoncNorm", "scordoulNorm", "ScoGlob",
                             "MAQ2_NBHREGT", "MAQ2_NBHREGPC"))

# coa0 = subset(a0, select = c("IdCohorte", "ArticIncl"))

# coa3 = subset(a3, select = c("IdCohorte", "KELL_HD", "KELL_HG", "EXT_FT_D", "SCH_FT_D", "EXT_FT_G",
#                             "SCH_FT_G", "HANCHEDMO", "HANCHETSCORE", "HANCHEZSCORE", "FEMURDMO",
#                             "FEMURTSCORE", "FEMURZSCORE", "Score_FemoP_GD", "Score_FemoP_GG",
#                             "Score_FemoP", "Score_Osteo_T_GD", "Score_Osteo_T_GG", "Score_Osteo_T", 
#                             "FCI01_1", "COMMORB04", "COMMORB07", "COMMORB08", "COMMORB09",
#                             "FCI06_1", "COMMORB10", "FCI08", "FCI09_1", 
#                             "COMMORB14", "COMMORB42", "FCI12", "COMMORB30", "FCI14_1",
#                             "FCI15_1", "COMMORB40", "FCI17_1", "COMMORB43"))

#-ADDING THE NON-VARIATING WITH TIME VARIABLES----------------------------------

#C0M <- merge(co0, C0Mses, by.x = "IdCohorte", by.y = "IdCohorte") # 878  39
#C3M <- merge(co3, C0Mses, by.x = "IdCohorte", by.y = "IdCohorte") # 747  39
#C5M <- merge(co5, C0Mses, by.x = "IdCohorte", by.y = "IdCohorte") # 675  39
#C7M <- merge(co7, C0Mses, by.x = "IdCohorte", by.y = "IdCohorte") # 545  39

#-MISSING DATA ANALYSIS---------------------------------------------------------

#C0M$AMIQUAL_Q09miss[is.na(C0M$AMIQUAL_Q09)] <- 99 
#table(C0M$AMIQUAL_Q09miss)
#dim(C0M)

#C3M$AMIQUAL_Q09miss[is.na(C3M$AMIQUAL_Q09)] <- 99 
#table(C3M$AMIQUAL_Q09miss)
#dim(C3M)

#C5M$AMIQUAL_Q09miss[is.na(C5M$AMIQUAL_Q09)] <- 99 
#table(C5M$AMIQUAL_Q09miss)
#dim(C5M)

#C7M$AMIQUAL_Q09miss[is.na(C7M$AMIQUAL_Q09)] <- 99 
#table(C7M$AMIQUAL_Q09miss)
#dim(C7M)

#-MERGING-THE-NEW-DATASETS------------------------------------------------------
#-LIST TO MERGE-----------------------------------------------------------------
dim(co0)
dim(co3)
dim(co5)
dim(co7)
dim(C0Mses)

df_list0 = list(co0, C0Mses)
df_list3 = list(co3, C0Mses)
df_list5 = list(co5, C0Mses)
df_list7 = list(co7, C0Mses)

co0 <- df_list0 %>% reduce(full_join, by='IdCohorte')
co3 <- df_list3 %>% reduce(full_join, by='IdCohorte')
co5 <- df_list5 %>% reduce(full_join, by='IdCohorte')
co7 <- df_list7 %>% reduce(full_join, by='IdCohorte')

#-ADD THE COUNT VARIABLE TO DATASETS--------------------------------------------

co0$time <- rep("1", times = "878")
co3$time <- rep("2", times = "878")
co5$time <- rep("3", times = "878")
co7$time <- rep("4", times = "878")

#-ACTUAL MERGE------------------------------------------------------------------
co0357 <- rbind(co0, co3, co5, co7)
dim(co0357)

#-ACTUAL MERGE------------------------------------------------------------------
#library(tidyverse)
#dim(coa3)
#lignes 3512
#colonnes 
#19+15 # 33 car ID cohorte is not repeated
#df_listF = list(co0357, coa3)
#codb <- df_listF %>% reduce(full_join, by='IdCohorte')
codb <- co0357
dim(codb)
View(codb)


#-FINAL DATASET-----------------------------------------------------------------
###
summary(codb)
dim(codb) #3512   45
###

#-EXCLUDING ART 3

codb = codb[!(codb$ArticIncl == 3),]
dim(codb) #3316 45


#-NEW VARS----------------------------------------------------------------------


codb$score_comorb = (codb$FCI01 + codb$FCI02 + codb$FCI03 + codb$FCI04 + codb$FCI05 +
                       codb$FCI06 + codb$FCI09 + codb$FCI10 + codb$FCI11 + 
                       codb$FCI13 + codb$FCI14 + codb$FCI15 + codb$FCI16 + codb$FCI17)
str(codb$score_comorb)
#codb$score_comorb = as.factor(codb$score_comorb)
#table(codb$score_comorb, useNA = "always")

#codb <- codb %>% 
#  mutate(score_comorbCL = case_when(
#    score_comorb == 1 ~ "1",
#    score_comorb == 2 ~ "2",
#    score_comorb == 3 ~ "3",
#    score_comorb >= 4 ~ "4" # four or more
#  ))
#table(codb$score_comorbCL, useNA = "always")

codb <- codb %>%
  mutate(EDUCATION.CL = case_when(
    EDUCATION == 10 ~ "1", # primaire
    EDUCATION == 21 ~ "2", # secondaire premier cycle
    EDUCATION == 22 ~ "2", # secondaire deuxieme cycle
    EDUCATION == 31 | EDUCATION == 32 ~ "3" # sup
  ))
table(codb$EDUCATION.CL, useNA = "always")

codb <- codb %>%
  mutate(MARITAL.CL = case_when(
    MARITAL == 1 | MARITAL == 2 ~ "1", #vit avec quelqu'un
    MARITAL >= 3 ~ "2" #vit seul-e
  ))
table(codb$MARITAL.CL, useNA = "always")

mean(codb$MAQ_L_MET, na.rm = T)
codb$MAQ_L_METm = codb$MAQ_L_MET*60

codb <- codb %>%
  mutate(MAQ_L_METmCL = case_when( 
    MAQ_L_METm < 600 ~ "1",
    MAQ_L_METm >= 600 & MAQ_L_METm < 1500 ~ "2",
    MAQ_L_METm >= 1500 ~ "3"
    ))
table(codb$MAQ_L_METmCL)

str(codb$KL)
codb$KL = as.factor(codb$KL)

str(codb$Nbre_Art_doul)
codb$Nbre_Art_doul = as.factor(codb$Nbre_Art_doul)

#-NEW MODELS--------------------------------------------------------------------

control = lmeControl(msMaxIter = 1000, msMaxEval = 1000)

m_q09 <- lme(AMIQUAL_Q09 ~ AGE + SEXE + BMI + MAQ2_NBHREGT + MAQ_L_METmCL + scorfoncNorm + 
               scordoulNorm + ScoGlob + EDUCATION.CL + MARITAL.CL + 
               ArticIncl + KL + Nbre_Art_doul +
               comorb_neuro + comorb_psy + comorb_sens + comorb_diab + comorb_dos,
             random = ~ time | IdCohorte, na.action = na.omit, data=codb)
summary(m_q09)
intervals(m_q09#, which = "fixed"
          )


m_q10 <- lme(AMIQUAL_Q10 ~ AGE + SEXE + BMI + MAQ2_NBHREGT + MAQ_L_METmCL + scorfoncNorm + 
               scordoulNorm + ScoGlob + EDUCATION.CL + MARITAL.CL + 
               ArticIncl + KL + Nbre_Art_doul +
               comorb_neuro + comorb_psy + comorb_sens + comorb_diab + comorb_dos,
             random = ~ time | IdCohorte, na.action = na.omit, data=codb)
summary(m_q10)
intervals(m_q10)


m_q11 <- lme(AMIQUAL_Q11 ~ AGE + SEXE + BMI + MAQ2_NBHREGT + MAQ_L_METmCL + scorfoncNorm + 
               scordoulNorm + ScoGlob + EDUCATION.CL + MARITAL.CL + 
               ArticIncl + KL + Nbre_Art_doul +
               comorb_neuro + comorb_psy + comorb_sens + comorb_diab + comorb_dos,
             random = ~ time | IdCohorte, na.action = na.omit, data=codb)
summary(m_q11)
intervals(m_q11, which = "fixed")

# Q24

m_q24 <- lme(AMIQUAL_Q24 ~ AGE + SEXE + BMI + MAQ2_NBHREGT + MAQ_L_METmCL + scorfoncNorm + 
               scordoulNorm + ScoGlob + EDUCATION.CL + MARITAL.CL + 
               ArticIncl + KL + Nbre_Art_doul +
               comorb_neuro + comorb_psy + comorb_sens + comorb_diab + comorb_dos,
             random = ~ time | IdCohorte, na.action = na.omit, data=codb)
summary(m_q24)
intervals(m_q24, which = "fixed")

m_q24e1 <- lme(AMIQUAL_Q24 ~ AGE + SEXE + BMI + MAQ2_NBHREGT + MAQ_L_METmCL + scorfoncNorm + 
               scordoulNorm + ScoGlob + EDUCATION.CL + MARITAL.CL + 
               ArticIncl + KL +                comorb_neuro + comorb_psy + comorb_sens + comorb_diab + comorb_dos,
             random = ~ time | IdCohorte, na.action = na.omit, data=codb)
anova.lme(m_q24, m_q24e1)


m_q24e2 <- lme(AMIQUAL_Q24 ~ AGE + SEXE + BMI + MAQ2_NBHREGT + MAQ_L_METmCL + scorfoncNorm + 
               scordoulNorm + ScoGlob + EDUCATION.CL + MARITAL.CL + 
               ArticIncl + Nbre_Art_doul +
               comorb_neuro + comorb_psy + comorb_sens + comorb_diab + comorb_dos,
             random = ~ time | IdCohorte, na.action = na.omit, data=codb)
anova.lme(m_q24, m_q24e2)

m_q24e3 <- lme(AMIQUAL_Q24 ~ AGE + SEXE + BMI + MAQ2_NBHREGT + MAQ_L_METmCL + scorfoncNorm + 
               scordoulNorm + ScoGlob + MARITAL.CL + 
               ArticIncl + KL + Nbre_Art_doul +
               comorb_neuro + comorb_psy + comorb_sens + comorb_diab + comorb_dos,
             random = ~ time | IdCohorte, na.action = na.omit, data=codb)
anova.lme(m_q24, m_q24e3)


m_q24e4 <- lme(AMIQUAL_Q24 ~ AGE + SEXE + BMI + MAQ2_NBHREGT + scorfoncNorm + 
               scordoulNorm + ScoGlob + EDUCATION.CL + MARITAL.CL + 
               ArticIncl + KL + Nbre_Art_doul +
               comorb_neuro + comorb_psy + comorb_sens + comorb_diab + comorb_dos,
             random = ~ time | IdCohorte, na.action = na.omit, data=codb)
anova.lme(m_q24, m_q24e4)

#####

#-DESCRIPTIVE ANALYSIS----------------------------------------------------------


dim(C0M) # 878 277

C0Md <- C0M
names(C0Md)

C0Md = C0Md[!(C0Md$ArticIncl == 3),]
dim(C0Md)


#-VARIABLE RECODING BEFORE MERGING----------------------------------------------

C0Md <- C0Md %>%
  mutate(comorb_neuro = case_when(
    FCI08 == "1" & FCI09 == "1" ~ "1",
    FCI08 == "0" & FCI09 == "1" ~ "1",
    FCI08 == "1" & FCI09 == "0" ~ "1",
    FCI08 == "0" & FCI09 == "0" ~ "0",
  ))
table(C0Md$comorb_neuro)

C0Md <- C0Md %>%
  mutate(comorb_psy = case_when(
    FCI13 == "1" & FCI14 == "1" ~ "1",
    FCI13 == "1" & FCI14 == "0" ~ "1",
    FCI13 == "0" & FCI14 == "1" ~ "1",
    FCI13 == "0" & FCI14 == "0" ~ "0"
  ))
table(C0Md$comorb_psy)

C0Md <- C0Md %>%
  mutate(comorb_sens = case_when(
    FCI15 == "1" & FCI16 == "1" ~ "1",
    FCI15 == "1" & FCI16 == "0" ~ "1",
    FCI15 == "0" & FCI16 == "1" ~ "1",
    FCI15 == "0" & FCI16 == "0" ~ "0"
  ))

table(C0Md$comorb_sens)

C0Md$comorb_diab = C0Md$FCI11
C0Md$comorb_dos = C0Md$FCI17

str(C0Md$Nbre_Art_doul)

C0Md <- C0Md %>%
  mutate(Nbre_Art_doulCL = case_when(
    Nbre_Art_doul == 0 ~ "0",
    Nbre_Art_doul == 1 ~ "1",
    Nbre_Art_doul == 2 ~ "2",
    Nbre_Art_doul == 3 | Nbre_Art_doul == 4 ~ "3ouplus"
  ))
table(C0Md$Nbre_Art_doulCL)

C0Md <- C0Md %>%
  mutate(EDUCATION.CL = case_when(
    EDUCATION == 10 ~ "1", # primaire
    EDUCATION == 21 ~ "2", # secondaire premier cycle
    EDUCATION == 22 ~ "2", # secondaire deuxieme cycle
    EDUCATION == 31 | EDUCATION == 32 ~ "3" # sup
  ))
table(C0Md$EDUCATION.CL, useNA = "always")

C0Md <- C0Md %>%
  mutate(MARITAL.CL = case_when(
    MARITAL == 1 | MARITAL == 2 ~ "1", #vit avec quelqu'un
    MARITAL >= 3 ~ "2" #vit seul-e
  ))
table(C0Md$MARITAL.CL, useNA = "always")

mean(C0Md$MAQ_L_MET, na.rm = T)
C0Md$MAQ_L_METm = C0Md$MAQ_L_MET*60

C0Md <- C0Md %>%
  mutate(MAQ_L_METmCL = case_when( 
    MAQ_L_METm < 600 ~ "1",
    MAQ_L_METm >= 600 & MAQ_L_METm < 1500 ~ "2",
    MAQ_L_METm >= 1500 ~ "3"
  ))
table(C0Md$MAQ_L_METmCL)

str(C0Md$KL)
C0Md$KL = as.factor(C0Md$KL)

str(C0Md$Nbre_Art_doul)
C0Md$Nbre_Art_doul = as.factor(C0Md$Nbre_Art_doul)

#-TABLEONE----------------------------------------------------------------------

require(tableone)

dput(names(C0Md))

vars = c("AGE", "SEXE", "BMI", "MAQ2_NBHREGT", "MAQ_L_METmCL", "scorfoncNorm", 
           "scordoulNorm", "ScoGlob", "EDUCATION.CL", "MARITAL.CL", 
           "ArticIncl", "KL", "Nbre_Art_doul",
           "comorb_neuro", "comorb_psy", "comorb_sens", "comorb_diab", "comorb_dos", 
         "AMIQUAL_Q09", "AMIQUAL_Q10", "AMIQUAL_Q11", "AMIQUAL_Q24")

fact = c("SEXE",  "MAQ_L_METmCL", "EDUCATION.CL", "MARITAL.CL", 
           "ArticIncl", "KL", "Nbre_Art_doul",
           "comorb_neuro", "comorb_psy", "comorb_sens", "comorb_diab", "comorb_dos")

tab1 = CreateTableOne(vars = vars, data = C0Md, factorVars = fact)
print(tab1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#-QUESTIONS DESCRIPTIVE---------------------------------------------------------

require("doBy")
summaryBy(AMIQUAL_Q09 ~ time, data = codb, na.rm = TRUE,
          FUN = list(mean,sd))

summaryBy(AMIQUAL_Q10 ~ time, data = codb, na.rm = TRUE,
          FUN = list(mean,sd))

summaryBy(AMIQUAL_Q11 ~ time, data = codb, na.rm = TRUE,
          FUN = list(mean,sd))

summaryBy(AMIQUAL_Q24 ~ time, data = codb, na.rm = TRUE,
          FUN = list(mean,sd))
