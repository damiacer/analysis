# FOLDER

getwd()
setwd("P:/CONSULTATION/Henry_Alexandra") # On PC
setwd("/Users/damianocerasuolo/Desktop/UBRC/2021_CONSULT/Henry_Alexandra") # On Mac

# install.packages("readxl")
library("readxl")
# library("tidyverse")
library(tidyverse)

# DATASET
ha <- read_excel("kiwisdb.xlsx", na="")
ha <- read_excel("KIWIS_AUGDATA.xlsx", na="") 
names(ha)

ha <- as_tibble(ha)
ha <- ha %>% rename(
  # new name = old name,
  "premierITK" = "1er_ITK" ,
  "Date_perte_RMM" = "Date perte RMM"
  )

library("tableone")

# age a l'arret
# duree median arret
# delta Ly_NK_M6_mm3 - PHENO_LNK_M0_mm3

ha$AGE                = as.numeric(as.character(ha$AGE))
ha$PHENO_LNK_M0_mm3   = as.numeric(as.character(ha$PHENO_LNK_M0_mm3))
ha$delai_arret_perte  = as.numeric(as.character(ha$delai_arret_perte))
ha$suividepuisarret   = as.numeric(as.character(ha$suividepuisarret))
# ha$Ly_NK_M6_mm3     = as.numeric(as.character(ha$Ly_NK_M6_mm3)) #ONLY IN "kiwisdb.xlsx" DATA

variables = c("AGE", "SEXE", "Sokal_au_diagnostic", "Phase_LMC_a_l_inclusion", 
              "BCRABL_variant", "ITK_a_larret", "ATCD_darret_des_ITK", "premierITK",
              "PHENO_LNK_M0_mm3", #"Ly_NK_M6_mm3", 
              "recidive01", 
              # DELAYS ALREADY CALCULTED
              "delai_arret_perte", "suividepuisarret")

categorical = c("SEXE", "Sokal_au_diagnostic", "Phase_LMC_a_l_inclusion",
                "BCRABL_variant", "ITK_a_larret", "ATCD_darret_des_ITK", "premierITK",
                "recidive01")

# CREATE THE DESCRIPTIVE TABLE TABLE 
tab1 = CreateTableOne(vars = variables, data = ha, factorVars = categorical)
print(tab1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

# CREATE THE UNIVARIATE TABLE 
tab2 = CreateTableOne(vars = variables, data = ha, factorVars = categorical, test = TRUE, includeNA = FALSE, 
                      strata = "recidive01")
print(tab2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

chisq.test(ha$SEXE, ha$recidive01, correct = FALSE, simulate.p.value = TRUE)
fisher.test(ha$SEXE, ha$recidive01)
chisq.test(ha$Sokal_au_diagnostic, ha$recidive01, correct = FALSE, simulate.p.value = TRUE)
chisq.test(ha$BCRABL_variant, ha$recidive01, correct = FALSE, simulate.p.value = TRUE)
chisq.test(ha$ITK_a_larret , ha$recidive01, correct = FALSE, simulate.p.value = TRUE)
chisq.test(ha$ATCD_darret_des_ITK, ha$recidive01, correct = FALSE, simulate.p.value = TRUE)
fisher.test(ha$ATCD_darret_des_ITK, ha$recidive01)
chisq.test(ha$premierITK, ha$recidive01, correct = FALSE, simulate.p.value = TRUE)

shapiro.test(ha$Ly_NK_M6_mm3) # 0.3591 ==> hypothesis of normality not rejected
t.test(ha$Ly_NK_M6_mm3, ha$recidive01, alternative = c("two.sided"), conf.level = 0.95)

shapiro.test(ha$AGE)
t.test(ha$AGE, ha$recidive01, alternative = c("two.sided"), conf.level = 0.95)
wilcox.test(ha$AGE~ha$recidive01, paired = FALSE, exact = FALSE, correct = FALSE)

shapiro.test(ha$PHENO_LNK_M0_mm3)
wilcox.test(ha$PHENO_LNK_M0_mm3~ha$recidive01, paired = FALSE, exact = FALSE, correct = FALSE)

shapiro.test(ha$suividepuisarret)
t.test(ha$suividepuisarret, ha$recidive01, alternative = c("two.sided"), conf.level = 0.95)

# 2.	Durée médiane depuis le premier ITK prescrit 

library("lubridate")
lubridate::dmy(ha$Date_du_premier_ITK_prescrit)
anytime::anydate(ha$Date_dinclusion)

# ha$Date_dinclusion = (ha$Date_dinclusion, "%d/%m/%y")

ha$durmed = as.Date(ha$Date_dinclusion) - as.Date(ha$Date_du_premier_ITK_prescrit)
median(ha$durmed, na.rm = TRUE)
807/365.25
q1 = quantile(ha$durmed, 0.25, na.rm = TRUE)
mean(durmed, na.rm = TRUE)
sd(durmed, na.rm = T)

# NEW VARIABLES 
# ha$CD_56_dim_M6_mm3 - ha$CD_56_dim_M0_mm3
ha$CD_56_dim_M6_mm3[ha$CD_56_dim_M6_mm3 == "NR"] = ""
ha$CD_56_dim_M6_mm3[ha$CD_56_dim_M6_mm3 != "NR"] = ha$CD_56_dim_M6_mm3
str(ha$CD_56_dim_M6_mm3)
ha$CD_56_dim_M6_mm3 = as.numeric(as.character(ha$CD_56_dim_M6_mm3))

ha$CD_56_dim_M0_mm3[ha$CD_56_dim_M0_mm3 == "NR"] = ""
ha$CD_56_dim_M0_mm3[ha$CD_56_dim_M0_mm3 != "NR"] = ha$CD_56_dim_M0_mm3
str(ha$CD_56_dim_M0_mm3)
ha$CD_56_dim_M0_mm3 = as.numeric(as.character(ha$CD_56_dim_M0_mm3))

ha$cd56dim_diff = ha$CD_56_dim_M6_mm3 - ha$CD_56_dim_M0_mm3
is.na(ha$cd56dim_diff)

mean(ha$cd56dim_diff, na.rm = TRUE)
sd(ha$cd56dim_diff, na.rm = TRUE)

# aggregate(ha$cd56dim_diff, list(ha$recidive01), FUN=mean) 
# aggregate(ha$cd56dim_diff, list(ha$recidive01), FUN=sd)

table(ha$recidive01, ha$cd56dim_diff)

library(dplyr)
ha %>%
  group_by(recidive01) %>%
  summarise_at(vars("cd56dim_diff"), funs(mean, sd), na.rm = TRUE)

shapiro.test(ha$cd56dim_diff) # 0.2857 on ne rejette pas l'H0 de normalite de la distribution
t.test(ha$cd56dim_diff, ha$recidive01, alternative = c("two.sided"), conf.level = 0.95)

# ha$CD56_bright_M6_mm3 - ha$CD56_bright_M0_mm3
ha$CD56_bright_M6_mm3[ha$CD56_bright_M6_mm3 == "NR"] = ""
ha$CD56_bright_M6_mm3[ha$CD56_bright_M6_mm3 != "NR"] = ha$CD56_bright_M6_mm3
str(ha$CD56_bright_M6_mm3)
ha$CD56_bright_M6_mm3 = as.numeric(as.character(ha$CD56_bright_M6_mm3))

ha$CD56_bright_M0_mm3[ha$CD56_bright_M0_mm3 == "NR"] = ""
ha$CD56_bright_M0_mm3[ha$CD56_bright_M0_mm3 != "NR"] = ha$CD56_bright_M0_mm3
str(ha$CD56_bright_M0_mm3)
ha$CD56_bright_M0_mm3 = as.numeric(as.character(ha$CD56_bright_M0_mm3))

ha$cd56bright_diff = ha$CD56_bright_M6_mm3 - ha$CD56_bright_M0_mm3
mean(ha$cd56bright_diff, na.rm = TRUE)
sd(ha$cd56bright_diff, na.rm = TRUE)

#library(dplyr)
ha %>%
  group_by(recidive01) %>%
  summarise_at(vars("cd56bright_diff"), funs(mean, sd), na.rm = TRUE)

shapiro.test(ha$cd56bright_diff) # 0.06205 on ne rejette pas l'H0 de normalite de la distribution
t.test(ha$cd56bright_diff, ha$recidive01, alternative = c("two.sided"), conf.level = 0.95)


# ha$Date_dinclusion - ha$Date_du_premier_ITK_prescrit

# ha$premierRM45 - ha$Date_d_arret_des_ITK

# dernier_bcrabldose - ha$Date_dinclusion

# ha$premierRM45 - ha$Date_d_arret_des_ITK


had <- read_excel("kiwisdates.xlsx", na="") 
View(had)
names(had)
str(had$Date_d_arret_des_ITK)

# FOR DATES, SEE ALSO: https://stackoverflow.com/questions/11666172/calculating-number-of-days-between-2-columns-of-dates-in-data-frame/11666533

# had$Date_dinclusion - had$Date_du_premier_ITK_prescrit

had$incitk = as.Date(had$Date_dinclusion) - as.Date(had$Date_du_premier_ITK_prescrit) 
had$incitk
had$incitk_y = had$incitk/365.25
mean(had$incitk_y, na.rm = T)
sd(had$incitk_y, na.rm = T)

had %>%
  group_by(recidive01) %>%
  summarise_at(vars("incitk_y"), funs(mean, sd), na.rm = TRUE)
str(had$incitk_y)
had$incitk_yn = as.numeric(as.character(had$incitk_y))
t.test(had$incitk_yn, had$recidive01, alternative = c("two.sided"), conf.level = 0.95)


# had$premierRM45 - had$Date_d_arret_des_ITK

had$RMarret = as.Date(had$premierRM45) - as.Date(had$Date_d_arret_des_ITK)
had$RMarret
had$RMarret_y = had$RMarret/365.25
mean(had$RMarret_y, na.rm = TRUE)
sd(had$RMarret_y, na.rm = TRUE)

had %>%
  group_by(recidive01) %>%
  summarise_at(vars("RMarret_y"), funs(mean, sd), na.rm = TRUE)

# had$dernier_bcrabldose - had$Date_dinclusion

had$doseincl = as.Date(had$dernier_bcrabldose) - as.Date(had$Date_dinclusion)
had$doseincl
had$doseincl_y = had$doseincl/365.25
mean(had$doseincl_y, na.rm = TRUE)
sd(had$doseincl_y, na.rm = TRUE)

had %>%
  group_by(recidive01) %>%
  summarise_at(vars("doseincl_y"), funs(mean, sd), na.rm = TRUE)

had$doseincl_yn = as.numeric(as.character(had$doseincl_y))
t.test(had$doseincl_yn, had$recidive01, alternative = c("two.sided"), conf.level = 0.95)


# Date_du_premier_ITK_prescrit - Date_de_diagnostic

had$ITKdiag = as.Date(had$Date_du_premier_ITK_prescrit) - as.Date(had$Date_de_diagnostic)
had$ITKdiag
had$ITKdiag_y = had$ITKdiag/365.25
mean(had$ITKdiag_y, na.rm = TRUE)
sd(had$ITKdiag_y, na.rm = TRUE)

had %>%
  group_by(recidive01) %>%
  summarise_at(vars("ITKdiag_y"), funs(mean, sd), na.rm = TRUE)

had$ITKdiag_yn = as.numeric(as.character(had$ITKdiag_y))
t.test(had$ITKdiag_yn, had$recidive01, alternative = c("two.sided"), conf.level = 0.95)
