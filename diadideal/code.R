# workspace----
getwd()
setwd("P:/CONSULTATION/DIADIDEAL")

# packages----

#install.packages("readxl")
require("readxl")
require("tidyverse")
require("tableone")

# base de données----

di <- read_excel("DIADIDEAL_Export_final_global_2023-11-20.xlsx")
names(di)

# patients informés 
# patients ayant signé
# => patients inclus
table(di$INCL_CE_SIGN, useNA = "always")
table(di$CI_CE, useNA = "always")
#  1 <NA> 
# 10    0 

# patients ayant fistule arterio veineuse fonctionnelle 
table(di$CI_FAV_FONCT, useNA = "always")
#  1 <NA> 
# 10    0 

# Date d installation a domicile de la machine d hemodialyse 
str(di$TECH_INSTAL_DTE)
di$TECH_INSTAL_DTEn = as.numeric(di$TECH_INSTAL_DTE)
table(di$TECH_INSTAL_DTE, useNA = "always")

# installation HDD
di <- di %>%
  mutate(instHDD = case_when(
    TECH_INSTAL_DTE > 0 ~ "Y"
  ))
table(di$instHDD)
di$instHDD[is.na(di$instHDD)] <- "N"

# N Y 
# 2 8 

# base patients installés----
# descriptive des patients installés 

did <- di[!(di$instHDD == "N"),] 
dim(did)

# analyse descriptive----

# selectionner les variables pour l'analyse descriptive 

didd = subset(did, select = c(AGE, SEXE, DEMO_DIPLOM, DEMO_ACT_PRO,
                              ENV_LIEU, 
                              ENV_DIST_NEPHRO,
                              ENV_DIST_FORMA,
                              ENV_DIST_IDEL,
                              ENV_DIST_MED_TRT,
                              ENV_CD,
                              ENV_CD_DIST,
                              ENV_CD,
                              ENV_CD_DIST,
                              CLIN_REN,
                              CLIN_REN_DTE,
                              CLIN_REN_LIST,
                              CLIN_HD_INCID,
                              CLIN_HD_MOD,
                              CLIN_HD_DTE,
                              CLIN_HD_FREQ,
                              CLIN_HD_DUR_HR,
                              CLIN_HD_DUR_MIN,
                              CLIN_HD_DIUR,
                              COMOR_F,
                              COMOR_TYPE,
                              COMOR_TYPE_ATE,
                              COMOR_ASSOC,
                              COMOR_ASSOC_IM,
                              COMOR_ASSOC_ICC,
                              COMOR_ASSOC_VASC,
                              COMOR_ASSOC_CEREB_VASC,
                              COMOR_ASSOC_HEPAT_NSEV,
                              COMOR_ASSOC_DEMENC,
                              COMOR_ASSOC_PULM,
                              COMOR_ASSOC_SYST,
                              COMOR_ASSOC_ULC,
                              COMOR_ASSOC_DIAB_SCOMP,
                              COMOR_ASSOC_DIAB_ACOMP,
                              COMOR_ASSOC_HEMI,
                              COMOR_ASSOC_LEUCE,
                              COMOR_ASSOC_LYMPH,
                              COMOR_ASSOC_TUM_SMETA,
                              COMOR_ASSOC_HEPAT_SEV,
                              COMOR_ASSOC_VIH,
                              COMOR_ASSOC_TUM_AMETA,
                              COMOR_ASSOC_SCORE,
                              COMOR_AUTONOM,
                              COMOR_AUTONOM_QUOTI,
                              COMOR_AUTONOM_DEPLAC,
                              TECH_DEB_DTE,
                              TECH_FORMA_NB,
                              TECH_PRESCR_SEANCE_NB,
                              TECH_INSTAL_DTE,
                              TECH_ABORD_TYPE,
                              TECH_ABORD_LOC,
                              TECH_ABORD_LOC_PRE,
                              TECH_ABORD_DTE))
dim(didd)

variables <- c("AGE", "SEXE", "DEMO_DIPLOM", "DEMO_ACT_PRO",
               "ENV_LIEU", "ENV_DIST_NEPHRO",
               "ENV_DIST_FORMA",
               "ENV_DIST_IDEL",
               "ENV_DIST_MED_TRT",
               "ENV_CD",
               "ENV_CD_DIST",
               "ENV_CD",
               "ENV_CD_DIST")

categorical <- c("SEXE", "DEMO_DIPLOM", "DEMO_ACT_PRO")


des1 <- CreateTableOne(vars = variables, data = didd, factorVars = categorical, includeNA = TRUE)
print(des1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

variables2 <- c("CLIN_REN",
                 "CLIN_REN_DTE",
                 "CLIN_REN_LIST",
                 "CLIN_HD_INCID",
                 "CLIN_HD_MOD",
                 "CLIN_HD_DTE",
                 "CLIN_HD_FREQ",
                 "CLIN_HD_DUR_HR",
                 "CLIN_HD_DUR_MIN",
                 "CLIN_HD_DIUR")


categorical2 <- c("CLIN_REN",
                "CLIN_REN_DTE",
                "CLIN_REN_LIST",
                "CLIN_HD_INCID",
                "CLIN_HD_MOD",
                "CLIN_HD_DTE",
                "CLIN_HD_FREQ")

des2 <- CreateTableOne(vars = variables2, data = didd, factorVars = categorical2, includeNA = TRUE)
print(des2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

variables3 <- c("COMOR_F", "COMOR_TYPE", "COMOR_TYPE_ATE",
                   "COMOR_ASSOC", "COMOR_ASSOC_IM",
                   "COMOR_ASSOC_ICC", "COMOR_ASSOC_VASC",
                   "COMOR_ASSOC_CEREB_VASC", "COMOR_ASSOC_HEPAT_NSEV",
                   "COMOR_ASSOC_DEMENC", "COMOR_ASSOC_PULM",
                   "COMOR_ASSOC_SYST", "COMOR_ASSOC_ULC",
                   "COMOR_ASSOC_DIAB_SCOMP", "COMOR_ASSOC_DIAB_ACOMP",
                   "COMOR_ASSOC_HEMI", "COMOR_ASSOC_LEUCE",
                   "COMOR_ASSOC_LYMPH", "COMOR_ASSOC_TUM_SMETA",
                   "COMOR_ASSOC_HEPAT_SEV", "COMOR_ASSOC_VIH",
                   "COMOR_ASSOC_TUM_AMETA", "COMOR_ASSOC_SCORE",
                   "COMOR_AUTONOM", "COMOR_AUTONOM_QUOTI",
                   "COMOR_AUTONOM_DEPLAC")

categorical3 <- c("COMOR_F",  "COMOR_TYPE",
                "COMOR_TYPE_ATE",  "COMOR_ASSOC",
                "COMOR_ASSOC_IM", "COMOR_ASSOC_ICC",
                "COMOR_ASSOC_VASC", "COMOR_ASSOC_CEREB_VASC",
                "COMOR_ASSOC_HEPAT_NSEV", "COMOR_ASSOC_DEMENC",
                "COMOR_ASSOC_PULM", "COMOR_ASSOC_SYST",
                "COMOR_ASSOC_ULC",  "COMOR_ASSOC_DIAB_SCOMP",
                "COMOR_ASSOC_DIAB_ACOMP", "COMOR_ASSOC_HEMI",
                "COMOR_ASSOC_LEUCE", "COMOR_ASSOC_LYMPH",
                "COMOR_ASSOC_TUM_SMETA", "COMOR_ASSOC_HEPAT_SEV",
                "COMOR_ASSOC_VIH", "COMOR_ASSOC_TUM_AMETA",
                "COMOR_ASSOC_SCORE", "COMOR_AUTONOM",
                "COMOR_AUTONOM_QUOTI", "COMOR_AUTONOM_DEPLAC")

des3 <- CreateTableOne(vars = variables3, data = didd, factorVars = categorical3, includeNA = TRUE)
print(des3, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

variables4 <- c("TECH_DEB_DTE",
                   "TECH_FORMA_NB",
                   "TECH_PRESCR_SEANCE_NB",
                   "TECH_INSTAL_DTE",
                   "TECH_ABORD_TYPE",
                   "TECH_ABORD_LOC",
                   "TECH_ABORD_LOC_PRE",
                   "TECH_ABORD_DTE")

categorical4 <- c("TECH_DEB_DTE",
                "TECH_PRESCR_SEANCE_NB",
                "TECH_ABORD_TYPE",
                "TECH_ABORD_LOC",
                "TECH_ABORD_LOC_PRE")

des4 <- CreateTableOne(vars = variables4, data = didd, factorVars = categorical4, includeNA = TRUE)
print(des4, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

# analyse du repli----

# Arret de l assistance par l IDEL a la ponction de la FAV
# Type d arret : temporaire==0 ; definitif==1

table(did$MED_IDEL_ARRET_V0_M1, useNA = "always")
table(did$MED_IDEL_ARRET_TYPE_V0_M1, useNA = "always")

table(did$MED_IDEL_ARRET_V1_M2, useNA = "always")
table(did$MED_IDEL_ARRET_TYPE_V1_M2, useNA = "always")

table(did$MED_IDEL_ARRET_V2_M3, useNA = "always")
table(did$MED_IDEL_ARRET_TYPE_V2_M3, useNA = "always")

table(did$MED_IDEL_ARRET_V3_M4, useNA = "always")
table(did$MED_IDEL_ARRET_TYPE_V3_M4, useNA = "always")

table(did$MED_IDEL_ARRET_V4_M5, useNA = "always")
table(did$MED_IDEL_ARRET_TYPE_V4_M5, useNA = "always")

table(did$MED_IDEL_ARRET_V5_M6, useNA = "always")
table(did$MED_IDEL_ARRET_TYPE_V5_M6, useNA = "always")

table(did$MED_IDEL_ARRET_V6_M7, useNA = "always")
table(did$MED_IDEL_ARRET_TYPE_V6_M7, useNA = "always")

table(did$MED_IDEL_ARRET_V7_M8, useNA = "always")
table(did$MED_IDEL_ARRET_TYPE_V7_M8, useNA = "always")

table(did$MED_IDEL_ARRET_V8_M9, useNA = "always")
table(did$MED_IDEL_ARRET_TYPE_V8_M9, useNA = "always")

table(did$MED_IDEL_ARRET_V9_M10, useNA = "always")
table(did$MED_IDEL_ARRET_TYPE_V9_M10, useNA = "always")

table(did$MED_IDEL_ARRET_V10_M11, useNA = "always")
table(did$MED_IDEL_ARRET_TYPE_V10_M11, useNA = "always")

table(did$MED_IDEL_ARRET_V11_M12, useNA = "always")
table(did$MED_IDEL_ARRET_TYPE_V11_M12, useNA = "always")

# Replis temporaires programmes depuis la derniere visite protocolaire
table(did$StudySubjectID, useNA = "always")

table(did$RT_PROG_V0_M1, useNA = "always")
table(did$RT_PROG_V1_M2, useNA = "always")
table(did$RT_PROG_V2_M3, useNA = "always")
table(did$RT_PROG_V3_M4, useNA = "always")
table(did$RT_PROG_V4_M5, useNA = "always")
table(did$RT_PROG_V5_M6, useNA = "always")
table(did$RT_PROG_V6_M7, useNA = "always")
table(did$RT_PROG_V7_M8, useNA = "always")
table(did$RT_PROG_V8_M9, useNA = "always")
table(did$RT_PROG_V9_M10, useNA = "always")
table(did$RT_PROG_V10_M11, useNA = "always")
table(did$RT_PROG_V11_M12, useNA = "always")

# nombre total par patient (programme et non programme)

library(dplyr)

did$RT_PROG_TOT <- rowSums(did[,c("RT_PROG_NB_V0_M1", "RT_PROG_NB_V1_M2", "RT_PROG_NB_V2_M3", "RT_PROG_NB_V3_M4",
                                  "RT_PROG_NB_V4_M5", "RT_PROG_NB_V5_M6", "RT_PROG_NB_V6_M7", "RT_PROG_NB_V7_M8",
                                  "RT_PROG_NB_V8_M9" #, "RT_PROG_NB_V9_M10" 
                                  , "RT_PROG_NB_V10_M11", "RT_PROG_NB_V11_M12")], na.rm=TRUE)
table(did$RT_PROG_TOT, useNA = "always")


did$RT_PROG_TOT = (did$RT_PROG_NB_V0_M1 + did$RT_PROG_NB_V1_M2 + did$RT_PROG_NB_V2_M3 + did$RT_PROG_NB_V3_M4
  + did$RT_PROG_NB_V4_M5 + did$RT_PROG_NB_V5_M6 + did$RT_PROG_NB_V6_M7 + did$RT_PROG_NB_V7_M8
  + did$RT_PROG_NB_V8_M9 #+ did$RT_PROG_NB_V9_M10 
  + did$RT_PROG_NB_V10_M11 + did$RT_PROG_NB_V11_M12
  )
table(did$RT_PROG_TOT)

did$RT_NPROG_TOT <- rowSums(did[,c("RT_NPROG_NB_V0_M1", "RT_NPROG_NB_V1_M2", "RT_NPROG_NB_V2_M3", "RT_NPROG_NB_V3_M4",
                                                   "RT_NPROG_NB_V4_M5", "RT_NPROG_NB_V5_M6", "RT_NPROG_NB_V6_M7", "RT_NPROG_NB_V7_M8",
                                                   "RT_NPROG_NB_V8_M9" #, "RT_NPROG_NB_V9_M10" 
                                                   , "RT_NPROG_NB_V10_M11", "RT_NPROG_NB_V11_M12")], na.rm=TRUE)
table(did$RT_NPROG_TOT, useNA = "always")


# repli programme
table(did$StudySubjectID, did$RT_PROG_TOT, useNA = "always")
#     1 3 4 5 6 <NA>
#2    0 1 0 0 0    0
#3    0 0 0 0 1    0
#4    0 0 0 0 1    0
#5    1 0 0 0 0    0
#7    0 0 1 0 0    0
#8    0 0 0 0 1    0
#9    0 0 0 1 0    0
#10   0 0 0 1 0    0
#<NA> 0 0 0 0 0    0

# repli non programme
table(did$StudySubjectID, did$RT_NPROG_TOT, useNA = "always")
#     0 1 2 6 8 12 <NA>
#2    0 1 0 0 0  0    0
#3    0 0 0 1 0  0    0
#4    0 0 1 0 0  0    0
#5    1 0 0 0 0  0    0
#7    1 0 0 0 0  0    0
#8    0 0 0 1 0  0    0
#9    0 0 0 0 0  1    0
#10   0 0 0 0 1  0    0
#<NA> 0 0 0 0 0  0    0

