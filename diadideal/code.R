#-WORKSPACE----
getwd()
setwd("P:/CONSULTATION/DIADIDEAL")

#-PACKAGES----

#install.packages("readxl")
require("readxl")
require("tidyverse")
require("tableone")

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
di$TECH_INSTAL_DTE = as.numeric(as.character(di$TECH_INSTAL_DTE))
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

# descriptive des patients installés 

did <- di[!(di$instHDD == "N"),] 
dim(did)

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
                              COMOR_ASSOC_SCORE_CALC,
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

variables <- c("AGE", "SEXE", "DEMO_DIPLOM", "DEMO_ACT_PRO",
               "ENV_LIEU", "ENV_DIST_NEPHRO",
               "ENV_DIST_FORMA", "ENV_DIST_IDEL",
               "ENV_DIST_MED_TRT", "ENV_CAD",
               "ENV_CAD_DIST", "ENV_CDM",
               "ENV_CDM_DIST", "ENV_CD",
               "ENV_CD_DIST")

categorical <- c("SEXE", "DEMO_DIPLOM", "DEMO_ACT_PRO")


des1 <- CreateTableOne(vars = variables, data = didd, factorVars = categorical, includeNA = TRUE)
print(tab.one, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
