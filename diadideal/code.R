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
table(did$StudySubjectID, did$RT_PROG_TOT, useNA = "always")



did$RT_NPROG_TOT <- rowSums(did[,c("RT_NPROG_NB_V0_M1", "RT_NPROG_NB_V1_M2", "RT_NPROG_NB_V2_M3", "RT_NPROG_NB_V3_M4",
                                                   "RT_NPROG_NB_V4_M5", "RT_NPROG_NB_V5_M6", "RT_NPROG_NB_V6_M7", "RT_NPROG_NB_V7_M8",
                                                   "RT_NPROG_NB_V8_M9" #, "RT_NPROG_NB_V9_M10" 
                                                   , "RT_NPROG_NB_V10_M11", "RT_NPROG_NB_V11_M12")], na.rm=TRUE)
table(did$RT_NPROG_TOT, useNA = "always")
hist(did$RT_NPROG_TOT)
table(did$StudySubjectID, did$RT_NPROG_TOT, useNA = "always")


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

# repli definitif 
table(did$StudySubjectID, did$REPL_DEF_V0_M1, useNA = "always")
table(did$StudySubjectID, did$REPL_DEF_V1_M2, useNA = "always")
table(did$StudySubjectID, did$REPL_DEF_V2_M3, useNA = "always")
table(did$StudySubjectID, did$REPL_DEF_V3_M4, useNA = "always")
table(did$StudySubjectID, did$REPL_DEF_V4_M5, useNA = "always")
table(did$StudySubjectID, did$REPL_DEF_V5_M6, useNA = "always")
table(did$StudySubjectID, did$REPL_DEF_V6_M7, useNA = "always")
table(did$StudySubjectID, did$REPL_DEF_V7_M8, useNA = "always")
table(did$StudySubjectID, did$REPL_DEF_V8_M9, useNA = "always")
table(did$StudySubjectID, did$REPL_DEF_V9_M10, useNA = "always")
table(did$StudySubjectID, did$REPL_DEF_V10_M11, useNA = "always")
table(did$StudySubjectID, did$REPL_DEF_V11_M12, useNA = "always")

did$REPL_DEF_V0_M1[is.na(did$REPL_DEF_V0_M1)] <- 1
did$REPL_DEF_V1_M2[is.na(did$REPL_DEF_V1_M2)] <- 1
did$REPL_DEF_V2_M3[is.na(did$REPL_DEF_V2_M3)] <- 1
did$REPL_DEF_V3_M4[is.na(did$REPL_DEF_V3_M4)] <- 1
did$REPL_DEF_V4_M5[is.na(did$REPL_DEF_V4_M5)] <- 1
did$REPL_DEF_V5_M6[is.na(did$REPL_DEF_V5_M6)] <- 1
did$REPL_DEF_V6_M7[is.na(did$REPL_DEF_V6_M7)] <- 1
did$REPL_DEF_V7_M8[is.na(did$REPL_DEF_V7_M8)] <- 1
did$REPL_DEF_V8_M9[is.na(did$REPL_DEF_V8_M9)] <- 1
did$REPL_DEF_V9_M10[is.na(did$REPL_DEF_V9_M10)] <- 1
did$REPL_DEF_V10_M11[is.na(did$REPL_DEF_V10_M11)] <- 1
did$REPL_DEF_V11_M12[is.na(did$REPL_DEF_V11_M12)] <- 1


did <- did %>%
  mutate(REPL_DEF = case_when(
    REPL_DEF_V0_M1 != 0 ~ "M1",
    REPL_DEF_V1_M2 != 0 ~ "M2",
    REPL_DEF_V2_M3 != 0 ~ "M3",
    REPL_DEF_V3_M4 != 0 ~ "M4",
    REPL_DEF_V4_M5 != 0 ~ "M5",
    REPL_DEF_V5_M6 != 0 ~ "M6",
    REPL_DEF_V6_M7 != 0 ~ "M7",
    REPL_DEF_V7_M8 != 0 ~ "M8",
    REPL_DEF_V8_M9 != 0 ~ "M9",
    REPL_DEF_V9_M10 != 0 ~ "M10",
    REPL_DEF_V10_M11 != 0 ~ "M11",
    REPL_DEF_V11_M12 != 0 ~ "M12"
  ))
table(did$REPL_DEF)
table(did$StudySubjectID, did$REPL_DEF, useNA = "always")

# MED_SEANCE_NB	
# Nombre de seances d HDD depuis la derniere visite protocolaire

table(did$MED_SEANCE_NB_V0_M1, useNA = "always")

did$MED_SEANCE_NBTOT <- rowSums(did[,c("MED_SEANCE_NB_V0_M1", "MED_SEANCE_NB_V1_M2", "MED_SEANCE_NB_V2_M3", "MED_SEANCE_NB_V3_M4",
                                   "MED_SEANCE_NB_V4_M5", "MED_SEANCE_NB_V5_M6", "MED_SEANCE_NB_V6_M7", "MED_SEANCE_NB_V7_M8",
                                   "MED_SEANCE_NB_V8_M9", "MED_SEANCE_NB_V10_M11" 
                                   , "MED_SEANCE_NB_V11_M12")], na.rm=TRUE)

table(did$StudySubjectID, did$MED_SEANCE_NBTOT, useNA = "always")

# DESCRIPTIF PAR PATIENT ET PAR VISITE 
# trois variables à inclure dans l'analyse 

table(did$StudySubjectID, did$MED_SEANCE_NB_V0_M1, useNA = "always")
table(did$StudySubjectID, did$MED_SEANCE_NB_V1_M2, useNA = "always")
table(did$StudySubjectID, did$MED_SEANCE_NB_V2_M3, useNA = "always")
table(did$StudySubjectID, did$MED_SEANCE_NB_V3_M4, useNA = "always")
table(did$StudySubjectID, did$MED_SEANCE_NB_V4_M5, useNA = "always")
table(did$StudySubjectID, did$MED_SEANCE_NB_V5_M6, useNA = "always")
table(did$StudySubjectID, did$MED_SEANCE_NB_V6_M7, useNA = "always")
table(did$StudySubjectID, did$MED_SEANCE_NB_V7_M8, useNA = "always")
table(did$StudySubjectID, did$MED_SEANCE_NB_V8_M9, useNA = "always")
table(did$StudySubjectID, did$MED_SEANCE_NB_V9_M10, useNA = "always")
table(did$StudySubjectID, did$MED_SEANCE_NB_V10_M11, useNA = "always")
table(did$StudySubjectID, did$MED_SEANCE_NB_V11_M12, useNA = "always")


# histogramme nb replis

did$RT_PROG_V0_M1[is.na(did$RT_PROG_V0_M1)] <- 0 #1
did$RT_PROG_V1_M2[is.na(did$RT_PROG_V1_M2)] <- 0 #0
did$RT_PROG_V2_M3[is.na(did$RT_PROG_V2_M3)] <- 0 #6
did$RT_PROG_V3_M4[is.na(did$RT_PROG_V3_M4)] <- 0 #1
did$RT_PROG_V4_M5[is.na(did$RT_PROG_V4_M5)] <- 0 #1
did$RT_PROG_V5_M6[is.na(did$RT_PROG_V5_M6)] <- 0 #7
did$RT_PROG_V6_M7[is.na(did$RT_PROG_V6_M7)] <- 0 #1
did$RT_PROG_V7_M8[is.na(did$RT_PROG_V7_M8)] <- 0 #2
did$RT_PROG_V8_M9[is.na(did$RT_PROG_V8_M9)] <- 0 #5
did$RT_PROG_V9_M10[is.na(did$RT_PROG_V9_M10)] <- 0 #0
did$RT_PROG_V10_M11[is.na(did$RT_PROG_V10_M11)] <- 0 #1
did$RT_PROG_V11_M12[is.na(did$RT_PROG_V11_M12)] <- 0 #6

histvar <- c(1,0,6,1,1,7,1,2,5,0,1,6)
hist(histvar)

# Load ggplot2
library(ggplot2)

# Create data
data <- data.frame(
  name=c("M01","M02","M03","M04","M05", "M06", "M07", "M08", "M09", "M10", "M11", "M12") ,  
  value=c(1,0,6,1,1,7,1,2,5,0,1,6)
)

# Barplot
ggplot(data, aes(x=name, y=value)) + 
  geom_bar(stat = "identity",color="blue", fill=rgb(0.1,0.4,0.5,0.7)) 


# FLOW CHART----

library(Gmisc, quietly = TRUE)
library(glue)
library(htmlTable)
library(grid)
library(magrittr)

org_cohort <- boxGrob(glue("Patients informés",
                           "n = {pop}",
                           pop = txtInt(10),
                           .sep = "\n"))
eligible <- boxGrob(glue("Inclusion",
                         "n = {pop}",
                         pop = txtInt(10),
                         .sep = "\n"))
included <- boxGrob(glue("Installation HD assistée",
                         "n = {incl}",
                         incl = txtInt(8),
                         .sep = "\n"))
grp_a <- boxGrob(glue("HDD à 12 mois",
                      "n = {recr}",
                      recr = txtInt(6),
                      .sep = "\n"))

grp_b <- boxGrob(glue("Décès",
                      "n = {recr}",
                      recr = txtInt(1),
                      .sep = "\n"))

grp_c <- boxGrob(glue("Repli",
                      "n = {recr}",
                      recr = txtInt(1),
                      .sep = "\n"))

excluded <- boxGrob(glue("Excluded (n = {tot}):",
                         " - thrombose FAV : {uninterested}",
                         " - échec formation : {contra}",
                         tot = 10,
                         uninterested = 1,
                         contra = 1,
                         .sep = "\n"),
                    just = "left")

grid.newpage()
vert <- spreadVertical(org_cohort,
                       eligible = eligible,
                       included = included,
                       grps = grp_a)
grps <- alignVertical(reference = vert$grps,
                      grp_a, grp_b, grp_c) %>%
  spreadHorizontal()
vert$grps <- NULL

excluded <- moveBox(excluded,
                    x = .8,
                    y = coords(vert$included)$top + distance(vert$eligible, vert$included, half = TRUE, center = FALSE))

for (i in 1:(length(vert) - 1)) {
  connectGrob(vert[[i]], vert[[i + 1]], type = "vert") %>%
    print
}
connectGrob(vert$included, grps[[1]], type = "N")
connectGrob(vert$included, grps[[2]], type = "N")
connectGrob(vert$included, grps[[3]], type = "N")

connectGrob(vert$eligible, excluded, type = "L")

# Print boxes
vert
grps
excluded

# qualité de vie 
table(did$KDQOL_F_V0_M1)
KDQOL_V0M1 = rowSums(did[,c("KDQOL_Q01_SANTE_V0_M1", "KDQOL_Q02_ANNEE_V0_M1", "KDQOL_Q03A_IMPORTANT_V0_M1", 
                            "KDQOL_Q03B_MODER_V0_M1", "KDQOL_Q03C_COURSE_V0_M1", "KDQOL_Q03D_PLSRS_ETAGES_V0_M1", "KDQOL_Q03E_ETAGE_V0_M1", 
                              "KDQOL_Q03F_GENOU_V0_M1", "KDQOL_Q03G_KM_V0_M1", "KDQOL_Q03H_PLSRS_CENTAINES_V0_M1", "KDQOL_Q03I_CENTAINE_V0_M1")], na.rm=TRUE)
KDQOL_V0M1_2 = rowSums(did[,c("KDQOL_Q03J_DOUCHE_V0_M1", "KDQOL_Q04A_TRAVAIL_V0_M1", "KDQOL_Q04B_ACCOMPLI_V0_M1", "KDQOL_Q04C_ARRET_V0_M1")], na.rm=TRUE)
KDQOL_V0M1_2 = rowSums(did[,c("KDQOL_Q04D_DIFFIC_V0_M1", "KDQOL_Q05A_TRAVAIL_V0_M1", "KDQOL_Q05B_ACCOMPLI_V0_M1", "KDQOL_Q05C_SOIN_V0_M1")], na.rm=TRUE)
                              "KDQOL_Q06_GENE_V0_M1", "KDQOL_Q07_INTENS_V0_M1", "KDQOL_Q08_LIMITE_V0_M1", 
                            #"KDQOL_Q09A_DYNAM_V0_M1", 
                              )], na.rm=TRUE)
                              "KDQOL_Q09B_NERV_V0_M1", "KDQOL_Q09C_MORAL_V0_M1", "KDQOL_Q09D_CALME_V0_M1", "KDQOL_Q09E_ENERGIE_V0_M1", 
                              "KDQOL_Q09F_TRISTE_V0_M1", "KDQOL_Q09G_EPUISE_V0_M1", "KDQOL_Q09H_HEUR_V0_M1", "KDQOL_Q09I_FATIG_V0_M1", 
                              "KDQOL_Q10_GENE_V0_M1", "KDQOL_Q11A_MALADE_V0_M1", "KDQOL_Q11B_PORTE_BIEN_V0_M1", "KDQOL_Q11C_DEGRADE_V0_M1", 
                              "KDQOL_Q11D_EXCELL_V0_M1", 
                             #"KDQOL_Q12A_RENALE_V0_M1", 
                            "KDQOL_Q12B_TEMPS_V0_M1", "KDQOL_Q12C_SUPPORTE_V0_M1", 
                              "KDQOL_Q12D_POIDS_V0_M1", "KDQOL_Q13A_ISOLE_V0_M1", "KDQOL_Q13B_REAG_V0_M1", "KDQOL_Q13C_AGRESS_V0_M1", 
                              "KDQOL_Q13D_CONCENTR_V0_M1", "KDQOL_Q13E_ENTENDU_V0_M1", "KDQOL_Q13F_PERTURBE_V0_M1", "KDQOL_Q14A_COURBAT_V0_M1", 
                              "KDQOL_Q14B_POITR_V0_M1", "KDQOL_Q14C_CRAMPE_V0_M1", "KDQOL_Q14D_DEMANG_V0_M1", "KDQOL_Q14E_PEAU_V0_M1", 
                              "KDQOL_Q14F_ESSOUFFL_V0_M1", "KDQOL_Q14G_VERTIGE_V0_M1", "KDQOL_Q14H_APPET_V0_M1", "KDQOL_Q14I_EPUIS_V0_M1", 
                              "KDQOL_Q14J_ENGOURDI_V0_M1", "KDQOL_Q14K_VOMIR_V0_M1", "KDQOL_Q14M_FISTULE_V0_M1", "KDQOL_Q15A_BOISSON_V0_M1", 
                              "KDQOL_Q15B_ALIM_V0_M1", "KDQOL_Q15C_MAISON_V0_M1", "KDQOL_Q15D_VOYAGE_V0_M1", "KDQOL_Q15E_MEDECIN_V0_M1", 
                              "KDQOL_Q15F_STRESS_V0_M1", "KDQOL_Q15G_SEX_VIE_V0_M1", "KDQOL_Q15H_APPAR_V0_M1", "KDQOL_Q16A_SEX_PLAIS_V0_M1", 
                              "KDQOL_Q16B_SEX_ABS_V0_M1", "KDQOL_Q17_SOMMEIL_V0_M1", "KDQOL_Q18A_REVEIL_V0_M1", "KDQOL_Q18B_SUFFIS_V0_M1", 
                              "KDQOL_Q18C_SOMNOLE_V0_M1", "KDQOL_Q19A_TEMPS_V0_M1", "KDQOL_Q19B_SOUTIEN_V0_M1", "KDQOL_Q20_REMUNERE_V0_M1", 
                              "KDQOL_Q21_EMPECHE_V0_M1", "KDQOL_Q22_SANTE_GLOBAL_V0_M1")], na.rm=TRUE)

EQV0M1 = rowSums(did[,c("EQ_Q01_MOBIL_V0_M1","EQ_Q02_AUTON_V0_M1","EQ_Q03_ACT_V0_M1",
"EQ_Q04_DOUL_V0_M1","EQ_Q05_ANX_V0_M1")], na.rm=TRUE)

EQV1M2 = rowSums(did[,c("EQ_Q01_MOBIL_V1_M2","EQ_Q02_AUTON_V1_M2","EQ_Q03_ACT_V1_M2",
                        "EQ_Q04_DOUL_V1_M2","EQ_Q05_ANX_V1_M2")], na.rm=TRUE)

EQV2M3 = rowSums(did[,c("EQ_Q01_MOBIL_V2_M3","EQ_Q02_AUTON_V2_M3","EQ_Q03_ACT_V2_M3",
                        "EQ_Q04_DOUL_V2_M3","EQ_Q05_ANX_V2_M3")], na.rm=TRUE)

EQV3M4 = rowSums(did[,c("EQ_Q01_MOBIL_V3_M4","EQ_Q02_AUTON_V3_M4","EQ_Q03_ACT_V3_M4",
                        "EQ_Q04_DOUL_V3_M4","EQ_Q05_ANX_V3_M4")], na.rm=TRUE)

EQV4M5 = rowSums(did[,c("EQ_Q01_MOBIL_V4_M5","EQ_Q02_AUTON_V4_M5","EQ_Q03_ACT_V4_M5",
                        "EQ_Q04_DOUL_V4_M5","EQ_Q05_ANX_V4_M5")], na.rm=TRUE)

EQV5M6 = rowSums(did[,c("EQ_Q01_MOBIL_V5_M6","EQ_Q02_AUTON_V5_M6","EQ_Q03_ACT_V5_M6",
                        "EQ_Q04_DOUL_V5_M6","EQ_Q05_ANX_V5_M6")], na.rm=TRUE)

EQV6M7 = rowSums(did[,c("EQ_Q01_MOBIL_V6_M7","EQ_Q02_AUTON_V6_M7","EQ_Q03_ACT_V6_M7",
                        "EQ_Q04_DOUL_V6_M7","EQ_Q05_ANX_V6_M7")], na.rm=TRUE)

EQV7M7 = rowSums(did[,c("EQ_Q01_MOBIL_V6_M7","EQ_Q02_AUTON_V6_M7","EQ_Q03_ACT_V6_M7",
                        "EQ_Q04_DOUL_V6_M7","EQ_Q05_ANX_V6_M7")], na.rm=TRUE)
