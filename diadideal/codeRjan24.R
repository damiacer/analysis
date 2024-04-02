# workspace----
getwd()
#setwd("P:/CONSULTATION/DIADIDEAL")
setwd("/Users/damianocerasuolo/Desktop/UBRC/UBRS_CONSULT_MAC/DIADIDEAL")

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
table(di$TECH_INSTAL_DTEn, useNA = "always")
str(di$TECH_INSTAL_DTE)

# installation HDD
di <- di %>%
  mutate(instHDD = case_when(
    TECH_INSTAL_DTEn > 0 ~ "Y"
  ))
table(di$instHDD)
di$instHDD[is.na(di$instHDD)] <- "N"
table(di$instHDD)

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
                              TECH_ABORD_DTE
                              ))
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

variables5 <- c()

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


# BIO DESCRIPTIVE--- 

biod <- subset(did, select = c(EXAM_POIDS_V0_M1,
                               EXAM_POIDS_V1_M2,
                               EXAM_POIDS_V2_M3,
                               EXAM_POIDS_V3_M4,
                               EXAM_POIDS_V4_M5,
                               EXAM_POIDS_V5_M6,
                               EXAM_POIDS_V6_M7,
                               EXAM_POIDS_V7_M8,
                               EXAM_POIDS_V8_M9,
                               EXAM_POIDS_V9_M10,
                               EXAM_POIDS_V10_M11,
                               EXAM_POIDS_V11_M12,
                               EXAM_PAS_V0_M1,
                               EXAM_PAS_V1_M2,
                               EXAM_PAS_V2_M3, 
                               EXAM_PAS_V3_M4,
                               EXAM_PAS_V4_M5,
                               EXAM_PAS_V5_M6,
                               EXAM_PAS_V6_M7,
                               EXAM_PAS_V7_M8,
                               EXAM_PAS_V8_M9,
                               EXAM_PAS_V9_M10,
                               EXAM_PAS_V10_M11,
                               EXAM_PAS_V11_M12,
                               EXAM_PAD_V0_M1,
                               EXAM_PAD_V1_M2,
                               EXAM_PAD_V2_M3,
                               EXAM_PAD_V3_M4,
                               EXAM_PAD_V4_M5,
                               EXAM_PAD_V5_M6,
                               EXAM_PAD_V6_M7,
                               EXAM_PAD_V7_M8,
                               EXAM_PAD_V8_M9,
                               EXAM_PAD_V9_M10,
                               EXAM_PAD_V10_M11,
                               EXAM_PAD_V11_M12,
                               BIO_PRE_F_INCL,
                               BIO_PRE_F_V2_M3,
                               BIO_PRE_F_V5_M6,
                               BIO_PRE_F_V8_M9,
                               BIO_PRE_F_V11_M12,
                               BIO_PRE_UREE_INCL,
                               BIO_PRE_UREE_V2_M3,
                               BIO_PRE_UREE_V5_M6,
                               BIO_PRE_UREE_V8_M9,
                               BIO_PRE_UREE_V11_M12,
                               BIO_PRE_CREAT_INCL,
                               BIO_PRE_CREAT_V2_M3,
                               BIO_PRE_CREAT_V5_M6,
                               BIO_PRE_CREAT_V8_M9,
                               BIO_PRE_CREAT_V11_M12,
                               
                               BIO_PRE_BICARB_INCL,
                               BIO_PRE_BICARB_V2_M3,
                               BIO_PRE_BICARB_V5_M6,
                               BIO_PRE_BICARB_V8_M9,
                               BIO_PRE_BICARB_V11_M12,
                               BIO_PRE_CALC_INCL,
                               BIO_PRE_CALC_V2_M3,
                               BIO_PRE_CALC_V5_M6,
                               BIO_PRE_CALC_V8_M9,
                               BIO_PRE_CALC_V11_M12,
                               BIO_PRE_PHOS_INCL,
                               BIO_PRE_PHOS_V2_M3,
                               BIO_PRE_PHOS_V5_M6,
                               BIO_PRE_PHOS_V8_M9,
                               BIO_PRE_PHOS_V11_M12,
                               
                               BIO_PRE_PTH_INCL,
                               BIO_PRE_PTH_V2_M3,
                               BIO_PRE_PTH_V5_M6,
                               BIO_PRE_PTH_V8_M9,
                               BIO_PRE_PTH_V11_M12,
                               BIO_PRE_PROT_INCL,
                               BIO_PRE_PROT_V2_M3,
                               BIO_PRE_PROT_V5_M6,
                               BIO_PRE_PROT_V8_M9,
                               BIO_PRE_PROT_V11_M12,
                               BIO_PRE_ALBU_INCL,
                               BIO_PRE_ALBU_V2_M3,
                               BIO_PRE_ALBU_V5_M6,
                               BIO_PRE_ALBU_V8_M9,
                               BIO_PRE_ALBU_V11_M12,
                               
                               BIO_PRE_HEMO_INCL,
                               BIO_PRE_HEMO_V2_M3,
                               BIO_PRE_HEMO_V5_M6,
                               BIO_PRE_HEMO_V8_M9,
                               BIO_PRE_HEMO_V11_M12,
                               BIO_PRE_FERR_INCL,
                               BIO_PRE_FERR_V2_M3,
                               BIO_PRE_FERR_V5_M6,
                               BIO_PRE_FERR_V8_M9,
                               BIO_PRE_FERR_V11_M12,
                               BIO_PRE_TRANSF_INCL,
                               BIO_PRE_TRANSF_V2_M3,
                               BIO_PRE_TRANSF_V5_M6,
                               BIO_PRE_TRANSF_V8_M9,
                               BIO_PRE_TRANSF_V11_M12,
                               BIO_PRE_B2_INCL,
                               BIO_PRE_B2_V2_M3,
                               BIO_PRE_B2_V5_M6,
                               BIO_PRE_B2_V8_M9,
                               BIO_PRE_B2_V11_M12,
                               
                               BIO_POST_F_INCL,
                               BIO_POST_F_V2_M3,
                               BIO_POST_F_V5_M6,
                               BIO_POST_F_V8_M9,
                               BIO_POST_F_V11_M12,
                               BIO_POST_UREE_INCL,
                               BIO_POST_UREE_V2_M3,
                               BIO_POST_UREE_V5_M6,
                               BIO_POST_UREE_V8_M9,
                               BIO_POST_UREE_V11_M12,
                               DIAL_PRE_POIDS_INCL,
                               DIAL_PRE_POIDS_V2_M3,
                               DIAL_PRE_POIDS_V5_M6,
                               DIAL_PRE_POIDS_V8_M9,
                               DIAL_PRE_POIDS_V11_M12,
                               
                               DIAL_POST_POIDS_INCL,
                               DIAL_POST_POIDS_V2_M3,
                               DIAL_POST_POIDS_V5_M6,
                               DIAL_POST_POIDS_V8_M9,
                               DIAL_POST_POIDS_V11_M12,
                               DIAL_DUREE_INCL,
                               DIAL_DUREE_V2_M3,
                               DIAL_DUREE_V5_M6,
                               DIAL_DUREE_V8_M9,
                               DIAL_DUREE_V11_M12,
                               DIAL_UF_INCL,
                               DIAL_UF_V2_M3,
                               DIAL_UF_V5_M6,
                               DIAL_UF_V8_M9,
                               DIAL_UF_V11_M12,
                               DIAL_NB_INCL,
                               DIAL_NB_V2_M3,
                               DIAL_NB_V5_M6,
                               DIAL_NB_V8_M9,
                               DIAL_NB_V11_M12,
                               DIAL_KTV_INCL,
                               DIAL_KTV_V2_M3,
                               DIAL_KTV_V8_M9,
                               DIAL_KTV_V11_M12
                               ))


var_poids <- c("EXAM_POIDS_V0_M1",
               "EXAM_POIDS_V1_M2",
               "EXAM_POIDS_V2_M3",
               "EXAM_POIDS_V3_M4",
               "EXAM_POIDS_V4_M5",
               "EXAM_POIDS_V5_M6",
               "EXAM_POIDS_V6_M7",
               "EXAM_POIDS_V7_M8",
               "EXAM_POIDS_V8_M9",
               "EXAM_POIDS_V9_M10",
               "EXAM_POIDS_V10_M11",
               "EXAM_POIDS_V11_M12")

fact_vars <- c("")


exampoids <- CreateTableOne(vars = var_poids, data = biod, factorVars = , 
                       includeNA = TRUE)
print(exampoids, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(exampoids)

#                    n miss p.miss mean sd median p25 p75 min max skew kurt
#EXAM_POIDS_V0_M1   8    0      0   69 22     60  56  80  46 108  1.0 -0.4
#EXAM_POIDS_V1_M2   8    0      0   70 22     61  56  81  46 105  0.9 -0.6
#EXAM_POIDS_V2_M3   8    0      0   70 21     61  58  80  47 107  1.0 -0.4
#EXAM_POIDS_V3_M4   8    1     12   72 22     62  60  86  46 105  0.7 -1.0
#EXAM_POIDS_V4_M5   8    1     12   72 22     61  60  86  45 105  0.7 -0.9
#EXAM_POIDS_V5_M6   8    1     12   72 22     60  60  86  45 106  0.7 -0.9
#EXAM_POIDS_V6_M7   8    1     12   71 22     60  60  86  44 105  0.7 -0.9
#EXAM_POIDS_V7_M8   8    1     12   72 22     61  60  85  45 106  0.7 -0.7
#EXAM_POIDS_V8_M9   8    2     25   76 20     68  60  90  59 106  0.8 -1.4
#EXAM_POIDS_V9_M10  8    2     25   76 20     68  60  90  60 106  0.9 -1.3
#EXAM_POIDS_V10_M11 8    2     25   76 20     68  60  88  59 106  0.9 -1.0
#EXAM_POIDS_V11_M12 8    2     25   76 20     68  60  88  59 107  0.9 -0.9

var_pas <- c( "EXAM_PAS_V0_M1",
        "EXAM_PAS_V1_M2",
        "EXAM_PAS_V2_M3", 
        "EXAM_PAS_V3_M4",
        "EXAM_PAS_V4_M5",
        "EXAM_PAS_V5_M6",
        "EXAM_PAS_V6_M7",
        "EXAM_PAS_V7_M8",
        "EXAM_PAS_V8_M9",
        "EXAM_PAS_V9_M10",
        "EXAM_PAS_V10_M11",
        "EXAM_PAS_V11_M12")

fact_vars <- c("")

exampas <- CreateTableOne(vars = var_pas, data = biod, factorVars = , 
                            includeNA = TRUE)
print(exampas, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(exampas)

#                n miss p.miss mean sd median p25 p75 min max   skew  kurt
#EXAM_PAS_V0_M1   8    0      0  150 15    154 136 158 127 170 -0.259 -1.26
#EXAM_PAS_V1_M2   8    0      0  157 14    154 144 172 141 175  0.224 -2.03
#EXAM_PAS_V2_M3   8    0      0  158 20    160 145 170 125 189 -0.163 -0.15
#EXAM_PAS_V3_M4   8    1     12  156 25    167 139 172 122 183 -0.802 -1.14
#EXAM_PAS_V4_M5   8    2     25  166 23    166 146 186 142 187 -0.022 -3.26
#EXAM_PAS_V5_M6   8    1     12  149 25    140 137 166 111 184 -0.002 -0.58
#EXAM_PAS_V6_M7   8    1     12  160 15    157 150 168 141 186  0.653 -0.04
#EXAM_PAS_V7_M8   8    1     12  159 28    175 142 176 111 187 -0.967 -0.49
#EXAM_PAS_V8_M9   8    2     25  146 13    142 138 153 130 166  0.648 -0.41
#EXAM_PAS_V9_M10  8    2     25  146 14    152 134 156 129 160 -0.749 -1.89
#EXAM_PAS_V10_M11 8    2     25  137 31    134 124 162  90 171 -0.398 -0.61
#EXAM_PAS_V11_M12 8    2     25  151 25    160 141 167 108 173 -1.255  0.83

var_pad <- c("EXAM_PAD_V0_M1",
        "EXAM_PAD_V1_M2",
        "EXAM_PAD_V2_M3",
        "EXAM_PAD_V3_M4",
        "EXAM_PAD_V4_M5",
        "EXAM_PAD_V5_M6",
        "EXAM_PAD_V6_M7",
        "EXAM_PAD_V7_M8",
        "EXAM_PAD_V8_M9",
        "EXAM_PAD_V9_M10",
        "EXAM_PAD_V10_M11",
        "EXAM_PAD_V11_M12")

exampad <- CreateTableOne(vars = var_pad, data = biod, factorVars = , 
                          includeNA = TRUE)
print(exampad, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(exampad)

#                 n miss p.miss mean sd median p25 p75 min max  skew   kurt
#EXAM_PAD_V0_M1   8    0      0   82 15     80  70  88  65 110  0.85  0.285
#EXAM_PAD_V1_M2   8    0      0   88 14     90  81  96  66 107 -0.32 -0.720
#EXAM_PAD_V2_M3   8    0      0   87 11     88  76  97  72 101 -0.19 -1.797
#EXAM_PAD_V3_M4   8    1     12   95 19    100  83 104  65 122 -0.31 -0.395
#EXAM_PAD_V4_M5   8    2     25   94 14     90  88 102  74 112  0.08 -0.407
#EXAM_PAD_V5_M6   8    1     12   81 19     78  70  91  56 112  0.48 -0.009
#EXAM_PAD_V6_M7   8    1     12   91 12     98  80  99  76 107 -0.21 -2.081
#EXAM_PAD_V7_M8   8    1     12   90 19     96  74 104  66 112 -0.20 -2.060
#EXAM_PAD_V8_M9   8    2     25   80 12     82  75  88  59  91 -1.12  0.856
#EXAM_PAD_V9_M10  8    2     25   84 11     80  77  94  73  99  0.77 -1.815
#EXAM_PAD_V10_M11 8    2     25   78 13     78  72  87  57  93 -0.53 -0.278
#EXAM_PAD_V11_M12 8    2     25   79 12     78  69  88  66  94  0.21 -2.372

var_bio1 <- c("BIO_PRE_F_INCL",
                "BIO_PRE_F_V2_M3",
                "BIO_PRE_F_V5_M6",
                "BIO_PRE_F_V8_M9",
                "BIO_PRE_F_V11_M12",
                "BIO_PRE_UREE_INCL",
                "BIO_PRE_UREE_V2_M3",
                "BIO_PRE_UREE_V5_M6",
                "BIO_PRE_UREE_V8_M9",
                "BIO_PRE_UREE_V11_M12",
                "BIO_PRE_CREAT_INCL",
                "BIO_PRE_CREAT_V2_M3",
                "BIO_PRE_CREAT_V5_M6",
                "BIO_PRE_CREAT_V8_M9",
                "BIO_PRE_CREAT_V11_M12")

bio1 <- CreateTableOne(vars = var_bio1, data = biod, factorVars = , 
                          includeNA = TRUE)
print(bio1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(bio1)

#                      n miss p.miss  mean    sd median p25 p75 min  max  skew  kurt
#BIO_PRE_F_INCL        8    0      0   1.0   0.0      1   1   1   1    1   NaN   NaN
#BIO_PRE_F_V2_M3       8    0      0   1.0   0.0      1   1   1   1    1   NaN   NaN
#BIO_PRE_F_V5_M6       8    1     12   1.0   0.0      1   1   1   1    1   NaN   NaN
#BIO_PRE_F_V8_M9       8    2     25   0.8   0.4      1   1   1   0    1 -2.45  6.00
#BIO_PRE_F_V11_M12     8    2     25   1.0   0.0      1   1   1   1    1   NaN   NaN
#BIO_PRE_UREE_INCL     8    0      0  21.2   4.4     22  18  23  16   29  0.49  0.10
#BIO_PRE_UREE_V2_M3    8    0      0  21.4   5.3     20  20  22  14   33  1.34  3.42
#BIO_PRE_UREE_V5_M6    8    1     12  24.0   7.7     23  20  27  14   37  0.54  0.37
#BIO_PRE_UREE_V8_M9    8    3     38  23.6   5.4     24  22  24  17   32  0.59  1.55
#BIO_PRE_UREE_V11_M12  8    2     25  21.8   3.7     22  19  25  17   26  0.02 -1.96
#BIO_PRE_CREAT_INCL    8    0      0 722.4 317.4    650 568 875 259 1256  0.46 -0.04
#BIO_PRE_CREAT_V2_M3   8    0      0 749.6 362.5    702 535 940 263 1397  0.55  0.08
#BIO_PRE_CREAT_V5_M6   8    1     12 785.0 291.6    787 634 958 314 1209 -0.25  0.08
#BIO_PRE_CREAT_V8_M9   8    3     38 796.0 365.1    798 569 844 403 1366  0.97  1.29
#BIO_PRE_CREAT_V11_M12 8    2     25 713.8 222.6    734 578 887 389  962 -0.39 -1.38

var_bio2 <- c("BIO_PRE_BICARB_INCL",
              "BIO_PRE_BICARB_V2_M3",
              "BIO_PRE_BICARB_V5_M6",
              "BIO_PRE_BICARB_V8_M9",
              "BIO_PRE_BICARB_V11_M12",
              "BIO_PRE_CALC_INCL",
              "BIO_PRE_CALC_V2_M3",
              "BIO_PRE_CALC_V5_M6",
              "BIO_PRE_CALC_V8_M9",
              "BIO_PRE_CALC_V11_M12",
              "BIO_PRE_PHOS_INCL",
              "BIO_PRE_PHOS_V2_M3",
              "BIO_PRE_PHOS_V5_M6",
              "BIO_PRE_PHOS_V8_M9",
              "BIO_PRE_PHOS_V11_M12")

bio2 <- CreateTableOne(vars = var_bio2, data = biod, factorVars = , 
                       includeNA = TRUE)
print(bio2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(bio2)

#                       n miss p.miss mean  sd median p25 p75 min max  skew  kurt
#BIO_PRE_BICARB_INCL    8    0      0   22 2.9     22  20  25  18  26  0.12 -1.54
#BIO_PRE_BICARB_V2_M3   8    0      0   25 3.2     26  23  27  21  30 -0.13 -0.85
#BIO_PRE_BICARB_V5_M6   8    1     12   23 2.6     24  22  24  20  27 -0.19 -0.73
#BIO_PRE_BICARB_V8_M9   8    3     38   24 1.5     24  24  25  22  26 -0.55  0.87
#BIO_PRE_BICARB_V11_M12 8    2     25   26 2.9     27  24  28  22  29 -0.59 -1.79
#BIO_PRE_CALC_INCL      8    0      0    2 0.1      2   2   2   2   2  0.14 -0.20
#BIO_PRE_CALC_V2_M3     8    0      0    2 0.1      2   2   2   2   2  0.05 -2.07
#BIO_PRE_CALC_V5_M6     8    1     12    2 0.1      2   2   2   2   2  0.12 -0.99
#BIO_PRE_CALC_V8_M9     8    3     38    2 0.2      2   2   2   2   2 -1.18 -0.06
#BIO_PRE_CALC_V11_M12   8    2     25    2 0.2      2   2   2   2   3  1.05  1.72
#BIO_PRE_PHOS_INCL      8    0      0    2 0.5      2   1   2   1   2 -0.38 -1.39
#BIO_PRE_PHOS_V2_M3     8    0      0    2 0.4      2   1   2   1   2  1.20 -0.30
#BIO_PRE_PHOS_V5_M6     8    1     12    2 0.4      2   2   2   1   3  0.63  0.65
#BIO_PRE_PHOS_V8_M9     8    3     38    2 0.3      2   2   2   2   2 -0.15  0.42
#BIO_PRE_PHOS_V11_M12   8    2     25    2 0.4      2   1   2   1   2 -0.53 -1.81

biod$BIO_PRE_ALBU_V2_M3 = as.numeric(as.character(biod$BIO_PRE_ALBU_V2_M3))

var_bio3 <- c("BIO_PRE_PTH_INCL",
                    "BIO_PRE_PTH_V2_M3",
                    "BIO_PRE_PTH_V5_M6",
                    "BIO_PRE_PTH_V8_M9",
                    "BIO_PRE_PTH_V11_M12",
                    "BIO_PRE_PROT_INCL",
                    "BIO_PRE_PROT_V2_M3",
                    "BIO_PRE_PROT_V5_M6",
                    "BIO_PRE_PROT_V8_M9",
                    "BIO_PRE_PROT_V11_M12",
                    "BIO_PRE_ALBU_INCL",
                    "BIO_PRE_ALBU_V2_M3",
                    "BIO_PRE_ALBU_V5_M6",
                    "BIO_PRE_ALBU_V8_M9",
                    "BIO_PRE_ALBU_V11_M12")

bio3 <- CreateTableOne(vars = var_bio3, data = biod, factorVars = , 
                       includeNA = TRUE)
print(bio3, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(bio3)

#                     n miss p.miss mean  sd median p25 p75 min  max  skew kurt
#BIO_PRE_PTH_INCL     8    0      0  400 455    270  92 530  26 1413  1.86  3.9
#BIO_PRE_PTH_V2_M3    8    0      0  446 499    266 126 498  66 1489  1.66  2.2
#BIO_PRE_PTH_V5_M6    8    1     12  732 859    353 218 936  21 2443  1.63  2.4
#BIO_PRE_PTH_V8_M9    8    3     38  556 508    348 298 463 222 1451  2.07  4.4
#BIO_PRE_PTH_V11_M12  8    2     25  564 446    404 340 773  62 1297  0.93  0.2
#BIO_PRE_PROT_INCL    8    0      0   67   4     68  65  70  61   73 -0.19 -0.4
#BIO_PRE_PROT_V2_M3   8    0      0   68   5     70  66  72  59   74 -0.89  0.3
#BIO_PRE_PROT_V5_M6   8    1     12   69   3     71  68  72  65   72 -1.03 -1.0
#BIO_PRE_PROT_V8_M9   8    3     38   72   6     74  69  74  63   78 -0.79  0.2
#BIO_PRE_PROT_V11_M12 8    2     25   71   5     70  68  72  66   79  1.25  1.8
#BIO_PRE_ALBU_INCL    8    0      0   36   6     37  32  39  28   45  0.05 -0.9
#BIO_PRE_ALBU_V2_M3   8    1     12   39   3     38  37  42  34   42 -0.25 -1.4
#BIO_PRE_ALBU_V5_M6   8    1     12   40   2     39  39  42  37   43  0.16 -0.9
#BIO_PRE_ALBU_V8_M9   8    3     38   40   5     37  37  43  35   48  0.96 -0.6
#BIO_PRE_ALBU_V11_M12 8    2     25   40   4     40  36  42  35   46  0.25 -0.6

biod$BIO_PRE_FERR_V2_M3 = as.numeric(as.character(biod$BIO_PRE_FERR_V2_M3))
biod$BIO_PRE_TRANSF_V2_M3 = as.numeric(as.character(biod$BIO_PRE_TRANSF_V2_M3))

var_bio4 <- c("BIO_PRE_HEMO_INCL",
              "BIO_PRE_HEMO_V2_M3",
              "BIO_PRE_HEMO_V5_M6",
              "BIO_PRE_HEMO_V8_M9",
              "BIO_PRE_HEMO_V11_M12",
              "BIO_PRE_FERR_INCL",
              "BIO_PRE_FERR_V2_M3",
              "BIO_PRE_FERR_V5_M6",
              "BIO_PRE_FERR_V8_M9",
              "BIO_PRE_FERR_V11_M12",
              "BIO_PRE_TRANSF_INCL",
              "BIO_PRE_TRANSF_V2_M3",
              "BIO_PRE_TRANSF_V5_M6",
              "BIO_PRE_TRANSF_V8_M9",
              "BIO_PRE_TRANSF_V11_M12",
              "BIO_PRE_B2_INCL",
              "BIO_PRE_B2_V2_M3",
              "BIO_PRE_B2_V5_M6",
              "BIO_PRE_B2_V8_M9",
              "BIO_PRE_B2_V11_M12")

bio4 <- CreateTableOne(vars = var_bio4, data = biod, factorVars = , 
                       includeNA = TRUE)
print(bio4, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(bio4)

#                      n miss p.miss mean  sd median p25 p75 min  max  skew  kurt
#BIO_PRE_HEMO_INCL      8    0      0   11   1     11  10  12   9   13 -0.02 -0.81
#BIO_PRE_HEMO_V2_M3     8    0      0   10   1     10   9  12   9   12  0.32 -1.91
#BIO_PRE_HEMO_V5_M6     8    1     12   10   2     10   8  11   7   13  0.04 -1.93
#BIO_PRE_HEMO_V8_M9     8    3     38   11   2     10  10  12   9   14  0.20 -0.96
#BIO_PRE_HEMO_V11_M12   8    2     25   10   2      9   9  10   9   13  1.95  4.01
#BIO_PRE_FERR_INCL      8    0      0  329 510    184 103 216  51 1580  2.72  7.54
#BIO_PRE_FERR_V2_M3     8    1     12  182 131    216  52 286  40  342 -0.06 -2.36
#BIO_PRE_FERR_V5_M6     8    1     12  253 353     73  40 320  11  964  1.76  2.63
#BIO_PRE_FERR_V8_M9     8    3     38  159 160     94  67 190  20  422  1.49  2.11
#BIO_PRE_FERR_V11_M12   8    2     25  407 320    460 132 660  14  758 -0.33 -1.98
#BIO_PRE_TRANSF_INCL    8    0      0   20  10     20  14  23   6   38  0.62  0.08
#BIO_PRE_TRANSF_V2_M3   8    1     12   23  11     23  18  28   7   41  0.29  0.16
#BIO_PRE_TRANSF_V5_M6   8    1     12   21  10     17  14  28  12   37  0.76 -1.30
#BIO_PRE_TRANSF_V8_M9   8    3     38   20   3     19  18  21  17   25  1.19  1.05
#BIO_PRE_TRANSF_V11_M12 8    2     25   28  10     24  24  28  18   46  1.75  3.65
#BIO_PRE_B2_INCL        8    0      0   28   5     30  27  32  18   32 -1.55  2.22
#BIO_PRE_B2_V2_M3       8    0      0   27   7     27  24  30  18   40  0.53  0.43
#BIO_PRE_B2_V5_M6       8    1     12   30   4     30  29  33  25   35 -0.24 -0.70
#BIO_PRE_B2_V8_M9       8    3     38   26   3     27  24  28  23   29 -0.23 -2.57
#BIO_PRE_B2_V11_M12     8    2     25   24  10     27  15  30  11   35 -0.58 -1.67

var_bio5 <- c("BIO_POST_F_INCL",
                     "BIO_POST_F_V2_M3",
                     "BIO_POST_F_V5_M6",
                     "BIO_POST_F_V8_M9",
                     "BIO_POST_F_V11_M12",
                     "BIO_POST_UREE_INCL",
                     "BIO_POST_UREE_V2_M3",
                     "BIO_POST_UREE_V5_M6",
                     "BIO_POST_UREE_V8_M9",
                     "BIO_POST_UREE_V11_M12",
                     "DIAL_PRE_POIDS_INCL",
                     "DIAL_PRE_POIDS_V2_M3",
                     "DIAL_PRE_POIDS_V5_M6",
                     "DIAL_PRE_POIDS_V8_M9",
                     "DIAL_PRE_POIDS_V11_M12")

bio5 <- CreateTableOne(vars = var_bio5, data = biod, factorVars = , 
                       includeNA = TRUE)
print(bio5, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(bio5)

#n miss p.miss mean   sd median  p25 p75 min max  skew   kurt
#BIO_POST_F_INCL        8    0      0  0.8  0.5      1  0.8   1   0   1 -1.44  4e-16
#BIO_POST_F_V2_M3       8    0      0  1.0  0.0      1  1.0   1   1   1   NaN    NaN
#BIO_POST_F_V5_M6       8    1     12  1.0  0.0      1  1.0   1   1   1   NaN    NaN
#BIO_POST_F_V8_M9       8    2     25  0.8  0.4      1  1.0   1   0   1 -2.45  6e+00
#BIO_POST_F_V11_M12     8    2     25  1.0  0.0      1  1.0   1   1   1   NaN    NaN
#BIO_POST_UREE_INCL     8    2     25 10.0  4.1      9  7.8  10   7  18  2.08  5e+00
#BIO_POST_UREE_V2_M3    8    0      0 11.6  3.1     11 10.0  12   8  18  1.64  3e+00
#BIO_POST_UREE_V5_M6    8    1     12 12.8  3.8     14 10.0  15   8  18 -0.08 -1e+00
#BIO_POST_UREE_V8_M9    8    3     38 13.7  3.0     14 12.2  15  10  18  0.19  2e-01
#BIO_POST_UREE_V11_M12  8    2     25 12.2  3.1     12 10.4  13   9  18  1.20  2e+00
#DIAL_PRE_POIDS_INCL    8    0      0 71.2 23.0     61 57.0  80  48 111  1.11 -2e-01
#DIAL_PRE_POIDS_V2_M3   8    0      0 70.7 21.1     61 58.7  81  49 107  1.02 -4e-01
#DIAL_PRE_POIDS_V5_M6   8    1     12 73.0 22.5     61 60.0  87  48 108  0.80 -9e-01
#DIAL_PRE_POIDS_V8_M9   8    2     25 77.3 21.5     68 61.3  92  59 110  0.90 -1e+00
#DIAL_PRE_POIDS_V11_M12 8    2     25 77.8 22.3     68 60.9  93  60 111  0.89 -1e+00

biod$DIAL_DUREE_INCL = as.numeric(as.character(biod$DIAL_DUREE_INCL))

var_bio6 <- c("DIAL_POST_POIDS_INCL",
              "DIAL_POST_POIDS_V2_M3",
              "DIAL_POST_POIDS_V5_M6",
              "DIAL_POST_POIDS_V8_M9",
              "DIAL_POST_POIDS_V11_M12",
              "DIAL_DUREE_INCL",
              "DIAL_DUREE_V2_M3",
              "DIAL_DUREE_V5_M6",
              "DIAL_DUREE_V8_M9",
              "DIAL_DUREE_V11_M12",
              "DIAL_UF_INCL",
              "DIAL_UF_V2_M3",
              "DIAL_UF_V5_M6",
              "DIAL_UF_V8_M9",
              "DIAL_UF_V11_M12",
              "DIAL_NB_INCL",
              "DIAL_NB_V2_M3",
              "DIAL_NB_V5_M6",
              "DIAL_NB_V8_M9",
              "DIAL_NB_V11_M12",
              "DIAL_KTV_INCL",
              "DIAL_KTV_V2_M3",
              "DIAL_KTV_V8_M9",
              "DIAL_KTV_V11_M12")

bio6 <- CreateTableOne(vars = var_bio6, data = biod, factorVars = , 
                       includeNA = TRUE)
print(bio6, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(bio6)

#                        n miss p.miss  mean    sd median   p25   p75   min   max  skew   kurt
#DIAL_POST_POIDS_INCL    8    0      0 7e+01  22.8  6e+01 6e+01 8e+01  46.7 1e+02  1.11 -2e-01
#DIAL_POST_POIDS_V2_M3   8    0      0 7e+01  20.7  6e+01 6e+01 8e+01  47.8 1e+02  0.94 -5e-01
#DIAL_POST_POIDS_V5_M6   8    1     12 7e+01  22.2  6e+01 6e+01 9e+01  46.5 1e+02  0.76 -9e-01
#DIAL_POST_POIDS_V8_M9   8    2     25 8e+01  20.9  7e+01 6e+01 9e+01  59.3 1e+02  0.87 -1e+00
#DIAL_POST_POIDS_V11_M12 8    2     25 8e+01  21.4  7e+01 6e+01 9e+01  59.5 1e+02  0.87 -1e+00
#DIAL_DUREE_INCL         8    2     25 1e+02  76.6  1e+02 1e+02 2e+02   4.0 2e+02 -0.60  2e+00
#DIAL_DUREE_V2_M3        8    0      0 1e+02  12.6  1e+02 1e+02 1e+02 120.0 2e+02  1.18 -4e-01
#DIAL_DUREE_V5_M6        8    1     12 1e+02  10.5  1e+02 1e+02 1e+02 119.0 1e+02  2.32  6e+00
#DIAL_DUREE_V8_M9        8    2     25 1e+02   8.0  1e+02 1e+02 1e+02 120.0 1e+02  1.75  4e+00
#DIAL_DUREE_V11_M12      8    2     25 1e+02  11.1  1e+02 1e+02 1e+02 120.0 2e+02  2.19  5e+00
#DIAL_UF_INCL            8    0      0 1e+03 604.6  1e+03 7e+02 2e+03 200.0 2e+03  0.03 -8e-01
#DIAL_UF_V2_M3           8    0      0 1e+03 679.8  1e+03 6e+02 2e+03 200.0 2e+03 -0.04 -7e-01
#DIAL_UF_V5_M6           8    1     12 1e+03 579.8  1e+03 1e+03 2e+03 400.0 2e+03 -0.09  3e-01
#DIAL_UF_V8_M9           8    2     25 1e+03 654.1  1e+03 8e+02 1e+03 200.0 2e+03 -0.19  2e-01
#DIAL_UF_V11_M12         8    2     25 1e+03 766.1  8e+02 4e+02 1e+03 200.0 2e+03  0.73 -4e-01
#DIAL_NB_INCL            8    0      0 5e+00   1.4  5e+00 3e+00 6e+00   3.0 6e+00 -0.34 -2e+00
#DIAL_NB_V2_M3           8    0      0 5e+00   0.5  5e+00 5e+00 5e+00   5.0 6e+00  1.44  4e-16
#DIAL_NB_V5_M6           8    1     12 5e+00   0.5  5e+00 5e+00 6e+00   5.0 6e+00  1.23 -8e-01
#DIAL_NB_V8_M9           8    2     25 5e+00   0.5  5e+00 5e+00 6e+00   5.0 6e+00  0.97 -2e+00
#DIAL_NB_V11_M12         8    2     25 5e+00   0.5  5e+00 5e+00 6e+00   5.0 6e+00  0.97 -2e+00
#DIAL_KTV_INCL           8    3     38 7e-01   0.2  8e-01 6e-01 8e-01   0.5 1e+00  0.27  3e-01
#DIAL_KTV_V2_M3          8    4     50 6e-01   0.2  6e-01 6e-01 7e-01   0.4 8e-01  0.10  1e+00
#DIAL_KTV_V8_M9          8    6     75 6e-01   0.0  6e-01 6e-01 6e-01   0.6 6e-01   NaN    NaN
#DIAL_KTV_V11_M12        8    5     62 7e-01   0.1  7e-01 6e-01 7e-01   0.6 8e-01  1.01    NaN



# quality of life----

qual = subset(did, select = c(KDQOL_Q01_SANTE_INCL,
                              KDQOL_Q01_SANTE_V0_M1, KDQOL_Q01_SANTE_V1_M2, KDQOL_Q01_SANTE_V2_M3, KDQOL_Q01_SANTE_V3_M4,
                              KDQOL_Q01_SANTE_V4_M5, KDQOL_Q01_SANTE_V5_M6, KDQOL_Q01_SANTE_V6_M7, KDQOL_Q01_SANTE_V7_M8,
                              KDQOL_Q01_SANTE_V8_M9, KDQOL_Q01_SANTE_V9_M10, KDQOL_Q01_SANTE_V10_M11, KDQOL_Q01_SANTE_V11_M12,
                              KDQOL_Q02_ANNEE_INCL, KDQOL_Q02_ANNEE_V0_M1, 
                              KDQOL_Q02_ANNEE_V1_M2, KDQOL_Q02_ANNEE_V2_M3, KDQOL_Q02_ANNEE_V3_M4, KDQOL_Q02_ANNEE_V4_M5, 
                              KDQOL_Q02_ANNEE_V5_M6, KDQOL_Q02_ANNEE_V6_M7, KDQOL_Q02_ANNEE_V7_M8, KDQOL_Q02_ANNEE_V8_M9,
                              KDQOL_Q02_ANNEE_V9_M10, KDQOL_Q02_ANNEE_V10_M11, KDQOL_Q02_ANNEE_V11_M12,
                              KDQOL_Q03A_IMPORTANT_INCL, KDQOL_Q03A_IMPORTANT_V0_M1, 
                              KDQOL_Q03A_IMPORTANT_V1_M2, KDQOL_Q03A_IMPORTANT_V2_M3, KDQOL_Q03A_IMPORTANT_V3_M4, 
                              KDQOL_Q03A_IMPORTANT_V4_M5, KDQOL_Q03A_IMPORTANT_V6_M7, KDQOL_Q03A_IMPORTANT_V7_M8, KDQOL_Q03A_IMPORTANT_V8_M9,
                              KDQOL_Q03A_IMPORTANT_V9_M10, KDQOL_Q03A_IMPORTANT_V10_M11,KDQOL_Q03A_IMPORTANT_V11_M12,
                              KDQOL_Q03B_MODER_INCL, KDQOL_Q03B_MODER_V0_M1, 
                              KDQOL_Q03B_MODER_V1_M2, KDQOL_Q03B_MODER_V2_M3, KDQOL_Q03B_MODER_V4_M5, KDQOL_Q03B_MODER_V5_M6, 
                              KDQOL_Q03B_MODER_V7_M8, KDQOL_Q03B_MODER_V8_M9, KDQOL_Q03B_MODER_V9_M10, KDQOL_Q03B_MODER_V10_M11,
                              KDQOL_Q03B_MODER_V11_M12,
                              KDQOL_Q03C_COURSE_INCL, KDQOL_Q03C_COURSE_V0_M1, 
                              KDQOL_Q03C_COURSE_V1_M2, KDQOL_Q03C_COURSE_V2_M3, KDQOL_Q03C_COURSE_V3_M4, KDQOL_Q03C_COURSE_V4_M5,
                              KDQOL_Q03C_COURSE_V5_M6, KDQOL_Q03C_COURSE_V6_M7, KDQOL_Q03C_COURSE_V7_M8, 
                              KDQOL_Q03C_COURSE_V8_M9, KDQOL_Q03C_COURSE_V9_M10, KDQOL_Q03C_COURSE_V10_M11, KDQOL_Q03C_COURSE_V11_M12,
                              KDQOL_Q03D_PLSRS_ETAGES_INCL, KDQOL_Q03D_PLSRS_ETAGES_V0_M1, 
                              KDQOL_Q03D_PLSRS_ETAGES_V1_M2, KDQOL_Q03D_PLSRS_ETAGES_V2_M3, KDQOL_Q03D_PLSRS_ETAGES_V3_M4,
                              KDQOL_Q03D_PLSRS_ETAGES_V4_M5, KDQOL_Q03D_PLSRS_ETAGES_V5_M6, KDQOL_Q03D_PLSRS_ETAGES_V6_M7, 
                              KDQOL_Q03D_PLSRS_ETAGES_V7_M8, KDQOL_Q03D_PLSRS_ETAGES_V8_M9, KDQOL_Q03D_PLSRS_ETAGES_V9_M10,
                              KDQOL_Q03D_PLSRS_ETAGES_V10_M11, KDQOL_Q03D_PLSRS_ETAGES_V11_M12,
                              KDQOL_Q03E_ETAGE_INCL, KDQOL_Q03E_ETAGE_V0_M1, 
                              KDQOL_Q03E_ETAGE_V1_M2, KDQOL_Q03E_ETAGE_V2_M3, KDQOL_Q03E_ETAGE_V3_M4, KDQOL_Q03E_ETAGE_V4_M5,
                              KDQOL_Q03E_ETAGE_V5_M6, KDQOL_Q03E_ETAGE_V6_M7, KDQOL_Q03E_ETAGE_V7_M8,
                              KDQOL_Q03E_ETAGE_V8_M9, KDQOL_Q03E_ETAGE_V9_M10, KDQOL_Q03E_ETAGE_V10_M11, KDQOL_Q03E_ETAGE_V11_M12,
                              KDQOL_Q03F_GENOU_INCL, KDQOL_Q03F_GENOU_V0_M1, 
                              KDQOL_Q03F_GENOU_V1_M2, KDQOL_Q03F_GENOU_V2_M3, KDQOL_Q03F_GENOU_V3_M4, KDQOL_Q03F_GENOU_V4_M5,
                              KDQOL_Q03F_GENOU_V5_M6, KDQOL_Q03F_GENOU_V6_M7, KDQOL_Q03F_GENOU_V7_M8,
                              KDQOL_Q03F_GENOU_V8_M9, KDQOL_Q03F_GENOU_V9_M10, KDQOL_Q03F_GENOU_V10_M11, KDQOL_Q03F_GENOU_V11_M12,
                              KDQOL_Q03G_KM_INCL, KDQOL_Q03G_KM_V0_M1, 
                              KDQOL_Q03G_KM_V1_M2, KDQOL_Q03G_KM_V2_M3, KDQOL_Q03G_KM_V3_M4, KDQOL_Q03G_KM_V4_M5,
                              KDQOL_Q03G_KM_V5_M6, KDQOL_Q03G_KM_V6_M7, KDQOL_Q03G_KM_V7_M8, KDQOL_Q03G_KM_V8_M9,
                              KDQOL_Q03G_KM_V9_M10, KDQOL_Q03G_KM_V10_M11, KDQOL_Q03G_KM_V11_M12,
                              KDQOL_Q03H_PLSRS_CENTAINES_INCL, KDQOL_Q03H_PLSRS_CENTAINES_V0_M1, 
                              KDQOL_Q03H_PLSRS_CENTAINES_V1_M2, KDQOL_Q03H_PLSRS_CENTAINES_V2_M3, KDQOL_Q03H_PLSRS_CENTAINES_V3_M4,
                              KDQOL_Q03H_PLSRS_CENTAINES_V4_M5, KDQOL_Q03H_PLSRS_CENTAINES_V5_M6, KDQOL_Q03H_PLSRS_CENTAINES_V6_M7,
                              KDQOL_Q03H_PLSRS_CENTAINES_V7_M8, KDQOL_Q03H_PLSRS_CENTAINES_V8_M9, KDQOL_Q03H_PLSRS_CENTAINES_V9_M10,
                              KDQOL_Q03H_PLSRS_CENTAINES_V10_M11, KDQOL_Q03H_PLSRS_CENTAINES_V11_M12,
                              KDQOL_Q03I_CENTAINE_INCL, KDQOL_Q03I_CENTAINE_V0_M1, 
                              KDQOL_Q03I_CENTAINE_V1_M2, KDQOL_Q03I_CENTAINE_V2_M3, KDQOL_Q03I_CENTAINE_V3_M4, KDQOL_Q03I_CENTAINE_V4_M5,
                              KDQOL_Q03I_CENTAINE_V5_M6, KDQOL_Q03I_CENTAINE_V6_M7, KDQOL_Q03I_CENTAINE_V7_M8, KDQOL_Q03I_CENTAINE_V8_M9,
                              KDQOL_Q03I_CENTAINE_V9_M10, KDQOL_Q03I_CENTAINE_V10_M11, KDQOL_Q03I_CENTAINE_V11_M12,
                              KDQOL_Q03J_DOUCHE_INCL, KDQOL_Q03J_DOUCHE_V0_M1, 
                              KDQOL_Q03J_DOUCHE_V1_M2, KDQOL_Q03J_DOUCHE_V2_M3, KDQOL_Q03J_DOUCHE_V3_M4, KDQOL_Q03J_DOUCHE_V4_M5,
                              KDQOL_Q03J_DOUCHE_V5_M6, KDQOL_Q03J_DOUCHE_V7_M8, KDQOL_Q03J_DOUCHE_V8_M9,
                              KDQOL_Q03J_DOUCHE_V10_M11, KDQOL_Q03J_DOUCHE_V11_M12,
                              KDQOL_Q04A_TRAVAIL_INCL, KDQOL_Q04A_TRAVAIL_V0_M1, 
                              KDQOL_Q04A_TRAVAIL_V1_M2, KDQOL_Q04A_TRAVAIL_V2_M3, KDQOL_Q04A_TRAVAIL_V3_M4, KDQOL_Q04A_TRAVAIL_V4_M5,
                              KDQOL_Q04A_TRAVAIL_V5_M6, KDQOL_Q04A_TRAVAIL_V6_M7, KDQOL_Q04A_TRAVAIL_V7_M8, KDQOL_Q04A_TRAVAIL_V8_M9,
                              KDQOL_Q04A_TRAVAIL_V9_M10, KDQOL_Q04A_TRAVAIL_V10_M11, KDQOL_Q04A_TRAVAIL_V11_M12,
                              KDQOL_Q04B_ACCOMPLI_INCL, KDQOL_Q04B_ACCOMPLI_V0_M1, 
                              KDQOL_Q04B_ACCOMPLI_V1_M2, KDQOL_Q04B_ACCOMPLI_V2_M3, KDQOL_Q04B_ACCOMPLI_V3_M4, KDQOL_Q04B_ACCOMPLI_V4_M5,
                              KDQOL_Q04B_ACCOMPLI_V5_M6, KDQOL_Q04B_ACCOMPLI_V6_M7, KDQOL_Q04B_ACCOMPLI_V7_M8, 
                              KDQOL_Q04B_ACCOMPLI_V9_M10, KDQOL_Q04B_ACCOMPLI_V10_M11, KDQOL_Q04B_ACCOMPLI_V11_M12,
                              KDQOL_Q04C_ARRET_INCL, KDQOL_Q04C_ARRET_V0_M1, 
                              KDQOL_Q04C_ARRET_V1_M2, KDQOL_Q04C_ARRET_V2_M3, KDQOL_Q04C_ARRET_V3_M4, KDQOL_Q04C_ARRET_V4_M5, 
                              KDQOL_Q04C_ARRET_V5_M6, KDQOL_Q04C_ARRET_V6_M7, KDQOL_Q04C_ARRET_V7_M8, 
                              KDQOL_Q04C_ARRET_V8_M9, KDQOL_Q04C_ARRET_V9_M10, KDQOL_Q04C_ARRET_V10_M11, KDQOL_Q04C_ARRET_V11_M12,
                              KDQOL_Q04D_DIFFIC_INCL, KDQOL_Q04D_DIFFIC_V0_M1, 
                              KDQOL_Q04D_DIFFIC_V1_M2, KDQOL_Q04D_DIFFIC_V2_M3, KDQOL_Q04D_DIFFIC_V3_M4, KDQOL_Q04D_DIFFIC_V4_M5, 
                              KDQOL_Q04D_DIFFIC_V5_M6, KDQOL_Q04D_DIFFIC_V6_M7, KDQOL_Q04D_DIFFIC_V7_M8, KDQOL_Q04D_DIFFIC_V8_M9,
                              KDQOL_Q04D_DIFFIC_V9_M10, KDQOL_Q04D_DIFFIC_V10_M11, KDQOL_Q04D_DIFFIC_V11_M12,
                              KDQOL_Q05A_TRAVAIL_INCL, KDQOL_Q05A_TRAVAIL_V0_M1, 
                              KDQOL_Q05A_TRAVAIL_V1_M2, KDQOL_Q05A_TRAVAIL_V2_M3, KDQOL_Q05A_TRAVAIL_V3_M4, KDQOL_Q05A_TRAVAIL_V4_M5,
                              KDQOL_Q05A_TRAVAIL_V5_M6, KDQOL_Q05A_TRAVAIL_V6_M7, KDQOL_Q05A_TRAVAIL_V7_M8, KDQOL_Q05A_TRAVAIL_V8_M9,
                              KDQOL_Q05A_TRAVAIL_V9_M10, KDQOL_Q05A_TRAVAIL_V10_M11, KDQOL_Q05A_TRAVAIL_V11_M12, 
                              KDQOL_Q05B_ACCOMPLI_INCL, KDQOL_Q05B_ACCOMPLI_V0_M1, 
                              KDQOL_Q05B_ACCOMPLI_V1_M2, KDQOL_Q05B_ACCOMPLI_V2_M3, KDQOL_Q05B_ACCOMPLI_V3_M4, KDQOL_Q05B_ACCOMPLI_V4_M5,
                              KDQOL_Q05B_ACCOMPLI_V5_M6, KDQOL_Q05B_ACCOMPLI_V6_M7, KDQOL_Q05B_ACCOMPLI_V7_M8,
                              KDQOL_Q05B_ACCOMPLI_V8_M9, KDQOL_Q05B_ACCOMPLI_V9_M10, KDQOL_Q05B_ACCOMPLI_V10_M11,
                              KDQOL_Q05B_ACCOMPLI_V11_M12,
                              KDQOL_Q05C_SOIN_INCL, KDQOL_Q05C_SOIN_V0_M1, 
                              KDQOL_Q05C_SOIN_V1_M2, KDQOL_Q05C_SOIN_V2_M3, KDQOL_Q05C_SOIN_V3_M4, KDQOL_Q05C_SOIN_V4_M5, 
                              KDQOL_Q05C_SOIN_V5_M6, KDQOL_Q05C_SOIN_V6_M7, KDQOL_Q05C_SOIN_V7_M8, 
                              KDQOL_Q05C_SOIN_V8_M9, KDQOL_Q05C_SOIN_V9_M10, KDQOL_Q05C_SOIN_V10_M11, KDQOL_Q05C_SOIN_V11_M12,
                              KDQOL_Q06_GENE_INCL, KDQOL_Q06_GENE_V0_M1, 
                              KDQOL_Q06_GENE_V1_M2, KDQOL_Q06_GENE_V2_M3, KDQOL_Q06_GENE_V3_M4, KDQOL_Q06_GENE_V4_M5,
                              KDQOL_Q06_GENE_V6_M7, KDQOL_Q06_GENE_V7_M8, KDQOL_Q06_GENE_V8_M9, KDQOL_Q06_GENE_V9_M10,
                              KDQOL_Q06_GENE_V10_M11, KDQOL_Q06_GENE_V11_M12, 
                              KDQOL_Q07_INTENS_INCL, KDQOL_Q07_INTENS_V0_M1, 
                              KDQOL_Q07_INTENS_V1_M2, KDQOL_Q07_INTENS_V2_M3, KDQOL_Q07_INTENS_V3_M4, KDQOL_Q07_INTENS_V4_M5,
                              KDQOL_Q07_INTENS_V5_M6, KDQOL_Q07_INTENS_V7_M8, KDQOL_Q07_INTENS_V8_M9, KDQOL_Q07_INTENS_V9_M10,
                              KDQOL_Q07_INTENS_V10_M11, KDQOL_Q07_INTENS_V11_M12,
                              KDQOL_Q08_LIMITE_INCL, KDQOL_Q08_LIMITE_V0_M1,
                              KDQOL_Q08_LIMITE_V1_M2, KDQOL_Q08_LIMITE_V2_M3, KDQOL_Q08_LIMITE_V3_M4, KDQOL_Q08_LIMITE_V4_M5,
                              KDQOL_Q08_LIMITE_V5_M6, KDQOL_Q08_LIMITE_V6_M7, KDQOL_Q08_LIMITE_V7_M8, KDQOL_Q08_LIMITE_V8_M9,
                              KDQOL_Q08_LIMITE_V9_M10, KDQOL_Q08_LIMITE_V10_M11, KDQOL_Q08_LIMITE_V11_M12,
                              KDQOL_Q09A_DYNAM_INCL, KDQOL_Q09A_DYNAM_V0_M1, 
                              KDQOL_Q09A_DYNAM_V1_M2, KDQOL_Q09A_DYNAM_V2_M3, KDQOL_Q09A_DYNAM_V3_M4, KDQOL_Q09A_DYNAM_V4_M5,
                              KDQOL_Q09A_DYNAM_V5_M6, KDQOL_Q09A_DYNAM_V7_M8, KDQOL_Q09A_DYNAM_V8_M9, KDQOL_Q09A_DYNAM_V9_M10,
                              KDQOL_Q09A_DYNAM_V10_M11, KDQOL_Q09A_DYNAM_V11_M12,
                              KDQOL_Q09B_NERV_INCL, KDQOL_Q09B_NERV_V0_M1, 
                              KDQOL_Q09B_NERV_V1_M2, KDQOL_Q09B_NERV_V2_M3, KDQOL_Q09B_NERV_V3_M4, KDQOL_Q09B_NERV_V4_M5,
                              KDQOL_Q09B_NERV_V5_M6, KDQOL_Q09B_NERV_V6_M7, KDQOL_Q09B_NERV_V7_M8, KDQOL_Q09B_NERV_V8_M9,
                              KDQOL_Q09B_NERV_V9_M10, KDQOL_Q09B_NERV_V10_M11, KDQOL_Q09B_NERV_V11_M12,
                              KDQOL_Q09C_MORAL_INCL, KDQOL_Q09C_MORAL_V0_M1, 
                              KDQOL_Q09C_MORAL_V1_M2, KDQOL_Q09C_MORAL_V2_M3, KDQOL_Q09C_MORAL_V3_M4, KDQOL_Q09C_MORAL_V4_M5,
                              KDQOL_Q09C_MORAL_V5_M6, KDQOL_Q09C_MORAL_V6_M7, KDQOL_Q09C_MORAL_V7_M8, KDQOL_Q09C_MORAL_V8_M9,
                              KDQOL_Q09C_MORAL_V9_M10, KDQOL_Q09C_MORAL_V10_M11, KDQOL_Q09C_MORAL_V11_M12,
                              KDQOL_Q09D_CALME_INCL, KDQOL_Q09D_CALME_V0_M1, 
                              KDQOL_Q09D_CALME_V1_M2, KDQOL_Q09D_CALME_V2_M3, KDQOL_Q09D_CALME_V3_M4, KDQOL_Q09D_CALME_V4_M5,
                              KDQOL_Q09D_CALME_V5_M6, KDQOL_Q09D_CALME_V6_M7, KDQOL_Q09D_CALME_V7_M8, KDQOL_Q09D_CALME_V8_M9,
                              KDQOL_Q09D_CALME_V9_M10, KDQOL_Q09D_CALME_V10_M11, KDQOL_Q09D_CALME_V11_M12,
                              KDQOL_Q09E_ENERGIE_INCL, KDQOL_Q09E_ENERGIE_V0_M1, 
                              KDQOL_Q09E_ENERGIE_V1_M2, KDQOL_Q09E_ENERGIE_V2_M3, KDQOL_Q09E_ENERGIE_V3_M4, KDQOL_Q09E_ENERGIE_V4_M5,
                              KDQOL_Q09E_ENERGIE_V5_M6, KDQOL_Q09E_ENERGIE_V6_M7, KDQOL_Q09E_ENERGIE_V7_M8, KDQOL_Q09E_ENERGIE_V8_M9,
                              KDQOL_Q09E_ENERGIE_V9_M10, KDQOL_Q09E_ENERGIE_V10_M11, KDQOL_Q09E_ENERGIE_V11_M12,
                              KDQOL_Q09F_TRISTE_INCL, KDQOL_Q09F_TRISTE_V0_M1, KDQOL_Q09G_EPUISE_V0_M1, KDQOL_Q09H_HEUR_V0_M1, KDQOL_Q09I_FATIG_V0_M1, 
                              # à vérifier from here
                              KDQOL_Q10_GENE_INCL, KDQOL_Q10_GENE_V0_M1, KDQOL_Q10_GENE_V1_M2, KDQOL_Q10_GENE_V3_M4, KDQOL_Q10_GENE_V4_M5,
                              KDQOL_Q10_GENE_V5_M6, KDQOL_Q10_GENE_V6_M7, KDQOL_Q10_GENE_V7_M8, KDQOL_Q10_GENE_V8_M9, KDQOL_Q10_GENE_V9_M10,
                              KDQOL_Q10_GENE_V10_M11, KDQOL_Q10_GENE_V11_M12,
                              KDQOL_Q11A_MALADE_INCL, KDQOL_Q11A_MALADE_V0_M1, KDQOL_Q11A_MALADE_V1_M2, 
                              KDQOL_Q11A_MALADE_V2_M3, KDQOL_Q11A_MALADE_V3_M4,
                              KDQOL_Q11A_MALADE_V4_M5, KDQOL_Q11A_MALADE_V5_M6, KDQOL_Q11A_MALADE_V6_M7, 
                              KDQOL_Q11A_MALADE_V7_M8, KDQOL_Q11A_MALADE_V8_M9, KDQOL_Q11A_MALADE_V9_M10,
                              KDQOL_Q11A_MALADE_V10_M11, KDQOL_Q11A_MALADE_V11_M12,
                              KDQOL_Q11B_PORTE_BIEN_INCL,
                              KDQOL_Q11B_PORTE_BIEN_V0_M1, KDQOL_Q11B_PORTE_BIEN_V1_M2, KDQOL_Q11B_PORTE_BIEN_V2_M3,
                              KDQOL_Q11B_PORTE_BIEN_V3_M4, KDQOL_Q11B_PORTE_BIEN_V4_M5, KDQOL_Q11B_PORTE_BIEN_V5_M6, 
                              KDQOL_Q11B_PORTE_BIEN_V6_M7, KDQOL_Q11B_PORTE_BIEN_V7_M8, KDQOL_Q11B_PORTE_BIEN_V8_M9,
                              KDQOL_Q11B_PORTE_BIEN_V9_M10, KDQOL_Q11B_PORTE_BIEN_V10_M11, KDQOL_Q11B_PORTE_BIEN_V11_M12,
                              KDQOL_Q11C_DEGRADE_INCL, KDQOL_Q11C_DEGRADE_V0_M1, 
                              KDQOL_Q11C_DEGRADE_V1_M2, KDQOL_Q11C_DEGRADE_V2_M3, KDQOL_Q11C_DEGRADE_V3_M4,
                              KDQOL_Q11C_DEGRADE_V4_M5, KDQOL_Q11C_DEGRADE_V5_M6, KDQOL_Q11C_DEGRADE_V6_M7,
                              KDQOL_Q11C_DEGRADE_V7_M8, KDQOL_Q11C_DEGRADE_V8_M9, KDQOL_Q11C_DEGRADE_V9_M10,
                              KDQOL_Q11C_DEGRADE_V10_M11, KDQOL_Q11C_DEGRADE_V11_M12,
                              KDQOL_Q11D_EXCELL_INCL, KDQOL_Q11D_EXCELL_V0_M1, KDQOL_Q11D_EXCELL_V1_M2, KDQOL_Q11D_EXCELL_V2_M3,
                              KDQOL_Q11D_EXCELL_V4_M5, KDQOL_Q11D_EXCELL_V5_M6, KDQOL_Q11D_EXCELL_V6_M7, KDQOL_Q11D_EXCELL_V8_M9,
                              KDQOL_Q11D_EXCELL_V9_M10, KDQOL_Q11D_EXCELL_V11_M12,
                              KDQOL_Q12A_RENALE_INCL, KDQOL_Q12A_RENALE_V0_M1, 
                              KDQOL_Q12A_RENALE_V2_M3, KDQOL_Q12A_RENALE_V3_M4, KDQOL_Q12A_RENALE_V4_M5, 
                              KDQOL_Q12B_TEMPS_INCL, KDQOL_Q12B_TEMPS_V0_M1, 
                              KDQOL_Q12B_TEMPS_V1_M2, KDQOL_Q12B_TEMPS_V2_M3, 
                              KDQOL_Q12C_SUPPORTE_INCL, KDQOL_Q12C_SUPPORTE_V0_M1, 
                              KDQOL_Q12C_SUPPORTE_V1_M2, KDQOL_Q12C_SUPPORTE_V2_M3, KDQOL_Q12C_SUPPORTE_V3_M4, 
                              KDQOL_Q12C_SUPPORTE_V4_M5, KDQOL_Q12C_SUPPORTE_V5_M6, KDQOL_Q12C_SUPPORTE_V6_M7,
                              KDQOL_Q12C_SUPPORTE_V7_M8, KDQOL_Q12C_SUPPORTE_V8_M9, KDQOL_Q12C_SUPPORTE_V9_M10,
                              KDQOL_Q12C_SUPPORTE_V10_M11, KDQOL_Q12C_SUPPORTE_V11_M12,
                              KDQOL_Q12D_POIDS_INCL, KDQOL_Q12D_POIDS_V0_M1, 
                              KDQOL_Q12D_POIDS_V1_M2, KDQOL_Q12D_POIDS_V2_M3, KDQOL_Q12D_POIDS_V3_M4, KDQOL_Q12D_POIDS_V4_M5,
                              KDQOL_Q12D_POIDS_V5_M6, KDQOL_Q12D_POIDS_V6_M7, KDQOL_Q12D_POIDS_V7_M8,
                              KDQOL_Q12D_POIDS_V8_M9, KDQOL_Q12D_POIDS_V9_M10, KDQOL_Q12D_POIDS_V10_M11,
                              KDQOL_Q12D_POIDS_V11_M12,
                              KDQOL_Q13A_ISOLE_INCL, KDQOL_Q13A_ISOLE_V0_M1, 
                              KDQOL_Q13A_ISOLE_V1_M2, KDQOL_Q13A_ISOLE_V2_M3, KDQOL_Q13A_ISOLE_V3_M4, KDQOL_Q13A_ISOLE_V4_M5,
                              KDQOL_Q13A_ISOLE_V5_M6, KDQOL_Q13A_ISOLE_V6_M7, KDQOL_Q13A_ISOLE_V7_M8, 
                              KDQOL_Q13A_ISOLE_V8_M9, KDQOL_Q13A_ISOLE_V9_M10, KDQOL_Q13A_ISOLE_V10_M11,
                              KDQOL_Q13A_ISOLE_V11_M12,
                              KDQOL_Q13B_REAG_INCL, KDQOL_Q13B_REAG_V0_M1, 
                              KDQOL_Q13B_REAG_V1_M2, KDQOL_Q13B_REAG_V2_M3, KDQOL_Q13B_REAG_V3_M4, KDQOL_Q13B_REAG_V4_M5,
                              KDQOL_Q13B_REAG_V5_M6, KDQOL_Q13B_REAG_V6_M7, KDQOL_Q13B_REAG_V7_M8, KDQOL_Q13B_REAG_V8_M9,
                              KDQOL_Q13B_REAG_V9_M10, KDQOL_Q13B_REAG_V10_M11, KDQOL_Q13B_REAG_V11_M12,
                              KDQOL_Q13C_AGRESS_INCL, KDQOL_Q13C_AGRESS_V0_M1, 
                              KDQOL_Q13C_AGRESS_V1_M2, KDQOL_Q13C_AGRESS_V2_M3, KDQOL_Q13C_AGRESS_V3_M4, 
                              KDQOL_Q13C_AGRESS_V4_M5, KDQOL_Q13C_AGRESS_V5_M6, KDQOL_Q13C_AGRESS_V6_M7,
                              KDQOL_Q13C_AGRESS_V7_M8, KDQOL_Q13C_AGRESS_V8_M9, KDQOL_Q13C_AGRESS_V9_M10,
                              KDQOL_Q13C_AGRESS_V10_M11, KDQOL_Q13C_AGRESS_V11_M12,
                              KDQOL_Q13D_CONCENTR_INCL, KDQOL_Q13D_CONCENTR_V0_M1, 
                              KDQOL_Q13D_CONCENTR_V1_M2, KDQOL_Q13D_CONCENTR_V2_M3, KDQOL_Q13D_CONCENTR_V3_M4,
                              KDQOL_Q13D_CONCENTR_V4_M5, KDQOL_Q13D_CONCENTR_V5_M6, KDQOL_Q13D_CONCENTR_V6_M7,
                              KDQOL_Q13D_CONCENTR_V7_M8, KDQOL_Q13D_CONCENTR_V8_M9, KDQOL_Q13D_CONCENTR_V9_M10,
                              KDQOL_Q13D_CONCENTR_V10_M11, KDQOL_Q13D_CONCENTR_V11_M12,
                              KDQOL_Q13E_ENTENDU_INCL,
                              KDQOL_Q13E_ENTENDU_V0_M1, KDQOL_Q13E_ENTENDU_V1_M2, KDQOL_Q13E_ENTENDU_V2_M3,
                              KDQOL_Q13E_ENTENDU_V3_M4, KDQOL_Q13E_ENTENDU_V4_M5, KDQOL_Q13E_ENTENDU_V5_M6,
                              KDQOL_Q13E_ENTENDU_V6_M7, KDQOL_Q13E_ENTENDU_V7_M8, KDQOL_Q13E_ENTENDU_V8_M9,
                              KDQOL_Q13E_ENTENDU_V9_M10, KDQOL_Q13E_ENTENDU_V10_M11, KDQOL_Q13E_ENTENDU_V11_M12,
                              KDQOL_Q13F_PERTURBE_INCL,
                              KDQOL_Q13F_PERTURBE_V0_M1, KDQOL_Q13F_PERTURBE_V1_M2, KDQOL_Q13F_PERTURBE_V2_M3, 
                              KDQOL_Q13F_PERTURBE_V3_M4, KDQOL_Q13F_PERTURBE_V4_M5, KDQOL_Q13F_PERTURBE_V5_M6,
                              KDQOL_Q13F_PERTURBE_V6_M7, KDQOL_Q13F_PERTURBE_V7_M8, KDQOL_Q13F_PERTURBE_V8_M9,
                              KDQOL_Q13F_PERTURBE_V9_M10, KDQOL_Q13F_PERTURBE_V10_M11, KDQOL_Q13F_PERTURBE_V11_M12,
                              KDQOL_Q14A_COURBAT_INCL, KDQOL_Q14A_COURBAT_V0_M1, 
                              KDQOL_Q14A_COURBAT_V1_M2, KDQOL_Q14A_COURBAT_V2_M3, KDQOL_Q14A_COURBAT_V3_M4,
                              KDQOL_Q14A_COURBAT_V4_M5, KDQOL_Q14A_COURBAT_V5_M6, KDQOL_Q14A_COURBAT_V6_M7,
                              KDQOL_Q14A_COURBAT_V7_M8, KDQOL_Q14A_COURBAT_V8_M9, KDQOL_Q14A_COURBAT_V9_M10,
                              KDQOL_Q14A_COURBAT_V10_M11, KDQOL_Q14A_COURBAT_V11_M12, 
                              KDQOL_Q14B_POITR_INCL, KDQOL_Q14B_POITR_V0_M1,
                              KDQOL_Q14B_POITR_V1_M2, KDQOL_Q14B_POITR_V2_M3, KDQOL_Q14B_POITR_V3_M4, 
                              KDQOL_Q14B_POITR_V4_M5, KDQOL_Q14B_POITR_V5_M6, KDQOL_Q14B_POITR_V6_M7,
                              KDQOL_Q14B_POITR_V7_M8, KDQOL_Q14B_POITR_V8_M9, KDQOL_Q14B_POITR_V9_M10,
                              KDQOL_Q14B_POITR_V10_M11, KDQOL_Q14B_POITR_V11_M12, 
                              KDQOL_Q14C_CRAMPE_INCL, KDQOL_Q14C_CRAMPE_V0_M1, 
                              KDQOL_Q14C_CRAMPE_V1_M2, KDQOL_Q14C_CRAMPE_V2_M3, KDQOL_Q14C_CRAMPE_V3_M4, 
                              KDQOL_Q14C_CRAMPE_V4_M5, KDQOL_Q14C_CRAMPE_V5_M6, KDQOL_Q14C_CRAMPE_V6_M7, 
                              KDQOL_Q14C_CRAMPE_V7_M8, KDQOL_Q14C_CRAMPE_V8_M9, KDQOL_Q14C_CRAMPE_V9_M10, 
                              KDQOL_Q14C_CRAMPE_V10_M11, KDQOL_Q14C_CRAMPE_V11_M12,
                              KDQOL_Q14D_DEMANG_INCL, KDQOL_Q14D_DEMANG_V0_M1, 
                              KDQOL_Q14D_DEMANG_V1_M2, KDQOL_Q14D_DEMANG_V2_M3, KDQOL_Q14D_DEMANG_V3_M4,
                              KDQOL_Q14D_DEMANG_V4_M5, KDQOL_Q14D_DEMANG_V5_M6, KDQOL_Q14D_DEMANG_V6_M7,
                              KDQOL_Q14D_DEMANG_V7_M8, KDQOL_Q14D_DEMANG_V8_M9, KDQOL_Q14D_DEMANG_V9_M10,
                              KDQOL_Q14D_DEMANG_V10_M11, KDQOL_Q14D_DEMANG_V11_M12,
                              KDQOL_Q14E_PEAU_INCL,
                              KDQOL_Q14E_PEAU_V0_M1, KDQOL_Q14E_PEAU_V1_M2, KDQOL_Q14E_PEAU_V2_M3, KDQOL_Q14E_PEAU_V3_M4,
                              KDQOL_Q14E_PEAU_V4_M5, KDQOL_Q14E_PEAU_V5_M6, KDQOL_Q14E_PEAU_V6_M7,
                              KDQOL_Q14E_PEAU_V7_M8, KDQOL_Q14E_PEAU_V8_M9, KDQOL_Q14E_PEAU_V9_M10,
                              KDQOL_Q14E_PEAU_V10_M11, KDQOL_Q14E_PEAU_V11_M12,
                              KDQOL_Q14F_ESSOUFFL_INCL, KDQOL_Q14F_ESSOUFFL_V0_M1, 
                              KDQOL_Q14F_ESSOUFFL_V1_M2, KDQOL_Q14F_ESSOUFFL_V2_M3, KDQOL_Q14F_ESSOUFFL_V3_M4,
                              KDQOL_Q14F_ESSOUFFL_V4_M5, KDQOL_Q14F_ESSOUFFL_V5_M6, KDQOL_Q14F_ESSOUFFL_V6_M7,
                              KDQOL_Q14F_ESSOUFFL_V7_M8, KDQOL_Q14F_ESSOUFFL_V8_M9, KDQOL_Q14F_ESSOUFFL_V10_M11,
                              KDQOL_Q14F_ESSOUFFL_V11_M12,
                              KDQOL_Q14G_VERTIGE_INCL, KDQOL_Q14G_VERTIGE_V0_M1, 
                              KDQOL_Q14G_VERTIGE_V1_M2, KDQOL_Q14G_VERTIGE_V2_M3, KDQOL_Q14G_VERTIGE_V3_M4,
                              KDQOL_Q14G_VERTIGE_V4_M5, KDQOL_Q14G_VERTIGE_V5_M6, KDQOL_Q14G_VERTIGE_V6_M7,
                              KDQOL_Q14G_VERTIGE_V7_M8, KDQOL_Q14G_VERTIGE_V8_M9, KDQOL_Q14G_VERTIGE_V9_M10,
                              KDQOL_Q14G_VERTIGE_V10_M11, KDQOL_Q14G_VERTIGE_V11_M12,
                              KDQOL_Q14H_APPET_INCL, KDQOL_Q14H_APPET_V0_M1, 
                              KDQOL_Q14H_APPET_V1_M2, KDQOL_Q14H_APPET_V2_M3, KDQOL_Q14H_APPET_V3_M4, KDQOL_Q14H_APPET_V4_M5,
                              KDQOL_Q14H_APPET_V5_M6, KDQOL_Q14H_APPET_V6_M7, KDQOL_Q14H_APPET_V7_M8, KDQOL_Q14H_APPET_V8_M9,
                              KDQOL_Q14H_APPET_V9_M10, KDQOL_Q14H_APPET_V10_M11, KDQOL_Q14H_APPET_V11_M12,
                              KDQOL_Q14I_EPUIS_INCL, KDQOL_Q14I_EPUIS_V0_M1, 
                              KDQOL_Q14I_EPUIS_V1_M2, KDQOL_Q14I_EPUIS_V2_M3, KDQOL_Q14I_EPUIS_V3_M4, KDQOL_Q14I_EPUIS_V4_M5,
                              KDQOL_Q14I_EPUIS_V5_M6, KDQOL_Q14I_EPUIS_V6_M7, KDQOL_Q14I_EPUIS_V7_M8, KDQOL_Q14I_EPUIS_V8_M9,
                              KDQOL_Q14I_EPUIS_V9_M10, KDQOL_Q14I_EPUIS_V10_M11, KDQOL_Q14I_EPUIS_V11_M12,
                              KDQOL_Q14J_ENGOURDI_INCL, KDQOL_Q14J_ENGOURDI_V0_M1, 
                              KDQOL_Q14J_ENGOURDI_V1_M2, KDQOL_Q14J_ENGOURDI_V2_M3, KDQOL_Q14J_ENGOURDI_V3_M4, KDQOL_Q14J_ENGOURDI_V4_M5,
                              KDQOL_Q14J_ENGOURDI_V5_M6, KDQOL_Q14J_ENGOURDI_V6_M7, KDQOL_Q14J_ENGOURDI_V7_M8,
                              KDQOL_Q14J_ENGOURDI_V8_M9, KDQOL_Q14J_ENGOURDI_V9_M10, KDQOL_Q14J_ENGOURDI_V10_M11,
                              KDQOL_Q14J_ENGOURDI_V11_M12,
                              KDQOL_Q14K_VOMIR_INCL, KDQOL_Q14K_VOMIR_V0_M1, 
                              KDQOL_Q14K_VOMIR_V1_M2, KDQOL_Q14K_VOMIR_V2_M3, KDQOL_Q14K_VOMIR_V3_M4, KDQOL_Q14K_VOMIR_V4_M5,
                              KDQOL_Q14K_VOMIR_V5_M6, KDQOL_Q14K_VOMIR_V6_M7,KDQOL_Q14K_VOMIR_V7_M8, KDQOL_Q14K_VOMIR_V8_M9,
                              KDQOL_Q14K_VOMIR_V9_M10, KDQOL_Q14K_VOMIR_V10_M11, KDQOL_Q14K_VOMIR_V11_M12,
                              KDQOL_Q14M_FISTULE_INCL, KDQOL_Q14M_FISTULE_V0_M1, 
                              KDQOL_Q14M_FISTULE_V1_M2, KDQOL_Q14M_FISTULE_V2_M3, KDQOL_Q14M_FISTULE_V3_M4, KDQOL_Q14M_FISTULE_V4_M5, 
                              KDQOL_Q14M_FISTULE_V5_M6, KDQOL_Q14M_FISTULE_V6_M7, KDQOL_Q14M_FISTULE_V7_M8, KDQOL_Q14M_FISTULE_V8_M9,
                              KDQOL_Q14M_FISTULE_V9_M10, KDQOL_Q14M_FISTULE_V10_M11,
                              KDQOL_Q15A_BOISSON_INCL, KDQOL_Q15A_BOISSON_V0_M1, 
                              KDQOL_Q15A_BOISSON_V1_M2, KDQOL_Q15A_BOISSON_V2_M3, KDQOL_Q15A_BOISSON_V3_M4, 
                              KDQOL_Q15A_BOISSON_V4_M5, KDQOL_Q15A_BOISSON_V5_M6, KDQOL_Q15A_BOISSON_V6_M7,
                              KDQOL_Q15A_BOISSON_V7_M8, KDQOL_Q15A_BOISSON_V8_M9, KDQOL_Q15A_BOISSON_V10_M11,
                              KDQOL_Q15A_BOISSON_V11_M12, 
                              KDQOL_Q15B_ALIM_INCL, KDQOL_Q15B_ALIM_V0_M1, 
                              KDQOL_Q15B_ALIM_V1_M2, KDQOL_Q15B_ALIM_V1_M3, KDQOL_Q15B_ALIM_V3_M4,
                              KDQOL_Q15B_ALIM_V4_M5, KDQOL_Q15B_ALIM_V5_M6, KDQOL_Q15B_ALIM_V6_M7, 
                              KDQOL_Q15B_ALIM_V7_M8, KDQOL_Q15B_ALIM_V8_M9, KDQOL_Q15B_ALIM_V9_M10,
                              KDQOL_Q15B_ALIM_V10_M11, KDQOL_Q15B_ALIM_V11_M12, 
                              KDQOL_Q15C_MAISON_INCL, KDQOL_Q15C_MAISON_V0_M1, 
                              KDQOL_Q15C_MAISON_V1_M2, KDQOL_Q15C_MAISON_V2_M3, KDQOL_Q15C_MAISON_V3_M4, 
                              KDQOL_Q15C_MAISON_V4_M5, KDQOL_Q15C_MAISON_V5_M6, KDQOL_Q15C_MAISON_V6_M7, 
                              KDQOL_Q15C_MAISON_V7_M8, KDQOL_Q15C_MAISON_V8_M9, KDQOL_Q15C_MAISON_V9_M10,
                              KDQOL_Q15C_MAISON_V10_M11, KDQOL_Q15C_MAISON_V11_M12,
                              KDQOL_Q15D_VOYAGE_INCL, KDQOL_Q15D_VOYAGE_V0_M1, 
                              KDQOL_Q15D_VOYAGE_V1_M2, KDQOL_Q15D_VOYAGE_V2_M3, KDQOL_Q15D_VOYAGE_V3_M4, KDQOL_Q15D_VOYAGE_V4_M5,
                              KDQOL_Q15D_VOYAGE_V5_M6, KDQOL_Q15D_VOYAGE_V6_M7, KDQOL_Q15D_VOYAGE_V7_M8, 
                              KDQOL_Q15D_VOYAGE_V8_M9, KDQOL_Q15D_VOYAGE_V9_M10, KDQOL_Q15D_VOYAGE_V10_M11, 
                              KDQOL_Q15D_VOYAGE_V11_M12,
                              KDQOL_Q15E_MEDECIN_INCL,
                              KDQOL_Q15E_MEDECIN_V0_M1, KDQOL_Q15E_MEDECIN_V1_M2, KDQOL_Q15E_MEDECIN_V2_M3, 
                              KDQOL_Q15E_MEDECIN_V3_M4, KDQOL_Q15E_MEDECIN_V4_M5, KDQOL_Q15E_MEDECIN_V5_M6,
                              KDQOL_Q15E_MEDECIN_V6_M7, KDQOL_Q15E_MEDECIN_V7_M8, KDQOL_Q15E_MEDECIN_V8_M9,
                              KDQOL_Q15E_MEDECIN_V9_M10, KDQOL_Q15E_MEDECIN_V10_M11, KDQOL_Q15E_MEDECIN_V11_M12,
                              KDQOL_Q15F_STRESS_INCL, KDQOL_Q15F_STRESS_V0_M1, 
                              KDQOL_Q15F_STRESS_V1_M2, KDQOL_Q15F_STRESS_V2_M3, KDQOL_Q15F_STRESS_V3_M4, 
                              KDQOL_Q15F_STRESS_V4_M5, KDQOL_Q15F_STRESS_V5_M6, KDQOL_Q15F_STRESS_V6_M7, 
                              KDQOL_Q15F_STRESS_V7_M8, KDQOL_Q15F_STRESS_V8_M9, KDQOL_Q15F_STRESS_V9_M10,
                              KDQOL_Q15F_STRESS_V10_M11, KDQOL_Q15F_STRESS_V11_M12,
                              KDQOL_Q15G_SEX_VIE_INCL, KDQOL_Q15G_SEX_VIE_V0_M1, 
                              KDQOL_Q15G_SEX_VIE_V1_M2, KDQOL_Q15G_SEX_VIE_V2_M3, KDQOL_Q15G_SEX_VIE_V3_M4, 
                              KDQOL_Q15G_SEX_VIE_V4_M5, KDQOL_Q15G_SEX_VIE_V5_M6, KDQOL_Q15G_SEX_VIE_V6_M7,
                              KDQOL_Q15G_SEX_VIE_V7_M8, KDQOL_Q15G_SEX_VIE_V8_M9, KDQOL_Q15G_SEX_VIE_V9_M10,
                              KDQOL_Q15G_SEX_VIE_V10_M11, KDQOL_Q15G_SEX_VIE_V11_M12,
                              KDQOL_Q15H_APPAR_INCL,
                              KDQOL_Q15H_APPAR_V0_M1, KDQOL_Q15H_APPAR_V1_M2, KDQOL_Q15H_APPAR_V2_M3, 
                              KDQOL_Q15H_APPAR_V3_M4, KDQOL_Q15H_APPAR_V4_M5, KDQOL_Q15H_APPAR_V5_M6, 
                              KDQOL_Q15H_APPAR_V6_M7, KDQOL_Q15H_APPAR_V7_M8, KDQOL_Q15H_APPAR_V8_M9,
                              KDQOL_Q15H_APPAR_V9_M10, KDQOL_Q15H_APPAR_V10_M11, KDQOL_Q15H_APPAR_V11_M12,
                              KDQOL_Q16A_SEX_PLAIS_INCL, 
                              KDQOL_Q16A_SEX_PLAIS_V0_M1, KDQOL_Q16A_SEX_PLAIS_V1_M2, KDQOL_Q16A_SEX_PLAIS_V2_M3, 
                              KDQOL_Q16A_SEX_PLAIS_V4_M5, KDQOL_Q16A_SEX_PLAIS_V5_M6, KDQOL_Q16A_SEX_PLAIS_V6_M7, 
                              KDQOL_Q16A_SEX_PLAIS_V7_M8, KDQOL_Q16A_SEX_PLAIS_V8_M9, KDQOL_Q16A_SEX_PLAIS_V9_M10,
                              KDQOL_Q16A_SEX_PLAIS_V10_M11, KDQOL_Q16A_SEX_PLAIS_V11_M12,
                              KDQOL_Q16B_SEX_ABS_INCL, KDQOL_Q16B_SEX_ABS_V0_M1, 
                              KDQOL_Q16B_SEX_ABS_V1_M2, KDQOL_Q16B_SEX_ABS_V2_M3, KDQOL_Q16B_SEX_ABS_V3_M4,
                              KDQOL_Q16B_SEX_ABS_V4_M5, KDQOL_Q16B_SEX_ABS_V5_M6, KDQOL_Q16B_SEX_ABS_V6_M7,
                              KDQOL_Q16B_SEX_ABS_V7_M8, KDQOL_Q16B_SEX_ABS_V8_M9, KDQOL_Q16B_SEX_ABS_V9_M10,
                              KDQOL_Q16B_SEX_ABS_V10_M11, KDQOL_Q16B_SEX_ABS_V11_M12,
                              KDQOL_Q17_SOMMEIL_INCL, KDQOL_Q17_SOMMEIL_V0_M1,
                              KDQOL_Q17_SOMMEIL_V1_M2, KDQOL_Q17_SOMMEIL_V2_M3, KDQOL_Q17_SOMMEIL_V3_M4,
                              KDQOL_Q17_SOMMEIL_V4_M5, KDQOL_Q17_SOMMEIL_V5_M6, KDQOL_Q17_SOMMEIL_V6_M7,
                              KDQOL_Q17_SOMMEIL_V7_M8, KDQOL_Q17_SOMMEIL_V8_M9,
                              KDQOL_Q17_SOMMEIL_V9_M10, KDQOL_Q17_SOMMEIL_V10_M11, KDQOL_Q17_SOMMEIL_V11_M12,
                              KDQOL_Q18A_REVEIL_INCL, KDQOL_Q18A_REVEIL_V0_M1, 
                              KDQOL_Q18A_REVEIL_V1_M2, KDQOL_Q18A_REVEIL_V2_M3, KDQOL_Q18A_REVEIL_V3_M4,
                              KDQOL_Q18A_REVEIL_V4_M5, KDQOL_Q18A_REVEIL_V5_M6, KDQOL_Q18A_REVEIL_V6_M7,
                              KDQOL_Q18A_REVEIL_V7_M8, KDQOL_Q18A_REVEIL_V8_M9, KDQOL_Q18A_REVEIL_V10_M11,
                              KDQOL_Q18A_REVEIL_V11_M12, 
                              KDQOL_Q18B_SUFFIS_V0_M1, 
                              KDQOL_Q18C_SOMNOLE_INCL, KDQOL_Q18C_SOMNOLE_V0_M1, 
                              KDQOL_Q18C_SOMNOLE_V1_M2, KDQOL_Q18C_SOMNOLE_V2_M3, KDQOL_Q18C_SOMNOLE_V3_M4,
                              KDQOL_Q18C_SOMNOLE_V4_M5, KDQOL_Q18C_SOMNOLE_V5_M6, KDQOL_Q18C_SOMNOLE_V6_M7,
                              KDQOL_Q18C_SOMNOLE_V7_M8, KDQOL_Q18C_SOMNOLE_V8_M9, KDQOL_Q18C_SOMNOLE_V9_M10, 
                              KDQOL_Q18C_SOMNOLE_V10_M11, KDQOL_Q18C_SOMNOLE_V11_M12, 
                              KDQOL_Q19A_TEMPS_INCL, 
                              KDQOL_Q19A_TEMPS_V0_M1, KDQOL_Q19A_TEMPS_V1_M2, KDQOL_Q19A_TEMPS_V2_M3, 
                              KDQOL_Q19A_TEMPS_V3_M4, KDQOL_Q19A_TEMPS_V4_M5, KDQOL_Q19A_TEMPS_V5_M6, KDQOL_Q19A_TEMPS_V6_M7,
                              KDQOL_Q19A_TEMPS_V7_M8, KDQOL_Q19A_TEMPS_V8_M9, KDQOL_Q19A_TEMPS_V10_M11, KDQOL_Q19A_TEMPS_V11_M12,
                              KDQOL_Q19B_SOUTIEN_INCL, KDQOL_Q19B_SOUTIEN_V0_M1, 
                              KDQOL_Q19B_SOUTIEN_V1_M2, KDQOL_Q19B_SOUTIEN_V2_M3, KDQOL_Q19B_SOUTIEN_V3_M4,
                              KDQOL_Q19B_SOUTIEN_V4_M5, KDQOL_Q19B_SOUTIEN_V5_M6, KDQOL_Q19B_SOUTIEN_V6_M7, 
                              KDQOL_Q19B_SOUTIEN_V7_M8, KDQOL_Q19B_SOUTIEN_V8_M9, KDQOL_Q19B_SOUTIEN_V9_M10,
                              KDQOL_Q19B_SOUTIEN_V10_M11, KDQOL_Q19B_SOUTIEN_V11_M12, 
                              KDQOL_Q20_REMUNERE_INCL, KDQOL_Q20_REMUNERE_V0_M1, 
                              KDQOL_Q20_REMUNERE_V1_M2, KDQOL_Q20_REMUNERE_V2_M3, KDQOL_Q20_REMUNERE_V3_M4,
                              KDQOL_Q20_REMUNERE_V4_M5, KDQOL_Q20_REMUNERE_V5_M6, KDQOL_Q20_REMUNERE_V6_M7,
                              KDQOL_Q20_REMUNERE_V7_M8, KDQOL_Q20_REMUNERE_V8_M9, KDQOL_Q20_REMUNERE_V9_M10, 
                              KDQOL_Q20_REMUNERE_V10_M11, KDQOL_Q20_REMUNERE_V11_M12,
                              KDQOL_Q21_EMPECHE_INCL, KDQOL_Q21_EMPECHE_V0_M1, 
                              KDQOL_Q21_EMPECHE_V1_M2, KDQOL_Q21_EMPECHE_V2_M3, KDQOL_Q21_EMPECHE_V3_M4, 
                              KDQOL_Q21_EMPECHE_V4_M5, KDQOL_Q21_EMPECHE_V5_M6, KDQOL_Q21_EMPECHE_V6_M7,
                              KDQOL_Q21_EMPECHE_V7_M8, KDQOL_Q21_EMPECHE_V8_M9, KDQOL_Q21_EMPECHE_V9_M10,
                              KDQOL_Q21_EMPECHE_V10_M11, KDQOL_Q21_EMPECHE_V11_M12,
                              KDQOL_Q22_SANTE_GLOBAL_INCL, KDQOL_Q22_SANTE_GLOBAL_V0_M1,
                              KDQOL_Q22_SANTE_GLOBAL_V1_M2, KDQOL_Q22_SANTE_GLOBAL_V2_M3, KDQOL_Q22_SANTE_GLOBAL_V3_M4,
                              KDQOL_Q22_SANTE_GLOBAL_V4_M5, KDQOL_Q22_SANTE_GLOBAL_V5_M6, KDQOL_Q22_SANTE_GLOBAL_V6_M7,
                              KDQOL_Q22_SANTE_GLOBAL_V7_M8, KDQOL_Q22_SANTE_GLOBAL_V8_M9, KDQOL_Q22_SANTE_GLOBAL_V10_M11,
                              KDQOL_Q22_SANTE_GLOBAL_V11_M12
                              ))

# var transformation

did$KDQOL_Q05C_SOIN_V0_M1 = as.numeric(as.character(did$KDQOL_Q05C_SOIN_V0_M1))
did$KDQOL_Q05C_SOIN_V0_M1 = as.numeric(as.character(did$KDQOL_Q05C_SOIN_V0_M1))
did$KDQOL_Q11A_MALADE_V0_M1 = as.numeric(as.character(did$KDQOL_Q11A_MALADE_V0_M1))

# didité de vie M1------

did$KDQOL_Q03_SUM = rowSums(did[,c("KDQOL_Q03A_IMPORTANT_V0_M1", 
                                "KDQOL_Q03B_MODER_V0_M1", "KDQOL_Q03C_COURSE_V0_M1", "KDQOL_Q03D_PLSRS_ETAGES_V0_M1", "KDQOL_Q03E_ETAGE_V0_M1", 
                                "KDQOL_Q03F_GENOU_V0_M1", "KDQOL_Q03G_KM_V0_M1", "KDQOL_Q03H_PLSRS_CENTAINES_V0_M1", "KDQOL_Q03I_CENTAINE_V0_M1",
                                "KDQOL_Q03J_DOUCHE_V0_M1")], na.rm = TRUE)

did$KDQOL_Q04_SUM = rowSums(did[,c("KDQOL_Q04A_TRAVAIL_V0_M1", "KDQOL_Q04B_ACCOMPLI_V0_M1", "KDQOL_Q04C_ARRET_V0_M1",
                                "KDQOL_Q04D_DIFFIC_V0_M1")], na.rm = TRUE)

did$KDQOL_Q05C_SOIN_V0_M1 = as.numeric(as.character(did$KDQOL_Q05C_SOIN_V0_M1))
did$KDQOL_Q05_SUM = rowSums(did[,c("KDQOL_Q05A_TRAVAIL_V0_M1", "KDQOL_Q05B_ACCOMPLI_V0_M1", 
                                "KDQOL_Q05C_SOIN_V0_M1")], na.rm = TRUE)

did$KDQOL_Q09_SUM = rowSums(did[,c("KDQOL_Q09A_DYNAM_V0_M1",
                                "KDQOL_Q09B_NERV_V0_M1", "KDQOL_Q09C_MORAL_V0_M1", "KDQOL_Q09D_CALME_V0_M1", "KDQOL_Q09E_ENERGIE_V0_M1", 
                                "KDQOL_Q09F_TRISTE_V0_M1", "KDQOL_Q09G_EPUISE_V0_M1", 
                                "KDQOL_Q09H_HEUR_V0_M1", "KDQOL_Q09I_FATIG_V0_M1")], na.rm = TRUE)

did$KDQOL_Q11A_MALADE_V0_M1 = as.numeric(as.character(did$KDQOL_Q11A_MALADE_V0_M1))
did$KDQOL_Q11_SUM = rowSums(did[,c("KDQOL_Q11A_MALADE_V0_M1", "KDQOL_Q11B_PORTE_BIEN_V0_M1", 
                                     "KDQOL_Q11C_DEGRADE_V0_M1", 
                                "KDQOL_Q11D_EXCELL_V0_M1")], na.rm = TRUE)

did$KDQOL_Q12_SUM = rowSums(did[, c("KDQOL_Q12A_RENALE_V0_M1", 
                                 "KDQOL_Q12B_TEMPS_V0_M1", "KDQOL_Q12C_SUPPORTE_V0_M1", 
                                 "KDQOL_Q12D_POIDS_V0_M1")], na.rm = TRUE)

did$KDQOL_Q13_SUM = rowSums(did[,c("KDQOL_Q13A_ISOLE_V0_M1", "KDQOL_Q13B_REAG_V0_M1", "KDQOL_Q13C_AGRESS_V0_M1", 
                                "KDQOL_Q13D_CONCENTR_V0_M1", "KDQOL_Q13E_ENTENDU_V0_M1", 
                                "KDQOL_Q13F_PERTURBE_V0_M1")], na.rm = TRUE)

did$KDQOL_Q14_SUM = rowSums(did[, c("KDQOL_Q14A_COURBAT_V0_M1", 
                                 "KDQOL_Q14B_POITR_V0_M1", "KDQOL_Q14C_CRAMPE_V0_M1", "KDQOL_Q14D_DEMANG_V0_M1", "KDQOL_Q14E_PEAU_V0_M1", 
                                 "KDQOL_Q14F_ESSOUFFL_V0_M1", "KDQOL_Q14G_VERTIGE_V0_M1", "KDQOL_Q14H_APPET_V0_M1", "KDQOL_Q14I_EPUIS_V0_M1", 
                                 "KDQOL_Q14J_ENGOURDI_V0_M1", "KDQOL_Q14K_VOMIR_V0_M1", 
                                 "KDQOL_Q14M_FISTULE_V0_M1")], na.rm = TRUE)

did$KDQOL_Q15_SUM = rowSums(did[,c("KDQOL_Q15A_BOISSON_V0_M1", 
                                "KDQOL_Q15B_ALIM_V0_M1", "KDQOL_Q15C_MAISON_V0_M1", "KDQOL_Q15D_VOYAGE_V0_M1", "KDQOL_Q15E_MEDECIN_V0_M1", 
                                "KDQOL_Q15F_STRESS_V0_M1", "KDQOL_Q15G_SEX_VIE_V0_M1", 
                                "KDQOL_Q15H_APPAR_V0_M1")], na.rm = TRUE)

did$KDQOL_Q16_SUM = rowSums(did[,c("KDQOL_Q16A_SEX_PLAIS_V0_M1", 
                                "KDQOL_Q16B_SEX_ABS_V0_M1")])

did$KDQOL_Q18_SUM = rowSums(did[,c("KDQOL_Q18A_REVEIL_V0_M1", "KDQOL_Q18B_SUFFIS_V0_M1", 
                                "KDQOL_Q18C_SOMNOLE_V0_M1")])

did$KDQOL_Q19_SUM = rowSums(did[,c("KDQOL_Q19A_TEMPS_V0_M1", "KDQOL_Q19B_SOUTIEN_V0_M1")])

# tableone for didity of life 

qolvars = c("KDQOL_Q01_SANTE_V0_M1", "KDQOL_Q02_ANNEE_V0_M1", "KDQOL_Q03_SUM", "KDQOL_Q04_SUM",
            "KDQOL_Q05_SUM", "KDQOL_Q06_GENE_V0_M1", "KDQOL_Q07_INTENS_V0_M1", "KDQOL_Q08_LIMITE_V0_M1",  
            "KDQOL_Q09_SUM", "KDQOL_Q10_GENE_V0_M1", "KDQOL_Q11_SUM", "KDQOL_Q12_SUM", "KDQOL_Q13_SUM",
            "KDQOL_Q14_SUM", "KDQOL_Q15_SUM", "KDQOL_Q16_SUM", "KDQOL_Q17_SOMMEIL_V0_M1",
            "KDQOL_Q18_SUM", "KDQOL_Q19_SUM", "KDQOL_Q20_REMUNERE_V0_M1", 
            "KDQOL_Q21_EMPECHE_V0_M1", "KDQOL_Q22_SANTE_GLOBAL_V0_M1")

kdqol_tone <- CreateTableOne(vars = qolvars, data = did, factorVars = , 
                       includeNA = TRUE)
print(kdqol_tone, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(kdqol_tone)


# qualité de vie M12------

did$KDQOL_Q03_SUM11 = rowSums(did[,c("KDQOL_Q03A_IMPORTANT_V11_M12", 
                                     "KDQOL_Q03B_MODER_V11_M12", "KDQOL_Q03C_COURSE_V11_M12", "KDQOL_Q03D_PLSRS_ETAGES_V11_M12", "KDQOL_Q03E_ETAGE_V11_M12", 
                                     "KDQOL_Q03F_GENOU_V11_M12", "KDQOL_Q03G_KM_V11_M12", "KDQOL_Q03H_PLSRS_CENTAINES_V11_M12", "KDQOL_Q03I_CENTAINE_V11_M12",
                                     "KDQOL_Q03J_DOUCHE_V11_M12")], na.rm = TRUE)

did$KDQOL_Q04_SUM11 = rowSums(did[,c("KDQOL_Q04A_TRAVAIL_V11_M12", "KDQOL_Q04B_ACCOMPLI_V11_M12", "KDQOL_Q04C_ARRET_V11_M12",
                                     "KDQOL_Q04D_DIFFIC_V11_M12")], na.rm = TRUE)

did$KDQOL_Q05C_SOIN_V11_M12 = as.numeric(as.character(did$KDQOL_Q05C_SOIN_V11_M12))
did$KDQOL_Q05_SUM11 = rowSums(did[,c("KDQOL_Q05A_TRAVAIL_V11_M12", "KDQOL_Q05B_ACCOMPLI_V11_M12", 
                                     "KDQOL_Q05C_SOIN_V11_M12")], na.rm = TRUE)

did$KDQOL_Q09_SUM11 = rowSums(did[,c("KDQOL_Q09A_DYNAM_V11_M12",
                                     "KDQOL_Q09B_NERV_V11_M12", "KDQOL_Q09C_MORAL_V11_M12", "KDQOL_Q09D_CALME_V11_M12", "KDQOL_Q09E_ENERGIE_V11_M12", 
                                     "KDQOL_Q09F_TRISTE_V11_M12", "KDQOL_Q09G_EPUISE_V11_M12", 
                                     "KDQOL_Q09H_HEUR_V11_M12", "KDQOL_Q09I_FATIG_V11_M12")], na.rm = TRUE)

did$KDQOL_Q11A_MALADE_V11_M12 = as.numeric(as.character(did$KDQOL_Q11A_MALADE_V11_M12))
did$KDQOL_Q11_SUM11 = rowSums(did[,c("KDQOL_Q11A_MALADE_V11_M12", "KDQOL_Q11B_PORTE_BIEN_V11_M12", 
                                     "KDQOL_Q11C_DEGRADE_V11_M12", 
                                     "KDQOL_Q11D_EXCELL_V11_M12")], na.rm = TRUE)

did$KDQOL_Q12_SUM11 = rowSums(did[, c("KDQOL_Q12A_RENALE_V11_M12", 
                                      "KDQOL_Q12B_TEMPS_V11_M12", "KDQOL_Q12C_SUPPORTE_V11_M12", 
                                      "KDQOL_Q12D_POIDS_V11_M12")], na.rm = TRUE)

did$KDQOL_Q13_SUM11 = rowSums(did[,c("KDQOL_Q13A_ISOLE_V11_M12", "KDQOL_Q13B_REAG_V11_M12", "KDQOL_Q13C_AGRESS_V11_M12", 
                                     "KDQOL_Q13D_CONCENTR_V11_M12", "KDQOL_Q13E_ENTENDU_V11_M12", 
                                     "KDQOL_Q13F_PERTURBE_V11_M12")], na.rm = TRUE)

did$KDQOL_Q14_SUM11 = rowSums(did[, c("KDQOL_Q14A_COURBAT_V11_M12", 
                                      "KDQOL_Q14B_POITR_V11_M12", "KDQOL_Q14C_CRAMPE_V11_M12", "KDQOL_Q14D_DEMANG_V11_M12", "KDQOL_Q14E_PEAU_V11_M12", 
                                      "KDQOL_Q14F_ESSOUFFL_V11_M12", "KDQOL_Q14G_VERTIGE_V11_M12", "KDQOL_Q14H_APPET_V11_M12", "KDQOL_Q14I_EPUIS_V11_M12", 
                                      "KDQOL_Q14J_ENGOURDI_V11_M12", "KDQOL_Q14K_VOMIR_V11_M12", 
                                      "KDQOL_Q14M_FISTULE_V11_M12")], na.rm = TRUE)

did$KDQOL_Q15_SUM11 = rowSums(did[,c("KDQOL_Q15A_BOISSON_V11_M12", 
                                     "KDQOL_Q15B_ALIM_V11_M12", "KDQOL_Q15C_MAISON_V11_M12", "KDQOL_Q15D_VOYAGE_V11_M12", "KDQOL_Q15E_MEDECIN_V11_M12", 
                                     "KDQOL_Q15F_STRESS_V11_M12", "KDQOL_Q15G_SEX_VIE_V11_M12", 
                                     "KDQOL_Q15H_APPAR_V11_M12")], na.rm = TRUE)

did$KDQOL_Q16A_SEX_PLAIS_V11_M12 = as.numeric(as.character(did$KDQOL_Q16A_SEX_PLAIS_V11_M12))
did$KDQOL_Q16B_SEX_ABS_V11_M12 = as.numeric(as.character(did$KDQOL_Q16B_SEX_ABS_V11_M12))
did$KDQOL_Q16_SUM11 = rowSums(did[,c("KDQOL_Q16A_SEX_PLAIS_V11_M12", 
                                     "KDQOL_Q16B_SEX_ABS_V11_M12")])

did$KDQOL_Q18_SUM11 = rowSums(did[,c("KDQOL_Q18A_REVEIL_V11_M12", "KDQOL_Q18B_SUFFIS_V11_M12", 
                                     "KDQOL_Q18C_SOMNOLE_V11_M12")])

did$KDQOL_Q19_SUM11 = rowSums(did[,c("KDQOL_Q19A_TEMPS_V11_M12", "KDQOL_Q19B_SOUTIEN_V11_M12")])

# tableone for didity of life 

qolvars11 = c("KDQOL_Q01_SANTE_V11_M12", "KDQOL_Q02_ANNEE_V11_M12", "KDQOL_Q03_SUM11", "KDQOL_Q04_SUM11",
            "KDQOL_Q05_SUM11", "KDQOL_Q06_GENE_V11_M12", "KDQOL_Q07_INTENS_V11_M12", "KDQOL_Q08_LIMITE_V11_M12",  
            "KDQOL_Q09_SUM11", "KDQOL_Q10_GENE_V11_M12", "KDQOL_Q11_SUM11", "KDQOL_Q12_SUM11", "KDQOL_Q13_SUM11",
            "KDQOL_Q14_SUM11", "KDQOL_Q15_SUM11", "KDQOL_Q16_SUM11", "KDQOL_Q17_SOMMEIL_V11_M12",
            "KDQOL_Q18_SUM11", "KDQOL_Q19_SUM11", "KDQOL_Q20_REMUNERE_V11_M12", 
            "KDQOL_Q21_EMPECHE_V11_M12", "KDQOL_Q22_SANTE_GLOBAL_V11_M12")

kdqol_tone11 <- CreateTableOne(vars = qolvars11, data = did, factorVars = , 
                             includeNA = TRUE)
print(kdqol_tone11, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(kdqol_tone11)

#-qual difference-----

did$Q1 = did$KDQOL_Q01_SANTE_V11_M12 - did$KDQOL_Q01_SANTE_V0_M1
did$Q2 = did$KDQOL_Q02_ANNEE_V11_M12 - did$KDQOL_Q02_ANNEE_V0_M1
did$Q3 = did$KDQOL_Q03_SUM11 - did$KDQOL_Q03_SUM
did$Q4 = did$KDQOL_Q04_SUM11 - did$KDQOL_Q04_SUM
did$Q5 = did$KDQOL_Q05_SUM11 - did$KDQOL_Q05_SUM
did$Q6 = did$KDQOL_Q06_GENE_V11_M12 - did$KDQOL_Q06_GENE_V0_M1
did$Q7 = did$KDQOL_Q07_INTENS_V11_M12 - did$KDQOL_Q07_INTENS_V0_M1
did$Q8 = did$KDQOL_Q08_LIMITE_V11_M12 - did$KDQOL_Q08_LIMITE_V0_M1
did$Q9 = did$KDQOL_Q09_SUM11 - did$KDQOL_Q09_SUM 
did$Q10 = did$KDQOL_Q10_GENE_V11_M12 - did$KDQOL_Q10_GENE_V0_M1
did$Q11 = did$KDQOL_Q11_SUM11 - did$KDQOL_Q11_SUM
did$Q12 = did$KDQOL_Q12_SUM11 - did$KDQOL_Q12_SUM
did$Q13 = did$KDQOL_Q13_SUM11 - did$KDQOL_Q13_SUM
did$Q14 = did$KDQOL_Q14_SUM11 - did$KDQOL_Q14_SUM
did$Q15 = did$KDQOL_Q15_SUM11 - did$KDQOL_Q15_SUM
did$Q16 = did$KDQOL_Q16_SUM11 - did$KDQOL_Q16_SUM 
did$Q17 = did$KDQOL_Q17_SOMMEIL_V11_M12 - did$KDQOL_Q17_SOMMEIL_V0_M1
did$Q18 = did$KDQOL_Q18_SUM11 - did$KDQOL_Q18_SUM
did$Q19 = did$KDQOL_Q19_SUM11 - did$KDQOL_Q19_SUM
did$Q20 = did$KDQOL_Q20_REMUNERE_V11_M12 - did$KDQOL_Q20_REMUNERE_V0_M1 
did$Q21 = did$KDQOL_Q21_EMPECHE_V11_M12 - did$KDQOL_Q21_EMPECHE_V0_M1
did$Q22 = did$KDQOL_Q22_SANTE_GLOBAL_V11_M12 - did$KDQOL_Q22_SANTE_GLOBAL_V0_M1

qoldiff = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8",
            "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17",
            "Q18", "Q19", "Q20", "Q21", "Q22")

kdqol_diff <- CreateTableOne(vars = qoldiff, data = did, factorVars = , 
                               includeNA = TRUE)
print(kdqol_diff, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(kdqol_diff)

# score 2 qualité de vie 

did$EQV0M1 = rowSums(did[,c("EQ_Q01_MOBIL_V0_M1","EQ_Q02_AUTON_V0_M1","EQ_Q03_ACT_V0_M1",
                        "EQ_Q04_DOUL_V0_M1","EQ_Q05_ANX_V0_M1")], na.rm=TRUE)

did$EQV1M2 = rowSums(did[,c("EQ_Q01_MOBIL_V1_M2","EQ_Q02_AUTON_V1_M2","EQ_Q03_ACT_V1_M2",
                        "EQ_Q04_DOUL_V1_M2","EQ_Q05_ANX_V1_M2")], na.rm=TRUE)

did$EQV2M3 = rowSums(did[,c("EQ_Q01_MOBIL_V2_M3","EQ_Q02_AUTON_V2_M3","EQ_Q03_ACT_V2_M3",
                        "EQ_Q04_DOUL_V2_M3","EQ_Q05_ANX_V2_M3")], na.rm=TRUE)

did$EQV3M4 = rowSums(did[,c("EQ_Q01_MOBIL_V3_M4","EQ_Q02_AUTON_V3_M4","EQ_Q03_ACT_V3_M4",
                        "EQ_Q04_DOUL_V3_M4","EQ_Q05_ANX_V3_M4")], na.rm=TRUE)

did$EQV4M5 = rowSums(did[,c("EQ_Q01_MOBIL_V4_M5","EQ_Q02_AUTON_V4_M5","EQ_Q03_ACT_V4_M5",
                        "EQ_Q04_DOUL_V4_M5","EQ_Q05_ANX_V4_M5")], na.rm=TRUE)

did$EQV5M6 = rowSums(did[,c("EQ_Q01_MOBIL_V5_M6","EQ_Q02_AUTON_V5_M6","EQ_Q03_ACT_V5_M6",
                        "EQ_Q04_DOUL_V5_M6","EQ_Q05_ANX_V5_M6")], na.rm=TRUE)

did$EQV6M7 = rowSums(did[,c("EQ_Q01_MOBIL_V6_M7","EQ_Q02_AUTON_V6_M7","EQ_Q03_ACT_V6_M7",
                        "EQ_Q04_DOUL_V6_M7","EQ_Q05_ANX_V6_M7")], na.rm=TRUE)

did$EQV7M8 = rowSums(did[,c("EQ_Q01_MOBIL_V7_M8","EQ_Q02_AUTON_V7_M8","EQ_Q03_ACT_V7_M8",
                        "EQ_Q04_DOUL_V7_M8","EQ_Q05_ANX_V7_M8")], na.rm=TRUE)

did$EQV8M9 = rowSums(did[,c("EQ_Q01_MOBIL_V8_M9","EQ_Q02_AUTON_V8_M9","EQ_Q03_ACT_V8_M9",
                            "EQ_Q04_DOUL_V8_M9","EQ_Q05_ANX_V8_M9")], na.rm=TRUE)

did$EQV9M10 = rowSums(did[,c("EQ_Q01_MOBIL_V9_M10","EQ_Q02_AUTON_V9_M10","EQ_Q03_ACT_V9_M10",
                            "EQ_Q04_DOUL_V9_M10","EQ_Q05_ANX_V9_M10")], na.rm=TRUE)

did$EQV10M11 = rowSums(did[,c("EQ_Q01_MOBIL_V10_M11","EQ_Q02_AUTON_V10_M11","EQ_Q03_ACT_V10_M11",
                             "EQ_Q04_DOUL_V10_M11","EQ_Q05_ANX_V10_M11")], na.rm=TRUE)

did$EQV11M12 = rowSums(did[,c("EQ_Q01_MOBIL_V11_M12","EQ_Q02_AUTON_V11_M12","EQ_Q03_ACT_V11_M12",
                              "EQ_Q04_DOUL_V11_M12","EQ_Q05_ANX_V11_M12")], na.rm=TRUE)


eqvars = c("EQV0M1", "EQV1M2", "EQV2M3", "EQV3M4", "EQV4M5", "EQV5M6", "EQV6M7", "EQV7M8",
           "EQV8M9", "EQV9M10", "EQV10M11", "EQV11M12")

eqtone <- CreateTableOne(vars = eqvars, data = did, factorVars = , 
                             includeNA = TRUE)
print(eqtone, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(eqtone)


#-BIOLOGIE EN MOYENNE TOTALE-----
#-12 visites 

did$poids = rowSums(did[,c("EXAM_POIDS_V0_M1", "EXAM_POIDS_V1_M2", "EXAM_POIDS_V2_M3", "EXAM_POIDS_V3_M4", "EXAM_POIDS_V4_M5",
                           "EXAM_POIDS_V5_M6", "EXAM_POIDS_V6_M7", "EXAM_POIDS_V7_M8", "EXAM_POIDS_V8_M9", "EXAM_POIDS_V9_M10",
                           "EXAM_POIDS_V10_M11", "EXAM_POIDS_V11_M12")], na.rm = TRUE)

did$PAS = rowSums(did[, c("EXAM_PAS_V0_M1", "EXAM_PAS_V1_M2", "EXAM_PAS_V2_M3", "EXAM_PAS_V3_M4", 
                          "EXAM_PAS_V4_M5", "EXAM_PAS_V5_M6",
                          "EXAM_PAS_V6_M7", "EXAM_PAS_V7_M8", "EXAM_PAS_V8_M9", "EXAM_PAS_V9_M10", 
                          "EXAM_PAS_V10_M11", "EXAM_PAS_V11_M12")], na.rm = TRUE)

did$PAD = rowSums(did[,c("EXAM_PAD_V0_M1",
                        "EXAM_PAD_V1_M2",
                        "EXAM_PAD_V2_M3",
                        "EXAM_PAD_V3_M4",
                        "EXAM_PAD_V4_M5",
                        "EXAM_PAD_V5_M6",
                        "EXAM_PAD_V6_M7",
                        "EXAM_PAD_V7_M8",
                        "EXAM_PAD_V8_M9",
                        "EXAM_PAD_V9_M10",
                        "EXAM_PAD_V10_M11",
                        "EXAM_PAD_V11_M12")], na.rm = TRUE)

did$PREF = rowSums(did[,c("BIO_PRE_F_INCL",
                           "BIO_PRE_F_V2_M3",
                           "BIO_PRE_F_V5_M6",
                           "BIO_PRE_F_V8_M9",
                           "BIO_PRE_F_V11_M12")], na.rm = TRUE)

did$UREE = rowSums(did[,c("BIO_PRE_UREE_INCL",
                          "BIO_PRE_UREE_V2_M3",
                          "BIO_PRE_UREE_V5_M6",
                          "BIO_PRE_UREE_V8_M9",
                          "BIO_PRE_UREE_V11_M12")], na.rm = T)

did$CREAT = rowSums(did[, c("BIO_PRE_CREAT_INCL",
                            "BIO_PRE_CREAT_V2_M3",
                            "BIO_PRE_CREAT_V5_M6",
                            "BIO_PRE_CREAT_V8_M9",
                            "BIO_PRE_CREAT_V11_M12")], na.rm = TRUE)

did$BICAR = rowSums(did[,c("BIO_PRE_BICARB_INCL",
                           "BIO_PRE_BICARB_V2_M3",
                           "BIO_PRE_BICARB_V5_M6",
                           "BIO_PRE_BICARB_V8_M9",
                           "BIO_PRE_BICARB_V11_M12")], na.rm = T)

did$CALC = rowSums(did[, c("BIO_PRE_CALC_INCL",
                           "BIO_PRE_CALC_V2_M3",
                           "BIO_PRE_CALC_V5_M6",
                           "BIO_PRE_CALC_V8_M9",
                           "BIO_PRE_CALC_V11_M12")], na.rm = TRUE)

did$PHOS = rowSums(did[,c("BIO_PRE_PHOS_INCL",
                          "BIO_PRE_PHOS_V2_M3",
                          "BIO_PRE_PHOS_V5_M6",
                          "BIO_PRE_PHOS_V8_M9",
                          "BIO_PRE_PHOS_V11_M12")], na.rm = T)

did$PTH = rowSums(did[,c("BIO_PRE_PTH_INCL",
                          "BIO_PRE_PTH_V2_M3",
                          "BIO_PRE_PTH_V5_M6",
                          "BIO_PRE_PTH_V8_M9",
                          "BIO_PRE_PTH_V11_M12")], na.rm = T)

did$PROT = rowSums(did[,c("BIO_PRE_PROT_INCL",
                          "BIO_PRE_PROT_V2_M3",
                          "BIO_PRE_PROT_V5_M6",
                          "BIO_PRE_PROT_V8_M9",
                          "BIO_PRE_PROT_V11_M12")], na.rm = T)

did$BIO_PRE_ALBU_V2_M3 = as.numeric(as.character(did$BIO_PRE_ALBU_V2_M3))
did$ALBU = rowSums(did[,c("BIO_PRE_ALBU_INCL",
                          "BIO_PRE_ALBU_V2_M3",
                          "BIO_PRE_ALBU_V5_M6",
                          "BIO_PRE_ALBU_V8_M9",
                          "BIO_PRE_ALBU_V11_M12")], na.rm = TRUE)

did$HEMO = rowSums(did[,c("BIO_PRE_HEMO_INCL",
                          "BIO_PRE_HEMO_V2_M3",
                          "BIO_PRE_HEMO_V5_M6",
                          "BIO_PRE_HEMO_V8_M9",
                          "BIO_PRE_HEMO_V11_M12")], na.rm = T)

did$BIO_PRE_FERR_V2_M3 = as.numeric(as.character(did$BIO_PRE_FERR_V2_M3))
did$FERR = rowSums(did[,c("BIO_PRE_FERR_INCL",
                          "BIO_PRE_FERR_V2_M3",
                          "BIO_PRE_FERR_V5_M6",
                          "BIO_PRE_FERR_V8_M9",
                          "BIO_PRE_FERR_V11_M12")], na.rm = TRUE)

did$BIO_PRE_TRANSF_V2_M3 = as.numeric(as.character(did$BIO_PRE_TRANSF_V2_M3))
did$TRANSF = rowSums(did[,c("BIO_PRE_TRANSF_INCL",
                           "BIO_PRE_TRANSF_V2_M3",
                           "BIO_PRE_TRANSF_V5_M6",
                           "BIO_PRE_TRANSF_V8_M9",
                           "BIO_PRE_TRANSF_V11_M12")], na.rm = TRUE)

did$B2 = rowSums(did[,c("BIO_PRE_B2_INCL",
                        "BIO_PRE_B2_V2_M3",
                        "BIO_PRE_B2_V5_M6",
                        "BIO_PRE_B2_V8_M9",
                        "BIO_PRE_B2_V11_M12")], na.rm = TRUE)

did$POSTF = rowSums(did[,c("BIO_POST_F_INCL",
                           "BIO_POST_F_V2_M3",
                           "BIO_POST_F_V5_M6",
                           "BIO_POST_F_V8_M9",
                           "BIO_POST_F_V11_M12")], na.rm = TRUE)

did$UREE = rowSums(did[,c("BIO_POST_UREE_INCL",
                          "BIO_POST_UREE_V2_M3",
                          "BIO_POST_UREE_V5_M6",
                          "BIO_POST_UREE_V8_M9",
                          "BIO_POST_UREE_V11_M12")], na.rm = TRUE)

did$PREPOIDS = rowSums(did[,c("DIAL_PRE_POIDS_INCL",
                              "DIAL_PRE_POIDS_V2_M3",
                              "DIAL_PRE_POIDS_V5_M6",
                              "DIAL_PRE_POIDS_V8_M9",
                              "DIAL_PRE_POIDS_V11_M12")], na.rm = TRUE)

did$POSTPOIDS = rowSums(did[,c("DIAL_POST_POIDS_INCL",
                               "DIAL_POST_POIDS_V2_M3",
                               "DIAL_POST_POIDS_V5_M6",
                               "DIAL_POST_POIDS_V8_M9",
                               "DIAL_POST_POIDS_V11_M12")], na.rm = TRUE)

did$DIAL_DUREE_INCL = as.numeric(as.character(did$DIAL_DUREE_INCL))
did$DIAL_DUREE_V2_M3 = as.numeric(as.character(did$DIAL_DUREE_V2_M3))
did$DUREE = rowSums(did[,c("DIAL_DUREE_INCL",
                           "DIAL_DUREE_V2_M3",
                           "DIAL_DUREE_V5_M6",
                           "DIAL_DUREE_V8_M9",
                           "DIAL_DUREE_V11_M12")], na.rm = TRUE)

did$UF = rowSums(did[,c("DIAL_UF_INCL",
                        "DIAL_UF_V2_M3",
                        "DIAL_UF_V5_M6",
                        "DIAL_UF_V8_M9",
                        "DIAL_UF_V11_M12")], na.rm = TRUE)

did$NB = rowSums(did[,c("DIAL_NB_INCL",
                        "DIAL_NB_V2_M3",
                        "DIAL_NB_V5_M6",
                        "DIAL_NB_V8_M9",
                        "DIAL_NB_V11_M12")], na.rm = TRUE)

did$KTV = rowSums(did[,c("DIAL_KTV_INCL",
                         "DIAL_KTV_V2_M3",
                         "DIAL_KTV_V8_M9",
                         "DIAL_KTV_V11_M12")], na.rm = TRUE)

varsmean = c("poids", "PAS", "PAD", "PREF", "UREE", "CREAT", "BICAR", "CALC", "PHOS", "PTH",
             "PROT", "ALBU", "HEMO", "FERR", "TRANSF", "B2", "POSTF", "UREE",
             "PREPOIDS", "POSTPOIDS", "DUREE", "UF", "NB", "KTV")

biomean <- CreateTableOne(vars = varsmean, data = did, factorVars = , 
                               includeNA = TRUE)
print(biomean, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(biomean)

#-SATISFACTION-----
#0,1,2 = d'accord, moyennement, pas d'accord

#IDEL_FORMAT
#IDEL_TEMPS
#IDEL_OCTROIE
#IDEL_JOIGN
#IDEL_HDD
#IDEL_NGAP

table(did$IDEL_FORMAT_V11_M12)
table(did$IDEL_TEMPS_V11_M12)
table(did$IDEL_OCTROIE_V11_M12, useNA = "always")
table(did$IDEL_JOIGN_V11_M12)
table(did$IDEL_HDD_V11_M12)
table(did$IDEL_NGAP_V11_M12)

savar = c("IDEL_FORMAT_V11_M12", "IDEL_TEMPS_V11_M12", "IDEL_OCTROIE_V11_M12", 
          "IDEL_JOIGN_V11_M12", "IDEL_HDD_V11_M12", "IDEL_NGAP_V11_M12")

csavar = c("IDEL_FORMAT_V11_M12", "IDEL_TEMPS_V11_M12", "IDEL_OCTROIE_V11_M12", 
          "IDEL_JOIGN_V11_M12", "IDEL_HDD_V11_M12", "IDEL_NGAP_V11_M12")

satisfaction <- CreateTableOne(vars = savar, data = did, factorVars = csavar, 
                          includeNA = TRUE)
print(satisfaction, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(satisfaction)

#-formation

fovar = c("CLIN_HD_FREQ", "CLIN_HD_DUR_HR", "VST_NV_V4_M5", "VST_NV_EFF_V1_M2",
          "VST_NV_NB_V1_M2", "VST_NV_FORMA_NB_V1_M2", "VST_NV_FORMA_DUR_HR_V1_M2")
cfovar = c("CLIN_HD_FREQ", "CLIN_HD_DUR_HR", "VST_NV_V4_M5", "VST_NV_EFF_V1_M2",
           "VST_NV_NB_V1_M2", "VST_NV_FORMA_NB_V1_M2", "VST_NV_FORMA_DUR_HR_V1_M2")
formation <- CreateTableOne(vars = fovar, data = did, factorVars = cfovar, 
                               includeNA = TRUE)
print(formation, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(formation)

table(did$CLIN_HD_DTE)
table(did$CLIN_HD_FREQ)
table(did$CLIN_HD_DUR_HR, useNA = "always")
table(did$CLIN_HD_DUR_MIN)

table(did$VST_NV_V4_M5)
table(did$VST_NV_EFF_V1_M2)
table(did$VST_NV_NB_V1_M2)
table(did$VST_NV_FORMA_NB_V1_M2)
table(did$VST_NV_FORMA_DUR_HR_V1_M2)
table(did$VST_NV_FORMA_DUR_MIN_V1_M2)

#-score de charlson----

chavars = c("COMOR_F", "COMOR_TYPE", "COMOR_TYPE_ATE", "COMOR_ASSOC",
            "COMOR_ASSOC_IM", "COMOR_ASSOC_ICC", "COMOR_ASSOC_VASC",
            "COMOR_ASSOC_CEREB_VASC", "COMOR_ASSOC_HEPAT_NSEV",
            "COMOR_ASSOC_DEMENC", "COMOR_ASSOC_PULM", "COMOR_ASSOC_SYST",
            "COMOR_ASSOC_ULC", "COMOR_ASSOC_DIAB_SCOMP",
            "COMOR_ASSOC_DIAB_ACOMP", "COMOR_ASSOC_HEMI",
            "COMOR_ASSOC_LEUCE", "COMOR_ASSOC_LYMPH", "COMOR_ASSOC_TUM_SMETA",
            "COMOR_ASSOC_HEPAT_SEV", "COMOR_ASSOC_VIH",
            "COMOR_ASSOC_TUM_AMETA", "COMOR_ASSOC_SCORE_CALC",
            "COMOR_AUTONOM", "COMOR_AUTONOM_QUOTI", "COMOR_AUTONOM_DEPLAC")

cchavars = c("COMOR_F", "COMOR_TYPE", "COMOR_TYPE_ATE", "COMOR_ASSOC",
            "COMOR_ASSOC_IM", "COMOR_ASSOC_ICC", "COMOR_ASSOC_VASC",
            "COMOR_ASSOC_CEREB_VASC", "COMOR_ASSOC_HEPAT_NSEV",
            "COMOR_ASSOC_DEMENC", "COMOR_ASSOC_PULM", "COMOR_ASSOC_SYST",
            "COMOR_ASSOC_ULC", "COMOR_ASSOC_DIAB_SCOMP",
            "COMOR_ASSOC_DIAB_ACOMP", "COMOR_ASSOC_HEMI",
            "COMOR_ASSOC_LEUCE", "COMOR_ASSOC_LYMPH", "COMOR_ASSOC_TUM_SMETA",
            "COMOR_ASSOC_HEPAT_SEV", "COMOR_ASSOC_VIH",
            "COMOR_ASSOC_TUM_AMETA", "COMOR_ASSOC_SCORE_CALC",
            "COMOR_AUTONOM", "COMOR_AUTONOM_QUOTI", "COMOR_AUTONOM_DEPLAC")


charlson <- CreateTableOne(vars = chavars, data = did, factorVars = cchavars, 
                            includeNA = TRUE)
print(charlson, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
print(charlson)

table(did$COMOR_F)
table(did$COMOR_TYPE)
table(did$COMOR_TYPE_ATE)
table(did$COMOR_ASSOC)
table(did$COMOR_ASSOC_IM)
table(did$COMOR_ASSOC_ICC)
table(did$COMOR_ASSOC_VASC)
table(did$COMOR_ASSOC_CEREB_VASC)
table(did$COMOR_ASSOC_HEPAT_NSEV)
table(did$COMOR_ASSOC_DEMENC)
table(did$COMOR_ASSOC_PULM)
table(did$COMOR_ASSOC_SYST)
table(did$COMOR_ASSOC_ULC)
table(did$COMOR_ASSOC_DIAB_SCOMP)
table(did$COMOR_ASSOC_DIAB_ACOMP)
table(did$COMOR_ASSOC_HEMI)
table(did$COMOR_ASSOC_LEUCE)
table(did$COMOR_ASSOC_LYMPH)
table(did$COMOR_ASSOC_TUM_SMETA)
table(did$COMOR_ASSOC_HEPAT_SEV)
table(did$COMOR_ASSOC_VIH)
table(did$COMOR_ASSOC_TUM_AMETA)
table(did$COMOR_ASSOC_SCORE_CALC) # score calc
table(did$COMOR_AUTONOM)
table(did$COMOR_AUTONOM_QUOTI)
table(did$COMOR_AUTONOM_DEPLAC)

#-transp------

did$transpfrais = rowSums(did[,c("TRANSP_FRAIS_V0_M1", "TRANSP_FRAIS_V1_M2",
                                 "TRANSP_FRAIS_V2_M3", "TRANSP_FRAIS_V3_M4", 
                                 "TRANSP_FRAIS_V4_M5", "TRANSP_FRAIS_V5_M6",
                                 "TRANSP_FRAIS_V6_M7", "TRANSP_FRAIS_V7_M8",
                                 "TRANSP_FRAIS_V8_M9", "TRANSP_FRAIS_V9_M10",
                                 "TRANSP_FRAIS_V11_M12")], na.rm = T)

did$transamb = rowSums(did[,c("TRANSP_MOD_AMB_V0_M1", "TRANSP_MOD_AMB_V1_M2",
                              "TRANSP_MOD_AMB_V2_M3", "TRANSP_MOD_AMB_V3_M4", 
                              "TRANSP_MOD_AMB_V4_M5", "TRANSP_MOD_AMB_V5_M6",
                              "TRANSP_MOD_AMB_V6_M7", "TRANSP_MOD_AMB_V7_M8",
                              "TRANSP_MOD_AMB_V8_M9", "TRANSP_MOD_AMB_V10_M11")], na.rm = T)

did$transambmed = rowSums(did[,c("TRANSP_MOD_AMB_MED_NB_V0_M1",
                                 "TRANSP_MOD_AMB_MED_NB_V1_M2",
                                 "TRANSP_MOD_AMB_MED_NB_V2_M3",
                                 "TRANSP_MOD_AMB_MED_NB_V3_M4", 
                                 "TRANSP_MOD_AMB_MED_NB_V4_M5",
                                 "TRANSP_MOD_AMB_MED_NB_V5_M6",
                                 "TRANSP_MOD_AMB_MED_NB_V6_M7",
                                 "TRANSP_MOD_AMB_MED_NB_V7_M8",
                                 "TRANSP_MOD_AMB_MED_NB_V8_M9",
                                 "TRANSP_MOD_AMB_MED_NB_V10_M11",
                                 "TRANSP_MOD_AMB_MED_NB_V11_M12")], na.rm = T)

did$transpnephro = rowSums(did[,c("TRANSP_MOD_AMB_NEPHR_NB_V0_M1",
                                  "TRANSP_MOD_AMB_NEPHR_NB_V1_M2",
                                  "TRANSP_MOD_AMB_NEPHR_NB_V2_M3",
                                  "TRANSP_MOD_AMB_NEPHR_NB_V3_M4",
                                  "TRANSP_MOD_AMB_NEPHR_NB_V4_M5",
                                  "TRANSP_MOD_AMB_NEPHR_NB_V5_M6",
                                  "TRANSP_MOD_AMB_NEPHR_NB_V6_M7",
                                  "TRANSP_MOD_AMB_NEPHR_NB_V7_M8",
                                  "TRANSP_MOD_AMB_NEPHR_NB_V8_M9",
                                  "TRANSP_MOD_AMB_NEPHR_NB_V9_M10",
                                  "TRANSP_MOD_AMB_NEPHR_NB_V10_M11",
                                  "TRANSP_MOD_AMB_NEPHR_NB_V11_M12")], na.rm = TRUE)

did$transpambcad = rowSums(did[,c("TRANSP_MOD_AMB_CAD_NB_V1_M2",
                                  "TRANSP_MOD_AMB_CAD_NB_V0_M1",
                                  "TRANSP_MOD_AMB_CAD_NB_V2_M3",
                                  "TRANSP_MOD_AMB_CAD_NB_V3_M4",
                                  "TRANSP_MOD_AMB_CAD_NB_V4_M5",
                                  "TRANSP_MOD_AMB_CAD_NB_V5_M6",
                                  "TRANSP_MOD_AMB_CAD_NB_V6_M7",
                                  "TRANSP_MOD_AMB_CAD_NB_V7_M8",
                                  "TRANSP_MOD_AMB_CAD_NB_V8_M9",
                                  "TRANSP_MOD_AMB_CAD_NB_V9_M10",
                                  "TRANSP_MOD_AMB_CAD_NB_V10_M11",
                                  "TRANSP_MOD_AMB_CAD_NB_V11_M12")], na.rm = TRUE)

did$transpcdm = rowSums(did[,c("TRANSP_MOD_AMB_CDM_NB_V0_M1",
                               "TRANSP_MOD_AMB_CDM_NB_V1_M2",
                               "TRANSP_MOD_AMB_CDM_NB_V2_M3",
                               "TRANSP_MOD_AMB_CDM_NB_V3_M4",
                               "TRANSP_MOD_AMB_CDM_NB_V4_M5",
                               "TRANSP_MOD_AMB_CDM_NB_V5_M6",
                               "TRANSP_MOD_AMB_CDM_NB_V6_M7",
                               "TRANSP_MOD_AMB_CDM_NB_V7_M8",
                               "TRANSP_MOD_AMB_CDM_NB_V8_M9",
                               "TRANSP_MOD_AMB_CDM_NB_V9_M10",
                               "TRANSP_MOD_AMB_CDM_NB_V10_M11",
                               "TRANSP_MOD_AMB_CDM_NB_V11_M12")], na.rm = TRUE)

did$transpcdamb = rowSums(did[,c("TRANSP_MOD_AMB_CD_NB_V0_M1",
                                 "TRANSP_MOD_AMB_CD_NB_V1_M2", 
                                 "TRANSP_MOD_AMB_CD_NB_V2_M3",
                                 "TRANSP_MOD_AMB_CD_NB_V3_M4",
                                 "TRANSP_MOD_AMB_CD_NB_V4_M5",
                                 "TRANSP_MOD_AMB_CD_NB_V5_M6",
                                 "TRANSP_MOD_AMB_CD_NB_V6_M7",
                                 "TRANSP_MOD_AMB_CD_NB_V7_M8",
                                 "TRANSP_MOD_AMB_CD_NB_V8_M9",
                                 "TRANSP_MOD_AMB_CD_NB_V9_M10",
                                 "TRANSP_MOD_AMB_CD_NB_V10_M11",
                                 "TRANSP_MOD_AMB_CD_NB_V11_M12")], na.rm = TRUE)

did$transperso = rowSums(did[,c("TRANSP_MOD_PERSO_V0_M1",
                                "TRANSP_MOD_PERSO_V1_M2",
                                "TRANSP_MOD_PERSO_V2_M3",
                                "TRANSP_MOD_PERSO_V3_M4",
                                "TRANSP_MOD_PERSO_V4_M5",
                                "TRANSP_MOD_PERSO_V5_M6",
                                "TRANSP_MOD_PERSO_V6_M7",
                                "TRANSP_MOD_PERSO_V7_M8",
                                "TRANSP_MOD_PERSO_V8_M9",
                                "TRANSP_MOD_PERSO_V9_M10",
                                "TRANSP_MOD_PERSO_V10_M11",
                                "TRANSP_MOD_PERSO_V11_M12")], na.rm = TRUE)

did$transppersomed = rowSums(did[,c("TRANSP_MOD_PERSO_MED_NB_V0_M1",
                                    "TRANSP_MOD_PERSO_MED_NB_V1_M2",
                                    "TRANSP_MOD_PERSO_MED_NB_V2_M3",
                                    "TRANSP_MOD_PERSO_MED_NB_V3_M4",
                                    "TRANSP_MOD_PERSO_MED_NB_V4_M5",
                                    "TRANSP_MOD_PERSO_MED_NB_V5_M6",
                                    "TRANSP_MOD_PERSO_MED_NB_V6_M7",
                                    "TRANSP_MOD_PERSO_MED_NB_V7_M8",
                                    "TRANSP_MOD_PERSO_MED_NB_V8_M9",
                                    "TRANSP_MOD_PERSO_MED_NB_V9_M10",
                                    "TRANSP_MOD_PERSO_MED_NB_V10_M11",
                                    "TRANSP_MOD_PERSO_MED_NB_V11_M12")], na.rm = TRUE)

did$transppersonephro = rowSums(did[,c("TRANSP_MOD_PERSO_NEPHR_NB_V0_M1",
                                       "TRANSP_MOD_PERSO_NEPHR_NB_V1_M2",
                                       "TRANSP_MOD_PERSO_NEPHR_NB_V2_M3",
                                       "TRANSP_MOD_PERSO_NEPHR_NB_V3_M4",
                                       "TRANSP_MOD_PERSO_NEPHR_NB_V4_M5",
                                       "TRANSP_MOD_PERSO_NEPHR_NB_V5_M6",
                                       "TRANSP_MOD_PERSO_NEPHR_NB_V6_M7",
                                       "TRANSP_MOD_PERSO_NEPHR_NB_V7_M8",
                                       "TRANSP_MOD_PERSO_NEPHR_NB_V8_M9",
                                       "TRANSP_MOD_PERSO_NEPHR_NB_V9_M10",
                                       "TRANSP_MOD_PERSO_NEPHR_NB_V10_M11",
                                       "TRANSP_MOD_PERSO_NEPHR_NB_V11_M12")], na.rm = T)

did$transppersocad = rowSums(did[,c("TRANSP_MOD_PERSO_CAD_NB_V0_M1",
                                    "TRANSP_MOD_PERSO_CAD_NB_V1_M2",
                                    "TRANSP_MOD_PERSO_CAD_NB_V2_M3",
                                    "TRANSP_MOD_PERSO_CAD_NB_V3_M4",
                                    "TRANSP_MOD_PERSO_CAD_NB_V4_M5",
                                    "TRANSP_MOD_PERSO_CAD_NB_V5_M6",
                                    "TRANSP_MOD_PERSO_CAD_NB_V6_M7",
                                    "TRANSP_MOD_PERSO_CAD_NB_V7_M8",
                                    "TRANSP_MOD_PERSO_CAD_NB_V8_M9",
                                    "TRANSP_MOD_PERSO_CAD_NB_V9_M10",
                                    "TRANSP_MOD_PERSO_CAD_NB_V10_M11",
                                    "TRANSP_MOD_PERSO_CAD_NB_V11_M12")], na.rm = TRUE)

did$transppersocdm = rowSums(did[,c("TRANSP_MOD_PERSO_CDM_NB_V0_M1",
                                    "TRANSP_MOD_PERSO_CDM_NB_V1_M2",
                                    "TRANSP_MOD_PERSO_CDM_NB_V2_M3",
                                    "TRANSP_MOD_PERSO_CDM_NB_V3_M4",
                                    "TRANSP_MOD_PERSO_CDM_NB_V4_M5",
                                    "TRANSP_MOD_PERSO_CDM_NB_V5_M6",
                                    "TRANSP_MOD_PERSO_CDM_NB_V6_M7",
                                    "TRANSP_MOD_PERSO_CDM_NB_V7_M8",
                                    "TRANSP_MOD_PERSO_CDM_NB_V8_M9",
                                    "TRANSP_MOD_PERSO_CDM_NB_V9_M10",
                                    "TRANSP_MOD_PERSO_CDM_NB_V10_M11",
                                    "TRANSP_MOD_PERSO_CDM_NB_V11_M12")], na.rm = T)

did$trasppersocd = rowSums(did[,c("TRANSP_MOD_PERSO_CD_NB_V0_M1",
                                  "TRANSP_MOD_PERSO_CD_NB_V1_M2",
                                  "TRANSP_MOD_PERSO_CD_NB_V2_M3",
                                  "TRANSP_MOD_PERSO_CD_NB_V3_M4",
                                  "TRANSP_MOD_PERSO_CD_NB_V4_M5",
                                  "TRANSP_MOD_PERSO_CD_NB_V5_M6",
                                  "TRANSP_MOD_PERSO_CD_NB_V6_M7",
                                  "TRANSP_MOD_PERSO_CD_NB_V7_M8",
                                  "TRANSP_MOD_PERSO_CD_NB_V8_M9",
                                  "TRANSP_MOD_PERSO_CD_NB_V9_M10",
                                  "TRANSP_MOD_PERSO_CD_NB_V10_M11",
                                  "TRANSP_MOD_PERSO_CD_NB_V11_M12")], na.rm = T)

did$transppersoremb = rowSums(did[,c("TRANSP_MOD_PERSO_REMB_V0_M1",
                                     "TRANSP_MOD_PERSO_REMB_V1_M2",
                                     "TRANSP_MOD_PERSO_REMB_V2_M3",
                                     "TRANSP_MOD_PERSO_REMB_V3_M4",
                                     "TRANSP_MOD_PERSO_REMB_V4_M5",
                                     "TRANSP_MOD_PERSO_REMB_V5_M6",
                                     "TRANSP_MOD_PERSO_REMB_V6_M7",
                                     "TRANSP_MOD_PERSO_REMB_V7_M8",
                                     "TRANSP_MOD_PERSO_REMB_V8_M9",
                                     "TRANSP_MOD_PERSO_REMB_V9_M10",
                                     "TRANSP_MOD_PERSO_REMB_V10_M11",
                                     "TRANSP_MOD_PERSO_REMB_V11_M12")], na.rm = TRUE)

traspvars = c("transamb", "transambmed", "transpnephro", "transpambcad", 
              "transpcdm", "transpcdamb", "transperso", "transppersomed", "transppersonephro",
              "transppersocad", "transppersocdm", "trasppersocd", "transppersoremb")

ctraspvars = c("transamb", "transambmed", "transpnephro", "transpambcad", 
              "transpcdm", "transpcdamb", "transperso", "transppersomed", "transppersonephro",
              "transppersocad", "transppersocdm", "trasppersocd", "transppersoremb")

traspone <- CreateTableOne(vars = traspvars, data = did, factorVars = ctraspvars, 
                           includeNA = TRUE)
print(traspone, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
summary(traspone)

