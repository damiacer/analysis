require("readxl")
require("tidyverse")
#require("plyr")
require("lubridate")
require("here")

setwd("P:/CONSULTATION/Rat_AnneChristine/Conduite/DATA")

#-ON PC-------------------------------------------------------------------------

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

#-MERGING-----------------------------------------------------------------------

C0M <- merge(c0, a0c, by.x = "IdCohorte", by.y = "IdCohorte")
C3M <- merge(c3, a3c, by.x = "IdCohorte", by.y = "IdCohorte")
C5M <- merge(c5, a5c, by.x = "IdCohorte", by.y = "IDCOHORTE")
C7M <- merge(c7, a7c, by.x = "IdCohorte", by.y = "IDCOHORTE")

#-ON MACBOOK--------------------------------------------------------------------

c0 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "conduite0.xlsx"))
dim(c0)

c3 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "conduite3.xlsx"))
dim(c3)

c5 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "conduite5.xlsx"))
dim(c5)

c7 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "conduite7.xlsx"))
dim(c7)

a0 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "DMO_Annee0.xls"))
a3 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "DMO_Annee3.xls"))
a4 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "DMO_Annee4.xls"))
a5 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "DMO_Annee5.xls"))
a6 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "DMO_Annee6.xls"))
a7 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "DMO_Annee7.xls"))
a8 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "DMO_Annee8.xls"))
a9 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "DMO_Annee9.xls"))
a10 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                       "DMO_Annee10.xls"))
pro <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                       "DMO_Protheses.xls"))

#-VISIT VARIABLE----------------------------------------------------------------
# REPLACE VARIABLES

#a0$visit.0 = a0$VISIT
a0 = subset(a0, select = c("IdCohorte", "ArticIncl"))
names(a0)
dim(a0)

c0 = subset(c0, select = c("IdCohorte", "SEXE", "AGE", "ORIGINEG", "EDUCATION", 
                           "MARITAL", "COMMUNE", "COMMUNE2", "PROFESSION", "RETRAITE", "POIDS", 
                           "TAILLE", "BMI", "Delai_1ersymp",  
                           "Delai_diag", "PF", "RP", "BP", "MH", "RE", "SF", "VT", "GH", 
                           "HT", "PCS", "MCS", "AP", "SM", "D", "SS", "AS", "SQ12", "SQ22", 
                           "SQ23", "scorfonc", "scordoul", "scorraid", "scorfoncNorm", "scordoulNorm", 
                           "scorraidNorm", "womac", "womacNorm", "MAQ_L", "MAQ_L_MET", "MAQ_LssM", 
                           "MAQ_LssM_MET", "MAQ_P", "MAQ_P_MET", "MAQ_PMOD", "MAQ_PMOD_MET", 
                           "MAQ_PINT", "MAQ_PINT_MET", "MAQ_TOT", "MAQ_TOTssM", "MAQ_TOT_MET", 
                           "MAQ_TOTssM_MET", "SomatBr", "AnxiBr", "DysSociaBr", "HumDeprBr", 
                           "ScoGlobBr", "Somat", "Anxi", "DysSocia", "HumDepr", "ScoGlob", 
                           "STATUTPROF", "METIER1", "METIER2", "TEMPSPARTIEL1", "TEMPSPARTIEL2", 
                           "DTARRETD1", "ARRETCAUSE", "DTMALADIED1", "MALADIECAUSE", "DTINVALIDED1", 
                           "INVALIDECAUSE", "CATEGINVALIDE", "JOURSPERDUS", 
                           "CONSSITPROF", "REVENU", "SOCIALE", "PENSION", 
                           "VS_COUPLE", "VS_PROPRIO", "VS_PBARGENT", 
                           "VS_SPORT", "VS_LOISIR", "VS_VACANCES", "VS_HEBERGE", "VS_AIDEFINANCE", 
                           "VS_FAMILLE", "VS_TRAVSOC", "lecteur", "E__genoux_face", "E__genous_schuss", 
                           "E__genoux_profil", "E__genoux_defiles_FP", "Equalite_image", 
                           "Equalite_position", "EKL_GD_FT", "EKL_GD_FP", "EKL_GG_FT", "EKL_GG_FP", 
                           "EPincementD_topo", "EpincementG_topo", "EPincement_GD_FT", "EPincement_GD_FP", 
                           "Epincement_GG_FT", "Epincement_GG_FP", "EOsteoD_condylI", "EOsteoD_condylE", 
                           "EOsteoD_platI", "EOsteoD_platE", "EOsteoD_trochlI", "EOsteoD_trochlE", 
                           "EOsteoD_rotuleI", "EOsteoD_rotuleE", "EOsteoG_condylI", "EOsteoG_condylE", 
                           "EOsteoG_platI", "EOsteoG_platE", "EOsteoG_trochlI", "EOsteoG_trochlE", 
                           "EOsteoG_rotuleI", "EOsteoG_rotuleE", "Econdens_D_condI", "Econdens_D_condE", 
                           "Econdens_D_platI", "Econdens_D_platE", "Econdens_D_rotI", "Econdens_D_rotE", 
                           "Econdens_G_condI", "Econdens_G_condE", "Econdens_G_platI", "Econdens_G_platE", 
                           "Econdens_G_rotI", "Econdens_G_rotE", "Eautres_patho_D", "Eautres_patho_G", 
                           "Eautres_ano_D", "Eautres_ano_G", "Edesaxation_D", "Edesaxation_G", 
                           "Eremarques", "Eremarques2", "Eremarques3", "Eremarques4", "date_lecture", 
                           "date_RX", "S__genoux_face", "S__genous_schuss", "S__genoux_profil", 
                           "S__genoux_defiles_FP", "Squalite_image", "Squalite_position", 
                           "SKL_GD_FT", "SKL_GG_FT", "SPincementD_topo", "SpincementG_topo", 
                           "SPincement_GD_FT", "Spincement_GG_FT", "SOsteoD_condylI", "SOsteoD_condylE", 
                           "SOsteoD_platI", "SOsteoD_platE", "SOsteoG_condylI", "SOsteoG_condylE", 
                           "SOsteoG_platI", "SOsteoG_platE", "Scondens_D_condI", "Scondens_D_condE", 
                           "Scondens_D_platI", "Scondens_D_platE", "Scondens_G_condI", "Scondens_G_condE", 
                           "Scondens_G_platI", "Scondens_G_platE", "Sdesaxation_D", "Sdesaxation_G", 
                           "Sremarques", "Sremarques2", "Sremarques3", "Sremarques4", "Hbassin_face", 
                           "HFx_profil", "Hautre", "HQualite_image", "Hqualite_position", 
                           "HKL_HD", "HKL_HG", "HPinc_HD_topo", "HPinc_HG_topo", "HPincement_HD_grade", 
                           "HPincement_HG_grade", "HOsteo_HD_cotyle_E", "HOsteo_HD_cotyle_Int", 
                           "HOsteo_HD_cotyle_Inf", "HOsteo_HD_tete_S", "HOsteo_HD_tete_I", 
                           "HOsteo_HG_cotyle_E", "HOsteo_HG_cotyle_Int", "HOsteo_HG_cotyle_Inf", 
                           "HOsteo_HG_tete_S", "HOsteo_HG_tete_I", "Hcondens_HD_cotyle", 
                           "Hcondens_HD_tete", "Hcondens_HG_cotyle", "Hcondens_HG_tete", 
                           "Hgeodes_HD_cotyle", "Hgeodes_HD_tete", "Hgeodes_HG_cotyle", 
                           "Hgeodes_HG_tete", "Hautres_patho_HD", "Hautres_patho_HG", "Hautres_ano_bassin", 
                           "Hremarques", "Hremarques2", "Hremarques3", "Hremarques4", "Hremarques5", 
                           "Hremarques6", "RGenoux", "RHanches", "RHanchesGenoux", "Arth_radio_GG", 
                           "Arth_radio_GD", "Arth_radio_HG", "Arth_radio_HD", "GKL", "HKL")
              
)

#a3$visit.3 = a3$VISIT
#a3 = subset(a3, select = -c(VISIT))

#a4$visit.4 = a4$VISIT
#a4 = subset(a4, select = -c(VISIT))

#a5$visit.5 = a5$VISIT
#a5 = subset(a5, select = -c(VISIT))

#a6$visit.6 = a6$VISIT
#a6 = subset(a6, select = -c(VISIT))

#a7$visit.7 = a7$VISIT
#a7 = subset(a7, select = -c(VISIT))

#a8$visit.8 = a8$VISIT
#a8$IdCohorte = a8$IDCOHORTE
#a8 = subset(a8, select = -c(IDCOHORTE, VISIT))

#a9$visit.9 = a9$VISIT
#a9$IdCohorte = a9$IDCOHORTE 
#a9 = subset(a9, select = -c(IDCOHORTE, VISIT))

#a10$visit.10 = rep("10", times = 462)

#-MERGE-------------------------------------------------------------------------
#-LIST TO MERGE-----------------------------------------------------------------
dim(a0)
dim(c0)
df_list = list(a0, c0)

#-ACTUAL MERGE------------------------------------------------------------------
#library(tidyverse)
baseline <- df_list %>% reduce(full_join, by='IdCohorte')
dim(baseline)

#-VAR-RECODE--------------------------------------------------------------------

baseline <- baseline %>%
  mutate(HFx_profil = case_when(
    HFx_profil == "HD only" ~ "HD only",
    HFx_profil == "NON" ~ "NON",
    HFx_profil == "oiui" ~ "OUI",
    HFx_profil == "OUI" ~ "OUI"
  ))

baseline <- baseline %>%
  mutate(HPinc_HD_topo = case_when(
    HPinc_HD_topo == "global" ~ "global",
    HPinc_HD_topo == "Global" ~ "global",
    HPinc_HD_topo == "GLOBAL" ~ "global",
    HPinc_HD_topo == "inf" ~ "inf",
    HPinc_HD_topo  == "Inf" ~ "inf",
    HPinc_HD_topo == "onf" ~ "inf",
    HPinc_HD_topo == "post" ~ "post",
    HPinc_HD_topo == "POST" ~ "post",
    HPinc_HD_topo == "SE" ~ "SE",
    HPinc_HD_topo == "SI" ~ "SI",
    HPinc_HD_topo == "inf/post" ~ "infpost",
    HPinc_HD_topo == "INF/POST" ~ "infpost"
  ))

baseline <- baseline %>% 
  mutate(HPinc_HG_topo = case_when(
    HPinc_HG_topo == "Glo" ~ "global",
    HPinc_HG_topo == "global" ~ "global",
    HPinc_HG_topo == "inf" ~ "inf",
    HPinc_HG_topo == "INF" ~ "inf",
    HPinc_HG_topo == "Inf" ~ "inf",
    HPinc_HG_topo == "inferieur" ~ "inf",
    HPinc_HG_topo == "INF/POST" ~ "infpost"
  ))

baseline$Edesaxation_D = if_else(baseline$Edesaxation_D == "0", "0", "1")

#-DESCRIPTIVE-------------------------------------------------------------------
library(tableone)

# dput(names(baseline))

vars = c("ArticIncl", "SEXE", "AGE", "ORIGINEG", "EDUCATION", 
         "MARITAL", "COMMUNE2", "PROFESSION", "RETRAITE", "POIDS", 
         "TAILLE", "BMI", "Delai_1ersymp",  
         "Delai_diag", "PF", "RP", "BP", "MH", "RE", "SF", "VT", "GH", 
         "HT", "PCS", "MCS", "AP", "SM", "D", "SS", "AS", "SQ12", "SQ22", 
         "SQ23", "scorfonc", "scordoul", "scorraid", "scorfoncNorm", "scordoulNorm", 
         "scorraidNorm", "womac", "womacNorm", "MAQ_L", "MAQ_L_MET", "MAQ_LssM", 
         "MAQ_LssM_MET", "MAQ_P", "MAQ_P_MET", "MAQ_PMOD", "MAQ_PMOD_MET", 
         "MAQ_PINT", "MAQ_PINT_MET", "MAQ_TOT", "MAQ_TOTssM", "MAQ_TOT_MET", 
         "MAQ_TOTssM_MET", "SomatBr", "AnxiBr", "DysSociaBr", "HumDeprBr", 
         "ScoGlobBr", "Somat", "Anxi", "DysSocia", "HumDepr", "ScoGlob", 
         "STATUTPROF",  "TEMPSPARTIEL1",  
         "ARRETCAUSE", "DTMALADIED1", "MALADIECAUSE",  
         "INVALIDECAUSE", "CATEGINVALIDE", "JOURSPERDUS", 
         "REVENU", "SOCIALE", "PENSION", 
         "VS_COUPLE", "VS_PROPRIO", "VS_PBARGENT", 
         "VS_SPORT", "VS_LOISIR", "VS_VACANCES", "VS_HEBERGE", "VS_AIDEFINANCE", 
         "VS_FAMILLE", "VS_TRAVSOC", "lecteur", "E__genoux_face", "E__genous_schuss", 
         "E__genoux_profil", "E__genoux_defiles_FP", "Equalite_image", 
         "Equalite_position", "EKL_GD_FT", "EKL_GD_FP", "EKL_GG_FT", "EKL_GG_FP", 
         "EPincementD_topo", "EpincementG_topo", "EPincement_GD_FT", "EPincement_GD_FP", 
         "Epincement_GG_FT", "Epincement_GG_FP", "EOsteoD_condylI", "EOsteoD_condylE", 
         "EOsteoD_platI", "EOsteoD_platE", "EOsteoD_trochlI", "EOsteoD_trochlE", 
         "EOsteoD_rotuleI", "EOsteoD_rotuleE", "EOsteoG_condylI", "EOsteoG_condylE", 
         "EOsteoG_platI", "EOsteoG_platE", "EOsteoG_trochlI", "EOsteoG_trochlE", 
         "EOsteoG_rotuleI", "EOsteoG_rotuleE", "Econdens_D_condI", "Econdens_D_condE", 
         "Econdens_D_platI", "Econdens_D_platE", "Econdens_D_rotI", "Econdens_D_rotE", 
         "Econdens_G_condI", "Econdens_G_condE", "Econdens_G_platI", "Econdens_G_platE", 
         "Econdens_G_rotI", "Econdens_G_rotE",  
         "Edesaxation_D", "Edesaxation_G") 
          
vars2 = c("S__genoux_face", "S__genous_schuss", "S__genoux_profil", 
         "S__genoux_defiles_FP", "Squalite_image", "Squalite_position", 
         "SKL_GD_FT", "SKL_GG_FT", "SPincementD_topo", "SpincementG_topo", 
         "SPincement_GD_FT", "Spincement_GG_FT", "SOsteoD_condylI", "SOsteoD_condylE", 
         "SOsteoD_platI", "SOsteoD_platE", "SOsteoG_condylI", "SOsteoG_condylE", 
         "SOsteoG_platI", "SOsteoG_platE", "Scondens_D_condI", "Scondens_D_condE", 
         "Scondens_D_platI", "Scondens_D_platE", "Scondens_G_condI", "Scondens_G_condE", 
         "Scondens_G_platI", "Scondens_G_platE", "Sdesaxation_D", "Sdesaxation_G", 
         "Hbassin_face", 
         "HFx_profil", "Hautre", "HQualite_image", "Hqualite_position", 
         "HKL_HD", "HKL_HG", "HPinc_HD_topo", "HPinc_HG_topo", "HPincement_HD_grade", 
         "HPincement_HG_grade", "HOsteo_HD_cotyle_E", "HOsteo_HD_cotyle_Int", 
         "HOsteo_HD_cotyle_Inf", "HOsteo_HD_tete_S", "HOsteo_HD_tete_I", 
         "HOsteo_HG_cotyle_E", "HOsteo_HG_cotyle_Int", "HOsteo_HG_cotyle_Inf", 
         "HOsteo_HG_tete_S", "HOsteo_HG_tete_I", "Hcondens_HD_cotyle", 
         "Hcondens_HD_tete", "Hcondens_HG_cotyle", "Hcondens_HG_tete", 
         "Hgeodes_HD_cotyle", "Hgeodes_HD_tete", "Hgeodes_HG_cotyle", 
         "Hgeodes_HG_tete", 
         "RGenoux", "RHanches", "RHanchesGenoux", "Arth_radio_GG", 
         "Arth_radio_GD", "Arth_radio_HG", "Arth_radio_HD", "GKL", "HKL"
)

fvars = c("ArticIncl", "SEXE", "ORIGINEG", "EDUCATION", 
           "MARITAL", "COMMUNE2", "PROFESSION", "RETRAITE", 
           "STATUTPROF", "TEMPSPARTIEL1",  
           "ARRETCAUSE", "DTMALADIED1", "MALADIECAUSE", "DTINVALIDED1", 
           "INVALIDECAUSE", "CATEGINVALIDE", "AUTRESITLIB", 
           "REVENU", "SOCIALE", "PENSION", 
           "VS_COUPLE", "VS_PROPRIO", "VS_PBARGENT", 
           "VS_SPORT", "VS_LOISIR", "VS_VACANCES", "VS_HEBERGE", "VS_AIDEFINANCE", 
           "VS_FAMILLE", "VS_TRAVSOC", "lecteur", "E__genoux_face", "E__genous_schuss", 
           "E__genoux_profil", "E__genoux_defiles_FP", "Equalite_image", 
           "Equalite_position", "EKL_GD_FT", "EKL_GD_FP", "EKL_GG_FT", "EKL_GG_FP", 
           "EPincementD_topo", "EpincementG_topo", "EPincement_GD_FT", "EPincement_GD_FP", 
           "Epincement_GG_FT", "Epincement_GG_FP", "EOsteoD_condylI", "EOsteoD_condylE", 
           "EOsteoD_platI", "EOsteoD_platE", "EOsteoD_trochlI", "EOsteoD_trochlE", 
           "EOsteoD_rotuleI", "EOsteoD_rotuleE", "EOsteoG_condylI", "EOsteoG_condylE", 
           "EOsteoG_platI", "EOsteoG_platE", "EOsteoG_trochlI", "EOsteoG_trochlE", 
           "EOsteoG_rotuleI", "EOsteoG_rotuleE", "Econdens_D_condI", "Econdens_D_condE", 
           "Econdens_D_platI", "Econdens_D_platE", "Econdens_D_rotI", "Econdens_D_rotE", 
           "Econdens_G_condI", "Econdens_G_condE", "Econdens_G_platI", "Econdens_G_platE", 
           "Econdens_G_rotI", "Econdens_G_rotE",
           "Edesaxation_D", "Edesaxation_G") 
            
fvars2 = c("S__genoux_face", "S__genous_schuss", "S__genoux_profil", 
           "S__genoux_defiles_FP", "Squalite_image", "Squalite_position", 
           "SKL_GD_FT", "SKL_GG_FT", "SPincementD_topo", "SpincementG_topo", 
           "SPincement_GD_FT", "Spincement_GG_FT", "SOsteoD_condylI", "SOsteoD_condylE", 
           "SOsteoD_platI", "SOsteoD_platE", "SOsteoG_condylI", "SOsteoG_condylE", 
           "SOsteoG_platI", "SOsteoG_platE", "Scondens_D_condI", "Scondens_D_condE", 
           "Scondens_D_platI", "Scondens_D_platE", "Scondens_G_condI", "Scondens_G_condE", 
           "Scondens_G_platI", "Scondens_G_platE", "Sdesaxation_D", "Sdesaxation_G", 
           "Hbassin_face", 
           "HFx_profil", "Hautre", "HQualite_image", "Hqualite_position", 
           "HKL_HD", "HKL_HG", "HPinc_HD_topo", "HPinc_HG_topo", "HPincement_HD_grade", 
           "HPincement_HG_grade", "HOsteo_HD_cotyle_E", "HOsteo_HD_cotyle_Int", 
           "HOsteo_HD_cotyle_Inf", "HOsteo_HD_tete_S", "HOsteo_HD_tete_I", 
           "HOsteo_HG_cotyle_E", "HOsteo_HG_cotyle_Int", "HOsteo_HG_cotyle_Inf", 
           "HOsteo_HG_tete_S", "HOsteo_HG_tete_I", "Hcondens_HD_cotyle", 
           "Hcondens_HD_tete", "Hcondens_HG_cotyle", "Hcondens_HG_tete", 
           "Hgeodes_HD_cotyle", "Hgeodes_HD_tete", "Hgeodes_HG_cotyle", 
           "Hgeodes_HG_tete",  
           "RGenoux", "RHanches", "RHanchesGenoux", "Arth_radio_GG", 
           "Arth_radio_GD", "Arth_radio_HG", "Arth_radio_HD", "GKL", "HKL"
  )


descriptive = CreateTableOne(vars = vars, factorVars = fvars, data = baseline)
print(descriptive, showAllLevels = T, quote = TRUE, noSpaces = T)

descriptive2 = CreateTableOne(vars = vars2, factorVars = fvars2, data = baseline)
print(descriptive2, showAllLevels = T, quote = TRUE, noSpaces = T)

#-BIVARIATE---------------------------------------------------------------------

vars1 = c("ArticIncl", "SEXE", "AGE", "ORIGINEG", "EDUCATION", 
         "MARITAL", "COMMUNE2", "PROFESSION", "RETRAITE", "POIDS", 
         "TAILLE", "BMI", "Delai_1ersymp",  
         "Delai_diag", "PF", "RP", "BP", "MH", "RE", "SF", "VT", "GH", 
         "HT", "PCS", "MCS", "AP", "SM", "D", "SS", "AS", "SQ12", "SQ22", 
         "SQ23", "scorfonc", "scordoul", "scorraid", "scorfoncNorm", "scordoulNorm", 
         "scorraidNorm", "womac", "womacNorm", "MAQ_L", "MAQ_L_MET", "MAQ_LssM", 
         "MAQ_LssM_MET", "MAQ_P", "MAQ_P_MET", "MAQ_PMOD", "MAQ_PMOD_MET", 
         "MAQ_PINT", "MAQ_PINT_MET", "MAQ_TOT", "MAQ_TOTssM", "MAQ_TOT_MET", 
         "MAQ_TOTssM_MET", "SomatBr", "AnxiBr", "DysSociaBr", "HumDeprBr", 
         "ScoGlobBr", "Somat", "Anxi", "DysSocia", "HumDepr", "ScoGlob", 
         "STATUTPROF",  "TEMPSPARTIEL1",  
         "ARRETCAUSE", "DTMALADIED1", "MALADIECAUSE",  
         "INVALIDECAUSE", "CATEGINVALIDE", "JOURSPERDUS", 
         "REVENU", "SOCIALE", "PENSION", 
         "VS_COUPLE", "VS_PROPRIO", "VS_PBARGENT", 
         "VS_SPORT", "VS_LOISIR", "VS_VACANCES", "VS_HEBERGE", "VS_AIDEFINANCE", 
         "VS_FAMILLE", "VS_TRAVSOC", "lecteur", "E__genoux_face", "E__genous_schuss", 
         "E__genoux_profil", "E__genoux_defiles_FP", "Equalite_image") 
         
vars2 = c("Equalite_position", "EKL_GD_FT", "EKL_GD_FP", "EKL_GG_FT", "EKL_GG_FP", 
         "EPincementD_topo", "EpincementG_topo", "EPincement_GD_FT", "EPincement_GD_FP", 
         "Epincement_GG_FT", "Epincement_GG_FP", "EOsteoD_condylI", "EOsteoD_condylE", 
         "EOsteoD_platI", "EOsteoD_platE", "EOsteoD_trochlI", "EOsteoD_trochlE", 
         "EOsteoD_rotuleI", "EOsteoD_rotuleE", "EOsteoG_condylI", "EOsteoG_condylE", 
         "EOsteoG_platI", "EOsteoG_platE", "EOsteoG_trochlI", "EOsteoG_trochlE", 
         "EOsteoG_rotuleI", "EOsteoG_rotuleE", "Econdens_D_condI", "Econdens_D_condE", 
         "Econdens_D_platI", "Econdens_D_platE", "Econdens_D_rotI", "Econdens_D_rotE", 
         "Econdens_G_condI", "Econdens_G_condE", "Econdens_G_platI", "Econdens_G_platE", 
         "Econdens_G_rotI", "Econdens_G_rotE",  
         "Edesaxation_D", "Edesaxation_G") 

vars3 = c("S__genoux_face", "S__genous_schuss", "S__genoux_profil", 
          "S__genoux_defiles_FP", "Squalite_image", "Squalite_position", 
          "SKL_GD_FT", "SKL_GG_FT", "SPincementD_topo", "SpincementG_topo", 
          "SPincement_GD_FT", "Spincement_GG_FT", "SOsteoD_condylI", "SOsteoD_condylE", 
          "SOsteoD_platI", "SOsteoD_platE", "SOsteoG_condylI", "SOsteoG_condylE", 
          "SOsteoG_platI", "SOsteoG_platE", "Scondens_D_condI", "Scondens_D_condE", 
          "Scondens_D_platI", "Scondens_D_platE", "Scondens_G_condI", "Scondens_G_condE", 
          "Scondens_G_platI", "Scondens_G_platE", "Sdesaxation_D", "Sdesaxation_G", 
          "Hbassin_face", 
          "HFx_profil", "Hautre", "HQualite_image", "Hqualite_position") 

vars4 = c("HKL_HD", "HKL_HG", "HPinc_HD_topo", "HPinc_HG_topo", "HPincement_HD_grade", 
          "HPincement_HG_grade", "HOsteo_HD_cotyle_E", "HOsteo_HD_cotyle_Int", 
          "HOsteo_HD_cotyle_Inf", "HOsteo_HD_tete_S", "HOsteo_HD_tete_I", 
          "HOsteo_HG_cotyle_E", "HOsteo_HG_cotyle_Int", "HOsteo_HG_cotyle_Inf", 
          "HOsteo_HG_tete_S", "HOsteo_HG_tete_I", "Hcondens_HD_cotyle", 
          "Hcondens_HD_tete", "Hcondens_HG_cotyle", "Hcondens_HG_tete", 
          "Hgeodes_HD_cotyle", "Hgeodes_HD_tete", "Hgeodes_HG_cotyle", 
          "Hgeodes_HG_tete", 
          "RGenoux", "RHanches", "RHanchesGenoux", "Arth_radio_GG", 
          "Arth_radio_GD", "Arth_radio_HG", "Arth_radio_HD", "GKL", "HKL")

fvars1 = c("ArticIncl", "SEXE", "ORIGINEG", "EDUCATION", 
          "MARITAL", "COMMUNE2", "PROFESSION", "RETRAITE", 
          "STATUTPROF", "TEMPSPARTIEL1",  
          "ARRETCAUSE", "DTMALADIED1", "MALADIECAUSE", "DTINVALIDED1", 
          "INVALIDECAUSE", "CATEGINVALIDE", "AUTRESITLIB", 
          "REVENU", "SOCIALE", "PENSION", 
          "VS_COUPLE", "VS_PROPRIO", "VS_PBARGENT", 
          "VS_SPORT", "VS_LOISIR", "VS_VACANCES", "VS_HEBERGE", "VS_AIDEFINANCE", 
          "VS_FAMILLE", "VS_TRAVSOC", "lecteur", "E__genoux_face", "E__genous_schuss", 
          "E__genoux_profil", "E__genoux_defiles_FP", "Equalite_image") 

fvars2 = c("Equalite_position", "EKL_GD_FT", "EKL_GD_FP", "EKL_GG_FT", "EKL_GG_FP", 
          "EPincementD_topo", "EpincementG_topo", "EPincement_GD_FT", "EPincement_GD_FP", 
          "Epincement_GG_FT", "Epincement_GG_FP", "EOsteoD_condylI", "EOsteoD_condylE", 
          "EOsteoD_platI", "EOsteoD_platE", "EOsteoD_trochlI", "EOsteoD_trochlE", 
          "EOsteoD_rotuleI", "EOsteoD_rotuleE", "EOsteoG_condylI", "EOsteoG_condylE", 
          "EOsteoG_platI", "EOsteoG_platE", "EOsteoG_trochlI", "EOsteoG_trochlE", 
          "EOsteoG_rotuleI", "EOsteoG_rotuleE", "Econdens_D_condI", "Econdens_D_condE", 
          "Econdens_D_platI", "Econdens_D_platE", "Econdens_D_rotI", "Econdens_D_rotE", 
          "Econdens_G_condI", "Econdens_G_condE", "Econdens_G_platI", "Econdens_G_platE", 
          "Econdens_G_rotI", "Econdens_G_rotE",
          "Edesaxation_D", "Edesaxation_G") 

fvars3 = c("S__genoux_face", "S__genous_schuss", "S__genoux_profil", 
           "S__genoux_defiles_FP", "Squalite_image", "Squalite_position", 
           "SKL_GD_FT", "SKL_GG_FT", "SPincementD_topo", "SpincementG_topo", 
           "SPincement_GD_FT", "Spincement_GG_FT", "SOsteoD_condylI", "SOsteoD_condylE", 
           "SOsteoD_platI", "SOsteoD_platE", "SOsteoG_condylI", "SOsteoG_condylE", 
           "SOsteoG_platI", "SOsteoG_platE", "Scondens_D_condI", "Scondens_D_condE", 
           "Scondens_D_platI", "Scondens_D_platE", "Scondens_G_condI", "Scondens_G_condE", 
           "Scondens_G_platI", "Scondens_G_platE", "Sdesaxation_D", "Sdesaxation_G", 
           "Hbassin_face", "HFx_profil", "Hautre", "HQualite_image", "Hqualite_position")

fvars4 = c("HKL_HD", "HKL_HG", "HPinc_HD_topo", "HPinc_HG_topo", "HPincement_HD_grade", 
           "HPincement_HG_grade", "HOsteo_HD_cotyle_E", "HOsteo_HD_cotyle_Int", 
           "HOsteo_HD_cotyle_Inf", "HOsteo_HD_tete_S", "HOsteo_HD_tete_I", 
           "HOsteo_HG_cotyle_E", "HOsteo_HG_cotyle_Int", "HOsteo_HG_cotyle_Inf", 
           "HOsteo_HG_tete_S", "HOsteo_HG_tete_I", "Hcondens_HD_cotyle", 
           "Hcondens_HD_tete", "Hcondens_HG_cotyle", "Hcondens_HG_tete", 
           "Hgeodes_HD_cotyle", "Hgeodes_HD_tete", "Hgeodes_HG_cotyle", 
           "Hgeodes_HG_tete",  
           "RGenoux", "RHanches", "RHanchesGenoux", "Arth_radio_GG", 
           "Arth_radio_GD", "Arth_radio_HG", "Arth_radio_HD", "GKL", "HKL")


desbygroup_1 = CreateTableOne(vars = vars1, factorVars = fvars1, data = baseline,
                              test = FALSE, strata = "ArticIncl")
print(desbygroup_1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

desbygroup_2 = CreateTableOne(vars = vars2, factorVars = fvars2, data = baseline,
                              test = FALSE, strata = "ArticIncl")
print(desbygroup_2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

desbygroup_3 = CreateTableOne(vars = vars3, factorVars = fvars3, data = baseline,
                              test = FALSE, strata = "ArticIncl")
print(desbygroup_3, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

desbygroup_4 = CreateTableOne(vars = vars4, factorVars = fvars4, data = baseline,
                              test = FALSE, strata = "ArticIncl")
print(desbygroup_4, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#-MIXED-LINEAR------------------------------------------------------------------

require(lme4)

# y = difficultés d'utilisation de la voiture (c0-3-5-7)
# x = caractéristiques du patient 

modmix1 = lmer(y ~ x + (1 | patientid), data = database)

#-estimate-of-the-random-effects------------------------------------------------
#random intercept explanation: https://m-clark.github.io/mixed-models-with-R/random_intercepts.html

ranef(modmix1)$patientid %>% head(5)

coef(modmix1)$patientid %>% head(5)

#If we did not allow occasion to vary, it is constant (fixed effect) for al
#the patients. We can be interested in these effects, and under LME4, we can 
#evaluate them by the bootstraping function "bootMer". 
#alternatively, we can use the "metTools" package in the following way:

require(merTools)

#for model predictions (with new data)
predictInterval(modmix1)

#mean, meadian and sd of the random effects estimates

REsim(modmix1)

#plot thje interval estimates 
plotREsim(REsim(modmix1))

#The  plot is of the estimated random effects for each student and their interval 
#estimate (a modified version of the plot produced by that last line of code10).
#The random effects are normally distributed with a mean of zero, shown by the 
#horizontal line. Intervals that do not include zero are in bold.

#prediction
predict(modmix1)
