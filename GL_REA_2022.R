getwd()
setwd("P:/CONSULTATION/Guerre_Leonard")

#-------------------------------------------------------------------------------

# PACKAGES

# install.packages("readxl")
library("readxl")
#install.packages("Rtools", lib="C:/Users/cerasuolo-d.CHU-CAEN/Documents/R/win-library/4.1")
#install.packages("rlang", lib="C:/Users/cerasuolo-d.CHU-CAEN/Documents/R/win-library/4.1")
library(tableone)
library(dplyr)

#-------------------------------------------------------------------------------

# DATABASE

lg <- read_excel("DATABASE16032022_Vf.xlsx", na="NA")
#dim(lg)
#names(lg)

#-------------------------------------------------------------------------------

# DESCRIPTIVE FOR THE COMPLICATIONS 
# BY TABLEONE

CreateTableOne(data = lg) # create the tableone object
dput(names(lg)) # name output

vars <- c("sexe", "age", "taille_m", "poids_kg", "imc", "atcd_ei", 
          "atcd_cmi", "fevg_pourcent", "atcd_aomi", "atcd_avc1", "atcd_troublerythme", 
          "atcd_troubleconduction", "atcd_itc", "atcd_bpco", "atcd_saos", 
          "atcd_diabete", "atcd_immunodep", "atcd_tabac", "atcd_ethylisme", 
          "atcd_toxicomanie", "bmr", "ttt_kardegic", "ttt_AntiP2Y12", 
          "ttt_HBPMCuratif", "ttt_antivitK", "ttt_AOD", "ttt_betabloquant", 
          "ttt_IEC_Sartan", "ttt_HTAautre", "ttt_diuretique", "ttt_antiarythmique", 
          "ttt_antidiabétiquesoraux", "ttt_insuline", "hemoglobine_gdL", 
          "plaquettes_GL", "TP_pourcent", "TCA", "Fibrinogene_gL", 
          "leucocytes_GL", "PNN", "lymphocytes_GL", "creatinine_micromolL", 
          "albumine_gL", "preAlbumine_gL", "HbA1c_pourcent", "troponine_ngmL", 
          "valve", "localisationEI", "severite_atteinteValvulaire", 
          "embols", "porteEntree", "germe", "hemoculture", "cultureValve", 
          "PCR", "atb_penicilline", "atb_cephalosporine", "atb_rifampicine", 
          "atb_vancomycine", "atb_aminoside", "atb_quinolone", "atb_autres", 
          "duree_atbpreop", "atb_Stopee", "typeChirurgie", "delai_diagChir", 
          "degree_urgenceChir", "indicationChir", "duree_CEC", 
          "min_clampageAortique", "cardioplegie", "cardiople_anteretro", 
          "nbAmines", "NO", "IGSII", "SOFA", "EUROSCORE_II", "jours_sejour", 
          "jours_sejourREA", "deces_enHospit", "arretcardiaqueRecupere", 
          "infarctus_myocarde", "Choc", "Duree_jj_supportAminergique", 
          "defaillance_neuro", "AVC", "IRA_KDIGO", "duree_jj_EER", 
          "transfusion_CGR", "transfusion_PFC", "transfusionCUP", "CIVD", 
          "duree_jj_ventilationMecan", "reintubation", "PAVM", 
          "BAV_HgradePostOP_inf24h", "rehosp_3mois", "complication")

qualvars <- c("sexe", "atcd_ei", 
              "atcd_cmi", "atcd_aomi", "atcd_avc1", "atcd_troublerythme", 
              "atcd_troubleconduction", "atcd_itc", "atcd_bpco", "atcd_saos", 
              "atcd_diabete", "atcd_immunodep", "atcd_tabac", "atcd_ethylisme", 
              "atcd_toxicomanie", "bmr", "ttt_kardegic", "ttt_AntiP2Y12", 
              "ttt_HBPMCuratif", "ttt_antivitK", "ttt_AOD", "ttt_betabloquant",
              "ttt_IEC_Sartan", "ttt_HTAautre", "ttt_diuretique", 
              "ttt_antiarythmique", "ttt_antidiabétiquesoraux", "ttt_insuline", 
              "valve", "localisationEI", "severite_atteinteValvulaire", 
              "embols", "porteEntree", "germe", "hemoculture", "cultureValve", 
              "PCR", "atb_penicilline", "atb_cephalosporine", "atb_rifampicine", 
              "atb_vancomycine", "atb_aminoside", "atb_quinolone", "atb_autres", 
              "atb_Stopee", "typeChirurgie", 
              "degree_urgenceChir", "indicationChir", "deces_enHospit", 
              "arretcardiaqueRecupere", "infarctus_myocarde", "Choc",  
              "defaillance_neuro", "AVC", "IRA_KDIGO", "transfusion_CGR", 
              "transfusion_PFC", "transfusionCUP", "CIVD", 
              "reintubation", "PAVM", "BAV_HgradePostOP_inf24h", "rehosp_3mois", 
              "complication")

### TAB 1 = no strata
### show all levels of a categorial variable
tab1 <- CreateTableOne(vars = vars, data = lg, factorVars = qualvars)
print(tab1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

### TAB 2 = strata
### add test = FALSE to avoid testing, test = TRUE to test
### add strata to stratify on the interest variable


vars_1 <- c("sexe", "age", "taille_m", "poids_kg", "imc", "atcd_ei", 
          "atcd_cmi", "fevg_pourcent", "atcd_aomi", "atcd_avc1", "atcd_troublerythme", 
          "atcd_troubleconduction", "atcd_itc", "atcd_bpco", "atcd_saos", 
          "atcd_diabete", "atcd_immunodep", "atcd_tabac", "atcd_ethylisme", 
          "atcd_toxicomanie", "bmr", "ttt_kardegic", "ttt_AntiP2Y12", 
          "ttt_HBPMCuratif", "ttt_antivitK", "ttt_AOD", "ttt_betabloquant", 
          "ttt_IEC_Sartan", "ttt_HTAautre", "ttt_diuretique", "ttt_antiarythmique", 
          "ttt_antidiabétiquesoraux", "ttt_insuline", "hemoglobine_gdL") 
        vars_2 <- c("plaquettes_GL", "TP_pourcent", "TCA", "Fibrinogene_gL", 
          "leucocytes_GL", "PNN", "lymphocytes_GL", "creatinine_micromolL", 
          "albumine_gL", "preAlbumine_gL", "HbA1c_pourcent", "troponine_ngmL", 
          "valve", "localisationEI", "severite_atteinteValvulaire", 
          "embols", "porteEntree", "germe", "hemoculture", "cultureValve", 
          "PCR", "atb_penicilline", "atb_cephalosporine", "atb_rifampicine", 
          "atb_vancomycine", "atb_aminoside", "atb_quinolone", "atb_autres", 
          "duree_atbpreop", "atb_Stopee", "typeChirurgie", "delai_diagChir") 
        vars_3 <-  c("degree_urgenceChir", "indicationChir", "duree_CEC", 
          "min_clampageAortique", "cardioplegie", "cardiople_anteretro", 
          "nbAmines", "NO", "IGSII", "SOFA", "EUROSCORE_II", "jours_sejour", 
          "jours_sejourREA", "deces_enHospit", "arretcardiaqueRecupere", 
          "infarctus_myocarde", "Choc", "Duree_jj_supportAminergique", 
          "defaillance_neuro", "AVC", "IRA_KDIGO", "duree_jj_EER", 
          "transfusion_CGR", "transfusion_PFC", "transfusionCUP", "CIVD", 
          "duree_jj_ventilationMecan", "reintubation", "PAVM", 
          "BAV_HgradePostOP_inf24h", "rehosp_3mois", "complication")

qualvars_1 <- c("sexe", "atcd_ei", 
              "atcd_cmi", "atcd_aomi", "atcd_avc1", "atcd_troublerythme", 
              "atcd_troubleconduction", "atcd_itc", "atcd_bpco", "atcd_saos", 
              "atcd_diabete", "atcd_immunodep", "atcd_tabac", "atcd_ethylisme", 
              "atcd_toxicomanie", "bmr", "ttt_kardegic", "ttt_AntiP2Y12", 
              "ttt_HBPMCuratif", "ttt_antivitK", "ttt_AOD", "ttt_betabloquant",
              "ttt_IEC_Sartan", "ttt_HTAautre", "ttt_diuretique", 
              "ttt_antiarythmique", "ttt_antidiabétiquesoraux", "ttt_insuline") 
              qualvars_2 <- c("valve", "localisationEI", "severite_atteinteValvulaire", 
              "embols", "porteEntree", "germe", "hemoculture", "cultureValve", 
              "PCR", "atb_penicilline", "atb_cephalosporine", "atb_rifampicine", 
              "atb_vancomycine", "atb_aminoside", "atb_quinolone", "atb_autres", 
              "atb_Stopee", "typeChirurgie") 
              qualvars_3 <- c("degree_urgenceChir", "indicationChir", "deces_enHospit", 
              "arretcardiaqueRecupere", "infarctus_myocarde", "Choc",  
              "defaillance_neuro", "AVC", "IRA_KDIGO", "transfusion_CGR", 
              "transfusion_PFC", "transfusionCUP", "CIVD", 
              "reintubation", "PAVM", "BAV_HgradePostOP_inf24h", "rehosp_3mois", 
              "complication")

tab2_1 <- CreateTableOne(vars = vars_1, data = lg, factorVars = qualvars_1, test = FALSE, 
                       strata = "complication")
print(tab2_1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

tab2_2 <- CreateTableOne(vars = vars_2, data = lg, factorVars = qualvars_2, test = FALSE, 
                         strata = "complication")
print(tab2_2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

tab2_3 <- CreateTableOne(vars = vars_3, data = lg, factorVars = qualvars_3, test = FALSE, 
                         strata = "complication")
print(tab2_3, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#-------------------------------------------------------------------------------

# VARIABLE COMPLICATION IN TWO MODALITIES

table(lg$complication)

# 0  1  2  3  4 
# 28 77 46 93 46 

# COMPLICATION EN DEUX CATEGORIE EN INCLUANT LE DECES EN CAT 2

lg <- lg %>% 
  mutate(complication.2 = case_when(
    # aucune complication
    complication == "0" ~ "0", 
    # complication mineure 
    complication == "1" ~ "0", 
    # complication modérée
    complication == "2" ~ "1",
    # complication sévère 
    complication == "3" ~ "1",
    # décès
    complication == "4" ~ "1"
  )) 

table(lg$complication.2)
# 0   1 
# 105 185

# COMPLICATION EN DEUX CATEGORIE EN EXCLUANT LE DECES

lg <- lg %>% 
  mutate(complication.d = case_when(
    # aucune complication
    complication == "0" ~ "0", 
    # complication mineure 
    complication == "1" ~ "0", 
    # complication modérée
    complication == "2" ~ "1",
    # complication sévère 
    complication == "3" ~ "1",
    # décès
    #complication == "4" ~ "1"
  )) 

table(lg$complication.d)
#  0   1 
# 105 139 

# COMPLICATION EN TROIS CATEGORIES

lg <- lg %>% 
  mutate(complication.3 = case_when(
    # aucune complication
    complication == "0" ~ "0", 
    # complication mineure 
    complication == "1" ~ "0", 
    # complication modérée
    complication == "2" ~ "1",
    # complication sévère 
    complication == "3" ~ "1",
    # décès
    complication == "4" ~ "2"
  )) 

table(lg$complication.3)
#   0   1   2 
# 105 139  46 

# EVENEMENT DECES

lg <- lg %>% 
  mutate(death = case_when(
    # aucune complication
    complication == "0" ~ "0", 
    # complication mineure 
    complication == "1" ~ "0", 
    # complication modérée
    complication == "2" ~ "0",
    # complication sévère 
    complication == "3" ~ "0",
    # décès
    complication == "4" ~ "1"
  )) 

table(lg$death)
#  0   1 
#244  46 

#-------------------------------------------------------------------------------

# TABLE 1-2-3
# "img1.jpg" IN THE FOLDER 

# variables à inclure dans le tableau 
#   arretcardiaqueRecupere
#   infarctus_myocarde
#   Choc
#   defaillance_neuro
#   AVC
#   IRA_KDIGO
#   CIVD
#   PAVM
#   BAV_HgradePostOP_inf24h

com_vars <- c("arretcardiaqueRecupere", "infarctus_myocarde", "Choc",
              "defaillance_neuro", "AVC", "IRA_KDIGO", "CIVD", "PAVM", 
              "BAV_HgradePostOP_inf24h",
              "complication.2", "complication.d", "complication.3", "death")

comc_vars <- c("arretcardiaqueRecupere", "infarctus_myocarde", "Choc",
              "defaillance_neuro", "AVC", "IRA_KDIGO", "CIVD", "PAVM", 
              "BAV_HgradePostOP_inf24h",
              "complication.2", "complication.d", "complication.3", "death")

com_t1 <- CreateTableOne(vars = com_vars, data = lg, factorVars = comc_vars,
                             test = FALSE, strata = "complication.2")
print(com_t1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)


com_t2 <- CreateTableOne(vars = com_vars, data = lg, factorVars = comc_vars,
                         test = FALSE, strata = "complication.d")
print(com_t2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)


com_t3 <- CreateTableOne(vars = com_vars, data = lg, factorVars = comc_vars,
                         test = FALSE, strata = "complication.3")
print(com_t3, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)


death <- CreateTableOne(vars = com_vars, data = lg, factorVars = comc_vars,
                         test = FALSE, strata = "death")
print(death, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# ATTENTION : CETTE ANALYSE N'EST peut-etre PAS PERTINENTE 
# CAR LES CLASSEMENT DES COMPLICATIONS EST EMBOITE DANS LE DECES

# ANALYSE UNIVARIEE POUR DEATH

# arret card recupe
fisher.test(lg$arretcardiaqueRecupere, lg$death)

# infarctus_myocarde
#chisq.test(table(lg$infarctus_myocarde, lg$death),
#           simulate.p.value = TRUE)
x <- matrix(c(240, 45, 1, 0, 3, 1), byrow = TRUE, nrow = 3, ncol = 2)
# cette matrice fonctionne par lignes 240 et 45 sont la 1ere ligne
# 1 et 0 sont la deuxieme ligne etc
lg$infarctus_myocarde <- as.factor(lg$infarctus_myocarde)
fisher.test(lg$infarctus_myocarde, lg$death, conf.level = .95, 
            conf.int = TRUE, simulate.p.value = T, B = 2000)
#fisher.test(table(lg$infarctus_myocarde, lg$death), simulate.p.value = TRUE)

# CHOC --> les matrices montrent que la condition 
#          d'application n'est pas respectées

#44 (18.0)	2 (4.3)
#110 (45.1)	11 (23.9)
#81 (33.2)	32 (69.6)
chocmat <- matrix(data = 0, nrow = 3, ncol =2)
chocmat[1,1] = 44
chocmat[2,1] = 110
chocmat[3,1] = 81
chocmat[1,2] = 2
chocmat[2,2] = 11
chocmat[3,2] = 32
# see the concatenation method 

chocmat2 <- matrix(data = 0, nrow = 3, ncol = 2)
chocmat2[1,1] <- (sum(chocmat[1,]) + sum(chocmat[,1]))/sum(chocmat)
chocmat2[2,1] <- (sum(chocmat[2,]) + sum(chocmat[,1]))/sum(chocmat)
chocmat2[3,1] <- (sum(chocmat[3,]) + sum(chocmat[,1]))/sum(chocmat)
chocmat2[1,2] <- (sum(chocmat[1,]) + sum(chocmat[,2]))/sum(chocmat)
chocmat2[2,2] <- (sum(chocmat[2,]) + sum(chocmat[,2]))/sum(chocmat)
chocmat2[3,2] <- (sum(chocmat[3,]) + sum(chocmat[,2]))/sum(chocmat)

fisher.test(chocmat, conf.level = .95, conf.int = TRUE, 
            simulate.p.value = TRUE, B = 2000)
fisher.test(lg$Choc, lg$death, conf.level = .95, conf.int = TRUE, 
            simulate.p.value = TRUE, B = 2000)

# defaillance_neuro
#220 (90.2)	36 (78.3)
#24 (9.8)	10 (21.7)

fisher.test(lg$defaillance_neuro, lg$death)

# AVC
#231 (94.7)	39 (84.8)
#11 (4.5)	5 (10.9)
#2 (0.8)	2 (4.3)

xdef <- matrix(c(231,39,11,5,2,2), nrow = 3, ncol = 2)
fisher.test(xdef, conf.level = .95, conf.int = TRUE, 
            simulate.p.value = TRUE, B = 2000)

fisher.test(lg$AVC, lg$death, conf.level = .95, conf.int = TRUE, 
            simulate.p.value = TRUE, B = 2000)

# IRA_KDIGO
#152 (62.3)	8 (17.4)
#33 (13.5)	1 (2.2)
#15 (6.1)	3 (6.5)
#44 (18.0)	34 (73.9)

xira <- matrix(c(152,8,33,1,15,3,44,34), nrow = 4, ncol = 2)
fisher.test(xira, conf.int = TRUE, conf.level = .95,
            simulate.p.value = TRUE, B = 2000)

fisher.test(lg$IRA_KDIGO, lg$death, conf.int = TRUE, conf.level = .95,
            simulate.p.value = TRUE, B = 2000)

# CIVD 
fisher.test(lg$CIVD, lg$death)

# PAVM 
fisher.test(lg$PAVM, lg$death)

#BAV_HgradePostOP_inf24h 
#0	201 (82.4)	34 (73.9)
#1	22 (9.0)	5 (10.9)
#2	21 (8.6)	4 (8.7)
#3	0 (0.0)	3 (6.5)

xbav = matrix(c(201,34,22,5,21,4,0,3), nrow = 4, ncol = 2)
fisher.test(xbav, conf.int = TRUE, conf.level = .95, 
            simulate.p.value = TRUE, B = 2000)

fisher.test(lg$BAV_HgradePostOP_inf24h, lg$death, conf.int = TRUE, conf.level = .95, 
            simulate.p.value = TRUE, B = 2000)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# ANALYSE DES FACTEURS ASSOCIE AUX EVENEMENTS 
# LA VARIABLE COMPLICATION EST LA VARIABLE QUI EXCLUT LE DECES
# CAR IL EST CENSE ETRE SUBSTENTIELLEMENT DIFFERENT QUE LE RESTE 


vars_1 <- c("sexe", "age", "taille_m", "poids_kg", "imc", "atcd_ei", 
            "atcd_cmi", "fevg_pourcent", "atcd_aomi", "atcd_avc1", "atcd_troublerythme", 
            "atcd_troubleconduction", "atcd_itc", "atcd_bpco", "atcd_saos", 
            "atcd_diabete", "atcd_immunodep", "atcd_tabac", "atcd_ethylisme", 
            "atcd_toxicomanie", "bmr", "ttt_kardegic", "ttt_AntiP2Y12", 
            "ttt_HBPMCuratif", "ttt_antivitK", "ttt_AOD", "ttt_betabloquant", 
            "ttt_IEC_Sartan", "ttt_HTAautre", "ttt_diuretique", "ttt_antiarythmique", 
            "ttt_antidiabétiquesoraux", "ttt_insuline", "hemoglobine_gdL") 
vars_2 <- c("plaquettes_GL", "TP_pourcent", "TCA", "Fibrinogene_gL", 
            "leucocytes_GL", "PNN", "lymphocytes_GL", "creatinine_micromolL", 
            "albumine_gL", "preAlbumine_gL", "HbA1c_pourcent", "troponine_ngmL", 
            "valve", "localisationEI", "severite_atteinteValvulaire", 
            "embols", "porteEntree", "germe", "hemoculture", "cultureValve", 
            "PCR", "atb_penicilline", "atb_cephalosporine", "atb_rifampicine", 
            "atb_vancomycine", "atb_aminoside", "atb_quinolone", "atb_autres", 
            "duree_atbpreop", "atb_Stopee", "typeChirurgie", "delai_diagChir") 
vars_3 <-  c("degree_urgenceChir", "indicationChir", "duree_CEC", 
             "min_clampageAortique", "cardioplegie", "cardiople_anteretro", 
             "nbAmines", "NO", "IGSII", "SOFA", "EUROSCORE_II", "jours_sejour", 
             "jours_sejourREA", "deces_enHospit")

qualvars_1 <- c("sexe", "atcd_ei", 
                "atcd_cmi", "atcd_aomi", "atcd_avc1", "atcd_troublerythme", 
                "atcd_troubleconduction", "atcd_itc", "atcd_bpco", "atcd_saos", 
                "atcd_diabete", "atcd_immunodep", "atcd_tabac", "atcd_ethylisme", 
                "atcd_toxicomanie", "bmr", "ttt_kardegic", "ttt_AntiP2Y12", 
                "ttt_HBPMCuratif", "ttt_antivitK", "ttt_AOD", "ttt_betabloquant",
                "ttt_IEC_Sartan", "ttt_HTAautre", "ttt_diuretique", 
                "ttt_antiarythmique", "ttt_antidiabétiquesoraux", "ttt_insuline") 
qualvars_2 <- c("valve", "localisationEI", "severite_atteinteValvulaire", 
                "embols", "porteEntree", "germe", "hemoculture", "cultureValve", 
                "PCR", "atb_penicilline", "atb_cephalosporine", "atb_rifampicine", 
                "atb_vancomycine", "atb_aminoside", "atb_quinolone", "atb_autres", 
                "atb_Stopee", "typeChirurgie") 
qualvars_3 <- c("degree_urgenceChir", "indicationChir", "deces_enHospit")

uni1 <- CreateTableOne(vars = vars_1, data = lg, factorVars = qualvars_1, test = TRUE, 
                         strata = "complication.d")
print(uni1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

uni2 <- CreateTableOne(vars = vars_2, data = lg, factorVars = qualvars_2, test = TRUE, 
                         strata = "complication.d")
print(uni2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

uni3 <- CreateTableOne(vars = vars_3, data = lg, factorVars = qualvars_3, test = TRUE, 
                         strata = "complication.d")
print(uni3, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

# LA VARIABLE COMPLICATION INCLUT LE DECES COMME TROISIEME CAT
# complication.3

uni1.2 <- CreateTableOne(vars = vars_1, data = lg, factorVars = qualvars_1, test = TRUE, 
                       strata = "complication.3")
print(uni1.2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

uni2.2 <- CreateTableOne(vars = vars_2, data = lg, factorVars = qualvars_2, test = TRUE, 
                       strata = "complication.3")
print(uni2.2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

uni3.2 <- CreateTableOne(vars = vars_3, data = lg, factorVars = qualvars_3, test = TRUE, 
                       strata = "complication.3")
print(uni3.2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# UNIVARIATE ANALYIS

univar <- c("age", "imc", "atcd_ei", "hemoglobine_gdL", "plaquettes_GL",
            "leucocytes_GL", "creatinine_micromolL", "albumine_gL", "valve", 
            "germe", "cultureValve", "duree_atbpreop", "duree_CEC",
            "min_clampageAortique", "cardiople_anteretro",
            "nbAmines", "NO", "IGSII", "SOFA", "EUROSCORE_II", 
            "complication.3", "complication.d")

univarcat <- c("atcd_ei", "germe", "cardiople_anteretro", 'valve', "nbAmines", "cultureValve", "complication.3",
               "complication.d")

univ2 <- CreateTableOne(vars = univar, data = lg, factorVars = univarcat, 
                        test = F, strata = "complication.d")
print(univ2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#-------------------------------------------------------------------------------

w <- function(x){
  #result = shapiro.test(x)
  result = wilcox.test(x ~ lg$complication.d, paired=F, exact=F, correct=F,
                       alternative = "two.sided",
                       conf.int = TRUE, conf.level = 0.95)
  #result = t.test(x, y, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
  return(result)
}

k <- function(x){
  result = chisq.test(table(x, lg$complication.d), correct = F,
                      simulate.p.value = TRUE, B = 2000)
  return(result)
}

#-------------------------------------------------------------------------------

# age
w(lg$age)

# imc
w(lg$imc)

# atcd_ei 
k(lg$atcd_ei)

# hemoglobine_gdL
w(lg$hemoglobine_gdL)

# plaquettes_GL
w(lg$plaquettes_GL)

# leucocytes_GL
w(lg$leucocytes_GL)

# creatinine_micromolL
w(lg$creatinine_micromolL)

# albumine_gL
w(lg$albumine_gL)

# valve
lg$valve <- as.factor(lg$valve)
k(lg$valve)

fisher.test(lg$valve, lg$complication.d, simulate.p.value = TRUE, B = 2000)

# germe 
k(lg$germe)

# cultureValve 
k(lg$cultureValve)

# duree_atbpreop
w(lg$duree_atbpreop)

# duree_CEC
w(lg$duree_CEC)

# min_clampageAortique
w(lg$min_clampageAortique)

# cardiople_anteretro
k(lg$cardiople_anteretro)

# nbAmines 
fisher.test(lg$nbAmines, lg$complication.d, simulate.p.value = T, B = 2000)

# NO

# IGSII
w(lg$IGSII)

# SOFA 
w(lg$SOFA)

# EUROSCORE_II
w(lg$EUROSCORE_II)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

univ3 <- CreateTableOne(vars = univar, data = lg, factorVars = univarcat, 
                        test = F, strata = "complication.3")
print(univ3, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#-------------------------------------------------------------------------------

w3 <- function(x){
  #result = shapiro.test(x)
  result = kruskal.test(x, lg$complication.3)
  #result = t.test(x, y, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
  return(result)
}

k3 <- function(x){
  result = chisq.test(table(x, lg$complication.3), correct = F,
                      simulate.p.value = TRUE, B = 2000)
  return(result)
}

#-------------------------------------------------------------------------------

# age
w3(lg$age)

# imc
w3(lg$imc)

# atcd_ei 
k3(lg$atcd_ei)

# hemoglobine_gdL
w3(lg$hemoglobine_gdL)

# plaquettes_GL
w3(lg$plaquettes_GL)

# leucocytes_GL
w3(lg$leucocytes_GL)

# creatinine_micromolL
w3(lg$creatinine_micromolL)

# albumine_gL
w3(lg$albumine_gL)

# valve
lg$valve <- as.factor(lg$valve)
k3(lg$valve)

fisher.test(lg$valve, lg$complication.d, simulate.p.value = TRUE, B = 2000)

# germe 
k3(lg$germe)

# cultureValve 
k3(lg$cultureValve)

# duree_atbpreop
w3(lg$duree_atbpreop)

# duree_CEC
w3(lg$duree_CEC)

# min_clampageAortique
w3(lg$min_clampageAortique)

# cardiople_anteretro
k3(lg$cardiople_anteretro)

# nbAmines 
fisher.test(lg$nbAmines, lg$complication.d, simulate.p.value = T, B = 2000)
k3(lg$nbAmines)

# NO

# IGSII
w3(lg$IGSII)

# SOFA 
w3(lg$SOFA)

# EUROSCORE_II
w3(lg$EUROSCORE_II)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
lg$complication.d = as.factor(lg$complication.d)

fit <- glm(complication.d ~ imc + atcd_ei + creatinine_micromolL + albumine_gL + 
             duree_CEC + min_clampageAortique + as.factor(nbAmines) + 
             IGSII + SOFA + EUROSCORE_II,
           data = lg, family = binomial)
summary(fit)
summary(fit)$coefficients
exp(coefficients(fit))
exp(confint.default(fit))

cbind(exp(coefficients(fit)), exp(confint.default(fit)))
    
