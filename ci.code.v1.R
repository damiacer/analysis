getwd()
#setwd("P:/CONSULTATION/CIMHBaDD") # ON PC
setwd("/Users/damianocerasuolo/Desktop/UBRC/21_22_CONSULT_MAC/CIMHBaDD")
#-------------------------------------------------------------------------------
#install.packages("readxl")
library("readxl")
#install.packages("dplyr")
library("tidyverse")
#install.packages("plyr")
#library("plyr")
#install.packages("lubridate")
library("lubridate")
#-------------------------------------------------------------------------------
ci <- read_excel("ci.datab.xlsx", na="NA")
names(ci)
dim(ci)

table(ci$ANALYSIS)

ci2 <- ci[!(ci$ANALYSIS=="0"),]
dim(ci2)
View(ci2)
dput(names(ci2))
#-------------------------------------------------------------------------------
# numeric variables 

n <- function(x){
  result = as.numeric(as.character(x))
}


# VARIABLES AT THE INCLUSION
ci2$AGE  = n(ci2$AGE)
ci2$HEMODIAL_UF_VOL  = n(ci2$HEMODIAL_UF_VOL)
ci2$HEMODIAL_ANTICOAG_DOSE  = n(ci2$HEMODIAL_ANTICOAG_DOSE)
ci2$HEMODIAL_SANG_DEBIT = n(ci2$HEMODIAL_SANG_DEBIT)
ci2$IMC_CALC = n(ci2$IMC_CALC)
ci2$TEMP = n(ci2$TEMP)
ci2$BRANCH_TAS  = n(ci2$BRANCH_TAS)
ci2$BRANCH_TAD  = n(ci2$BRANCH_TAD)
ci2$BRANCH_FC  = n(ci2$BRANCH_FC)
ci2$BRANCH_POIDS  = n(ci2$BRANCH_POIDS)
ci2$BRANCH_SANG_DEBIT  = n(ci2$BRANCH_SANG_DEBIT)
ci2$BRANCH_PV  = n(ci2$BRANCH_PV)
ci2$BRANCH_PA  = n(ci2$BRANCH_PA)
ci2$BRANCH_UF  = n(ci2$BRANCH_UF)
ci2$BRANCH_ANTICOAG  = n(ci2$BRANCH_ANTICOAG)
ci2$BRANCH_ANTICOAG_DOSE  = n(ci2$BRANCH_ANTICOAG_DOSE)
ci2$MONIT_DEB_TAS  = n(ci2$MONIT_DEB_TAS)
ci2$MONIT_DEB_TAD  = n(ci2$MONIT_DEB_TAD)
ci2$MONIT_DEB_FC  = n(ci2$MONIT_DEB_FC)
ci2$MONIT_DEB_SANG_DEBIT  = n(ci2$MONIT_DEB_SANG_DEBIT)
ci2$MONIT_DEB_PV  = n(ci2$MONIT_DEB_PV)
ci2$MONIT_DEB_PA  = n(ci2$MONIT_DEB_PA)
ci2$ICC_SCOR = n(ci2$ICC_SCOR)
#"MONIT_DEB_PTM = n(ci2$XXX)
ci2$MONIT_DEB_UF  = n(ci2$MONIT_DEB_UF)
ci2$MONIT_FIN_F  = n(ci2$MONIT_FIN_F)
ci2$MONIT_FIN_TAS  = n(ci2$MONIT_FIN_TAS)
ci2$MONIT_FIN_TAD  = n(ci2$MONIT_FIN_TAD)
ci2$MONIT_FIN_FC = n(ci2$MONIT_FIN_FC)
ci2$MONIT_FIN_POIDS = n(ci2$MONIT_FIN_POIDS)
ci2$MONIT_FIN_SANG_DEBIT = n(ci2$MONIT_FIN_SANG_DEBIT)
ci2$MONIT_FIN_PV = n(ci2$MONIT_FIN_PV)
ci2$MONIT_FIN_PA = n(ci2$MONIT_FIN_PA)
#"MONIT_FIN_PTM = n(ci2$XXX)
ci2$MONIT_FIN_REINJ = n(ci2$MONIT_FIN_REINJ)
ci2$MONIT_FIN_UF = n(ci2$MONIT_FIN_UF)
ci2$MONIT_FIN_DIAL_VOL = n(ci2$MONIT_FIN_DIAL_VOL)
ci2$MONIT_FIN_POIDS_PERTE = n(ci2$MONIT_FIN_POIDS_PERTE)
ci2$MONIT_FIN_DUR = n(ci2$MONIT_FIN_DUR)
#"AV_DIAL_NFS_F",
ci2$AV_DIAL_NFS_HEMO = n(ci2$AV_DIAL_NFS_HEMO)
ci2$AV_DIAL_NFS_HEMAT = n(ci2$AV_DIAL_NFS_HEMAT)
ci2$AV_DIAL_NFS_LEUCO = n(ci2$AV_DIAL_NFS_LEUCO)
ci2$AV_DIAL_NFS_LYMPH = n(ci2$AV_DIAL_NFS_LYMPH)
ci2$AV_DIAL_NFS_MONO = n(ci2$AV_DIAL_NFS_MONO)
ci2$AV_DIAL_NFS_PLAQ = n(ci2$AV_DIAL_NFS_PLAQ)
#"AV_DIAL_ION_F", 
ci2$AV_DIAL_ION_SODI = n(ci2$AV_DIAL_ION_SODI)
ci2$AV_DIAL_ION_POTA = n(ci2$AV_DIAL_ION_POTA)
ci2$AV_DIAL_ION_PHOS = n(ci2$AV_DIAL_ION_PHOS)
ci2$AV_DIAL_ION_CALC = n(ci2$AV_DIAL_ION_CALC)
ci2$AV_DIAL_ION_PROT = n(ci2$AV_DIAL_ION_PROT)
ci2$AV_DIAL_ION_CREA = n(ci2$AV_DIAL_ION_CREA)
ci2$AV_DIAL_ION_UREE = n(ci2$AV_DIAL_ION_UREE)
ci2$AV_DIAL_PROT_F = n(ci2$AV_DIAL_PROT_F)
ci2$AV_DIAL_PROT_MYOG = n(ci2$AV_DIAL_PROT_MYOG)
ci2$AV_DIAL_PROT_KAPPA = n(ci2$AV_DIAL_PROT_KAPPA)
ci2$AV_DIAL_PROT_LAMBDA = n(ci2$AV_DIAL_PROT_LAMBDA)
ci2$AV_DIAL_PROT_OROS = n(ci2$AV_DIAL_PROT_OROS)
ci2$AV_DIAL_PROT_PROL = n(ci2$AV_DIAL_PROT_PROL)
ci2$AP_DIAL_PROT_F = n(ci2$AP_DIAL_PROT_F)
ci2$AP_DIAL_PROT_BETA = n(ci2$AP_DIAL_PROT_BETA)
ci2$AP_DIAL_PROT_MYOG = n(ci2$AP_DIAL_PROT_MYOG)
ci2$AP_DIAL_PROT_KAPPA = n(ci2$AP_DIAL_PROT_KAPPA)
ci2$AP_DIAL_PROT_LAMBDA = n(ci2$AP_DIAL_PROT_LAMBDA)
ci2$AP_DIAL_PROT_OROS = n(ci2$AP_DIAL_PROT_OROS)
ci2$AP_DIAL_PROT_PROL = n(ci2$AP_DIAL_PROT_PROL)
#"AP_DIAL_ION_F", 
ci2$AP_DIAL_ION_SODI = n(ci2$AP_DIAL_ION_SODI)
ci2$AP_DIAL_ION_POTA = n(ci2$AP_DIAL_ION_POTA)
ci2$AP_DIAL_ION_PHOS = n(ci2$AP_DIAL_ION_PHOS)
ci2$AP_DIAL_ION_CALC = n(ci2$AP_DIAL_ION_CALC)
ci2$AP_DIAL_ION_PROT = n(ci2$AP_DIAL_ION_PROT)
ci2$AP_DIAL_ION_CREA = n(ci2$AP_DIAL_ION_CREA)
ci2$AP_DIAL_ION_UREE = n(ci2$AP_DIAL_ION_UREE)
#ci2$PRLVMNT_Fci2$, 
ci2$PRLVMNT_BETA = n(ci2$PRLVMNT_BETA)
ci2$PRLVMNT_ALBU = n(ci2$PRLVMNT_ALBU)
ci2$PRLVMNT_UREE = n(ci2$PRLVMNT_UREE)
ci2$PRLVMNT_SODI = n(ci2$PRLVMNT_SODI)
ci2$BRANCH_TAS1 = n(ci2$BRANCH_TAS1)
ci2$BRANCH_TAD1 = n(ci2$BRANCH_TAD1)
ci2$BRANCH_FC1 = n(ci2$BRANCH_FC1)
ci2$BRANCH_POIDS1 = n(ci2$BRANCH_POIDS1)
ci2$BRANCH_SANG_DEBIT1 = n(ci2$BRANCH_SANG_DEBIT1)
ci2$BRANCH_PV1 = n(ci2$BRANCH_PV1)
ci2$BRANCH_PA1 = n(ci2$BRANCH_PA1)
#ci2$BRANCH_PTM1 = n(ci2$XXX)
ci2$BRANCH_UF1 = n(ci2$BRANCH_UF1)
ci2$BRANCH_ANTICOAG1 = n(ci2$BRANCH_ANTICOAG1)
ci2$BRANCH_ANTICOAG_DOSE1 = n(ci2$BRANCH_ANTICOAG_DOSE1)
#ci2$MONIT_DEB_F1ci2$, 
ci2$MONIT_DEB_TAS1 = n(ci2$MONIT_DEB_TAS1)
ci2$MONIT_DEB_TAD1 = n(ci2$MONIT_DEB_TAD1)
ci2$MONIT_DEB_FC1 = n(ci2$MONIT_DEB_FC1)
ci2$MONIT_DEB_SANG_DEBIT1 = n(ci2$MONIT_DEB_SANG_DEBIT1)
ci2$MONIT_DEB_PV1 = n(ci2$MONIT_DEB_PV1)
ci2$MONIT_DEB_PA1 = n(ci2$MONIT_DEB_PA1)
# ci2$MONIT_DEB_PTM1 = n(ci2$XXX)
ci2$MONIT_DEB_UF1 = n(ci2$MONIT_DEB_UF1)
ci2$MONIT_FIN_F1 = n(ci2$MONIT_FIN_F1)
ci2$MONIT_FIN_TAS1 = n(ci2$MONIT_FIN_TAS1)
ci2$MONIT_FIN_TAD1 = n(ci2$MONIT_FIN_TAD1)
ci2$MONIT_FIN_FC1 = n(ci2$MONIT_FIN_FC1)
ci2$MONIT_FIN_POIDS1 = n(ci2$MONIT_FIN_POIDS1)
ci2$MONIT_FIN_SANG_DEBIT1 = n(ci2$MONIT_FIN_SANG_DEBIT1)
ci2$MONIT_FIN_PV1 = n(ci2$MONIT_FIN_PV1)
ci2$MONIT_FIN_PA1 = n(ci2$MONIT_FIN_PA1)
#ci2$MONIT_FIN_PTM1 = n(ci2$XXX)
ci2$MONIT_FIN_REINJ1 = n(ci2$MONIT_FIN_REINJ1)
ci2$MONIT_FIN_UF1 = n(ci2$MONIT_FIN_UF1)
ci2$MONIT_FIN_DIAL_VOL1 = n(ci2$MONIT_FIN_DIAL_VOL1)
ci2$MONIT_FIN_POIDS_PERTE1 = n(ci2$MONIT_FIN_POIDS_PERTE1)
ci2$MONIT_FIN_DUR1 = n(ci2$MONIT_FIN_DUR1)
#ci2$AV_DIAL_NFS_F1ci2$,
#ci2$AV_DIAL_ION_F1ci2$, 
ci2$AV_DIAL_ION_SODI1 = n(ci2$AV_DIAL_ION_SODI1)
ci2$AV_DIAL_ION_POTA1 = n(ci2$AV_DIAL_ION_POTA1)
ci2$AV_DIAL_ION_PHOS1 = n(ci2$AV_DIAL_ION_PHOS1)
ci2$AV_DIAL_ION_CALC1 = n(ci2$AV_DIAL_ION_CALC1)
ci2$AV_DIAL_ION_PROT1 = n(ci2$AV_DIAL_ION_PROT1)
ci2$AV_DIAL_ION_CREA1 = n(ci2$AV_DIAL_ION_CREA1)
ci2$AV_DIAL_ION_UREE1 = n(ci2$AV_DIAL_ION_UREE1)
ci2$AV_DIAL_PROT_F1 = n(ci2$AV_DIAL_PROT_F1)
ci2$AV_DIAL_PROT_MYOG1 = n(ci2$AV_DIAL_PROT_MYOG1)
ci2$AV_DIAL_PROT_KAPPA1 = n(ci2$AV_DIAL_PROT_KAPPA1)
ci2$AV_DIAL_PROT_LAMBDA1 = n(ci2$AV_DIAL_PROT_LAMBDA1)
ci2$AV_DIAL_PROT_OROS1 = n(ci2$AV_DIAL_PROT_OROS1)
ci2$AV_DIAL_PROT_PROL1 = n(ci2$AV_DIAL_PROT_PROL1)
ci2$AP_DIAL_PROT_F1 = n(ci2$AP_DIAL_PROT_F1)
ci2$AP_DIAL_PROT_BETA1 = n(ci2$AP_DIAL_PROT_BETA1)
ci2$AP_DIAL_PROT_MYOG1 = n(ci2$AP_DIAL_PROT_MYOG1)
ci2$AP_DIAL_PROT_KAPPA1 = n(ci2$AP_DIAL_PROT_KAPPA1)
ci2$AP_DIAL_PROT_LAMBDA1 = n(ci2$AP_DIAL_PROT_LAMBDA1)
ci2$AP_DIAL_PROT_OROS1 = n(ci2$AP_DIAL_PROT_OROS1)
ci2$AP_DIAL_PROT_PROL1 = n(ci2$AP_DIAL_PROT_PROL1)
#ci2$AP_DIAL_ION_F1ci2$, 
ci2$AP_DIAL_ION_SODI1 = n(ci2$AP_DIAL_ION_SODI1)
ci2$AP_DIAL_ION_POTA1 = n(ci2$AP_DIAL_ION_POTA1)
ci2$AP_DIAL_ION_PHOS1 = n(ci2$AP_DIAL_ION_PHOS1)
ci2$AP_DIAL_ION_CALC1 = n(ci2$AP_DIAL_ION_CALC1)
ci2$AP_DIAL_ION_CREA1 = n(ci2$AP_DIAL_ION_CREA1)
ci2$AP_DIAL_ION_UREE1 = n(ci2$AP_DIAL_ION_UREE1)
#ci2$PRLVMNT_F1ci2$, 
ci2$PRLVMNT_BETA1 = n(ci2$PRLVMNT_BETA1)
ci2$PRLVMNT_ALBU1 = n(ci2$PRLVMNT_ALBU1)
ci2$PRLVMNT_UREE1 = n(ci2$PRLVMNT_UREE1)
ci2$PRLVMNT_SODI1  = n(ci2$PRLVMNT_SODI1) 

#-RANDO---------------------------------------------------------------------------------------
ci2 <- ci2 %>% 
  mutate(RANDO = case_when(
    RANDO_RES_REF == "A - Convection interne maitrisée à 9L puis libre à 0L" ~ "A", # matrisée  
    RANDO_RES_REF == "B - Convection interne libre à 0L puis maîtrisée à 9L" ~ "B"
  ))

ci2$RANDO

#-------------------------------------------------------------------------------
dput(names(ci2))

variables = c(
               # VARIABLESA TE INCLUSION
              "AGE", # cont 
              "RANDO_RES_REF", 
              #"INCL_DTE", 
              "RANDO_BRAS",
              #"RANDO_DTE", 
              "SEXE", 
              "IMC_CALC", 
              "TEMP", 
              "HEMODIAL_VOIE", 
              "HEMODIAL_TRANSPL", 
              "HEMODIAL_TRANSPL_NB",
              #"HEMODIAL_TRANSPL_DTE", 
              "HEMODIAL_IR_CAUS", 
              "HEMODIAL_IR_CAUS_VASC",
              "HEMODIAL_IR_CAUS_IATRO",
              "HEMODIAL_IR_CAUS_MYEL",
              "HEMODIAL_IR_CAUS_GLOM", 
              "HEMODIAL_IR_CAUS_INFECT", 
              "HEMODIAL_IR_CAUS_OBSTR",
              "HEMODIAL_IR_CAUS_ATE", 
              "HEMODIAL_SANG_DEBIT", 
              "HEMODIAL_UF_VOL", #  cont
              "HEMODIAL_HABIT",
              "HEMODIAL_ANTICOAG_ENOX", 
              #"HEMODIAL_ANTICOAG_PRE", 
              "HEMODIAL_ANTICOAG_DOSE", # cont
              "HEMODIAL_DIUR",
              "HEMODIAL_ATCD",
              "HEMODIAL_ATCD_HEMOPAT",
              #"HEMODIAL_ATCD_ATE_PRE",
              "ICC_F",
              "ICC_AGE_COD",
              "ICC_AGE_CALC", 
              "ICC_IM",
              "ICC_IC_CONG",
              "ICC_VASC",
              "ICC_DEM",
              "ICC_CHRON_PULM",
              "ICC_SYST",
              "ICC_ULCER",
              "ICC_HEPAT_MOD",  
              "ICC_DIAB_SCOMPL",
              "ICC_HEMI",
              "ICC_IR",
              "ICC_DIAB_COMPL",
              "ICC_TUM_SMETA",
              "ICC_LEUCEM",
              "ICC_HEPAT_SEV",
              "ICC_TUM_SOLID",
              "ICC_SIDA",
              "ICC_SCOR",
              # V1
              # "DIAL_DTE",
              "DIAL_RANDO_REF",
              "BRANCH_F", 
              "BRANCH_TAS", # cont
              "BRANCH_TAD", # cont
              "BRANCH_FC", # cont
              "BRANCH_POIDS", # cont
              "BRANCH_SANG_DEBIT", # cont
              "BRANCH_PV", # cont
              "BRANCH_PA", # cont
              #"BRANCH_PTM", # cont
              "BRANCH_UF", # cont
              "BRANCH_ANTICOAG", # cont
              "BRANCH_ANTICOAG_TYPE", 
              "BRANCH_ANTICOAG_DOSE", # cont
              "MONIT_DEB_F", 
              "MONIT_DEB_TAS", # cont
              "MONIT_DEB_TAD", # cont
              "MONIT_DEB_FC", # cont
              "MONIT_DEB_SANG_DEBIT", # cont
              "MONIT_DEB_PV", # cont
              "MONIT_DEB_PA", # cont
              #"MONIT_DEB_PTM", # cont
              "MONIT_DEB_UF", # cont
              "MONIT_FIN_F", # cont
              "MONIT_FIN_TAS", # cont
              "MONIT_FIN_TAD", # cont
              "MONIT_FIN_FC", # cont
              "MONIT_FIN_POIDS", # cont
              "MONIT_FIN_SANG_DEBIT", # cont
              "MONIT_FIN_PV", # cont
              "MONIT_FIN_PA", # cont
              #"MONIT_FIN_PTM", # cont
              "MONIT_FIN_REINJ", # cont
              "MONIT_FIN_UF", # cont
              "MONIT_FIN_DIAL_VOL", # cont
              "MONIT_FIN_POIDS_PERTE", # cont
              "MONIT_FIN_DUR", # cont
              "AV_DIAL_NFS_F",
              "AV_DIAL_NFS_HEMO", # cont
              "AV_DIAL_NFS_HEMAT", # cont
              "AV_DIAL_NFS_LEUCO", # cont
              "AV_DIAL_NFS_LYMPH", # cont
              "AV_DIAL_NFS_MONO", # cont
              "AV_DIAL_NFS_PLAQ", # cont
              "AV_DIAL_ION_F", 
              "AV_DIAL_ION_SODI", # cont
              "AV_DIAL_ION_POTA", # cont
              "AV_DIAL_ION_PHOS", # cont
              "AV_DIAL_ION_CALC", # cont
              "AV_DIAL_ION_PROT", # cont
              "AV_DIAL_ION_CREA", # cont
              "AV_DIAL_ION_UREE", # cont
              "AV_DIAL_PROT_F", # cont
              "AV_DIAL_PROT_MYOG", # cont
              "AV_DIAL_PROT_KAPPA", # cont
              "AV_DIAL_PROT_LAMBDA", # cont
              "AV_DIAL_PROT_OROS", # cont
              "AV_DIAL_PROT_PROL", # cont
              "AP_DIAL_PROT_F", # cont
              "AP_DIAL_PROT_BETA", # cont
              "AP_DIAL_PROT_MYOG", # cont
              "AP_DIAL_PROT_KAPPA", # cont
              "AP_DIAL_PROT_LAMBDA", # cont
              "AP_DIAL_PROT_OROS", # cont
              "AP_DIAL_PROT_PROL", # cont
              "AP_DIAL_ION_F", 
              "AP_DIAL_ION_SODI", # cont
              "AP_DIAL_ION_POTA", # cont
              "AP_DIAL_ION_PHOS", # cont
              "AP_DIAL_ION_CALC", # cont
              "AP_DIAL_ION_PROT", # cont
              "AP_DIAL_ION_CREA", # cont
              "AP_DIAL_ION_UREE", # cont
              "PRLVMNT_F", 
              "PRLVMNT_BETA", # cont
              "PRLVMNT_ALBU", # cont
              "PRLVMNT_UREE", # cont
              "PRLVMNT_SODI", # cont
              #V2
              #"DIAL_DTE1",
              "DIAL_RANDO_REF1",
              "BRANCH_F1", 
              "BRANCH_TAS1", # cont
              "BRANCH_TAD1", # cont
              "BRANCH_FC1", # cont
              "BRANCH_POIDS1", # cont
              "BRANCH_SANG_DEBIT1", # cont
              "BRANCH_PV1", # cont
              "BRANCH_PA1", # cont
              #"BRANCH_PTM1", # cont
              "BRANCH_UF1", # cont
              "BRANCH_ANTICOAG1", # cont
              "BRANCH_ANTICOAG_TYPE1", 
              "BRANCH_ANTICOAG_DOSE1", # cont
              "MONIT_DEB_F1", 
              "MONIT_DEB_TAS1", # cont
              "MONIT_DEB_TAD1", # cont
              "MONIT_DEB_FC1", # cont
              "MONIT_DEB_SANG_DEBIT1", # cont
              "MONIT_DEB_PV1", # cont
              "MONIT_DEB_PA1", # cont
              # "MONIT_DEB_PTM1", # cont
              "MONIT_DEB_UF1", # cont
              "MONIT_FIN_F1", # cont
              "MONIT_FIN_TAS1", # cont
              "MONIT_FIN_TAD1", # cont
              "MONIT_FIN_FC1", # cont
              "MONIT_FIN_POIDS1", # cont
              "MONIT_FIN_SANG_DEBIT1", # cont
              "MONIT_FIN_PV1", # cont
              "MONIT_FIN_PA1", # cont
              #"MONIT_FIN_PTM1", # cont
              "MONIT_FIN_REINJ1", # cont
              "MONIT_FIN_UF1", # cont
              "MONIT_FIN_DIAL_VOL1", # cont
              "MONIT_FIN_POIDS_PERTE1", # cont
              "MONIT_FIN_DUR1", # cont
              "AV_DIAL_NFS_F1",
              #"AV_DIAL_NFS_HEMO1", # cont
              #"AV_DIAL_NFS_HEMAT1", # cont
              #"AV_DIAL_NFS_LEUCO1", # cont
              #"AV_DIAL_NFS_LYMPH1", # cont
              #"AV_DIAL_NFS_MONO1", # cont
              #"AV_DIAL_NFS_PLAQ1", # cont
              "AV_DIAL_ION_F1", 
              "AV_DIAL_ION_SODI1", # cont
              "AV_DIAL_ION_POTA1", # cont
              "AV_DIAL_ION_PHOS1", # cont
              "AV_DIAL_ION_CALC1", # cont
              "AV_DIAL_ION_PROT1", # cont
              "AV_DIAL_ION_CREA1", # cont
              "AV_DIAL_ION_UREE1", # cont
              "AV_DIAL_PROT_F1", # cont
              "AV_DIAL_PROT_MYOG1", # cont
              "AV_DIAL_PROT_KAPPA1", # cont
              "AV_DIAL_PROT_LAMBDA1", # cont
              "AV_DIAL_PROT_OROS1", # cont
              "AV_DIAL_PROT_PROL1", # cont
              "AP_DIAL_PROT_F1", # cont
              "AP_DIAL_PROT_BETA1", # cont
              "AP_DIAL_PROT_MYOG1", # cont
              "AP_DIAL_PROT_KAPPA1", # cont
              "AP_DIAL_PROT_LAMBDA1", # cont
              "AP_DIAL_PROT_OROS1", # cont
              "AP_DIAL_PROT_PROL1", # cont
              "AP_DIAL_ION_F1", 
              "AP_DIAL_ION_SODI1", # cont
              "AP_DIAL_ION_POTA1", # cont
              "AP_DIAL_ION_PHOS1", # cont
              "AP_DIAL_ION_CALC1", # cont
              #"AP_DIAL_ION_PROT1", # cont
              "AP_DIAL_ION_CREA1", # cont
              "AP_DIAL_ION_UREE1", # cont
              "PRLVMNT_F1", 
              "PRLVMNT_BETA1", # cont
              "PRLVMNT_ALBU1", # cont
              "PRLVMNT_UREE1", # cont
              "PRLVMNT_SODI1", # cont
              "RANDO"
              )

categoricals = c(
  # VARIABLESA TE INCLUSION
  "RANDO_RES_REF", 
  #"INCL_DTE", 
  "RANDO_BRAS",
  #"RANDO_DTE", 
  "SEXE", 
  "HEMODIAL_VOIE", 
  "HEMODIAL_TRANSPL", 
  "HEMODIAL_TRANSPL_NB",
  #"HEMODIAL_TRANSPL_DTE", 
  "HEMODIAL_IR_CAUS", 
  "HEMODIAL_IR_CAUS_VASC",
  "HEMODIAL_IR_CAUS_IATRO",
  "HEMODIAL_IR_CAUS_MYEL",
  "HEMODIAL_IR_CAUS_GLOM", 
  "HEMODIAL_IR_CAUS_INFECT", 
  "HEMODIAL_IR_CAUS_OBSTR",
  "HEMODIAL_IR_CAUS_ATE", 
  "HEMODIAL_HABIT",
  "HEMODIAL_ANTICOAG_ENOX", 
  #"HEMODIAL_ANTICOAG_PRE", 
  "HEMODIAL_DIUR",
  "HEMODIAL_ATCD",
  "HEMODIAL_ATCD_HEMOPAT",
  #"HEMODIAL_ATCD_ATE_PRE",
  "ICC_F",
  "ICC_AGE_COD",
  "ICC_AGE_CALC", 
  "ICC_IM",
  "ICC_IC_CONG",
  "ICC_VASC",
  "ICC_DEM",
  "ICC_CHRON_PULM",
  "ICC_SYST",
  "ICC_ULCER",
  "ICC_HEPAT_MOD",  
  "ICC_DIAB_SCOMPL",
  "ICC_HEMI",
  "ICC_IR",
  "ICC_DIAB_COMPL",
  "ICC_TUM_SMETA",
  "ICC_LEUCEM",
  "ICC_HEPAT_SEV",
  "ICC_TUM_SOLID",
  "ICC_SIDA",
  # V1
  # "DIAL_DTE",
  "DIAL_RANDO_REF",
  "BRANCH_F", 
  "BRANCH_ANTICOAG_TYPE", 
  "MONIT_DEB_F", 
  "AV_DIAL_NFS_F",
  "AV_DIAL_ION_F", 
  "AP_DIAL_ION_F", 
  "PRLVMNT_F", 
  #V2
  #"DIAL_DTE1",
  "DIAL_RANDO_REF1",
  "BRANCH_F1", 
  "BRANCH_ANTICOAG_TYPE1", 
  "MONIT_DEB_F1", 
  "AV_DIAL_NFS_F1",
  "AV_DIAL_ION_F1", 
  "AP_DIAL_ION_F1", 
  "PRLVMNT_F1",
  "RANDO"
)

descriptive = CreateTableOne(vars = variables, data = ci2, factorVars = categoricals)
print(descriptive, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

# CREATE THE UNIVARIATE TABLE 
bivariate = CreateTableOne(vars = variables, data = ci2, factorVars = categoricals, 
                           test = FALSE,
                           strata = "RANDO")
print(bivariate, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)


# HEMODIAL_SANG_DEBIT = debit habituel 
# MONIT_DEB_SANG_DEBIT
# MONIT_DEB_SANG_DEBIT1

mean(ci2$HEMODIAL_SANG_DEBIT)
min(ci2$HEMODIAL_SANG_DEBIT)
max(ci2$HEMODIAL_SANG_DEBIT)

mean(ci2$MONIT_DEB_SANG_DEBIT)
min(ci2$MONIT_DEB_SANG_DEBIT)
mean(ci2$MONIT_DEB_SANG_DEBIT1)
min(ci2$MONIT_DEB_SANG_DEBIT1)

table(ci2$RANDO_RES_REF)

#-1ST----------------------------------------------------------------------------------------

# reduction du taux de beta-2-microglobuline en fonction du groupe de randomisation

# VARIABLES = moyenne entre branchement et monitoring à 1 heure

mean(ci2$BRANCH_SANG_DEBIT)
mean(ci2$MONIT_DEB_SANG_DEBIT)
ci2$SANGDEB = (ci2$MONIT_DEB_SANG_DEBIT) - (ci2$BRANCH_SANG_DEBIT)
shapiro.test(ci2$SANGDEB) # p-value = 2.388e-05

ci2$BRANCH_SANG_DEBIT1
ci2$MONIT_DEB_SANG_DEBIT1
ci2$SANGDEB1 = ci2$MONIT_DEB_SANG_DEBIT1 - ci2$BRANCH_SANG_DEBIT1
shapiro.test(ci2$SANGDEB1) # p-value = 1.461e-06
table(ci2$SANGDEB1, ci2$RANDO)


ci2 <- ci2 %>%
  mutate(groupe.A1 = case_when(
    RANDO == "A" & (SANGDEB <= 0 |  SANGDEB > 0) ~ SANGDEB ,
    RANDO == "B" & (SANGDEB1 <= 0 | SANGDEB1 > 0) ~ SANGDEB1
  ))
table(ci2$groupe.A1)
mean(ci2$groupe.A1)
sd(ci2$groupe.A1)
median(ci2$groupe.A1)
quantile(ci2$groupe.A1)
#quantile(ci2$groupe.A1,prob=(0.25),na.rm = TRUE)
#quantile(ci2$groupe.A1,prob=(0.75),na.rm = TRUE)

ci2 <- ci2 %>%
  mutate(groupe.A2 = case_when(
    RANDO == "A" & (SANGDEB1 <= 0 |  SANGDEB1 > 0) ~ SANGDEB1,
    RANDO == "B" & (SANGDEB <= 0 | SANGDEB > 0) ~ SANGDEB
  ))
table(ci2$groupe.A1, ci2$groupe.A2)
median(ci2$groupe.A2)
quantile(ci2$groupe.A2)

shapiro.test(ci2$groupe.A1)
shapiro.test(ci2$groupe.A2)

wilcox.test(ci2$groupe.A1, ci2$groupe.A2, exact = FALSE, paired = TRUE, #conf.int = T
            )

#-MONIT-PV----------------------------------------------------------------------------------

ci2$MONIT_DEB_PA
ci2$MONIT_DEB_PV

ci2$MONIT_DEB_PA1
ci2$MONIT_DEB_PV1

ci2 <- ci2 %>% 
  mutate(groupe.A1pa = case_when(
    RANDO == "A" & (MONIT_DEB_PA <= 0 | MONIT_DEB_PA > 0) ~ MONIT_DEB_PA,
    RANDO == "B" & (MONIT_DEB_PA1 <= 0 | MONIT_DEB_PA1 > 0) ~ MONIT_DEB_PA1
  ))
ci2$groupe.A1pa
median(ci2$groupe.A1pa)
quantile(ci2$groupe.A1pa)

ci2 <- ci2 %>% 
  mutate(groupe.A2pa = case_when(
    RANDO == "A" & (MONIT_DEB_PA1 <= 0 | MONIT_DEB_PA1 > 0) ~ MONIT_DEB_PA1,
    RANDO == "B" & (MONIT_DEB_PA <= 0 | MONIT_DEB_PA > 0) ~ MONIT_DEB_PA
  ))
ci2$groupe.A2pa
median(ci2$groupe.A2pa)
quantile(ci2$groupe.A2pa)

wilcox.test(ci2$groupe.A1pa, ci2$groupe.A2pa, exact = F, paired = T)

#------------------------------------------------------------------------------------------
# TENSION VEINEUSE
ci2 <- ci2 %>% 
  mutate(groupe.A1pv = case_when(
    RANDO == "A" & (MONIT_DEB_PV <= 0 | MONIT_DEB_PV > 0) ~ MONIT_DEB_PV,
    RANDO == "B" & (MONIT_DEB_PV1 <= 0 | MONIT_DEB_PV1 > 0) ~ MONIT_DEB_PV1
  ))
ci2$groupe.A1pv
median(ci2$groupe.A1pv)
quantile(ci2$ci2groupe.A1pv)

ci2 <- ci2 %>% 
  mutate(groupe.A2pv = case_when(
    RANDO == "A" & (MONIT_DEB_PV1 <= 0 | MONIT_DEB_PV1 > 0) ~ MONIT_DEB_PV1,
    RANDO == "B" & (MONIT_DEB_PV <= 0 | MONIT_DEB_PV > 0) ~ MONIT_DEB_PV
  ))
ci2$groupe.A2pv
median(ci2$groupe.A2pv)
quantile(ci2$groupe.A2pv)

wilcox.test(ci2$groupe.A1pv, ci2$groupe.A2pv, exact = F, paired = T)

#------------------------------------------------------------------------------------------
# BETA-2
ci2$AV_DIAL_PROT_BETA = as.numeric(as.character(ci2$AV_DIAL_PROT_BETA))
ci2$AP_DIAL_PROT_BETA = as.numeric(as.character(ci2$AP_DIAL_PROT_BETA))
ci2$APAV_PROT_BETA = ci2$AP_DIAL_PROT_BETA - ci2$AV_DIAL_PROT_BETA

ci2$AV_DIAL_PROT_BETA1 = as.numeric(as.character(ci2$AV_DIAL_PROT_BETA1))
ci2$AP_DIAL_PROT_BETA1 = as.numeric(as.character(ci2$AP_DIAL_PROT_BETA1))
ci2$APAV_PROT_BETA1 = ci2$AP_DIAL_PROT_BETA1 - ci2$AV_DIAL_PROT_BETA1

ci2 <- ci2 %>%
  mutate(groupeA.beta = case_when(
    RANDO == "A" & (APAV_PROT_BETA <= 0 | APAV_PROT_BETA > 0) ~ APAV_PROT_BETA,
    RANDO == "B" & (APAV_PROT_BETA1 <=0 | APAV_PROT_BETA1 > 0) ~ APAV_PROT_BETA1
  ))
median(ci2$groupeA.beta)
quantile(ci2$groupeA.beta)

ci2 <- ci2 %>%
  mutate(groupeB.beta = case_when(
    RANDO == "A" & (APAV_PROT_BETA1 <= 0 | APAV_PROT_BETA1 > 0) ~ APAV_PROT_BETA1,
    RANDO == "B" & (APAV_PROT_BETA <= 0 | APAV_PROT_BETA > 0) ~ APAV_PROT_BETA,
  ))
median(ci2$groupeB.beta)
quantile(ci2$groupeB.beta)

wilcox.test(ci2$groupeA.beta, ci2$groupeB.beta, exact = F, paired = T)

#------------------------------------------------------------------------------------------
# Myoglobine

ci2$AV_DIAL_PROT_MYOG = as.numeric(as.character(ci2$AV_DIAL_PROT_MYOG))
ci2$AP_DIAL_PROT_MYOG = as.numeric(as.character(ci2$AP_DIAL_PROT_MYOG))
ci2$MYOG = ci2$AP_DIAL_PROT_MYOG - ci2$AV_DIAL_PROT_MYOG

ci2$AV_DIAL_PROT_MYOG1 = as.numeric(as.character(ci2$AV_DIAL_PROT_MYOG1))
ci2$AP_DIAL_PROT_MYOG1 = as.numeric(as.character(ci2$AP_DIAL_PROT_MYOG1))
ci2$MYOG1 = ci2$AP_DIAL_PROT_MYOG1  - ci2$AV_DIAL_PROT_MYOG1

ci2 <- ci2 %>%
  mutate(groupeA.myog = case_when(
    RANDO == "A" & (ci2$MYOG <= 0 | ci2$MYOG > 0) ~ ci2$MYOG,
    RANDO == "B" & (ci2$MYOG1 <= 0 | ci2$MYOG1 > 0) ~ ci2$MYOG1
  ))
median(ci2$groupeA.myog)
quantile(ci2$groupeA.myog)

ci2 <- ci2 %>%
  mutate(groupeB.myog = case_when(
    RANDO == "A" & (ci2$MYOG1 <= 0 | ci2$MYOG > 0) ~ ci2$MYOG1,
    RANDO == "B" & (ci2$MYOG <= 0 | ci2$MYOG > 0) ~ ci2$MYOG
   ))
quantile(ci2$groupeB.myog)

wilcox.test(ci2$groupeA.myog, ci2$groupeB.myog, exact = F, paired = T)

#------------------------------------------------------------------------------------------
# kappa

ci2$AV_DIAL_PROT_KAPPA = as.numeric(as.character(ci2$AV_DIAL_PROT_KAPPA))
ci2$AP_DIAL_PROT_KAPPA = as.numeric(as.character(ci2$AP_DIAL_PROT_KAPPA))
ci2$KAPPA = ci2$AP_DIAL_PROT_KAPPA - ci2$AV_DIAL_PROT_KAPPA 

ci2$AV_DIAL_PROT_KAPPA1 = as.numeric(as.character(ci2$AV_DIAL_PROT_KAPPA1))
ci2$AP_DIAL_PROT_KAPPA1 = as.numeric(as.character(ci2$AP_DIAL_PROT_KAPPA1))
ci2$KAPPA1 = ci2$AP_DIAL_PROT_KAPPA1 - ci2$AV_DIAL_PROT_KAPPA1

ci2 <- ci2 %>%
  mutate(groupeA.kappa = case_when(
    RANDO == "A" & (KAPPA <= 0 | KAPPA > 0) ~ KAPPA, 
    RANDO == "B" & (KAPPA1 <= 0 | KAPPA1 > 0) ~ KAPPA1
  ))
quantile(ci2$groupeA.kappa)

ci2 <- ci2 %>%
  mutate(groupeB.kappa = case_when(
    RANDO == "A" & (KAPPA1 <= 0 | KAPPA1 > 0) ~ KAPPA1,
    RANDO == "B" & (KAPPA <= 0 | KAPPA > 0) ~ KAPPA
  ))
quantile(ci2$groupeB.kappa)

wilcox.test(ci2$groupeA.kappa, ci2$groupeB.kappa, exact = F, paired = T)

#------------------------------------------------------------------------------------------
# LAMBDA

ci2$AV_DIAL_PROT_LAMBDA = as.numeric(as.character(ci2$AV_DIAL_PROT_LAMBDA))
ci2$AP_DIAL_PROT_LAMBDA = as.numeric(as.character(ci2$AP_DIAL_PROT_LAMBDA))
ci2$LAMBDA = ci2$AP_DIAL_PROT_LAMBDA - ci2$AV_DIAL_PROT_LAMBDA

ci2$AV_DIAL_PROT_LAMBDA1 = as.numeric(as.character(ci2$AV_DIAL_PROT_LAMBDA1))
ci2$AP_DIAL_PROT_LAMBDA1 = as.numeric(as.character(ci2$AP_DIAL_PROT_LAMBDA1))
ci2$LAMBDA1 = ci2$AP_DIAL_PROT_LAMBDA1 - ci2$AV_DIAL_PROT_LAMBDA1

ci2 <- ci2 %>%
  mutate(groupeA.lambda = case_when(
      RANDO == "A" & (LAMBDA <= 0 | LAMBDA > 0) ~ LAMBDA,
      RANDO == "B" & (LAMBDA1 <= 0 | LAMBDA > 0) ~ LAMBDA1
                                    ))
quantile(ci2$groupeA.lambda)

ci2 <- ci2 %>%
  mutate(groupeB.lambda = case_when(
    RANDO == "A" & (LAMBDA1 <= 0 | LAMBDA > 0) ~ LAMBDA1,
    RANDO == "B" & (LAMBDA <= 0 | LAMBDA > 0) ~ LAMBDA
  ))
quantile(ci2$groupeB.lambda)
wilcox.test(ci2$groupeA.lambda, ci2$groupeB.lambda, exact = F, paired = T)

#------------------------------------------------------------------------------------------
# OROSOMUCOIDE

ci2$AV_DIAL_PROT_OROS = as.numeric(as.character(ci2$AV_DIAL_PROT_OROS))
ci2$AP_DIAL_PROT_OROS = as.numeric(as.character(ci2$AP_DIAL_PROT_OROS))
ci2$ORO = ci2$AP_DIAL_PROT_OROS - ci2$AV_DIAL_PROT_OROS

ci2$AV_DIAL_PROT_OROS1 = as.numeric(as.character(ci2$AV_DIAL_PROT_OROS1))
ci2$AP_DIAL_PROT_OROS1 = as.numeric(as.character(ci2$AP_DIAL_PROT_OROS1))
ci2$ORO1 = ci2$AP_DIAL_PROT_OROS1 - ci2$AV_DIAL_PROT_OROS1 

ci2 <- ci2 %>%
  mutate(groupeA.oro = case_when(
    RANDO == "A" & (ORO <= 0 | ORO > 0) ~ ORO,
    RANDO == "B" & (ORO1 <= 0 | ORO > 0) ~ ORO1
   ))
quantile(ci2$groupeA.oro, na.rm = TRUE)
is.na(ci2$groupeA.oro)

ci2 <- ci2 %>%
  mutate(groupeB.oro = case_when(
    RANDO == "A" & (ORO1 <= 0 | ORO1 > 0) ~ ORO1,
    RANDO == "B" & (ORO <= 0 | ORO > 0) ~ ORO
  ))
quantile(ci2$groupeB.oro, na.rm = T)

wilcox.test(ci2$groupeA.oro, ci2$groupeB.oro, exact = F, paired = T)

#------------------------------------------------------------------------------------------
# PROLACTINE

ci2$AV_DIAL_PROT_PROL = as.numeric(as.character(ci2$AV_DIAL_PROT_PROL))
ci2$AP_DIAL_PROT_PROL = as.numeric(as.character(ci2$AP_DIAL_PROT_PROL))
ci2$PROL = ci2$AP_DIAL_PROT_PROL - ci2$AV_DIAL_PROT_PROL

ci2$AV_DIAL_PROT_PROL1 = as.numeric(as.character(ci2$AV_DIAL_PROT_PROL1))
ci2$AP_DIAL_PROT_PROL1 = as.numeric(as.character(ci2$AP_DIAL_PROT_PROL1))
ci2$PROL1 = ci2$AP_DIAL_PROT_PROL1 - ci2$AV_DIAL_PROT_PROL1

ci2 <- ci2 %>% 
  mutate(groupeA.prol = case_when(
    RANDO == "A" & (PROL <= 0 | PROL > 0) ~ PROL,
    RANDO == "B" & (PROL1 <= 0 | PROL > 0) ~ PROL1
  ))
quantile(ci2$groupeA.prol )

ci2 <- ci2 %>%
  mutate(groupeB.prol = case_when(
    RANDO == "A" & (PROL1 <= 0 | PROL > 0) ~ PROL1,
    RANDO == "B" & (PROL <= 0 | PROL > 0) ~ PROL
  ))
quantile(ci2$groupeB.prol)

wilcox.test(ci2$groupeA.prol, ci2$groupeB.prol, exact = F, paired = T)

#------------------------------------------------------------------------------------------

ci2$AV_DIAL_ION_SODI = as.numeric(as.character(ci2$AV_DIAL_ION_SODI))
ci2$AP_DIAL_ION_SODI = as.numeric(as.character(ci2$AP_DIAL_ION_SODI))
ci2$SOD = ci2$AP_DIAL_ION_SODI - ci2$AV_DIAL_ION_SODI

ci2$AV_DIAL_ION_SODI1 = as.numeric(as.character(ci2$AV_DIAL_ION_SODI1))
ci2$AP_DIAL_ION_SODI1 = as.numeric(as.character(ci2$AP_DIAL_ION_SODI1))
ci2$SOD1 = ci2$AP_DIAL_ION_SODI1 - ci2$AV_DIAL_ION_SODI1

ci2 <- ci2 %>%
  mutate(groupeA.sod = case_when(
    RANDO == "A" & (SOD <= 0 | SOD > 0) ~ SOD,
    RANDO == "B" & (SOD1 <= 0 | SOD1 > 0) ~ SOD1
  ))
quantile(ci2$groupeA.sod)

ci2 <- ci2 %>% 
  mutate(groupeB.sod = case_when(
    RANDO == "A" & (SOD1 <= 0 | SOD1 > 0) ~ SOD1,
    RANDO == "B" & (SOD <= 0 | SOD > 0) ~ SOD
  ))
quantile(ci2$groupeB.sod)

wilcox.test(ci2$groupeA.sod, ci2$groupeB.sod, exact = F, paired = T)

#------------------------------------------------------------------------------------------

ci2$AV_DIAL_ION_PHOS = as.numeric(as.character(ci2$AV_DIAL_ION_PHOS))
ci2$AP_DIAL_ION_PHOS = as.numeric(as.character(ci2$AP_DIAL_ION_PHOS))
ci2$PHOS = ci2$AP_DIAL_ION_PHOS - ci2$AV_DIAL_ION_PHOS

ci2$AV_DIAL_ION_PHOS1 = as.numeric(as.character(ci2$AV_DIAL_ION_PHOS1))
ci2$AP_DIAL_ION_PHOS1 = as.numeric(as.character(ci2$AP_DIAL_ION_PHOS1))
ci2$PHOS1 = ci2$AP_DIAL_ION_PHOS1 - ci2$AV_DIAL_ION_PHOS1

ci2 <- ci2 %>%
  mutate(groupeA.phos = case_when(
    RANDO == "A" & (PHOS <= 0 | PHOS > 0) ~ PHOS,
    RANDO == "B" & (PHOS1 <= 0 | PHOS1 > 0)  ~ PHOS1
  ))
quantile(ci2$groupeA.phos)

ci2 <- ci2 %>%
  mutate(groupeB.phos = case_when(
    RANDO == "A"  & (PHOS1 <= 0 | PHOS1 > 0) ~ PHOS1,
    RANDO == "B" & (PHOS <= 0 | PHOS > 0) ~ PHOS
  ))
quantile(ci2$groupeB.phos)

wilcox.test(ci2$groupeA.phos, ci2$groupeB.phos, exact = F, paired = T)

#------------------------------------------------------------------------------------------
# BETA DANS LE DIALYSAT

ci2$PRLVMNT_BETA = as.numeric(as.character(ci2$PRLVMNT_BETA))
ci2$PRLVMNT_BETA1 = as.numeric(as.character(ci2$PRLVMNT_BETA1))

ci2 <- ci2 %>%
  mutate(groupeA.betad = case_when(
    RANDO == "A" & (PRLVMNT_BETA <= 0 | PRLVMNT_BETA > 0) ~ PRLVMNT_BETA,
    RANDO == "B" & (PRLVMNT_BETA1 <= 0 | PRLVMNT_BETA1 > 0) ~ PRLVMNT_BETA1
  ))
quantile(ci2$groupeA.betad)

ci2 <- ci2 %>%
  mutate(groupeB.betad = case_when(
    RANDO == "A" & (PRLVMNT_BETA1 <= 0 | PRLVMNT_BETA1 > 0) ~ PRLVMNT_BETA1,
    RANDO == "B" & (PRLVMNT_BETA <= 0 | PRLVMNT_BETA > 0) ~ PRLVMNT_BETA
  ))
quantile(ci2$groupeB.betad)

wilcox.test(ci2$groupeA.betad, ci2$groupeB.betad, exact = F, paired = T)

#------------------------------------------------------------------------------------------
# MICRO ALB DIALYSAT

ci2$PRLVMNT_ALBU = as.numeric(as.character(ci2$PRLVMNT_ALBU))
ci2$PRLVMNT_ALBU1 = as.numeric(as.character(ci2$PRLVMNT_ALBU1))

ci2 <- ci2 %>%
  mutate(groupeA.alb = case_when(
    RANDO == "A" & (PRLVMNT_ALBU <= 0 | PRLVMNT_ALBU > 0) ~ PRLVMNT_ALBU,
    RANDO == "B" & (PRLVMNT_ALBU1 <= 0 | PRLVMNT_ALBU1 > 0) ~ PRLVMNT_ALBU1
  ))
quantile(ci2$groupeA.alb, na.rm = T)

ci2 <- ci2 %>%
  mutate(groupeB.alb = case_when(
    RANDO == "A" & (PRLVMNT_ALBU1 <= 0 | PRLVMNT_ALBU1 > 0) ~ PRLVMNT_ALBU1,
    RANDO == "B" & (PRLVMNT_ALBU <= 0 | PRLVMNT_ALBU > 0) ~ PRLVMNT_ALBU
  ))
quantile(ci2$groupeB.alb, na.rm = T)

wilcox.test(ci2$groupeA.alb, ci2$groupeB.alb, exact = F, paired = T)

#------------------------------------------------------------------------------------------
# UREE
ci2$PRLVMNT_UREE = as.numeric(as.character(ci2$PRLVMNT_UREE))
ci2$PRLVMNT_UREE1 = as.numeric(as.character(ci2$PRLVMNT_UREE1))

ci2 <- ci2 %>%
  mutate(groupeA.ur = case_when(
    RANDO == "A" & (PRLVMNT_UREE <= 0 | PRLVMNT_UREE > 0) ~ PRLVMNT_UREE, 
    RANDO == "B" & (PRLVMNT_UREE1 <= 0 | PRLVMNT_UREE1 > 0) ~ PRLVMNT_UREE1
  ))
quantile(ci2$groupeA.ur)

ci2 <- ci2 %>%
  mutate(groupeB.ur = case_when(
    RANDO == "A" & (PRLVMNT_UREE1 <= 0 | PRLVMNT_UREE1 > 0) ~ PRLVMNT_UREE1,
    RANDO == "B" & (PRLVMNT_UREE <= 0 | PRLVMNT_UREE > 0) ~ PRLVMNT_UREE
  ))
quantile(ci2$groupeB.ur)

wilcox.test(ci2$groupeA.ur, ci2$groupeB.ur, exact = F, paired = T)

#------------------------------------------------------------------------------------------
# SODIUM

ci2$PRLVMNT_SODI = as.numeric(as.character(ci2$PRLVMNT_SODI))
ci2$PRLVMNT_SODI1 = as.numeric(as.character(ci2$PRLVMNT_SODI1))

ci2 <- ci2 %>%
  mutate(groupeA.so = case_when(
    RANDO == "A" & (PRLVMNT_SODI <= 0 | PRLVMNT_SODI > 0) ~ PRLVMNT_SODI,
    RANDO == "B" & (PRLVMNT_SODI1 <= 0 | PRLVMNT_SODI1 > 0) ~ PRLVMNT_SODI1
  ))
quantile(ci2$groupeA.so)

ci2 <- ci2 %>%
  mutate(groupeB.so = case_when(
    RANDO == "A" & (PRLVMNT_SODI1 <= 0 | PRLVMNT_SODI1 > 0) ~ PRLVMNT_SODI1,
    RANDO == "B" & (PRLVMNT_SODI <= 0 | PRLVMNT_SODI > 0) ~ PRLVMNT_SODI
  ))
quantile(ci2$groupeB.so)

wilcox.test(ci2$groupeA.so, ci2$groupeB.so, exact = F, paired = T)
