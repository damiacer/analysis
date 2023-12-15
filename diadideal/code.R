#-WORKSPACE----
getwd()
setwd("/Users/damianocerasuolo/Desktop/UBRC/UBRS_CONSULT_MAC/DIADIDEAL")

#-PACKAGES----

#install.packages("readxl")
require("readxl")

dd <- read_excel("DIADIDEAL_Export_final_global_2023-11-20.xlsx")
names(dd)

table(dd$MED_IDEL_ARRET_V11_M12, useNA = "always")
# 1 deces 
# 1 repli def pour mauvaise compliace au ttt 
# 1 transfer en HDD --> toutes les données 
# 2 commence jamais HDD 
table(dd$MED_IDEL_ARRET_)

# nb seances totales sur les 12 mois 

table(dd$MED_SEANCE_NB_V1_M2, dd$MED_SEANCE_NB_V2_M3, useNA = "always")
table(dd$MED_SEANCE_NB_V2_M3, useNA = "always")
table(dd$MED_SEANCE_NB_V3_M4, useNA = "always")
table(dd$MED_SEANCE_NB_V4_M5, useNA = "always")
table(dd$MED_SEANCE_NB_V5_M6, useNA = "always")
table(dd$MED_SEANCE_NB_V6_M7, useNA = "always")
table(dd$MED_SEANCE_NB_V7_M8, useNA = "always")
table(dd$MED_SEANCE_NB_V8_M9, useNA = "always")
table(dd$MED_SEANCE_NB_V9_M10, useNA = "always")
table(dd$MED_SEANCE_NB_V10_M11, useNA = "always")
table(dd$MED_SEANCE_NB_V11_M12, useNA = "always")
RT_PROG_CAUS
table(dd$RT_PROG_CAUS_V2_M3_1)

# cause des replis non programme 
table(dd$RT_NPROG_CAUS_V2_M3_1, useNA = "always")
table(dd$RT_NPROG_SEANCE_NB_V2_M3_1, useNA = "always")

# description des pt installés (V0)
# nb de repli par patient 
# nb de seance hospitalier par patient a domicile et en centre 

# TRANSP_FRAIS

table(dd$EQ_Q01_MOBIL_V11_M12)
table(dd$MED_NEPHR_CONSULT_NB)
table(dd$MED_NEPHR_CONSULT_NB_V0_M1)
table(dd$MED_NEPHR_CONSULT_NB_V11_M12)

table(dd$MED_NEPHR_CONSULT_V11_M12, useNA = "always")
table(dd$MED_NEPHR_CONSULT_NB_V10_M11)

MED_IDEL_ARRET
table(dd$MED_IDEL_ARRET_V0_M1)
table(dd$MED_IDEL_ARRET_V11_M12)

# formation 
# nb de séances de formation 
# duree totale de formation 
# duree moyenne 
# nb heures minutes pour le cabinet 
