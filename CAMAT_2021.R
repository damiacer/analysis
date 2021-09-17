getwd()
setwd("P:/CONSULTATION/CAMAT") # ON PC

################################################################################

# PACKAGES

#install.packages("readxl")
library("readxl")
#install.packages("tidyverse")
library("tidyverse")

################################################################################

# DATA

camat <- read_excel("database.xlsx", na=".")

names(camat)
View(camat)
table(camat$groupe)

################################################################################

# Critère de jugement principal :
# Comparaison du delta du score à l'inclusion (T0) et après 6 mois de prise 
# en charge (T1) à l'inventaire d'alliance de travail (de Roten, 2006) pour le 
# groupe case management comparativement au groupe prise en charge 
# infirmière traditionnelle.

# Critère de jugement Secondaire :
# Scores aux échelles suivantes : Birchwood Insight Scale (Jaafari et al., 2007), 
# The Scale to assess Unawareness of Mental Disorder (Jaafari et al., 2010), 
# Echelle de bien-être au travail (Biétry et Creusier, 2013)
# Corrélations entre les scores à l'inventaire d'alliance de travail (de Roten, 2006)
# et les échelles suivantes : Birchwood Insight Scale (Jaafari et al., 2007), 
# The Scale to assess Unawareness of Mental Disorder (Jaafari et al., 2010), 
# Echelle de bien-être au travail (Biétry et Creusier, 2013)

################################################################################

# VARIABLE TRANSFORMATION

nf <- function(x){
  result = as.numeric(as.character(x))
  return(result)
}

################################################################################

install.packages("tableone")
library("tableone")

camatvars = c("groupe",
              "age",
              "m6_age",
              "antecedents_fam",
              "duree_mal_annee",
              "nb_hospit" ,
              "antipsychotique",
              "chlorpromazine_equivalent_mg_j",
              "m6_antipsychotique",
              "m6_chlorpromazine_equivalent_mg_j",
              "lieu_vie",
              "statut_marital",
              "niveau_etudes",
              "profession",
              "m6_profession",
              "posneg_P",
              "posneg_N",
              "posneg_G",
              "posneg_T",
              "m6_posneg_P",
              "m6_posneg_N",
              "m6_posneg_G",
              "m6_posneg_T",
              "sumd_conscient_ca",
              "sumd_conscient_cp",
              "sumd_conscient_aa",
              "sumd_conscient_ap",
              "sumd_conscient_na",
              "sumd_conscient_np",
              "m6_sumd_conscient_ca",
              "m6_sumd_conscient_cp",
              "m6_sumd_conscient_aa",
              "m6_sumd_conscient_ap",
              "m6_sumd_conscient_na",
              "m6_sumd_conscient_np",
              "sumd_partconscien_ca",
              "sumd_partconscien_cp",
              "sumd_partconscien_aa",
              "sumd_partconscien_ap",
              "sumd_partconscien_na",
              "sumd_partconscien_np",
              "m6_sumd_partconscien_ca",
              "m6_sumd_partconscien_cp",
              "m6_sumd_partconscien_aa",
              "m6_sumd_partconscien_ap",
              "m6_sumd_partconscien_na",
              "m6_sumd_partconscien_np",
              "sumd_inc_ca",
              "sumd_inc_cp",
              "sumd_inc_aa",
              "sumd_inc_ap",
              "sumd_inc_na",
              "sumd_inc_np",
              "m6_sumd_inc_ca",
              "m6_sumd_inc_cp",
              "m6_sumd_inc_aa",
              "m6_sumd_inc_ap",
              "m6_sumd_inc_na",
              "m6_sumd_inc_np",
              "sumd_ratio_ca",
              "sumd_ratio_cp",
              "sumd_ratio_aa",
              "sumd_ratio_ap",
              "sumd_ratio_na",
              "sumd_ratio_np",
              "m6_sumd_ratio_ca",
              "m6_sumd_ratio_cp",
              "m6_sumd_ratio_aa",
              "m6_sumd_ratio_ap",
              "m6_sumd_ratio_na",
              "m6_sumd_ratio_np",
              "sumd_somme",
              "m6_sumd_somme",
              "medadhe",
              "m6_medadhe",
              "invenalliance",
              "m6_invenalliance",
              "birch",
              "m6_birch",
              "bientravail",
              "m6_bientravail"   
              )

camatcat = c("groupe",
              "age",
              "antecedents_fam",
              "antipsychotique",
              "m6_antipsychotique",
              "lieu_vie",
              "statut_marital",
              "niveau_etudes",
              "profession",
              "m6_profession"
)

camat_descriptive = CreateTableOne(vars = camatvars, data = camat, factorVars = camatcat)
print(camat_descriptive, showAllLevels = TRUE, quote = TRUE, noSpaces = TRUE)
