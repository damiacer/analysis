# packages----------------------------------------------------------------------

require("readxl")
require("tableone")

# data--------------------------------------------------------------------------

getwd()
setwd("P:/CONSULTATION/VRS60")

vrs <- read_xlsx("VRS60_NDU_vA.xlsx")
names(vrs)
View(vrs)

# DP variable-------------------------------------------------------------------

table(vrs$DP)

# 1440
# 19601

vrs <- vres %>%
  mutate(DP.short = case_when(
    DP == "A090" ~ "Gastroent_colites", 
    DP == "A402" ~ "Sepsis", 
    DP ==  "A415" | DP == "A419" ~ "Sepsis",
    DP == "A46" ~ "Erysipele",
    DP == "A481" | DP == "A490" ~ "Autres_bacteriennes",
    DP == "B338" | DP == "B348" | DP == "B349" ~ "Autres_virales", 
    DP == "C434" ~ "Melanome_autres",
    DP == "C61" ~ "Prostate",
    DP == "C679" ~ "Vessie",
    DP == "C793" ~ "Autres_tumeurs"
    DP == "C825" ~ "Lymphome folliculaire",
    DP == "C920" ~ "Leucemie_myeloide",
    DP == "D135" ~ "Tumeurben_digestif",
    DP == "D180" ~ "Hemang-lymphang",
    DP == "D500" | DP == "D528" ~ "Anemie",
    DP == "D618" ~ "Apalsies_autres",
    DP == "D70" ~ "Agranulocytose",
    DP == "E109" | DP == "E1120" | DP == "E1140" ~ "Diabete"
    DP == "E86" ~ "Hypovolemie",
    DP == "E8710" | DP == "E8718" | DP == "E872" ~ "deseq_hydroelyacidobas",
    DP == "F03"  | DP == "F058" | DP == "F078" ~ "Trmentaux_orga",
    DP == "G459" ~ "AIT",
    DP == "H492" ~ "Affections_oculaires", 
    DP == "H602" ~ "Otite",
    DP == ""
    
  ))

# descriptive-------------------------------------------------------------------

dput(names(vrs))

vars <- c("age", "sexe", 
          "type_ech", "autre_ech", 
          "service_preleveur", "Autre_service", "Typage_VRS", "Technique", 
          "autre_technique", "coinfection_1", "coinfection_2", "coinfection_3", 
          "coinfection_4", "date_entree", "mode_entree", "passage_urgences", 
          "duree_urgences", "urgences", "motif_admission", "motif_admission2", 
          "DP", "DAS",  
          "codage_VRS", "maladie_respi_chronique", "autre_respi", "maladie_card_chronique", 
          "maladie_renale_chronique", "diabete", "immunodepression", "oxygenorequerance", 
          "vaccin_grippe", "vaccin_Cov2", "vaccin_pneumo", "taille", "poids", 
          "O2", "deces", "ATB", "cortico", "antipyretiques", 
          "bronchodilatateurs", "service_sortie", "autre_sortie", 
          "mode_sortie", "readmission")