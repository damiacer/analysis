getwd()
setwd("C:/Users/lafitte-r/Desktop/UBRC/GO")

library("readxl")
library("tidyverse")
library("tableone")
install.packages("lubridate")

go <- read_excel("Base_GO.xlsx")
names(go)
View(go)
dput(names(go))

c("soumission", "examenGO", "avisGO", "cotech", "examenCotech", 
  "avisCotech", "CLERS_avis1", "CLERS_avis2", "avisCLERS", "resptraitement", 
  "RGPD", "domaine", "objet", "publi", "type_questionnaire", "type_épidémio", 
  "type_essai_clinique", "type_eval_pratique", "type_prono", "type_diag", 
  "type_dépistage", "type_fdr", "type_eval_eco", "type_valid_outils", 
  "type_qol", "type_autre", "finalité", "nature", "effectif_max", 
  "centres_nb", "source_dossmed", "source_pds", "source_patients", 
  "source_autres", "info_sujet", "date_soutenance", "recherche_développement_connaissances", 
  "recherche_physio", "recherche_sécurité_traitement", "intervention_non_justifiee", 
  "intervention_contraintes_minimes", "intervention_aucun_risque", 
  "recherche_données")

--------------------------------------------------------------------------------------
  
d <- function(x){
  result = as.Date(as.character(x))
  return(result)
}

go$soumission = d(go$soumission)
go$examenGO = d(go$examenGO)
go$date_soutenance= d(go$date_soutenance)

go$examenCotech = d(go$examenCotech)

-------------------------------------------------------------------------------------
  
n <- function(x){
  result = as.numeric(as.character(x))
  return(result)
}

go$effectif_max = n(go$effectif_max)
go$centres_nb = n(go$centres_nb)

--------------------------------------------------------------------------------------
  
f <- function(x){
  result = as.factor(x)
  return(result)
}

go$avisGO= f(go$avisGO)
go$cotech= f(go$cotech)
go$avisCotech= f(go$avisCotech)
go$avisCLERS= f(go$avisCLERS)
go$CLERS_avis1= f(go$CLERS_avis1)
go$CLERS_avis2= f(go$CLERS_avis2)
go$avisCLERS= f(go$avisCLERS)
go$resptraitement= f(go$resptraitement)
go$RGPD= f(go$RGPD)
go$domaine= f(go$domaine)
go$objet= f(go$objet)
go$publi= f(go$publi)
go$type_questionnaire= f(go$type_questionnaire)
go$type_épidémio= f(go$type_épidémio)
go$type_essai_clinique= f(go$type_essai_clinique)
go$type_eval_pratique= f(go$type_eval_pratique)
go$type_prono= f(go$type_prono)
go$type_diag= f(go$type_diag)
go$type_dépistage= f(go$type_dépistage)
go$type_fdr= f(go$type_fdr)
go$type_eval_eco= f(go$type_eval_eco)
go$type_valid_outils= f(go$type_valid_outils)
go$type_qol= f(go$type_qol)
go$type_autre= f(go$type_autre)
go$finalité = f(go$finalité)
go$nature = f(go$nature)
go$source_dossmed = f(go$source_dossmed)
go$source_pds = f(go$source_pds)
go$source_patients = f(go$source_patients)
go$source_autres = f(go$source_autres)
go$info_sujet = f(go$info_sujet)
go$recherche_développement_connaissances = f(go$recherche_développement_connaissances)
go$recherche_physio = f(go$recherche_physio)
go$recherche_sécurité_traitement = f(go$recherche_sécurité_traitement)
go$intervention_non_justifiee = f(go$intervention_non_justifiee)
go$intervention_contraintes_minimes = f(go$intervention_contraintes_minimes)
go$intervention_aucun_risque = f(go$intervention_aucun_risque)
go$recherche_données = f(go$recherche_données)

-------------------------------------------------------------------------------
table(go$avisGO)
table(go$cotech)
table(go$avisCotech)
table(go$avisCLERS)

go <- go %>%
  mutate(CLERS = case_when(
    avisCLERS == "favorable" ~ "FAVORABLE"
    avisCLERS == "FAVORABLE  SOUS RESERVE DE CONFORMITE" ~ "FAVORABLE SOUS RESERVES"
    avisCLERS == "FAVORABLE, SOUS RESERVE DE CONFORMITE (à préciser) ~ "FAVORABLE SOUS RESERVES"
    
  ))
