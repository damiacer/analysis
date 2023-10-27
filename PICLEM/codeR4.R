# FOLDER

getwd()
setwd("P:/CONSULTATION/Pinot_Clemence") # On PC

# PACKAGES
library("readxl")

install.packages("tableone")
library("tableone")

install.packages("desctable")
library("desctable")

# DATA
cp <- read_excel("CP_data.xlsx", na=".")

names(cp)

#################################################################

# TABLEONE
CreateTableOne(data = cp) 

# VARIABLE NAMES
dput(names(cp))

# VECTORS OF VARIABLES TO SUMMARIZE
# ALL VARIABLES
myVars <- 	c("ID", "decedevivant", "datedec_missing", "lieu_deces", 
"Deces a domicile", "accord_fiche", "hosp_noprogram", "hosp_noprogrambinaire", 
"fiche_souhaithosp", "motifhosp", "service", "urgences", "fiche_litrepliprevu", 
"fiche_litrepliprevubinaire", "dureehospit", "rea_usci", "sexe", "fiche_ageredaction", 
"Situation", "redacteur", 
"lieudevie", "department", "joignablenuit", "medhospitreferent", 
"servicehospitreferent", "litsderepli", "litrepli_service", "structures_suivi1", 
"structures_suivi2", "structures_suivi3", "intervenants_domicile", 
"intervenants_domicile2", "pathologie", "Pathologie_code", "connaissancediagn", 
"connaissanceprogn", "connaissancediag_entourage", "connaissanceprogn_entourage", 
"reflexioncollegiale", "projettherap", "symptassocies", "prescranticipees", 
"prescranticipees_binaire", "demarcheprevu_accordpatient", "hospit", 
"soinsreconfortexclu", "decesadomicile", "reanimation_cas", "sedation_progvitalengage", 
"directivesanticip", "personnedeconfiance", "precision") 

# ONLY CATEGORIAL VARIABLES THAT NEED TRANSFORMATION
catVars <- c("ID", "decedevivant", "datedec_missing", "lieu_deces", 
"Deces a domicile", "accord_fiche", "hosp_noprogram", "hosp_noprogrambinaire", 
"fiche_souhaithosp", "motifhosp", "service", "urgences", "fiche_litrepliprevu", 
"fiche_litrepliprevubinaire", "dureehospit", "rea_usci", "sexe", "fiche_ageredaction", 
"Situation", "redacteur", 
"lieudevie", "department", "joignablenuit", "medhospitreferent", 
"servicehospitreferent", "litsderepli", "litrepli_service", "structures_suivi1", 
"structures_suivi2", "structures_suivi3", "intervenants_domicile", 
"intervenants_domicile2", "pathologie", "Pathologie_code", "connaissancediagn", 
"connaissanceprogn", "connaissancediag_entourage", "connaissanceprogn_entourage", 
"reflexioncollegiale", "projettherap", "symptassocies", "prescranticipees", 
"prescranticipees_binaire", "demarcheprevu_accordpatient", "hospit", 
"soinsreconfortexclu", "decesadomicile", "reanimation_cas", "sedation_progvitalengage", 
"directivesanticip", "personnedeconfiance", "precision") 

# CREATE A TABLEONE OBJECT
tab1 <- CreateTableOne(vars = myVars, data = cp, factorVars = catVars)

# SHOW ALL LEVELS OF A CATEGORIAL VARIABLE 
print(tab1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

# CREATE A TABLE BY STRATIFICATION
# ADD A TEST (FALSE TO AVOID TESTING) 
# ADD STRATA TO CREATE A UNIVARIATE ANALYSIS

tab2 <- CreateTableOne(vars = myVars, data = cp, factorVars = catVars, test = TRUE, strata = "XXXXXX")

# SHOW ALL LEVELS BY STRATA
print(tab2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
