getwd()
setwd("P:/CONSULTATION/Pelage_JeanPierre") # ON PC
setwd("/Users/damianocerasuolo/Desktop/UBRC/2021_CONSULT/Pelage_JeanPierre") # ON MAC

################################################################################

# PACKAGES

#install.packages("readxl")
library("readxl")
#install.packages("tidyverse")
library("tidyverse")

################################################################################

# DATA

hem <- read_excel("hematome_DC.xlsx", na="NA")

names(hem)

hem <- as_tibble(hem)
hem <- hem %>% rename(
  # new name = old name,
  "TP_J0_perc" = "TP_J0_%",
  "Nombre_hematomes" = "Nombre_d'hematomes"
  )

################################################################################

# DECES BINAIRE

table(hem$DATE_DC)
str(hem$DATE_DC)

hem$DATE_DCNUM = as.numeric(hem$DATE_DC)
hem$DATE_DCC = as.factor(hem$DATE_DCNUM)

hd <- as.data.frame(hem$DATE_DCNUM)
  is.data.frame(hd)
  View(hd)
  is.na(hd)
hd[is.na(hd)] <- 0
table(hd)
hem$DATE_DCNUM <- hd
hem$deces01 = ifelse(hem$DATE_DCNUM == 0, 0, 1) # 0=pas de deces, 1=deces
table(hem$deces01)
tabedeces <- table(hem$deces01)
prop.table(tabedeces)

################################################################################

install.packages("tableone")
library("tableone")  
dput(names(hem)) 

variables = c("Sexe",
	"Taille_cm",
	"Poids_kg",
	"BMI",
	"HTA",
	"Comorbidite",
	"Autre_comorbidite",
	"Cardiopathie",
	"Diabete",
	"Hb_J0_gdL",
	"Anemie",
	"Plaquettes_J0_GL",
	"DFG_mLmin",
	"Indication_anticoagulant",
	"Indication_anticoagulation_eq_trouble_du_rythme_cardiaque",
	"Indication_anticoagulation_eq_MTEV",
	"Indication_anticoagulation_eq_valve_mecanique",
	"Indication_anticoagulation_eq_autre",
	"TP_J0_perc",
	"TCAr_J0",
	"INR_J0",
	"Anti_Xa_J0_UImL",
	"Surdosage_anticoagulant",
	"Antiaggregant_plaquettaire",
	"Helice_apres_injection_temps_arteriel",
	"Helice_apres_injection_temps_portal", 
	"Nombre_hematomes",
	"Localisation",
	"Taille_de_l'hematome_grand_axe,_mm",
	"Surface_de_l'hematome_cm2",
	"Volume_de_l'hematome_cm3",
	"Fuite_arterielle",
	"Fuite_portale",
	"Nombre_de_fuites",
	"Hematome_rompu",
	"Embolisation",
	"Fuite_arteriographique",
	"Recidive_de_l'hemorragie_apres_embolisation",
	"Deces_inf1_mois",
	"deces01") 

categorical = c("Sexe",
	"HTA",
	"Comorbidite",
	"Autre_comorbidite",
	"Cardiopathie",
	"Diabete",
	"Anemie",
	"Indication_anticoagulant",
	"Indication_anticoagulation_eq_trouble_du_rythme_cardiaque",
	"Indication_anticoagulation_eq_MTEV",
	"Indication_anticoagulation_eq_valve_mecanique",
	"Indication_anticoagulation_eq_autre",
	"Surdosage_anticoagulant",
	"Antiaggregant_plaquettaire",
	"Helice_apres_injection_temps_arteriel",
	"Helice_apres_injection_temps_portal", 
	"Nombre_hematomes",
	"Localisation",
	"Fuite_arterielle",
	"Fuite_portale",
	"Nombre_de_fuites",
	"Hematome_rompu",
	"Embolisation",
	"DATE_embolisation",
	"Fuite_arteriographique",
	"Recidive_de_l'hemorragie_apres_embolisation",
	"Deces_inf1_mois",
	"deces01")

# CREATE THE DESCRIPTIVE TABLE TABLE  
tab1 = CreateTableOne(vars = variables, data = hem, factorVars = categorical) 
print(tab1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)  

# CREATE THE UNIVARIATE TABLE 
tab2 = CreateTableOne(vars = variables, data = hem, factorVars = categorical, test = TRUE,  
                      includeNA = FALSE, strata = "deces01")
print(tab2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)  
