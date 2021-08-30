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
  "Nombre_hematomes" = "Nombre_d'hematomes",
  "Taille_hematome_grand_axe_mm" = "Taille_de_l'hematome_grand_axe,_mm"
  )

################################################################################

# DECES BINAIRE

table(hem$DATE_DC)
str(hem$DATE_DC)

hem$DATE_DCNUM = as.numeric(hem$DATE_DC)
hem$DATE_DCC = as.factor(hem$DATE_DCNUM)

hd <- as.data.frame(hem$DATE_DCNUM)
  is.data.frame(hd)
  # View(hd)
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
	"Taille_de_l'hematome_grand_axe_mm",
	"Surface_de_l'hematome_cm2",
	"Volume_de_l'hematome_cm3",
	"Fuite_arterielle",
	"Fuite_portale",
	"Nombre_de_fuites",
	"Hematome_rompu",
	"Embolisation",
	"Fuite_arteriographique",
	"Recidive_hemorragie_apres_embolisation",
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
	"Recidive_hemorragie_apres_embolisation",
	"Deces_inf1_mois",
	"deces01")

# CREATE THE DESCRIPTIVE TABLE TABLE  
tab1 = CreateTableOne(vars = variables, data = hem, factorVars = categorical) 
print(tab1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)  

# CREATE THE UNIVARIATE TABLE 
tab2 = CreateTableOne(vars = variables, data = hem, factorVars = categorical, test = TRUE,  
                      includeNA = FALSE, strata = "deces01")
print(tab2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)  

################################################################################

# RECODAGE DES VARIABLES

hem$Indication_anticoagulant[hem$Indication_anticoagulant == "ACFA"] <- "ACFA"
hem$Indication_anticoagulant[hem$Indication_anticoagulant == "MTEV"] <- "MTEV"
hem$Indication_anticoagulant[hem$Indication_anticoagulant == "VALVE"] <- "VALVE"
hem$Indication_anticoagulant[hem$Indication_anticoagulant == "ACFA+MTEV"] <- "autre"
hem$Indication_anticoagulant[hem$Indication_anticoagulant == "ACFA+VALVE"] <- "autre"
hem$Indication_anticoagulant[hem$Indication_anticoagulant == "autre"] <- "autre"
table(hem$Indication_anticoagulant)

anticotab = table(hem$Indication_anticoagulant, hem$deces01)
prop.table(anticotab, margin = 2)

hem$Nombre_hematomes = as.factor(hem$Nombre_hematomes)

hem$Comorbidite[hem$Comorbidite == "cancer_sauf_remission"] <- "autre"
hem$Comorbidite[hem$Comorbidite == "cardiopathie_autre"] <- "cardiopathie_autre"
hem$Comorbidite[hem$Comorbidite == "cardiopathie_rythmique"] <- "cardiopathie_rythmique"
hem$Comorbidite[hem$Comorbidite == "diabete"] <- "autre"
hem$Comorbidite[hem$Comorbidite == "hemopathie_maligne"] <- "autre"
table(hem$Comorbidite)

comtab = table(hem$Comorbidite, hem$deces01)
prop.table(comtab, margin = 2)

hem$Nombre_de_fuites2[hem$Nombre_de_fuites == "1"] <- "1"
hem$Nombre_de_fuites2[hem$Nombre_de_fuites == "X"] <- ""
hem$Nombre_de_fuites2[hem$Nombre_de_fuites == "2"] <- "2"
hem$Nombre_de_fuites2[hem$Nombre_de_fuites == "3"] <- "3"
hem$Nombre_de_fuites2[hem$Nombre_de_fuites == "4"] <- "4"
hem$Nombre_de_fuites2[hem$Nombre_de_fuites == "5"] <- "5"
hem$Nombre_de_fuites2[hem$Nombre_de_fuites == "6"] <- "6"
table(hem$Nombre_de_fuites2)
hem$Nombre_de_fuites2 = as.numeric(as.character(hem$Nombre_de_fuites2))
str(hem$Nombre_de_fuites2)
hem$Nombre_de_fuites01 = ifelse(hem$Nombre_de_fuites2 == 1, "1", "2")

fuitab = table(hem$Nombre_de_fuites01, hem$deces01)
prop.table(fuitab, margin = 2)

################################################################################

# FOLLOW-UP

a = as.Date(hem$Date_des_dernieres_nouvelles) - as.Date(hem$DSI)
b = as.Date(hem$DATE_DC) - as.Date(hem$DSI)
hem$fup = ifelse(hem$deces01 == "1", b, a)

table(hem$fup)
min(hem$fup, na.rm=T)

hem$fuppos = ifelse(hem$fup < 0, "", hem$fup)
hem$fuppos = as.numeric(as.character(hem$fuppos))

#-------------------------------------------------------------------------------

# NEW DATABASE WITH ONLY COMPLETE DATA
# more on drop: https://blog.rstudio.com/2016/08/15/tidyr-0-6-0/
require(dplyr)

hemna <- hem %>% drop_na(fuppos)
count(hemna) # 203
count(hem) #224

################################################################################

# KAPLAN-MEIER

require("survival")

hemna$status = hemna$deces01
hemna$time = hemna$fuppos

km <- with(hemna, Surv(time, status))
plot(km, xscale = 365.25)


# install.packages("ggfortify")
library("ggfortify")

km_fit <- survfit(Surv(time, status) ~ 1, data = hemna)
summary(km_fit, times = c(1,100,200,500*(1:10)))

autoplot(km_fit, surv.linetype = "dashed", surv.colour = "orange", 
         censor.colour = "red", conf.int = "TRUE", censor.shape = "*")

################################################################################

# SURVIVAL ANALYSIS

# install.packages("My.stepwise")
require("My.stepwise")

time = hemna$fup
status = hemna$deces01

var.list = c("Sexe",
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
             "Taille_de_l'hematome_grand_axe_mm",
             "Surface_de_l'hematome_cm2",
             "Volume_de_l'hematome_cm3",
             "Fuite_arterielle",
             "Fuite_portale",
             "Nombre_de_fuites",
             "Hematome_rompu",
             "Embolisation",
             "Fuite_arteriographique",
             "Recidive_hemorragie_apres_embolisation",
             "Deces_inf1_mois")

My.stepwise.coxph(Time = "time", Status = "status", variable.list = var.list, 
                  data = hemna, sle = 0.25, sls = 0.25)
