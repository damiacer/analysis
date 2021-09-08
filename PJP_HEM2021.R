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
  "Taille_de_hematome_grand_axe_mm" = "Taille_de_l'hematome_grand_axe,_mm",
  "Surface_hematome_cm2" = "Surface_de_l'hematome_cm2",
  "Volume_hematome_cm3" = "Volume_de_l'hematome_cm3",
  "Recidive_hemorragie_apres_embolisation" = "Recidive_de_l'hemorragie_apres_embolisation"
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
              "Localisation2",
              "Taille_de_hematome_grand_axe_mm",
              "Surface_hematome_cm2",
              "Volume_hematome_cm3",
              "Fuite_arterielle2",
              "Fuite_portale",
              "Nombre_de_fuites2",
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
                "Localisation2",
                "Fuite_arterielle2",
                "Fuite_portale",
                "Nombre_de_fuites2",
                "Hematome_rompu",
                "Embolisation",
                "DATE_embolisation",
                "Fuite_arteriographique2",
                "Recidive_hemorragie_apres_embolisation",
                "Deces_inf1_mois",
                "deces01")

# CREATE THE DESCRIPTIVE TABLE TABLE  
tab1 = CreateTableOne(vars = variables, data = hem, factorVars = categorical) 
print(tab1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)  

# CREATE THE UNIVARIATE TABLE 
tab2 = CreateTableOne(vars = variables, data = hemna, factorVars = categorical, test = FALSE,  
                      includeNA = FALSE, strata = "deces01")
print(tab2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)  

tab3 = CreateTableOne(vars = variables, data = hem, factorVars = categorical, test = TRUE,  
                      includeNA = FALSE, strata = "Embolisation")
print(tab3, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)  

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
table(hem$Nombre_de_fuites01)

fuitab = table(hem$Nombre_de_fuites01, hem$deces01)
prop.table(fuitab, margin = 2)

table(hem$Localisation)
hem$Localisation2[hem$Localisation == "ANT"] <- "ANT"
hem$Localisation2[hem$Localisation == "CUISSE"] <- "CUISSEFESSE"
hem$Localisation2[hem$Localisation == "FESSE"] <- "CUISSEFESSE"
hem$Localisation2[hem$Localisation == "POST"] <- "POST"
table(hem$Localisation2)

table(hem$Fuite_arterielle)
hem$Fuite_arterielle2[hem$Fuite_arterielle == "0"] <- "0"
hem$Fuite_arterielle2[hem$Fuite_arterielle == "1"] <- "1"
hem$Fuite_arterielle2[hem$Fuite_arterielle == "X"] <- "."
hem$Fuite_arterielle2 = as.numeric(as.character(hem$Fuite_arterielle2))
table(hem$Fuite_arterielle2)
hem$Fuite_arterielle2 = as.factor(hem$Fuite_arterielle2)

table(hem$Fuite_arteriographique)
hem$Fuite_arteriographique2[hem$Fuite_arteriographique == "1"] <- "1"
hem$Fuite_arteriographique2[hem$Fuite_arteriographique == "0"] <- "0"
hem$Fuite_arteriographique2[hem$Fuite_arteriographique == "X"] <- "."
hem$Fuite_arteriographique2  = as.numeric(as.character(hem$Fuite_arteriographique2))
hem$Fuite_arteriographique2 = as.factor(hem$Fuite_arteriographique2)

hemna$Fuite_portale2[hemna$Fuite_portale == "0"] <- "0"
hemna$Fuite_portale2[hemna$Fuite_portale == "1"] <- "1"
hemna$Fuite_portale2[hemna$Fuite_portale == "X"] <- "X"
table(hemna$Fuite_portale2, hemna$deces01)

################################################################################

# FOLLOW-UP

hem$deces01 = as.factor(hem$deces01)

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
count(hemna) # 202
count(hem) #224

################################################################################

# KAPLAN-MEIER

require("survival")

# STATUS
hemna$deces12[hemna$deces01 == "1"] <- "2"
hemna$deces12[hemna$deces01 == "0"] <- "1"
table(hemna$deces12)
hemna$status = as.factor(hemna$deces12)
str(hemna$status)

# TIME
hemna$time = hemna$fuppos
str(hemna$time)

#-------------------------------------------------------------------------------

km <- with(hemna, Surv(time, status))
plot(km, xscale = 365.25)


install.packages("ggfortify")
library("ggfortify")

km_fit <- survfit(Surv(time, status) ~ 1, data = hemna)
plot(km_fit)
summary(km_fit, times = c(1,100,200,500*(1:10)))

autoplot(km_fit, surv.linetype = "dashed", surv.colour = "orange", 
         censor.colour = "red", conf.int = "TRUE", censor.shape = "*")

#------------------------------------------------------------------------------

survobj = with(hemna, Surv(time, status))
km.by.emb <- survfit(survobj ~ Embolisation, data = hemna, conf.type = "log-log")
plot(km.by.emb)

km_fitembo <- survfit(Surv(time, status) ~ Embolisation, data = hemna)
autoplot(km_fitembo, surv.linetype = "dashed", surv.colour = "orange", 
         censor.colour = "red", conf.int = "TRUE", censor.shape = "*")

################################################################################

# VARIABLE TRANSFORMATION FOR COX

hemna$Sexe = as.factor(hemna$Sexe)
str(hemna$Sexe)
hemna$BMI = as.numeric(hemna$BMI)
str(hemna$BMI)
hemna$Comorbidite = as.factor(hemna$Comorbidite)
str(hemna$Comorbidite)
hemna$Indication_anticoagulant = as.factor(hemna$Indication_anticoagulant) 
str(hemna$Indication_anticoagulant)
hemna$Taille_de_hematome_grand_axe_mm = as.numeric(hemna$Taille_de_hematome_grand_axe_mm)
str(hemna$Taille_de_hematome_grand_axe_mm)
hemna$Surface_hematome_cm2 = as.numeric(hemna$Surface_hematome_cm2)
str(hemna$Surface_hematome_cm2)
hemna$Volume_hematome_cm3 = as.numeric(hemna$Volume_hematome_cm3)
str(hemna$Volume_hematome_cm3)
hemna$Recidive_hemorragie_apres_embolisation = as.factor(hemna$Recidive_hemorragie_apres_embolisation)
str(hemna$Recidive_hemorragie_apres_embolisation)
hemna$Nombre_de_fuites01 = as.factor(hemna$Nombre_de_fuites01)
str(hemna$Nombre_de_fuites01)
hemna$Localisation = as.factor(hemna$Localisation)
str(hemna$Localisation)
hemna$Fuite_arterielle2 = as.factor(hemna$Fuite_arterielle2)
table(hemna$Fuite_arterielle2)
hemna$Embolisation = as.factor(hemna$Embolisation)
hemna$Hematome_rompu = as.factor(hemna$Hematome_rompu)

#------------------------------------------------------------------------------

# COX MODEL SELECTION

library(My.stepwise)

vars.list = c("Sexe",
              "BMI", 
              "Comorbidite",
              "Indication_anticoagulant",
              "Taille_de_hematome_grand_axe_mm",
              "Surface_hematome_cm2",
              "Volume_hematome_cm3",
              "Recidive_hemorragie_apres_embolisation",
              "Nombre_de_fuites01",
              "Localisation2",
              "Fuite_arterielle2",
              "Embolisation",
              "Hematome_rompu"
              #,"id"
              )

My.stepwise.coxph(Time = "time", Status = "status", variable.list = var.list,
                  #in.variable = "id",
                  data = hemna, sle = 0.15, sls = 0.15)
#Error in coxph(as.formula(paste("Surv(", Time, ", ", Status, ") ~ ", 
#paste(in.variable,  : an id statement is required for multi-state models
#an id statement is required for multi-state models

#------------------------------------------------------------------------------

# MULTIVARIATE MODEL
library(survival)
hemcox = coxph(formula = Surv(time, status) ~ 
                 Sexe + #included in the final model
                 BMI + #
                 Comorbidite + 
                 Indication_anticoagulant + #
                 Taille_de_hematome_grand_axe_mm + #
                 Surface_hematome_cm2 + #
                 Volume_hematome_cm3 + #
                 Recidive_hemorragie_apres_embolisation + #
                 Nombre_de_fuites01 + #
                 Localisation2 + 
                 Fuite_arterielle2 + #
                 Embolisation + #
                 Hematome_rompu #
               , data = hemna, id=ID)

summary(hemcox)
exp(cbind(coef(hemcox), confint(hemcox)))

#------------------------------------------------------------------------------

# TABLEONE FOR Comorbidite AND Localisation

require("tableone")

tab.variables = c("Sexe",
                  "BMI", 
                  "Comorbidite",
                  "Indication_anticoagulant",
                  "Taille_de_hematome_grand_axe_mm",
                  "Surface_hematome_cm2",
                  "Volume_hematome_cm3",
                  "Recidive_hemorragie_apres_embolisation",
                  "Nombre_de_fuites01",
                  "Localisation",
                  "Fuite_arterielle2",
                  "Embolisation",
                  "Hematome_rompu")

tab.categorial = c("Sexe",
                   "Comorbidite",
                   "Indication_anticoagulant",
                   "Recidive_hemorragie_apres_embolisation",
                   "Nombre_de_fuites01",
                   "Localisation",
                   "Fuite_arterielle2",
                   "Embolisation",
                   "Hematome_rompu")

tab.comorb = CreateTableOne(vars = tab.variables, data = hemna, 
                            factorVars = tab.categorial, test = FALSE, includeNA = TRUE,
                            strata = "Comorbidite")
print(tab.comorb, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

tab.local = CreateTableOne(vars = tab.variables, data = hemna, 
                            factorVars = tab.categorial, test = FALSE, includeNA = TRUE,
                            strata = "Localisation")
print(tab.local, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)


#------------------------------------------------------------------------------

# UNIVARIATE MODELS

hemsexe = coxph(formula = Surv(time, status) ~ Sexe, data = hemna, id=ID)
summary(hemsexe)

hembmi = coxph(formula = Surv(time, status) ~ BMI, data = hemna, id=ID)
summary(hembmi)

hemna$HTA = as.factor(hemna$HTA)
HTAcox = coxph(formula = Surv(time, status) ~ HTA, data = hemna, id=ID)
summary(HTAcox)

hemcomorbx = coxph(formula = Surv(time, status) ~ Comorbidite, data = hemna, id=ID)
summary(hemcomorbx)

str(hemna$Hb_J0_gdL)
coxHb_J0_gdL = coxph(formula = Surv(time, status) ~ Hb_J0_gdL, data = hemna, id=ID)
summary(coxHb_J0_gdL)

str(hemna$Anemie)
Anemiecox = coxph(formula = Surv(time, status) ~ Anemie, data = hemna, id=ID)
summary(Anemiecox)

str(hemna$Plaquettes_J0_GL)
coxPlaquettes_J0_GL = coxph(formula = Surv(time, status) ~ Plaquettes_J0_GL, data = hemna, id=ID)
summary(coxPlaquettes_J0_GL)

str(hemna$DFG_mLmin)
coxDFG_mLmin = coxph(formula = Surv(time, status) ~ DFG_mLmin, data = hemna, id=ID)
summary(coxDFG_mLmin)

hemantico = coxph(formula = Surv(time, status) ~ Indication_anticoagulant, data = hemna, id=ID)
summary(hemantico)

str(hemna$Indication_anticoagulation_eq_trouble_du_rythme_cardiaque)
hemna$Indication_anticoagulation_eq_trouble_du_rythme_cardiaque = as.factor(hemna$Indication_anticoagulation_eq_trouble_du_rythme_cardiaque)
ineqcox = coxph(formula = Surv(time, status) ~ Indication_anticoagulation_eq_trouble_du_rythme_cardiaque, data = hemna, id=ID)
summary(ineqcox)

str(hemna$Indication_anticoagulation_eq_MTEV)
hemna$Indication_anticoagulation_eq_MTEV = as.factor(hemna$Indication_anticoagulation_eq_MTEV)
mtevcox = coxph(formula = Surv(time, status) ~ Indication_anticoagulation_eq_MTEV, data = hemna, id=ID)
summary(mtevcox)

str(hemna$Indication_anticoagulation_eq_valve_mecanique)
hemna$Indication_anticoagulation_eq_valve_mecanique = as.factor(hemna$Indication_anticoagulation_eq_valve_mecanique)
valvecox = coxph(formula = Surv(time, status) ~ Indication_anticoagulation_eq_valve_mecanique, data = hemna, id=ID)
summary(valvecox)

str(hemna$TP_J0_perc)
tpcox = coxph(formula = Surv(time, status) ~ TP_J0_perc, data = hemna, id = ID)
summary(tpcox)

str(hemna$TCAr_J0)
tcar = coxph(formula = Surv(time, status) ~ TCAr_J0, data = hemna, id = ID)
summary(tcar)

str(hemna$INR_J0)
inrcox = coxph(formula = Surv(time, status) ~ INR_J0, data = hemna, id = ID)
summary(inrcox)

str(hemna$Surdosage_anticoagulant)
hemna$Surdosage_anticoagulant = as.factor(hemna$Surdosage_anticoagulant)
surdoscox = coxph(formula = Surv(time, status) ~ Surdosage_anticoagulant, data = hemna, id = ID)
summary(surdoscox)

str(hemna$Antiaggregant_plaquettaire )
hemna$Antiaggregant_plaquettaire  = as.factor(hemna$Antiaggregant_plaquettaire)
anticox = coxph(formula = Surv(time, status) ~ Antiaggregant_plaquettaire, data = hemna, id = ID)
summary(anticox)

str(hemna$Helice_apres_injection_temps_arteriel)
hemna$Helice_apres_injection_temps_arteriel = as.factor(hemna$Helice_apres_injection_temps_arteriel)
heliceacox = coxph(formula = Surv(time, status) ~ Helice_apres_injection_temps_arteriel, data = hemna, id = ID)
summary(heliceacox)

str(hemna$Helice_apres_injection_temps_portal)
hemna$Helice_apres_injection_temps_portal = as.factor(hemna$Helice_apres_injection_temps_portal)
helicepcox = coxph(formula = Surv(time, status) ~ Helice_apres_injection_temps_portal, data = hemna, id = ID)
summary(helicepcox)

str(hemna$Nombre_hematomes)
hemna$Nombre_hematomes = as.factor(hemna$Nombre_hematomes)
hemnbcox = coxph(formula = Surv(time, status) ~ Nombre_hematomes, data = hemna, id = ID)
summary(hemnbcox)

hemaxe = coxph(formula = Surv(time, status) ~ Taille_de_hematome_grand_axe_mm, data = hemna, id=ID)
summary(hemaxe)

hemsurf = coxph(formula = Surv(time, status) ~ Surface_hematome_cm2, data = hemna, id=ID)
summary(hemsurf)

hemvolum = coxph(formula = Surv(time, status) ~ Volume_hematome_cm3, data = hemna, id=ID)
summary(hemvolum)

hemreci = coxph(formula = Surv(time, status) ~ Recidive_hemorragie_apres_embolisation, data = hemna, id=ID)
summary(hemreci)

hemnbfuit = coxph(formula = Surv(time, status) ~ Nombre_de_fuites01, data = hemna, id=ID)
summary(hemnbfuit)

str(hemna$Localisation2)
hemloc = coxph(formula = Surv(time, status) ~ Localisation2, data = hemna, id=ID)
summary(hemloc)

str(hemna$Fuite_arterielle2)
table(hemna$Fuite_arterielle2)
hemfuitart = coxph(formula = Surv(time, status) ~ Fuite_arterielle2, data = hemna, id=ID)
summary(hemfuitart)

hemembo = coxph(formula = Surv(time, status) ~ Embolisation, data = hemna, id=ID)
summary(hemembo)

hemrupt = coxph(formula = Surv(time, status) ~ Hematome_rompu, data = hemna, id=ID)
summary(hemrupt)

fuitecox = coxph(formula = Surv(time, status) ~ Fuite_arteriographique2, data = hemna, id = ID)
summary(fuitecox)

#------------------------------------------------------------------------------

tabcardio = table(hemna$Comorbidite, hemna$deces01)
prop.table(tabcardio, margin = 2)

tabfuite = table(hemna$Nombre_de_fuites01, hemna$deces01)
prop.table(tabfuite, margin = 2)

tabarterio = table(hemna$Fuite_arteriographique2, hemna$deces01)
prop.table(tabarterio, margin = 2)
