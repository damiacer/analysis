#-PACKAGES----------------------------------------------------------------------
#require("here")
require("readxl")
require("tidyverse")

#-DATA--------------------------------------------------------------------------
#no <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", 
#                      "Benoit_Alexandra", "nomodata_sum.xlsx"))

setwd("P:/CONSULTATION/Benoit_Alexandra")

no <- read_excel("normo_data.xlsx", na = "")

names(no)
dim(no)

#-INCLUSION-CRITERIA------------------------------------------------------------
no.i <- no[!(no$Inclus_total=="0"),]
dim(no.i)

#-DATA WITH ONLY EVENTS OCCURED AFFET THE INCLUSION-----------------------------
table(no$Nombre_de_stim)
#no1 <- no.i[!(no.i$Nombre_de_stim=="1"),] not equal to 1
no1 <- no.i[(no.i$Numero_de_stim=="1"),] # equal to 1
dim(no1)

#-REDIRECT-no1-TO-no------------------------------------------------------------
no <- no1
dim(no)

#-VAR-RECODING------------------------------------------------------------------
table(no$Protocole)
no$Protocole[no$Protocole == "antagoniste"] <- "Antagoniste"

no$Dose_de_depart = as.numeric(as.character(no$Dose_de_depart))
no$Age_debut_de_stim = as.numeric(as.character(no$Age_debut_de_stim))
no$Dose_totale = as.numeric(as.character(no$Dose_totale))
no$Duree = as.numeric(as.character(no$Duree))
no$nbre_ovo_recueillis = as.numeric(as.character(no$nbre_ovo_recueillis))
no$Nbre_ovo_matures = as.numeric(as.character(no$Nbre_ovo_matures))
no$AMH = as.numeric(as.character(no$AMH))
no$IMC = as.numeric(as.character(no$IMC))

no <- no %>% 
  mutate(Indication_re = case_when(
    Indication == "ATCD d'annexectomie pour KO" ~ "ATCD KO", #1
    Indication == "Baisse de la RO" ~ "Baisse de la RO", 
    Indication == "baisse de la RO" ~ "Baisse de la RO", 
    Indication == "cancer du sein" ~ "K sein",
    Indication == "Cancer du sein" ~ "K sein",
    Indication == "Cancer digestif" ~ "K digestif",
    Indication == "Cancer ORL" ~ "K ORL",
    Indication == "Cancer du col de l'utérus" ~ "K col uterus",
    Indication == "Tumeur cérébral" ~ "K cerebral",
    Indication == "Endométriose" ~ "Endometriose",
    Indication == "Cancer pulmonaire" ~ "K pulmonaire",
    Indication == "endométriose" ~ "Endometriose", #12
    Indication == "Maladie autoimmune" ~ "Mal Autoimmune",
    Indication == "IOP familiale" ~ "IOP fam",
    Indication == "Mutation génétique" ~ "Mutation genetique",
    Indication == "Myomectomie" ~ "Myomectomie",
    Indication == "Mélanome" ~ "Melanome",
    Indication == "Autre cancer" ~ "K autre" #18
  ))
table(no$Indication_re)
str(no$Indication_re)

no <- no %>% 
  mutate(Indication_res = case_when(
    Indication_re == "ATCD KO" ~ "ATCD KO",
    Indication_re == "Baisse de la RO" ~ "Baisse de la RO",
    Indication_re == "Endometriose" ~ "Endometriose", 
    Indication_re == "IOP fam" ~ "IOP fam", 
    Indication_re == "K autre" | Indication_re == "K col uterus" | Indication_re == "K digestif" 
    | Indication_re == "K ORL" | Indication_re == "K pulmonaire" |
      Indication_re == "K sein" | Indication_re == "Melanome" ~ "Cancer", 
    Indication_re == "Mal Autoimmune" ~ "Mal Autoimmune", 
    Indication_re == "Mutation genetique" ~ "Mutation genetique",
    Indication_re == "Myomectomie" ~ "Myomectomie"
  ))
table(no$Indication_res)

no <- no %>% 
  mutate(ind = case_when(
    Indication_res == "ATCD KO" | Indication_res == "Cancer" ~ "3K", # cancer and history of cancer
    Indication_res == "Baisse de la RO" ~ "2Baisse_RO", 
    Indication_res == "Endometriose" ~ "1Endometriose",
    Indication_res == "IOP fam" | Indication_res == "Mal Autoimmune" | 
      Indication_res == "Mutation genetique" |
      Indication_res == "Myomectomie" ~ "4Autre"
  ))

no <- no %>% 
  mutate(ind2 = case_when(
    ind == "1Endometriose" ~ "1Endo",
    ind == "2Baisse_RO" ~"2Baisse",
    ind == "3K" | ind == "4Autre" ~ "3AutreK"
  ))

no$CFA = as.numeric(as.factor(no$CFA))

#-LINEAR------------------------------------------------------------------------
# DEPENDENT VARIABLE: Nb_total_ovo_matures
# INDEPENDENT VARIABLEs: Agedebutdestim + Nombredestim + ind + nbredovorecueillis
# INDEPENDENT VARIABLE TO ADD: IMC

#-NB-MATURES-BINAIRE-----------------------------------------------------------

mean(no$Nbre_ovo_matures, na.rm = T)
median(no$Nbre_ovo_matures)
quantile(no$Nbre_ovo_matures)

#≥5
#≥10
#≥15
#≤3

no$cut3 = if_else(no$Nbre_ovo_matures <= 3, "1", "0")
no$cut5 = if_else(no$Nbre_ovo_matures >= 5, "1", "0")
no$cut10 = if_else(no$Nbre_ovo_matures >= 10, "1", "0")
no$cut15 = if_else(no$Nbre_ovo_matures >= 15, "1", "0")

#-NOMOGRAMS---------------------------------------------------------------------
#install.packages("PASWR")
require("PASWR")

#install.packages("rms")
require("rms")

#-CUT-10

cut10base = subset(no, select = c("cut10", "Age_debut_de_stim",  
                                  "ind2", "IMC",
                                  "AMH", "CFA"))

cut10base.t <- datadist(cut10base)
options(datadist = "cut10base.t")

fit <- lrm(formula = cut10 ~ Age_debut_de_stim + ind2 + 
             IMC + AMH + CFA, data = no)

plot(nomogram(fit, fun = function(x)plogis(x)))

#-CUT-3

cut3base = subset(no, select = c("cut3", "Age_debut_de_stim",  
                                 "ind2", "IMC",
                                 "AMH", "CFA"))

cut3base.t <- datadist(cut3base)
options(datadist = "cut3base.t")

fit <- lrm(formula = cut3 ~ Age_debut_de_stim + ind2 + 
             IMC + AMH + CFA, data = no)

plot(nomogram(fit, fun = function(x)plogis(x)))

#-CUT-5

cut5base = subset(no, select = c("cut5", "Age_debut_de_stim",  
                                 "ind2", "IMC",
                                 "AMH", "CFA"))

cut5base.t <- datadist(cut5base)
options(datadist = "cut5base.t")

fit <- lrm(formula = cut5 ~ Age_debut_de_stim + ind2 + 
             IMC + AMH + CFA, data = no)

plot(nomogram(fit, fun = function(x)plogis(x)))


#-CUT-15

cut15base = subset(no, select = c("cut15", "Age_debut_de_stim",  
                                  "ind2", "IMC",
                                  "AMH", "CFA"))

cut15base.t <- datadist(cut15base)
options(datadist = "cut15base.t")

fit <- lrm(formula = cut15 ~ Age_debut_de_stim + ind2 + 
             IMC + AMH + CFA, data = no)

plot(nomogram(fit, fun = function(x)plogis(x)))
