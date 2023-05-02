#-PACKAGES----------------------------------------------------------------------
require("here")
require("readxl")
require("tidyverse")

#-DATA--------------------------------------------------------------------------
no <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", 
                      "Benoit_Alexandra", "nomodata_sum.xlsx"))

names(no)

#-VAR-RECODING------------------------------------------------------------------
table(no$Protocole)
no$Protocole[no$Protocole == "antagoniste"] <- "Antagoniste"

no$Dosededepart = as.numeric(as.character(no$Dosededepart))
no$Agedebutdestim = as.numeric(as.character(no$Agedebutdestim))
no$Dosetotale = as.numeric(as.character(no$Dosetotale))
no$Duree = as.numeric(as.character(no$Duree))
no$nbredovorecueillis = as.numeric(as.character(no$nbredovorecueillis))
no$Nbreovomatures = as.numeric(as.character(no$Nbreovomatures))
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

#-LINEAR------------------------------------------------------------------------
# DEPENDENT VARIABLE: Nb_total_ovo_matures
# INDEPENDENT VARIABLEs: Agedebutdestim + Nombredestim + ind + nbredovorecueillis
# INDEPENDENT VARIABLE TO ADD: IMC

#-NB-MATURES-BINAIRE-----------------------------------------------------------

mean(no$Nb_total_ovo_matures, na.rm = T)
median(no$Nb_total_ovo_matures)
quantile(no$Nb_total_ovo_matures)

#≥5
#≥10
#≥15
#≤3

no$cut3 = if_else(no$Nb_total_ovo_matures <= 3, "1", "0")
no$cut5 = if_else(no$Nb_total_ovo_matures >= 5, "1", "0")
no$cut10 = if_else(no$Nb_total_ovo_matures >= 10, "1", "0")
no$cut10 = if_else(no$Nb_total_ovo_matures >= 15, "1", "0")

#-NOMOGRAMS---------------------------------------------------------------------
install.packages("PASWR")
require("PASWR")

install.packages("rms")
require("rms")

#-CUT-10

cut10base = subset(no, select = c("cut10", "Agedebutdestim", "Nombredestim", 
                                    "ind", "nbredovorecueillis", "IMC",
                                    "AMH", "CFA"))

cut10base.t <- datadist(cut10base)
options(datadist = "cut10base.t")

fit <- lrm(formula = cut10 ~ Agedebutdestim + Nombredestim 
           + ind + nbredovorecueillis + IMC + AMH + CFA, data = no)

plot(nomogram(fit, fun = function(x)plogis(x)))

#-CUT-3

cut3base = subset(no, select = c("cut3", "Agedebutdestim", "Nombredestim", 
                                  "ind", "nbredovorecueillis", "IMC",
                                  "AMH", "CFA"))

cut3base.t <- datadist(cut3base)
options(datadist = "cut3base.t")

fit <- lrm(formula = cut3 ~ Agedebutdestim + Nombredestim 
           + ind + nbredovorecueillis + IMC + AMH + CFA, data = no)

plot(nomogram(fit, fun = function(x)plogis(x)))

###############################################################################
###############################################################################
###############################################################################
#-BY CYCLE---------------------------------------------------------------------

no.all <- read_excel("normo_data.xlsx", na = "")

