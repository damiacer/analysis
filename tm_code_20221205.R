getwd()
setwd("P:/CONSULTATION/Metayer_Thomas") # ON PC
setwd("/Users/damianocerasuolo/Desktop/UBRC/21_22_CONSULT_MAC/Metayer_Thomas")
#-------------------------------------------------------------------------------
#install.packages("readxl")
library("readxl")
#install.packages("dplyr")
library("dplyr")
#install.packages("plyr")
#library("plyr") IT HIDES DPLYR!!!
#install.packages("tidyverse")
library("tidyverse")
#-------------------------------------------------------------------------------
# DATABASE
tm <- read_excel("thme_db.xlsx", na="N/A")
warnings()
#View(tm)
names(tm)
dim(tm)
#-------------------------------------------------------------------------------
# RENAME VARIABLES 
tm <- as_tibble(tm)
tm <- tm %>% rename(
  "Clip_simple" = "Clip_simple_",
  "sexe" = "Femme",
  "Volume_Hematome_cc" = "Volume_Hematome_(cc)"
  )
#-------------------------------------------------------------------------------
# VARIBALE ENDOCHIR
table(tm$Endovasculaire_premier)
table(tm$Chirurgie_premiere)

tm <- tm %>%
  mutate(endo.chir = case_when(
    Endovasculaire_premier == 1 ~ "endo",
    Chirurgie_premiere == 1 ~ "chir"
  ))

table(tm$endo.chir)
# chir endo 
# 16   53 == 69

#--------------

# VARIABLE CENTRE
tm <- tm %>%
  mutate(center = case_when(
    Rouen == 1 ~ "rouen",
    Caen == 1 ~ "caen",
    Rotschild == 1 ~ "rotschild"
  ))

table(tm$center)
#caen rotschild     rouen 
#9        54         6

#--------------

# VAR SEXE
tm$sexe = if_else(tm$sexe == 1, "femme", "homme")
table(tm$sexe)

#--------------

# WFNS
table(tm$WFNS)

tm <- tm %>%
  mutate(WFNS.2 = case_when(
    #tm == 0 ~ ".",
    WFNS == 1 | WFNS == 2 | WFNS == 3 ~ "1-3",
    WFNS == 4 | WFNS == 5 ~ "4-5"
  ))
table(tm$WFNS.2) # WARNING: ZEROS NOT TAKEN INTO ACCOUNT 

#--------------

# FISHER

table(tm$Fischer)

tm <- tm %>% 
  mutate(fisher.2 = case_when(
    Fischer == 1 | Fischer == 2 | Fischer == 3 ~ "1-3",
    Fischer == 4 ~ "4"
  ))
table(tm$fisher.2)

#--------------

# IVH in the table
table(tm$Hematome_intra_cerebral)

#--------------

tm$Graeb = as.numeric(as.factor(tm$Graeb))

#--------------

# ENDO/CHIR RECODING 0/1
table(tm$endo.chir)
#chir endo 
#16   53 

tm <- tm %>%
  mutate(ench = case_when(
    endo.chir == "chir" ~ "0",
    endo.chir == "endo"  ~ "1"
  ))

#-------------------------------------------------------------------------------
# DESCRIPTIVE
library("tableone")

CreateTableOne(data = tm)
dput(names(tm))

f <- function(x){
  result = as.factor(x)
  return(result)
}

str(tm$ench)

tm$ench = as.factor(tm$ench)
tm$Anevrisme_familial <- f(tm$Anevrisme_familial)
tm$ATCD_rompu = f(tm$ATCD_rompu)
tm$Echec_embolisation_premiere = f(tm$Echec_embolisation_premiere)
tm$Rompu = f(tm$Rompu)
tm$Hematome_intra_cerebral = f(tm$Hematome_intra_cerebral)
tm$Hemorragie_intra_ventriculaire = f(tm$Hemorragie_intra_ventriculaire)
tm$DVE_en_urgence = f(tm$DVE_en_urgence)
tm$Localisation = f(tm$Localisation)
tm$Cote_gauche = f(tm$Cote_gauche)
tm$Anevrisme_dissequant = f(tm$Anevrisme_dissequant)
tm$Thrombose_partielle = f(tm$Thrombose_partielle)
tm$Multilobulaire = f(tm$Multilobulaire)
tm$Branche_base_dome = f(tm$Branche_base_dome)
tm$Clip_simple = f(tm$Clip_simple)
tm$Remodelling = f(tm$Remodelling)
tm$By_pass = f(tm$By_pass)
tm$neuronavigation = f(tm$neuronavigation)
tm$Fluorosceine = f(tm$Fluorosceine)
tm$Utilisation_clip_temporaire = f(tm$Utilisation_clip_temporaire)
tm$Coils = f(tm$Coils)
tm$Coils_plus_stent = f(tm$Coils_plus_stent)
tm$Flow_diverter = f(tm$Flow_diverter)
tm$Rupture_per_procedure = f(tm$Rupture_per_procedure)
tm$Deces_par_rupture_per_procedure = f(tm$Deces_par_rupture_per_procedure)
tm$Ischemie_asymptomatique = f(tm$Ischemie_asymptomatique) 
tm$Ischemie_symptomatique = f(tm$Ischemie_symptomatique)
tm$Deces = f(tm$Deces) 
tm$DCI = f(tm$DCI)
tm$DVP = f(tm$DVP)
tm$Raymond_roy_post_op = f(tm$Raymond_roy_post_op)
tm$Raymond_Roy_1_an = f(tm$Raymond_Roy_1_an)
tm$Retraitement_suite_a_lacte_de_cette_etude = f(tm$Retraitement_suite_a_lacte_de_cette_etude)
tm$Retraitement_suite_a_un_traitement_prealable_a_cette_etude = f(tm$Retraitement_suite_a_un_traitement_prealable_a_cette_etude)
tm$endo.chir = f(tm$endo.chir)
tm$center = f(tm$center)

tm$Largeur_collet = as.numeric(as.character(tm$Largeur_collet))
tm$Graeb = as.numeric(as.character(tm$Graeb))
tm$Volume_Hematome_cc = as.numeric(as.character(tm$Volume_Hematome_cc))
tm$mRs_de_base = as.numeric(as.character(tm$mRs_de_base))
tm$mrs_a_6_mois = as.numeric(as.character(tm$mrs_a_6_mois))

vars <- c("Age", #num
          "sexe", 
          "Tabac", 
          "HTA", 
          "Anevrisme_familial", 
          "ATCD_rompu", 
          "ATCD_non_rompu",
          "mRs_de_base", #num
          "Glasgow", 
          "WFNS.2", 
          "fisher.2",
          "Hemorragie_intra_ventriculaire",
          "Graeb", #num
          "Hematome_intra_cerebral",
          "Volume_Hematome_cc", #num
          "DVE_en_urgence",
          "DCI",
          "DVP",
          "Rompu",
          "Localisation",
          "Multilobulaire",                                        
          "Branche_base_dome",
          "Largeur_dome",                                     
          "Largeur_collet",                                   
          "hauteur_dome",                                           
          "Aspect_ratio",
          
          "Clip_simple",                                            
          "Remodelling",
          "By_pass",
          "neuronavigation",
          "Fluorosceine",
          "Utilisation_clip_temporaire",
          "Coils",
          "Coils_plus_stent",
          "Flow_diverter",
          
          "Rupture_per_procedure",                                  
          "Deces_par_rupture_per_procedure",               
          "Ischemie_asymptomatique",                          
          "Ischemie_symptomatique",
          
          "Deces",
          "mrs_a_6_mois", #num                                         
          "Raymond_roy_post_op",
          "Raymond_Roy_1_an",
          "Retraitement_suite_a_lacte_de_cette_etude",
          "Retraitement_suite_a_un_traitement_prealable_a_cette_etude",
          "date_en_mois_retraitement" #num                             
          )


catvars <- c("sexe", 
             "Tabac", 
             "HTA", 
             "Anevrisme_familial", 
             "ATCD_rompu", 
             "ATCD_non_rompu",
             "Glasgow", 
             "WFNS.2", 
             "fisher.2",
             "Hemorragie_intra_ventriculaire",
             "Hematome_intra_cerebral",
             "DVE_en_urgence",
             "DCI",
             "DVP",
             "Rompu",
             "Localisation",
             "Multilobulaire",                                        
             "Branche_base_dome",
             
             "Clip_simple",                                            
             "Remodelling",
             "By_pass",
             "neuronavigation",
             "Fluorosceine",
             "Utilisation_clip_temporaire",
             "Coils",
             "Coils_plus_stent",
             "Flow_diverter",
             
             "Rupture_per_procedure",                                  
             "Deces_par_rupture_per_procedure",               
             "Ischemie_asymptomatique",                          
             "Ischemie_symptomatique",
             
             "Deces",
             "Raymond_roy_post_op",
             "Raymond_Roy_1_an",
             "Retraitement_suite_a_lacte_de_cette_etude",
             "Retraitement_suite_a_un_traitement_prealable_a_cette_etude"
)

tab.des <- CreateTableOne(vars = vars, data = tm, factorVars = catvars)
print(tab.des, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

tab.univ <- CreateTableOne(vars = vars, data = tm, factorVars = catvars, test = TRUE, 
                           strata = "endo.chir")
print(tab.univ, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#-------------------------------------------------------------------------------
#LOGIT UNIVARIATE

gf <- function(x){
  result = glm(ench ~ x, data = tm, family = binomial)
  return(summary(result))
}

gf2 <- function(x){
  result = (mod = glm(ench ~ x, data = tm, family = binomial))
  return(cbind(exp(coef(mod)), exp(confint.default(mod))))
}


# age
gf(tm$Age)
gf2(tm$Age)

#sexe
gf(tm$sexe)
gf2(tm$sexe)

#Tabac
gf(tm$Tabac)
gf2(tm$Tabac)

#HTA
gf(tm$HTA)
gf2(tm$HTA)

#Anevrisme_familial
#library(MASS)
#M1 <- glm(endo.chir ~ Anevrisme_familial, data = tm, family = binomial)
#nboot <- 1000
#bres <- matrix(NA,nrow=nboot,
#               ncol=length(coef(M1)),
#               dimnames=list(rep=seq(nboot),
#                             coef=names(coef(M1))))
#set.seed(101)
#bootsize <- 200
#for (i in seq(nboot)) {
#  bdat <- tm[sample(nrow(tm),size=bootsize,replace=TRUE),]
#  bfit <- update(M1, data=tm)  ## refit with new data
#  bres[i,] <- coef(bfit)
#}

#data.frame(mean_est=colMeans(bres), t(apply(bres, 2, quantile, c(0.025,0.975)))

library(boot)
library(car)
M1 <- glm(ench ~ Anevrisme_familial, data = tm, family = binomial)
betahat.boot <- Boot(M1, R=199)

gf(tm$Anevrisme_familial)
gf2(tm$Anevrisme_familial)

#ATCD_rompu 
gf(tm$ATCD_rompu)
gf2(tm$ATCD_rompu)

#ATCD_non_rompu
gf(tm$ATCD_non_rompu)
gf2(tm$ATCD_non_rompu)

#mRs_de_base //num
mean(tm$mRs_de_base)
tm <- tm %>% 
  mutate(mRs_de_base.class = case_when(
    mRs_de_base == 0 ~ "0",
    mRs_de_base == 1 | mRs_de_base == 2 ~ "1",
    mRs_de_base >= 3 ~ "2"
  ))
table(tm$mRs_de_base.class)

gf(tm$mRs_de_base)
gf2(tm$mRs_de_base)

#Glasgow
gf(tm$Glasgow)
gf2(tm$Glasgow)

tm %>%
  group_by(ench) %>%
  summarise_at(vars("Glasgow"), funs(mean, sd), na.rm = TRUE)


#WFNS.2
gf(tm$WFNS.2)
gf2(tm$WFNS.2)

#fisher.2
gf(tm$fisher.2)
gf2(tm$fisher.2)

#Hemorragie_intra_ventriculaire
gf(tm$Hemorragie_intra_ventriculaire)
gf2(tm$Hemorragie_intra_ventriculaire)

#Graeb // num
gf(tm$Graeb)
gf2(tm$Graeb)

#Hematome_intra_cerebral
gf(tm$Hematome_intra_cerebral)
gf2(tm$Hematome_intra_cerebral)

#Volume_Hematome_cc // num
gf(tm$Volume_Hematome_cc)
gf2(tm$Volume_Hematome_cc)

#DVE_en_urgence
gf(tm$DVE_en_urgence)
gf2(tm$DVE_en_urgence)

#DCI
#DVP
#Rompu
gf(tm$Rompu)
gf2(tm$Rompu)

#Localisation
gf(tm$Localisation)
gf2(tm$Localisation)

#Multilobulaire
gf(tm$Multilobulaire)
gf2(tm$Multilobulaire)

#Branche_base_dome
gf(tm$Branche_base_dome)
gf2(tm$Branche_base_dome)

#Largeur_dome
gf(tm$Largeur_dome)
gf2(tm$Largeur_dome)

#Largeur_collet
gf(tm$Largeur_collet)
gf2(tm$Largeur_collet)

#hauteur_dome 
gf(tm$hauteur_dome)
gf2(tm$hauteur_dome)

#Aspect_ratio
gf(tm$Aspect_ratio)
gf2(tm$Aspect_ratio)

#"Clip_simple", "Remodelling", "By_pass", "neuronavigation",
#"Fluorosceine", "Utilisation_clip_temporaire",

#Coils
gf(tm$Coils)
gf2(tm$Coils)

#Coils_plus_stent
gf(tm$Coils_plus_stent)
gf2(tm$Coils_plus_stent)
"Flow_diverter",

#Rupture_per_procedure"                                
#Deces_par_rupture_per_procedure"

#Ischemie_asymptomatique
gf(tm$Ischemie_asymptomatique)
gf2(tm$Ischemie_asymptomatique)

#Ischemie_symptomatique
gf(tm$Ischemie_symptomatique)
gf2(tm$Ischemie_symptomatique)

#Deces
gf(tm$Deces)
gf2(tm$Deces)

#mrs_a_6_mois / num  
gf(tm$mrs_a_6_mois)
gf2(tm$mrs_a_6_mois)

#Raymond_roy_post_op

#Raymond_Roy_1_an
tm$Raymond_Roy_1_an = as.numeric(as.character(tm$Raymond_Roy_1_an))
tm <- tm %>%
  mutate(Raymond_Roy_1_an.class = case_when(
    Raymond_Roy_1_an == 0 | Raymond_Roy_1_an == 1 ~ "1",
    Raymond_Roy_1_an >= 2 ~ "2"
  ))

table(tm$Raymond_Roy_1_an.class, tm$ench)
prop.table(table(tm$Raymond_Roy_1_an.class, tm$ench), margin = 2)*100

gf(tm$Raymond_Roy_1_an.class)
gf2(tm$Raymond_Roy_1_an.class)
"Retraitement_suite_a_lacte_de_cette_etude",
"Retraitement_suite_a_un_traitement_prealable_a_cette_etude",
"date_en_mois_retraitement" #num   

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# ONLY CHIR

tm.chir <- tm[!(tm$endo.chir=="endo"),]
dim(tm.chir)

tab.des.chi <- CreateTableOne(vars = vars, data = tm.chir, factorVars = catvars)
print(tab.des.chi, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#-------------------------------------------------------------------------------
# ONLY ENDO

tm.endo <- tm[!(tm$endo.chir=="chir"),]


tab.des.endo <- CreateTableOne(vars = vars, data = tm.endo, factorVars = catvars)
print(tab.des.endo, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#-------------------------------------------------------------------------------

#UNIVARIATE ANALYSIS

w <- function(x){
  #result = shapiro.test(x)
  result = wilcox.test(x ~ tm$endo.chir, paired=F, exact=F, correct=F,
                       alternative = "two.sided",
                       conf.int = TRUE, conf.level = 0.95)
  #result = t.test(x, y, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
  return(result)
}

k <- function(x){
  result = chisq.test(table(x, tm$endo.chir), correct = F,
                      simulate.p.value = TRUE, B = 2000)
  return(result)
}


w(tm$Age)
k(tm$sexe)
k(tm$Tabac) 
k(tm$HTA) 
k(tm$Anevrisme_familial)
k(tm$ATCD_rompu)
k(tm$ATCD_non_rompu)
k(tm$mRs_de_base)
k(tm$Glasgow)
k(tm$WFNS.2)
k(tm$fisher.2)
#fisher.test(tm$endo.chir, tm$fisher.2)
k(tm$Hemorragie_intra_ventriculaire)
w(tm$Graeb)
k(tm$Hematome_intra_cerebral)
w(tm$Volume_Hematome_cc)
k(tm$DVE_en_urgence)
#DCI
k(tm$DVP)
k(tm$Rompu)
k(tm$Localisation)
k(tm$Multilobulaire)                                       
k(tm$Branche_base_dome)
w(tm$Largeur_dome)                                
w(tm$Largeur_collet)                               
w(tm$hauteur_dome)                                        
w(tm$Aspect_ratio)

#Clip_simple                                        
#Remodelling
#By_pass
#neuronavigation
#Fluorosceine
#Utilisation_clip_temporaire

k(tm$Coils)
k(tm$Coils_plus_stent)
#####Flow_diverter

k(tm$Rupture_per_procedure)   
table(tm$Rupture_per_procedure, tm$endo.chir)

k(tm$Deces_par_rupture_per_procedure)               
k(tm$Ischemie_asymptomatique)                        
k(tm$Ischemie_symptomatique)

k(tm$Deces)
w(tm$mrs_a_6_mois)                                    
#Raymond_roy_post_op
k(tm$Raymond_Roy_1_an)
k(tm$Retraitement_suite_a_lacte_de_cette_etude)
k(tm$Retraitement_suite_a_un_traitement_prealable_a_cette_etude)
table(tm$endo.chir, tm$date_en_mois_retraitement)
)

#-------------------------------------------------------------------------------

mod1 <- glm(endo.chir ~ Largeur_collet + Branche_base_dome, data = tm, 
	family = binomial)

summary(mod1)
exp(cbind(OR = coef(mod1), confint(mod1)))

table(tm$Ischemie_symptomatique)

#-------------------------------------------------------------------------------

table(tm$endo.chir)
str(tm$endo.chir)
count(tm)
# seulement les patients chir+ : 
chir = tm[!(tm$endo.chir == "endo"),]
count(chir)

names(chir)

library("tidyverse")

# centre 

chir <- chir %>%
	mutate(centre = case_when(
	Rouen == "1" ~ "Rouen", 
	Caen == "1" ~ "Caen"
	))
	
str(chir$mrs_a_6_mois)
table(chir$mrs_a_6_mois)

chir$mrs_a_6_mois.bin = if_else(chir$mrs_a_6_mois >= 3, "bad", "good")
	
table(chir$centre, chir$mrs_a_6_mois)

chisq.test(chir$centre, chir$mrs_a_6_mois.bin, simulate.p.value = TRUE, B = 2000)
fisher.test(chir$centre, chir$mrs_a_6_mois.bin)
table(chir$centre, chir$mrs_a_6_mois.bin)