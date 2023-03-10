library("ciTools")

setwd("/Users/damianocerasuolo/Desktop/UBRC/22_23_CONSULT_MAC/Bazille_C/Steatose")

library("readxl")

ba <- read_excel("ceba_data.xlsx", na = "")

names(ba)

# VARIABLES DEPENDANTES POUR ANALYSES BIVARIEES 
# Steatose
# Steatose_Qpath
# Score_de_Clavien_7
table(ba$Score_de_Clavien_7)

table(ba$Steatose)
mean(ba$Steatose)
min(ba$Steatose)
max(ba$Steatose)

ba$Steatose.bin = if_else(ba$Steatose < 30, "0", "1")
table(ba$Steatose.bin, useNA = "always")

library("tableone")

dput(names(ba))

vars = c("Age", "Age_date_operatoire", 
         "sexe", "BMI", "diabete", "DID", 
         "DNID", "Dyslipidemie", "Cardiopathie", "HTA", "Sd_metabolique", 
         "ATCD_cardio", "Alcool", "cirrhose", "VHB", "VHC", "Hemochromatose", 
         "Chimio", "type_de_chir_minmaj", 
         "Conversion_op", "Type_histologique_de_la_lesion_resequee", 
         "Benin1_Malin2", "Steatose", "class_steatose", "Steatose_Qpath", 
         "Ballonisation_Hepatocytaire_1", "Inflammation_lobulaire_2bin", 
         "Activite_3bin", "Fibrose_METAVIRbin", "Fibrose_SAF_4bin", "NAFLD_5", 
         "NASH", "SOS_Syndrome", "noyaux_glycogeniques_", "complication_aigue_post_op_med", 
         "complication_aigue_chir", "Score_de_Clavien_7", "Mortalite_a_90jours")

catvars = c("sexe", "diabete", "DID", 
            "DNID", "Dyslipidemie", "Cardiopathie", "HTA", "Sd_metabolique", 
            "ATCD_cardio", "Alcool", "cirrhose", "VHB", "VHC", "Hemochromatose", 
            "Chimio", "type_de_chir_minmaj",  
            "Conversion_op", "Type_histologique_de_la_lesion_resequee", 
            "Benin1_Malin2", "class_steatose", 
            "Ballonisation_Hepatocytaire_1", "Inflammation_lobulaire_2bin", 
            "Activite_3bin", "Fibrose_METAVIRbin", "Fibrose_SAF_4bin", "NAFLD_5", 
            "NASH", "SOS_Syndrome", "noyaux_glycogeniques_", "complication_aigue_post_op_med", 
            "complication_aigue_chir", "Score_de_Clavien_7", "Mortalite_a_90jours")

ba.des = CreateTableOne(vars = vars, data = ba, factorVars = catvars)
print(ba.des, showAllLevels = TRUE, quote = TRUE, noSpaces = TRUE, includeNA = TRUE)  

ba.steat = CreateTableOne(vars = vars, data = ba, factorVars = catvars, test = TRUE, includeNA = FALSE, 
                         strata = "Steatose.bin")
print(ba.steat, showAllLevels = TRUE, quote = TRUE, noSpaces = TRUE)

#-----------------------------------------------------------------------------------------

# variables recoded 

str(ba$Activite_3)
ba$Activite_3 = as.numeric(as.character(ba$Activite_3))
ba$Activite_3bin = if_else(ba$Activite_3 >= 1, "1", "0")
table(ba$Activite_3bin)

table(ba$Fibrose_METAVIR)
ba$Fibrose_METAVIR = as.numeric(as.character(ba$Fibrose_METAVIR))
ba$Fibrose_METAVIRbin = if_else(ba$Fibrose_METAVIR >=3, "1", "0")
table(ba$Fibrose_METAVIRbin)

table(ba$Fibrose_SAF_4)
ba$Fibrose_SAF_4 = as.numeric(as.character(ba$Fibrose_SAF_4))
ba$Fibrose_SAF_4bin = if_else(ba$Fibrose_SAF_4 >= 2, "1", "0")
table(ba$Fibrose_SAF_4bin)

#-----------------------------------------------------------------------------------------

# table(row, col)
table(ba$Activite_3bin, ba$Steatose.bin)
chisq.test(ba$Activite_3bin, ba$Steatose.bin, simulate.p.value = T, B = 2000, 
           correct = F)
prop.table(table(ba$Activite_3bin, ba$Steatose.bin), margin = 2)*100
mod.actbin = glm(Steatose.bin ~ Activite_3bin, data = ba, family = "binomial")
summary(mod.actbin)
cbind(exp(coef(mod.actbin)), exp(confint.default(mod.actbin)))

table(ba$Fibrose_METAVIRbin, ba$Steatose.bin)
chisq.test(ba$Fibrose_METAVIRbin, ba$Steatose.bin, simulate.p.value = T, B = 2000, 
           correct = F)
prop.table(table(ba$Fibrose_METAVIRbin, ba$Steatose.bin), margin = 2)*100
mod.metabin = glm(Steatose.bin ~ Fibrose_METAVIRbin, data = ba, family = "binomial")
summary(mod.metabin)
cbind(exp(coef(mod.metabin)), exp(confint.default(mod.metabin)))

table(ba$Fibrose_SAF_4bin, ba$Steatose.bin)
chisq.test(ba$Fibrose_SAF_4bin, ba$Steatose.bin, simulate.p.value = T, B = 2000,
           correct = F)
prop.table(table(ba$Fibrose_SAF_4bin, ba$Steatose.bin), margin = 2)*100
mod.safbin = glm(Steatose.bin ~ Fibrose_SAF_4bin, data =  ba, family = "binomial")
summary(mod.safbin)
cbind(exp(coef(mod.safbin)), exp(confint.default(mod.safbin)))

#-----------------------------------------------------------------------------------------

# logistic regression for the univariate 

# function

lf = function(x,y){
  result = glm(y ~ x, data = ba, family = binomial)
  return(cbind(exp(coef(result)), exp(confint.default(result))))
}

lfr = function(y,x){
  result = glm(y ~ x, data = ba, family = binomial)
  return(cbind(exp(coef(result)), exp(confint.default(result))))
}

lfr.s = function(y,x){
  result = glm(y ~ x, data = ba, family = binomial)
  return(summary(result))
}

str(ba$Steatose.bin)
ba$Steatose.bin = as.factor(ba$Steatose.bin)

Age <- glm(Steatose.bin ~ Age, data = ba, family = binomial)
summary(Age)
exp(coef(Age))
exp(confint(Age))
cbind(exp(coef(Age)), exp(confint.default(Age)))

Age_date_operatoire <- glm(Steatose.bin ~ Age_date_operatoire, data = ba, family = "binomial")
summary(Age_date_operatoire)
cbind(exp(coef(Age_date_operatoire)), exp(confint.default(Age_date_operatoire)))

sexe <- glm(Steatose.bin ~ sexe, data = ba, family = "binomial")
cbind(exp(coef(sexe)), exp(confint.default(sexe)))
summary(sexe)
lf(ba$sexe, ba$Steatose.bin)

BMI <- glm(Steatose.bin ~ BMI, data = ba, family = "binomial")
cbind(exp(coef(BMI)), exp(confint.default(BMI)))
summary(BMI)

diabete <- glm(Steatose.bin ~ diabete, data = ba, family = binomial)
cbind(exp(coef(diabete)), exp(confint.default(diabete)))
summary(diabete)

DID 
lf(ba$DID, ba$Steatose.bin)

DNID

Dyslipidemie <- glm(ba$Steatose.bin ~ ba$Dyslipidemie, family = binomial)
summary(Dyslipidemie)
lfr(ba$Steatose.bin, ba$Dyslipidemie)

lfr(ba$Steatose.bin, ba$Cardiopathie)
Cardiopathie = glm(Steatose.bin ~ Cardiopathie, data = ba, family = binomial)
summary(Cardiopathie)

lfr(ba$Steatose.bin, ba$HTA)
HTA = glm(Steatose.bin ~ HTA, data = ba, family = binomial)
summary(HTA)

lfr(ba$Steatose.bin, ba$Sd_metabolique)
Sd_metabolique = glm(Steatose.bin ~ Sd_metabolique, family = binomial, data = ba)
summary(Sd_metabolique)

lfr(ba$Steatose.bin, ba$ATCD_cardio)
ATCD_cardio = glm(Steatose.bin ~ ATCD_cardio, family = "binomial", data = ba)
summary(ATCD_cardio)

lfr(ba$Steatose.bin, ba$Alcool)
Alcool = glm(Steatose.bin ~ Alcool, data = ba, family = "binomial")
summary(Alcool)

lfr(ba$Steatose.bin, ba$cirrhose)
cirrhose = glm(Steatose.bin ~ cirrhose, data = ba, family = "binomial")
summary(cirrhose)

lfr(ba$Steatose.bin, ba$VHB)
VHB = glm(Steatose.bin ~ VHB, data = ba, family = "binomial")
summary(VHB)

lfr(ba$Steatose.bin, ba$VHC)
VHC = glm(Steatose.bin ~ VHC, data = ba, family = "binomial")
summary(VHC)

lfr(ba$Steatose.bin, ba$Hemochromatose)
Hemochromatose = glm(Steatose.bin ~ Hemochromatose, data = ba, family = "binomial")
summary(Hemochromatose)

lfr(ba$Steatose.bin, ba$Chimio)
Chimio = glm(Steatose.bin ~ Chimio, data = ba, family = "binomial")
summary(Chimio)

lfr(ba$Steatose.bin, ba$type_de_chir_minmaj)
type_de_chir_minmaj = glm(ba$Steatose.bin ~ ba$type_de_chir_minmaj, family = binomial)
summary(type_de_chir_minmaj)
cbind(exp(coef(type_de_chir_minmaj)), exp(confint.default(type_de_chir_minmaj)))

lfr(ba$Steatose.bin, ba$Conversion_op)
lfr.s(ba$Steatose.bin, ba$Conversion_op)
Conversion_op = glm(ba$Steatose.bin ~ ba$Conversion_op,
                    family = "binomial")
summary(Conversion_op)

table(ba$Type_histologique_de_la_lesion_resequee)
lfr(ba$Steatose.bin, ba$Type_histologique_de_la_lesion_resequee)
mod1 <- glm(Steatose.bin ~ Type_histologique_de_la_lesion_resequee, data = ba, family = binomial)
mod.e <- glm(Steatose.bin ~ 1, data = ba, family = binomial)
library("lmtest")
lrtest(mod1, mod.e)
anova(mod1, mod.e)

lfr(ba$Steatose.bin, ba$Benin1_Malin2)
Benin1_Malin2 = glm(Steatose.bin ~ Benin1_Malin2, data = ba, family = "binomial")
summary(Benin1_Malin2)

lfr(ba$Steatose.bin, ba$Steatose_Qpath)
mod.qpath = glm(ba$Steatose.bin ~ ba$Steatose_Qpath, family = "binomial")
summary(mod.qpath)

ba$Ballonisation_Hepatocytaire_1 = as.factor(ba$Ballonisation_Hepatocytaire_1)
lfr(ba$Steatose.bin, ba$Ballonisation_Hepatocytaire_1)
mod.bal = glm(ba$Steatose.bin ~ ba$Ballonisation_Hepatocytaire_1,
              family = "binomial")
lrtest(mod.bal, mod.e)

str(ba$Ballonisation_Hepatocytaire_1)

ba <- ba %>%
  mutate(ballon2 = case_when(
    Ballonisation_Hepatocytaire_1 == "0" ~ "0",
    Ballonisation_Hepatocytaire_1 == "1" ~ "1",
    Ballonisation_Hepatocytaire_1 == "2" ~ "1"
   ))

table(ba$ballon2)
mod.ballon = glm(Steatose.bin ~ ballon2, data = ba, family = "binomial")
summary(mod.ballon)
cbind(exp(coef(mod.ballon)), exp(confint.default(mod.ballon)))

table(ba$ballon2, ba$Steatose.bin)
prop.table(table(ba$ballon2, ba$Steatose.bin), margin = 2)*100

ba$Inflammation_lobulaire_2 = as.factor(ba$Inflammation_lobulaire_2)
lfr(ba$Steatose.bin, ba$Inflammation_lobulaire_2)
mod.inf = glm(Steatose.bin ~ Inflammation_lobulaire_2, data = ba, family = binomial)
lrtest(mod.inf, mod.e)

str(ba$Inflammation_lobulaire_2)
ba$Inflammation_lobulaire_2 = as.numeric(as.character(ba$Inflammation_lobulaire_2))
ba$Inflammation_lobulaire_2bin = if_else(ba$Inflammation_lobulaire_2 >= 1, "Oui", "Non")
table(ba$Inflammation_lobulaire_2bin)
table(ba$Inflammation_lobulaire_2bin, ba$Steatose.bin)
prop.table(table(ba$Inflammation_lobulaire_2bin, ba$Steatose.bin), margin = 2)*100
chisq.test(ba$Inflammation_lobulaire_2bin, ba$Steatose.bin, simulate.p.value = T, 
           B = 2000, correct = F)
mod.inflabin = glm(Steatose.bin ~ Inflammation_lobulaire_2bin, data = ba, 
                   family = "binomial")
summary(mod.inflabin)
cbind(exp(coef(mod.inflabin)), exp(confint.default(mod.inflabin)))

ba$Activite_3 = as.factor(ba$Activite_3)
lfr(ba$Steatose.bin, ba$Activite_3)
mod.act = glm(Steatose.bin ~ Activite_3, data = ba, family = binomial)
lrtest(mod.act, mod.e)

table(ba$Fibrose_METAVIR)
ba$Fibrose_METAVIR = as.factor(ba$Fibrose_METAVIR)
lfr(ba$Steatose.bin, ba$Fibrose_METAVIR)
mod.fib = glm(Steatose.bin ~ Fibrose_METAVIR, data = ba, family = binomial)
lrtest(mod.fib, mod.e)

table(ba$Fibrose_SAF_4)
ba$Fibrose_SAF_4 = as.factor(ba$Fibrose_SAF_4)
lfr(ba$Steatose.bin, ba$Fibrose_SAF_4)
mod.fibSAF = glm(Steatose.bin ~ Fibrose_SAF_4, data = ba, family = binomial)
lrtest(mod.fibSAF, mod.e)

ba$NAFLD_5 = as.factor(ba$NAFLD_5)
lfr(ba$Steatose.bin, ba$NAFLD_5)
lfr.s(ba$Steatose.bin, ba$NAFLD_5)

ba$NASH = as.factor(ba$NASH)
lfr(ba$Steatose.bin, ba$NASH)
lfr.s(ba$Steatose.bin, ba$NASH)

ba$SOS_Syndrome = ba$SOS_Syndrome
lfr(ba$Steatose.bin, ba$SOS_Syndrome)
lfr.s(ba$Steatose.bin, ba$SOS_Syndrome)
mod.soss = glm(Steatose.bin ~ SOS_Syndrome, data = ba, family = binomial)
add_ci(ba, mod.soss, type = "parametric", nSims = 2000, alpha = 0.5)

ba$noyaux_glycogeniques_ = as.factor(ba$noyaux_glycogeniques_)
lfr(ba$Steatose.bin, ba$noyaux_glycogeniques_)
lfr.s(ba$Steatose.bin, ba$noyaux_glycogeniques_)

ba$complication_aigue_post_op_med = as.factor(ba$complication_aigue_post_op_med)
lfr(ba$Steatose.bin, ba$complication_aigue_post_op_med)
lfr.s(ba$Steatose.bin, ba$complication_aigue_post_op_med)

ba$complication_aigue_chir = as.factor(ba$complication_aigue_chir)
lfr(ba$Steatose.bin, ba$complication_aigue_chir)

table(ba$Score_de_Clavien_7)
ba$Score_de_Clavien_7 = as.factor(ba$Score_de_Clavien_7)
lfr(ba$Steatose.bin, ba$Score_de_Clavien_7)
Score_de_Clavien_7 = glm(Steatose.bin ~ Score_de_Clavien_7, data = ba, family = "binomial")
lrtest(Score_de_Clavien_7, mod.e)

table(ba$CL2)
table(ba$CL2, ba$Steatose.bin)
prop.table(table(ba$CL2, ba$Steatose.bin), margin = 2)*100
chisq.test(ba$CL2, ba$Steatose.bin, simulate.p.value = T, B = 2000, correct = F)
mod.stecl2 = glm(ba$Steatose.bin ~ ba$CL2, family = "binomial")
summary(mod.stecl2)
cbind(exp(coef(mod.stecl2)), exp(confint.default(mod.stecl2)))

ba$Mortalite_a_90jours = as.factor(ba$Mortalite_a_90jours)
lfr(ba$Steatose.bin, ba$Mortalite_a_90jours)
lfr.s(ba$Steatose.bin, ba$Mortalite_a_90jours)

#-----------------------------------------------------------------------------------------

table(ba$Score_de_Clavien_7)
str(ba$Score_de_Clavien_7)

ba <- ba %>%
  mutate(CL2 = case_when(
    Score_de_Clavien_7 <= 2 ~ "0",
    Score_de_Clavien_7 > 2 ~ "1"
  ))
table(ba$CL2)

library("tableone")


vars = c("Age", "Age_date_operatoire", 
         "sexe", "BMI", "diabete", "DID", 
         "DNID", "Dyslipidemie", "Cardiopathie", "HTA", "Sd_metabolique", 
         "ATCD_cardio", "Alcool", "cirrhose", "VHB", "VHC", "Hemochromatose", 
         "Chimio", "type_de_chir_minmaj", 
         "Conversion_op", "Type_histologique_de_la_lesion_resequee", 
         "Benin1_Malin2", "Steatose", "class_steatose", "Steatose_Qpath", "Steatose.bin",
         "Ballonisation_Hepatocytaire_1", "Inflammation_lobulaire_2", 
         "Activite_3", "Fibrose_METAVIR", "Fibrose_SAF_4", "NAFLD_5", 
         "NASH", "SOS_Syndrome", "noyaux_glycogeniques_", "complication_aigue_post_op_med", 
         "complication_aigue_chir", "CL2", "Mortalite_a_90jours")

catvars = c("sexe", "diabete", "DID", 
            "DNID", "Dyslipidemie", "Cardiopathie", "HTA", "Sd_metabolique", 
            "ATCD_cardio", "Alcool", "cirrhose", "VHB", "VHC", "Hemochromatose", 
            "Chimio", "type_de_chir_minmaj",  
            "Conversion_op", "Type_histologique_de_la_lesion_resequee", 
            "Benin1_Malin2", "class_steatose", "Steatose.bin",
            "Ballonisation_Hepatocytaire_1", "Inflammation_lobulaire_2", 
            "Activite_3", "Fibrose_METAVIR", "Fibrose_SAF_4", "NAFLD_5", 
            "NASH", "SOS_Syndrome", "noyaux_glycogeniques_", "complication_aigue_post_op_med", 
            "complication_aigue_chir", "CL2", "Mortalite_a_90jours")


ba.cl = CreateTableOne(vars = vars, data = ba, factorVars = catvars, test = TRUE, includeNA = FALSE, 
                          strata = "CL2")
print(ba.cl, showAllLevels = TRUE, quote = TRUE, noSpaces = TRUE)



# logistic regression for the univariate 

# function

#lf2 = function(x,y){
#  result = glm(y ~ x, data = ba, family = binomial)
#  return(cbind(exp(coef(result)), exp(confint.default(result))))
#}

lfr = function(y,x){
  result = glm(y ~ x, data = ba, family = binomial)
  return(cbind(exp(coef(result)), exp(confint.default(result))))
}

lfr.s = function(y,x){
  result = glm(y ~ x, data = ba, family = binomial)
  return(summary(result))
}

str(ba$CL2)
ba$CL2 = as.factor(ba$CL2)

Age <- glm(CL2 ~ Age, data = ba, family = binomial)
summary(Age)
summary(Age)
exp(coef(Age))
exp(confint(Age))
cbind(exp(coef(Age)), exp(confint.default(Age)))
lfr(ba$CL2, ba$Age)
lfr.s(ba$CL2, ba$Age)

Age_date_operatoire <- glm(CL2 ~ Age_date_operatoire, data = ba, family = "binomial")
summary(Age_date_operatoire)
cbind(exp(coef(Age_date_operatoire)), exp(confint.default(Age_date_operatoire)))
lfr(ba$CL2, ba$Age_date_operatoire)
lfr.s(ba$CL2, ba$Age_date_operatoire)

sexe <- glm(CL2 ~ sexe, data = ba, family = "binomial")
cbind(exp(coef(sexe)), exp(confint.default(sexe)))
summary(sexe)
lfr(ba$CL2, ba$sexe)
lfr.s(ba$CL2, ba$sexe)

BMI <- glm(CL2 ~ BMI, data = ba, family = "binomial")
cbind(exp(coef(BMI)), exp(confint.default(BMI)))
summary(BMI)
lfr(ba$CL2, ba$BMI)
lfr.s(ba$CL2, ba$BMI)

diabete <- glm(CL2 ~ diabete, data = ba, family = binomial)
cbind(exp(coef(diabete)), exp(confint.default(diabete)))
summary(diabete)
lfr(ba$CL2, ba$diabete)
lfr.s(ba$CL2, ba$diabete)

DID 
lf(ba$DID, ba$CL2)

DNID

Dyslipidemie <- glm(ba$CL2 ~ ba$Dyslipidemie, family = binomial)
summary(Dyslipidemie)
lfr(ba$CL2, ba$Dyslipidemie)
lfr.s(ba$CL2, ba$Dyslipidemie)

lfr(ba$CL2, ba$Cardiopathie)
Cardiopathie = glm(CL2 ~ Cardiopathie, data = ba, family = binomial)
summary(Cardiopathie)

lfr(ba$CL2, ba$HTA)
HTA = glm(CL2 ~ HTA, data = ba, family = binomial)
summary(HTA)

lfr(ba$CL2, ba$Sd_metabolique)
Sd_metabolique = glm(CL2 ~ Sd_metabolique, family = binomial, data = ba)
summary(Sd_metabolique)

lfr(ba$CL2, ba$ATCD_cardio)
ATCD_cardio = glm(CL2 ~ ATCD_cardio, family = "binomial", data = ba)
summary(ATCD_cardio)

lfr(ba$CL2, ba$Alcool)
Alcool = glm(CL2 ~ Alcool, data = ba, family = "binomial")
summary(Alcool)

lfr(ba$CL2, ba$cirrhose)
cirrhose = glm(CL2 ~ cirrhose, data = ba, family = "binomial")
summary(cirrhose)

lfr(ba$CL2, ba$VHB)
VHB = glm(CL2 ~ VHB, data = ba, family = "binomial")
summary(VHB)

lfr(ba$CL2, ba$VHC)
VHC = glm(CL2 ~ VHC, data = ba, family = "binomial")
summary(VHC)

lfr(ba$CL2, ba$Hemochromatose)
Hemochromatose = glm(CL2 ~ Hemochromatose, data = ba, family = "binomial")
summary(Hemochromatose)

lfr(ba$CL2, ba$Chimio)
Chimio = glm(CL2 ~ Chimio, data = ba, family = "binomial")
summary(Chimio)

lfr(ba$CL2, ba$type_de_chir_minmaj)
type_de_chir_minmaj = glm(ba$CL2 ~ ba$type_de_chir_minmaj, family = binomial)
summary(type_de_chir_minmaj)
cbind(exp(coef(type_de_chir_minmaj)), exp(confint.default(type_de_chir_minmaj)))

lfr(ba$CL2, ba$Conversion_op)
Conversion_op = glm(CL2 ~ Conversion_op, data = ba, family = "binomial")
summary(Conversion_op)

table(ba$Type_histologique_de_la_lesion_resequee)
lfr(ba$CL2, ba$Type_histologique_de_la_lesion_resequee)
mod1 <- glm(CL2 ~ Type_histologique_de_la_lesion_resequee, data = ba, family = binomial)
mod.e <- glm(CL2 ~ 1, data = ba, family = binomial)
library("lmtest")
lrtest(mod1, mod.e)
anova(mod1, mod.e)

lfr(ba$CL2, ba$Benin1_Malin2)
Benin1_Malin2 = glm(CL2 ~ Benin1_Malin2, data = ba, family = "binomial")
summary(Benin1_Malin2)

lfr(ba$CL2, ba$Steatose_Qpath)
Steatose_Qpath = glm(CL2 ~ Steatose_Qpath, data = ba, family = "binomial")
summary(Steatose_Qpath)

lfr(ba$CL2, ba$Steatose.bin)
Steatose.bin = glm(CL2 ~ Steatose.bin, data = ba, family = "binomial")
summary(Steatose.bin)

ba$Ballonisation_Hepatocytaire_1 = as.factor(ba$Ballonisation_Hepatocytaire_1)
lfr(ba$CL2, ba$Ballonisation_Hepatocytaire_1)
Ballonisation_Hepatocytaire_1 = glm(CL2 ~ Ballonisation_Hepatocytaire_1, data = ba, family = "binomial")
lrtest(Ballonisation_Hepatocytaire_1, mod.e)

ba$ballon2 = as.factor(ba$ballon2)
lfr(ba$CL2, ba$ballon2)
ball2cl2 = glm(CL2 ~ ballon2, data = ba, family = "binomial")
summary(ball2cl2)

table(ba$ballon2, ba$CL2)
prop.table(table(ba$ballon2, ba$CL2), margin = 2)*100

ba$Inflammation_lobulaire_2 = as.factor(ba$Inflammation_lobulaire_2)
lfr(ba$CL2, ba$Inflammation_lobulaire_2)
mod.inf = glm(CL2 ~ Inflammation_lobulaire_2, data = ba, family = binomial)
lrtest(mod.inf, mod.e)

table(ba$Inflammation_lobulaire_2bin, ba$CL2)
prop.table(table(ba$Inflammation_lobulaire_2bin, ba$CL2), margin = 2)*100
chisq.test(ba$Inflammation_lobulaire_2bin, ba$CL2, simulate.p.value = T, B = 2000, 
           correct = F)
mod.infcl2bin = glm(CL2 ~ Inflammation_lobulaire_2bin, family = "binomial", data = ba)
summary(mod.infcl2bin)
cbind(exp(coef(mod.infcl2bin)), exp(confint.default(mod.infcl2bin)))

ba$Activite_3 = as.factor(ba$Activite_3)
lfr(ba$CL2, ba$Activite_3)
mod.act = glm(CL2 ~ Activite_3, data = ba, family = binomial)
lrtest(mod.act, mod.e)

table(ba$Activite_3bin, ba$CL2)
prop.table(table(ba$Activite_3bin, ba$CL2), margin = 2)*100
chisq.test(ba$Activite_3bin, ba$CL2, simulate.p.value = T, B = 2000, correct = F)
mod.actcl2 = glm(CL2 ~ Activite_3bin, data = ba, family = "binomial")
summary(mod.actcl2)
cbind(exp(coef(mod.actcl2)), exp(confint.default(mod.actcl2)))

table(ba$Fibrose_METAVIR)
ba$Fibrose_METAVIR = as.factor(ba$Fibrose_METAVIR)
lfr(ba$CL2, ba$Fibrose_METAVIR)
mod.fib = glm(CL2 ~ Fibrose_METAVIR, data = ba, family = binomial)
lrtest(mod.fib, mod.e)

table(ba$Fibrose_METAVIRbin, ba$CL2)
prop.table(table(ba$Fibrose_METAVIRbin, ba$CL2), margin = 2)*100
chisq.test(ba$Fibrose_METAVIRbin, ba$CL2, simulate.p.value = TRUE, correct = F,
           B = 2000)
mod.metacl2 = glm(CL2 ~ Fibrose_METAVIRbin, data = ba, family = "binomial")
summary(mod.metacl2)
cbind(exp(coef(mod.metacl2)), exp(confint.default(mod.metacl2)))

table(ba$Fibrose_SAF_4)
ba$Fibrose_SAF_4 = as.factor(ba$Fibrose_SAF_4)
lfr(ba$CL2, ba$Fibrose_SAF_4)
mod.fibSAF = glm(CL2 ~ Fibrose_SAF_4, data = ba, family = binomial)
lrtest(mod.fibSAF, mod.e)

table(ba$Fibrose_SAF_4bin, ba$CL2)
prop.table(table(ba$Fibrose_SAF_4bin, ba$CL2), margin = 2)*100
chisq.test(ba$Fibrose_SAF_4bin, ba$CL2, simulate.p.value = T, B = 2000, correct = F)
mod.safcl2 = glm(CL2 ~ Fibrose_SAF_4bin, data = ba, family = "binomial")
summary(mod.safcl2)
cbind(exp(coef(mod.safcl2)), exp(confint.default(mod.safcl2)))

ba$NAFLD_5 = as.factor(ba$NAFLD_5)
lfr(ba$CL2, ba$NAFLD_5)
lfr.s(ba$CL2, ba$NAFLD_5)

ba$NASH = as.factor(ba$NASH)
lfr(ba$CL2, ba$NASH)
lfr.s(ba$CL2, ba$NASH)

ba$SOS_Syndrome = ba$SOS_Syndrome
lfr(ba$CL2, ba$SOS_Syndrome)
lfr.s(ba$CL2, ba$SOS_Syndrome)

ba$noyaux_glycogeniques_ = as.factor(ba$noyaux_glycogeniques_)
lfr(ba$CL2, ba$noyaux_glycogeniques_)
lfr.s(ba$CL2, ba$noyaux_glycogeniques_)

ba$complication_aigue_post_op_med = as.factor(ba$complication_aigue_post_op_med)
lfr(ba$CL2, ba$complication_aigue_post_op_med)
mod.comp = glm(ba$CL2 ~ ba$complication_aigue_post_op_med, family = "binomial")
summary(mod.comp)
lfr.s(ba$CL2, ba$complication_aigue_post_op_med)

ba$complication_aigue_chir = as.factor(ba$complication_aigue_chir)
lfr(ba$CL2, ba$complication_aigue_chir)
lfr.s(ba$CL2, ba$complication_aigue_chir)

ba$Mortalite_a_90jours = as.factor(ba$Mortalite_a_90jours)
lfr(ba$CL2, ba$Mortalite_a_90jours)
lfr.s(ba$CL2, ba$Mortalite_a_90jours)

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

dim(ba)
names(ba)

table(ba$CL2, useNA = "always")
table(ba$Steatose.bin, useNA = "always")
table(ba$Mortalite_a_90jours, useNA = "always")

ba.death = CreateTableOne(vars = vars, data = ba, factorVars = catvars, test = TRUE, includeNA = FALSE, 
                       strata = "Mortalite_a_90jours")
print(ba.death, showAllLevels = TRUE, quote = TRUE, noSpaces = TRUE)

names(ba)

ba$de = ba$Mortalite_a_90jours
                               
lfr(ba$de, ba$Age)
lfr.s(ba$de, ba$Age) 
#age = glm(ba$de ~ ba$Age, family = binomial)
#summary(age)
#cbind(exp(coef(age)), exp(confint.default(age)))

lfr(ba$de, ba$Age_date_operatoire)
lfr.s(ba$de, ba$Age_date_operatoire) 

lfr(ba$de, ba$sexe)  
lfr.s(ba$de, ba$sexe)

lfr(ba$de, ba$BMI)
lfr.s(ba$de, ba$BMI)  

lfr(ba$de, ba$diabete)  
lfr.s(ba$de, ba$diabete)  

lfr(ba$de, ba$DID)                                    
lfr(ba$de, ba$DNID) 

lfr(ba$de, ba$Dyslipidemie)
lfr.s(ba$de, ba$Dyslipidemie) 

lfr(ba$de, ba$Cardiopathie)
lfr.s(ba$de, ba$Cardiopathie)                           

lfr(ba$de, ba$HTA)
lfr.s(ba$de, ba$HTA) 

lfr(ba$de, ba$Sd_metabolique)
lfr.s(ba$de, ba$Sd_metabolique)  

lfr(ba$de, ba$ATCD_cardio)    
lfr.s(ba$de, ba$ATCD_cardio)                            

lfr(ba$de, ba$Alcool) 
lfr.s(ba$de, ba$Alcool)                                 

lfr(ba$de, ba$cirrhose) 
lfr.s(ba$de, ba$cirrhose)                               

lfr(ba$de, ba$VHB)   
lfr.s(ba$de, ba$VHB)                                    

lfr(ba$de, ba$VHC)  
lfr.s(ba$de, ba$VHC)                                    

lfr(ba$de, ba$Hemochromatose)  
lfr.s(ba$de, ba$Hemochromatose)                         

lfr(ba$de, ba$Chimio)      
lfr.s(ba$de, ba$Chimio)                                 

lfr(ba$de, ba$type_chimio) 
lfr.s(ba$de, ba$type_chimio)                            

lfr(ba$de, ba$type_de_chir_minmaj) 
lfr.s(ba$de, ba$type_de_chir_minmaj)                    

lfr(ba$de, ba$type_de_chir)    
lfr.s(ba$de, ba$type_de_chir)                           

lfr(ba$de, ba$geste_associe)     
lfr.s(ba$de, ba$geste_associe)                          

lfr(ba$de, ba$voie_chirurgicale)  
lfr.s(ba$de, ba$voie_chirurgicale)                      

lfr(ba$de, ba$Conversion_op) 
lfr.s(ba$de, ba$Conversion_op)                          

lfr(ba$de, ba$Type_histologique_de_la_lesion_resequee)
lfr.s(ba$de, ba$Type_histologique_de_la_lesion_resequee)
mod.typehist = glm(de ~ Type_histologique_de_la_lesion_resequee, data = ba, 
                   family = "binomial")
mod.e = glm(de ~ 1, data = ba, family = "binomial")
lrtest(mod.typehist, mod.e)

lfr(ba$de, ba$Benin1_Malin2)  
lfr.s(ba$de, ba$Benin1_Malin2)                          

lfr(ba$de, ba$Steatose)  
lfr.s(ba$de, ba$Steatose)                               

lfr(ba$de, ba$class_steatose) 
lfr.s(ba$de, ba$class_steatose)                         

lfr(ba$de, ba$Steatose_Qpath)  
lfr.s(ba$de, ba$Steatose_Qpath)   

lfr(ba$de, ba$Steatose.bin)  
lfr.s(ba$de, ba$Steatose.bin)  

lfr(ba$de, ba$Ballonisation_Hepatocytaire_1)   
lfr.s(ba$de, ba$Ballonisation_Hepatocytaire_1)    
mod.bal = glm(de ~ Ballonisation_Hepatocytaire_1, data = ba, family = "binomial")
lrtest(mod.bal, mod.e)

lfr(ba$de, ba$ballon2)   
lfr.s(ba$de, ba$ballon2)    

lfr(ba$de, ba$Inflammation_lobulaire_2)   
lfr.s(ba$de, ba$Inflammation_lobulaire_2) 
mod.infl = glm(de ~ Inflammation_lobulaire_2, data = ba, family = "binomial")
lrtest(mod.infl, mod.e)

table(ba$Inflammation_lobulaire_2bin, ba$de)
prop.table(table(ba$Inflammation_lobulaire_2bin, ba$de), margin = 2)*100
chisq.test(ba$Inflammation_lobulaire_2bin, ba$de, correct = F, simulate.p.value = T, 
           B = 2000)
mod.infde = glm(de ~ Inflammation_lobulaire_2bin, data = ba, family = "binomial")
summary(mod.infde)
cbind(exp(coef(mod.infde)), exp(confint.default(mod.infde)))

lfr(ba$de, ba$Activite_3) 
lfr.s(ba$de, ba$Activite_3) 
mod.activ = glm(de ~ Activite_3, data = ba, family = "binomial")
lrtest(mod.activ, mod.e)

table(ba$Activite_3bin, ba$de)
prop.table(table(ba$Activite_3bin, ba$de), margin = 2)*100
chisq.test(ba$Activite_3bin, ba$de, simulate.p.value = T, B = 2000, correct = F)
mod.actdebin = glm(de ~ Activite_3bin, data = ba, family = "binomial")
summary(mod.actdebin)
cbind(exp(coef(mod.actdebin)), exp(confint.default(mod.actdebin)))

lfr(ba$de, ba$Fibrose_METAVIR)
lfr.s(ba$de, ba$Fibrose_METAVIR)  
mod.metavir = glm(de ~ Fibrose_METAVIR, data = ba, family = "binomial")
lrtest(mod.metavir, mod.e)

table(ba$Fibrose_METAVIRbin, ba$de)
prop.table(table(ba$Fibrose_METAVIRbin, ba$de), margin = 2)*100
chisq.test(ba$Fibrose_METAVIRbin, ba$de, correct = F,
           simulate.p.value = T, B = 2000)
mod.metade = glm(de ~ Fibrose_METAVIRbin, data = ba, family = "binomial")
summary(mod.metade)
cbind(exp(coef(mod.metade)), exp(confint.default(mod.metade)))

lfr(ba$de, ba$Fibrose_SAF_4)  
lfr.s(ba$de, ba$Fibrose_SAF_4)  
mod.fibroseSAF = glm(de ~ Fibrose_SAF_4, data = ba, family = "binomial")
lrtest(mod.fibroseSAF, mod.e)

table(ba$Fibrose_SAF_4bin, ba$de)
prop.table(table(ba$Fibrose_SAF_4bin, ba$de), margin = 2)*100
chisq.test(ba$Fibrose_SAF_4bin, ba$de, correct = F,
           simulate.p.value = T, B = 2000)
mod.safde = glm(de ~ Fibrose_SAF_4bin, data = ba, family = binomial)
summary(mod.safde)
cbind(exp(coef(mod.safde)), exp(confint.default(mod.safde)))

lfr(ba$de, ba$NAFLD_5)       
lfr.s(ba$de, ba$NAFLD_5)                                

lfr(ba$de, ba$NASH)   
lfr.s(ba$de, ba$NASH)                                   

lfr(ba$de, ba$SOS_Syndrome)         
lfr.s(ba$de, ba$SOS_Syndrome)                           

lfr(ba$de, ba$noyaux_glycogeniques_)         
lfr.s(ba$de, ba$noyaux_glycogeniques_)                  

lfr(ba$de, ba$complication_aigue_post_op_med)   
lfr.s(ba$de, ba$complication_aigue_post_op_med)         

lfr(ba$de, ba$complication_aigue_chir)      
lfr.s(ba$de, ba$complication_aigue_chir)                

lfr(ba$de, ba$CL2)    
lfr.s(ba$de, ba$CL2)                     

lfr(ba$de, ba$Steatose.bin)    
lfr.s(ba$de, ba$Steatose.bin)                           

lfr(ba$de, ba$CL2)     
lfr.s(ba$de, ba$CL2)     

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

# NEGATIVE BINOMIAL

library("MASS")
library("ggplot2")
library("boot")
library("pscl")

bab = subset(ba, select = c(Score_de_Clavien_7, Age, HTA, Alcool, cirrhose, 
                            type_de_chir_minmaj, Steatose.bin, Mortalite_a_90jours,
                            Activite_3, NASH, NAFLD_5, Hemochromatose, SOS_Syndrome))

str(bab$Steatose.bin)
bab$Steatose.bin = as.numeric(as.character(bab$Steatose.bin))
str(bab$Score_de_Clavien_7)
bab$Score_de_Clavien_7 = as.numeric(as.character(bab$Score_de_Clavien_7))

#-----------------------------------------------------------------------------------------

# histogram with x axis in log10 scale 
ggplot(bab, aes(Score_de_Clavien_7, fill = Steatose.bin)) +
  geom_histogram() +
  scale_x_log10() +
  facet_grid(Steatose.bin ~ ., margins = TRUE, scales = "free_y")

#-----------------------------------------------------------------------------------------

# zero-inflated negative binomial 

m1 = zeroinfl(formula = Score_de_Clavien_7 ~ Age | 1, 
         data = bab, dist = "negbin"#, EM = TRUE
         )
summary(m1)

m0 = update(m1, . ~ 1)

pchisq(2* (logLik(m1) - logLik(m0)), df = (2-1), lower.tail = F)

# estimates

est = cbind(Estimate = coef(m1), confint(m1))
est
dput(round(coef(m1, "count"), 4))
dput(round(coef(m1, "zero"), 4))

f <- function(data, i){
  require(pscl)
  m <- zeroinfl(Score_de_Clavien_7 ~ Age + Alcool, 
                data = data[i,], dist = "negbin",
                start = list(count = c(-3.0107, 0.043, 0.3165), zero = c(0.7925)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

set.seed(10)
res <- boot(bab, f, R = 1200, parallel = "snow", ncpus = 4)

#-----------------------------------------------------------------------------------------
table(ba$Score_de_Clavien_7)
str(ba$Score_de_Clavien_7)
ba$CL7 = ba$Score_de_Clavien_7
ba$CL7 = as.numeric(as.character(ba$CL7))

m1 = glm.nb(CL7 ~ NAFLD_5, data = ba)
summary(m1)

m3 = glm(CL7 ~ NAFLD_5, data = ba, family = "poisson")
2*(logLik(m1) - logLik(m3))
pchisq(2 * (logLik(m1) - logLik(m3)), df = 1, lower.tail = FALSE)

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

# multivariate model

# y = 
table(ba$Steatose.bin)
table(ba$de)
# x = 
table(ba$Activite_3bin)
table(ba$Fibrose_METAVIRbin)
table(ba$Fibrose_SAF_4bin)
table(ba$cirrhose)
table(ba$NAFLD_5)
table(ba$NASH)
table(ba$CL2)

mul.steatose = glm(Steatose.bin ~ 
                     Fibrose_SAF_4bin + 
                     NAFLD_5 + NASH + CL2, data = ba, family = binomial)

library("performance")
library("glmtoolbox")

performance::check_collinearity(mul.steatose)
glmtoolbox::hltest(mul.steatose)

summary(mul.steatose)
cbind(exp(coef(mul.steatose)), exp(confint.default(mul.steatose)))


mul.death = glm(de ~ Activite_3bin + 
                     Fibrose_SAF_4bin +
                     NAFLD_5 #+ NASH + CL2
                , data = ba, family = binomial)
performance::check_collinearity(mul.death)
glmtoolbox::hltest(mul.death)

summary(mul.death)
cbind(exp(coef(mul.death)), exp(confint.default(mul.death)))

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

# CORRELATION
table(ba$Steatose_Qpath)
mean(ba$Steatose_Qpath)

table(ba$Steatose)
mean(ba$Steatose)
quantile(ba$Steatose)

cor(ba$Steatose, ba$Steatose_Qpath)
cor.test(ba$Steatose, ba$Steatose_Qpath, method = "spearman", exact = F)

mod = lm(Steatose_Qpath ~ Steatose, data = ba)
summary(mod)
#y = 0.47846 + 0.17795x

library("ggstatsplot")
ggscatterstats(
  data = ba,
  x = Steatose,
  y = Steatose_Qpath,
  bf.message = FALSE
)


a = 0.47846 
b = 0.17795
y <- ba$Steatose
x <- ba$Steatose_Qpath
plot(x, y, pch = 1, col = "orange", ylab = "Steatosis", xlab = "Qpath",
     cex = 2.2, cex.axis = 1.5, cex.lab = 1.5)
abline(lm(y ~ x), col = "black", lwd = 1)
text(8,37,"y = 0.478 + 0.1785x",srt=33.2,pos=4, cex = 3)
#text(mean(x), b * mean(x) + a, "text", srt=atan(b)/(2*pi)*360, pos=3, col = "blue")
#text(mean(x),(b*max(x)/2+a),"text",srt=33.2,pos=3)
#text(paste("Correlation:", round(cor(x, y), 2)), x = 25, y = 95)

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

# ROC CURVE
str(ba$Steatose.bin)
str(ba$Steatose_Qpath)

library("pROC")

roc.st = roc(ba$Steatose.bin, ba$Steatose_Qpath)
auc(roc.st)

par(xaxs = "i", yaxs = "i", xpd = TRUE, cex.axis = 1.2, cex.lab = 1.2)
plot.roc(roc.st , legacy.axes = TRUE, identity = FALSE, col = "orange", lwd = 1,
         print.auc=TRUE, auc.polygon=TRUE, auc.polygon.col=rgb(.3,0,.8,0.2),
         print.thres.best.method="youden",
         print.thres = "best"
         #, print.thres.pattern = "", 
         #print.thres.cex = 0.3, print.thres.col = "brown"
         , xlab = "1-Specificity (FPR)",
         ylab = "Sensitivity (TPR)")
par(xpd = TRUE)
segments(1, 0, 0, 1)
plot(smooth(roc.st), add=TRUE, col="black", lwd = 0.8)
#legend("bottomright", legend=c("", ""),
#col=c(par("fg"), "black"), lwd=2)


plot(roc.st)
coords(roc.st, x="best", best.method="youden", transpose = TRUE)

plot(roc.st, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="lightblue", print.thres=TRUE)


has.partial.auc(roc.st)

# To plot a different partial AUC, we need to ignore the existing value with reuse.auc=FALSE
plot(roc.st, print.auc=TRUE, auc.polygon=TRUE, partial.auc=c(1, 0.8),
     partial.auc.focus="se", grid=c(0.1, 0.2), grid.col=c("green", "red"),
     max.auc.polygon=TRUE, auc.polygon.col="lightblue",
     print.thres=TRUE, print.thres.adj = c(1, -1),
     reuse.auc=FALSE)