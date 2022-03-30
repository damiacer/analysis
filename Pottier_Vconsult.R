### folder
getwd()
setwd("P:/UBRC_Consultatations/Pottier_Veronique")
getwd()


### database
dx <- read.csv2("potver_db_v2.csv", header = TRUE, na.string=".")

names(dx)

# tableone

###############################################################################################


### description by tableone
library(tableone)

### create tableone
CreateTableOne(data = dx)

### names of the vars
dput(names(dx))

### new variables or recoded variables 

# SOFA_implantation
# median value == 3

str(dx$SOFA_implantation)
table(dx$SOFA_implantation)
mean(dx$SOFA_implantation, na.rm=T)
median(dx$SOFA_implantation)
dx$SOFA_implantation <- as.numeric(as.character(dx$SOFA_implantation))
dx$SOFA_implantation_2[dx$SOFA_implantation<="3"] <- "0"
dx$SOFA_implantation_2[dx$SOFA_implantation>"3"] <- "1"
table(dx$SOFA_implantation_2)

# age
# median value == 64 (mean == 60)

str(dx$age)
dx$age <- as.numeric(as.character(dx$age))
dx$age_2[dx$age<="64"] <- "0"
dx$age_2[dx$age>"64"] <- "1"
table(dx$age_2)

# dureesejour
# median value == 22 (mean == 33)
dx$dureesejour <- as.numeric(as.character(dx$dureesejour))
dx$dureesejour_2[dx$dureesejour<="22"] <- "0"
dx$dureesejour_2[dx$dureesejour>"22"] <- "1"
table(dx$dureesejour_2)

### variables

myVars <- c("sexe", 
            "age", 
            "age_2",
            "dureesejour", 
            "dureesejour_2",
            "CMI_CMD", 
            "deces", 
            "taille_cm", #quantitative
            "poids_kg", #quantitative
            "SC_m2", #quantitative
            "SAPS_II", #quantitative
            "NYHA", #4 classes
            "INTER_MACS", #4 classes
            "BTT_DT", 
            "TYPE_LVAD", 
            "redux", 
            "VM_Av", 
            "EER_Av", 
            "OndeS_Av", #quantitative
            "TAPSE_Av", #quantitative
            "VD_VG_Av",
            "SP_Av", 
            "VCI_sup20mm", 
            "IC_KtDt", #quantitative
            "IT", 
            "BNP_Av", #quantitative
            "BiliT_AvSup20",
            "TGOxN", #quantitative
            "IplusAv", 
            "Inh_PDE_Av", 
            "Levo_Av", 
            "Amines_Av",
            "SOFA_implantation_2", #in 2 classes after recoding
            "SOFA_implantation",
            "Assistance_preop", 
            "oragerythmique_preop", 
            "duree_VM_ap_j", #quantitative
            "EER_ap", 
            "KDIGO", 
            "Onde_S_ap", #quantitative
            "TAPSE_Ap", #quantitative
            "VD__VG_sup1_Ap",
            "SP_Ap", 
            "BNP_Ap", #quantitative
            "Bili_T_Ap_sup20", 
            "TGOx_N_2", #quantitative
            "IplusAp_2", 
            "duree_Iplus_sup14j", 
            "Inh_PDE_Ap", 
            "Amines_Ap_2", 
            "PVC", #quantitative
            "NO_sup48h", 
            "Assistance_dte", 
            "droite_outcome")

catVars <- c("sexe", 
             "age_2", 
             "dureesejour_2",
             "CMI_CMD", 
             "deces", 
             "NYHA", #4 classes
             "INTER_MACS", #4 classes
             "BTT_DT", 
             "TYPE_LVAD", 
             "redux", 
             "VM_Av", 
             "EER_Av", 
             "VD_VG_Av",
             "SP_Av", 
             "VCI_sup20mm", 
             "IT", 
             "BiliT_AvSup20",
             "IplusAv", 
             "Inh_PDE_Av", 
             "Levo_Av", 
             "Amines_Av",
             "SOFA_implantation_2", 
             "Assistance_preop", 
             "oragerythmique_preop", 
             "EER_ap", 
             "KDIGO", 
             "VD__VG_sup1_Ap",
             "SP_Ap", 
             "Bili_T_Ap_sup20", 
             "IplusAp_2", 
             "duree_Iplus_sup14j", 
             "Inh_PDE_Ap", 
             "Amines_Ap_2", 
             "NO_sup48h", 
             "Assistance_dte", 
             "droite_outcome")

### TAB 1 = no strata
### show all levels of a categorial variable
tab1 <- CreateTableOne(vars = myVars, data = dx, factorVars = catVars)
print(tab1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

### TAB 2 = strata
### add test = FALSE to avoid testing, test = TRUE to test
### add strata to stratify on the interest variable
tab2 <- CreateTableOne(vars = myVars, data = dx, factorVars = catVars, test = TRUE, strata = "droite_outcome")
print(tab2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)


# univariate
###############################################################################################

### UNIVARIATE

str(dx$droite_outcome)
dx$droite_outcome <- as.numeric(as.character(dx$droite_outcome))

# sexe
fisher.test(dx$sexe, dx$droite_outcome)

# age and age_2
str(dx$age)
hist(dx$age)
shapiro.test(dx$age)
wilcox.test(dx$age ~ dx$droite_outcome, correct=FALSE)
#t.test(dx$age, dx$droite_outcome, paired=FALSE, conf.level=0.95)

chisq.test(dx$age, dx$droite_outcome, correct=FALSE)
fisher.test(dx$age, dx$droite_outcome)

# dureesejour and dureesejour_2
str(dx$dureesejour)
hist(dx$dureesejour)
shapiro.test(dx$dureesejour)
wilcox.test(dx$dureesejour ~ dx$droite_outcome, correct=FALSE)

chisq.test(dx$dureesejour_2, dx$droite_outcome, correct=FALSE)

# CMI_CMD
fisher.test(dx$CMI_CMD, dx$droite_outcome)

# deces
fisher.test(dx$deces, dx$droite_outcome)

# taille_cm
str(dx$taille_cm)
dx$taille_cm <- as.numeric(as.character(dx$taille_cm))
hist(dx$taille_cm)
shapiro.test(dx$taille_cm)
t.test(dx$taille_cm, dx$droite_outcome, paired=FALSE, conf.level=0.95)

# poids_kg
str(dx$poids_kg)
dx$poids_kg <- as.numeric(as.character(dx$poids_kg))
hist(dx$poids_kg)
shapiro.test(dx$poids_kg)
t.test(dx$poids_kg, dx$droite_outcome, paired=FALSE, conf.level=0.95)

# SC_m2
str(dx$SC_m2)
dx$SC_m2 <- as.numeric(as.character(dx$SC_m2))
hist(dx$SC_m2)
shapiro.test(dx$SC_m2)
t.test(dx$SC_m2, dx$droite_outcome, paired=FALSE, conf.level=0.95)
#wilcox.test(dx$SC_m2 ~ dx$droite_outcome, correct=FALSE)

# SAPS_II
str(dx$SAPS_II)
dx$OndeS_Av <- as.numeric(as.character(dx$SAPS_II))
hist(dx$SAPS_II)
shapiro.test(dx$SAPS_II)
#t.test(dx$SAPS_II, dx$droite_outcome, paired=FALSE, conf.level=0.95)
wilcox.test(dx$SAPS_II ~ dx$droite_outcome, correct=FALSE)

# NYHA
chisq.test(dx$NYHA, dx$droite_outcome, correct = FALSE, simulate.p.value = TRUE)

# INTER_MACS
chisq.test(dx$INTER_MACS, dx$droite_outcome, correct = FALSE, simulate.p.value = TRUE)

# BTT_DT
chisq.test(dx$BTT_DT, dx$droite_outcome, correct = FALSE)

# TYPE_LVAD
chisq.test(dx$TYPE_LVAD, dx$droite_outcome, correct = FALSE, simulate.p.value = TRUE)

# redux
fisher.test(dx$redux, dx$droite_outcome)

# VM_Av
fisher.test(dx$VM_Av, dx$droite_outcome)

# EER_Av
fisher.test(dx$EER_Av, dx$droite_outcome)

# OndeS_Av
str(dx$OndeS_Av)
dx$OndeS_Av <- as.numeric(as.character(dx$OndeS_Av))
hist(dx$OndeS_Av)
shapiro.test(dx$OndeS_Av)
#t.test(dx$OndeS_Av, dx$droite_outcome, paired=FALSE, conf.level=0.95)
wilcox.test(dx$OndeS_Av ~ dx$droite_outcome, correct=FALSE)

# TAPSE_Av
str(dx$TAPSE_Av)
dx$TAPSE_Av <- as.numeric(as.character(dx$TAPSE_Av))
hist(dx$TAPSE_Av)
shapiro.test(dx$TAPSE_Av)
#t.test(dx$TAPSE_Av, dx$droite_outcome, paired=FALSE, conf.level=0.95)
wilcox.test(dx$TAPSE_Av ~ dx$droite_outcome, correct=FALSE)

# VD_VG
fisher.test(dx$VD_VG, dx$droite_outcome)

# VCI_sup20mm
fisher.test(dx$VCI_sup20mm, dx$droite_outcome)

# IC_KtDt
str(dx$IC_KtDt)
dx$IC_KtDt <- as.numeric(as.character(dx$IC_KtDt))
hist(dx$IC_KtDt)
shapiro.test(dx$IC_KtDt)
#t.test(dx$IC_KtDt, dx$droite_outcome, paired=FALSE, conf.level=0.95)
wilcox.test(dx$IC_KtDt ~ dx$droite_outcome, correct=FALSE)

# IT
chisq.test(dx$IT, dx$droite_outcome, correct = FALSE, simulate.p.value = TRUE)

# BNP_Av
str(dx$BNP_Av)
dx$BNP_Av <- as.numeric(as.character(dx$BNP_Av))
hist(dx$BNP_Av)
shapiro.test(dx$BNP_Av)
#t.test(dx$BNP_Av, dx$droite_outcome, paired=FALSE, conf.level=0.95)
wilcox.test(dx$BNP_Av ~ dx$droite_outcome, correct=FALSE)

# BiliT_AvSup20
chisq.test(dx$BiliT_AvSup20, dx$droite_outcome, correct = FALSE)

# TGOxN
str(dx$TGOxN)
dx$TGOxN <- as.numeric(as.character(dx$TGOxN))
hist(dx$TGOxN)
shapiro.test(dx$TGOxN)
#t.test(dx$TGOxN, dx$droite_outcome, paired=FALSE, conf.level=0.95)
wilcox.test(dx$TGOxN ~ dx$droite_outcome, correct=FALSE)

# IplusAv
fisher.test(dx$IplusAv, dx$droite_outcome)

# Inh_PDE_Av
fisher.test(dx$Inh_PDE_Av, dx$droite_outcome)

# Levo_Av
fisher.test(dx$Levo_Av, dx$droite_outcome)
chisq.test(dx$Levo_Av, dx$droite_outcome, correct = FALSE)

# Amines_Av
fisher.test(dx$Amines_Av, dx$droite_outcome)

# SOFA_implantation_2
chisq.test(dx$SOFA_implantation_2, dx$droite_outcome, correct = FALSE)
# SOFA
wilcox.test(dx$SOFA_implantation ~ dx$droite_outcome, correct=FALSE)

# Assistance_preop
chisq.test(dx$Assistance_preop, dx$droite_outcome, correct = FALSE)

# oragerythmique_preop
chisq.test(dx$oragerythmique_preop, dx$droite_outcome, correct = FALSE)

# duree_VM_ap_j
str(dx$duree_VM_ap_j)
dx$duree_VM_ap_j <- as.numeric(as.character(dx$duree_VM_ap_j))
hist(dx$duree_VM_ap_j)
shapiro.test(dx$duree_VM_ap_j)
#t.test(dx$duree_VM_ap_j, dx$droite_outcome, paired=FALSE, conf.level=0.95)
wilcox.test(dx$duree_VM_ap_j ~ dx$droite_outcome, correct=FALSE)

# EER_ap
chisq.test(dx$EER_ap, dx$droite_outcome, correct = FALSE)

# Onde_S_ap
str(dx$Onde_S_ap)
dx$Onde_S_ap <- as.numeric(as.character(dx$Onde_S_ap))
hist(dx$Onde_S_ap)
shapiro.test(dx$Onde_S_ap)
t.test(dx$Onde_S_ap, dx$droite_outcome, paired=FALSE, conf.level=0.95)
#wilcox.test(dx$Onde_S_ap ~ dx$droite_outcome, correct=FALSE)

# TAPSE_Ap
str(dx$TAPSE_Ap)
dx$TAPSE_Ap <- as.numeric(as.character(dx$TAPSE_Ap))
hist(dx$TAPSE_Ap)
shapiro.test(dx$TAPSE_Ap)
t.test(dx$TAPSE_Ap, dx$droite_outcome, paired=FALSE, conf.level=0.95)
#wilcox.test(dx$TAPSE_Ap ~ dx$droite_outcome, correct=FALSE)

# VD__VG_sup1_Ap
fisher.test(dx$VD__VG_sup1_Ap, dx$droite_outcome)

# SP_Ap
fisher.test(dx$SP_Ap, dx$droite_outcome)

# BNP_Ap
str(dx$BNP_Ap)
dx$BNP_Ap <- as.numeric(as.character(dx$BNP_Ap))
hist(dx$BNP_Ap)
shapiro.test(dx$BNP_Ap)
#t.test(dx$BNP_Ap, dx$droite_outcome, paired=FALSE, conf.level=0.95)
wilcox.test(dx$BNP_Ap ~ dx$droite_outcome, correct=FALSE)

# Bili_T_Ap_sup20
fisher.test(dx$Bili_T_Ap_sup20, dx$droite_outcome)

# TGOx_N_2
str(dx$TGOx_N_2)
dx$TGOx_N_2 <- as.numeric(as.character(dx$TGOx_N_2))
hist(dx$TGOx_N_2)
shapiro.test(dx$TGOx_N_2)
#t.test(dx$TGOx_N_2, dx$droite_outcome, paired=FALSE, conf.level=0.95)
wilcox.test(dx$TGOx_N_2 ~ dx$droite_outcome, correct=FALSE)

# IplusAp_2
fisher.test(dx$IplusAp_2, dx$droite_outcome)

# duree_Iplus_sup14j
fisher.test(dx$duree_Iplus_sup14j, dx$droite_outcome)

# Inh_PDE_Ap
chisq.test(dx$Inh_PDE_Ap, dx$droite_outcome, correct = FALSE)

# Amines_Ap_2
fisher.test(dx$Amines_Ap_2, dx$droite_outcome)

# PVC
str(dx$PVC)
dx$PVC <- as.numeric(as.character(dx$PVC))
hist(dx$PVC)
shapiro.test(dx$PVC)
t.test(dx$PVC, dx$droite_outcome, paired=FALSE, conf.level=0.95)
#wilcox.test(dx$PVC ~ dx$droite_outcome, correct=FALSE)

# NO_sup48h
fisher.test(dx$NO_sup48h, dx$droite_outcome)

# Assistance_dte
fisher.test(dx$Assistance_dte, dx$droite_outcome)

###############################################################################################

table(dx$droite_outcome, dx$EER_Av)


model_droite <- glm(droite_outcome ~ Amines_Av + 
                                   EER_Av + 
                                   OndeS_Av + CMI_CMD,
                                   data = dx, family = binomial(link = "logit"))

summary(model_droite)

exp(cbind(OR = coef(model_droite), confint(model_droite)))



model_droite2 <- glm(droite_outcome ~ Amines_Av + 
                         #EER_Av + 
                         OndeS_Av + CMI_CMD,
                    data = dx, family = binomial(link = "logit"))

summary(model_droite2)

exp(cbind(OR = coef(model_droite2), confint(model_droite2)))

