getwd()
setwd("/Users/damianocerasuolo/Desktop/UBRC/21_22_CONSULT_MAC/Gueutin_Victor")
setwd("P:/CONSULTATION/Gueutin_Victor")

library("readxl")

gu <- read_excel("BaseGBM2020_DCMOD.xlsx", na = "NA")
warnings()
names(gu)
#View(gu)

############################################################################

library("tableone")

dput(names(gu))

nf <- function(x){
	result = as.numeric(as.character(x))
	return(result)
	}

gu$bmi = nf(gu$bmi)
gu$age_diag = nf(gu$age_diag)
gu$antiMBG_taux = nf(gu$antiMBG_taux)
gu$SCR = nf(gu$SCR)
gu$protu_mgL = nf(gu$protu_mgL)
gu$creat_mmol_L = nf(gu$creat_mmol_L)
gu$prot_creat = nf(gu$prot_creat)
gu$leucour_mm3 = nf(gu$leucour_mm3)
gu$albumine = nf(gu$albumine)
gu$Hg = nf(gu$Hg)
gu$CRP = nf(gu$CRP)
gu$C3 = nf(gu$C3)
gu$C4 = nf(gu$C4)
gu$corticoide_kgbolus = nf(gu$corticoide_kgbolus)
gu$CYC_dosebolus = nf(gu$CYC_dosebolus)
gu$CYC_mgbolus = nf(gu$CYC_mgbolus)
gu$CYC_PO_mgkgJ = nf(gu$CYC_PO_mgkgJ)
gu$albumine = nf(gu$albumine)
gu$prot_creat = nf(gu$prot_creat)
gu$creat_mmol_L = nf(gu$creat_mmol_L)
gu$protu_mgL = nf(gu$protu_mgL)
gu$SCR = nf(gu$SCR)

variables = c("sex", "bmi", "diabete", 
"hta", "type_atteinte_rein", "risk_cv", "neoplasie", "age_diag", "oms", "charlson", "CYC", 
"CYC_PO_mgkgJ", "RTX", "ESRD_M1", "dialyse_M1", "ESRD_M6", "dialyse_M12", "DCD_M12")

qualivar = c("sex", "diabete", 
"hta", "type_atteinte_rein", "risk_cv", "neoplasie", "oms", "charlson", "CYC", "RTX", "ESRD_M1", "dialyse_M1", "ESRD_M6", "dialyse_M12", "DCD_M12")


gu.des = CreateTableOne(vars = variables, data = gu, factorVars = qualivar)
print(gu.des, showAllLevels = TRUE, quote = TRUE, noSpaces = TRUE, includeNA = TRUE)  

gu.des4 = CreateTableOne(vars = variables, data = gu, factorVars = qualivar, test = FALSE, includeNA = TRUE, 
                         strata = "tt_etude_4classes")
print(gu.des4, showAllLevels = FALSE, quote = TRUE, noSpaces = TRUE)

gu.des3 = CreateTableOne(vars = variables, data = gu, factorVars = qualivar, test = FALSE, includeNA = TRUE, 
                         strata = "tt_etude_3classes")
print(gu.des3, showAllLevels = FALSE, quote = TRUE, noSpaces = TRUE)

############################################################################

# fonction test


w <- function(x){
  #result = shapiro.test(x)
  result = wilcox.test(x ~ y, paired=F, exact=F, 
                       correct=F,  alternative = "two.sided")
  #result = t.test(x, y, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
  return(result)
}

t <- function(x){
  #result = shapiro.test(x)
  #result = wilcox.test(x ~ y, paired=F, exact=F, 
  #                     correct=F,  alternative = "two.sided")
  result = t.test(x, y, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
  return(result)
}

k <- function(x){
  result = chisq.test(x, gu$tt_etude_4classes, correct =FALSE, simulate.p.value = TRUE, B = 10000)
  return(result)
}

k2 <- function(x){
  result = chisq.test(x, gu$tt_etude_3classes, correct =FALSE, simulate.p.value = TRUE, B = 10000)
  return(result)
}

############################################################################

# univariate analysis for tt_etude_4classes

k(gu$sex)
kruskal.test(gu$bmi, gu$tt_etude_4classes)
k(gu$diabete)
k(gu$hta)
prop.table(table(gu$type_atteinte_rein, gu$tt_etude_4classes), margin = 2)*100
k(gu$type_atteinte_rein)
k(gu$risk_cv) 
k(gu$neoplasie)
kruskal.test(gu$age_diag, gu$tt_etude_4classes)
k(gu$oms) 
k(gu$charlson)
k(gu$CYC) 
k(gu$RTX) 
k(gu$ESRD_M1)
k(gu$dialyse_M1)
k(gu$ESRD_M6)
k(gu$dialyse_M12)
k(gu$DCD_M12)

############################################################################

k2(gu$sex)
kruskal.test(gu$bmi, gu$tt_etude_3classes)
k2(gu$diabete)
k2(gu$hta)
prop.table(table(gu$type_atteinte_rein, gu$tt_etude_3classes), margin = 2)*100
k2(gu$type_atteinte_rein)
k2(gu$risk_cv) 
k2(gu$neoplasie)
kruskal.test(gu$age_diag, gu$tt_etude_3classes)
k2(gu$oms) 
k2(gu$charlson)
k2(gu$CYC) 
k2(gu$RTX) 
k2(gu$ESRD_M1)
k2(gu$dialyse_M1)
k2(gu$ESRD_M6)
k2(gu$dialyse_M12)
k2(gu$DCD_M12)
