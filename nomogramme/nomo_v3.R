getwd()
setwd("P:/CONSULTATION/Benoit_Alexandra")
# on mac
setwd("/Users/damianocerasuolo/Desktop/UBRC/22_23_CONSULT_MAC/Benoit_Alexandra")

library("readxl")

no <- read_excel("nomodata_sum.xlsx", na = "NA")
warnings()
names(no)
dim(no)
View(no) 

#------------------------------------------------------------------------------

# PACKAGES TO RUN

library("tidyverse")
library("lmtest")
library("leaps")
library("performance")

#------------------------------------------------------------------------------

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

#------------------------------------------------------------------------------

# TABLEONE DESCRIPTIVE

library("tableone")

dput(names(no))

variables = c("Nombredestim",  
              "Indication_re", "Indication_res", "IMC", "Tabac", 
              "CFA", "AMH", "GroupespronostiquesAMH", 
              "GroupesAMHCFA", "Agedebutdestim", "Protocole", 
              "protocolesimple", "Dosededepart", "Dosetotale", "Duree", "nbredovorecueillis", 
              "Nbreovomatures", "Nb_total_ovo_matures")

catvariables = c("Indication_re", "Indication_res", "Tabac", 
              "GroupespronostiquesAMH", 
              "GroupesAMHCFA",  "Protocole", 
              "protocolesimple")

no.des = CreateTableOne(vars = variables, factorVars = catvariables, data = no)
print(no.des, showAllLevels = TRUE, quote = TRUE, noSpaces = TRUE, includeNA = TRUE)

###

# 

table(no$ind)
prop.table(table(no$ind))*100

#------------------------------------------------------------------------------

no$Y = no$Nb_total_ovo_matures

mod = lm(Nb_total_ovo_matures ~ Nombredestim, data = no) 
summary(mod)
confint(mod)

mod2 = lm(Nb_total_ovo_matures ~ Indication_re, data = no)
summary(mod2)
confint(mod2)

mod.e = lm(Y ~ 1, data = no)
summary(mod.e)

##
mod2.data = subset(no, select = c("Nb_total_ovo_matures", "Indication_re"))
dim(mod2.data)
mod2.data = na.omit(mod2.data)
mod.e.2 = lm(Nb_total_ovo_matures ~ 1, data = mod2.data)
mod2 = lm(Nb_total_ovo_matures ~ Indication_re, data = mod2.data)
lrtest(mod2, mod.e.2)
##

mod3 = lm(Nb_total_ovo_matures ~ Indication_res, data = no)
summary(mod3)
confint(mod3)
##
mod3.data = subset(no, select = c("Nb_total_ovo_matures", "Indication_res"))
dim(mod3.data)
mod3.data = na.omit(mod3.data)
mod.e.3 = lm(Nb_total_ovo_matures ~ 1, data = mod3.data)
mod3 = lm(Nb_total_ovo_matures ~ Indication_res, data = mod3.data)
lrtest(mod3, mod.e.3)
##

mod4 = lm(Y ~ IMC, data = no)
summary(mod4)
confint(mod4)

no$Tabac = as.factor(no$Tabac)
mod5 = lm(Y ~ Tabac, data = no)
summary(mod5)
confint(mod5)
lrtest(mod5, mod.e)

mod6 = lm(Y ~ CFA, data = no)
summary(mod6)
confint(mod6)

mod7 = lm(Y ~ AMH, data = no)
summary(mod7)
confint(mod7)

no$GroupespronostiquesAMH = as.factor(no$GroupespronostiquesAMH)
mod8 = lm(Y ~ GroupespronostiquesAMH, data = no)
summary(mod8)
confint(mod8)
lrtest(mod8, mod.e)

no$GroupesAMHCFA = as.factor(no$GroupesAMHCFA)
mod9 = lm(Y ~ GroupesAMHCFA, data = no)
summary(mod9)
confint(mod9)
lrtest(mod9, mod.e)

mod10 = lm(Y ~ Agedebutdestim, data = no)
summary(mod10)
confint(mod10)

mod11 = lm(Y ~ Protocole, data = no)
summary(mod11)
confint(mod11)
lrtest(mod11, mod.e)

no$protocolesimple = as.factor(no$protocolesimple)
mod12 = lm(Y ~ protocolesimple, data = no)
summary(mod12)
confint(mod12)

mod13 = lm(Y ~ Dosededepart, data = no)
summary(mod13)
confint(mod13)

mod14 = lm(Y ~ Dosetotale, data = no)
summary(mod14)
confint(mod14)

mod15 = lm(Y ~ Duree, data = no)
summary(mod15)
confint(mod15)

mod16 = lm(Y ~ nbredovorecueillis, data = no)
summary(mod16)
confint(mod16)

mod.ind = lm(Y ~ ind, data = no)
summary(mod.ind)
confint(mod.ind)

##
data.ind = subset(no, select = c(Nb_total_ovo_matures, ind))
data.indNA = na.omit(data.ind)
dim(data.indNA)
mod.indNA = lm(Nb_total_ovo_matures ~ ind, data = data.indNA)
mod.ind.e = lm(Nb_total_ovo_matures ~ 1, data = data.indNA)
lrtest(mod.indNA, mod.ind.e)
##

#------------------------------------------------------------------------------

nos = subset(no, 
             select = 
              c(
               "Nombredestim", "IMC", "Tabac", "CFA", "AMH", "GroupespronostiquesAMH", 
                "GroupesAMHCFA", "Agedebutdestim",  
                "protocolesimple", "Dosededepart", "Dosetotale", "Duree", "nbredovorecueillis", 
                "Nb_total_ovo_matures", "Indication_res"))

regfit.full = regsubsets(Nb_total_ovo_matures ~ ., nos)
summary(regfit.full)

reg.summary = summary(regfit.full)
names(reg.summary)

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab = "Number of vars", ylab = "RSS", type = "l") # RSS (residual sum of squares = 
# level of variance in the error term
plot(reg.summary$adjr2, xlab = "Number of vars", ylab = "Adjusted R squared", type = "l") #R2
which.max(reg.summary$adjr2)
# 4

points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], 
       col = "red", cex = 2, pch = 20) # which.max = maximizing function
plot(reg.summary$cp, xlab = "Number of vars", ylab = "Cp", type = "l")
which.min(reg.summary$cp)

# where Cp is the Mallows's Cp
# it can be defined as a parameter to select the number of the variables 
# to be included in the model - where the regression model has been estimated
# using ordinary least squares
# it can be calculated as (see Wiki for formulas) https://en.wikipedia.org/wiki/Mallows%27s_Cp 

points(which.min(reg.summary$cp), reg.summary$adjr2[which.min(reg.summary$cp)], 
       col = "red", cex = 2, pch = 20)
plot(reg.summary$bic, xlab = "Number of vars", ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(which.min(reg.summary$bic), reg.summary$adjr2[which.min(reg.summary$bic)], 
       col = "red", cex = 2, pch = 20)

# leaps plotting

plot(regfit.full, scale ="r2")
plot(regfit.full, scale ="bic")
plot(regfit.full, scale ="adjr2")
plot(regfit.full, scale ="Cp")

# reg coef according to variables included n = X
coef(regfit.full, 8)

# backward or foward selection
regfit.bkw = regsubsets(Nb_total_ovo_matures ~ ., data = nos, nvmax = 8, 
                        #force.in = nos$Nombre_de_stim, 
                        method ="backward")
summary(regfit.bkw)

#Nombredestim IMC Tabac1 Tabac2 CFA AMH GroupespronostiquesAMH2 GroupespronostiquesAMH3 GroupesAMHCFA1 GroupesAMHCFA2
#1  ( 1 ) " "          " " " "    " "    " " " " " "                     " "                     " "            " "           
#2  ( 1 ) "*"          " " " "    " "    " " " " " "                     " "                     " "            " "           
#3  ( 1 ) "*"          " " " "    " "    " " " " " "                     " "                     " "            " "           
#4  ( 1 ) "*"          " " " "    " "    " " " " " "                     " "                     " "            " "           
#5  ( 1 ) "*"          " " " "    " "    " " " " " "                     " "                     " "            "*"           
#6  ( 1 ) "*"          " " " "    " "    " " " " " "                     " "                     "*"            "*"           
#7  ( 1 ) "*"          " " " "    " "    " " " " " "                     " "                     "*"            "*"           
#8  ( 1 ) "*"          " " " "    " "    " " " " " "                     " "                     "*"            "*"           
#GroupesAMHCFA3 Agedebutdestim protocolesimpleantagoniste + letro Dosededepart Dosetotale Duree nbredovorecueillis
#1  ( 1 ) " "            " "            " "                                " "          " "        " "   "*"               
#2  ( 1 ) " "            " "            " "                                " "          " "        " "   "*"               
#3  ( 1 ) " "            " "            " "                                " "          " "        " "   "*"               
#4  ( 1 ) "*"            " "            " "                                " "          " "        " "   "*"               
#5  ( 1 ) "*"            " "            " "                                " "          " "        " "   "*"               
#6  ( 1 ) "*"            " "            " "                                " "          " "        " "   "*"               
#7  ( 1 ) "*"            "*"            " "                                " "          " "        " "   "*"               
#8  ( 1 ) "*"            "*"            " "                                " "          " "        " "   "*"               
#Indication_resBaisse de la RO Indication_resCancer Indication_resEndometriose Indication_resIOP fam
#1  ( 1 ) " "                           " "                  " "                        " "                  
#2  ( 1 ) " "                           " "                  " "                        " "                  
#3  ( 1 ) " "                           "*"                  " "                        " "                  
#4  ( 1 ) " "                           "*"                  " "                        " "                  
#5  ( 1 ) " "                           "*"                  " "                        " "                  
#6  ( 1 ) " "                           "*"                  " "                        " "                  
#7  ( 1 ) " "                           "*"                  " "                        " "                  
#8  ( 1 ) "*"                           "*"                  " "                        " "                  
#Indication_resMal Autoimmune Indication_resMutation genetique Indication_resMyomectomie
#1  ( 1 ) " "                          " "                              " "                      
#2  ( 1 ) " "                          " "                              " "                      
#3  ( 1 ) " "                          " "                              " "                      
#4  ( 1 ) " "                          " "                              " "                      
#5  ( 1 ) " "                          " "                              " "                      
#6  ( 1 ) " "                          " "                              " "                      
#7  ( 1 ) " "                          " "                              " "                      
#8  ( 1 ) " "                          " "                              " "             

# VARIABLE TO BE INCLUDED IN THE MODEL 
# Nombredestim ~ 8
# GroupesAMHCFA ~ 5
# Agedebutdestim ~ 2
# nbredovorecueillis ~ 8
# Indication_res ~ 6

table(nos$Indication_res)

nos <- nos %>% 
  mutate(ind = case_when(
    Indication_res == "ATCD KO" | Indication_res == "Cancer" ~ "3K", # cancer and history of cancer
    Indication_res == "Baisse de la RO" ~ "2Baisse_RO", 
    Indication_res == "Endometriose" ~ "1Endometriose",
    Indication_res == "IOP fam" | Indication_res == "Mal Autoimmune" | 
      Indication_res == "Mutation genetique" |
      Indication_res == "Myomectomie" ~ "4Autre"
    ))

table(nos$ind)

# MULTIVARIATE MODEL 

mod.multi = lm(Nb_total_ovo_matures ~ Agedebutdestim + Nombredestim + GroupesAMHCFA + 
                 ind + nbredovorecueillis, data = nos)
summary(mod.multi)
confint(mod.multi)

#Call:
#  lm(formula = Nb_total_ovo_matures ~ Agedebutdestim + Nombredestim + 
#       GroupesAMHCFA + ind + nbredovorecueillis, data = nos)
#
# Residuals:
# Min       1Q   Median       3Q      Max 
# -21.8424  -1.8777  -0.0365   1.6876  19.9049 
#
# Coefficients:
#                       Estimate   Std.Error t value  Pr(>|t|)    
#  (Intercept)        -10.38346    2.36386  -4.393    1.27e-05 ***
#  Agedebutdestim       0.16314    0.06681   2.442    0.0148 *  
#  Nombredestim         5.42124    0.22016  24.624    < 2e-16 ***
#  GroupesAMHCFA1       1.63839    0.63495   2.580    0.0100 *  
#  GroupesAMHCFA2       2.41798    0.54584   4.430    1.07e-05 ***
#  GroupesAMHCFA3       2.76551    0.44365   6.234    7.26e-10 ***
#  indBaisse_RO        -1.12995    0.74266  -1.521    0.1285    
#  indEndometriose     -0.72243    0.66939  -1.079    0.2808    
#  indK                -2.73155    0.68530  -3.986    7.31e-05 ***
#  nbredovorecueillis   0.73224    0.02364  30.977    < 2e-16 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 4.073 on 829 degrees of freedom
# (8 observations effacées parce que manquantes)
# Multiple R-squared:  0.6791,	Adjusted R-squared:  0.6756 
# F-statistic: 194.9 on 9 and 829 DF,  p-value: < 2.2e-16

# MODEL CHECK

check_collinearity(mod.multi)
#Term               VIF   VIF 95% CI Increased SE Tolerance Tolerance 95% CI
#Agedebutdestim     1.06 [1.02, 1.21]         1.03      0.94     [0.83, 0.98]
#Nombredestim       1.27 [1.19, 1.40]         1.13      0.78     [0.71, 0.84]
#GroupesAMHCFA      1.74 [1.59, 1.92]         1.32      0.58     [0.52, 0.63]
#ind                1.60 [1.47, 1.76]         1.26      0.63     [0.57, 0.68]
#nbredovorecueillis 1.44 [1.33, 1.58]         1.20      0.70     [0.63, 0.75]

par(mfrow = c(2,2))
plot(mod.multi)

mod.multi.e1 = lm(Nb_total_ovo_matures ~ Agedebutdestim + Nombredestim +  
                 ind + nbredovorecueillis, data = nos)

lrtest(mod.multi, mod.multi.e1)

##
data.m2 = subset(nos, select = c("Nb_total_ovo_matures", "Agedebutdestim",
                                 "Nombredestim","GroupesAMHCFA", 
                                   "ind", "nbredovorecueillis"))
data.m3 = na.omit(data.m2)

mod.multi.NA = lm(Nb_total_ovo_matures ~ Agedebutdestim + Nombredestim + GroupesAMHCFA + 
                 ind + nbredovorecueillis, data = data.m3) 

mod.multi.e2 = lm(Nb_total_ovo_matures ~ Agedebutdestim + Nombredestim + GroupesAMHCFA + 
                 nbredovorecueillis, data = data.m3)
lrtest(mod.multi.NA, mod.multi.e2)
##
