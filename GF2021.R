# FOLDER

# SOME REFERENCES
# http://r-statistics.co/Linear-Regression.html

getwd()
setwd("P:/CONSULTATION/Garnier_Francois") # On PC
setwd("/Users/damianocerasuolo/Desktop/UBRC/2021_CONSULT/Garnier_Francois") # On Mac

install.packages("readxl")
library("readxl")
library("tidyverse")

# DATASET

fg <- read_excel("fgarnier.xlsx", na="NA")

names(fg)
View(fg)

# RENAME COLUMNS
# Note: not necessary on MacBook

fg <- as_tibble(fg)
fg <- fg %>% rename(
  # new name = old name,
  "LDL_2016" = "LDL _2016",
  "LDL_2019" = "LDL _2019")

################################################################################

str(fg$LDL_2016)
mean(fg$LDL_2016, rm.na = TRUE)

str(fg$LDL_2019)
mean(fg$LDL_2019, rm.na = TRUE)

################################################################################

# OUTLIERS CHECK 

par(mfrow=c(1, 2))  
boxplot(fg$VVV, main="", sub=paste("Outlier rows: ", boxplot.stats(fg$VVV)$out)) 

#------------------------------------------------------------------------------#

# DENSITY PLOT 
# CHECK IF RESPONSE VARIABLE IS CLOSE TO NORMALITY
library(e1071)
par(mfrow=c(1, 2))  
plot(density(fg$VVV), main="Density Plot: VVV", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(fg$VVV), 2)))  

################################################################################


# MODELE LINEAIRE 

model1 <- lm(y ~ x, data = fg)
model2 <- lm(y ~ x, na.action = na.omit, data = fg)
model3 <- lm(y ~ x, na.action = na.fail, data = fg)

#------------------------------------------------------------------------------#

exp(cbind(coef(model1), confint(model1)))  

#------------------------------------------------------------------------------#

modelSummary <- summary(model1)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["VVV", "Estimate"]  # get beta estimate for VVV
std.error <- modelCoeffs["VVV", "Std. Error"]  # get std.error for VVV
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(VVV1)-ncol(VVV2))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # f statistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

################################################################################

library(tableone)
dput(names(fg))

#------------------------------------------------------------------------------#

CreateTableOne(data = fg) 

fg$IMC_2016 <- as.numeric(as.character(fg$IMC_2016))
fg$IMC_2019 <- as.numeric(as.character(fg$IMC_2019))

fg$hypercontrole[fg$hypercholestcontrolee_2016=="1" & fg$hypercholestcontrolee_2019=="0"] <- "0" # controlee 2016 -> non controlee 2019
fg$hypercontrole[fg$hypercholestcontrolee_2016=="0" & fg$hypercholestcontrolee_2019=="0"] <- "2" # pas controlee pas changee
fg$hypercontrole[fg$hypercholestcontrolee_2016=="0" & fg$hypercholestcontrolee_2019=="1"] <- "2" 
fg$hypercontrole[fg$hypercholestcontrolee_2016=="1" & fg$hypercholestcontrolee_2019=="1"] <- "1" # controlee pas changee

table(fg$hypercontrole, fg$hypercholestcontrolee_2019)

fg$hypercontroleNA[fg$hypercholestcontrolee_2016=="1" & fg$hypercholestcontrolee_2019=="0"] <- "0" # controlee 2016 -> non controlee 2019
fg$hypercontroleNA[fg$hypercholestcontrolee_2016=="0" & fg$hypercholestcontrolee_2019=="0"] <- "2" 
fg$hypercontroleNA[fg$hypercholestcontrolee_2016=="0" & fg$hypercholestcontrolee_2019=="1"] <- "2" 
fg$hypercontroleNA[fg$hypercholestcontrolee_2016=="1" & fg$hypercholestcontrolee_2019=="1"] <- "1" # controlee pas changee

fg2<-fg[!(fg$hypercontroleNA=="2"),]
CreateTableOne(data = fg2) 

#------------------------------------------------------------------------------#

variables = c("MT_2016", "age", "hta_2016", "diab_2016", 
              "cancer_2016", "cardiopathisch_2016", "aomi_2016", "avc_2016", 
              "SBP1AN_2016", "DBP1AN_2016", "tabac_2016", "CT_2016", "LDL_2016", 
              "HDL_2016", "HbA1c_2016", "IMC_2016", "HTAdefcompl_2016", "tttdiab_2016", 
              "diabdefcompl_2016", "tttdyslipid_2016", "prevsecondaire_2016", 
              "nbttt_2016", "SCORE_2016", "veryhighrisk_2016", "highrisk_2016", 
              "moderaterisk_2016", "lowrisk_2016", "risquecardiovasc_2016", 
              "LDLcible_2016", "hypercholestcontrolee_2016", "MT_2019", "hta_2019", 
              "diab_2019", "cancer_2019", "cardiopathisch_2019", "aomi_2019", 
              "avc_2019", "SBP1AN_2019", "DBP1AN_2019", "tabac_2019", "CT_2019", 
              "LDL_2019", "HDL_2019", "HbA1c_2019", "IMC_2019", "HTAdefcompl_2019", 
              "tttdiab_2019", "diabdefcompl_2019", "tttdyslipid_2019", "prevsecondaire_2019", 
              "nbttt_2019", "SCORE_2019", "veryhighrisk_2019", "highrisk_2019", 
              "moderaterisk_2019", "lowrisk_2019", "risquecardiovasc_2019", 
              "LDLcible_2019", "hypercholestcontrolee_2019", "hypercontrole", "hypercontroleNA")

categorical = c("MT_2016", "hta_2016", "diab_2016", 
                "cancer_2016", "cardiopathisch_2016", "aomi_2016", "avc_2016", 
                "HTAdefcompl_2016", "tttdiab_2016", 
                "diabdefcompl_2016", "tttdyslipid_2016", "prevsecondaire_2016", 
                "veryhighrisk_2016", "highrisk_2016", 
                "moderaterisk_2016", "lowrisk_2016", "risquecardiovasc_2016", 
                "LDLcible_2016", "hypercholestcontrolee_2016", "MT_2019", "hta_2019", 
                "diab_2019", "cancer_2019", "cardiopathisch_2019", "aomi_2019", 
                "avc_2019",  "HTAdefcompl_2019", 
                "tttdiab_2019", "diabdefcompl_2019", "tttdyslipid_2019", "prevsecondaire_2019", 
                "veryhighrisk_2019", "highrisk_2019", 
                "moderaterisk_2019", "lowrisk_2019", "risquecardiovasc_2019", 
                "LDLcible_2019", "hypercholestcontrolee_2019", "hypercontrole", "hypercontroleNA")


# CREATE THE DESCRIPTIVE TABLE
tab1 = CreateTableOne(vars = variables, data = fg, factorVars = categorical)
print(tab1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

# CREATE THE UNIVARIATE TABLE 
tab2 = CreateTableOne(vars = variables, data = fg2, factorVars = categorical, test = TRUE, includeNA = FALSE, 
                      strata = "hypercontroleNA")
print(tab2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

table(fg$hypercholestcontrolee_2016, fg$hypercholestcontrolee_2019)

################################################################################

fg3 <- fg[c("MT_2016", "age", "hta_2016", "diab_2016", 
            "cancer_2016", "cardiopathisch_2016", "aomi_2016", "avc_2016", 
            "SBP1AN_2016", "DBP1AN_2016", "tabac_2016", "CT_2016", "LDL_2016", 
            "HDL_2016", "HbA1c_2016", "IMC_2016", "HTAdefcompl_2016", "tttdiab_2016", 
            "diabdefcompl_2016", "tttdyslipid_2016", "prevsecondaire_2016", 
            "nbttt_2016", "SCORE_2016", "veryhighrisk_2016", "highrisk_2016", 
            "moderaterisk_2016", "lowrisk_2016", "risquecardiovasc_2016", 
            "LDLcible_2016", "hypercholestcontrolee_2016")]
names(fg3)
count(fg3)

table(fg3$hypercholestcontrolee_2016)
fg3$hypercholestcontrolee_2016 <- as.factor(fg3$hypercholestcontrolee_2016)

fg3$hyper01[fg3$hypercholestcontrolee_2016=="0"] <- "1"
fg3$hyper01[fg3$hypercholestcontrolee_2016=="1"] <- "0"
fg3$hyper01 <- as.factor(fg3$hyper01)

model1 <- glm(hyper01 ~ age + cancer_2016 + cardiopathisch_2016 + aomi_2016 + 
                avc_2016 + tabac_2016 + HTAdefcompl_2016 + diabdefcompl_2016 + 
                tttdyslipid_2016 + #prevsecondaire_2016 +
                nbttt_2016 + SCORE_2016 + risquecardiovasc_2016 +
                LDLcible_2016, data=fg3, family=binomial)

model1

summary(model1)
exp(cbind(coef(model1), confint(model1)))  

#------------------------------------------------------------------------------#

model2 <- lm(LDL_2016 ~ age + cancer_2016 + cardiopathisch_2016 + aomi_2016 + 
                avc_2016 + tabac_2016 + HTAdefcompl_2016 + diabdefcompl_2016 + 
                tttdyslipid_2016 + #prevsecondaire_2016 +
                nbttt_2016 + SCORE_2016 + risquecardiovasc_2016 +
                LDLcible_2016, data=fg3)

model2
summary(model2)
exp(cbind(coef(model2), confint(model2)))  
