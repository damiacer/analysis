# ANALYS PLAN AND COMMENTARY
# the primary outcome is to evaluate the success of the procedure 
# that could be done by a mixed model for repeated data, where the 
# repeated data could be both the "focal thickness" or the 
# "general thickness". 

# the secondary outcome is the evaluation of the timelaps between 
# the intervention. this information is provided by the intMU_MX variable 
# the missing data can be replaced by "4" as no stretch in the timelaps 
# could be assumed as "standard" 4 week delay

# FOLDER

getwd()
setwd("P:/CONSULTATION/Dekeyser_Jérémie") # On PC
setwd("/Users/damianocerasuolo/Desktop/UBRC/2021_CONSULT/Dekeyser_Jérémie") # On Mac

# PACKAGES
install.packages("readxl")
library("readxl")
#install.packages("tableone")
#library("tableone")
#install.packages("desctable")
#library("desctable")
#install.packages("dpylr")
#library("dplyr")

# DATA
dj <- read_excel("DEKEYSER_data.xlsx", na="NA")

names(dj)

is.na(dj$IntMU_M0)
table(dj$IntMU_M0)

#install.packages("tidyr")
#library("tidyr")
#dj2 <- tibble(dj)
#dj2 %>% replace_na(dj)

#dj %>% mutate_at(vars(IntMU_M0, IntMU_M3, IntMU_M6, IntMU_M9), replace_na, "4")

# MISSING VALUES IN THE DELAY ARE CONVERTED TO "4"
# 4 IS THE NUMBER OF WEEKS BETWEEN EACH INTERVENTION
table(dj$IntMU_M0)
dj$IntMU_M0_m[dj$IntMU_M0 == "."] <- "4"
dj$IntMU_M0_m[dj$IntMU_M0 == "0"] <- "0"
dj$IntMU_M0_m[dj$IntMU_M0 == "12"] <- "12"
dj$IntMU_M0_m[dj$IntMU_M0 == "16"] <- "16"
dj$IntMU_M0_m[dj$IntMU_M0 == "20"] <- "20"
dj$IntMU_M0_m[dj$IntMU_M0 == "4"] <- "4"
dj$IntMU_M0_m[dj$IntMU_M0 == "6"] <- "6"
dj$IntMU_M0_m[dj$IntMU_M0 == "8"] <- "8"
table(dj$IntMU_M0_m)

table(dj$IntMU_M3)
dj$IntMU_M3_m[dj$IntMU_M3 == "."] <- "4"
dj$IntMU_M3_m[dj$IntMU_M3 == "0"] <- "0"
dj$IntMU_M3_m[dj$IntMU_M3 == "12"] <- "12"
dj$IntMU_M3_m[dj$IntMU_M3 == "16"] <- "16"
dj$IntMU_M3_m[dj$IntMU_M3 == "20"] <- "20"
dj$IntMU_M3_m[dj$IntMU_M3 == "4"] <- "4"
dj$IntMU_M3_m[dj$IntMU_M3 == "6"] <- "6"
dj$IntMU_M3_m[dj$IntMU_M3 == "8"] <- "8"
table(dj$IntMU_M3_m)

table(dj$IntMU_M6)
dj$IntMU_M6_m[dj$IntMU_M6 == "."] <- "4"
dj$IntMU_M6_m[dj$IntMU_M6 == "10"] <- "10"
dj$IntMU_M6_m[dj$IntMU_M6 == "14"] <- "14"
dj$IntMU_M6_m[dj$IntMU_M6 == "20"] <- "20"
dj$IntMU_M6_m[dj$IntMU_M6 == "24"] <- "24"
dj$IntMU_M6_m[dj$IntMU_M6 == "4"] <- "4"
dj$IntMU_M6_m[dj$IntMU_M6 == "6"] <- "6"
dj$IntMU_M6_m[dj$IntMU_M6 == "8"] <- "8"
table(dj$IntMU_M6_m)

table(dj$IntMU_M9)
dj$IntMU_M9_m[dj$IntMU_M9 == "."] <- "4"
dj$IntMU_M9_m[dj$IntMU_M9 == "12"] <- "12"
dj$IntMU_M9_m[dj$IntMU_M9 == "16"] <- "16"
dj$IntMU_M9_m[dj$IntMU_M9 == "2"] <- "2"
dj$IntMU_M9_m[dj$IntMU_M9 == "6"] <- "6"
dj$IntMU_M9_m[dj$IntMU_M9 == "8"] <- "8"
table(dj$IntMU_M9_m)

################################################################################

# MIXED MODEL ANALYSIS 
djmix <- read_excel("DEKEYSER_mixeddata.xlsx", na="NA")
names(djmix)
table(djmix$IntMU)
is.na(djmix$IntMU)

djmix$IntMU[djmix$IntMU == "."] <- "4"
djmix$IntMU[djmix$IntMU == "0"] <- "0"
djmix$IntMU[djmix$IntMU == "10"] <- "10"
djmix$IntMU[djmix$IntMU == "12"] <- "12"
djmix$IntMU[djmix$IntMU == "14"] <- "14"
djmix$IntMU[djmix$IntMU == "16"] <- "16"
djmix$IntMU[djmix$IntMU == "2"] <- "2"
djmix$IntMU[djmix$IntMU == "20"] <- "20"
djmix$IntMU[djmix$IntMU == "24"] <- "24"
djmix$IntMU[djmix$IntMU == "4"] <- "4"
djmix$IntMU[djmix$IntMU == "6"] <- "6"
djmix$IntMU[djmix$IntMU == "8"] <- "8"
table(djmix$IntMU)

# install.packages("lme4")
library("lme4")

# reference: https://stackoverflow.com/questions/37090722/lme4lmer-reports-fixed-effect-model-matrix-is-rank-deficient-do-i-need-a-fi
fix.formula <- OCTgl ~ age + diabete + ovcr0_crsc1 + type_fc0df1 + 
  sexe + cote_dx0sn1
X <- model.matrix (fix.formula, djmix)
X

dj1 = lmer(OCTgl ~ age + diabete + ovcr0_crsc1 + type_fc0df1 + 
             sexe + cote_dx0sn1 + (ID|#THERE IS NO CLUSTER THERE
             ),
           data = djmix)

# reference : https://stats.stackexchange.com/questions/492735/is-a-mixed-model-appropriate-for-repeated-measures-of-multiple-covariates

djmix$OCTgl = as.numeric(as.character(djmix$OCTgl))
djmix$age = as.numeric(as.character(djmix$age))

formula = OCTgl ~ age + sexe + diabete + ovcr0_crsc1 + type_fc0df1 + 
  cote_dx0sn1 + (Mesure|ID)
model1 = lmer(formula, REML = TRUE, data = djmix)
coef(summary(model1)) # coef for fixed parameter estimates
summary(model1)
anova(model1)

# exp(cbind(coef(model1), confint(model1)))  
# exp(coef(model1))

install.packages("DAAG")
library("DAAG")
mod1_mcmc = mcmcsamp(model1, n=1000)
?mcmcsamp
