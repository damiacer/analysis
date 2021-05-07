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

# DATA COMPLETE
djmix = read_excel("DEKEYSER_mixeddata.xlsx", na="NA")
names(djmix)
table(djmix$IntMU)
is.na(djmix$IntMU)

djmix$OCTgl = as.numeric(as.character(djmix$OCTgl))
djmix$age = as.numeric(as.character(djmix$age))
djmix$OCTfc_M0 = as.numeric(as.character(djmix$OCTfc_M0))

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

# 1 PATIENT EXCLUDED
djmix2 = djmix[!(djmix$cote_dx0sn1=="."),]
# More on this code: https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/

# install.packages("lme4")
library("lme4")

#-------------------------------------------------------------------------------

# MODEL 1

# reference: https://stackoverflow.com/questions/37090722/lme4lmer-reports-fixed-effect-model-matrix-is-rank-deficient-do-i-need-a-fi
# fix.formula <- OCTgl ~ age + diabete + ovcr0_crsc1 + type_fc0df1 + 
#             sexe + cote_dx0sn1
# X <- model.matrix (fix.formula, djmix)
# X

# dj1 = lmer(OCTgl ~ age + diabete + ovcr0_crsc1 + type_fc0df1 + 
#              sexe + cote_dx0sn1 + (ID|#THERE IS NO CLUSTER THERE
#              ),
#            data = djmix)

# reference : https://stats.stackexchange.com/questions/492735/is-a-mixed-model-appropriate-for-repeated-measures-of-multiple-covariates

#-------------------------------------------------------------------------------

# MODEL 2: OCTgl

formula = OCTgl ~ age + sexe + diabete + ovcr0_crsc1 + type_fc0df1 + 
  cote_dx0sn1 + (Mesure|ID)
model1 = lmer(formula, REML = TRUE, data = djmix)
model2 = lmer(formula, REML = TRUE, data = djmix2)
coef(summary(model1)) # coef for fixed parameter estimates
coef(summary(model2))
summary(model1)
summary(model2)
anova(model1)

# MODEL 2: OCTfc_M0

formula2 = OCTfc_M0 ~ age + sexe + diabete + ovcr0_crsc1 + type_fc0df1 + 
  cote_dx0sn1 + (Mesure|ID)
model3 = lmer(formula2, REML = TRUE, data = djmix2)
coef(summary(model3))
summary(model2)
anova(model1)

# exp(cbind(coef(model1), confint(model1)))  
# exp(coef(model1))

# install.packages("DAAG")
# library("DAAG")
# mod1_mcmc = mcmcsamp(model1, n=1000)
# ?mcmcsamp

################################################################################

# COEFFICIENTS

# USING NORMAL DISTRIBUTION TO APPROXIMATE P-VALUES
coefs2 <- data.frame(coef(summary(model2)))
coefs2$p.z <- 2 * (1 - pnorm(abs(coefs2$t.value)))
coefs2

coefs2 <- data.frame(coef(summary(model2)))
coefs$p.z2 <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

#-------------------------------------------------------------------------------

# USING lmerTest
install.packages("lmerTest")
library("lmerTest")

coefs$df.Satt <- coef(summary(model1))[, 3]
coefs$p.Satt <- coef(summary(model1))[, 3]
coefs

#-------------------------------------------------------------------------------

# USING THE KENWARD-ROGER APPROXIMATION TO GET APPROXIMATE DEGREES OF FREEDOM
# NOT WORKING ON PC
install.packages("pbkrtest")
library("pbkrtest")

df.KR <- get_ddf_Lb(model1, fixef(model1))
# get p-values from the t-distribution using the t-values and approximated
# degrees of freedom
coefs$p.KR <- 2 * (1 - pt(abs(coefs$t.value), df.KR))
coefs

################################################################################

# ODDS RATIO FOR THE MODEL

# install.packages("oddsratio")
# library("oddsratio")

# m1OR <- or_glm(data = djmix, model = model1, incr = list(week = 5))

################################################################################

install.packages("sjPlot")
library("sjPlot")
# More: https://lmudge13.github.io/sample_code/mixed_effects.html
sjPlot::plot_model(model1)

sjPlot::plot_model(model2, 
                   axis.labels=c("Age", "Sexe", "Diabete", "OVCR/CRSC", "Type focal/diffus", "Cote DX-SN"),
                   show.values=TRUE, show.p=TRUE,
                   title="Modèle mixte OTC Global")

plot_model(model3, 
           axis.labels=c("Age", "Sexe", "Diabete", "OVCR/CRSC", "Type focal/diffus", "Cote DX-SN"),
           show.values=TRUE, show.p=TRUE,
           title="Modèle mixte OTC Focal")

#-------------------------------------------------------------------------------

tab_model(model1) # OR
tab_model(model2, 
          show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Age", "Sexe", "Diabete", "OVCR/CRSC", "Type focal/diffus", "Cote DX-SN"),
          dv.labels= "Modèle mixte OTC Global")

tab_model(model3, 
          show.re.var= TRUE, 
          pred.labels =c("(Intercept)", "Age", "Sexe", "Diabete", "OVCR/CRSC", "Type focal/diffus", "Cote DX-SN"),
          dv.labels= "Modèle mixte OTC Focal")

#-------------------------------------------------------------------------------

# SAVING THE EFFECT SIZE INTO ESTIMATES
install.packages("effects")
library("effects")
install.packages("ggplot2")
library("ggplot2")
require("ggplot2")

effects_type <- effects::effect(term= "type_fc0df1", mod=model2)
summary(effects_type) 

x_type <- as.data.frame(effects_type)

# PLOT THE ESTIMATES
type_plot <- ggplot() + 
  geom_point(data=djmix2,
               #subset(me_data, LAI_nonzero==1), 
               aes(type_fc0df1, log(OCTgl))) + 
  geom_point(data=x_type, aes(x=type_fc0df1, y=fit), color="blue") +
  geom_line(data=x_type, aes(x=type_fc0df1, y=fit), color="blue") +
  geom_ribbon(data= x_type, aes(x=type_fc0df1, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Type focal/diffus", y="OCT epaisseur gl")

urchin_plot
