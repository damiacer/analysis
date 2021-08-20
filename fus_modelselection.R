# FUSION
# MODEL SELECTION FOR COX 

# AGE
str(fusna$age)

# SEXE
str(fusna$SEXE)
fusna$SEXE2[fusna$SEXE=="M"] <- "1"
fusna$SEXE2[fusna$SEXE=="F"] <- "0"
table(fusna$SEXE2)
is.na(fusna$SEXE2)
str(fusna$SEXE2)
fusna$SEXE2 = as.numeric(as.character(fusna$SEXE2))

# Primitifpoumon
str(fusna$Primitifpoumon)
fusna$Primitifpoumon2[fusna$Primitifpoumon == "Primitif"] <- "1"
fusna$Primitifpoumon2[fusna$Primitifpoumon == "Secondaire"] <- "2"
table(fusna$Primitifpoumon2)
fusna$Primitifpoumon2 = as.numeric(as.character(fusna$Primitifpoumon2))

# ETIOLOGIE2
str(fusna$ETIOLOGIE2)
fusna$ETIOLOGIE2f[fusna$ETIOLOGIE2 == "Adenocarcinome"] <- "1"
fusna$ETIOLOGIE2f[fusna$ETIOLOGIE2 == "Epidermoide"] <- "2"
fusna$ETIOLOGIE2f[fusna$ETIOLOGIE2 == "Autre"] <- 3
fusna$ETIOLOGIE2f = as.numeric(as.character(fusna$ETIOLOGIE2f))

# HEMOPTYSIE.VOLUME
str(fusna$HEMOPTYSIE.VOLUME)
fusna$HEMOPTYSIE.VOLUME2[fusna$HEMOPTYSIE.VOLUME == "Faible abondance"] <- "1"
fusna$HEMOPTYSIE.VOLUME2[fusna$HEMOPTYSIE.VOLUME == "Grande abondance"] <- "2"
fusna$HEMOPTYSIE.VOLUME2[fusna$HEMOPTYSIE.VOLUME == "Moyenne abondance"] <- "3"
fusna$HEMOPTYSIE.VOLUME2 = as.numeric(as.character(fusna$HEMOPTYSIE.VOLUME2))

# HEMOPTYSIE.IMPORTANTE
str(fusna$HEMOPTYSIE.IMPORTANTE)
fusna$HEMOPTYSIE.IMPORTANTE2[fusna$HEMOPTYSIE.IMPORTANTE == "Oui"] <- "1"
fusna$HEMOPTYSIE.IMPORTANTE2[fusna$HEMOPTYSIE.IMPORTANTE == "Non"] <- "0"
fusna$HEMOPTYSIE.IMPORTANTE2 = as.numeric(as.character(fusna$HEMOPTYSIE.IMPORTANTE2))

# Hemodynamique
str(fusna$Hemodynamique)
fusna$Hemodynamique2[fusna$Hemodynamique == "défaillance"] <- "1"
fusna$Hemodynamique2[fusna$Hemodynamique == "stable"] <- "2"
fusna$Hemodynamique2 = as.numeric(as.character(fusna$Hemodynamique2))

# Factfavor2
str(fusna$Factfavor2)
fusna$Factfavor2 = as.numeric(as.character(fusna$Factfavor2))

# IMAGERIE.EXCAVE_class
str(fusna$IMAGERIE.EXCAVE_class)
fusna$IMAGERIE.EXCAVE_class = as.numeric(as.character(fusna$IMAGERIE.EXCAVE_class))

# LESION.NECROTIQUE_class
str(fusna$LESION.NECROTIQUE_class)
fusna$LESION.NECROTIQUE_class = as.numeric(as.character(fusna$LESION.NECROTIQUE_class))

# lesionarterepulm
str(fusna$lesionarterepulm)
fusna$lesionarterepulm2[fusna$lesionarterepulm == "Irrégularité"] <- "1"
fusna$lesionarterepulm2[fusna$lesionarterepulm == "normale"] <- "0"
fusna$lesionarterepulm2[fusna$lesionarterepulm == "occlusion"] <- "2"
fusna$lesionarterepulm2[fusna$lesionarterepulm == "Pseudoanévrisme"] <- "3"
fusna$lesionarterepulm2[fusna$lesionarterepulm == ""] <- "."
fusna$lesionarterepulm2 = as.numeric(as.character(fusna$lesionarterepulm2))

# taillelesion_m
str(fusna$taillelesion_m)

# prox_distal_class
str(fusna$prox_distal_class)
fusna$prox_distal_class = as.numeric(as.character(fusna$prox_distal_class))

# TECHembolPULM1
str(fusna$TECHembolPULM1)
fusna$TECHembolPULM12[fusna$TECHembolPULM1 == "Coils"] <- "1"
fusna$TECHembolPULM12[fusna$TECHembolPULM1 == "Colle"] <- "2"
fusna$TECHembolPULM12[fusna$TECHembolPULM1 == "Gélatine"] <- "3"
fusna$TECHembolPULM12[fusna$TECHembolPULM1 == "Plug"] <- "4"
fusna$TECHembolPULM12[fusna$TECHembolPULM1 == "Stent couvert"] <- "5"
fusna$TECHembolPULM12[fusna$TECHembolPULM1 == ""] <- "."
fusna$TECHembolPULM12 = as.numeric(as.character(fusna$TECHembolPULM12))

# fusna$delaissurvgeste
table(fusna$IMAGERIE.EXCAVE_class)

var.list = c("age", 
             "SEXE2",
             "Primitifpoumon2",
             "ETIOLOGIE2f",
             "HEMOPTYSIE.VOLUME2",
             "HEMOPTYSIE.IMPORTANTE2",
             "Hemodynamique2",
             "Factfavor2",
             "IMAGERIE.EXCAVE_class",
             "LESION.NECROTIQUE_class",
             "lesionarterepulm",
             "taillelesion_m",
             "prox_distal_class",
             "TECHembolPULM12"
)

My.stepwise.coxph(Time = "time", Status = "status", variable.list = var.list, 
                  data = fusna, sle = 0.25, sls = 0.25)

################################################################################

require("survival")

#1
coxfusna1 = coxph(formula = Surv(time, status) ~ age + SEXE2, data = fusna)
coxfusna1
extractAIC(coxfusna1)
AIC(coxfusna1)

#2
coxfusna2 = coxph(formula = Surv(time, status) ~ age + 
                    SEXE2 + ETIOLOGIE2f, data = fusna)
AIC(coxfusna2)

#3
coxfusna = coxph(formula = Surv(time, status) ~ age + 
                   SEXE2 + ETIOLOGIE2f + HEMOPTYSIE.VOLUME2 + Hemodynamique2
                 + Factfavor2 + IMAGERIE.EXCAVE_class, data = fusna)
AIC(coxfusna) # 230 (without IMAGERIE = 253)

#4 
coxfusna4 = coxph(formula = Surv(time, status) ~ age + 
                    SEXE2 + ETIOLOGIE2f + HEMOPTYSIE.VOLUME2 + Hemodynamique2
                  + Factfavor2 + IMAGERIE.EXCAVE_class + LESION.NECROTIQUE_class
                  + lesionarterepulm + taillelesion_m, data = fusna)
AIC(coxfusna4) # 179

# 5
coxfusna5 = coxph(formula = Surv(time, status) ~ age + 
                    SEXE2 + ETIOLOGIE2f + HEMOPTYSIE.VOLUME2 + Hemodynamique2
                  + Factfavor2 + IMAGERIE.EXCAVE_class + LESION.NECROTIQUE_class
                  + lesionarterepulm + taillelesion_m + TECHembolPULM12, data = fusna)
AIC(coxfusna5) # 179

age + 
  SEXE2 + ETIOLOGIE2f + HEMOPTYSIE.VOLUME2 + Hemodynamique2
+ Factfavor2 + IMAGERIE.EXCAVE_class + LESION.NECROTIQUE_class
+ lesionarterepulm + taillelesion_m

################################################################################
################################################################################
################################################################################


# SEXE
fusna$SEXE2 = as.factor(fusna$SEXE2)

# Primitifpoumon
fusna$Primitifpoumon2 = as.factor(fusna$Primitifpoumon2)

# ETIOLOGIE2
fusna$ETIOLOGIE2f = as.factor(fusna$ETIOLOGIE2f)

# HEMOPTYSIE.VOLUME
fusna$HEMOPTYSIE.VOLUME2 = as.factor(fusna$HEMOPTYSIE.VOLUME2)

# HEMOPTYSIE.IMPORTANTE
fusna$HEMOPTYSIE.IMPORTANTE2 = as.factor(fusna$HEMOPTYSIE.IMPORTANTE2)

# Hemodynamique
fusna$Hemodynamique2 = as.factor(fusna$Hemodynamique2)

# Factfavor2
fusna$Factfavor2 = as.factor(fusna$Factfavor2)

# IMAGERIE.EXCAVE_class
fusna$IMAGERIE.EXCAVE_class = as.factor(fusna$IMAGERIE.EXCAVE_class)

# LESION.NECROTIQUE_class
fusna$LESION.NECROTIQUE_class = as.factor(fusna$LESION.NECROTIQUE_class)

# lesionarterepulm
fusna$lesionarterepulm2 = as.factor(fusna$lesionarterepulm2)

# taillelesion_m
fusna$taillelesion_m = fusna$taillelesion_m

# prox_distal_class
fusna$prox_distal_class = as.factor(fusna$prox_distal_class)

# TECHembolPULM1
fusna$TECHembolPULM12 = as.factor(fusna$TECHembolPULM12)

# fusna$delaissurvgeste

# COX MODELS
coxfusna = coxph(formula = Surv(time, status) ~ age, data = fusna)
AIC(coxfusna)

fusna$status = fusna$relapse
fusna$time = fusna$fup

coxfusna = coxph(formula = Surv(time, status) ~ age + SEXE2 + Primitifpoumon + ETIOLOGIE2 + HEMOPTYSIE.VOLUME2
                 + HEMOPTYSIE.IMPORTANTE + Hemodynamique2 + Factfavor2 + IMAGERIE.EXCAVE_class
                 + LESION.NECROTIQUE_class + LESION.NECROTIQUE_class
                 + lesionarterepulm2 + taillelesion_m + TECHembolPULM12, data = fusna)

summary(coxfusna)
confint(coxfusna)  #coefficient CIs
exp(confint(coxfusna))  #Also HR CIs
AIC(coxfusna)
BIC(coxfusna)

res.zph1 <- cox.zph(coxfusna)
plot(res.zph1)

# http://rstudio-pubs-static.s3.amazonaws.com/5896_8f0fed2ccbbd42489276e554a05af87e.html

#-------------------------------------------------------------------------------

require("survival")
require("survminer")
require("Rcpp")

fusfit = survfit(Surv(time, status) ~ SEXE2, data = fusna)
print(fusfit)
summary(fusfit)$table

ggsurvplot(fusfit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

################################################################################

require("survival")

time2 = fusna$fupd 
status2 = as.numeric(as.character(fusna$DECES01))
fusna$relapse2 = as.factor(fusna$relapse)

coxfusnad = coxph(formula = Surv(time2, status2) ~ age + SEXE2 + Primitifpoumon + ETIOLOGIE2 + HEMOPTYSIE.VOLUME2
                 + HEMOPTYSIE.IMPORTANTE + Hemodynamique2 + Factfavor2 + IMAGERIE.EXCAVE_class
                 + LESION.NECROTIQUE_class + LESION.NECROTIQUE_class
                 + lesionarterepulm2 + taillelesion_m + TECHembolPULM12 + arrethem + relapse2, data = fusna)

summary(coxfusnad)
confint(coxfusna)  #coefficient CIs
exp(confint(coxfusna))  #Also HR CIs
AIC(coxfusnad)
BIC(coxfusnad)

################################################################################

# SURVPLOT

relapsekm = survfit(Surv(time, status) ~ arrethem, data = fusna)

install.packages("survminer")
library("survminer")

ggsurvplot(
  relapsekm,
  data = fusna,
  size = 1,
  palette = c("#E7B800", "#2E9FDF"),
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  risk.table.col = "strata",
  legend.labs = c("pas arret imm", "arret imm"),
  risk.table.height = 0.25,
  ggtheme = theme_bw()
)
