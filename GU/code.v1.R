getwd()
setwd("/Users/damianocerasuolo/Desktop/UBRC/21_22_CONSULT_MAC/Gueutin_Victor")
setwd("P:/CONSULTATION/Gueutin_Victor")

library("readxl")

gu <- read_excel("BaseGBM2020_DCMOD.xlsx", na = "NA")
warnings()
names(gu)
#View(gu)

#---------------------------------------------------------------------------------

# TRAITMENT VARIABLE 

table(gu$traitement)
# 0  1 
# 5 66

table(gu$tt_etude_3classes)
# 0  1  2 
# 6 51 45
 
table(gu$tt_etude_4classes)
# 0  1  2  3 
# 6 13 39 45 

library("tidyverse")

str(gu$tt_etude_4classes)
gu <- gu %>%
	mutate(groups = case_when(
			tt_etude_4classes ==  0 | tt_etude_4classes == 1 ~ "0", 
			tt_etude_4classes == 2 | tt_etude_4classes == 3 ~ "1"
			))
			
table(gu$groups, gu$tt_etude_4classes)


# OUTCOME : ESRD_M1 and ESRD_M12
dim(gu)
table(gu$ESRD_M1, useNA = "always")
table(gu$dialyse_M1, useNA = "always")

table(gu$ESRD_M6, useNA = "always")
table(gu$ESRD_M12, useNA = "always")

##################################################################################

# outcome à M6 

table(gu$DCD_M6, gu$ESRD_M6, useNA = "always")

gu$ESRD_M6na <- gu$ESRD_M6
gu$ESRD_M6na[is.na(gu$ESRD_M6)] <- "99"
gu$DCD_M6na <- gu$DCD_M6
gu$DCD_M6na[is.na(gu$DCD_M6)] <- "99"
table(gu$DCD_M6na, useNA = "always")
table(gu$ESRD_M6, useNA = "always")
table(gu$ESRD_M1, useNA = "always")
table(gu$ESRD_M12, useNA = "always")


##################################################################################

### DATASET COMPLETE FOR DEATH

gu$DCD_M12na <- gu$DCD_M12
gu$DCD_M12na[is.na(gu$DCD_M12)] <- "99"
dim(gu)

gu2 <- gu[!(gu$DCD_M12na=="99"),]
dim(gu2)

##################################################################################

gu2 <- gu2 %>%
	mutate(groups = case_when(
			tt_etude_4classes ==  0 | tt_etude_4classes == 1 ~ "0",
			tt_etude_4classes == 2 | tt_etude_4classes == 3 ~ "1"
			))
table(gu2$groups)

gu2 <- gu2 %>%
	mutate(groups2 = case_when(
			tt_etude_4classes ==  0 | tt_etude_4classes == 1 ~ "1",
			tt_etude_4classes == 2 ~ "2",
			tt_etude_4classes == 3 ~ "3"
			))
table(gu2$groups2)

##################################################################################

#-FOLLOWUP------------------------------------------------------------------------

library("lubridate")

# deces_date
# 30-06-2020
# date_diaginitial
# dateder_nouvelles

# CREATE A DAY FOR EVERY ENTRY IN THE DATABASE
# EVERY ENTRY DAY WILL BE SET TO 30
gu2$june.d <- rep("30", times=102)
gu2$june.m <- rep("06", times=102)
gu2$june.y <- rep("2020", times=102)

# install.packages("lubridate")
# IF NECESSARY (WINDOWS): library(lubridate, warn.conflicts = FALSE)
# MORE ON LUBRIDATE: https://lubridate.tidyverse.org/

# CREATE A NEW VARIABLE FOR DATE
# THE CODE DOES NOT RECOGNIZE THE sep = "" COMMAND. DATE CANNOT BE AUTOMATICALLY CREATED
gu2$june <- as.numeric(paste(gu2$june.y, gu2$june.m, gu2$june.d, sep = ""))
table(gu2$june)
gu2$june = ymd(gu2$june)
table(gu2$june)
str(gu2$june)

#-FUP CALUCLATE--------------------------------------------------------------------
# FUP 2020
# FUP.20 = as.Date(gu2$june, "%d/%m/%Y") - as.Date(gu2$date_diaginitial, "%d/%m/%Y") 
# FUP DEATH
FUP.D = as.Date(gu2$deces_date, "%d/%m/%Y") - as.Date(gu2$date_diaginitial, "%d/%m/%Y")
# FUP LAST FOLLOWUP
FUP.LF = as.Date(gu2$dateder_nouvelles, "%d/%m/%Y") - as.Date(gu2$date_diaginitial, "%d/%m/%Y")

table(gu2$date_diaginitial)
str(gu2$date_diaginitial)

gu2$date3ans = (as.Date(gu2$date_diaginitial) %m+% years(3))
gu2$date1an = (as.Date(gu2$date_diaginitial) %m+% years(1))

# suivi à trois ans et suivi jusqu'au décès
gu2$fup3 = (as.Date(gu2$date3ans, "%d/%m/%Y") - as.Date(gu2$date_diaginitial, "%d/%m/%Y"))
gu2$fupd = (as.Date(gu2$deces_date, "%d/%m/%Y") - as.Date(gu2$date_diaginitial, "%d/%m/%Y"))
gu2$fup1 = (as.Date(gu2$date1an, "%d/%m/%Y") - as.Date(gu2$date_diaginitial, "%d/%m/%Y"))

library("tidyverse")

gu2 <- gu2 %>%
	mutate(follow = case_when(
	DCD_M12 == "1" ~ fupd,
	DCD_M12 == "0" ~ fup3
	))
table(gu2$follow, useNA = "always")

    # definition of a binary event 
    gu2$fupdNA <- gu2$fupd
    gu2$fupdNA[is.na(gu2$fupdNA)] <- "2000"
    gu2$fupdNA = as.numeric(gu2$fupdNA)
    gu2$death3 = if_else(gu2$fupdNA <= 1096, "1", "0") 
    table(gu2$death3)
    str(gu2$death3)

#gu2 <- gu2 %>% 
#  mutate(follow = case_when(
#    DCD_M12 == "1" ~ FUP.D,
#    DCD_M12 == "0" & (as.Date(gu2$dateder_nouvelles) < (as.Date(gu2$june)) ~ FUP.LF,
#    DCD_M12 == "0" ~ FUP.20 
#  ))

# type_atteinte_rein

gu2 <- gu2 %>%
		mutate(type_atteinte_rein = case_when(
		type_atteinte_rein == "3" ~ "0",
		type_atteinte_rein == "0" ~ "0", # scler
		type_atteinte_rein == "1" ~ "1", # focal
		type_atteinte_rein == "2" ~ "2" # cellul
		))
table(gu2$type_atteinte_rein)

# follow up death 3 y

gu2 <- gu2 %>%
  mutate(followup3 = case_when(
    death3 == "1" ~ fupd,
    death3 == "0" ~ fup3
  ))
table(gu2$followup3)
  
##################################################################################

#-VAR-TRANSFORMATION-------------------------------------------------------------

gu2$taille = as.numeric(as.character(gu2$taille))
gu2$poids = as.numeric(as.character(gu2$poids))
gu2$IMC = gu2$poids/((gu2$taille)^2)

gu2$hta = as.numeric(as.character(gu2$hta))
gu2$antiMBG_taux = as.numeric(as.character(gu2$antiMBG_taux))
gu2$C3 = as.numeric(as.character(gu2$C3))
gu2$C4 = as.numeric(as.character(gu2$C4))
gu2$CRP = as.numeric(as.character(gu2$CRP))
gu2$age_diag = as.numeric(as.character(gu2$age_diag))
gu2$type_atteinte_rein = as.factor(gu2$type_atteinte_rein)
gu2$tt_etude_4classes = as.factor(gu2$tt_etude_4classes)
gu2$groups = as.factor(gu2$groups)
gu2$sex = as.factor(gu2$sex)
gu2$diabete = as.factor(gu2$diabete)
gu2$ESRD_M12 = as.factor(gu2$ESRD_M12)
gu2$type_atteinte_rein = as.factor(gu2$type_atteinte_rein)

##################################################################################

#-IMPUTATION CHECK FOR DEATH AT 12------------------------------------------------

# install.packages("finalfit")
library("finalfit")

### IMPUTATION DATASET
gu2$time = gu2$follow
gu2$status = gu2$DCD_M12

gu.toimput = subset(gu2, select = c(status, age_diag, sex, diabete, hta, IMC, ESRD_M12, 
                                   tt_etude_4classes, antiMBG_taux,
                                   type_atteinte_rein, C3, C4, CRP, time, groups))

gu.toimput %>%
	missing_plot()
	
# Looking for pattern of missingness

explanatory = c("age_diag", "sex", "diabete", "hta", "IMC", "ESRD_M12", 
                                   "tt_etude_4classes", "antiMBG_taux",
                                   "type_atteinte_rein", "C3", "C4", "CRP", "groups")
                                   
gu.toimput$status.f = as.factor(gu.toimput$status) 
str(gu.toimput$status.f)                                  
dependent = "status.f"

gu.toimput %>%
	missing_pattern(dependent, explanatory)

# Check for association between missing and observed data

gu.toimput %>%
	missing_pairs(dependent, explanatory)
	
gu.toimput %>%
	missing_pairs(dependent, explanatory, position = "fill")
	
# MISSING COMPARE

explanatory = c("age_diag", "sex", "diabete", "hta", "IMC", "ESRD_M12", "tt_etude_4classes", "antiMBG_taux", "type_atteinte_rein", "C3", "C4", "CRP")
                                   
gu.toimput$status.f = as.factor(gu.toimput$status) 
str(gu.toimput$status.f)                                  
dependent = "status.f"
table(gu.toimput$status.f, useNA = "always")

library("knitr")

gu.toimput %>%
	missing_compare(dependent, explanatory) %>%
	knitr::kable(row.names = FALSE, align = c("l", "l", "r", "r", "r"))

#-IMPUTATION CHECK FOR DEATH AT 3 YEARS-------------------------------------------

install.packages("finalfit")
library("finalfit")

### IMPUTATION DATASET
gu2$time = gu2$followup3
gu2$status = gu2$death3

gu.toimput = subset(gu2, select = c(status, age_diag, sex, diabete, hta, IMC, ESRD_M12, 
                                    tt_etude_4classes, antiMBG_taux,
                                    type_atteinte_rein, C3, C4, CRP, time, groups))

gu.toimput %>%
  missing_plot()

# Looking for pattern of missingness

explanatory = c("age_diag", "sex", "diabete", "hta", "IMC", "ESRD_M12", 
                "tt_etude_4classes", "antiMBG_taux",
                "type_atteinte_rein", "C3", "C4", "CRP", "groups")

gu.toimput$status.f = as.factor(gu.toimput$status) 
str(gu.toimput$status.f)                                  
dependent = "status.f"

gu.toimput$time = as.numeric(gu.toimput$time)

gu.toimput %>%
  missing_pattern(dependent, explanatory)

# Check for association between missing and observed data

gu.toimput %>%
  missing_pairs(dependent, explanatory)

gu.toimput %>%
  missing_pairs(dependent, explanatory, position = "fill")

# MISSING COMPARE

explanatory = c("age_diag", "sex", "diabete", "hta", 
              "IMC", "ESRD_M12", 
                "tt_etude_4classes", 
                "antiMBG_taux", "type_atteinte_rein", "C3", "C4", "CRP")
str(gu.toimput)

install.packages("naniar")
library("naniar")
gutn <- subset(gu.toimput, select = c("age_diag", "hta", "IMC", "antiMBG_taux", "CRP"))
str(gutn)
mcar_test(gutn)
# 0.275 data is missing completely at random

gu.toimput$status.f = as.factor(gu.toimput$status) 
str(gu.toimput$status.f)   
table(gu.toimput$status.f, useNA = "always")
gu.toimput$status.f = if_else(gu.toimput$status.f == "0", "1", "2")
dependent = "status.f"
table(gu.toimput$status.f, useNA = "always")

library("knitr")

gu.toimput %>%
  missing_compare(dependent, explanatory) %>%
  knitr::kable(row.names = FALSE, align = c("l", "l", "r", "r", "r"))

#-TEST----------------------------------------------------------------------------

str(gu.toimput)

gu.toimput$IMCNA <- gu.toimput$IMC
gu.toimput$IMCNA[is.na(gu.toimput$IMCNA)] <- "NA"
gu.toimput$IMCNA = if_else(gu.toimput$IMCNA == "NA", "NA", "NNA")

chisq.test(gu.toimput$IMCNA, gu.toimput$status.f, correct = F)

gu.toimput$type_atteinte_reinNA <- gu.toimput$type_atteinte_rein
gu.toimput$type_atteinte_reinNA[is.na(gu.toimput$type_atteinte_reinNA)] <- "NA"
gu.toimput$type_atteinte_reinNA = if_else(gu.toimput$type_atteinte_reinNA == "NA", "NA", "NNA")
chisq.test(gu.toimput$type_atteinte_reinNA, gu.toimput$status.f, correct = FALSE, simulate.p.value = T)

#-IMPUTATION----------------------------------------------------------------------


# RUN THE CODE BELOW FOR FOLLOW UP 
gu2$time = gu2$follow
gu2$status = gu2$death3

### IMPUTATION DATASET
gu3 = subset(gu2, select = c(status, age_diag, sex, diabete, hta, IMC, 
                                   tt_etude_4classes, antiMBG_taux,
                                   type_atteinte_rein, time))
summary(gu3)


#---------------------------------------------------------------------------------

# CUMHAZ TO IMPUTE 

# install.packages("survminer")
library("survminer")
library("survival")

cox.fit <- coxph(Surv(time, status=="1") ~  age_diag + sex + diabete + hta + 
                   IMC + tt_etude_4classes + antiMBG_taux +
                 type_atteinte_rein, data = gu3)

# In coxph.fit(X, Y, istrat, offset, init, control, weights = weights,  :
# Loglik converged before variable  6,7,8,10 ; coefficient may be infinite. 

# survfit(), in addition to the survival function, also computes the cumulative baseline hazard function.

SF <- survfit(cox.fit)
gu3$cumhaz <- NA

for(i in 1:nrow(gu3)) {
  # Can only compute at non-missing times
  if (!is.na(gu3$time[i])) {
    gu3$cumhaz[i] <- 
      summary(SF, times = gu3$time[i])$cumhaz
  }
}

gu3for.imp <- gu3 %>% 
  select(cumhaz, age_diag, sex, diabete, hta, IMC, 
         tt_etude_4classes, antiMBG_taux,
         type_atteinte_rein, status)

summary(gu3for.imp)

# data imputation

imp.gu <- mice(gu3for.imp,
               seed  = 21051986,
               m     = 20, #nimpute(gu3for.imp),
               maxit = 10,
               meth = "pmm",
               print = F)
imp.gu


# Checking... Is the data in the same order as before?
# First imputation
imp.gu.dat <- complete(imp.gu)
table(imp.gu.dat$sex, gu3$sex, exclude=NULL)
# etc...
# All in the same order

# Imputed datasets in long form
imp.gu.dat <- complete(imp.gu, "long", include = TRUE)

# Repeat time variable m + 1 times since impdat
# includes the original data as well as m imputations
imp.gu.dat$time <- rep(gu3$time, imp.gu$m + 1)

# Replace missing time values with time corresponding
# to the imputed cumulative hazard value
SUB <- imp.gu.dat$.imp > 0 & is.na(imp.gu.dat$time)
if(sum(SUB) > 0) {
  
  # Create a data frame with the unique event times
  # and corresponding cumulative hazards from the
  # complete case analysis
  bhz <- data.frame(time   = survfit(cox.fit)$time,
                    cumhaz = survfit(cox.fit)$cumhaz)
  
  # The following only works if pmm (the default) was used
  # to impute missing cumhaz values (because it relies on
  # the imputed values being values present in the non-missing
  # values)
  for(i in 1:sum(SUB)) {
    # Use max since last 2 times have the same cumhaz
    imp.gu.dat$time[SUB][i] <-
      max(bhz$time[bhz$cumhaz == imp.gu.dat$cumhaz[SUB][i]])
  }
}

# Convert back to a mids object
imp.gu.new <- as.mids(imp.gu.dat)

# fit the cox model 
# fit.imp.cox <- with(imp.gu.new,
#                    coxph(Surv(gestage37, preterm01) ~
#                            RF_PPTERM + MAGER + MRACEHISP + DMAR))
# Do NOT include the -1 here since a Cox model has no intercept
# summary(pool(fit.imp.cox), conf.int = T,
#         exponentiate = T)[, c("term", "estimate", "2.5 %", "97.5 %", "p.value")]
#round.summary(fit.imp.cox, digits = 3,
#              exponentiate = T)[, c("estimate", "2.5 %", "97.5 %", "p.value")]

##################################################################################

### IMPUTATION
library(mice)

gu.imputed <- mice(gu.toimput, m=20, maxit=10, meth='pmm', seed=210586,
                    print = FALSE)
                    
               
##################################################################################

### COX UNIVARIATE WITH IMPUTATION
library(survival)

# age diag
cox.agediag = with(imp.gu.new, coxph(Surv(time, status=="1") ~ age_diag))
summary(pool(cox.agediag))
est.VAR.age <- pool(cox.agediag)
summary(est.VAR.age, conf.int = TRUE, exponentiate = TRUE)

# sexe
cox.sexe = with(imp.gu.new, coxph(Surv(time, status=="1") ~ sex))
summary(pool(cox.sexe))
est.VAR.sex <- pool(cox.sexe)
summary(est.VAR.sex, conf.int = TRUE, exponentiate = TRUE)

# tt_etude_4classes
cox.tt = with(imp.gu.new, coxph(Surv(time, status=="1") ~ tt_etude_4classes))
summary(pool(cox.tt))
est.VAR.tt <- pool(cox.tt)
summary(est.VAR.tt, conf.int = TRUE, exponentiate = TRUE)
cox.tt = with(imp.gu.new, coxph(Surv(time, status=="1") ~ tt_etude_4classes))
cox.tte = with(imp.gu.new, coxph(Surv(time, status=="1") ~ 1))
# install.packages("mitml")
library("mitml")
D1(cox.tt, cox.tte)

# antiMBG_taux
cox.anti = with(imp.gu.new, coxph(Surv(time, status=="1") ~ antiMBG_taux))
summary(pool(cox.anti))
est.VAR.anti <- pool(cox.anti)
summary(est.VAR.anti, conf.int = TRUE, exponentiate = TRUE)

# type_atteinte_rein
cox.type = with(imp.gu.new, coxph(Surv(time, status=="1") ~ type_atteinte_rein))
summary(pool(cox.type))
est.VAR.type <- pool(cox.type)
summary(est.VAR.type, conf.int = TRUE, exponentiate = TRUE)
cox.typee = with(imp.gu.new, coxph(Surv(time, status=="1") ~ 1))
D1(cox.type, cox.typee)

# C3
cox.C3 = with(gu.imputed, coxph(Surv(time, status=="1") ~ C3))
summary(pool(cox.C3))
est.VAR.C3 <- pool(cox.C3)
summary(est.VAR.C3, conf.int = TRUE, exponentiate = TRUE)

# C4
cox.C4 = with(gu.imputed, coxph(Surv(time, status=="1") ~ C4))
summary(pool(cox.C4))
est.VAR.C4 <- pool(cox.C4)
summary(est.VAR.C4, conf.int = TRUE, exponentiate = TRUE)

# CRP
cox.CRP = with(imp.gu.new, coxph(Surv(time, status=="1") ~ CRP))
summary(pool(cox.CRP))
est.VAR.CRP <- pool(cox.CRP)
summary(est.VAR.CRP, conf.int = TRUE, exponentiate = TRUE)

# diabete
cox.dia = with(imp.gu.new, coxph(Surv(time, status=="1") ~ diabete))
summary(pool(cox.dia))
est.VAR.dia <- pool(cox.dia)
summary(est.VAR.dia, conf.int = TRUE, exponentiate = TRUE)

# hta
cox.hta = with(imp.gu.new, coxph(Surv(time, status=="1") ~ hta))
summary(pool(cox.hta))
est.VAR.hta <- pool(cox.hta)
summary(est.VAR.hta, conf.int = TRUE, exponentiate = TRUE)

# groups2
cox.group = with(imp.gu.new, coxph(Surv(time, status=="1") ~ groups2))
summary(pool(cox.group))
est.VAR.group <- pool(cox.group)
summary(est.VAR.group, conf.int = TRUE, exponentiate = TRUE)

# IMC
cox.imc = with(imp.gu.new, coxph(Surv(time, status=="1") ~ IMC))
summary(pool(cox.imc))
est.VAR.imc <- pool(cox.imc)
summary(est.VAR.imc, conf.int = TRUE, exponentiate = TRUE)

cox.imc <- coxph(Surv(time, status=="1") ~ pspline(IMC, df = 2), data = gu2)
termplot(cox.imc, se=TRUE, col.term=1, col.se=1)

#---------------------------------------------------------------------------------
# cox multivarie
# cox multivare for death at 12th month of follow up 
# this analysis is the principal analysis 

cox.multi = with(gu.imputed, coxph(Surv(time, status=="1") ~ age_diag + 
									#type_atteinte_rein 
									#+ groups
									+ tt_etude_4classes))
summary(pool(cox.multi))
est.VAR.imc <- pool(cox.multi)
summary(est.VAR.imc, conf.int = TRUE, exponentiate = TRUE)


##################################################################################

# SURV
#install.packages(survival)
#install.packages(lubridate)
install.packages("ggsurvfit")
library("ggsurvfit")

install.packages("gtsummary")
library("gtsummary")

install.packages("tidycmprsk")
library("tidycmprsk")

install.packages("condSURV")
library("condSURV")

survfit(Surv(gu2$time, gu2$status==1) ~ 1, 24) # at 24 months 

survfit2(Surv(time, status==1) ~ 1, data = gu2) %>% 
  ggsurvfit() +
  labs(
    x = "Years",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()

##################################################################################

library(survival)
install.packages("survminer")
library("survminer")
str(gu2$time)
gu2$time = as.numeric(gu2$time)
str(gu2$status)
gu2$status = as.factor(gu2$status)

death.fit <- surv_fit(Surv(time, status == 1) ~ 1, data = gu2#, group.by = "groups"
)
plot(death.fit)
surv_median(death.fit)


km.death <- survfit(Surv(time, status == 1) ~ groups2, data = gu2, conf.type = "log-log")
km.death1 <- survfit(Surv(time, status == 1) ~ 1, data = gu2, conf.type = "log-log")

plot(km.death)

install.packages("ggfortify")
library("ggfortify")
install.packages("ggplot2")
library("ggplot2")

autoplot(km.death) +
labs(x = "\n Survival Time (Days) ", y = "Survival Probabilities \n", 
 title = "Survival Times Of \n Patients \n") + 
 theme(plot.title = element_text(hjust = 0.5), 
 axis.title.x = element_text(face="bold", colour="#FF7A33", size = 12),
 axis.title.y = element_text(face="bold", colour="#FF7A33", size = 12),
 legend.title = element_text(face="bold", size = 10))

gu2$time
10651/365.25 # 29 ans de suivi 

install.packages("rms")
library("rms")

survplot(fit  = km.death,
         conf = c("none","bands","bars")[1],
         xlab = "", ylab = "Survival",
         ## xlim(0,100),
         label.curves = TRUE,                     # label curves directly
         ## label.curves = list(keys = "lines"),  # legend instead of direct label
         levels.only  = FALSE,                    # show only levels, no label
         abbrev.label = FALSE,                    # if label used, abbreviate
         ## fun = function(x) {1 - x},            # Cumulative probability plot         
         loglog   = FALSE,                        # log(-log Survival) plot
         logt     = FALSE,                        # log time
         time.inc = 100,                          # time increment
         dots     = TRUE,                         # dot grid
         n.risk   = TRUE,                         # show number at risk
         ## srt.n.risk = 0, sep.n.risk = 0.056, adj.n.risk = 1,
         ## y.n.risk = 0, cex.n.risk = 0.6
         )

mean(gu2$time)
median(gu2$time)

# incidence

install.packages("casebase")
library("casebase")
library(survival)
library(ggplot2)

#https://cran.r-project.org/web/packages/casebase/vignettes/plotabsRisk.html

smooth.death <- absoluteRisk(object = km.death, 
                                     newdata = gu2[c(1,600),],
                                     type = survival)
                                     
# http://dwoll.de/rexrepos/posts/survivalKM.html
                                     
plot(km.death, main=expression(paste("KM cumulative incidence")),
     fun=function(x) { 1- x },
     #xscale = 400,
     #yscale = 0.5,
     xlim=c(0,500),
     ylim=c(0,0.5),
     xlab="Time", ylab="Cumulative incidence", lwd=2, col=1:3)
legend(x="topright", col=1:3, lwd=2, legend=LETTERS[1:3])

##################################################################################
##################################################################################
##################################################################################

fullmodel <- glm( ~ , 
                 data = gu, family = binomial)

performance::check_collinearity(fit)

glmtoolbox::hltest(fit)

# PSEUDO-R2
mod <- glm(Y ~ X, data = lg, family="binomial")
nullmod <- glm(Y ~ 1, data = lg, family="binomial")
1-logLik(mod)/logLik(nullmod)

# 'log Lik.' 0.5329728 (df=11)

### ACCURACY OF THE COMPLETE MODEL BY BOOTSTRAPPING

performance_accuracy(
  cmod.lg,
  method = c(#"cv"#, #USE CROSSVALIDATION
  "boot" #USE BOOTSTRAP
  	), 
  k = 5,
  n = 1000,
  verbose = TRUE)

p1 <- predict(mod, train, type = 'response') 
head(p1, n=30)

### MISCLASSIFICATION ERROR ON TRAIN DATA
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$compl.s)
tab1
#         Actual
#Predicted   0   1
#        0  20   7
#        1  19 105

### MISCLASSIFICATION ERROR RATE 
1 - sum(diag(tab1))/sum(tab1)
# 0.1721854 ==> 17%

### MISCLASSIFICATION ON TEST DATA
p2 <- predict(mod.lg, test, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$compl.s)
tab2


### GOODNESS-OF-FIT 
pvalue = 1-pchisq(172 - 118, df=(150-140) #, lower.tail = F #specify lower tail to get extreme values or substract -1
)

### SHRINKED MODEL 

rest.mod <- glm(DEP ~ VAE, data = gu, family = "binomial")
summary(rest.mod)
AIC(rest.mod)

p1.r <- predict(rest.mod, train, type = 'response')
pred1.rest <- ifelse(p1.r>0.5, 1, 0)
rest.tab1 <- table(Predicted = pred1.rest, Actual = train$compl.s)
rest.tab1
1 - sum(diag(rest.tab1))/sum(rest.tab1)
#  ==> misclassification = 23%

p2.r <- predict(rest.mod, test, type = 'response')
pred2.rest <- ifelse(p2.r>0.5, 1, 0)
rest.tab2 <- table(Predicted = pred2.rest, Actual = test$compl.s)
rest.tab2
1 - sum(diag(rest.tab2))/sum(rest.tab2)
#  ==> misclassification = 28% (compared to 34% of the full model)
