getwd()
setwd("P:/CONSULTATION/Rat_AnneChristine/DMO_Fichiers_Excel_20220316/TablesExcel") # ON PC
setwd("/Users/damianocerasuolo/Desktop/UBRC/21_22_CONSULT_MAC/Rat_AnneChristine/DMO_Fichiers_Excel_20220316/TablesExcel")
#-------------------------------------------------------------------------------
#install.packages("readxl")
library("readxl")
#install.packages("dplyr")
library("dplyr")
#install.packages("plyr")
library("plyr")
#-------------------------------------------------------------------------------
# ON MERGING DATAFRAMES:  https://stackoverflow.com/questions/15162197/combine-
# rbind-data-# frames-and-create-column-with-name-of-original-data-frames

a0 <- read_excel("DMO_Annee0.xls", na="NA")
a3 <- read_excel("DMO_Annee3.xls", na="NA")
#a03 <- rbind.fill(a0, a3)
a4 <- read_excel("DMO_Annee4.xls", na="NA")
a5 <- read_excel("DMO_Annee5.xls", na="NA")
a6 <- read_excel("DMO_Annee6.xls", na="NA")
a7 <- read_excel("DMO_Annee7.xls", na="NA")
a8 <- read_excel("DMO_Annee8.xls", na="NA")
a9 <- read_excel("DMO_Annee9.xls", na="NA")
a10 <- read_excel("DMO_Annee10.xls", na="NA")
pro <- read_excel("DMO_Protheses.xls", na="NA")

#full<-do.call(rbind.fill, list(df1 = a0, df2 = a3, df3 = a4, df4 = a5, df5 = a6,
#                                df6 = a7, df7 = a8, df8 = a9, df9 = a10)) #working
#dim(full)
dim(a0)
dim(a3)
dim(a4)
dim(a5)
dim(a6)
dim(a7)
dim(a8)
dim(a9)
dim(a10)
878+747+631+675+573+547+520+466+462

#-------------------------------------------------------------------------------
names(a0)
table(a0$ArticIncl) # articulation d'inclusion
#-------------------------------------------------------------------------------
# REPLACE VARIABLES

a0$visit.0 = a0$VISIT
a0 = subset(a0, select = -c(VISIT))

a3$visit.3 = a3$VISIT
a3 = subset(a3, select = -c(VISIT))

a4$visit.4 = a4$VISIT
a4 = subset(a4, select = -c(VISIT))

a5$visit.5 = a5$VISIT
a5 = subset(a5, select = -c(VISIT))

a6$visit.6 = a6$VISIT
a6 = subset(a6, select = -c(VISIT))

a7$visit.7 = a7$VISIT
a7 = subset(a7, select = -c(VISIT))

a8$visit.8 = a8$VISIT
a8$IdCohorte = a8$IDCOHORTE
a8 = subset(a8, select = -c(IDCOHORTE, VISIT))

a9$IdCohorte = a9$IDCOHORTE 
a9$visit.9 = a9$VISIT
a9 = subset(a9, select = -c(IDCOHORTE, VISIT))

a10$visit.10 = rep(10, times = 462)
#-------------------------------------------------------------------------------
### DATE OF THE VISIT 
# create the date of the visit 3 adding 3 years to the inclusion date
# notes: this variable will be created 

library(lubridate)
pro$visit.0d = as.Date(all.v$date_inclusion)

a3$visit.3[is.na(a3$visit.3)] <- "0"
table(a3$visit.3)

pro$inclusion3 = (as.Date(pro$date_inclusion) %m+% years(3))
table(pro$inclusion3, pro$date_inclusion)
#-------------------------------------------------------------------------------
### FIRST EVENT CHECK
## RECODE THE BINARY EVENT VARIABLE IN ORDER TO HAVE 0s WHERE THE EVENT DIDN'T HAPPEN
str(pro$PROTH_GD) # THE VARIABLES ARE NUMERIC

pro$PROTH_GD[is.na(pro$PROTH_GD)] <- 0
table(pro$PROTH_GD)

pro$PROTH_GG[is.na(pro$PROTH_GG)] <- 0
pro$PROTH_HD[is.na(pro$PROTH_HD)] <- 0
pro$PROTH_HG[is.na(pro$PROTH_HG)] <- 0
#------------------------------------------------
# EVENT DATE AFTER THE INCLUSION DATE 
# ONLY INCIDENT CASES 

pro <- pro %>%
  mutate(DTPROTH_GD.i = case_when(
    as.Date(DTPROTH_GD) > as.Date(date_inclusion) ~ as.Date(DTPROTH_GD)
  ))

pro <- pro %>% 
  mutate(DTPROTH_GG.i = case_when(
    as.Date(DTPROTH_GG) > as.Date(date_inclusion) ~ as.Date(DTPROTH_GG)
  ))

pro <- pro %>%
  mutate(DTPROTH_HD.i = case_when(
    as.Date(DTPROTH_HD) > as.Date(date_inclusion) ~ as.Date(DTPROTH_HD)
  ))

pro <- pro %>%
  mutate(DTPROTH_HG.i = case_when(
    as.Date(DTPROTH_HG) > as.Date(DTPROTH_HG) ~ as.Date(DTPROTH_HG)
  ))


## FIRST EVENT

pro <- pro %>%
  mutate(first = case_when(
    # LA DATE DE LA prothese genou droit EST INFERIEURE AUX AUTRES PROTHESES
    as.Date(DTPROTH_GD.i) < as.Date(DTPROTH_GG.i) | 
      as.Date(DTPROTH_GD.i) < as.Date(DTPROTH_HD.i) | 
      as.Date(DTPROTH_GD.i) < as.Date(DTPROTH_HG.i) ~ "pro.gd",
    # LA DATE DE LA prothese genou gauche EST INFERIEURE AUX AUTRES PROTHESES
    as.Date(DTPROTH_GG.i) < as.Date(DTPROTH_GD.i) |
      as.Date(DTPROTH_GG.i) < as.Date(DTPROTH_HD.i) |
      as.Date(DTPROTH_GG.i) < as.Date(DTPROTH_HG.i) ~ "pro.gg",
    # LA DATE DE LA prothese de hanche droite EST INFERIEURE AUX AUTRES PROTHESES
    as.Date(DTPROTH_HD.i) < as.Date(DTPROTH_GD.i) |
      as.Date(DTPROTH_HD.i) < as.Date(DTPROTH_GG.i) |
      as.Date(DTPROTH_HD.i) < as.Date(DTPROTH_HG.i) ~ "pro.hd",
    # LA DATE DE LA prothese de hance gauche EST INFERIEURE AUX AUTRES PROTHESES 
    as.Date(DTPROTH_HG.i) < as.Date(DTPROTH_GD.i) |
      as.Date(DTPROTH_HG.i) < as.Date(DTPROTH_GG.i) |
      as.Date(DTPROTH_HG.i) < as.Date(DTPROTH_HD.i) ~ "pro.hg"))

table(pro$first)
# before having taken into account the inclusion date
# pro.gd pro.gg pro.hd pro.hg 
# 35     20     19     11      = 85 first events 

# after having taken into account the inclusion date
#pro.gd pro.gg pro.hd pro.hg 
#31     19     17     10 

# ONLY INCIDENT EVENTS
#pro.gd pro.gg pro.hd 
#28     17      6 

#------------------------------------------------
# veryfing thate there is no HANCHE GAUCHE event in the data

pro$PROTH_HG.n = as.numeric(pro$PROTH_HG)
pro$date_inclusion.n = as.numeric(pro$date_inclusion)

hanche.g <- pro %>%
  select(PROTH_GD, PROTH_GG, PROTH_HD, PROTH_HG.n, date_inclusion.n) %>%
  filter(PROTH_HG.n > date_inclusion.n)
dim(hanche.g)
View(hanche.g)
# there is no incident event for the HANCHE GAUCHE

pro = subset(pro, select = -c(date_inclusion.n, PROTH_HG.n))

#------------------------------------------------
### EVENT DATE

str(pro$DTPROTH_GD)

pro <- pro %>%
  mutate(event.date = case_when(
    PROTH_GD == 1 & first == "pro.gd" ~ as.Date(DTPROTH_GD.i),
    PROTH_GG == 1 & first == "pro.gg" ~ as.Date(DTPROTH_GG.i),
    PROTH_HD == 1 & first == "pro.hd" ~ as.Date(DTPROTH_HD.i), 
    PROTH_HG == 1 & first == "pro.hg" ~ as.Date(DTPROTH_HG.i)
  ))

str(pro$event.date)
table(pro$event.date)

#------------------------------------------------
### EVENT AS BINARY VARIABLE 

pro <- pro %>% 
  mutate(event.bin = case_when(
    PROTH_GD == 1 & first == "pro.gd" ~ "1",
    PROTH_GG == 1 & first == "pro.gg" ~ "1",
    PROTH_HD == 1 & first == "pro.hd" ~ "1",
    PROTH_HG == 1 & first == "pro.hg" ~ "1"
                                 ))
table(pro$event.bin)
pro$event.bin[is.na(pro$event.bin)] <- "0"
#   0   1 
# 793  85 first events, while 793 have no event at all ? 

#------------------------------------------------

### NUMBER OF MULTIPLE EVENT

pro <- pro %>%
  mutate(multiple = case_when(
    (PROTH_GD + PROTH_GG + PROTH_HD + PROTH_HG) == 4 ~ "4",
    (PROTH_GD + PROTH_GG + PROTH_HD + PROTH_HG) == 3 ~ "3",
    (PROTH_GD + PROTH_GG + PROTH_HD + PROTH_HG) == 2 ~ "2",
    (PROTH_GD + PROTH_GG + PROTH_HD + PROTH_HG) == 1 ~ "1",
    (PROTH_GD + PROTH_GG + PROTH_HD + PROTH_HG) == 0 ~ "0"
  ))

table(pro$multiple)
table(pro$multiple, pro$first)







#-------------------------------------------------------------------------------
## LAST VISIT FOLLOW UP 

all.v <- all.v %>% mutate(last = case_when(
  visit.3 == "lost" ~ visit.0d, 
  visit.4 == "lost" ~ visit.3d,
  visit.5 == "lost" ~ visit.4d,
  visit.6 == "lost" ~ visit.5d,
  visit.7 == "lost" ~ visit.6d,
  visit.8 == "lost" ~ visit.7d,
  visit.9 == "lost" ~ visit.8d,
  visit.10 == "lost" ~ visit.9d,
  visit.10 == "10" ~ visit.10d
)) 

table(all.v$last)
#-------------------------------------------------------------------------------
### SELECT THE VARIABLES NEEDED TO CALCULATE THE FOLLOW-UP 
all.v.small = subset(all.v, select = c(IdCohorte, last))

# LIST TO MERGE
df_list.2 = list(pro, all.v.small)

# MERGE
pro.f <- df_list.2 %>% reduce(full_join, by='IdCohorte')
dim(pro)
dim(pro.f)
#-------------------------------------------------------------------------------
### CALUCULATE THE FOLLOW UP 
# AS FOLLOWS: 
# to the last visit for patients not having the event
# to the event, for patients having the event

pro.f$event.bin = as.factor(pro$event.bin)

pro.f <- pro.f %>%
  mutate(fup = case_when(
    event.bin == "1" ~ (as.Date(DTPROTH_GD.i) - as.Date(date_inclusion)),
    event.bin == "1" ~ (as.Date(DTPROTH_GG.i) - as.Date(date_inclusion)),
    event.bin == "1" ~ (as.Date(DTPROTH_HD.i) - as.Date(date_inclusion)),
    event.bin == "1" ~ (as.Date(DTPROTH_HG.i) - as.Date(date_inclusion)),
    event.bin == "0" ~ (as.Date(last) - as.Date(date_inclusion))
  ))

table(pro.f$fup)
#-------------------------------------------------------------------------------
### DATABASE TO MERGE WITH OTHER DATABASES CONTAINING PATIENTS CHARACTERISTICS
pro.merge = subset(pro.f, select = c(IdCohorte, fup, event.bin, event.date, first, last,
                                     PROTH_GD, PROTH_GG, PROTH_HD, PROTH_HG,
                                     DTPROTH_GD.i, DTPROTH_GG.i, DTPROTH_HD.i, DTPROTH_HG.i, 
                                     date_inclusion))

### SAVE DATA
write.table(pro.merge, file = "prothesis.csv", sep = "\t", row.names = TRUE)
procsv <- read.csv2("prothesis.csv", header = TRUE, na.string="NA", sep = "\t")
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
### CRITERE DE JUGEMENT PRINCIPAL 
### SURVIE A LA POSE DE LA PROTHESE 
library("survival")
library("survminer")
install.packages("ranger")
library("ranger")
library("ggplot2")
library("ggfortify")

str(pro.merge$event.bin)
str(pro.merge$fup)
pro.merge$followup = as.numeric(as.character(pro.merge$fup))
pro.merge$event.binaire = as.numeric(as.character(pro.merge$event.bin))

# follow up in months
pro.merge <- pro.merge %>% 
  mutate(followup.months = round(followup/30.417, digit=0))

KM.pro = with(pro.merge, Surv(followup, event.bin))
head(KM.pro, 100)

KM.pro.fit = survfit(Surv(followup.months, event.binaire) ~ 1, data = pro.merge)
summary(KM.pro.fit, times = c(1, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120*(1:10)))
autoplot(KM.pro.fit)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
### UNIVARIATE ON THE INCLUSION BASE
### SELECT THE VARIABLES NEEDED TO CALCULATE THE FOLLOW-UP 
names(a0)
names(pro.merge)

# LIST TO MERGE
data.univ = list(pro.merge, a0)

# MERGE
baseline <- data.univ %>% reduce(full_join, by='IdCohorte')

# VARIABLE AGE AT BASELINE
baseline$age = as.numeric(as.character((as.Date(baseline$date_inclusion) - 
                                          as.Date(baseline$DTNAISS))/365.25))

# VAR TRANSFORMATION BEFORE DESCRIPTIVE AND UNIVARIATE 
library("tableone")
dput(names(baseline))

baseline$followup.descriptive = as.numeric(as.character(baseline$fup))
baseline$event.binaire = as.numeric(as.character(baseline$event.bin))

# FACTORIAL TRANSFORMATION FUNCTION
f <- function(x){
  result = as.factor(x)
  return(result)
}

baseline$SEXE = f(baseline$SEXE)
baseline$EDUCATION = f(baseline$EDUCATION)
baseline$MARITAL <- f(baseline$MARITAL)
baseline$COMMUNE2 = f(baseline$COMMUNE2)
baseline$PROFESSION = f(baseline$PROFESSION)
baseline$RETRAITE = f(baseline$RETRAITE)
baseline$ArticIncl = f(baseline$ArticIncl)

baseline.var <- c("followup.descriptive", "event.bin", 
                  "PROTH_GD", "PROTH_GG", "PROTH_HD", "PROTH_HG", "SEXE", "age", 
                  "EDUCATION", "MARITAL", "COMMUNE2", "PROFESSION", "RETRAITE", 
                  "ArticIncl", "QC_G_ARTHR", "QC_G_DROITE", "QC_G_GAUCHE", "QC_H_ARTHR", 
                  "QC_H_DROITE", "QC_H_GAUCHE", "QC_SYMPT_MOIS", "QC_SYMPT_ANNEE", 
                  "QC_DIAG_MOIS", "EKL_GD_FT", "EKL_GG_FT", "SKL_GD_FT", 
                  "SKL_GG_FT", "HKL_HD", "HKL_HG", "EOsteoD_condylI", "EOsteoG_condylI", 
                  "EOsteoD_condylE", "EOsteoG_condylE", "EOsteoD_platI", "EOsteoG_platI", 
                  "EOsteoD_platE", "EOsteoG_platE", "Score_Osteo_GD", "Score_Osteo_GG", 
                  "Score_Osteo", "SOsteoD_condylI", "SOsteoG_condylI", "SOsteoD_condylE", 
                  "SOsteoG_condylE", "SOsteoD_platI", "SOsteoG_platI", "SOsteoD_platE", 
                  "SOsteoG_platE", "Score_Schuss_GD", "Score_Schuss_GG", "Score_Schuss", 
                  "EOsteoD_trochlI", "EOsteoG_trochlI", "EOsteoD_trochlE", "EOsteoG_trochlE", 
                  "EOsteoD_rotuleI", "EOsteoG_rotuleI", "EOsteoD_rotuleE", "EOsteoG_rotuleE", 
                  "Score_FemoP_GD", "Score_FemoP_GG", "Score_FemoP", "Score_Osteo_T_GD", 
                  "Score_Osteo_T_GG", "Score_Osteo_T", "HOsteo_HD_cotyle_E", "HOsteo_HG_cotyle_E", 
                  "HOsteo_HD_cotyle_Int", "HOsteo_HG_cotyle_Int", "HOsteo_HD_cotyle_Inf", 
                  "HOsteo_HG_cotyle_Inf", "HOsteo_HD_tete_S", "HOsteo_HG_tete_S", 
                  "HOsteo_HD_tete_I", "HOsteo_HG_tete_I", "Score_Osteo_HD", "Score_Osteo_HG", 
                  "Score_Osteo_H", "EPincement_GD_FT", "Epincement_GG_FT", "SPincement_GD_FT", 
                  "Spincement_GG_FT", "Pincement_Max_grade_GD", "Pincement_Max_grade_GG", 
                  "Pincement_Max_grade_G", "HPincement_HD_grade", "HPincement_HG_grade", 
                  "Pincement_Max_grade_H")

baseline.catvar <- c("event.bin", "PROTH_GD", "PROTH_GG", "PROTH_HD", 
                     "PROTH_HG", "SEXE", "EDUCATION", "MARITAL", 
                     "COMMUNE2", "PROFESSION", "RETRAITE", "ArticIncl")

descriptive = CreateTableOne(vars = baseline.var, data = baseline, 
                             factorVars = baseline.catvar)
print(descriptive, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

univariate = CreateTableOne(vars = baseline.var, data = baseline, 
                            factorVars = baseline.catvar, test = TRUE,
                            strata = "event.bin")
print(univariate, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# COX UNIVARIATE

covariates <- c("SEXE", "age", 
                  "EDUCATION", 
                #"MARITAL", 
                "COMMUNE2", "PROFESSION", "RETRAITE", 
                  "ArticIncl", 
                "QC_G_ARTHR", 
                #"QC_G_DROITE", "QC_G_GAUCHE", "QC_H_ARTHR", 
                  "QC_H_DROITE", "QC_H_GAUCHE", "QC_SYMPT_MOIS", "QC_SYMPT_ANNEE", 
                  "QC_DIAG_MOIS", "EKL_GD_FT", "EKL_GG_FT", "SKL_GD_FT", 
                  "SKL_GG_FT", "HKL_HD", "HKL_HG", "EOsteoD_condylI", "EOsteoG_condylI", 
                  "EOsteoD_condylE", "EOsteoG_condylE", "EOsteoD_platI", "EOsteoG_platI", 
                  "EOsteoD_platE", "EOsteoG_platE", "Score_Osteo_GD", "Score_Osteo_GG", 
                  "Score_Osteo", "SOsteoD_condylI", "SOsteoG_condylI", "SOsteoD_condylE", 
                  "SOsteoG_condylE", "SOsteoD_platI", "SOsteoG_platI", "SOsteoD_platE", 
                  "SOsteoG_platE", "Score_Schuss_GD", "Score_Schuss_GG", "Score_Schuss", 
                  "EOsteoD_trochlI", "EOsteoG_trochlI", "EOsteoD_trochlE", "EOsteoG_trochlE", 
                  "EOsteoD_rotuleI", "EOsteoG_rotuleI", "EOsteoD_rotuleE", "EOsteoG_rotuleE", 
                  "Score_FemoP_GD", "Score_FemoP_GG", "Score_FemoP", "Score_Osteo_T_GD", 
                  "Score_Osteo_T_GG", "Score_Osteo_T", "HOsteo_HD_cotyle_E", "HOsteo_HG_cotyle_E", 
                  "HOsteo_HD_cotyle_Int", "HOsteo_HG_cotyle_Int", "HOsteo_HD_cotyle_Inf", 
                  "HOsteo_HG_cotyle_Inf", "HOsteo_HD_tete_S", "HOsteo_HG_tete_S", 
                  "HOsteo_HD_tete_I", "HOsteo_HG_tete_I", "Score_Osteo_HD", "Score_Osteo_HG", 
                  "Score_Osteo_H", "EPincement_GD_FT", "Epincement_GG_FT", "SPincement_GD_FT", 
                  #"Spincement_GG_FT", "Pincement_Max_grade_GD", 
                "Pincement_Max_grade_GG", 
                  "Pincement_Max_grade_G" 
                #, "HPincement_HD_grade", "HPincement_HG_grade", "Pincement_Max_grade_H"
                )


univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(followup.descriptive, event.bin==1)~', x)))

univ_models <- lapply(univ_formulas, function(x){coxph(x, data = baseline)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)

#-------------------------------------------------------------------------------

# UNIVARIATE BY COX FORMULA

cox <- function(x){
  result = coxph(Surv(baseline$followup.descriptive, baseline$event.bin==1) ~ x)
  return(summary(result))
}

cox(baseline$SEXE)
sex <- coxph(Surv(baseline$followup.descriptive, baseline$event.bin==1) ~ baseline$SEXE)
summary(sex)

cox(baseline$age)
cox(baseline$EDUCATION)
cox(baseline$MARITAL)
cox(baseline$COMMUNE2)

cox(baseline$PROFESSION)
cox(baseline$RETRAITE)
cox(baseline$ArticIncl)
cox(baseline$QC_G_ARTHR)
cox(baseline$QC_G_DROITE)
cox(baseline$QC_G_GAUCHE)

cox(baseline$QC_H_ARTHR)
cox(baseline$QC_H_DROITE)
cox(baseline$QC_H_GAUCHE)

cox(baseline$QC_SYMPT_MOIS)
cox(baseline$QC_SYMPT_ANNEE)
cox(baseline$QC_DIAG_MOIS)
cox(baseline$EKL_GD_FT)

cox(baseline$EKL_GG_FT)
cox(baseline$SKL_GD_FT)

cox(baseline$SKL_GG_FT)
cox(baseline$HKL_HD)
cox(baseline$HKL_HG)

cox(baseline$EOsteoD_condylI)
cox(baseline$EOsteoG_condylI)
cox(baseline$EOsteoD_condylE)
cox(baseline$EOsteoG_condylE) 
cox(baseline$EOsteoD_platI) 
cox(baseline$EOsteoG_platI)

cox(baseline$EOsteoD_platE)
cox(baseline$EOsteoG_platE) 
cox(baseline$Score_Osteo_GD) 
cox(baseline$Score_Osteo_GG)
cox(baseline$Score_Osteo)

cox(baseline$SOsteoD_condylI)
cox(baseline$SOsteoG_condylI)
cox(baseline$SOsteoD_condylE)
cox(baseline$SOsteoG_condylE)

cox(baseline$SOsteoD_platI)
cox(baseline$SOsteoG_platI)
cox(baseline$SOsteoD_platE)
cox(baseline$SOsteoG_platE)
cox(baseline$Score_Schuss_GD)

cox(baseline$Score_Schuss_GG)

cox(baseline$Score_Schuss)

cox(baseline$EOsteoD_trochlI)
cox(baseline$EOsteoG_trochlI)
cox(baseline$EOsteoD_trochlE)
cox(baseline$EOsteoG_trochlE)

cox(baseline$EOsteoD_rotuleI)
cox(baseline$EOsteoG_rotuleI)
cox(baseline$EOsteoD_rotuleE)
cox(baseline$EOsteoG_rotuleE)

cox(baseline$Score_FemoP_GD)
cox(baseline$Score_FemoP_GG)
cox(baseline$Score_FemoP)

cox(baseline$Score_Osteo_T_GD)
cox(baseline$Score_Osteo_T_GG)
cox(baseline$Score_Osteo_T)

cox(baseline$HOsteo_HD_cotyle_E)
cox(baseline$HOsteo_HG_cotyle_E)

cox(baseline$HOsteo_HD_cotyle_Int)
cox(baseline$HOsteo_HG_cotyle_Int)
cox(baseline$HOsteo_HD_cotyle_Inf)
cox(baseline$HOsteo_HG_cotyle_Inf)

cox(baseline$HOsteo_HD_tete_S)
cox(baseline$HOsteo_HG_tete_S)
cox(baseline$HOsteo_HD_tete_I)
cox(baseline$HOsteo_HG_tete_I)

cox(baseline$Score_Osteo_HD)

cox(baseline$Score_Osteo_HG)
cox(baseline$Score_Osteo_H)

cox(baseline$EPincement_GD_FT)
cox(baseline$Epincement_GG_FT)
cox(baseline$SPincement_GD_FT)
cox(baseline$Spincement_GG_FT)
cox(baseline$Pincement)

cox(baseline$Max_grade_GD)
cox(baseline$Pincement_Max_grade_GG)
cox(baseline$Pincement_Max_grade_G)
cox(HPincement_HD_grade)
cox(HPincement_HG_grade)
cox(Pincement_Max_grade_H)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

### THE MULTILINE DATABASE FOR THE FRAILTY MODEL

names(pro.merge)
library(tidyr)
library(here)

all.id <- subset(all.v, select = c(IdCohorte, 
                                  visit.0d, 
                                  visit.3d, 
                                  visit.4d, 
                                  visit.5d, 
                                  visit.6d, 
                                  visit.7d, 
                                  visit.8d, 
                                  visit.9d,
                                  visit.10d))
names(all.id)

all.id <- as_tibble(all.id)

all.idlong <- all.id %>%
  pivot_longer(!IdCohorte, names_to = "visitID", values_to = "visitDATE")
dim(all.idlong)
View(all.idlong)
names(all.idlong)

all.idlong$ID[all.idlong$visitID == "visit.0d"] <- "1"
all.idlong$ID[all.idlong$visitID == "visit.3d"] <- "2"
all.idlong$ID[all.idlong$visitID == "visit.4d"] <- "3"
all.idlong$ID[all.idlong$visitID == "visit.5d"] <- "4"
all.idlong$ID[all.idlong$visitID == "visit.6d"] <- "5"
all.idlong$ID[all.idlong$visitID == "visit.7d"] <- "6"
all.idlong$ID[all.idlong$visitID == "visit.8d"] <- "7"
all.idlong$ID[all.idlong$visitID == "visit.9d"] <- "8"
all.idlong$ID[all.idlong$visitID == "visit.10d"] <- "9"

all.idlong <- subset(all.idlong, select = -c(visitID)) 
                                   
### INCIDENT EVENT TO THE FRAILTY MODEL 

event.base = subset(pro, select = c(IdCohorte, 
                                    #date_inclusion,
                                    DTPROTH_GD, #PROTH_GD,
                                    DTPROTH_GG, #PROTH_GG,
                                    DTPROTH_HD, #PROTH_HD,
                                    DTPROTH_HG #, PROTH_HG
                                    ))

event.base.long = event.base %>%
  pivot_longer(!IdCohorte, names_to = "protype", values_to = "protdate")
View(event.base.long)

ID.INCL = subset(all.v, select = c(IdCohorte, date_inclusion))

list.inclpro = list(ID.INCL, event.base.long)
incidentbase = list.inclpro %>% reduce(full_join, by = "IdCohorte")

incidentbase$date_inclusion = as.Date(incidentbase$date_inclusion)
incidentbase$protdate = as.Date(incidentbase$protdate)
dim(incidentbase)

event.incident = incidentbase %>%
  filter(protdate > date_inclusion)
dim(event.incident)

#event.base.inc <- event.base %>% 
#  select(IdCohorte, date_inclusion,
#         DTPROTH_GD, PROTH_GD,
#         DTPROTH_GG, PROTH_GG,
#         DTPROTH_HD, PROTH_HD,
#         DTPROTH_HG, PROTH_HG) %>%
#  filter(DTPROTH_GD > date_inclusion | date_inclusion)

### MERGING
# DATABASE LIST
frailist = list(all.idlong, event.base)

# MERGE
library(tidyverse)
frailbase <- frailist %>% reduce(full_join, by='IdCohorte')
dim(frailbase)
dim(event.base)
dim(all.idlong)
names(frailbase)
View(frailbase)

#-------------------------------------------------------------------------------

### THE INCIDENT EVENT IS STORED IN THE event.incident base 
### THE MULTIPLE LINE FOR PATIENT BASE IS THE frailbase
### THE NEXT STEP IS TO REPLACE THE EVENT INTO A VISIT, IF NEEDED
### OR, BETTER, TO ASSOCIATE IT WITH THE PROPER ID


score = a3$FCI01_1 + a3$COMMORB04 + a3$COMMORB07 + a3$COMMORB08 + a3$COMMORB09 +
  a3$FCI06_1 + a3$COMMORB10 + a3$FCI08 + a3$FCI09_1 + 
  a3$COMMORB14 + a3$COMMORB42 + a3$FCI12 + a3$COMMORB30 + a3$FCI14_1
  + a3$FCI15_1 + a3$COMMORB40 + a3$FCI17_1 + a3$COMMORB43

table(score, a3$FCI01_1)
