#-CODE FOR A FIRST MODEL-----------------------------------------------------------------

require("here")
require("readxl")
require("tidyverse")
require("lme4")

here()

vt <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Vargeois_Thibaut", 
                      "vata_dataANSWERS.xlsx"))

names(vt)
dim(vt)

#table(vt$Time)
str(vt$Time)

# exposition aux guidelines oui, non 
table(vt$Guidelines)

# questions questionnaire standardisé 
table(vt$IHScore)
str(vt$IHScore)

table(vt$IBScore)
str(vt$IBScore)

# idéntifiant de chaque participant 
# IDTri 
# ID
str(vt$ID)
# chaue participant peut intervenir de 0 à 15 fois 

# variable dépendante Answer
# utilitarian and non-utilitarian answer
table(vt$Answer)
vt$Answer01 = as.factor(if_else(vt$Answer == "NU", "0", "1"))

# model 

m <- glmer(Answer01 ~ IHScore + IBScore + (1|IDTri_or), data = vt, family = "binomial",
           control = glmerControl(optimizer = "bobyqa"))

print(m, corr = FALSE)

# confidence intervals for the independent variables 
se <- sqrt(diag(vcov(m)))
tab <- (cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 * se))
tab

# and odds ratios
exp(tab)

# model 2

m2 <- glmer(Answer01 ~ IHScore + IBScore + Sexe + ATCDEthicKnowledge + Guidelines + 
              #Religion + 
              Age + (1|IDTri_or), data = vt, family = "binomial", 
              control = glmerControl(optimizer = "bobyqa",
              optCtrl = list(maxfun = 100000)
              ))
print(m2, corr = F)

# model 3 
# recode the age variable 

vt <- vt %>%
  mutate(AgeCl = case_when(
    Age < 29 ~ "1",
    Age >= 29 & Age < 34 ~ "2",
    Age >= 34 & Age < 42 ~ "3",
    Age >= 42 ~ "4"
  ))
table(vt$AgeCl, useNA = "always")


m3 <- glmer(Answer01 ~ IHScore + IBScore + Sexe + 
              # ATCDEthicKnowledge + Guidelines + 
              #Religion + 
              AgeCl + (1|IDTri_or), data = vt, family = "binomial", 
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 100000)
            ))
print(m3, corr = F)
# confidence intervals for the independent variables 
se <- sqrt(diag(vcov(m3)))
tab3 <- (cbind(Est = fixef(m3), LL = fixef(m3) - 1.96 * se, UL = fixef(m3) + 1.96 * se))
tab3

# model 4 
# recoding the variables

vt <- vt %>%
  mutate(AgeCl = case_when(
    Age < 29 ~ "1",
    Age >= 29 & Age < 34 ~ "2",
    Age >= 34 & Age < 42 ~ "3",
    Age >= 42 ~ "4"
  ))
table(vt$AgeCl, useNA = "always")

str(vt$FamiliarityEthic)

vt <- vt %>%
  mutate(fa_eth = case_when(
    FamiliarityEthic ==  0 ~ "0",
    FamiliarityEthic > 0 & FamiliarityEthic <= 4 ~ "1",
    FamiliarityEthic >= 5 & FamiliarityEthic <= 8 ~ "2",
    FamiliarityEthic >= 9 & FamiliarityEthic <= 10 ~ "3"
  ))

table(vt$FamiliarityEthic, useNA = "always")
table(vt$fa_eth, useNA = "always")

str(vt$FamiliarityMoralPhilo)

vt <- vt %>%
  mutate(fa_moral = case_when(
    FamiliarityMoralPhilo == 0 ~ "0",
    FamiliarityMoralPhilo > 0 & FamiliarityMoralPhilo <= 4 ~ "1",
    FamiliarityMoralPhilo >= 5 & FamiliarityMoralPhilo <= 8 ~ "2",
    FamiliarityMoralPhilo >= 9 & FamiliarityMoralPhilo <= 10 ~ "3",
  ))

table(vt$fa_moral, useNA = "always")

vt <- vt %>%
  mutate(re2 = case_when(
    Religion == 0 ~ "No",
    Religion > 0 ~ "Yes"
  ))

table(vt$Religion, useNA = "always")
table(vt$re2, useNA = "always")

m4 <- glmer(Answer01 ~ IHScore + IBScore + Sexe + 
              fa_moral + fa_eth + re2 + 
              AgeCl + (1|IDTri_or), data = vt, family = "binomial", 
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 100000) #, nAGQ = 10
            ))
print(m4, corr = F)
# confidence intervals for the independent variables 
se <- sqrt(diag(vcov(m4)))
tab4 <- (cbind(Est = fixef(m4), LL = fixef(m4) - 1.96 * se, UL = fixef(m4) + 1.96 * se))
tab4

exp(tab4)

m4 <- glmer(Answer01 ~ IHScore + IBScore + Sexe + 
              fa_moral + fa_eth + re2 + 
              AgeCl + (1|IDTri_or), data = vt, family = "binomial", 
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 100000) #, nAGQ = 10
            ))
print(m4, corr = F)
# confidence intervals for the independent variables 
se <- sqrt(diag(vcov(m4)))
tab4 <- (cbind(Est = fixef(m4), LL = fixef(m4) - 1.96 * se, UL = fixef(m4) + 1.96 * se))
tab4

exp(tab4)



m5 <- glmer(Answer01 ~ Time + IHScore + IBScore + Sexe + 
              fa_moral + fa_eth + re2 + 
              AgeCl + (1|IDTri_or), data = vt, family = "binomial", 
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 100000) #, nAGQ = 10
            ))
print(m5, corr = F)
# confidence intervals for the independent variables 
se <- sqrt(diag(vcov(m5)))
tab5 <- (cbind(Est = fixef(m5), LL = fixef(m5) - 1.96 * se, UL = fixef(m5) + 1.96 * se))
tab5

exp(tab5)



m6 <- glmer(Answer01 ~ Time + Guidelines +  IHScore + IBScore + 
              fa_moral + fa_eth + re2 + 
              (1|IDTri_or), data = vt, family = "binomial", 
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 100000) #, nAGQ = 10
            ))
print(m6, corr = F)
# confidence intervals for the independent variables 
se <- sqrt(diag(vcov(m6)))
tab6 <- (cbind(Est = fixef(m6), LL = fixef(m6) - 1.96 * se, UL = fixef(m6) + 1.96 * se))
tab6

exp(tab6)




m7 <- glmer(Answer01 ~ Time +  IHScore + IBScore +  
              fa_moral + fa_eth + re2 + 
              (1|IDTri_or) + (1|Guidelines), data = vt, family = "binomial", 
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 100000), nAGQ = 0
            ))
print(m7, corr = F)
# confidence intervals for the independent variables 
se <- sqrt(diag(vcov(m7)))
tab7 <- (cbind(Est = fixef(m7), LL = fixef(m7) - 1.96 * se, UL = fixef(m7) + 1.96 * se))
tab7

exp(tab7)


m8 <- glmer(Answer01 ~ Time +  IHScore + IBScore +  
              fa_moral + fa_eth + re2 + 
              (1|Guidelines/IDTri_or), data = vt, family = "binomial", 
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 100000), nAGQ = 0
            ))
print(m8, corr = F)
# confidence intervals for the independent variables 
se <- sqrt(diag(vcov(m8)))
tab8 <- (cbind(Est = fixef(m8), LL = fixef(m8) - 1.96 * se, UL = fixef(m8) + 1.96 * se))
tab8

exp(tab8)

#-PREDICTION-----------------------------------------------------------------------------

tmpdat = vt[, c("IHScore", "IBScore", "Sexe", "fa_moral", "fa_eth", "re2", 
                  "AgeCl", "IDTri_or")]

# prediction for IHScore

summary(vt$IHScore)

jvalues <- with(vt, seq(from = min(IHScore), to = max(IHScore), length.out = 100))

# calculate predicted probabilities and store in a list
pp <- lapply(jvalues, function(j) {
  tmpdat$IHScore <- j
  predict(m4, newdata = tmpdat, 
          allow.new.levels = TRUE, type = "response") # am i sure i want to allow new levels? 
})

sapply(pp[c(1, 20, 40, 60, 80, 100)], mean)

# get the means with lower and upper quartiles
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))

# add in LengthofStay values and convert to data frame
plotdat <- as.data.frame(cbind(plotdat, jvalues))

# better names and show the first few rows
colnames(plotdat) <- c("PredictedProbability", "Lower", "Upper", "IHScore")
head(plotdat)

# plot average marginal predicted probabilities
ggplot(plotdat, aes(x = IHScore, y = PredictedProbability)) + geom_line() +
  ylim(c(0, 1))

#----------

# prediction for IBScore

summary(vt$IBScore)

jvalues <- with(vt, seq(from = min(IBScore), to = max(IBScore), length.out = 100))

# calculate predicted probabilities and store in a list
pp <- lapply(jvalues, function(j) {
  tmpdat$IBScore <- j
  predict(m4, newdata = tmpdat, 
          allow.new.levels = TRUE, type = "response") # am i sure i want to allow new levels? 
})

sapply(pp[c(1, 20, 40, 60, 80, 100)], mean)

# get the means with lower and upper quartiles
plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))

# add in LengthofStay values and convert to data frame
plotdat <- as.data.frame(cbind(plotdat, jvalues))

# better names and show the first few rows
colnames(plotdat) <- c("PredictedProbability", "Lower", "Upper", "IBScore")
head(plotdat)

# plot average marginal predicted probabilities
ggplot(plotdat, aes(x = IBScore, y = PredictedProbability)) + geom_line() +
  ylim(c(0, 1))


#-CODE FOR DATA ANALYSIS BEFORE MODELLING------------------------------------------------

require(ggplot2)
require(GGally)
require(reshape2)
require(lme4)
require(compiler)
require(parallel)
require(boot)
require(lattice)

# is answer missing? 
table(vt$Answer01, useNA = "always")
# 1390+11823 = 13213
# missing = 4175
# 4175/17388*100 = 24% of missing data 

# data viz 
# si les variables d'ajustement sont indépendantes, l'ajout de ces variables dans le modèle
# ne devrait pas changer l'estimation des autres 

ggpairs(vt[, c("Time", "IHScore", "IBScore", "Age")])

# elle ne semblent pas avoir une corrélation linéaire forte (?)
# en effet, le coef de corrélation est faible, mais les variables sont associées

# dans la prochaine étape, je regarde la distribution des variables quantitative 
# par "Answer" (utilitariste / non-utilitariste)

ggplot(vt, aes(x = Answer01, y = IHScore)) +
  stat_sum(aes(size = ..n.., group = 1)) +
  scale_size_area(max_size = 4)

table(vt$Answer01, vt$IHScore, useNA = "always")

#-MULTILEVEL-BOOTSTRAPPING---------------------------------------------------------------
# funtion from here: https://biostat.app.vumc.org/wiki/Main/HowToBootstrapCorrelatedData

sampler <- function(dat, clustervar, replace = TRUE, reps = 1) {
  cid <- unique(dat[, clustervar[1]])
  ncid <- length(cid)
  recid <- sample(cid, size = ncid * reps, replace = TRUE)
  if (replace) {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, RowID = sample(which(dat[, clustervar] == recid[i]),
                                      size = length(which(dat[, clustervar] == recid[i])), replace = TRUE))
    })
  } else {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, RowID = which(dat[, clustervar] == recid[i]))
    })
  }
  dat <- as.data.frame(do.call(rbind, rid))
  dat$Replicate <- factor(cut(dat$NewID, breaks = c(1, ncid * 1:reps), include.lowest = TRUE,
                              labels = FALSE))
  dat$NewID <- factor(dat$NewID)
  return(dat)
}

vts = subset(vt, select = c("ID", "Sexe", "AgeCl", "IHScore", "IBScore", "Answer01"))
vts$IDf = as.factor(vts$ID)

set.seed(20)
tmp <- resample(vts, IDf, replace = TRUE, reps = 100)
bigdata <- cbind(tmp, vts[tmp$RowID])
