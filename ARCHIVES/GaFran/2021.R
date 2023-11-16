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

fg$sexe = as.character(fg$sexe)
str(fg$sexe)

#------------------------------------------------------------------------------#

variables = c("MT_2016", "sex", "age", "hta_2016", "diab_2016", 
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

categorical = c("MT_2016", "sexe", "hta_2016", "diab_2016", 
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

#------------------------------------------------------------------------------#

# CONTROLE SELON LE SEXE (MANQUANT DANS LA BASE ORIGINALE)

tabsexe2016 = table(fg$sexe, fg$hypercholestcontrolee_2016)
prop.table(tabsexe2016, margin = 2)
0.4818049*100
0.6414763*100
0.5181951*100
0.3585237*100

chisq.test(tabsexe2016, correct = FALSE)

tabsexe2019 = table(fg$sexe, fg$hypercholestcontrolee_2019)
prop.table(tabsexe2019, margin = 2)
0.8296943*100
0.8769772*100
0.1703057*100
0.1230228*100
chisq.test(tabsexe2019, correct = FALSE)

#hypercontrole

tabsexecontr = table(fg$sexe, fg$hypercontrole)
prop.table(tabsexecontr, margin = 2)
0.8296943*100
0.8769772*100
0.1703057*100
0.1230228*100
chisq.test(tabsexecontr, correct = FALSE)

# 0.6407507 0.6256684 0.4755747
# 0.3592493 0.3743316 0.5244253

#fg2$hypercontroleNA
tabsexeNA = table(fg2$sexe, fg2$hypercontrole)
prop.table(tabsexeNA, margin = 2)

chisq.test(tabsexeNA, correct = FALSE)

################################################################################

fg3 <- fg[c("sexe", "age", "hta_2016", "diab_2016", 
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
           "LDLcible_2019", "hypercholestcontrolee_2019", "hypercontrole", 
           "hypercontroleNA")]

names(fg3)
count(fg3)

table(fg3$hypercholestcontrolee_2016)
fg3$hypercholestcontrolee_2016 <- as.factor(fg3$hypercholestcontrolee_2016)

fg3$hyper01[fg3$hypercholestcontrolee_2016=="0"] <- "1"
fg3$hyper01[fg3$hypercholestcontrolee_2016=="1"] <- "0"
fg3$hyper01 <- as.factor(fg3$hyper01)

model1 <- glm(hyper01 ~ age + hta_2016 +  SBP1AN_2016 + 
                DBP1AN_2016 + tttdiab_2016 + veryhighrisk_2016 +
                highrisk_2016 + moderaterisk_2016 + lowrisk_2016, 
                data=fg3, family=binomial)

model1

summary(model1)
exp(cbind(coef(model1), confint(model1)))

names(fg3)

#------------------------------------------------------------------------------#

model_lin <- lm(LDL_2016 ~ sexe + age + SCORE_2016 + cancer_2016 + HTAdefcompl_2016 
             + diabdefcompl_2016 + cardiopathisch_2016 + aomi_2016 + avc_2016 +
             + tttdyslipid_2016 + nbttt_2016 + veryhighrisk_2016 + highrisk_2016
             + moderaterisk_2016 + lowrisk_2016 
             #+  veryhighrisk_2019 + highrisk_2019
             #+ moderaterisk_2019 + lowrisk_2019 
             + tabac_2016 + prevsecondaire_2016
             + IMC_2016, data=fg3)

model_lin
summary(model_lin)
exp(cbind(coef(model2), confint(model2)))


model_lin
summary(model_lin)
exp(cbind(coef(model_lin), confint(model_lin)))

ci <- confint(model_lin)
apply(ci, 1, function(x) paste0("95% CI: [",round(x[1], 2), ",", round(x[2], 2), "]"))

# SPLITTING THE MODEL

set.seed(123)
N.train <- ceiling(1 * nrow(fg3$LDL_2016))
N.test <- nrow(fg3$LDL_2016) - N.train
trainset <- sample(seq_len(nrow(fg3$LDL_2016)), N.train)
testset <- setdiff(seq_len(nrow(fg3$LDL_2016)), trainset)

# RESIDUALS 

ldlres = resid(model_lin)

plot(fg3$LDL_2016, ldlres, ylab="Residuals", xlab="LDL", main="LDL 2016") 
abline(0, 0)             
# THERE IS A STRONG CORRELATION BETWEEN THE MODEL PREDICTION AND THE ACTUAL RESULTS


# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(fg3), 0.8*nrow(fg3))  # row indices for training data
trainingData <- fg3[trainingRowIndex, ]  # model training data
testData  <- fg3[-trainingRowIndex, ]   # test data

# Build the model on training data
lmMod <- lm(LDL_2016 ~ sexe + age + hta_2016 +  SBP1AN_2016 + 
              DBP1AN_2016 + tttdiab_2016 + veryhighrisk_2016 +
              highrisk_2016 + moderaterisk_2016 + lowrisk_2016, 
            data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
summary(lmMod) # R-squared and adjusted R-squared the higher the better
AIC (lmMod) # the lower the better
AIC(model_lin)

#------------------------------------------------------------------------------#

# Confidence Interval around a Linear Regression Line

reg.conf.intervals <- function(x, y) {
  n <- length(y) # Find length of y to use as sample size
  lm.model <- lm(y ~ x) # Fit linear model
  
  # Extract fitted coefficients from model object
  b0 <- lm.model$coefficients[1]
  b1 <- lm.model$coefficients[2]
  
  # Find SSE and MSE
  sse <- sum((y - lm.model$fitted.values)^2)
  mse <- sse / (n - 2)
  
  t.val <- qt(0.975, n - 2) # Calculate critical t-value
  
  # Fit linear model with extracted coefficients
  x_new <- 1:max(x)
  y.fit <- b1 * x_new + b0
  
  # Find the standard error of the regression line
  se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
  
  # Fit a new linear model that extends past the given data points (for plotting)
  x_new2 <- 1:max(x + 100)
  y.fit2 <- b1 * x_new2 + b0
  
  # Warnings of mismatched lengths are suppressed
  slope.upper <- suppressWarnings(y.fit2 + t.val * se)
  slope.lower <- suppressWarnings(y.fit2 - t.val * se)
  
  # Collect the computed confidence bands into a data.frame and name the colums
  bands <- data.frame(cbind(slope.lower, slope.upper))
  colnames(bands) <- c('Lower Confidence Band', 'Upper Confidence Band')
  
  # Plot the fitted linear regression line and the computed confidence bands
  plot(x, y, cex = 1.75, pch = 21, bg = 'gray')
  lines(y.fit2, col = 'black', lwd = 2)
  lines(bands[1], col = 'blue', lty = 2, lwd = 2)
  lines(bands[2], col = 'blue', lty = 2, lwd = 2)
  
  return(bands)
}

conf.intervals <- reg.conf.intervals(fg3$LDL_2016, fg3$SBP1AN_2016)
