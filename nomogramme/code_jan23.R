getwd()
setwd("")

library("readxl")

no <- read_excel("nomogramme.xlsx", na = "NA")
warnings()
names(no)
View(no)

table(no$Nbre_ovo_matures, useNA = "always")
str(no$Nbre_ovo_matures)

table(no$Numero_STIM)

#-DESCRIPTIVE ANALYSIS----------------------------------------------------------

library("tableone")

dput(names(no))

vars = c("Nombre_de_stim", 
         "Numero_de_stim", "Indication", "IMC", "Tabac", "CFA", "AMH", 
         "Age_1ere_stim", "Age_debut_de_stim", "Protocole", 
         "protocole_simple", "Dose_de_depart", "Dose_totale", "Duree", 
         "nbre_ovo_recueillis", "Nbre_ovo_matures")

categoricalvars = c("Numero_de_stim", "Indication", "Tabac","Protocole", 
         "protocole_simple")

no.des = CreateTableOne(vars = vars, factorVars = categoricalvars, data = no)
print(no.des, showAllLevels = TRUE, quote = TRUE, noSpaces = TRUE, includeNA = TRUE)

#-VARIABLE SELECTION------------------------------------------------------------

nos = subset(no, select = c("Nombre_de_stim", "IMC", "Tabac", "CFA", "AMH", "Age_1ere_stim",
             "Dose_de_depart", "Dose_totale", "Duree", "nbre_ovo_recueillis", "Nbre_ovo_matures"))

install.packages("leaps")
library("leaps")

regfit.full = regsubsets(Nbre_ovo_matures ~ ., nos)
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
coef(regfit.full, 5)

# backward or foward selection
regfit.bkw = regsubsets(Nbre_ovo_matures ~ ., data = nos , nvmax = 5 , method ="backward")
summary(regfit.bkw)

################################################################################
# MODEL SELECTION BY LEARNING-VALIDATION

set.seed(1)
train = sample (c(TRUE,FALSE), nrow(nos) , rep = TRUE)
test=(!train)

regfit.best = regsubsets(Nbre_ovo_matures ~ ., data = nos[train,], nvmax = 5)

# design matrix of the model
test.mat = model.matrix(Nbre_ovo_matures ~ ., data = nos[test,])

# prediction error on the validation sample for every var number in the model 
val.errors = rep(NA, 5)
for(i in 1:5){
  coefi = coef(regfit.best, id = i)
  pred = test.mat[, names(coefi)]%*%coefi
                  val.errors[i] = mean((nos$Nbre_ovo_matures[test] - pred)^2)
}
val.errors
# prediction error:
# [1] 4.787384 4.732835 4.721457 4.720452 4.741416
which.min(val.errors)
coef(regfit.full, 5)

predict.regsubsets = function(object , newdata , id , ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix( form , newdata)
  coefi = coef(object , id=id)
  xvars = names(coefi)
  mat[ ,xvars]%*% coefi
}

# crossed validation

k=5
set.seed(1)
folds = sample(1:k, nrow(nos) , replace = TRUE)
cv.errors = matrix(NA, k, 5, dimnames = list(NULL , paste(1:5)))

for(j in 1:k){
  best.fit = regsubsets(Nbre_ovo_matures ~ ., data=nos[folds!=j,] , nvmax=5)
  for (i in 1:5){
    pred = predict(best.fit , nos[folds==j,] , id=i)
    cv.errors[j,i]= mean((nos$Nbre_ovo_matures[folds==j]-pred) ^2)
  }
}

# error mean on 5 folds

mean.cv.errors = apply (cv.errors ,2 ,mean)
# 5 lignes et 5 colonnes
################################################################################
