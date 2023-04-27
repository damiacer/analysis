#install.packages("here")
library(here)
#here::here()
#here::here("P", "CONSULTATION", "Lhermitte_Amaury", "DATA")

#install.packages("usethis")
library(usethis)
usethis::use_blank_slate()


#packages
library("ggplot2")
library("caret")
library("pROC")
library("ggthemes")
library("plyr")
library("ROCR")
library("reshape2")
#install.packages("gridExtra")
library("gridExtra")
library("dplyr")
#install.packages("randomForest")
library("randomForest")
library("readxl")
library("MLeval")


setwd("P:/CONSULTATION/Lhermitte_Amaury/DATA") # On PC
la <- read_excel("MERP_20210707.xlsx", na = "NA")

names(la)

################################################################################
################################################################################
################################################################################

my_data <- la

la$Delta_P_F_sup20 # class variable
la$Vrec_VteaPEEP15 # numeric variable 

names(my_data)

my_data = subset(my_data, select = c(Delta_P_F_sup20, Vrec_VteaPEEP15, Age,
                                     Homme, IMC, covid01, SOFA, IGSII, charlsonscore,
                                     SpO2_sup96p100, FEVG_sup50, Pneumothorax, 
                                     hypotension_arterielle, 
                                     necessiteNO, necessiteAlmitrine, necessiteECMO, 
                                     vivant_j28
                                     ))

names(my_data)[1] <- "Class"

str(my_data$Class)
my_data$Class = as.factor(my_data$Class)

### RANDOM FOREST ROC CURVE

#levels(my_data$Class) <- c("first_class", "second_class")
my_data$Class = if_else(my_data$Class == "0", "noevent", "event")
table(my_data$Class)
str(my_data$Class)

#fit_rf<-randomForest(Class~.,
#        data=my_data,
#        importance=TRUE,
#        prOximity=TRUE,
#        na.action=na.roughfix)


set.seed(21051986)
ind <- createDataPartition(my_data$Class, p = 2/3, list = FALSE)
train <- my_data[ ind,]
test  <- my_data[-ind,]

#Train random forest (RF from now on)
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 3,
                     classProbs = TRUE,
                     savePredictions = TRUE,
                     summaryFunction = twoClassSummary)

grid <- data.frame(mtry = seq(1,3,1))

set.seed(21051986)
rf_mod <- train(Class ~ Vrec_VteaPEEP15, 
                data = train,
                method = "rf",
                metric = "ROC",
                na.action = na.omit,
                tuneGrid = grid,
                ntree = 1000,
                trControl = ctrl)

rfClasses <- predict(rf_mod, test)

#ROC curve from held-out samples
roc_train <- function(object, best_only = TRUE, ...) {
  
  
  lvs <- object$modelInfo$levels(object$finalModel)
  
  if(best_only) {
    object$pred <- merge(object$pred, object$bestTune)
  }
  
  ## find tuning parameter names
  p_names <- as.character(object$modelInfo$parameters$parameter)
  p_combos <- object$pred[, p_names, drop = FALSE]
  
  ## average probabilities across resamples
  object$pred <- plyr::ddply(.data = object$pred, 
                             .variables = c("obs", "rowIndex", p_names),
                             .fun = function(dat, lvls = lvs) {
                               out <- mean(dat[, lvls[1]])
                               names(out) <- lvls[1]
                               out
                             })
  
  make_roc <- function(x, lvls = lvs, nms = NULL, ...) {
    out <- pROC::roc(response = x$obs,
                     predictor = x[, lvls[1]],
                     levels = rev(lvls))
    
    out$model_param <- x[1,nms,drop = FALSE]
    out
  }
  out <- plyr::dlply(.data = object$pred, 
                     .variables = p_names,
                     .fun = make_roc,
                     lvls = lvs,
                     nms = p_names)
  if(length(out) == 1)  out <- out[[1]]
  out
}

temp <- roc_train(rf_mod)

plot_data_ROC <- data.frame(Model='Random Forest', sens = temp$sensitivities, spec=1-temp$specificities)
plot(plot_data_ROC)

auc.1 <- abs(sum(diff(1-temp$specificities)*(head(temp$sensitivities,-1)+tail(temp$sensitivities,-1)))/2)

# [1] 0.4277778

#-------------------------------------------------------------------------------

### SVM ROC CURVE

#Build SVM
library("kernlab")
set.seed(21051986)
svm_mod <- train(Class ~ ., 
                 data = train,
                 method = "svmRadial",
                 metric = "ROC",
                 na.action = na.omit,
                 trControl = ctrl)

svmClasses <- predict(svm_mod, test)

#ROC curve into df
temp <- roc_train(svm_mod)
plot_data_ROC <- rbind(plot_data_ROC, data.frame(Model='Support Vector Machine', sens = temp$sensitivities, spec=1-temp$specificities))
plot(plot_data_ROC)
#AUC of roc curve for SVM
auc.2 <- abs(sum(diff(1-temp$specificities) * (head(temp$sensitivities,-1)+tail(temp$sensitivities,-1)))/2)

# [1] 0.5888889

#-------------------------------------------------------------------------------

#Plotting final data 
#Check ggplot

####ROC of held-out samples
q <- ggplot(data=plot_data_ROC, aes(x=spec, y=sens, group = Model, colour =  Model)) 
q <- q + geom_path() + geom_abline(intercept = 0, slope = 1) + xlab("False Positive Rate (1-Specificity)") + ylab("True Positive Rate (Sensitivity)") 
q + theme(axis.line = element_line(), axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'),     legend.text=element_text(), legend.title=element_text())

####ROC of testing set
rf.probs <- predict(rf_mod, test,type="prob")
pr <- prediction(rf.probs$event, factor(test$Class, levels = c("noevent", "event"), ordered = TRUE))
pe <- performance(pr, "tpr", "fpr")
roc.data <- data.frame(Model='Random Forest',fpr=unlist(pe@x.values),     tpr=unlist(pe@y.values))

svm.probs <- predict(svm_mod, test,type="prob")
pr <- prediction(svm.probs$event, factor(test$Class, levels = c("noevent","event"), ordered = TRUE))
pe <- performance(pr, "tpr", "fpr")
roc.data <- rbind(roc.data, data.frame(Model='Support Vector Machine',fpr=unlist(pe@x.values), tpr=unlist(pe@y.values)))

q <- ggplot(data=roc.data, aes(x=fpr, y=tpr, group = Model, colour = Model)) 
q <- q + geom_line() + geom_abline(intercept = 0, slope = 1) + xlab("False     Positive Rate (1-Specificity)") + ylab("True Positive Rate (Sensitivity)") 
q + theme(axis.line = element_line(), axis.text=element_text(color='black'), 
          axis.title = element_text(colour = 'black'), legend.text=element_text(), legend.title=element_text())

####AUC of hold out samples
data.frame(Rf = auc.1, Svm = auc.2)

###AUC of testing set. Source is  from Max Kuhns 2016 UseR! code here: https://github.com/topepo/useR2016
test_pred <- data.frame(Class = factor(test$Class, levels = c("noevent", "event"), ordered = TRUE))
test_pred$Rf <- predict(rf_mod, test, type = "prob")[, "event"]
test_pred$Svm <- predict(svm_mod, test, type = "prob")[, "event"]

get_auc <- function(pred, ref){
  auc(roc(ref, pred, levels = rev(levels(ref))))
}

#AUC sim comp
apply(test_pred[, -1], 2, get_auc, ref = test_pred$Class) 

#       Rf       Svm 
#0.6944444 0.5000000 

################################################################################
################################################################################
################################################################################

library("caret")
library("MLeval")

#data(Sonar)

my_data = la

my_data = subset(my_data, select = c(Delta_P_F_sup20, Vrec_VteaPEEP15, Age,
                                     Homme, IMC, covid01, SOFA, IGSII, charlsonscore,
                                     SpO2_sup96p100, FEVG_sup50, Pneumothorax, 
                                     hypotension_arterielle, 
                                     necessiteNO, necessiteAlmitrine, necessiteECMO, 
                                     vivant_j28
))

summary(my_data)
names(my_data)[1] <- "Class"

str(my_data$Class)
my_data$Class = as.factor(my_data$Class)

levels(my_data$Class) <- c("first_class", "second_class")
table(my_data$Class)

# Split data 
# https://stackoverflow.com/questions/70075400/roc-curve-of-the-testing-dataset

a <- createDataPartition(my_data$Class, p=0.8, list=FALSE)
train <- my_data[ a, ]
test <- my_data[ -a, ]

myControl = trainControl(
  method = "cv",
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = FALSE,
)

model_knn = train(
  Class ~ .,
  train,
  method = "knn",
  metric = "ROC",
  tuneLength = 10,
  trControl = trainControl(
    classProbs = TRUE,
    method = "cv",
    number = 10,
    summaryFunction = twoClassSummary))

pred <- predict(model_knn, newdata=test, type="prob")
ROC <- evalm(data.frame(pred, test$Class, Group = "KNN"))
plot(ROC)

# ***MLeval: Machine Learning Model Evaluation***
#  Input: data frame of probabilities of observed labels
#Group column exists.
#Observations: 7
#Number of groups: 1
#Observations per group: 7
#Positive: second_class
#Negative: first_class
#Group: KNN
#Positive: 2
#Negative: 5
#***Performance Metrics***
#  KNN Optimal Informedness = 0.4
#KNN AUC-ROC = 0.7

################################################################################
################################################################################
################################################################################

library(caret)
library(MLeval)

my_data = la

my_data = subset(my_data, select = c(Delta_P_F_sup20, Vrec_VteaPEEP15, Age,
                                     Homme, IMC, covid01, SOFA, IGSII, charlsonscore,
                                     SpO2_sup96p100, FEVG_sup50, Pneumothorax, 
                                     hypotension_arterielle, 
                                     necessiteNO, necessiteAlmitrine, necessiteECMO, 
                                     vivant_j28
))

summary(my_data)
names(my_data)[1] <- "Class"

levels(my_data$Class) <- c("noev", "ev")
table(my_data$Class)
my_data$Class = as.factor(my_data$Class)

Train <- createDataPartition(my_data$Class, p=0.6, list=FALSE)
training <- my_data[ Train, ]
testing <- my_data[ -Train, ]


ctrl <- trainControl(method = "repeatedcv", number = 10, classProbs = TRUE, savePredictions = TRUE)

mod_fit <- train(Class ~ .,  data=training, method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5, metric = "ROC")

pred <- predict(mod_fit, newdata=testing, type="prob")

confusionMatrix(data=pred, testing$Class)

test = evalm(mod_fit) # this gives the ROC curve for test set
m1 = data.frame(pred, testing$Class)

test1 <- evalm(m1)

#Train and eval a second model: 
mod_fit2 <- train(Class ~ Age + ForeignWorker + Property.RealEstate + Housing.Own,  
                  data=training, method="glm", family="binomial",
                  trControl = ctrl, tuneLength = 5, metric = "ROC")


pred2 <- predict(mod_fit2, newdata=testing, type="prob")
m2 = data.frame(pred2, testing$Class)

test2 <- evalm(m2)


# Display ROCs for both models in one graph: 

compare <- evalm(list(m1, m1), gnames=c('logistic1','logistic2')) 

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# some references 
# https://www.machinelearningplus.com/machine-learning/caret-package/
