getwd()
setwd("/Users/damianocerasuolo/Desktop/UBRC/Consultations UBRC _ December Backup/Legrand_Ignace")
getwd()

li <- read.csv2("database.csv", header = TRUE, na.string=".")
summary(li)
names(li)


library(ggplot2)
library(caret)
library(pROC)
library(ggthemes)
library(plyr)
library(ROCR)
library(reshape2)
#install.packages("gridExtra")
library(gridExtra)
#library(dplyr)
#install.packages("randomForest")
#library("randomForest")


my_data <- li
names(my_data)[13] <- "Class"

names(my_data)
is.na(my_data$Class)
is.na(my_data)

my_data = subset(my_data, select = -c(coronarographie))
my_data = subset(my_data, select=-c(lesion_significative))
my_data = subset(my_data, select=-c(diagnostic_autre))
my_data = subset(my_data, select=-c(Tropo_H0, Tropo_H3))


names(my_data)

#fit_rf<-randomForest(Class~.,
#        data=my_data,
#        importance=TRUE,
#        prOximity=TRUE,
#        na.action=na.roughfix)

#na.exclude(my_data)

str(my_data)
my_data$age <- as.numeric(as.factor(my_data$age))

my_data$Class <- ifelse(my_data$Class == 1, "event", "noevent")

my_data$Class <- factor(my_data$Class, levels = c("noevent", "event"), ordered = TRUE)
str(my_data$Class)
table(my_data$Class)
#count(my_data)

set.seed(1732)
ind <- createDataPartition(my_data$Class, p = 2/3, list = FALSE)
train <- my_data[ ind,]
test  <- my_data[-ind,]

###

#Train RF
ctrl <- trainControl(method = "repeatedcv",
                 number = 5,
                 repeats = 3,
                 classProbs = TRUE,
                 savePredictions = TRUE,
                 summaryFunction = twoClassSummary)

grid <- data.frame(mtry = seq(1,3,1))

set.seed(1537)
rf_mod <- train(Class ~ ., 
                data = train,
                method = "rf",
                metric = "ROC",
                na.action = na.omit,
                tuneGrid = grid,
                ntree = 1000,
                trControl = ctrl)


rfClasses <- predict(rf_mod, test)

#This is the ROC curve from held out samples. Source is  from Max Kuhns 2016 UseR! code here: https://github.com/topepo/useR2016
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

#This is the AUC of the held-out samples roc curve for RF
auc.1 <- abs(sum(diff(1-temp$specificities)*(head(temp$sensitivities,-1)+tail(temp$sensitivities,-1)))/2)

#Build SVM
set.seed(1537)
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

#This is the AUC of the held-out samples roc curve for SVM
auc.2 <- abs(sum(diff(1-temp$specificities) *   (head(temp$sensitivities,-1)+tail(temp$sensitivities,-1)))/2)

###

#Plotting Final

#ROC of held-out samples
q <- ggplot(data=plot_data_ROC, aes(x=spec, y=sens, group = Model, colour =     Model)) 
q <- q + geom_path() + geom_abline(intercept = 0, slope = 1) + xlab("False     Positive Rate (1-Specificity)") + ylab("True Positive Rate (Sensitivity)") 
q + theme(axis.line = element_line(), axis.text=element_text(color='black'), 
      axis.title = element_text(colour = 'black'),     legend.text=element_text(), legend.title=element_text())

#ROC of testing set
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
      axis.title = element_text(colour = 'black'),     legend.text=element_text(), legend.title=element_text())


#AUC of hold out samples
data.frame(Rf = auc.1, Svm = auc.2)

#AUC of testing set. Source is  from Max Kuhns 2016 UseR! code here: https://github.com/topepo/useR2016
test_pred <- data.frame(Class = factor(test$Class, levels = c("noevent",     "event"), ordered = TRUE))
test_pred$Rf <- predict(rf_mod, test, type = "prob")[, "event"]
test_pred$Svm <- predict(svm_mod, test, type = "prob")[, "event"]

get_auc <- function(pred, ref){
  auc(roc(ref, pred, levels = rev(levels(ref))))
}

apply(test_pred[, -1], 2, get_auc, ref = test_pred$Class)

###

install.packages("cutpointr")
library("cutpointr")
cutpointr(roc.data, method = oc_youden_kernel)



# Calculate the Youden's J statistic
youdenJ=y-x

youden_d = (x+y)
maxCol(youden_d)
youden_index = argmax(youden_d, margin=1)

# Find the optimal threshold
install.packages("ramify")
library("ramify")
youdenJ_pos=youdenJ*(-1)
index = argmax(youdenJ)

thresholdOpt = round(thresholds[index], ndigits=4)

youdenJOpt = round(gmean[index], ndigits = 4)

fprOpt = round(fpr[index], ndigits = 4)

tprOpt = round(tpr[index], ndigits = 4)

print('Best Threshold: {} with Youden J statistic: {}'.format(thresholdOpt, youdenJOpt))

print('FPR: {}, TPR: {}'.format(fprOpt, tprOpt))



# Create a data viz

plotnine.options.figure_size = (8, 4.8)

(

ggplot(data = df_fpr_tpr)+

geom_point(aes(x = 'FPR',

y = 'TPR'),

size = 0.4)+

# Best threshold

geom_point(aes(x = fprOpt,

y = tprOpt),

color = '#981220',

size = 4)+

geom_line(aes(x = 'FPR',

y = 'TPR'))+

# Annotate the text

geom_text(aes(x = fprOpt,

y = tprOpt),

label = 'Optimal threshold for \n negative class {}'.format(thredholdOpt),

nudge_x = 0.14,

nudge_y = -0.10,

size = 10,

fontstyle = 'italic')+

labs(title = 'ROC Curve')+

xlab('False Positive Rate (FPR)')+

ylab('True Positive Rate (TPR)')+

theme_minimal()

)


####################

# load the data
df2 <- my_data
df2 = subset(df2, select=-c(Tropo_H0, Tropo_H3))
df2 = subset(df2, select=-c(IEP))
names(df2)
names(df2)
# change the quality column to a 1 (if quality > 5) or a 0
df2$Class <- ifelse(df2$Class=="event",1,0)
# change the quality column to a factor type
df2$Class <- as.factor(df2$Class)
# split the dataframe into train and test sets
index <- sample(1:nrow(df2),size = 0.8*nrow(df2))
train <- df2[index,]
test <- df2[-index,]
# build the random forest model and test it
library(randomForest)
rf_model <- randomForest(Class ~., data = train, na.action=na.omit)
rf_prediction <- predict(rf_model, test, type = "prob")
# build the logistic regression model and test it
lr_model <- glm(Class ~., data = train, family = "binomial")
lr_prediction <- predict(lr_model, test, type = "response")
# ROC curves
library(pROC)
ROC_rf <- roc(test$Class, rf_prediction[,2])
ROC_lr <- roc(test$Class, lr_prediction)
# Area Under Curve (AUC) for each ROC curve (higher -> better)
ROC_rf_auc <- auc(ROC_rf)
ROC_lr_auc <- auc(ROC_lr)

#see also
# http://appliedpredictivemodeling.com/blog/2014/2/1/lw6har9oewknvus176q4o41alqw2ow
# and https://github.com/topepo/useR2016



# code 2

getwd()
setwd("/Users/damianocerasuolo/Desktop/UBRC/Consultations UBRC _ December Backup/Legrand_Ignace")
getwd()

li <- read.csv2("database.csv", header = TRUE, na.string=".")
summary(li)
names(li)


library(ggplot2)
library(caret)
library(pROC)
library(ggthemes)
library(plyr)
library(ROCR)
library(reshape2)
#install.packages("gridExtra")
library(gridExtra)
library(dplyr)
#install.packages("randomForest")
library(randomForest)


my_data <- li
names(my_data)[13] <- "Class"

names(my_data)
is.na(my_data$Class)
is.na(my_data)

my_data = subset(my_data, select = -c(coronarographie))
my_data = subset(my_data, select=-c(lesion_significative))
my_data = subset(my_data, select=-c(diagnostic_autre))
my_data = subset(my_data, select=-c(Tropo_H0, Tropo_H3))


names(my_data)

#fit_rf<-randomForest(Class~.,
#        data=my_data,
#        importance=TRUE,
#        prOximity=TRUE,
#        na.action=na.roughfix)

#na.exclude(my_data)

str(my_data)
my_data$age <- as.numeric(as.factor(my_data$age))

my_data$Class <- ifelse(my_data$Class == 1, "event", "noevent")

my_data$Class <- factor(my_data$Class, levels = c("noevent", "event"), ordered = TRUE)
str(my_data$Class)
table(my_data$Class)
#count(my_data)

set.seed(1732)
ind <- createDataPartition(my_data$Class, p = 2/3, list = FALSE)
train <- my_data[ ind,]
test  <- my_data[-ind,]

###

#Train RF
ctrl <- trainControl(method = "repeatedcv",
                 number = 5,
                 repeats = 3,
                 classProbs = TRUE,
                 savePredictions = TRUE,
                 summaryFunction = twoClassSummary)

grid <- data.frame(mtry = seq(1,3,1))

set.seed(1537)
rf_mod <- train(Class ~ ., 
                data = train,
                method = "rf",
                metric = "ROC",
                na.action = na.omit,
                tuneGrid = grid,
                ntree = 1000,
                trControl = ctrl)


rfClasses <- predict(rf_mod, test)

#This is the ROC curve from held out samples. Source is  from Max Kuhns 2016 UseR! code here: https://github.com/topepo/useR2016
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

#This is the AUC of the held-out samples roc curve for RF
auc.1 <- abs(sum(diff(1-temp$specificities)*(head(temp$sensitivities,-1)+tail(temp$sensitivities,-1)))/2)

#Build SVM
set.seed(1537)
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

#This is the AUC of the held-out samples roc curve for SVM
auc.2 <- abs(sum(diff(1-temp$specificities) *   (head(temp$sensitivities,-1)+tail(temp$sensitivities,-1)))/2)

###

#Plotting Final

#ROC of held-out samples
q <- ggplot(data=plot_data_ROC, aes(x=spec, y=sens, group = Model, colour =     Model)) 
q <- q + geom_path() + geom_abline(intercept = 0, slope = 1) + xlab("False     Positive Rate (1-Specificity)") + ylab("True Positive Rate (Sensitivity)") 
q + theme(axis.line = element_line(), axis.text=element_text(color='black'), 
      axis.title = element_text(colour = 'black'),     legend.text=element_text(), legend.title=element_text())

#ROC of testing set
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
      axis.title = element_text(colour = 'black'),     legend.text=element_text(), legend.title=element_text())


#AUC of hold out samples
data.frame(Rf = auc.1, Svm = auc.2)

#AUC of testing set. Source is  from Max Kuhns 2016 UseR! code here: https://github.com/topepo/useR2016
test_pred <- data.frame(Class = factor(test$Class, levels = c("noevent", "event"), ordered = TRUE))
test_pred$Rf <- predict(rf_mod, test, type = "prob")[, "event"]
test_pred$Svm <- predict(svm_mod, test, type = "prob")[, "event"]

get_auc <- function(pred, ref){
  auc(roc(ref, pred, levels = rev(levels(ref))))
}

apply(test_pred[, -1], 2, get_auc, ref = test_pred$Class)

###################################################

logg <- glm(Class~DELTA_H3H0,data=train,family="binomial")
coef(logg)
prediction <- predict(logg,test,type="response")
predicted <- round(prediction)

install.packages("pROC")
library("pROC")
youd_data<-roc(predicted, test$DELTA_H3H0)
coords(youd_data,"best",ret="threshold",transpose=FALSE,best.method="youden")

###################################################

fourstats <- function(my_data, lev=levels(my_data$Class), model=NULL){
	out <- c(twoClassSummary(data, lev))
	coords <- matriw(c(1,1, out["Spec"], out["Sens"]), ncol=2, byrow=TRUE)
	colnames(coords) <- c("Spec", "Sens")
	rownames(coords) <- c("Best", "Current")
	c(out, Dist=dist(coords)[1])
}

set.seed(1537)
rf_mod <- train(Class ~ ., 
                data = train,
                method = modelInfo,
                metric = "Dist",
                maximize = FALSE,
                tuneLength = 20,
                na.action = na.omit,
                trControl = trainControl(method = "cv",
                						classProbs = TRUE,
                						summaryFunction=fourstats))
                						
###################################################

library("cutpointr")
set.seed(1537)
opt_cut <- cutpointr(test$DELTA_H3H0, li$infarctus_myocarde, test, boot_runs=1000)

str(li$infarctus_myocarde)
li$infarctus_myocarde <- as.numeric(as.character(li$infarctus_myocarde))
my_data$class2[train$Class=="noevent"] <- "0"
my_data$class2[train$Class=="event"] <- "1"
str(class2)
