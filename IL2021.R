getwd()
setwd("FOLDER")
getwd()

li <- read.csv2("database.csv", header = TRUE, na.string=".")
summary(li)
names(li)

#packages
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

#data and "Class" as event variable
my_data <- li
names(my_data)[13] <- "Class"

names(my_data)
is.na(my_data$Class)
is.na(my_data)

#variable drop
my_data = subset(my_data, select = -c(coronarographie))
my_data = subset(my_data, select=-c(lesion_significative))
my_data = subset(my_data, select=-c(diagnostic_autre))
my_data = subset(my_data, select=-c(Tropo_H0, Tropo_H3))

#new data
names(my_data)

#fit_rf<-randomForest(Class~.,
#        data=my_data,
#        importance=TRUE,
#        prOximity=TRUE,
#        na.action=na.roughfix)

#na.exclude(my_data) #this function is integrated in the random forest sim

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

#Train random forest (RF from now on)
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

#AUC of the ROC curve for RF
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

#AUC of roc curve for SVM
auc.2 <- abs(sum(diff(1-temp$specificities) *   (head(temp$sensitivities,-1)+tail(temp$sensitivities,-1)))/2)

###

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

###################################################

#the following method is not supported anymore by caret - and is not available in RF
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
