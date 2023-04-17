names(la)

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

summary(my_data)
names(my_data)[1] <- "Class"

#fit_rf<-randomForest(Class~.,
#        data=my_data,
#        importance=TRUE,
#        prOximity=TRUE,
#        na.action=na.roughfix)

my_data$Class = as.factor(my_data$Class)

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

table(my_data$Class)

my_data <- my_data %>% 
  mutate(Class = case_when(
    Class == "0" ~ "no",
    Class == "1" ~ "yes"
  ))

table(my_data$Class)

set.seed(1537)
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
