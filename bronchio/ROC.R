require("pROC")

reaped$VNI = as.factor(if_else(reaped$VNI == "Oui", "1", "0"))

reaped <- reaped %>%
  mutate(ROXI2 = case_when(
    ROXI > 3 ~ ROXI
  ))
str(reaped$ROXI2)
  
logit = glm(VNI ~ ROXI, family = binomial, data = reaped)
summary(logit)

predicted_prob<-predict(logit, type="response")
roccurve <- roc(logit$y, predicted_prob)
plot(roccurve)

auc(roccurve)
#Area under the curve: 0.6271

par(pty = "s") 
plot(roccurve, print.thres = "best", print.thres.best.method = "youden")
par(pty = "m") #reset parameter

coords(roccurve, "best")
#  threshold specificity sensitivity
#1 0.2046285   0.6680498       0.625

reaped$ROXIb = if_else(reaped$ROXI <= 0.03023995, "0", "1")
table(reaped$ROXIb, useNA = "always")

#---

# optimal cutpoint with cutpointr 

require("cutpointr")
str(reaped$ROXI)
str(reaped$VNI)

cp <- cutpointr(reaped, ROXI, VNI, 
                method = maximize_metric, metric = sum_sens_spec, na.rm = TRUE)

summary(cp)

#    AUC   n n_pos n_neg
# 0.6467 297   241    56

# optimal_cutpoint sum_sens_spec    acc sensitivity specificity  tp fn fp tn
#              7.6         1.293 0.6599       0.668       0.625 161 80 21 35

#Predictor summary: 
#    Data Min.    5% 1st Qu. Median     Mean 3rd Qu.   95% Max.       SD NAs
# Overall 0.00 5.160    6.67   8.33 9.084714   10.42 15.87 29.1 3.677912   0
#       0 0.00 5.490    7.06   8.55 9.272697   10.60 15.48 29.1 3.486107   0
#       1 2.14 4.685    5.66   6.89 8.275714    9.24 16.30 25.4 4.352906   0

plot(cp)

opt_cut <- cutpointr(reaped, ROXI, VNI, direction = ">=", 
                     pos_class = "1", neg_class = "0", 
                     method = maximize_metric, metric = youden, na.rm = T)
plot_metric(opt_cut)

#  direction  optimal_cutpoint  method             youden      acc        sensitivity
#  1 >=                  17.59  maximize_metric    0.0286752   0.801347   0.0535714
#  specificity      AUC       pos_class neg_class   prevalence outcome predictor
#1 0.975104         0.353290  1         0           0.188552   VNI     ROXI     


#---

# optimal cutpoint : https://rpubs.com/LIMKYUSON/681052

# for VNI

install.packages("Epi")
require("Epi")

a1 = ROC(form=VNI ~ ROXI, data = reaped, plot="ROC")
plot(a1)

a1$AUC
# 0.6271478

head(a1$res)
#                    sens        spec pvp       pvn      lr.eta
#                       1 0.000000000 NaN 0.9797980        -Inf
#0.00454402602148837    1 0.003436426   0 0.9797297 0.004544026
#0.0061922456063823     1 0.006872852   0 0.9796610 0.006192246
#0.00695074512547002    1 0.010309278   0 0.9795918 0.006950745
#0.00768968436242759    1 0.013745704   0 0.9795222 0.007689684
#0.00797206404528058    1 0.017182131   0 0.9794521 0.007972064

# optimal threshold, slope
optimal_lr.eta=function(x){
  no=which.max(x$res$sens+x$res$spec)[1]
  result=x$res$lr.eta[no]
  result
}
optimal_lr.eta(a1) 
# 0.2045557

# optimal point in distribution
optimal_cutpoint=function(x){
  y=optimal_lr.eta(x)
  b0=unname(x$lr$coeff[1])
  b1=unname(x$lr$coeff[2])
  result=(-log(1/y-1)-b0)/b1
  result
} 
optimal_cutpoint(a1)
# 7.6

reaped$ROXIb2 = if_else(reaped$ROXI <= 7.6, "0", "1")
table(reaped$ROXIb2, useNA = "always")
#   0    1 <NA> 
# 116  181   86 

# for TRe

reaped$TRe01 = as.factor(if_else(reaped$TRe == "2", "1", "0"))

a2 = ROC(form=TRe01 ~ ROXI, data = reaped, plot="ROC")
plot(a2)

a2$AUC
# 0.6597371

head(a2$res)
#                       sens        spec pvp       pvn     lr.eta
#                   1.000000 0.000000000 NaN 0.7845118       -Inf
#0.065620219637784  1.000000 0.004291845 0.0 0.7837838 0.06562022
#0.0826400860164332 0.984375 0.004291845 0.5 0.7864407 0.08264009
#0.092638591702785  0.968750 0.008583691 0.5 0.7883959 0.09263859
#0.0997743538942264 0.953125 0.008583691 0.6 0.7910959 0.09977435
#0.101971556666741  0.953125 0.012875536 0.5 0.7903780 0.10197156

# optimal threshold, slope
optimal_lr.eta=function(x){
  no=which.max(x$res$sens+x$res$spec)[1]
  result=x$res$lr.eta[no]
  result
}
optimal_lr.eta(a2) 
# 0.2387867

# optimal point in distribution
optimal_cutpoint=function(x){
  y=optimal_lr.eta(x)
  b0=unname(x$lr$coeff[1])
  b1=unname(x$lr$coeff[2])
  result=(-log(1/y-1)-b0)/b1
  result
} 
optimal_cutpoint(a2)
# 6.86

reaped$ROXIb2 = if_else(reaped$ROXI <= 7.6, "0", "1")
table(reaped$ROXIb2, useNA = "always")
#   0    1 <NA> 
# 116  181   86 



#---

install.packages("ROCit")
require("ROCit")

roc4 <- rocit(score = reaped$ROXI, class = reaped$VNI, negref = 0) 
da<-cbind(Cutoff=roc4$Cutoff, TPR=roc4$TPR, FPR=roc4$FPR)
head(da)

# youden

roc4 <- rocit(score = reaped$ROXI, class = reaped$VNI, negref = 0) 
plot(roc4)
