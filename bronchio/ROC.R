require("pROC")

logit = glm(OHb ~ ROXI, family = binomial, data = re2s)
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
#1 0.03023995   0.9003436         0.5

re2s$ROXIb = if_else(re2s$ROXI <= 0.03023995, "0", "1")
table(re2s$ROXIb, useNA = "always")

#---

install.packages("Epi")
require("Epi")

a1 = ROC(form=OHb ~ ROXI, data = re2s, plot="ROC")
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
#  0.02974967

# optimal point in distribution
optimal_cutpoint=function(x){
  y=optimal_lr.eta(x)
  b0=unname(x$lr$coeff[1])
  b1=unname(x$lr$coeff[2])
  result=(-log(1/y-1)-b0)/b1
  result
} 
optimal_cutpoint(a1)
# 13.1

re2s$ROXIb2 = if_else(re2s$ROXI <= 13.1, "0", "1")
table(re2s$ROXIb2, useNA = "always")

#---

install.packages("ROCit")
require("ROCit")

roc4 <- rocit(score = re2s$ROXI, class = re2s$OHb, negref = 0) 
da<-cbind(Cutoff=roc4$Cutoff, TPR=roc4$TPR, FPR=roc4$FPR)
head(da)

#    Cutoff       TPR         FPR
#[1,]    Inf 0.0000000 0.000000000
#[2,]  29.10 0.0000000 0.003436426
#[3,]  25.40 0.0000000 0.006872852
#[4,]  23.54 0.1666667 0.010309278
#[5,]  22.32 0.1666667 0.013745704
#[6,]  21.96 0.1666667 0.017182131

# youden

roc4 <- rocit(score = re2s$ROXI, class = re2s$OHb, negref = 0) 
plot(roc4)
