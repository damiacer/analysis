### folder
getwd()
setwd("P:/CONSULTATION/FMOstéo")
getwd()

### database
fm <- read.csv2("db_missing.csv", header = TRUE, na.string="")
names(fm)
#View(fm)

#####################################################################

# install.packages("psych")
library("psych")

# install.packages("dplyr")
library("dplyr")

#####################################################################

# NEW DATASET FOR IMPUTATION

qdsa_data <- fm[,c(
# baseline
			"QDSA_AVAL", "QDSA_BVAL", "QDSA_CVAL", 
			"QDSA_DVAL", "QDSA_EVAL", "QDSA_FVAL", "QDSA_GVAL", "QDSA_HVAL", "QDSA_IVAL",
			"QDSA_JVAL", "QDSA_KVAL", "QDSA_LVAL", "QDSA_MVAL", "QDSA_NVAL", "QDSA_OVAL",
			"QDSA_PVAL",
# jour 232
			"QDSA_AVAL_J232", "QDSA_BVAL_J232", "QDSA_CVAL_J232", 
			"QDSA_DVAL_J232", "QDSA_EVAL_J232", "QDSA_FVAL_J232", "QDSA_GVAL_J232", 
			"QDSA_HVAL_J232", "QDSA_IVAL_J232",
			"QDSA_JVAL_J232", "QDSA_KVAL_J232", "QDSA_LVAL_J232", "QDSA_MVAL_J232", 
			"QDSA_NVAL_J232", "QDSA_OVAL_J232",
			"QDSA_PVAL_J232",
# jour 301
			"QDSA_AVAL_J301", "QDSA_BVAL_J301", "QDSA_CVAL_J301", 
			"QDSA_DVAL_J301", "QDSA_EVAL_J301", "QDSA_FVAL_J301", "QDSA_GVAL_J301", 
			"QDSA_HVAL_J301", "QDSA_IVAL_J301",
			"QDSA_JVAL_J301", "QDSA_KVAL_J301", "QDSA_LVAL_J301", "QDSA_MVAL_J301", 
			"QDSA_NVAL_J301", "QDSA_OVAL_J301",
			"QDSA_PVAL_J301")]

names(qdsa_data)
summary(qdsa_data)

#####################################################################

descriptive <- function(x){
  mean <- round(mean(x, na.rm=TRUE), 2)
  SD <- round(sd(x, na.rm=TRUE), 2)
  median <- round(median(x, na.rm = TRUE), 2)
  min <- round(min(x, na.rm = TRUE), 2)
  max <- round(max(x, na.rm = TRUE), 2)
  print(mean)
  print(SD)
  print(median)
  print(min)
  print(max)
}

#####################################################################

# SCORE - QUESTIONNAIRE DE LA DOULEUR DE SAINT-ANTOINE
# SENSORIEL A + B + C + D + E + F + G + H + I
# AFFECTIF J + K + L + M + N + O + P

# a non validated explanation of this calculus :
# http://physiotherapytest.com/questionnaire-douleur-de-saint-antoine-qdsa/

names(qdsa_data)

sens_base = qdsa_data$QDSA_AVAL + qdsa_data$QDSA_BVAL + 
  qdsa_data$QDSA_CVAL + qdsa_data$QDSA_DVAL + 
  qdsa_data$QDSA_EVAL + qdsa_data$QDSA_FVAL + 
  qdsa_data$QDSA_GVAL + qdsa_data$QDSA_HVAL + 
  qdsa_data$QDSA_IVAL 
descriptive(sens_base)

aff_base = qdsa_data$QDSA_JVAL + qdsa_data$QDSA_KVAL + 
  qdsa_data$QDSA_LVAL + qdsa_data$QDSA_MVAL + 
  qdsa_data$QDSA_NVAL + qdsa_data$QDSA_OVAL + 
  qdsa_data$QDSA_PVAL
descriptive(aff_base)

#-----------------------------------------------------------------------

sens_j232 = qdsa_data$QDSA_AVAL_J232 + qdsa_data$QDSA_BVAL_J232 + 
  qdsa_data$QDSA_CVAL_J232 + qdsa_data$QDSA_DVAL_J232 + 
  qdsa_data$QDSA_EVAL_J232 + qdsa_data$QDSA_FVAL_J232 + 
  qdsa_data$QDSA_GVAL_J232 + qdsa_data$QDSA_HVAL_J232 + 
  qdsa_data$QDSA_IVAL_J232 
descriptive(sens_j232)

aff_j232 = qdsa_data$QDSA_JVAL_J232 + qdsa_data$QDSA_KVAL_J232 + 
  qdsa_data$QDSA_LVAL_J232 + qdsa_data$QDSA_MVAL_J232 + 
  qdsa_data$QDSA_NVAL_J232 + qdsa_data$QDSA_OVAL_J232 + 
  qdsa_data$QDSA_PVAL_J232
descriptive(aff_j232)

#-----------------------------------------------------------------------

sens_j301 = qdsa_data$QDSA_AVAL_J301 + qdsa_data$QDSA_BVAL_J301 + 
  qdsa_data$QDSA_CVAL_J301 + qdsa_data$QDSA_DVAL_J301 + 
  qdsa_data$QDSA_EVAL_J301 + qdsa_data$QDSA_FVAL_J301 + 
  qdsa_data$QDSA_GVAL_J301 + qdsa_data$QDSA_HVAL_J301 + 
  qdsa_data$QDSA_IVAL_J301 
descriptive(sens_j301)


aff_j301 = qdsa_data$QDSA_JVAL_J301 + qdsa_data$QDSA_KVAL_J301 + 
  qdsa_data$QDSA_LVAL_J301 + qdsa_data$QDSA_MVAL_J301 + 
  qdsa_data$QDSA_NVAL_J301 + qdsa_data$QDSA_OVAL_J301 + 
  qdsa_data$QDSA_PVAL_J301
descriptive(aff_j301)

#####################################################################

wilcox.test(sens_base, sens_j232, paired=T, exact=F)
wilcox.test(aff_base, aff_j232, paired=T, exact=F)

wilcox.test(sens_base, sens_j301, paired=T, exact=F)
wilcox.test(aff_base, aff_j301, paired=T, exact=F)


#####################################################################
#####################################################################

missing_scores <- function(x) {
  result = x != "."
  return(result)
}

missing_scores(fm$QDSA_PVAL_J301)

#####################################################################

library('VIM')
library('magrittr')
library('mice')

#####################################################################


# VISUAL REPRESENTATION OF THE DATABASE 
md.pattern(qdsa_data)

aggr(qdsa_data, delimiter = NULL, plot = TRUE)
aggr(qdsa_data, delimiter = NULL, plot = TRUE)
plot(x = qdsa_data, 
     col = c("red"),
     numbers = FALSE,
     prop = FALSE, 
     varheight = FALSE,
     only.miss = FALSE, 
     border = par("fg"),
)

marginplot(qdsa_data[c(3,6)])

# The red box plot on the left shows the distribution of var X 
# the blue box plot shows the distribution of the remaining datapoints
# Likewhise for the red box plots at the bottom of the graph
# if our assumption of MCAR data is correct, then we expect the 
# red and blue box plots to be very similar

tempData_qdsa <- mice(qdsa_data,m=5,maxit=50,meth='pmm',seed=500)

# m=5 refers to the number of imputed datasets. Five is the default value
# meth='pmm' refers to the imputation method
# pmm predictive mean matching as imputation method
# methods(mice) for a list of the available imputation methods under 'mice'
summary(tempData_qdsa)

# check for imputed data as follows
tempData_qdsa$imp$QDSA_AVAL
tempData_qdsa$meth

qdsa_completedData <- complete(tempData_qdsa,1)
summary(qdsa_completedData)
summary(qdsa_data)

names(qdsa_completedData)

#####################################################################

# SCORE - QUESTIONNAIRE DE LA DOULEUR DE SAINT-ANTOINE
# SENSORIEL A + B + C + D + E + F + G + H + I
# AFFECTIF J + K + L + M + N + O + P

# a non validated explanation of this calculus :
# http://physiotherapytest.com/questionnaire-douleur-de-saint-antoine-qdsa/

names(qdsa_completedData)

sens_base = qdsa_completedData$QDSA_AVAL + qdsa_completedData$QDSA_BVAL + 
            qdsa_completedData$QDSA_CVAL + qdsa_completedData$QDSA_DVAL + 
            qdsa_completedData$QDSA_EVAL + qdsa_completedData$QDSA_FVAL + 
            qdsa_completedData$QDSA_GVAL + qdsa_completedData$QDSA_HVAL + 
            qdsa_completedData$QDSA_IVAL 
descriptive(sens_base)


aff_base = qdsa_completedData$QDSA_JVAL + qdsa_completedData$QDSA_KVAL + 
            qdsa_completedData$QDSA_LVAL + qdsa_completedData$QDSA_MVAL + 
            qdsa_completedData$QDSA_NVAL + qdsa_completedData$QDSA_OVAL + 
            qdsa_completedData$QDSA_PVAL
descriptive(aff_base)

#-----------------------------------------------------------------------

sens_j232 = qdsa_completedData$QDSA_AVAL_J232 + qdsa_completedData$QDSA_BVAL_J232 + 
  qdsa_completedData$QDSA_CVAL_J232 + qdsa_completedData$QDSA_DVAL_J232 + 
  qdsa_completedData$QDSA_EVAL_J232 + qdsa_completedData$QDSA_FVAL_J232 + 
  qdsa_completedData$QDSA_GVAL_J232 + qdsa_completedData$QDSA_HVAL_J232 + 
  qdsa_completedData$QDSA_IVAL_J232 
descriptive(sens_j232)

aff_j232 = qdsa_completedData$QDSA_JVAL_J232 + qdsa_completedData$QDSA_KVAL_J232 + 
  qdsa_completedData$QDSA_LVAL_J232 + qdsa_completedData$QDSA_MVAL_J232 + 
  qdsa_completedData$QDSA_NVAL_J232 + qdsa_completedData$QDSA_OVAL_J232 + 
  qdsa_completedData$QDSA_PVAL_J232
descriptive(aff_j232)

#-----------------------------------------------------------------------

sens_j301 = qdsa_completedData$QDSA_AVAL_J301 + qdsa_completedData$QDSA_BVAL_J301 + 
  qdsa_completedData$QDSA_CVAL_J301 + qdsa_completedData$QDSA_DVAL_J301 + 
  qdsa_completedData$QDSA_EVAL_J301 + qdsa_completedData$QDSA_FVAL_J301 + 
  qdsa_completedData$QDSA_GVAL_J301 + qdsa_completedData$QDSA_HVAL_J301 + 
  qdsa_completedData$QDSA_IVAL_J301 
descriptive(sens_j301)

aff_j301_2 = qdsa_completedData$QDSA_JVAL_J301 + qdsa_completedData$QDSA_KVAL_J301 + 
  qdsa_completedData$QDSA_LVAL_J301 + qdsa_completedData$QDSA_MVAL_J301 + 
  qdsa_completedData$QDSA_NVAL_J301 + qdsa_completedData$QDSA_OVAL_J301 + 
  qdsa_completedData$QDSA_PVAL_J301
descriptive(aff_j301_2)

#####################################################################

wilcox.test(sens_base, sens_j232, paired=T, exact=F)
wilcox.test(aff_base, aff_j232, paired=T, exact=F)

wilcox.test(sens_base, sens_j301, paired=T, exact=F)
wilcox.test(aff_base, aff_j301, paired=T, exact=F)


#####################################################################

# QDSA compared to baselinne (all subjects)

w <- function(x, y){
  #result = shapiro.test(x)
  result = wilcox.test(x, y, paired=F, exact=F, correct=F)
  #result = t.test(x, y, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
  return(result)
}

w(qdsa_completedData$QDSA_AVAL_J232, qdsa_completedData$QDSA_AVAL)
w(qdsa_completedData$QDSA_BVAL_J232, qdsa_completedData$QDSA_BVAL)
w(qdsa_completedData$QDSA_CVAL_J232, qdsa_completedData$QDSA_CVAL)
w(qdsa_completedData$QDSA_DVAL_J232, qdsa_completedData$QDSA_DVAL) 
w(qdsa_completedData$QDSA_EVAL_J232, qdsa_completedData$QDSA_EVAL)
w(qdsa_completedData$QDSA_FVAL_J232, qdsa_completedData$QDSA_FVAL)
w(qdsa_completedData$QDSA_GVAL_J232, qdsa_completedData$QDSA_GVAL)
w(qdsa_completedData$QDSA_HVAL_J232, qdsa_completedData$QDSA_HVAL)
w(qdsa_completedData$QDSA_IVAL_J232, qdsa_completedData$QDSA_IVAL)
w(qdsa_completedData$QDSA_JVAL_J232, qdsa_completedData$QDSA_JVAL)
w(qdsa_completedData$QDSA_KVAL_J232, qdsa_completedData$QDSA_KVAL)
w(qdsa_completedData$QDSA_LVAL_J232, qdsa_completedData$QDSA_LVAL)
w(qdsa_completedData$QDSA_MVAL_J232, qdsa_completedData$QDSA_MVAL)
w(qdsa_completedData$QDSA_NVAL_J232, qdsa_completedData$QDSA_NVAL)
w(qdsa_completedData$QDSA_OVAL_J232, qdsa_completedData$QDSA_OVAL)
w(qdsa_completedData$QDSA_PVAL_J232, qdsa_completedData$QDSA_PVAL) 

w(qdsa_completedData$QDSA_AVAL_J301, qdsa_completedData$QDSA_AVAL) 
w(qdsa_completedData$QDSA_BVAL_J301, qdsa_completedData$QDSA_BVAL)
w(qdsa_completedData$QDSA_CVAL_J301, qdsa_completedData$QDSA_CVAL)
w(qdsa_completedData$QDSA_DVAL_J301, qdsa_completedData$QDSA_DVAL) 
w(qdsa_completedData$QDSA_EVAL_J301, qdsa_completedData$QDSA_EVAL)
w(qdsa_completedData$QDSA_FVAL_J301, qdsa_completedData$QDSA_FVAL)
w(qdsa_completedData$QDSA_GVAL_J301, qdsa_completedData$QDSA_GVAL)
w(qdsa_completedData$QDSA_HVAL_J301, qdsa_completedData$QDSA_HVAL)
w(qdsa_completedData$QDSA_IVAL_J301, qdsa_completedData$QDSA_IVAL)
w(qdsa_completedData$QDSA_JVAL_J301, qdsa_completedData$QDSA_JVAL)
w(qdsa_completedData$QDSA_KVAL_J301, qdsa_completedData$QDSA_KVAL)
w(qdsa_completedData$QDSA_LVAL_J301, qdsa_completedData$QDSA_LVAL)
w(qdsa_completedData$QDSA_MVAL_J301, qdsa_completedData$QDSA_MVAL)
w(qdsa_completedData$QDSA_NVAL_J301, qdsa_completedData$QDSA_NVAL)
w(qdsa_completedData$QDSA_OVAL_J301, qdsa_completedData$QDSA_OVAL)
w(qdsa_completedData$QDSA_PVAL_J301, qdsa_completedData$QDSA_PVAL) 

#####################################################################
#####################################################################

# comparer la variation du score QDSA (à J232 et J301, par rapport à J0) 
# entre le groupe OSTEO et le groupe TEMOIN.

deltafunc <- function(x, y){
  result = x-y
  return(result)
}

# ONE OF THE OBJECTIVE OF THE STUDY IS THE VARIATION OF QDSA BETWEEN DAY 232 AND BASELINE
# AND DAY 301 AND BASELINE
# HOW THE SCORE IS MADE ? THE SCALE DOES NOT SEEM TO BE ONE 

DA_232 <- deltafunc(qdsa_completedData$QDSA_AVAL_J232, qdsa_completedData$QDSA_AVAL) 
DB_232 <- deltafunc(qdsa_completedData$QDSA_BVAL_J232, qdsa_completedData$QDSA_BVAL)
DC_232 <- deltafunc(qdsa_completedData$QDSA_CVAL_J232, qdsa_completedData$QDSA_CVAL)
DD_232 <- deltafunc(qdsa_completedData$QDSA_DVAL_J232, qdsa_completedData$QDSA_DVAL) 
DE_232 <- deltafunc(qdsa_completedData$QDSA_EVAL_J232, qdsa_completedData$QDSA_EVAL)
DF_232 <- deltafunc(qdsa_completedData$QDSA_FVAL_J232, qdsa_completedData$QDSA_FVAL)
DG_232 <- deltafunc(qdsa_completedData$QDSA_GVAL_J232, qdsa_completedData$QDSA_GVAL)
DH_232 <- deltafunc(qdsa_completedData$QDSA_HVAL_J232, qdsa_completedData$QDSA_HVAL)
DI_232 <- deltafunc(qdsa_completedData$QDSA_IVAL_J232, qdsa_completedData$QDSA_IVAL)
DJ_232 <- deltafunc(qdsa_completedData$QDSA_JVAL_J232, qdsa_completedData$QDSA_JVAL)
DK_232 <- deltafunc(qdsa_completedData$QDSA_KVAL_J232, qdsa_completedData$QDSA_KVAL)
DL_232 <- deltafunc(qdsa_completedData$QDSA_LVAL_J232, qdsa_completedData$QDSA_LVAL)
DM_232 <- deltafunc(qdsa_completedData$QDSA_MVAL_J232, qdsa_completedData$QDSA_MVAL)
DN_232 <- deltafunc(qdsa_completedData$QDSA_NVAL_J232, qdsa_completedData$QDSA_NVAL)
DO_232 <- deltafunc(qdsa_completedData$QDSA_OVAL_J232, qdsa_completedData$QDSA_OVAL)
DP_232 <- deltafunc(qdsa_completedData$QDSA_PVAL_J232, qdsa_completedData$QDSA_PVAL) 

DA_301 <- deltafunc(qdsa_completedData$QDSA_AVAL_J301, qdsa_completedData$QDSA_AVAL) 
DB_301 <- deltafunc(qdsa_completedData$QDSA_BVAL_J301, qdsa_completedData$QDSA_BVAL)
DC_301 <- deltafunc(qdsa_completedData$QDSA_CVAL_J301, qdsa_completedData$QDSA_CVAL)
DD_301 <- deltafunc(qdsa_completedData$QDSA_DVAL_J301, qdsa_completedData$QDSA_DVAL) 
DE_301 <- deltafunc(qdsa_completedData$QDSA_EVAL_J301, qdsa_completedData$QDSA_EVAL)
DF_301 <- deltafunc(qdsa_completedData$QDSA_FVAL_J301, qdsa_completedData$QDSA_FVAL)
DG_301 <- deltafunc(qdsa_completedData$QDSA_GVAL_J301, qdsa_completedData$QDSA_GVAL)
DH_301 <- deltafunc(qdsa_completedData$QDSA_HVAL_J301, qdsa_completedData$QDSA_HVAL)
DI_301 <- deltafunc(qdsa_completedData$QDSA_IVAL_J301, qdsa_completedData$QDSA_IVAL)
DJ_301 <- deltafunc(qdsa_completedData$QDSA_JVAL_J301, qdsa_completedData$QDSA_JVAL)
DK_301 <- deltafunc(qdsa_completedData$QDSA_KVAL_J301, qdsa_completedData$QDSA_KVAL)
DL_301 <- deltafunc(qdsa_completedData$QDSA_LVAL_J301, qdsa_completedData$QDSA_LVAL)
DM_301 <- deltafunc(qdsa_completedData$QDSA_MVAL_J301, qdsa_completedData$QDSA_MVAL)
DN_301 <- deltafunc(qdsa_completedData$QDSA_NVAL_J301, qdsa_completedData$QDSA_NVAL)
DO_301 <- deltafunc(qdsa_completedData$QDSA_OVAL_J301, qdsa_completedData$QDSA_OVAL)
DP_301 <- deltafunc(qdsa_completedData$QDSA_PVAL_J301, qdsa_completedData$QDSA_PVAL) 

wg <- function(x, y){
  #result = shapiro.test(x)
  result = wilcox.test(x ~ fm$GP_RANDO, paired=F, exact=F, correct=F)
  #result = t.test(x, y, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
  return(result)
}

wg(DA_232)
wg(DB_232)
wg(DC_232)
wg(DD_232) 
wg(DE_232)
wg(DF_232)
wg(DG_232)
wg(DH_232)
wg(DI_232)
wg(DJ_232)
wg(DK_232)
wg(DL_232)
wg(DM_232)
wg(DN_232)
wg(DO_232)
wg(DP_232) 

wg(DA_301) 
wg(DB_301)
wg(DC_301)
wg(DD_301) 
wg(DE_301)
wg(DF_301)
wg(DG_301)
wg(DH_301)
wg(DI_301)
wg(DJ_301)
wg(DK_301)
wg(DL_301)
wg(DM_301)
wg(DN_301)
wg(DO_301)
wg(DP_301) 

#####################################################################

# SECONDARY OUTCOME
# DELTA TO THE BASELINE FOR EVERY QSDA
# to compare QDSA DELTAS TO BASELINE ACCORDING TO THE GROUP

sens_b232 <- (sens_j232 - sens_base)
shapiro.test(sens_b232) # NOTE : H0 CANNOT BE REJECTED P>0.05 H0:POPULATION IS NORMALLY DISTRIBUTED
mean(sens_b232, na.rm=T)
describeBy(sens_b232, fm$GP_RANDO)
t.test(sens_b232, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(sens_b232 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)

aff_b232 <- (aff_j232 - aff_base)
describeBy(aff_b232, fm$GP_RANDO)
t.test(aff_b232, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(aff_b232 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)

sens_b301 <- (sens_j301 - sens_base)
describeBy(sens_b301, fm$GP_RANDO)
t.test(sens_b301, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(sens_b301 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)

aff_b301 <- (aff_j301 - sens_base)
describeBy(aff_b301, fm$GP_RANDO)
t.test(aff_b301, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(aff_b301 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)