### FOLDER
getwd()
setwd("P:/CONSULTATION/FMOstéo")
getwd()

#--------------------------------------------------------------------

### DATABASE
fm <- read.csv2("db_missing.csv", header = TRUE, na.string="")
names(fm)
#View(fm)

#--------------------------------------------------------------------

### PACKAGES

#install.packages("tidyverse")
library("tidyverse")

#install.packages("asht")
library("asht")

#install.packages("bootstrap")
library("bootstrap")

#install.packages("boot")
library("boot")

#--------------------------------------------------------------------

### DATA MANAGEMENT
### RENAME EVA

fm <- as_tibble(fm)
fm <- fm %>% rename(
  # new name = old name,
  "E_5" = "E_5",
  "E_47" = "E_47",
  "E_89" = "E_89",
  "E_131" = "E_131",
  "E_173" = "E_173",
  "E_215" = "E_215",
  "E_232" = "SC_DOL_MM_J232",
  "E_301" = "SC_DOL_MM_J301"
  )

#--------------------------------------------------------------------

# FUNCTIONS

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

w <- function(x, y){
  result1 = shapiro.test(x)
  result2 = wilcox.test(x, y, paired=F, exact=F, correct=F)
  result3 = t.test(x, y, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
  return(result1)
  print(result2)
  print(result3)
}

w <- function(x, y){
  #result = shapiro.test(x)
  result = wilcox.test(x, y, paired=F, exact=F, correct=F)
  #result = t.test(x, y, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
  return(result)
}

wg <- function(x, y){
  #result = shapiro.test(x)
  result = wilcox.test(x ~ fm$GP_RANDO, paired=F, exact=F, correct=F)
  #result = t.test(x, y, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
  return(result)
}

deltafunc <- function(x, y){
  result = x-y
  return(result)
}

#####################################################################
#####################################################################
#####################################################################

### EVA DESCRIPTIVE

descriptive(fm$E_5)
descriptive(fm$E_47)
descriptive(fm$E_89)
descriptive(fm$E_131)
descriptive(fm$E_173)
descriptive(fm$E_215)
descriptive(fm$E_232)
descriptive(fm$E_301)

### PRIMARY OUTCOME: EVA
# the primary outcome is the EVA delta between the two groups
# how are groups identified? "GP_RANDO"

#names(fm)
#table(fm$GP_RANDO)
#fm$eva_delta <-(fm$E_232 - fm$E_5)
#wilcox.test(fm$eva_delta ~ fm$GP_RANDO, paired=F, exact=F, correct=F)
#hist(fm$eva_delta)
#shapiro.test(fm$eva_delta)
# the p-value > 0.05 implying that the distribution of the data are not significantly 
# different from normal distribution: we can assume the normality
#t.test(fm$eva_delta ~ fm$GP_RANDO, var.equal=FALSE, paired=FALSE, conf.level=0.95)

fm$ED47 <- (fm$E_47 - fm$E_5)
#shapiro.test(fm$ED47)
#t.test(fm$ED47, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(fm$ED47 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)
#fm$ED47<-as.numeric(as.character(fm$ED47))
wilcox.test(eva_completedData$ED47 ~ fm$GP_RANDO, paired=F, exact=F, correct=F, 
            confint.merMod(method = "boot", boot.type = "perc", object = "boot.ci"))

eva_completedData$ED47 <-as.numeric(as.character(eva_completedData$ED47 ))
    x <- eva_completedData$ED47 
    theta <- function(x){mean(x)}
    results <- boott(x,theta,nbootsd=200,nboott=1000)

    set.seed(1000)
    x <- eva_completedData$ED47
    #x <- as.vector(x)
    theta <- function(p,x) {sum(p*x)/sum(p)}
    abcnonHtest(x, theta, conf.level=0.95, nullValue=0)
    abcnon(x, theta)

#------------------------------

fm$ED89 <- (fm$E_89 - fm$E_5)
#shapiro.test(fm$ED89)
#t.test(fm$ED89, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(fm$ED89 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)
wilcox.test(eva_completedData$ED89 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)
    x <- eva_completedData$ED89 
    theta <- function(x){mean(x)}
    results <- boott(x,theta,nbootsd=200,nboott=1000)

    set.seed(1000)
    x <- eva_completedData$ED89
    #x <- as.vector(x)
    theta <- function(p,x) {sum(p*x)/sum(p)}
    abcnonHtest(x, theta, conf.level=0.95, nullValue=0)
    abcnon(x, theta)
    
#------------------------------

fm$ED131 <- (fm$E_131 - fm$E_5)
#shapiro.test(fm$ED131)
#t.test(fm$ED131, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(fm$ED131 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)
wilcox.test(eva_completedData$ED131 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)
    x <- eva_completedData$ED131 
    theta <- function(x){mean(x)}
    results <- boott(x,theta,nbootsd=200,nboott=1000)
    results
    
    set.seed(1000)
    x <- eva_completedData$ED131
    #x <- as.vector(x)
    theta <- function(p,x) {sum(p*x)/sum(p)}
    abcnonHtest(x, theta, conf.level=0.95, nullValue=0)
    abcnon(x, theta)

#------------------------------

fm$ED173 <- (fm$E_173 - fm$E_5)
#shapiro.test(fm$ED173)
#t.test(fm$ED173, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(fm$ED173 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)
wilcox.test(eva_completedData$ED173 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)
    x <- eva_completedData$ED173 
    theta <- function(x){mean(x)}
    results <- boott(x,theta,nbootsd=200,nboott=1000)
    results
    
    set.seed(1000)
    x <- eva_completedData$ED173
    #x <- as.vector(x)
    theta <- function(p,x) {sum(p*x)/sum(p)}
    abcnonHtest(x, theta, conf.level=0.95, nullValue=0)
    abcnon(x, theta)

#------------------------------

fm$ED215 <- (fm$E_215 - fm$E_5)
#shapiro.test(fm$ED215)
#t.test(fm$ED215, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(fm$ED215 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)
wilcox.test(eva_completedData$ED215 ~ fm$GP_RANDO, paired=F, exact=F, correct=F 
            #, bootstrap(x, theta, nboot = 100)
    )
    x <- eva_completedData$ED215 
    theta <- function(x){mean(x)}
    results <- boott(x,theta,nbootsd=200,nboott=1000)
    results
    
    set.seed(1000)
    x <- eva_completedData$ED215
    #x <- as.vector(x)
    theta <- function(p,x) {sum(p*x)/sum(p)}
    abcnonHtest(x, theta, conf.level=0.95, nullValue=0)
    abcnon(x, theta)

#------------------------------

fm$ED232 <- (fm$E_232 - fm$E_5)
#shapiro.test(fm$ED232)
#t.test(fm$ED232, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(fm$ED232 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)
wilcox.test(eva_completedData$ED215 ~ fm$GP_RANDO, paired=F, exact=F, correct=F, conf.int = TRUE, conf.level = 0.95)
    x <- eva_completedData$ED232 
    theta <- function(x){mean(x)}
    results <- boott(x,theta,nbootsd=200,nboott=1000)
    results
    
    x <- eva_completedData$ED232
    theta <- function(p,x) {sum(p*x)/sum(p)}
    abcnonHtest(x, theta, conf.level=0.95, nullValue=0)
    abcnon(x, theta)

#------------------------------

fm$ED301 <- (fm$E_301 - fm$E_5)
#shapiro.test(fm$ED301)
#t.test(fm$ED301, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(fm$ED301 ~ fm$GP_RANDO, paired=F, exact=F, correct=F, conf.int=T, conf.level=0.95)
wilcox.test(eva_completedData$ED301 ~ fm$GP_RANDO, paired=F, exact=F, correct=F, conf.level = 0.95, conf.int = T)
    x <- eva_completedData$ED301
    theta <- function(x){mean(x)}
    results <- boott(x,theta,nbootsd=200,nboott=1000)
    results
    
    x <- eva_completedData$ED301
    theta <- function(p,x) {sum(p*x)/sum(p)}
    abcnonHtest(x, theta, conf.level=0.95, nullValue=0)
    abcnon(x, theta)

#####################################################################
#####################################################################
#####################################################################

# ONE OF THE OBJECTIVE OF THE STUDY IS THE VARIATION OF QDSA BETWEEN DAY 232 AND BASELINE
# AND DAY 301 AND BASELINE
# HOW THE SCORE IS MADE ? THE SCALE DOES NOT SEEM TO BE ONE 
# THE PURPOSE IS TO TEST THE DIFFERENCE

DA_232 <- deltafunc(fm$QDSA_AVAL_J232, fm$QDSA_AVAL) 
DB_232 <- deltafunc(fm$QDSA_BVAL_J232, fm$QDSA_BVAL)
DC_232 <- deltafunc(fm$QDSA_CVAL_J232, fm$QDSA_CVAL)
DD_232 <- deltafunc(fm$QDSA_DVAL_J232, fm$QDSA_DVAL) 
DE_232 <- deltafunc(fm$QDSA_EVAL_J232, fm$QDSA_EVAL)
DF_232 <- deltafunc(fm$QDSA_FVAL_J232, fm$QDSA_FVAL)
DG_232 <- deltafunc(fm$QDSA_GVAL_J232, fm$QDSA_GVAL)
DH_232 <- deltafunc(fm$QDSA_HVAL_J232, fm$QDSA_HVAL)
DI_232 <- deltafunc(fm$QDSA_IVAL_J232, fm$QDSA_IVAL)
DJ_232 <- deltafunc(fm$QDSA_JVAL_J232, fm$QDSA_JVAL)
DK_232 <- deltafunc(fm$QDSA_KVAL_J232, fm$QDSA_KVAL)
DL_232 <- deltafunc(fm$QDSA_LVAL_J232, fm$QDSA_LVAL)
DM_232 <- deltafunc(fm$QDSA_MVAL_J232, fm$QDSA_MVAL)
DN_232 <- deltafunc(fm$QDSA_NVAL_J232, fm$QDSA_NVAL)
DO_232 <- deltafunc(fm$QDSA_OVAL_J232, fm$QDSA_OVAL)
DP_232 <- deltafunc(fm$QDSA_PVAL_J232, fm$QDSA_PVAL) 

DA_301 <- deltafunc(fm$QDSA_AVAL_J301, fm$QDSA_AVAL) 
DB_301 <- deltafunc(fm$QDSA_BVAL_J301, fm$QDSA_BVAL)
DC_301 <- deltafunc(fm$QDSA_CVAL_J301, fm$QDSA_CVAL)
DD_301 <- deltafunc(fm$QDSA_DVAL_J301, fm$QDSA_DVAL) 
DE_301 <- deltafunc(fm$QDSA_EVAL_J301, fm$QDSA_EVAL)
DF_301 <- deltafunc(fm$QDSA_FVAL_J301, fm$QDSA_FVAL)
DG_301 <- deltafunc(fm$QDSA_GVAL_J301, fm$QDSA_GVAL)
DH_301 <- deltafunc(fm$QDSA_HVAL_J301, fm$QDSA_HVAL)
DI_301 <- deltafunc(fm$QDSA_IVAL_J301, fm$QDSA_IVAL)
DJ_301 <- deltafunc(fm$QDSA_JVAL_J301, fm$QDSA_JVAL)
DK_301 <- deltafunc(fm$QDSA_KVAL_J301, fm$QDSA_KVAL)
DL_301 <- deltafunc(fm$QDSA_LVAL_J301, fm$QDSA_LVAL)
DM_301 <- deltafunc(fm$QDSA_MVAL_J301, fm$QDSA_MVAL)
DN_301 <- deltafunc(fm$QDSA_NVAL_J301, fm$QDSA_NVAL)
DO_301 <- deltafunc(fm$QDSA_OVAL_J301, fm$QDSA_OVAL)
DP_301 <- deltafunc(fm$QDSA_PVAL_J301, fm$QDSA_PVAL) 

#####################################################################
#####################################################################
#####################################################################

sens_base = fm$QDSA_AVAL + fm$QDSA_BVAL + 
  fm$QDSA_CVAL + fm$QDSA_DVAL + 
  fm$QDSA_EVAL + fm$QDSA_FVAL + 
  fm$QDSA_GVAL + fm$QDSA_HVAL + 
  fm$QDSA_IVAL 
descriptive(sens_base)

aff_base = fm$QDSA_JVAL + fm$QDSA_KVAL + 
  fm$QDSA_LVAL + fm$QDSA_MVAL + 
  fm$QDSA_NVAL + fm$QDSA_OVAL + 
  fm$QDSA_PVAL
descriptive(aff_base)

#-----------------------------------------------------------------------

sens_j232 = fm$QDSA_AVAL_J232 + fm$QDSA_BVAL_J232 + 
  fm$QDSA_CVAL_J232 + fm$QDSA_DVAL_J232 + 
  fm$QDSA_EVAL_J232 + fm$QDSA_FVAL_J232 + 
  fm$QDSA_GVAL_J232 + fm$QDSA_HVAL_J232 + 
  fm$QDSA_IVAL_J232 
descriptive(sens_j232)

aff_j232 = fm$QDSA_JVAL_J232 + fm$QDSA_KVAL_J232 + 
  fm$QDSA_LVAL_J232 + fm$QDSA_MVAL_J232 + 
  fm$QDSA_NVAL_J232 + fm$QDSA_OVAL_J232 + 
  fm$QDSA_PVAL_J232
descriptive(aff_j232)

#-----------------------------------------------------------------------

sens_j301 = fm$QDSA_AVAL_J301 + fm$QDSA_BVAL_J301 + 
  fm$QDSA_CVAL_J301 + fm$QDSA_DVAL_J301 + 
  fm$QDSA_EVAL_J301 + fm$QDSA_FVAL_J301 + 
  fm$QDSA_GVAL_J301 + fm$QDSA_HVAL_J301 + 
  fm$QDSA_IVAL_J301 
descriptive(sens_j301)


aff_j301 = fm$QDSA_JVAL_J301 + fm$QDSA_KVAL_J301 + 
  fm$QDSA_LVAL_J301 + fm$QDSA_MVAL_J301 + 
  fm$QDSA_NVAL_J301 + fm$QDSA_OVAL_J301 + 
  fm$QDSA_PVAL_J301
descriptive(aff_j301)

#--------------------------------------------------------------------

wilcox.test(sens_base, sens_j232, paired=T, exact=F)
wilcox.test(aff_base, aff_j232, paired=T, exact=F)

wilcox.test(sens_base, sens_j301, paired=T, exact=F)
wilcox.test(aff_base, aff_j301, paired=T, exact=F)

#--------------------------------------------------------------------

# only testing the difference between the variables 

w(fm$QDSA_AVAL_J232, fm$QDSA_AVAL)
w(fm$QDSA_BVAL_J232, fm$QDSA_BVAL)
w(fm$QDSA_CVAL_J232, fm$QDSA_CVAL)
w(fm$QDSA_DVAL_J232, fm$QDSA_DVAL) 
w(fm$QDSA_EVAL_J232, fm$QDSA_EVAL)
w(fm$QDSA_FVAL_J232, fm$QDSA_FVAL)
w(fm$QDSA_GVAL_J232, fm$QDSA_GVAL)
w(fm$QDSA_HVAL_J232, fm$QDSA_HVAL)
w(fm$QDSA_IVAL_J232, fm$QDSA_IVAL)
w(fm$QDSA_JVAL_J232, fm$QDSA_JVAL)
w(fm$QDSA_KVAL_J232, fm$QDSA_KVAL)
w(fm$QDSA_LVAL_J232, fm$QDSA_LVAL)
w(fm$QDSA_MVAL_J232, fm$QDSA_MVAL)
w(fm$QDSA_NVAL_J232, fm$QDSA_NVAL)
w(fm$QDSA_OVAL_J232, fm$QDSA_OVAL)
w(fm$QDSA_PVAL_J232, fm$QDSA_PVAL) 

w(fm$QDSA_AVAL_J301, fm$QDSA_AVAL) 
w(fm$QDSA_BVAL_J301, fm$QDSA_BVAL)
w(fm$QDSA_CVAL_J301, fm$QDSA_CVAL)
w(fm$QDSA_DVAL_J301, fm$QDSA_DVAL) 
w(fm$QDSA_EVAL_J301, fm$QDSA_EVAL)
w(fm$QDSA_FVAL_J301, fm$QDSA_FVAL)
w(fm$QDSA_GVAL_J301, fm$QDSA_GVAL)
w(fm$QDSA_HVAL_J301, fm$QDSA_HVAL)
w(fm$QDSA_IVAL_J301, fm$QDSA_IVAL)
w(fm$QDSA_JVAL_J301, fm$QDSA_JVAL)
w(fm$QDSA_KVAL_J301, fm$QDSA_KVAL)
w(fm$QDSA_LVAL_J301, fm$QDSA_LVAL)
w(fm$QDSA_MVAL_J301, fm$QDSA_MVAL)
w(fm$QDSA_NVAL_J301, fm$QDSA_NVAL)
w(fm$QDSA_OVAL_J301, fm$QDSA_OVAL)
w(fm$QDSA_PVAL_J301, fm$QDSA_PVAL) 

# comparer la variation du score QDSA (à J232 et J301, par rapport à J0) 
# entre le groupe OSTEO et le groupe TEMOIN.

deltafunc <- function(x, y){
  result = x-y
  return(result)
}

#--------------------------------------------------------------------

# ONE OF THE OBJECTIVE OF THE STUDY IS THE VARIATION OF QDSA BETWEEN DAY 232 AND BASELINE
# AND DAY 301 AND BASELINE
# HOW THE SCORE IS MADE ? THE SCALE DOES NOT SEEM TO BE ONE 

DA_232 <- deltafunc(fm$QDSA_AVAL_J232, fm$QDSA_AVAL) 
DB_232 <- deltafunc(fm$QDSA_BVAL_J232, fm$QDSA_BVAL)
DC_232 <- deltafunc(fm$QDSA_CVAL_J232, fm$QDSA_CVAL)
DD_232 <- deltafunc(fm$QDSA_DVAL_J232, fm$QDSA_DVAL) 
DE_232 <- deltafunc(fm$QDSA_EVAL_J232, fm$QDSA_EVAL)
DF_232 <- deltafunc(fm$QDSA_FVAL_J232, fm$QDSA_FVAL)
DG_232 <- deltafunc(fm$QDSA_GVAL_J232, fm$QDSA_GVAL)
DH_232 <- deltafunc(fm$QDSA_HVAL_J232, fm$QDSA_HVAL)
DI_232 <- deltafunc(fm$QDSA_IVAL_J232, fm$QDSA_IVAL)
DJ_232 <- deltafunc(fm$QDSA_JVAL_J232, fm$QDSA_JVAL)
DK_232 <- deltafunc(fm$QDSA_KVAL_J232, fm$QDSA_KVAL)
DL_232 <- deltafunc(fm$QDSA_LVAL_J232, fm$QDSA_LVAL)
DM_232 <- deltafunc(fm$QDSA_MVAL_J232, fm$QDSA_MVAL)
DN_232 <- deltafunc(fm$QDSA_NVAL_J232, fm$QDSA_NVAL)
DO_232 <- deltafunc(fm$QDSA_OVAL_J232, fm$QDSA_OVAL)
DP_232 <- deltafunc(fm$QDSA_PVAL_J232, fm$QDSA_PVAL) 

DA_301 <- deltafunc(fm$QDSA_AVAL_J301, fm$QDSA_AVAL) 
DB_301 <- deltafunc(fm$QDSA_BVAL_J301, fm$QDSA_BVAL)
DC_301 <- deltafunc(fm$QDSA_CVAL_J301, fm$QDSA_CVAL)
DD_301 <- deltafunc(fm$QDSA_DVAL_J301, fm$QDSA_DVAL) 
DE_301 <- deltafunc(fm$QDSA_EVAL_J301, fm$QDSA_EVAL)
DF_301 <- deltafunc(fm$QDSA_FVAL_J301, fm$QDSA_FVAL)
DG_301 <- deltafunc(fm$QDSA_GVAL_J301, fm$QDSA_GVAL)
DH_301 <- deltafunc(fm$QDSA_HVAL_J301, fm$QDSA_HVAL)
DI_301 <- deltafunc(fm$QDSA_IVAL_J301, fm$QDSA_IVAL)
DJ_301 <- deltafunc(fm$QDSA_JVAL_J301, fm$QDSA_JVAL)
DK_301 <- deltafunc(fm$QDSA_KVAL_J301, fm$QDSA_KVAL)
DL_301 <- deltafunc(fm$QDSA_LVAL_J301, fm$QDSA_LVAL)
DM_301 <- deltafunc(fm$QDSA_MVAL_J301, fm$QDSA_MVAL)
DN_301 <- deltafunc(fm$QDSA_NVAL_J301, fm$QDSA_NVAL)
DO_301 <- deltafunc(fm$QDSA_OVAL_J301, fm$QDSA_OVAL)
DP_301 <- deltafunc(fm$QDSA_PVAL_J301, fm$QDSA_PVAL) 

#-----------------------------------------------------------------------

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

#-----------------------------------------------------------------------

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


#####################################################################
#####################################################################
#####################################################################

# scoring the SF36 : https://www.rand.org/health-care/surveys_tools/mos/36-item-short-form/scoring.html

fm$SF36_PF=(fm$SF36_3_1_J232 + fm$SF36_3_2_J232 + fm$SF36_3_3_J232 + fm$SF36_3_4_J232 +
              fm$SF36_3_5_J232 + fm$SF36_3_6_J232 + fm$SF36_3_7_J232 + fm$SF36_3_8_J232 +
              fm$SF36_3_9_J232 + fm$SF36_3_10_J232)/10

fm$SF36_RP=(fm$SF36_4_1 + fm$SF36_4_2 + fm$SF36_4_3 + fm$SF36_4_4)/4

fm$SF36_BP=(fm$SF36_7 + fm$SF36_8)/2

fm$SF36_GH=(fm$SF36_1 + fm$SF36_2 + fm$SF36_11_1 +
              fm$SF36_11_2 + fm$SF36_11_3 + fm$SF36_11_4)/6

fm$SF36_VT=(fm$SF36_9_1 + fm$SF36_9_5 + fm$SF36_9_7 + fm$SF36_9_9)/4

fm$SF36_SF=(fm$SF36_6 + fm$SF36_10)/2

fm$SF36_RE=(fm$SF36_5_1_J232 + fm$SF36_5_2 + fm$SF36_5_3)/3

fm$SF36_MH=(fm$SF36_9_2 + fm$SF36_9_3+ fm$SF36_9_4 + 
              fm$SF36_9_6 + fm$SF36_8)/5

fm$SF36_SC_MEN=(fm$SF36_VT + fm$SF36_SF + fm$SF36_RE + fm$SF36_MH)/4

fm$SF36_SC_PHY=(fm$SF36_PF + fm$SF36_RP + fm$SF36_BP + fm$SF36_GH)/4

#-----------------------------------------------------------------------

fm$SF36_PF_J232=(fm$SF36_3_1_J232 + fm$SF36_3_2_J232 + fm$SF36_3_3_J232 + fm$SF36_3_4_J232 +
                   fm$SF36_3_5_J232 + fm$SF36_3_6_J232 + fm$SF36_3_7_J232 + fm$SF36_3_8_J232 +
                   fm$SF36_3_9_J232 + fm$SF36_3_10_J232)/10

fm$SF36_RP_J232=(fm$SF36_4_1_J232 + fm$SF36_4_2_J232 + fm$SF36_4_3_J232 + fm$SF36_4_4_J232)/4

fm$SF36_BP_J232=(fm$SF36_7_J232 + fm$SF36_8_J232)/2

fm$SF36_GH_J232=(fm$SF36_1_J232 + fm$SF36_2_J232 + fm$SF36_11_1_J232 +
                   fm$SF36_11_2_J232 + fm$SF36_11_3_J232 + fm$SF36_11_4_J232)/6

fm$SF36_VT_J232=(fm$SF36_9_1_J232 + fm$SF36_9_5_J232 + fm$SF36_9_7_J232 + fm$SF36_9_9_J232)/4

fm$SF36_SF_J232=(fm$SF36_6_J232 + fm$SF36_10_J232)/2

fm$SF36_RE_J232=(fm$SF36_5_1_J232 + fm$SF36_5_2_J232 + fm$SF36_5_3_J232)/3

fm$SF36_MH_J232=(fm$SF36_9_2_J232 + fm$SF36_9_3_J232 + fm$SF36_9_4_J232 + 
                   fm$SF36_9_6_J232 + fm$SF36_8_J232)/5

fm$SF36_SC_MEN_J232=(fm$SF36_VT_J232 + fm$SF36_SF_J232 + fm$SF36_RE_J232 + fm$SF36_MH_J232)/4

fm$SF36_SC_PHY_J232=(fm$SF36_PF_J232 + fm$SF36_RP_J232 + fm$SF36_BP_J232 + fm$SF36_GH_J232)/4

#-----------------------------------------------------------------------

fm$SF36_PF_J301=(fm$SF36_3_1_J301 + fm$SF36_3_2_J301 + fm$SF36_3_3_J301 + fm$SF36_3_4_J301 +
                   fm$SF36_3_5_J301 + fm$SF36_3_6_J301 + fm$SF36_3_7_J301 + fm$SF36_3_8_J301 +
                   fm$SF36_3_9_J301 + fm$SF36_3_10_J301)/10

fm$SF36_RP_J301=(fm$SF36_4_1_J301 + fm$SF36_4_2_J301 + fm$SF36_4_3_J301 + fm$SF36_4_4_J301)/4

fm$SF36_BP_J301=(fm$SF36_7_J301 + fm$SF36_8_J301)/2

fm$SF36_GH_J301=(fm$SF36_1_J301 + fm$SF36_2_J301 + fm$SF36_11_1_J301 +
                   fm$SF36_11_2_J301 + fm$SF36_11_3_J301 + fm$SF36_11_4_J301)/6

fm$SF36_VT_J301=(fm$SF36_9_1_J301 + fm$SF36_9_5_J301 + fm$SF36_9_7_J301 + fm$SF36_9_9_J301)/4

fm$SF36_SF_J301=(fm$SF36_6_J301 + fm$SF36_10_J301)/2

fm$SF36_RE_J301=(fm$SF36_5_1_J301 + fm$SF36_5_2_J301 + fm$SF36_5_3_J301)/3

fm$SF36_MH_J301=(fm$SF36_9_2_J301 + fm$SF36_9_3_J301 + fm$SF36_9_4_J301 + 
                   fm$SF36_9_6_J301 + fm$SF36_8_J301)/5

fm$SF36_SC_MEN_J301=(fm$SF36_VT_J301 + fm$SF36_SF_J301 + fm$SF36_RE_J301 + fm$SF36_MH_J301)/4

fm$SF36_SC_PHY_J301=(fm$SF36_PF_J301 + fm$SF36_RP_J301 + fm$SF36_BP_J301 + fm$SF36_GH_J301)/4

wilcox.test(fm$SF36_SC_MEN, fm$SF36_SC_MEN_J301, paired=T, exact=F)

#####################################################################
#####################################################################
#####################################################################

# INTERVALS

library("asht")

wmwTest(fm$ED47 ~ fm$GP_RANDO, correct=FALSE, RemoveTieAdjustment=TRUE)
str(fm$ED47)
fm$ED47 <- as.numeric(as.character(fm$ED47))
fm$ED47 <- as.numeric(as.character(fm$ED47))

library(bootstrap)
x <- fm$ED47
theta <- function(x){mean(x)}
results <- boott(x,theta,nbootsd=200,nboott=1000)

library("boot")

stat_fx <- function(s, i){ return(mean(s[i])) }

set.seed(585)
bootstrap.success.95 <- 0
shape.parameter <- seq(1, 9, by=0.25)
scale.parameter <- seq(1, 2, by=0.25)

for (i in 1:4000) {
  my.population <- rgamma(100000, sample(shape.parameter, 1), sample(scale.parameter, 1))
  my.sample <- sample(my.population, size = sample(100:500, 1))
  my.bootstrap <- boot(my.sample, stat_fx, R=5000) 
}

library("asht")
set.seed(1000)
theta <- function(x){mean(x)}
abcnonHtest(x, theta, conf.level=0.95, nullValue=0)
