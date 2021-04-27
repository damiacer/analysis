### folder
getwd()
setwd("P:/CONSULTATION/FMOstéo")
getwd()

### database
fm <- read.csv2("db_missing.csv", header = TRUE, na.string="")
names(fm)
#View(fm)

########################################################################################

install.packages("bootstrap")
library("bootstrap")

########################################################################################

# notes on the project

# the primary objective is the difference between day 0 and day 232 for pain
# these values have not been collected
# only scores for day 5 and 215 have been collected

# a "UNK" value in the EVA_SCORE_J89 var is OUT OF THE BLUE. re-coding is mandatory... :/

str(fm$EVA_SCORE_J89)
table(fm$EVA_SCORE_J89)

# the following lines are not necessary if the database run is "db_missing"
# fm$EVA_SCORE_J89_2<-ifelse(fm$EVA_SCORE_J89=="UNK",NA,fm$EVA_SCORE_J89)
# table(fm$EVA_SCORE_J89_2, fm$EVA_SCORE_J89)
# fm$EVA_SCORE_J89<-fm$EVA_SCORE_J89_2
# str(fm$EVA_SCORE_J89)
# fm$EVA_SCORE_J89<-as.integer(fm$EVA_SCORE_J89)

########################################################################################

# new datasets

fm$EVA_SCORE_J301 <- fm$SC_DOL_MM_J301
fm$EVA_SCORE_J232 <- fm$SC_DOL_MM_J232
fm$ID 		<- fm$Study.Subject.ID

# only eva var
eva_data <- fm[,c("EVA_SCORE_J5", "EVA_SCORE_J47", "EVA_SCORE_J89", 
                  "EVA_SCORE_J131", "EVA_SCORE_J173", "EVA_SCORE_J215", "EVA_SCORE_J232",
		            	"EVA_SCORE_J301")]

# DESCRIPTIVE 

descriptive(eva_data$EVA_SCORE_J5)
descriptive(eva_data$EVA_SCORE_J47)
descriptive(eva_data$EVA_SCORE_J89)
descriptive(eva_data$EVA_SCORE_J131)
descriptive(eva_data$EVA_SCORE_J173)
descriptive(eva_data$EVA_SCORE_J215)
descriptive(eva_data$EVA_SCORE_J232)
descriptive(eva_data$EVA_SCORE_J301)


########################
#CONSIDER RUNNING
########################

eva_data <- fm[,c("GP_RANDO", "ID", "EVA_SCORE_J5", "EVA_SCORE_J47", "EVA_SCORE_J89", 
                  "EVA_SCORE_J131", "EVA_SCORE_J173", "EVA_SCORE_J215", "EVA_SCORE_J232",
		            	"EVA_SCORE_J301")]


install.packages("psych")
library("psych")

describe.by(eva_data, eva_data$GP_RANDO)


 install.packages("dplyr")
 library("dplyr")

########################################################################################

eva_data$E_5 	  <- eva_data$EVA_SCORE_J5
eva_data$E_47 	<- eva_data$EVA_SCORE_J47
eva_data$E_89 	<- eva_data$EVA_SCORE_J89
eva_data$E_131 	<- eva_data$EVA_SCORE_J131
eva_data$E_173 	<- eva_data$EVA_SCORE_J173
eva_data$E_215 	<- eva_data$EVA_SCORE_J215
eva_data$E_232 	<- eva_data$EVA_SCORE_J232
eva_data$E_301 	<- eva_data$EVA_SCORE_J301

eva_data<-select(eva_data,-c(EVA_SCORE_J5, EVA_SCORE_J47, EVA_SCORE_J89, 
                             EVA_SCORE_J131, EVA_SCORE_J173, EVA_SCORE_J215, EVA_SCORE_J232,
                             EVA_SCORE_J301))

names(eva_data)

########################################################################################

# PRIMARY OUTCOME
# to compare EVA DELTAS TO BASELINE ACCORDING TO THE GROUP

eva_data$ED47 <- (eva_data$E_47 - eva_data$E_5)
shapiro.test(eva_data$ED47)
t.test(eva_data$ED47, eva_data$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
#wilcox.test(eva_data$ED47 ~ eva_data$GP_RANDO, paired=F, exact=F, correct=F)
eva_data$ED47<-as.numeric(as.character(eva_data$ED47))
x <- eva_data$ED47
theta <- function(x){mean(x)}
results <- boott(x,theta,nbootsd=200,nboott=1000)

eva_data$ED89 <- (eva_data$E_89 - eva_data$E_5)
shapiro.test(eva_data$ED89)
t.test(eva_data$ED89, eva_data$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
#wilcox.test(eva_data$ED89 ~ eva_data$GP_RANDO, paired=F, exact=F, correct=F)

eva_data$ED131 <- (eva_data$E_131 - eva_data$E_5)
shapiro.test(eva_data$ED131)
t.test(eva_data$ED131, eva_data$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
#wilcox.test(eva_data$ED131 ~ eva_data$GP_RANDO, paired=F, exact=F, correct=F)

eva_data$ED173 <- (eva_data$E_173 - eva_data$E_5)
shapiro.test(eva_data$ED173)
t.test(eva_data$ED173, eva_data$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(eva_data$ED173 ~ eva_data$GP_RANDO, paired=F, exact=F, correct=F)

eva_data$ED215 <- (eva_data$E_215 - eva_data$E_5)
shapiro.test(eva_data$ED215)
t.test(eva_data$ED215, eva_data$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
#wilcox.test(eva_data$ED215 ~ eva_data$GP_RANDO, paired=F, exact=F, correct=F)

eva_data$ED232 <- (eva_data$E_232 - eva_data$E_5)
shapiro.test(eva_data$ED232)
t.test(eva_data$ED232, eva_data$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
#wilcox.test(eva_data$ED232 ~ eva_data$GP_RANDO, paired=F, exact=F, correct=F)

eva_data$ED301 <- (eva_data$E_301 - eva_data$E_5)
shapiro.test(eva_data$ED301)
t.test(eva_data$ED301, eva_data$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(eva_data$ED301 ~ eva_data$GP_RANDO, paired=F, exact=F, correct=F, conf.int=T, conf.level=0.95)


descriptive(eva_data$ED47)
descriptive(eva_data$ED89)
descriptive(eva_data$ED131)
descriptive(eva_data$ED173)
descriptive(eva_data$ED215)
descriptive(eva_data$ED232)
descriptive(eva_data$ED301)

eva_delta<-select(eva_data,c(GP_RANDO, ED47, ED89, ED131, ED173, ED215, ED232, ED301))
names(eva_delta)
describeBy(eva_delta, eva_delta$GP_RANDO)

########################################################################################

# wilcoxon signed rank test
# wilcox.test(fm$EVA_SCORE_J5, fm$EVA_SCORE_J301, paired=T, exact=F)

# calculate the effect size
# the effect size is given by the absolute (positive) standardised test statistic z by 
# the square root of the number of pairs (28 in this study)
# first: calculate the wilcoxon signed rank test

#	wiltest<-wilcox.test(fm$EVA_SCORE_J5, fm$EVA_SCORE_J232, paired=T, exact=F)
# second: calculate the standardised z statistics
#	Zstat <- qnorm(wiltest$p.value/2)
# third: calculate the effect size
#	abs(Zstat/sqrt(28))
#[1] 0.2673294 the effect size is small according to cohen's classification


########################################################################################


# MICE IMPUTATION

# packages for missing values installation
# visit rmisstastic.netlify.app/rpkg/ for more references
# install.packages('VIM')
# install.packages('magrittr')
# install.packages('mice')

library('VIM')
library('magrittr')
library('mice')

# THE FOLLOWING LINES CAN BE USED TO DESCRIBE THE DABASE 
# BUT ARE NOT REQUIRED FOR THE ANALYSE NOR THE IMPUTATION
# missing values
# functions for missing values

missing_scores <- function(x) {
  result = x != "."
  return(result)
}

table(fm$EVA_SCORE_J5)
missing_scores(fm$EVA_SCORE_J89)

# VISUAL REPRESENTATION OF THE DATABASE 
md.pattern(eva_data)

aggr(eva_data, delimiter = NULL, plot = TRUE)
aggr(eva_data, delimiter = NULL, plot = TRUE)
plot(x = eva_data, 
     col = c("red"),
     numbers = FALSE,
     prop = FALSE, 
     varheight = FALSE,
     only.miss = FALSE, 
     border = par("fg"),
)

marginplot(eva_data[c(3,6)])

# The red box plot on the left shows the distribution of var X 
# the blue box plot shows the distribution of the remaining datapoints
# Likewhise for the red box plots at the bottom of the graph
# if our assumption of MCAR data is correct, then we expect the 
# red and blue box plots to be very similar

tempData <- mice(eva_data,m=5,maxit=50,meth='pmm',seed=500)
# m=5 refers to the number of imputed datasets. Five is the default value
# meth='pmm' refers to the imputation method
# pmm predictive mean matching as imputation method
# methods(mice) for a list of the available imputation methods under 'mice'
summary(tempData)

# check for imputed data as follows
tempData$imp$E_215
tempData$meth

eva_completedData <- complete(tempData,1)
summary(eva_completedData)

names(eva_completedData)
xyplot(tempData,E_5~ + E_47 + E_89 + 
		E_131 + E_173 + E_215 + E_232 + E_301, pch=18,cex=1)
# does the shape of the magenta points (imputed) 
# match the shape of the blue ones (observed) 
# the matching shape tells us that the imputed values are indeed ?plausible values?

densityplot(tempData)
# the density of the imputed data for each imputed dataset is showed in magenta 
# while the density of the observed data is showed in blue
# under our previous assumptions we expect the distributions to be similar

stripplot(tempData, pch = 20, cex = 1.2)

# more about pooling, here: https://datascienceplus.com/imputing-missing-data-with-r-mice-package/


########################################################################################

# VIM IMPUTATION 

# imputation for missing data with VIM

#print("before imputation")
#summary(eva_data)

# IMPUTATION UNDER VIM 
# for more: http://127.0.0.1:23169/library/VIM/html/kNN.html
# "kNN" = k-Nearest Neighbour Imputation based on a variation of the Gower Distance for numerical, 
# categorical, ordered and semi-continous variables
#eva_imput<-kNN(eva_data)

#print("after imputation")
#summary(eva_imput)


########################################################################################

# SIMPUTATION 


#install.packages('simputation')
#library('simputation')

	# nonlinear models
	# https://cran.r-project.org/web/packages/gnm/gnm.pdf (page 31)
	# https://warwick.ac.uk/fac/sci/statistics/staff/academic-research/firth/software/gnm/gnmoverview_0.9-6.pdf
#	install.packages('gnm')
#	library('gnm')

#names(eva_data)
#str(eva_data$EVA_SCORE_J5)

#day_232 = lm(E_232 ~ E_5 + E_47 + E_89 + E_131 + E_173 + E_215 + E_301, data=eva_data)
#summary(day_232)
#eva_data_i<-impute(eva_data, E_232 ~ day_232)
#summary(eva_data_i)
#summary(eva_data)

#wilcox.test(eva_data$EVA_SCORE_J5, eva_data$EVA_SCORE_J215, paired=T, exact=F)
#wilcox.test(eva_data_i$EVA_SCORE_J5, eva_data_i$EVA_SCORE_J215, paired=T, exact=F)

########################################################################################
########################################################################################

summary(eva_completedData)

# Wilcoxon test for paired data
# is there any difference between EVA measured at the beginning and the end? 
cor.test(eva_completedData$E_5, eva_completedData$E_232, method="spearman")

# the primary outcome is the EVA delta between the two groups
# how are groups identified? 
# "GP_RANDO"
names(fm)
table(fm$GP_RANDO)

eva_completedData$eva_delta <-(eva_completedData$E_232 - eva_completedData$E_5)
fm$eva_delta <- eva_completedData$eva_delta

wilcox.test(fm$eva_delta ~ fm$GP_RANDO, paired=F, exact=F, correct=F)
hist(fm$eva_delta)
shapiro.test(fm$eva_delta)

# the p-value > 0.05 implying that the distribution of the data are not significantly 
# different from normal distribution
# we can assume the normality
# in other terms -> H0: normal distribution, H1: non normal distribution

t.test(fm$eva_delta ~ fm$GP_RANDO, var.equal=FALSE, paired=FALSE, conf.level=0.95)
t.test(fm$eva_delta, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
# var.equal: a logical variable indicating whether to treat the two variances as being equal. 
# if TRUE then the pooled variance is used to estimate the variance otherwise the Welch test is used

########################################################################################

descriptive(eva_completedData$E_5)
descriptive(eva_completedData$E_47)
descriptive(eva_completedData$E_89)
descriptive(eva_completedData$E_131)
descriptive(eva_completedData$E_173)
descriptive(eva_completedData$E_215)
descriptive(eva_completedData$E_232)
descriptive(eva_completedData$E_301)

describe.by(eva_completedData, fm$GP_RANDO)

########################################################################################
########################################################################################

### DELTAS
# variations of EVA after each visit compared to inclusion
# primary objective = comparison of EVA delta between each group

eva_completedData$ED47 <- (eva_completedData$E_47 - eva_completedData$E_5)
shapiro.test(eva_completedData$ED47)
wilcox.test(eva_completedData$ED47 ~ fm$GP_RANDO, paired=F, exact=F, correct=F, 
            confint.merMod(method = "boot", boot.type = "perc", object = "boot.ci"))
t.test(eva_completedData$ED47, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
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


eva_completedData$ED89 <- (eva_completedData$E_89 - eva_completedData$E_5)
shapiro.test(eva_completedData$ED89)
t.test(eva_completedData$ED89, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
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

eva_completedData$ED131 <- (eva_completedData$E_131 - eva_completedData$E_5)
shapiro.test(eva_completedData$ED131)
t.test(eva_completedData$ED131, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
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

eva_completedData$ED173 <- (eva_completedData$E_173 - eva_completedData$E_5)
shapiro.test(eva_completedData$ED173)
t.test(eva_completedData$ED173, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
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
  

eva_completedData$ED215 <- (eva_completedData$E_215 - eva_completedData$E_5)
shapiro.test(eva_completedData$ED215)
t.test(eva_completedData$ED215, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, 
       conf.level=0.95)
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

eva_completedData$ED232 <- (eva_completedData$E_232 - eva_completedData$E_5)
shapiro.test(eva_completedData$ED215)
t.test(eva_completedData$ED232, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(eva_completedData$ED215 ~ fm$GP_RANDO, paired=F, exact=F, correct=F, conf.int = TRUE, conf.level = 0.95)
  x <- eva_completedData$ED232 
  theta <- function(x){mean(x)}
  results <- boott(x,theta,nbootsd=200,nboott=1000)
  results
  
  x <- eva_completedData$ED232
  theta <- function(p,x) {sum(p*x)/sum(p)}
  abcnonHtest(x, theta, conf.level=0.95, nullValue=0)
  abcnon(x, theta)

eva_completedData$ED301 <- (eva_completedData$E_301 - eva_completedData$E_5)
shapiro.test(eva_completedData$ED301)
t.test(eva_completedData$ED301, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(eva_completedData$ED301 ~ fm$GP_RANDO, paired=F, exact=F, correct=F, conf.level = 0.95, conf.int = T)
  x <- eva_completedData$ED301
  theta <- function(x){mean(x)}
  results <- boott(x,theta,nbootsd=200,nboott=1000)
  results
  
  x <- eva_completedData$ED301
  theta <- function(p,x) {sum(p*x)/sum(p)}
  abcnonHtest(x, theta, conf.level=0.95, nullValue=0)
  abcnon(x, theta)


descriptive(eva_completedData$ED47)
descriptive(eva_completedData$ED89)
descriptive(eva_completedData$ED131)
descriptive(eva_completedData$ED173)
descriptive(eva_completedData$ED215)
descriptive(eva_completedData$ED232)
descriptive(eva_completedData$ED301)

edmice<-select(eva_completedData,c(ED47, ED89, ED131, ED173, ED215, ED232, ED301))

describeBy(edmice, fm$GP_RANDO)


########################################################################################
########################################################################################

# LME4 AND NONLINEAR MIXED MODEL BY FUNCTION NLMER 


# REFERENCES FOR MIXED MODELS AND LME4 PACKAGE
# https://stats.idre.ucla.edu/other/mult-pkg/introduction-to-generalized-linear-mixed-models/

install.packages("lme4")	# for linear and nonlinear mixed models
# for more references see :
# https://www.researchgate.net/publication/321692394_Nonlinear_Mixed-Effects_Modeling_Programs_in_R
# package 'seamix' is not available for R 4.0
libary("lme4")			# use the function lmer for fitting nonlinear model
# (page 86 on cran pdf) 
# see the package on cran here:
# https://cran.r-project.org/web/packages/lme4/lme4.pdf

startvec <- c(Asym = 200, xmid = 725, scal = 350)
nm1 <- nlmer(circumference ~ SSlogis(age, Asym, xmid, scal) ~ Asym|Tree,Orange, start = startvec)