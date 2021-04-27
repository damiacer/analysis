### folder
getwd()
setwd("P:/CONSULTATION/FMOstéo")
getwd()

### database
fm <- read.csv2("db_missing.csv", header = TRUE, na.string="")
names(fm)
#View(fm)

######################################################################################## 

# MISSING SCORE FUNCTION

missing_scores <- function(x) {
  result = x != "."
  return(result)
}

######################################################################################## 
######################################################################################## 

# MICE IMPUTATION FOR SF36

library('VIM')
library('magrittr')
library('mice')

sf_data <- fm[,c(
#BASELINE
"SF36_1", "SF36_2", "SF36_3_1", "SF36_3_2", "SF36_3_3", "SF36_3_4", "SF36_3_5", "SF36_3_6", 
"SF36_3_7", "SF36_3_8", "SF36_3_9", "SF36_3_10", "SF36_4_1", "SF36_4_2", "SF36_4_3", 
"SF36_4_4", "SF36_5_1", "SF36_5_2", "SF36_5_3", "SF36_6", "SF36_7", "SF36_8", "SF36_9_1",
"SF36_9_2", "SF36_9_3", "SF36_9_4", "SF36_9_5", "SF36_9_6", "SF36_9_7", "SF36_9_8", "SF36_9_9",
"SF36_10", "SF36_11_1", "SF36_11_2", "SF36_11_3", "SF36_11_4",
# 232
"SF36_1_J232", "SF36_2_J232", "SF36_3_1_J232", "SF36_3_2_J232", "SF36_3_3_J232", "SF36_3_4_J232",
"SF36_3_5_J232", "SF36_3_6_J232", "SF36_3_7_J232", "SF36_3_8_J232", "SF36_3_9_J232", "SF36_3_10_J232",
"SF36_4_1_J232", "SF36_4_2_J232", "SF36_4_3_J232", "SF36_4_4_J232", "SF36_5_1_J232",
"SF36_5_2_J232", "SF36_5_3_J232", "SF36_6_J232", "SF36_7_J232", "SF36_8_J232", "SF36_9_1_J232",
"SF36_9_2_J232", "SF36_9_3_J232", "SF36_9_4_J232", "SF36_9_5_J232", "SF36_9_6_J232", "SF36_9_7_J232",
"SF36_9_8_J232", "SF36_9_9_J232", "SF36_10_J232", "SF36_11_1_J232", "SF36_11_2_J232", 
"SF36_11_3_J232", "SF36_11_4_J232",
# 301
"SF36_2_J301", "SF36_3_1_J301", "SF36_3_2_J301", "SF36_3_3_J301", "SF36_3_4_J301", "SF36_3_5_J301",
"SF36_3_6_J301", "SF36_3_7_J301", "SF36_3_8_J301", "SF36_3_9_J301", "SF36_3_10_J301", 
"SF36_4_1_J301", "SF36_4_2_J301", "SF36_4_3_J301", "SF36_4_4_J301", "SF36_5_1_J301", 
"SF36_5_2_J301", "SF36_5_3_J301", "SF36_6_J301", "SF36_7_J301", "SF36_8_J301", 
"SF36_9_1_J301", "SF36_9_2_J301", "SF36_9_3_J301", "SF36_9_4_J301", "SF36_9_5_J301", "SF36_9_6_J301",
"SF36_9_7_J301", "SF36_9_8_J301", "SF36_9_9_J301", "SF36_10_J301", "SF36_11_1_J301",
"SF36_11_2_J301", "SF36_11_3_J301")]

######################################################################################## 


# VISUAL REPRESENTATION OF THE DATABASE 
md.pattern(sf_data)

aggr(sf_data, delimiter = NULL, plot = TRUE)
aggr(sf_data, delimiter = NULL, plot = TRUE)
plot(x = sf_data, 
     col = c("red"),
     numbers = FALSE,
     prop = FALSE, 
     varheight = FALSE,
     only.miss = FALSE, 
     border = par("fg"),
)

marginplot(sf_data[c(3,6)])

# The red box plot on the left shows the distribution of var X 
# the blue box plot shows the distribution of the remaining datapoints
# Likewhise for the red box plots at the bottom of the graph
# if our assumption of MCAR data is correct, then we expect the 
# red and blue box plots to be very similar

tempData_sf <- mice(sf_data,m=5,maxit=50,meth='pmm',seed=500)

# m=5 refers to the number of imputed datasets. Five is the default value
# meth='pmm' refers to the imputation method
# pmm predictive mean matching as imputation method
# methods(mice) for a list of the available imputation methods under 'mice'
summary(tempData_sf)

# check for imputed data as follows
#tempData_sf$imp$SF36_1
#tempData_sf$meth

sf_completedData <- complete(tempData_sf,1)
summary(sf_completedData)
summary(sf_data)

names(sf_completedData)

sf_data <- sf_completedData

fm <- sf_data

View(fm)

######################################################################################## 
######################################################################################## 

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

######################################################################################## 

# scoring the SF36 : https://www.rand.org/health-care/surveys_tools/mos/36-item-short-form/scoring.html

fm$SF36_PF=(fm$SF36_3_1 + fm$SF36_3_2 + fm$SF36_3_3 + fm$SF36_3_4 +
              fm$SF36_3_5 + fm$SF36_3_6 + fm$SF36_3_7 + fm$SF36_3_8 +
              fm$SF36_3_9 + fm$SF36_3_10)/10

fm$SF36_RP=(fm$SF36_4_1 + fm$SF36_4_2 + fm$SF36_4_3 + fm$SF36_4_4)/4

fm$SF36_BP=(fm$SF36_7 + fm$SF36_8)/2

fm$SF36_GH=(fm$SF36_1 + fm$SF36_2 + fm$SF36_11_1 +
              fm$SF36_11_2 + fm$SF36_11_3 + fm$SF36_11_4)/6

fm$SF36_VT=(fm$SF36_9_1 + fm$SF36_9_5 + fm$SF36_9_7 + fm$SF36_9_9)/4

fm$SF36_SF=(fm$SF36_6 + fm$SF36_10)/2

fm$SF36_RE=(fm$SF36_5_1_J232 + fm$SF36_5_2 + fm$SF36_5_3)/3

fm$SF36_MH=(fm$SF36_9_2 + fm$SF36_9_3+ fm$SF36_9_4 + 
              fm$SF36_9_6 + fm$SF36_8)/5

# results for the baseline

fm$SF36_SC_MEN=(fm$SF36_VT + fm$SF36_SF + fm$SF36_RE + fm$SF36_MH)/4
descriptive(fm$SF36_SC_MEN)

fm$SF36_SC_PHY=(fm$SF36_PF + fm$SF36_RP + fm$SF36_BP + fm$SF36_GH)/4
descriptive(fm$SF36_SC_PHY)

#---------------------------------------

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

# result for day 232

fm$SF36_SC_MEN_J232=(fm$SF36_VT_J232 + fm$SF36_SF_J232 + fm$SF36_RE_J232 + fm$SF36_MH_J232)/4
descriptive(fm$SF36_SC_MEN_J232)

fm$SF36_SC_PHY_J232=(fm$SF36_PF_J232 + fm$SF36_RP_J232 + fm$SF36_BP_J232 + fm$SF36_GH_J232)/4
descriptive(fm$SF36_SC_PHY_J232)

#---------------------------------------

fm$SF36_PF_J301=(fm$SF36_3_1_J301 + fm$SF36_3_2_J301 + fm$SF36_3_3_J301 + fm$SF36_3_4_J301 +
                   fm$SF36_3_5_J301 + fm$SF36_3_6_J301 + fm$SF36_3_7_J301 + fm$SF36_3_8_J301 +
                   fm$SF36_3_9_J301 + fm$SF36_3_10_J301)/10

fm$SF36_RP_J301=(fm$SF36_4_1_J301 + fm$SF36_4_2_J301 + fm$SF36_4_3_J301 + fm$SF36_4_4_J301)/4

fm$SF36_BP_J301=(fm$SF36_7_J301 + fm$SF36_8_J301)/2

fm$SF36_GH_J301=(#fm$SF36_1_J301 + 
  fm$SF36_2_J301 + fm$SF36_11_1_J301 +
                   fm$SF36_11_2_J301 + fm$SF36_11_3_J301 #+ fm$SF36_11_4_J301
  )/6

fm$SF36_VT_J301=(fm$SF36_9_1_J301 + fm$SF36_9_5_J301 + fm$SF36_9_7_J301 + fm$SF36_9_9_J301)/4

fm$SF36_SF_J301=(fm$SF36_6_J301 + fm$SF36_10_J301)/2

fm$SF36_RE_J301=(fm$SF36_5_1_J301 + fm$SF36_5_2_J301 + fm$SF36_5_3_J301)/3

fm$SF36_MH_J301=(fm$SF36_9_2_J301 + fm$SF36_9_3_J301 + fm$SF36_9_4_J301 + 
                   fm$SF36_9_6_J301 + fm$SF36_8_J301)/5

# result for day 301

fm$SF36_SC_MEN_J301=(fm$SF36_VT_J301 + fm$SF36_SF_J301 + fm$SF36_RE_J301 + fm$SF36_MH_J301)/4
descriptive(fm$SF36_SC_MEN_J301)

fm$SF36_SC_PHY_J301=(fm$SF36_PF_J301 + fm$SF36_RP_J301 + fm$SF36_BP_J301 + fm$SF36_GH_J301)/4
descriptive(fm$SF36_SC_PHY_J301)

######################################################################################## 

# testing the difference between 232 and 301 and baseline

wilcox.test(fm$SF36_SC_MEN, fm$SF36_SC_MEN_J301, paired=T, exact=F)
wilcox.test(fm$SF36_SC_PHY, fm$SF36_SC_PHY_J301, paired=T, exact=F)

wilcox.test(fm$SF36_SC_MEN, fm$SF36_SC_MEN_J232, paired=T, exact=F)
wilcox.test(fm$SF36_SC_PHY, fm$SF36_SC_PHY_J232, paired=T, exact=F)

######################################################################################## 

# SECONDARY OUTCOME
# DELTA TO THE BASELINE FOR EVERY SF36
# to compare QDSA DELTAS TO BASELINE ACCORDING TO THE GROUP

sf_men232 <- (fm$SF36_SC_MEN_J232 - fm$SF36_SC_MEN)
shapiro.test(sf_men232) # NOTE : H0 CANNOT BE REJECTED P>0.05 H0:POPULATION IS NORMALLY DISTRIBUTED
mean(sf_men232, na.rm=T)
describeBy(sf_men232, fm$GP_RANDO)
t.test(sf_men232, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(sf_men232 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)

sf_phy232 <- (fm$SF36_SC_PHY_J232 - fm$SF36_SC_PHY)
describeBy(sf_phy232, fm$GP_RANDO)
t.test(sf_phy232, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(sf_phy232 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)

sf_men301 <- (fm$SF36_SC_MEN_J301 - fm$SF36_SC_MEN)
describeBy(sf_men301, fm$GP_RANDO)
t.test(sf_men301, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(sf_men301 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)

sf_phy301 <- (fm$SF36_SC_PHY_J301 - fm$SF36_SC_PHY)
describeBy(sf_phy301, fm$GP_RANDO)
t.test(sf_phy301, fm$GP_RANDO, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
wilcox.test(sf_phy301 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)



