# WARNING
# THIS CODE IS MEANT TO RUN AFTER THE IMPUTATION CODE

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
t.test(fm$eva_delta ~ fm$GP_RANDO, var.equal=FALSE, paired=FALSE, conf.level=0.95)

### DELTAS
# variations of EVA after each visit compared to inclusion

fm$ED47 <- (eva_completedData$E_47 - eva_completedData$E_5)
wilcox.test(fm$ED47 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)

fm$ED89 <- (eva_completedData$E_89 - eva_completedData$E_5)
wilcox.test(fm$ED89 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)

fm$ED131 <- (eva_completedData$E_131 - eva_completedData$E_5)
wilcox.test(fm$ED131 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)

fm$ED173 <- (eva_completedData$E_173 - eva_completedData$E_5)
wilcox.test(fm$ED173 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)

fm$ED215 <- (eva_completedData$E_215 - eva_completedData$E_5)
wilcox.test(fm$ED215 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)

fm$ED301 <- (eva_completedData$E_301 - eva_completedData$E_5)
wilcox.test(fm$ED301 ~ fm$GP_RANDO, paired=F, exact=F, correct=F)


