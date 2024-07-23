install.packages("pwr")
require("pwr")

m1 = 27.0
s1 = 18.3
m2 = 29.8
s2 = 18.4

d = abs((m1 - m2) / sqrt((s1^2 + s2^2)/2))

pwr.t.test(n = NULL, 
			d = d, 
			sig.level = 0.05, 
			power = 0.8,
			type = "paired",
			alternative = "two.sided"
			)

#      Paired t test power calculation 
#
#              n = 339.0313
#              d = 0.152588
#      sig.level = 0.05
#          power = 0.8
#    alternative = two.sided
#
#NOTE: n is number of *pairs*
			

install.packages("presize")
require("presize")

prec_mean(m = 29.8, sd = 18.4, n = NULL, conf.width = 0.2, conf.level = 0.95)

#     sample size for mean 
#
#  mean   sd        n conf.width conf.level  lwr  upr
#  29.8 18.4 130058.9        0.2       0.95 29.7 29.9
