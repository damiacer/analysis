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
			
diffmean = abs(m1 - m2)
comsd = sqrt((s1^2 + s2^2)/2)

install.packages("presize")
require("presize")

prec_mean(m = 29.8, sd = 18.4, n = NULL, conf.width = 0.2, conf.level = 0.95)