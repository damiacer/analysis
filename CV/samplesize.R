library("pwr")

# 1st = extremely small Cohen's d and 90% power

pwr.t.test(d=0.1337089,
	power=0.9,
	sig.level=0.05,
	type="one.sample",
	alternative="two.sided")

# One-sample t test power calculation 
#
#              n = 589.6508
#              d = 0.1337089
#      sig.level = 0.05
#          power = 0.9
#    alternative = two.sided

# ARE (Asymptotic Relative Efficiency)
589.6508/0.846 
N = 696.9868
# Lost to Follow-Up
N + (N * 0.20) # = 836.3842

#---------------------------------------------------------------

# 2nd = extremely small Cohen's d and 80% power

pwr.t.test(d=0.1337089,
	power=0.8,
	sig.level=0.05,
	type="one.sample",
	alternative="two.sided")

# One-sample t test power calculation 
#
#              n = 440.9463
#              d = 0.1337089
#      sig.level = 0.05
#          power = 0.8
#    alternative = two.sided

440.9463/0.846
N = 521.2131
N + (N * 0.20) # = 625.4557

#---------------------------------------------------------------

# 3rd = bigger Cohen's d (0.2) with a 90% power

pwr.t.test(d=0.2,
	power=0.9,
	sig.level=0.05,
	type="one.sample",
	alternative="two.sided")

# One-sample t test power calculation 
#
#              n = 264.6137
#              d = 0.2
#      sig.level = 0.05
#          power = 0.9
#    alternative = two.sided

264.6137/0.846
N = 312.7822
N + (N * 0.20) # = 375.3386

#---------------------------------------------------------------

# 3rd = largest Cohen's d (0.5) with a 90% power

pwr.t.test(d=0.5,
	power=0.9,
	sig.level=0.05,
	type="one.sample",
	alternative="two.sided")

# One-sample t test power calculation 
#
#              n = 43.99548
#              d = 0.5
#      sig.level = 0.05
#          power = 0.9
#    alternative = two.sided

43.99548/0.846
N = 52.00411
N + (N * 0.20) # = 62.40493
