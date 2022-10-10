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
N = 589.6508/0.864 
# Taking into account a 20 p100 Lost to Follow-Up
N + (N * 0.20) # = 818.9594

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

N = 440.9463/0.864
N + (N * 0.20) # = 612.4254

#---------------------------------------------------------------

# 3rd = bigger Cohen's d (0.2) with 90% power

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


N = 264.6137/0.864
N + (N * 0.20) # = 367.519

#---------------------------------------------------------------

# 4rd = largest Cohen's d (0.5) with 90% power

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


N = 43.99548/0.864
N + (N * 0.20) # = 61.10483
