x=c(57,50,30,40,12,11,60,100,45,67,44,57,29,14,91,52)
mean(x)
sd(x)

y=c(40,33,0,33,9,0,0,82,37,33,35,33,0,13,40,41)
sd(y)
sd(y)

M1  = mean(x) # Mean for sample 1
M2  = mean(y) # Mean for sample 2
S1  = sd(x) # Std dev for sample 1
S2  = sd(y) # Std dev for sample 2

Cohen.d = (M1 - M2)/sqrt(((S1^2) + (S2^2))/2)  

library(pwr)

pwr.t.test(
  n = NULL,                   # Observations in _each_ group
  d = Cohen.d,            
  sig.level = 0.05,           # Type I probability
  power = 0.90,               # 1 minus Type II probability
  type = "two.sample",        # Change for one- or two-sample
  alternative = "two.sided")

#     Two-sample t test power calculation 
#
#              n = 29.20669
#              d = 0.8630696
#      sig.level = 0.05
#          power = 0.9
#    alternative = two.sided
#
#NOTE: n is number in *each* group

29.20669*2 # = 58.41338
58.41338/0.864 # = 67.60808 ~ 68

