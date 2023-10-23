install.packages("pwr")
library("pwr")

install.packages("MESS")
library("MESS")


power_t_test(
  n = NULL,
  delta = (49.39605-32.64408),
  sd = 16.33323,
  sig.level = 0.05,
  power = 0.9,
  ratio = 1,
  sd.ratio = (25.22202/16.33323),
  type = c("two.sample"),
  alternative = c("two.sided"),
  df.method = c("welch"#, "classical"
),
  strict = TRUE
)

#Two-sample t test power calculation with unequal variances 
#
#n = 34.96209, 34.96209
#delta = 16.75197
#sd = 16.33323, 25.22202
#sig.level = 0.05
#power = 0.9
#alternative = two.sided

#NOTE: n is number in *each* group

34.96209+34.96209 # = 69.92418
69.92418/0.864 # = 80.93076
80.93076 * 0.20 # = 16.18615
80.93076 + 16.18615 # = 97.11691
