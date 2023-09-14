install.packages("pwr")
library("pwr")

install.packages("MESS")
library("MESS")


power_t_test(
  n = NULL,
  delta = 13.75197,
  sd = 16.33323,
  sig.level = 0.05,
  power = 0.9,
  ratio = 5,
  sd.ratio = 2.3846,
  type = c("two.sample"),
  alternative = c("two.sided"),
  df.method = c("welch"#, "classical"
),
  strict = TRUE
)


#    Two-sample t test power calculation with unequal sample sizes and unequal variances 
#
#              n = 32.22229, 161.11147
#          delta = 13.75197
#             sd = 16.33323, 38.94822
#      sig.level = 0.05
#          power = 0.9
#    alternative = two.sided
#
#NOTE: n is vector of number in each group
#
#> 32.22229+161.11147
#[1] 193.3338
#> 161.1114/32.2222
#[1] 5.000012
