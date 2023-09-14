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