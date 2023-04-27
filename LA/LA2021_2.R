# DATA
getwd()
setwd("P:/CONSULTATION/Lhermitte_Amaury/DATA") # On PC

library("readxl")

la2 <- read_excel("MERP_202107.xlsx", na="NA")

library(tidyverse)

names(la2)

str(la$apresGdS15_postMRA_PEEP_cmH2O)
str(la$T0_PEEP_cmH2O)

la$deltapeep = (la$apresGdS15_postMRA_PEEP_cmH2O - la$T0_PEEP_cmH2O)
str(la$deltapeep)
mean(la$deltapeep, na.rm = T)

table(la$Delta_P_F_sup20)

t.test(la$deltapeep, la$Delta_P_F_sup20, conf.level = 0.95)

