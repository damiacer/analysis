#-WORKSPACE----
getwd()
setwd("P:/CONSULTATION/DIADIDEAL")

#-PACKAGES----

#install.packages("readxl")
require("readxl")

dd <- read_excel("DIADIDEAL_Export_final_global_2023-11-20.xlsx")
names(dd)
