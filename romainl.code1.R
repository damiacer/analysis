# download R: https://cran.r-project.org/
# download RSutdio: https://www.rstudio.com/products/rstudio/#rstudio-desktop

getwd()
setwd("/Users/damianocerasuolo/Desktop/UBRC/21_22_CONSULT_MAC/Lafitte_Romain")
#-------------------------------------------------------------------------------
install.packages("readxl")
library("readxl")
install.packages("tidyverse")
library("tidyverse")
#-------------------------------------------------------------------------------
sup <- read_excel("results-survey668447.xlsx", na="N/A")

names(sup)
#View(sup)
#-------------------------------------------------------------------------------
# tableau descriptif

install.packages("tableone")
library("tableone")

dput(names(prof))

variables = c("var1", "var2", "var3")

factors = c("var1", "var2", "var3")


descriptive = CreateTableOne(vars = variables, data = sup, factorVars = factors, includeNA = TRUE)
print(descriptive, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
