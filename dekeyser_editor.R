# FOLDER

getwd()
setwd("P:/CONSULTATION/Dekeyser_Jérémie") # On PC
setwd("/Users/damianocerasuolo/Desktop/UBRC/2021_CONSULT/Dekeyser_Jérémie") # On Mac

# PACKAGES
install.packages("readxl")
library("readxl")

install.packages("tableone")
library("tableone")

install.packages("desctable")
library("desctable")

# DATA
dj <- read_excel("DEKEYSER_data.xlsx", na=".")

names(dj)
