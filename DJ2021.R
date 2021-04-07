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

is.na(dj$IntMU_M0)
table(dj$IntMU_M0)

install.packages("tidyr")
library("tidyr")
dj2 <- tibble(dj)
dj2 %>% replace_na(dj)

dj %>% 
  mutate_at(vars(IntMU_M0, IntMU_M3, IntMU_M6, IntMU_M9), replace_na, 4)

install.packages("dplyr")
library("dplyr")
