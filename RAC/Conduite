require("readxl")
require("tidyverse")
#require("plyr")
require("lubridate")
require("here")


c0 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "conduite0.xlsx"))
dim(c0)

c3 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                     "conduite3.xlsx"))
dim(c3)

c5 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "conduite5.xlsx"))
dim(c5)

c7 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "conduite7.xlsx"))
dim(c7)

a0 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "DMO_Annee0.xls"))
a3 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "DMO_Annee3.xls"))
a4 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "DMO_Annee4.xls"))
a5 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "DMO_Annee5.xls"))
a6 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "DMO_Annee6.xls"))
a7 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "DMO_Annee7.xls"))
a8 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "DMO_Annee8.xls"))
a9 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                      "DMO_Annee9.xls"))
a10 <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                       "DMO_Annee10.xls"))
pro <- read_excel(here("Desktop", "UBRC", "22_23_CONSULT_MAC", "Rat_AnneChristine", "DATA", 
                       "DMO_Protheses.xls"))

#-VISIT VARIABLE------------------------------------------------------------------
# REPLACE VARIABLES

a0$visit.0 = a0$VISIT
a0 = subset(a0, select = -c(VISIT))

a3$visit.3 = a3$VISIT
a3 = subset(a3, select = -c(VISIT))

a4$visit.4 = a4$VISIT
a4 = subset(a4, select = -c(VISIT))

a5$visit.5 = a5$VISIT
a5 = subset(a5, select = -c(VISIT))

a6$visit.6 = a6$VISIT
a6 = subset(a6, select = -c(VISIT))

a7$visit.7 = a7$VISIT
a7 = subset(a7, select = -c(VISIT))

a8$visit.8 = a8$VISIT
a8$IdCohorte = a8$IDCOHORTE
a8 = subset(a8, select = -c(IDCOHORTE, VISIT))

a9$IdCohorte = a9$IDCOHORTE 

#-MERGE---------------------------------------------------------------------------
#-LIST TO MERGE-------------------------------------------------------------------
df_list = list(a0.v, a3.v, a4.v, a5.v, a6.v, a7.v, a8.v, a9.v, a10.v, pro.v)

#-ACTUAL MERGE-------------------------------------------------------------------
#library(tidyverse)
all.v <- df_list %>% reduce(full_join, by='IdCohorte')
dim(all.v)
dim(a0)
names(all.v)
a9$visit.9 = a9$VISIT
a9 = subset(a9, select = -c(IDCOHORTE, VISIT))

a10$visit.10 = rep(10, times = 462)
