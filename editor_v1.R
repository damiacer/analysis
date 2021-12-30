### FOLDER

getwd()
setwd("P:/CONSULTATION/Humbert_Xavier/IDACO_GUBBIO/IDACO/IDACO/databases")
getwd()

#------------------------------------------------------------------------------

### PACKAGES

#install.packages("readxl")
library("readxl")

install.packages("desctable")
library("desctable")

#install.packages("tableone")
library("tableone")

#------------------------------------------------------------------------------

### DATA

ida <- read_excel("IDACO.xlsx", na="")

################################################################################

### DATA VIZ

names(ida)
dim(ida)
#6824  56

################################################################################

### FUNCTIONS

num <- function(x){
  result1 = as.numeric(as.character(x))
  #return(result1)
}

################################################################################

### DATA DESCRITPION

# DESCTABLE

stats_auto(ida)

#------------------------------------------------------------------------------

# TABLEONE

dput(names(ida))

num(ida$AGE)

ida$AGE = num(ida$AGE)
ida$BMI = num(ida$BMI)

variables = c("CENTER", "SEX", "AGE", "RACE", "BMI", "SMK_C", "SMK_Q", 
              "SBPSI1", "SBPSI2", "SBPSI3", "SBPSI4", "SBPSI5", "DIFFSBP3", 
              "OWCET3", "SBPAV", "VAR_SD", "VAR_CV", "C19", "C20", "C21", "C22", 
              "C23", "C24", "C25", "PRSI", "TRT", "HCV", "HDM", "BTF", "BGLC", 
              "CHOL_TOT", "CHOL_HDL", "CREA", "C35", "C36", "C37", "C38", "C39", 
              "C40", "C41", "C42", "C43", "FUY", "FALL", "FCV", "ACBV_R", "TACBV_R", 
              "ACOR_R", "TACOR_R", "ACOR_R2", "TACOR_R2", "ACV_R", "TACV_R", 
              "ACV_R2", "TACV_R2")

catvars = c("CENTER", "SEX", "RACE", "SMK_C", "SMK_Q", 
            "SBPSI1", "SBPSI2", "SBPSI3", "SBPSI4", "SBPSI5", "DIFFSBP3", 
            "OWCET3", "SBPAV", "VAR_SD", "VAR_CV", "C19", "C20", "C21", "C22", 
            "C23", "C24", "C25", "PRSI", "TRT", "HCV", "HDM", "BTF", "BGLC", 
            "CHOL_TOT", "CHOL_HDL", "CREA", "C35", "C36", "C37", "C38", "C39", 
            "C40", "C41", "C42", "C43", "FUY", "FALL", "FCV", "ACBV_R", "TACBV_R", 
            "ACOR_R", "TACOR_R", "ACOR_R2", "TACOR_R2", "ACV_R", "TACV_R", 
            "ACV_R2", "TACV_R2")


idatab1 = CreateTableOne(vars = variables, data = ida, factorVars = catvars)
print(idatab1, showAllLevels = TRUE, quote = FALSE, nospaces = FALSE)
