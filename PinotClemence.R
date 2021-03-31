# FOLDER

getwd()
setwd("P:/CONSULTATION/Pinot_Clemence") # On PC

# PACKAGES
library("readxl")

install.packages("tableone")
library("tableone")

install.packages("desctable")
library("desctable")

# DATA
cp <- read_excel("CP_data.xlsx", na=".")

names(cp)

#################################################################

# PEOPLE HAVING 1, 2 OR 3 FOLLOW-UP

table(cp$structures_suivi1)
table(cp$structures_suivi2)
table(cp$structures_suivi3)

cp$structuresuivi[cp$structures_suivi1 != "."] <- "1"
cp$structuresuivi[cp$structures_suivi2 != "."] <- "2"
cp$structuresuivi[cp$structures_suivi3 != "."] <- "3"
table(cp$structuresuivi)

cp$redac[cp$redacteur=="EMSP"] <- "EMSP"
cp$redac[cp$redacteur=="IDE"] <- "IDE"
cp$redac[cp$redacteur=="IDE SP"] <- "IDE"
cp$redac[cp$redacteur=="IDE HAD"] <- "IDE"
cp$redac[cp$redacteur=="HAD"] <- "HAD"
cp$redac[cp$redacteur=="Medecin coordi.teur FAM"] <- "Med coord"
cp$redac[cp$redacteur=="Medecin coordi.teur EHPAD"] <- "Med coord"
cp$redac[cp$redacteur=="Medecin hospitalier soins palliatif"] <- "Med hosp"
cp$redac[cp$redacteur=="Medecin hospitalier"] <- "Med hosp"
cp$redac[cp$redacteur=="Reseau"] <- "Reseau"
cp$redac[cp$redacteur=="Reseau (IDE coordi.trice)"] <- "Reseau"
cp$redac[cp$redacteur=="Medecin traitant"] <- "Med traitant"
table(cp$redac)
str(cp$redac)

#################################################################

# TABLEONE
CreateTableOne(data = cp) 

# VARIABLE NAMES
dput(names(cp))

# VECTORS OF VARIABLES TO SUMMARIZE
# ALL VARIABLES
myVars <- 	c("redac", "connaissancediagn", "structuresuivi", 
             "structures_suivi1", "accord_fiche", "hosp_noprogrambinaire") 

# ONLY CATEGORIAL VARIABLES THAT NEED TRANSFORMATION
catVars <- c("redca", "connaissancediagn", "structuresuivi", 
             "structures_suivi1", "accord_fiche", "hosp_noprogrambinaire") 

# CREATE A TABLEONE OBJECT
  tab1 <- CreateTableOne(vars = myVars, data = cp, factorVars = catVars)
# SHOW ALL LEVELS OF A CATEGORIAL VARIABLE 
  print(tab1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

# CREATE A TABLE BY STRATIFICATION
# ADD A TEST (FALSE TO AVOID TESTING) 
# ADD STRATA TO CREATE A UNIVARIATE ANALYSIS
  tab2 <- CreateTableOne(vars = myVars, data = cp, factorVars = catVars, test = TRUE, strata = "accord_fiche")
# SHOW ALL LEVELS BY STRATA
  print(tab2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

# CHI SQUARE TESTS
cp$redac <- as.factor(as.character(cp$redac))
cp$accord_fiche <- as.factor(as.character(cp$accord_fiche))

tab1 <- table(cp$redac,cp$accord_fiche)
fisher.test(cp$redac,cp$accord_fiche)

#----------------------------------------------------------------
