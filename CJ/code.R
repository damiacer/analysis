getwd()
setwd("P:/CONSULTATION/Jacquenet_Cyril")

library("readxl")

si <- read_excel("cj_data.xlsx", na = "")
si2 <- read_excel("cj_data2.xlsx", na = "")
names(si2)
count(si)

library("tidyverse")

str(si$Malaise)
str(si$Sexe)

si <- si %>%
  mutate(effect = case_when(
    Bouche.seche == 2 | Bouche.seche == 3 ~ "1",
    Constipation == 2 | Constipation == 3 ~ "1",
    Palpitations == 2 | Palpitations == 3 ~ "1",
    Malaise == 2 | Malaise == 3 ~ "1",
    Insomnie == 2 | Insomnie == 3 ~ "1",
    Tb.orgasme == 2 | Tb.orgasme == 3 ~ "1",
    Besoin.bouger == 2 | Besoin.bouger == 3 ~ "1",
    Aug.appetit == 2 | Aug.appetit == 3 ~ "1",
    Sens.corp..Inh == 2 | Sens.corp..Inh == 3  ~ "1",
    Nausees == 2 | Nausees == 3 ~ "1",
    Vomissements == 2 | Vomissements == 3 ~ "1",
    Baillements == 2 | Baillements == 3 ~ "1",
    Somnolence == 2 | Somnolence == 3 ~ "1",
    (Tb.erection == 2 | Tb.erection == 3) & Sexe == "H" ~ "1",
    Tb.equilibre == 2 | Tb.equilibre == 3 ~ "1",
    Prise.de.poids == 2 | Prise.de.poids == 3 ~ "1",
    Douleurs.D_C == 2 | Douleurs.D_C == 3 ~ "1",
    Diarrhee == 2 | Diarrhee == 3 ~ "1",
    Vertiges == 2 | Vertiges == 3 ~ "1",
    Maux.de.tête == 2 | Maux.de.tête == 3 ~ "1",
    Tremblements == 2 | Tremblements == 3 ~ "1",
    Tb.Libido == 2 | Tb.Libido == 3 ~ "1",
    Anxiete == 2 | Anxiete == 3 ~ "1",
    Demotivation == 2 | Demotivation == 3 ~ "1",
    Diminution.emotion == 2 | Diminution.emotion == 3 ~ "1",
    Acouphenes. == 2 | Acouphenes. == 3  ~ "1"
  ))

str(si$effect)
si$effect[is.na(si$effect)] <- "0"
table(si$effect)

table(si$Nbre.atcd)
prop.table(table(si$Nbre.atcd))*100

si <- si %>%
  mutate(effectI = case_when(
    Bouche.seche == 3 ~ "1",
    Constipation == 3 ~ "1",
    Palpitations == 3 ~ "1",
    Malaise == 3 ~ "1",
    Insomnie == 3 ~ "1",
    Tb.orgasme == 3 ~ "1",
    Besoin.bouger == 3 ~ "1",
    Aug.appetit == 3 ~ "1",
    Sens.corp..Inh == 3  ~ "1",
    Nausees == 3 ~ "1",
    Vomissements == 3 ~ "1",
    Baillements == 3 ~ "1",
    Somnolence == 3 ~ "1",
    Tb.erection == 3 & Sexe == "H" ~ "1",
    Tb.equilibre == 3 ~ "1",
    Prise.de.poids == 3 ~ "1",
    Douleurs.D_C == 3 ~ "1",
    Diarrhee == 3 ~ "1",
    Vertiges == 3 ~ "1",
    Maux.de.tête == 3 ~ "1",
    Tremblements == 3 ~ "1",
    Tb.Libido == 3 ~ "1",
    Anxiete == 3 ~ "1",
    Demotivation  == 3 ~ "1",
    Diminution.emotion  == 3 ~ "1",
    Acouphenes.  == 3  ~ "1"
  ))

si$effectI[is.na(si$effectI)] <- "0"
table(si$effectI, si$effect)

si <- si %>%
  mutate(effectI.atleast = case_when(
    Bouche.seche == 3  | 
    Constipation == 3  | 
    Palpitations == 3  | 
    Malaise == 3  | 
    Insomnie == 3 | 
    Tb.orgasme == 3 | 
    Besoin.bouger == 3 | 
    Aug.appetit == 3  | 
    Sens.corp..Inh == 3  | 
    Nausees == 3  | 
    Vomissements == 3  | 
    Baillements == 3  | 
    Somnolence == 3  | 
    (Tb.erection == 3 & Sexe == "H")  | 
    Tb.equilibre == 3  | 
    Prise.de.poids == 3  | 
    Douleurs.D_C == 3  | 
    Diarrhee == 3| 
    Vertiges == 3 | 
    Maux.de.tête == 3  | 
    Tremblements == 3  | 
    Tb.Libido == 3 | 
    Anxiete == 3  | 
    Demotivation  == 3  | 
    Diminution.emotion  == 3  | 
    Acouphenes.  == 3  ~ "1"
  ))
table(si$effectI.atleast)
si$effectI.atleast[is.na(si$effectI.atleast)] <- "0"

#-------------------------------------------------------------------------------

# TRAITEMENT

str(si$Ttt)

si <- si %>% 
  mutate(ttt.class = case_when(
    Ttt == "Cl" ~ "1", 
    Ttt == "Ci" | Ttt == "Esc" | Ttt == "Fl" 
          | Ttt == "Pa" | Ttt == "Se" ~ "2",
    Ttt == "Ve" | Ttt == "Mil" | Ttt == "Du" ~ "3",
    Ttt == "Mia" | Ttt == "Mi" ~ "4",
    Ttt == "Vo" ~ "5",
    Ttt == "Mo" ~ "6"
  ))
table(si$ttt.class)


si2 <- si2 %>% 
  mutate(ttt.class = case_when(
    Ttt == "Cl" ~ "1", 
    Ttt == "Ci" | Ttt == "Esc" | Ttt == "Fl" 
    | Ttt == "Pa" | Ttt == "Se" ~ "2",
    Ttt == "Ve" | Ttt == "Mil" | Ttt == "Du" ~ "3",
    Ttt == "Mia" | Ttt == "Mi" ~ "4",
    Ttt == "Vo" ~ "5",
    Ttt == "Mo" ~ "6"
  ))
table(si2$ttt.class)

#-------------------------------------------------------------------------------

table(si2$effNUM)
mean(si2$effNUM)
median(si2$effNUM)
min(si2$effNUM)
max(si2$effNUM)

str(si2$effNUM)
si2 <- si2 %>% 
  mutate(effNUM3 = case_when(
    effNUM == 0 ~ 0,
    effNUM >=1 & effNUM <=3 ~ 1,
    effNUM > 3 ~ 2
  ))
table(si2$effNUM3)

#-------------------------------------------------------------------------------

table(si$I_M)

si$I_M = if_else(si$I_M == "m", "M", si$I_M)

#-------------------------------------------------------------------------------

library(tableone)

dput(names(si))

variables <- c("Sexe", "Age", "CSP", "ttt.class", 
  "I_M", "Poly", "TCM", "ATCD.CV", "ATCD.MP", "ATCD.MN", "ATCD.MR", 
  "ATCD.ME", "DC", "ATCD.AM", "ATCD.ANM", "Tabac", 
  "Auto_Hetero", "effect", "effectI")

categorical <- c( "Sexe", "CSP", "ttt.class", 
                 "I_M", "Poly", "TCM", "ATCD.CV", "ATCD.MP", "ATCD.MN", "ATCD.MR", 
                 "ATCD.ME", "DC", "ATCD.AM", "ATCD.ANM", "Tabac", 
                 "Auto_Hetero", 
                 "effect", "effectI")

descriptive = CreateTableOne(vars = variables, data = si, 
                               factorVars = categorical)
print(descriptive, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)


bivariate = CreateTableOne(vars = variables, data = si, 
                            factorVars = categorical, test = TRUE,
                            strata = "effect")
print(bivariate, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

table(si$I_M, si$effect)
prop.table(table(si$I_M, si$effect), margin = 2)*100
chisq.test(si$I_M, si$effect, correct = FALSE)

bivariate2 = CreateTableOne(vars = variables, data = si, 
                           factorVars = categorical, test = TRUE,
                           strata = "effectI")
print(bivariate2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

table(si$I_M, si$effectI)
prop.table(table(si$I_M, si$effectI), margin = 2)*100
chisq.test(si$I_M, si$effectI, correct = FALSE)

#-------------------------------------------------------------------------------
si$effect = as.factor(si$effect)
is.data.frame(si)
View(si)

s <- function(x){
  result = glm(effect ~ x, data = si, family = binomial)
  return(summary(result))
}

sf <- function(x){
  result = (mod = glm(effect ~ x, data = si, family = binomial))
  return(cbind(exp(coef(mod)), exp(confint.default(mod))))
}

s(si$Sexe)
sf(si$Sexe)

si$CSP = as.factor(si$CSP)
s(si$CSP)
sf(si$CSP)

#-------------------------------------------------------------------------------


variables3 <- c("Sexe", "Age", "CSP", "ttt.class", 
               "I_M", "Poly", "TCM", "ATCD.CV", "ATCD.MP", "ATCD.MN", "ATCD.MR", 
               "ATCD.ME", "DC", "ATCD.AM", "ATCD.ANM", "Tabac", 
               "Auto_Hetero", "effNUM3")

categorical3 <- c( "Sexe", "CSP", "ttt.class", 
                  "I_M", "Poly", "TCM", "ATCD.CV", "ATCD.MP", "ATCD.MN", "ATCD.MR", 
                  "ATCD.ME", "DC", "ATCD.AM", "ATCD.ANM", "Tabac", 
                  "Auto_Hetero", 
                  "effNUM3")

si2$I_M = if_else(si2$I_M == "m", "M", si2$I_M)

table(si2$I_M, si2$effNUM3)
prop.table(table(si2$I_M, si2$effNUM3), margin = 2)*100
chisq.test(si$I_M, si$effect, simulate.p.value = T, B = 2000)

bivariate3 = CreateTableOne(vars = variables3, data = si2, 
                            factorVars = categorical3, test = TRUE,
                            strata = "effNUM3")
print(bivariate3, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

names(si2)
table(si2$effNUM)




si2 <- si2 %>%
  mutate(effectI.atleast = case_when(
    Bouche.seche == 3  | 
      Constipation == 3  | 
      Palpitations == 3  | 
      Malaise == 3  | 
      Insomnie == 3 | 
      Tb.orgasme == 3 | 
      Besoin.bouger == 3 | 
      Aug.appetit == 3  | 
      Sens.corp..Inh == 3  | 
      Nausees == 3  | 
      Vomissements == 3  | 
      Baillements == 3  | 
      Somnolence == 3  | 
      (Tb.erection == 3 & Sexe == "H")  | 
      Tb.equilibre == 3  | 
      Prise.de.poids == 3  | 
      Douleurs.D_C == 3  | 
      Diarrhee == 3| 
      Vertiges == 3 | 
      Maux.de.tête == 3  | 
      Tremblements == 3  | 
      Tb.Libido == 3 | 
      Anxiete == 3  | 
      Demotivation  == 3  | 
      Diminution.emotion  == 3  | 
      Acouphenes.  == 3  ~ "1"
  ))

si2$effectI.atleast[is.na(si2$effectI.atleast)] <- "0"
table(si2$effectI.atleast, si2$effNUM3)
table(si2$effNUM3)
si2$effectI.atleast = as.numeric(as.character(si2$effectI.atleast))
str(si2$effNUM)

si2$effNUM.2 = ifelse(si2$effectI.atleast == 0, "0", si2$effNUM)
table(si2$effNUM, si2$effNUM.2)

si2$effNUM.2 = as.numeric(as.character(si2$effNUM.2))
str(si2$effNUM.2)
str(si2$effNUM2) = si2$effNUM.2

si2 <- si2 %>%
  mutate(effNUM23 = case_when(
    effNUM2 == 0 ~ 0,
    (effNUM2>=1 & effNUM2 <= 3) ~ 1,
    effNUM2 >= 4 ~ 2
    ))
table(si2$effNUM23)


variables4 <- c("Sexe", "Age", "CSP", "ttt.class", 
                "I_M", "Poly", "TCM", "ATCD.CV", "ATCD.MP", "ATCD.MN", "ATCD.MR", 
                "ATCD.ME", "DC", "ATCD.AM", "ATCD.ANM", "Tabac", 
                "Auto_Hetero", "effNUM23")

categorical4 <- c( "Sexe", "CSP", "ttt.class", 
                   "I_M", "Poly", "TCM", "ATCD.CV", "ATCD.MP", "ATCD.MN", "ATCD.MR", 
                   "ATCD.ME", "DC", "ATCD.AM", "ATCD.ANM", "Tabac", 
                   "Auto_Hetero", 
                   "effNUM23")

bivariate3c = CreateTableOne(vars = variables4, data = si2, 
                             factorVars = categorical4, test = TRUE,
                             strata = "effNUM23")
print(bivariate3c, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

si <- si %>%
  mutate(effect3c = case_when(
    Bouche.seche == 3 |
    Constipation == 3 |
    Palpitations == 3 | Malaise == 3 |  Insomnie == 3  |
    Tb.orgasme == 3 | Besoin.bouger == 3 | 
    Aug.appetit == 3| Sens.corp..Inh == 3 | Nausees == 3 | Vomissements == 3  |
    Baillements == 3 | Somnolence == 3 | (Tb.erection == 3 & Sexe == "H") | Tb.equilibre == 3 |
    Prise.de.poids == 3 | Douleurs.D_C == 3 | Diarrhee == 3 | Vertiges == 3 |
    Maux.de.tête == 3 | Tremblements == 3 | Tb.Libido == 3 |Anxiete == 3 |
    Demotivation == 3 | Diminution.emotion == 3 |
    Acouphenes. == 3 ~ "intol",
    
    Bouche.seche == 2 | Constipation == 2 | Palpitations == 2 |
      Malaise == 2 | Insomnie == 2 | Tb.orgasme == 2 | Besoin.bouger == 2 |
      Aug.appetit == 2 | Sens.corp..Inh == 2 | Nausees == 2 | Vomissements == 2 | 
      Baillements == 2 | Somnolence == 2 | (Tb.erection == 2 & Sexe == "H") |
      Tb.equilibre== 2 | Tb.equilibre== 2 | Douleurs.D_C == 2 | Diarrhee == 2 | 
      Vertiges == 2 | Maux.de.tête == 2 | Tremblements == 2 | 
      Tb.Libido == 2 | Anxiete == 2 | Demotivation == 2 | Diminution.emotion == 2 | 
      Acouphenes. == 2 ~ "tol"
  ))

table(si$effect3c)
si$effect3c[is.na(si$effect3c)] <- "0"

bivariate3c = CreateTableOne(vars = variables, data = si, 
                            factorVars = categorical, test = TRUE,
                            strata = "effect3c")
print(bivariate3c, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

table(si2$effNUM)
#0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 
#31 32 29 31 28 17 23 16  8  9  5  5  1  2  1  1  1 

library("tidyverse")

si2 <- si2 %>%
  mutate(events3 = case_when(
    effNUM == 0 ~ "0",
    effNUM == 1 ~ "1",
    effNUM > 1 ~ "2"
  ))

table(si2$events3)

quantile(si2$effNUM)
quantile(si2$effNUM, prob=(0.25), na.rm = TRUE)
quantile(si2$effNUM, prob=(0.75), na.rm = TRUE)


si2 <- si2 %>%
  mutate(ev.quant = case_when(
    effNUM == 0 ~ 1,
    effNUM >= 1 & effNUM < 3 ~ 2,
    effNUM >= 3 & effNUM < 6 ~ 3,
    effNUM >=6 & effNUM < 16 ~ 4
  ))
table(si2$ev.quant)

library("tableone")

variables <- c("Sexe", "Age", "CSP", "ttt.class", 
               "I_M", "Poly", "TCM", "ATCD.CV", "ATCD.MP", "ATCD.MN", "ATCD.MR", 
               "ATCD.ME", "DC", "ATCD.AM", "ATCD.ANM", "Tabac", 
               "Auto_Hetero", "ev.quant")

categorical <- c( "Sexe", "CSP", "ttt.class", 
                  "I_M", "Poly", "TCM", "ATCD.CV", "ATCD.MP", "ATCD.MN", "ATCD.MR", 
                  "ATCD.ME", "DC", "ATCD.AM", "ATCD.ANM", "Tabac", 
                  "Auto_Hetero", 
                  "ev.quant")

descriptive = CreateTableOne(vars = variables, data = si, 
                             factorVars = categorical)
print(descriptive, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)


bivariate.quartile = CreateTableOne(vars = variables, data = si2, 
                           factorVars = categorical, test = TRUE,
                           strata = "ev.quant")
print(bivariate.quartile, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)


k <- function(x){
  result = chisq.test(x, si2$ev.quant, simulate.p.value = T, B = 2000)
  return(result)
}

k(si2$Sexe)
kruskal.test(si2$Age, si2$ev.quant)

k(si2$CSP)
k(si2$ttt.class)
k(si2$I_M)
k(si2$Poly)
k(si2$TCM)
k(si2$ATCD.CV)
k(si$ATCD.MP)
k(si2$ATCD.MN)
k(si2$ATCD.MR) 
k(si2$ATCD.ME)
k(si2$DC)
k(si2$ATCD.AM)
k(si2$ATCD.ANM)
k(si2$Tabac)
k(si2$Auto_Hetero)

