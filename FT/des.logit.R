setwd("/Users/damianocerasuolo/Desktop/UBRC/21_22_CONSULT_MAC/Foulon_Thomas")
getwd()

#----------------------------------------------------------------------

library(readxl)
library(tidyverse)
library(tableone)

#----------------------------------------------------------------------

tf <- read_excel("TF_data.xlsx", na = "NA")

names(tf)

tf <- as_tibble(tf)
tf <- tf %>% rename(
			"Q1" = "Q1...2",
			"Q1det" = "Q1...3" 
			)
			
dput(names(tf))

variables <- c("Q1", "Q1det", "Q0", "Q2det", "Q3", "Q3det", "Q4", "Q5", 
"Q5det", "Q6", "Q6det", "Q7", "Q7det", "Q8", "Q8det", "Q9", "Q9det", 
"Q10", "Q10dat", "age", "dpt", "examen_pression", "mode_exc", 
"zone_exc", "contact_radio", "guide_bonusage", "diff_choixex", 
"formation_pres")

factors <- c("Q1", "Q1det", "Q0", "Q2det", "Q3", "Q3det", "Q4", "Q5", 
"Q5det", "Q6", "Q6det", "Q7", "Q7det", "Q8", "Q8det", "Q9", "Q9det", 
"Q10", "Q10dat", "age", "dpt", "examen_pression", "mode_exc", 
"zone_exc", "contact_radio", "guide_bonusage", "diff_choixex", 
"formation_pres")

tab_tf = CreateTableOne(vars = variables, factorVars = factors, data = tf)
print(tab_tf, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE) 

tf2 = subset(tf, select = -c(Q1det, Q2det, Q3det,
					Q5det, Q6det, Q7det, Q8det, Q9det, Q10dat))

names(tf2)

tf <- as_tibble(tf2)
tf2 <- tf2 %>% rename(
			"Q2" = "Q0",
			)
			
dput(names(tf2))

vars_q <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", 
"Q10", "age", "dpt", "examen_pression", "mode_exc", "zone_exc", 
"contact_radio", "guide_bonusage", "diff_choixex", "formation_pres")

fact_q <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", 
"Q10", "age", "dpt", "examen_pression", "mode_exc", "zone_exc", 
"contact_radio", "guide_bonusage", "diff_choixex", "formation_pres")


#----------------------------------------------------------------------

tf_q1 = CreateTableOne(vars = vars_q, factorVars = fact_q, data = tf2,
				test = TRUE, strata = "Q1")
print(tf_q1, showAllLevels = T, quote = T, nospaces = T)

#----------------------------------------------------------------------

tf.tot <- read_excel("TF_data_short.xlsx", na = "NA")

names(tf.tot)

str(tf.tot$Qtot)

mean(tf.tot$Qtot)
median(tf.tot$Qtot)

tf.tot <- tf.tot %>%
	mutate(q.tot = case_when(
	Qtot > 6.677083 ~ "1",
	Qtot <= 6.677083 ~ "0"
	))
	
table(tf.tot$q.tot)	

#----------------------------------------------------------------------

dput(names(tf.tot))

vars_tot = c("Qtot", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", 
"Q9", "Q10", "age", "dpt", "examen_pression", "mode_exc", "zone_exc", 
"contact_radio", "guide_bonusage", "diff_choixex", "formation_pres", 
"q.tot")

fact_tot = c("Qtot", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", 
"Q9", "Q10", "age", "dpt", "examen_pression", "mode_exc", "zone_exc", 
"contact_radio", "guide_bonusage", "diff_choixex", "formation_pres", 
"q.tot")

bivariate = CreateTableOne(vars = vars_tot, factorVars = fact_tot, 
			data = tf.tot, strata = "q.tot")
print(bivariate, showAllLevels = T, test = T, nospaces = T, quote = T)

#----------------------------------------------------------------------

chisq.test(tf.tot$q.tot, tf.tot$mode_exc, correct = F, simulate.p.value = T, B = 2000)
chisq.test(tf.tot$q.tot, tf.tot$zone_exc, correct = F, simulate.p.value = T, B = 2000)

#----------------------------------------------------------------------

tf.tot <- tf.tot %>%
		mutate(mode_exc3 = case_when(
		mode_exc == "Interne_remplaçant" ~ "Interne",
		mode_exc == "Medecin_hospitalier" ~ "Med_Hosp",
		mode_exc == "Medecin_liberal_en_association" | mode_exc == "Medecin_liberal_seul" ~ "Med_Lib"
		))
table(tf.tot$mode_exc3)
chisq.test(tf.tot$q.tot, tf.tot$mode_exc3, correct = F, simulate.p.value = T, B = 2000)

####################

table(tf.tot$age)

tf.tot <- tf.tot %>%
	mutate(age3 = case_when(
	age == "20_30" ~ "20_30",
	age == "30_40" ~ "30_40",
	age == "40_50" | age == "50_60" | age == "60_70" ~ "plus40"
	))
	
chisq.test(tf.tot$q.tot, tf.tot$age3, correct = F, simulate.p.value = T, B = 2000)

table(tf.tot$age3, tf.tot$q.tot)
prop.table(table(tf.tot$age3, tf.tot$q.tot), margin = 2)*100

####################

tf.tot <- tf.tot %>%
	mutate(zone_exc2 = case_when(
	zone_exc == "non_renseigne" ~ "",
	zone_exc == "rurale" ~ "1",
	zone_exc == "semi_urbaine" | zone_exc == "urbaine" ~ "2"
	))
table(tf.tot$zone_exc2)
tf.tot$zone_exc2 = as.numeric(as.character(tf.tot$zone_exc2))
tf.tot$zone_exc2f = as.factor(tf.tot$zone_exc2)
table(tf.tot$zone_exc2f)

chisq.test(tf.tot$zone_exc2f, tf.tot$q.tot, correct = FALSE, simulate.p.value = T, B = 2000)
table(tf.tot$zone_exc2f, tf.tot$q.tot)
prop.table(table(tf.tot$zone_exc2f, tf.tot$q.tot), margin = 2)*100

#----------------------------------------------------------------------
str(tf.tot$q.tot)
tf.tot$q.tot = as.factor(tf.tot$q.tot)

log1 <- glm(q.tot ~ mode_exc3 + age3 + zone_exc2f + guide_bonusage, data = tf.tot, family= binomial)
summary(log1)

exp(cbind(OR = coef(log1), confint(log1)))
	
