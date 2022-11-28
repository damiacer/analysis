getwd()
setwd("P:/CONSULTATION/CAMAT") # ON PC

################################################################################

# PACKAGES

#install.packages("readxl")
library("readxl")
#install.packages("tidyverse")
library("tidyverse")

################################################################################

# DATA

camat <- read_excel("database.xlsx", na=".")

names(camat)
#View(camat)
table(camat$groupe)

################################################################################

# Critère de jugement principal :
# Comparaison du delta du score à l’inclusion (T0) et après 6 mois de prise 
# en charge (T1) à l’inventaire d’alliance de travail (de Roten, 2006) pour le 
# groupe case management comparativement au groupe prise en charge 
# infirmière traditionnelle.

# Critère de jugement Secondaire :
# Scores aux échelles suivantes : Birchwood Insight Scale (Jaafari et al., 2007), 
# The Scale to assess Unawareness of Mental Disorder (Jaafari et al., 2010), 
# Echelle de bien-être au travail (Biétry et Creusier, 2013)
# Corrélations entre les scores à l’inventaire d’alliance de travail (de Roten, 2006)
# et les échelles suivantes : Birchwood Insight Scale (Jaafari et al., 2007), 
# The Scale to assess Unawareness of Mental Disorder (Jaafari et al., 2010), 
# Echelle de bien-être au travail (Biétry et Creusier, 2013)

################################################################################

# VARIABLE TRANSFORMATION

nf <- function(x){
  result = as.numeric(as.character(x))
  return(result)
}

camat$duree_mal_annee  <- nf(camat$duree_mal_annee)
camat$chlorpromazine_equivalent_mg_j <- nf(camat$chlorpromazine_equivalent_mg_j)
camat$m6_chlorpromazine_equivalent_mg_j <- nf(camat$m6_chlorpromazine_equivalent_mg_j)
camat$m6_bientravail <- nf(camat$m6_bientravail)
camat$sumd_partconscien_ca <- nf(camat$sumd_partconscien_ca)
camat$m6_sumd_partconscien_na <- nf(camat$m6_sumd_partconscien_na)
camat$m6_sumd_partconscien_np <- nf(camat$m6_sumd_partconscien_np)
camat$sumd_ratio_ca <- nf(camat$sumd_ratio_ca)
camat$sumd_ratio_cp <- nf(camat$sumd_ratio_cp)
camat$sumd_ratio_aa <- nf(camat$sumd_ratio_aa)
camat$sumd_ratio_ap <- nf(camat$sumd_ratio_ap)
camat$m6_sumd_ratio_ca <- nf(camat$m6_sumd_ratio_ca)
camat$m6_sumd_ratio_cp <- nf(camat$m6_sumd_ratio_cp)
camat$m6_sumd_ratio_aa <- nf(camat$m6_sumd_ratio_aa)
camat$m6_sumd_ratio_ap <- nf(camat$m6_sumd_ratio_ap)
camat$sumd_somme <- nf(camat$sumd_somme)
camat$m6_sumd_somme <- nf(camat$m6_sumd_somme)
camat$m6_bientravail <- nf(camat$m6_bientravail)

################################################################################

#install.packages("tableone")
library("tableone")

camatvars = c("groupe",
              "age",
              "m6_age",
              "antecedents_fam",
              "duree_mal_annee",
              "nb_hospit" ,
              "antipsychotique",
              "chlorpromazine_equivalent_mg_j",
              "m6_antipsychotique",
              "m6_chlorpromazine_equivalent_mg_j",
              "lieu_vie",
              "statut_marital",
              "niveau_etudes",
              "profession",
              "m6_profession",
              "posneg_P",
              "posneg_N",
              "posneg_G",
              "posneg_T",
              "m6_posneg_P",
              "m6_posneg_N",
              "m6_posneg_G",
              "m6_posneg_T",
              "sumd_conscient_ca",
              "sumd_conscient_cp",
              "sumd_conscient_aa",
              "sumd_conscient_ap",
              "sumd_conscient_na",
              "sumd_conscient_np",
              "m6_sumd_conscient_ca",
              "m6_sumd_conscient_cp",
              "m6_sumd_conscient_aa",
              "m6_sumd_conscient_ap",
              "m6_sumd_conscient_na",
              "m6_sumd_conscient_np",
              "sumd_partconscien_ca",
              "sumd_partconscien_cp",
              "sumd_partconscien_aa",
              "sumd_partconscien_ap",
              "sumd_partconscien_na",
              "sumd_partconscien_np",
              "m6_sumd_partconscien_ca",
              "m6_sumd_partconscien_cp",
              "m6_sumd_partconscien_aa",
              "m6_sumd_partconscien_ap",
              "m6_sumd_partconscien_na",
              "m6_sumd_partconscien_np",
              "sumd_inc_ca",
              "sumd_inc_cp",
              "sumd_inc_aa",
              "sumd_inc_ap",
              "sumd_inc_na",
              "sumd_inc_np",
              "m6_sumd_inc_ca",
              "m6_sumd_inc_cp",
              "m6_sumd_inc_aa",
              "m6_sumd_inc_ap",
              "m6_sumd_inc_na",
              "m6_sumd_inc_np",
              "sumd_ratio_ca",
              "sumd_ratio_cp",
              "sumd_ratio_aa",
              "sumd_ratio_ap",
              "sumd_ratio_na",
              "sumd_ratio_np",
              "m6_sumd_ratio_ca",
              "m6_sumd_ratio_cp",
              "m6_sumd_ratio_aa",
              "m6_sumd_ratio_ap",
              "m6_sumd_ratio_na",
              "m6_sumd_ratio_np",
              "sumd_somme",
              "m6_sumd_somme",
              "medadhe",
              "m6_medadhe",
              "invenalliance",
              "m6_invenalliance",
              "birch",
              "m6_birch",
              "bientravail",
              "m6_bientravail"   
              )

camatcat = c("groupe",
              "antecedents_fam",
              "antipsychotique",
              "m6_antipsychotique",
              "lieu_vie",
              "statut_marital",
              "niveau_etudes",
              "profession",
              "m6_profession"
)

camat_descriptive = CreateTableOne(vars = camatvars, data = camat, factorVars = camatcat)
print(camat_descriptive, showAllLevels = TRUE, quote = TRUE, noSpaces = TRUE)

camat_deux = CreateTableOne(vars = camatvars, data = camat, factorVars = camatcat,
                            test = FALSE, includeNA = FALSE, strata = "groupe")
print(camat_deux, showAllLevels = FALSE, quote = TRUE, noSpaces = TRUE)

################################################################################

# camat$groupe_n[camat$groupe == "A"] <- "1"
# camat$groupe_n[camat$groupe == "B"] <- "2"
# camat$groupe_n = as.numeric(as.character(camat$groupe_n))

camat2 <- camat %>% drop_na(groupe)

install.packages("doBy")
library("doBy")

diff <- function(x,y){
  result = x-y
  return(result)
}

w <- function(x){
  #result = shapiro.test(x)
  result = wilcox.test(x ~ camat2$groupe, paired=F, exact=F, 
                       correct=F,  alternative = "two.sided")
  #result = t.test(x, y, alternative="two.sided", paired=F, var.equal=F, conf.level=0.95)
  return(result)
}

# CREATION DE DELTAS POUR TOUTES LES VARIABLES SCORE

# posneg_P 
camat2$delta.posneg_P = diff(camat2$m6_posneg_P, camat2$posneg_P)
str(camat2$delta.posneg_P)
summaryBy(delta.posneg_P ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.posneg_P)

# posneg_N 
camat2$delta.posneg_N = (camat2$m6_posneg_N - camat2$posneg_N)
summaryBy(delta.posneg_N ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.posneg_N)

# posneg_G 
camat2$delta.posneg_G = diff(camat2$m6_posneg_G, camat2$posneg_G)
summaryBy(delta.posneg_G ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.posneg_G)

# posneg_T
camat2$delta.posneg_T = diff(camat2$m6_posneg_T, camat2$posneg_T)
summaryBy(delta.posneg_T ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.posneg_T)

# sumd_conscient_ca 
camat2$delta.sumd_conscient_ca  = diff(camat2$m6_sumd_conscient_ca, camat2$sumd_conscient_ca)
summaryBy(delta.sumd_conscient_ca ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_conscient_ca)

# sumd_conscient_cp
camat2$delta.sumd_conscient_cp = diff(camat2$m6_sumd_conscient_cp, camat2$sumd_conscient_cp)
summaryBy(delta.sumd_conscient_cp ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_conscient_cp)


# sumd_conscient_aa
camat2$delta.sumd_conscient_aa = diff(camat2$m6_sumd_conscient_aa, camat2$sumd_conscient_aa)
summaryBy(delta.sumd_conscient_aa ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_conscient_aa)

# sumd_conscient_ap
camat2$delta.sumd_conscient_ap = diff(camat2$m6_sumd_conscient_ap, camat2$sumd_conscient_ap)
summaryBy(delta.sumd_conscient_ap ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_conscient_ap)

# sumd_conscient_na
camat2$delta.sumd_conscient_na = diff(camat2$m6_sumd_conscient_na, camat2$sumd_conscient_na)
summaryBy(delta.sumd_conscient_na ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_conscient_na)

# sumd_conscient_np
camat2$delta.sumd_conscient_np = diff(camat2$m6_sumd_conscient_np, camat2$sumd_conscient_np)
summaryBy(delta.sumd_conscient_np ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_conscient_np)

# sumd_partconscien_ca
camat2$delta.sumd_partconscien_ca = diff(camat2$m6_sumd_partconscien_ca, camat2$sumd_partconscien_ca)
summaryBy(delta.sumd_partconscien_ca ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_partconscien_ca)

# sumd_partconscien_cp
camat2$delta.sumd_partconscien_cp = diff(camat2$m6_sumd_partconscien_cp, camat2$sumd_partconscien_cp)
summaryBy(delta.sumd_partconscien_cp ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_partconscien_cp)

# sumd_partconscien_aa
camat2$delta.sumd_partconscien_aa = diff(camat2$m6_sumd_partconscien_aa, camat2$sumd_partconscien_aa)
summaryBy(delta.sumd_partconscien_aa ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_partconscien_aa)

# sumd_partconscien_ap
camat2$delta.sumd_partconscien_ap = diff(camat2$m6_sumd_partconscien_ap, camat2$sumd_partconscien_ap)
summaryBy(delta.sumd_partconscien_ap ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_partconscien_ap)

# sumd_partconscien_na
camat2$delta.sumd_partconscien_na = diff(camat2$m6_sumd_partconscien_na, camat2$sumd_partconscien_na)
summaryBy(delta.sumd_partconscien_na ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_partconscien_na)

# sumd_partconscien_np
camat2$delta.sumd_partconscien_np = diff(camat2$m6_sumd_partconscien_np, camat2$sumd_partconscien_np)
summaryBy(delta.sumd_partconscien_np ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_partconscien_np)

# sumd_inc_ca
camat2$delta.sumd_inc_ca = diff(camat2$m6_sumd_inc_ca, camat2$sumd_inc_ca)
summaryBy(delta.sumd_inc_ca ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_inc_ca)

# sumd_inc_cp
camat2$delta.sumd_inc_cp = diff(camat2$m6_sumd_inc_cp, camat2$sumd_inc_cp)
summaryBy(delta.sumd_inc_cp ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_inc_cp)

# sumd_inc_aa
camat2$delta.sumd_inc_aa = diff(camat2$m6_sumd_inc_aa, camat2$sumd_inc_aa)
summaryBy(delta.sumd_inc_aa ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_inc_aa)

# sumd_inc_ap
camat2$delta.sumd_inc_ap = diff(camat2$m6_sumd_inc_ap, camat2$sumd_inc_ap)
summaryBy(delta.sumd_inc_ap ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_inc_ap)

# sumd_inc_na
camat2$delta.sumd_inc_na = diff(camat2$m6_sumd_inc_na, camat2$sumd_inc_na)
summaryBy(delta.sumd_inc_na ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_inc_na)

# sumd_inc_np
camat2$delta.sumd_inc_np = diff(camat2$m6_sumd_inc_np, camat2$sumd_inc_np)
summaryBy(delta.sumd_inc_np ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_inc_np)

# sumd_ratio_ca
camat2$delta.sumd_ratio_ca = diff(camat2$m6_sumd_ratio_ca, camat2$sumd_ratio_ca)
summaryBy(delta.sumd_ratio_ca ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_ratio_ca)

# sumd_ratio_cp
camat2$delta.sumd_ratio_cp = diff(camat2$m6_sumd_ratio_cp, camat2$sumd_ratio_cp)
summaryBy(delta.sumd_ratio_cp ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_ratio_cp)

# sumd_ratio_aa 
camat2$delta.sumd_ratio_aa = diff(camat2$m6_sumd_ratio_aa , camat2$sumd_ratio_aa )
summaryBy(delta.sumd_ratio_aa ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_ratio_aa)

# sumd_ratio_ap 
camat2$delta.sumd_ratio_ap = diff(camat2$m6_sumd_ratio_ap , camat2$sumd_ratio_ap )
summaryBy(delta.sumd_ratio_ap ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_ratio_ap)

# sumd_ratio_na
camat2$delta.sumd_ratio_na = diff(camat2$m6_sumd_ratio_na, camat2$sumd_ratio_na)
summaryBy(delta.sumd_ratio_na ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_ratio_na)

# sumd_ratio_np
camat2$delta.sumd_ratio_np = diff(camat2$m6_sumd_ratio_np, camat2$sumd_ratio_np)
summaryBy(delta.sumd_ratio_np ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_ratio_np)

# sumd_somme
camat2$delta.sumd_somme = diff(camat2$m6_sumd_somme, camat2$m6_sumd_somme)
summaryBy(delta.sumd_somme ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.sumd_somme)

# medadhe 
camat2$delta.medadhe = diff(camat2$m6_medadhe, camat2$medadhe )
summaryBy(delta.medadhe ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.medadhe)

# invenalliance 
camat2$delta.invenalliance = diff(camat2$m6_invenalliance, camat2$invenalliance )
summaryBy(delta.invenalliance ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.invenalliance)

# birch 
camat2$delta.birch = diff(camat2$m6_birch, camat2$birch)
summaryBy(delta.birch ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.birch)

# bientravail 
camat2$delta.bientravail = diff(camat2$m6_bientravail , camat2$bientravail)
summaryBy(delta.bientravail ~ groupe, data = camat2, na.rm = TRUE,
          FUN = list(median, min, max))
w(camat2$delta.bientravail)
