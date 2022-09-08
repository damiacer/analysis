getwd()
setwd("/Users/damianocerasuolo/Desktop/UBRC/21_22_CONSULT_MAC/Lafitte_Romain")
#-------------------------------------------------------------------------------
#install.packages("readxl")
library("readxl")
#install.packages("dplyr")
library("tidyverse")
#install.packages("plyr")
#library("plyr")
#install.packages("lubridate")
library("lubridate")
#-------------------------------------------------------------------------------
# ON MERGING DATAFRAMES:  https://stackoverflow.com/questions/15162197/combine-
# rbind-data-# frames-and-create-column-with-name-of-original-data-frames

prof <- read_excel("rl.prof.data.xlsx", na="NA")
sup <- read_excel("results-survey668447.xlsx", na="N/A")
names(prof)
View(prof)

library("tidyverse")
table(prof$matiere)
table(prof$matiere.autre)

prof <- prof %>%
  mutate(mat = case_when(
    matiere == "Arts" ~ "Lettres",
    matiere == "Biologie - écologie" ~ "Sciences",
    matiere ==  "Création et culture design" ~ "Ens.pro", 
    matiere == "Histoire Géographie" ~ "Lettres",
    matiere == "Langues et cultures de l'Antiquité : Latin" ~ "Lettres", 
    matiere == "Mathématiques" ~ "Maths", 
    matiere == "Philosophie" ~ "Lettres", 
    matiere == "Principes fondamentaux de l'économie et de la gestion" ~ "Ecogest",
    matiere == "Sciences de l'ingénieur" ~ "Sciences.app",
    matiere == "Sciences Economiques et Sociales" ~ "Ecogest",
    matiere == "Sciences numériques et technologie" ~ "Sciences.app", 
    matiere == "Biotechnologies" ~ "Sciences",
    matiere == "Education Physique et Sportive" ~ "Sport", 
    matiere == "Français" ~ "Lettres",
    matiere == "Humanités, littérature et philosophie" ~ "Lettres",
    matiere == "Langues vivantes" ~ "Lettres",
    matiere == "Numérique et sciences informatiques" ~ "Sciences.app",
    matiere == "Physique Chimie" ~ "Sciences",
    matiere == "Santé et social" ~ "Sante",
    matiere == "Sciences de la Vie et de la Terre" ~ "Sciences", 
    matiere == "Sciences et laboratoire" ~ "Sciences.app",
    matiere.autre == "Autre" ~ "Autre", 
    matiere.autre == "Ecogestion" ~ "Ecogest",
    matiere.autre == "Management" ~ "Ecogest", 
    matiere.autre == "BTP" ~ "Ens.pro", 
    matiere.autre == "Commerce" ~ "Ens.pro",
    matiere.autre == "conducteursroutiers" ~ "Ens.pro",
    matiere.autre == "Electrotechnique" ~ "Ens.pro",
    matiere.autre == "Genie" ~ "Ens.pro",
    matiere.autre == "Hotellerie" ~ "Ens.pro", 
    matiere.autre == "Lettres" ~ "Lettres", 
    matiere.autre == "Maths" ~ "Maths", 
    matiere.autre == "Mode" ~ "Ens.pro"
      ))
table(prof$mat)

library("tableone")

dput(names(prof))

variables = c("annee.ex", "mat", "evas", 
              "formationi.evas", "formationl.evas", "fevas.comment", "fevas.ist", 
              "fevas.contraption", "fevas.consentement", "fevas.anat", "fevas.legislation", 
              "fevas.hygiene", "fevas.violences", "fevas.lgbtqia", "fevas.genre", 
              "fevas.porno", "fevas.cyber", "fevas.egalite", "fevas.culture")

factors = c("mat", "evas", 
              "formationi.evas", "formationl.evas", "fevas.comment", "fevas.ist", 
              "fevas.contraption", "fevas.consentement", "fevas.anat", "fevas.legislation", 
              "fevas.hygiene", "fevas.violences", "fevas.lgbtqia", "fevas.genre", 
              "fevas.porno", "fevas.cyber", "fevas.egalite", "fevas.culture")


descriptive = CreateTableOne(vars = variables, data = prof, 
                             factorVars = factors)
print(descriptive, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#-----------------------------------------------------------------------------

mean(prof$annee.ex, na.rm = TRUE)
median(prof$annee.ex, na.rm = TRUE)

quantile(prof$annee.ex,prob=(0.25),na.rm = TRUE)
quantile(prof$annee.ex,prob=(0.75),na.rm = TRUE)

min()

prof <- prof %>%
  mutate(annee.ex3 = case_when(
    annee.ex <= 15 ~ "1",
    annee.ex > 15 & annee.ex <= 23 ~ "2",
    annee.ex > 23 & annee.ex <= 28 ~ "3",
    annee.ex > 28 ~ "4"
  ))
table(prof$annee.ex3)


bivariate = CreateTableOne(vars = variables, data = prof, 
                            factorVars = factors, test = TRUE, includeNA = TRUE,
                            strata = "annee.ex3")
print(bivariate, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#-----------------------------------------------------------------------------

k <- function(x){
  result = chisq.test(x, prof$annee.ex3, correct = FALSE, simulate.p.value = T,
                      B = 2000)
  return(result)
}

           
k(prof$evas)
k(prof$formationi.evas)
k(prof$formationl.evas) 
k(prof$fevas.comment)
k(prof$fevas.ist)
k(prof$fevas.contraption)
k(prof$fevas.consentement)
k(prof$fevas.anat)
k(prof$fevas.legislation)
k(prof$fevas.hygiene)
k(prof$fevas.violences)   
k(prof$fevas.lgbtqia)      
k(prof$fevas.genre)
k(prof$fevas.porno)
k(prof$fevas.cyber)       
k(prof$fevas.egalite)
k(prof$fevas.culture)

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------

dput(names(sup))
View(sup)

variables.sup = c("ID de la réponse", "Date de soumission", "Dernière page", 
                  "Langue de départ", "Tête de série", "Nom de l'établissement : ", 
                  "Nombre d'élèves inscrits dans l'établissement en 2022 : ", 
                  "Nombre de professeurs ayant suivi une formation initiale et/ou continue sur l'éducation à la vie affective et sexuelle en 2022 au sein du lycée : ", 
                  "Nombre de professeurs stagiaires dans votre établissement en 2022 : ", 
                  "Nombre total d'enseignants dans l'établissement en 2022 : ", 
                  "Existe-t-il des interventions prévues sur l'éducation à la vie affective et sexuelle durant le cursus d'un(e) élève dans l'établissement ?", 
                  "S'agissant des interventions ayant lieu dans le cadre d'un cours :   Existe-t-il des interventions sur l'éducation à la vie affective et sexuelle ayant lieu dans un cours dans le cadre du parcours éducatif de santé ?", 
                  "Nombre de professeurs proposant ces cours ? ", "Dans quelle matière ? [Mathématiques]", 
                  "Dans quelle matière ? [Physique Chimie]", "Dans quelle matière ? [Sciences de la Vie et de la Terre]", 
                  "Dans quelle matière ? [Français]", "Dans quelle matière ? [Histoire Géographie]", 
                  "Dans quelle matière ? [Sciences Economiques et Sociales]", 
                  "Dans quelle matière ? [Education Physique et Sportive]", "Dans quelle matière ? [Enseignement Moral et Civique]", 
                  "Dans quelle matière ? [Sciences numériques et technologie]", 
                  "Dans quelle matière ? [Langues vivantes]", "Dans quelle matière ? [Arts]", 
                  "Dans quelle matière ? [Langues et cultures de l'Antiquité : Latin]", 
                  "Dans quelle matière ? [Langues et cultures de l'Antiquité : Grec]", 
                  "Dans quelle matière ? [Biotechnologies]", "Dans quelle matière ? [Création et culture design]", 
                  "Dans quelle matière ? [Sciences de l'ingénieur]", "Dans quelle matière ? [Création et innovation technologiques]", 
                  "Dans quelle matière ? [Principes fondamentaux de l'économie et de la gestion]", 
                  "Dans quelle matière ? [Santé et social]", "Dans quelle matière ? [Sciences et laboratoire]", 
                  "Dans quelle matière ? [Philosophie]", "Dans quelle matière ? [Humanités scientifiques et numériques]", 
                  "Dans quelle matière ? [Humanités, littérature et philosophie]", 
                  "Dans quelle matière ? [Numérique et sciences informatiques]", 
                  "Dans quelle matière ? [Biologie - écologie ]", "Dans quelle matière ? [Autre]", 
                  "Combien d'interventions de ce type sont prévues sur l'ensemble des trois années de cursus d'un(e) élève ? [Seconde]...40", 
                  "Combien d'interventions de ce type sont prévues sur l'ensemble des trois années de cursus d'un(e) élève ? [Première]...41", 
                  "Combien d'interventions de ce type sont prévues sur l'ensemble des trois années de cursus d'un(e) élève ? [Terminale]...42", 
                  "S'agissant des interventions ayant lieu dans un cadre spécifiquement prévu pour l'éducation à la vie affective et sexuelle :  Existe-t-il des séances sans intervenants extérieurs à l'établissement ?", 
                  "Qui y participe ?  [Enseignants]", "Qui y participe ?  [Médecin scolaire, infirmière scolaire, CPE ou AES]", 
                  "Qui y participe ?  [Enseignants ET médecin scolaire, infirmière scolaire, CPE ou AES]", 
                  "Qui y participe ?  [Autre]", "Nombre d'interventions de ce type prévues durant les trois années du cursus d'un(e) élève ? [Seconde]", 
                  "Nombre d'interventions de ce type prévues durant les trois années du cursus d'un(e) élève ? [Première]", 
                  "Nombre d'interventions de ce type prévues durant les trois années du cursus d'un(e) élève ? [Terminale]", 
                  "Nombre de professeurs de votre établissement participant à ces interventions ?", 
                  "Dans quelle matière enseignent-ils ? [Mathématiques]...52", 
                  "Dans quelle matière enseignent-ils ? [Physique Chimie]...53", 
                  "Dans quelle matière enseignent-ils ? [Sciences de la Vie et de la Terre]...54", 
                  "Dans quelle matière enseignent-ils ? [Français]...55", "Dans quelle matière enseignent-ils ? [Histoire Géographie]...56", 
                  "Dans quelle matière enseignent-ils ? [Sciences Economiques et Sociales]...57", 
                  "Dans quelle matière enseignent-ils ? [Education Physique et Sportive]...58", 
                  "Dans quelle matière enseignent-ils ? [Enseignement Moral et Civique]...59", 
                  "Dans quelle matière enseignent-ils ? [Sciences numériques et technologie]...60", 
                  "Dans quelle matière enseignent-ils ? [Langues vivantes]...61", 
                  "Dans quelle matière enseignent-ils ? [Arts]...62", "Dans quelle matière enseignent-ils ? [Langues et cultures de l'Antiquité : Latin]...63", 
                  "Dans quelle matière enseignent-ils ? [Langues et cultures de l'Antiquité : Grec]...64", 
                  "Dans quelle matière enseignent-ils ? [Biotechnologies]...65", 
                  "Dans quelle matière enseignent-ils ? [Création et culture design]...66", 
                  "Dans quelle matière enseignent-ils ? [Sciences de l'ingénieur]...67", 
                  "Dans quelle matière enseignent-ils ? [Création et innovation technologiques]...68", 
                  "Dans quelle matière enseignent-ils ? [Principes fondamentaux de l'économie et de la gestion]...69", 
                  "Dans quelle matière enseignent-ils ? [Santé et social]...70", 
                  "Dans quelle matière enseignent-ils ? [Sciences et laboratoire]...71", 
                  "Dans quelle matière enseignent-ils ? [Philosophie]...72", "Dans quelle matière enseignent-ils ? [Humanités scientifiques et numériques]...73", 
                  "Dans quelle matière enseignent-ils ? [Humanités, littérature et philosophie]...74", 
                  "Dans quelle matière enseignent-ils ? [Numérique et sciences informatiques]...75", 
                  "Dans quelle matière enseignent-ils ? [Biologie - écologie ]...76", 
                  "Dans quelle matière enseignent-ils ? [Autre]...77", "Existe-t-il des interventions avec intervenants extérieurs à l'établissement ?", 
                  "Quels types de partenaires interviennent lors de ces séances ?  [Le Centre de Planification et d'Education Familiale]", 
                  "Quels types de partenaires interviennent lors de ces séances ?  [Planning Familial]", 
                  "Quels types de partenaires interviennent lors de ces séances ?  [Autres associations agréées ]", 
                  "Quels types de partenaires interviennent lors de ces séances ?  [Professionnels de santé (médecin, infirmière, etc.)]", 
                  "Quels types de partenaires interviennent lors de ces séances ?  [Psychologue, Sexologue]", 
                  "Quels types de partenaires interviennent lors de ces séances ?  [Autre]", 
                  "Combien d'interventions de ce type sont prévues sur l'ensemble des trois années de cursus d'un(e) élève ? [Seconde]...85", 
                  "Combien d'interventions de ce type sont prévues sur l'ensemble des trois années de cursus d'un(e) élève ? [Première]...86", 
                  "Combien d'interventions de ce type sont prévues sur l'ensemble des trois années de cursus d'un(e) élève ? [Terminale]...87", 
                  "Combien de professeurs de votre établissement participent à ces interventions ?", 
                  "Dans quelle matière enseignent-ils ? [Mathématiques]...89", 
                  "Dans quelle matière enseignent-ils ? [Physique Chimie]...90", 
                  "Dans quelle matière enseignent-ils ? [Sciences de la Vie et de la Terre]...91", 
                  "Dans quelle matière enseignent-ils ? [Français]...92", "Dans quelle matière enseignent-ils ? [Histoire Géographie]...93", 
                  "Dans quelle matière enseignent-ils ? [Sciences Economiques et Sociales]...94", 
                  "Dans quelle matière enseignent-ils ? [Education Physique et Sportive]...95", 
                  "Dans quelle matière enseignent-ils ? [Enseignement Moral et Civique]...96", 
                  "Dans quelle matière enseignent-ils ? [Sciences numériques et technologie]...97", 
                  "Dans quelle matière enseignent-ils ? [Langues vivantes]...98", 
                  "Dans quelle matière enseignent-ils ? [Arts]...99", "Dans quelle matière enseignent-ils ? [Langues et cultures de l'Antiquité : Latin]...100", 
                  "Dans quelle matière enseignent-ils ? [Langues et cultures de l'Antiquité : Grec]...101", 
                  "Dans quelle matière enseignent-ils ? [Biotechnologies]...102", 
                  "Dans quelle matière enseignent-ils ? [Création et culture design]...103", 
                  "Dans quelle matière enseignent-ils ? [Sciences de l'ingénieur]...104", 
                  "Dans quelle matière enseignent-ils ? [Création et innovation technologiques]...105", 
                  "Dans quelle matière enseignent-ils ? [Principes fondamentaux de l'économie et de la gestion]...106", 
                  "Dans quelle matière enseignent-ils ? [Santé et social]...107", 
                  "Dans quelle matière enseignent-ils ? [Sciences et laboratoire]...108", 
                  "Dans quelle matière enseignent-ils ? [Philosophie]...109", 
                  "Dans quelle matière enseignent-ils ? [Humanités scientifiques et numériques]...110", 
                  "Dans quelle matière enseignent-ils ? [Humanités, littérature et philosophie]...111", 
                  "Dans quelle matière enseignent-ils ? [Numérique et sciences informatiques]...112", 
                  "Dans quelle matière enseignent-ils ? [Biologie - écologie ]...113", 
                  "Dans quelle matière enseignent-ils ? [Autre]...114", "Quel est le facteur limitant principal à la mise en place de séance d'éducation à la vie affective et sexuelle dans votre établissement ?", 
                  "Quel est le facteur limitant principal à la mise en place de séance d'éducation à la vie affective et sexuelle dans votre établissement ? [Autre]", 
                  "Est-il prévu une formation initiale sur les sujets liés à l'éducation à la vie affective et sexuelle pour les enseignants de votre établissement ?", 
                  "Est-elle proposée à tous les enseignants à leur arrivée dans l'établissement ?", 
                  "Sous quelles conditions proposez-vous la formation initiale ? [A la demande de l'enseignant]", 
                  "Sous quelles conditions proposez-vous la formation initiale ? [Selon la discipline enseignée]", 
                  "Sous quelles conditions proposez-vous la formation initiale ? [Autre]", 
                  "Qui a été le dernier formateur en date ayant cumulé le plus d'heures de formation initiale à destination des enseignants ?", 
                  "Qui a été le dernier formateur en date ayant cumulé le plus d'heures de formation initiale à destination des enseignants ? [Autre]", 
                  "Combien de temps dure la formation initiale ? (en heures)", 
                  "Comment sont abordé les sujets durant cette formation initiale ?", 
                  "Quels sont ces sujets ? [Comment aborder l'éducation à la vie affective et sexuelle en tant qu'enseignant]", 
                  "Quels sont ces sujets ? [Les infections sexuellement transmissibles]", 
                  "Quels sont ces sujets ? [La contraception et la contraception d'urgence]", 
                  "Quels sont ces sujets ? [Le consentement]", "Quels sont ces sujets ? [L'anatomie]", 
                  "Quels sont ces sujets ? [La législation]", "Quels sont ces sujets ? [Les violences sexistes et sexuelles]", 
                  "Quels sont ces sujets ? [L'hygiène]", "Quels sont ces sujets ? [La sexualité LGBTQIA+]", 
                  "Quels sont ces sujets ? [Le genre]", "Quels sont ces sujets ? [La pornographie]", 
                  "Quels sont ces sujets ? [La cybersexualité (nudes, sexting, revenge porn, etc.)]", 
                  "Quels sont ces sujets ? [L'égalité femme - homme]", "Quels sont ces sujets ? [Culture, religion et sexualité]", 
                  "Quels sont ces sujets ? [Autre]", "Quel est le facteur limitant principal à la mise en place d'une formation initiale à l'éducation à la vie affective et sexuelle dans votre établissement ?", 
                  "Quel est le facteur limitant principal à la mise en place d'une formation initiale à l'éducation à la vie affective et sexuelle dans votre établissement ? [Autre]", 
                  "Existe-t-il une formation continue concernant l'éducation à la vie affective et sexuelle dispensée aux professeurs enseignant dans l'établissement ?", 
                  "Est-elle proposée à tous les enseignants de l'établissement ?", 
                  "Sous quelles conditions proposez-vous la formation continue ? [A la demande de l'enseignant]", 
                  "Sous quelles conditions proposez-vous la formation continue ? [Selon la discipline enseignée]", 
                  "Sous quelles conditions proposez-vous la formation continue ? [Suite à une formation initiale ]", 
                  "Sous quelles conditions proposez-vous la formation continue ? [Autre]", 
                  "Qui a été le dernier formateur en date ayant cumulé le plus d'heures de formation continue à destination des enseignants ?", 
                  "Qui a été le dernier formateur en date ayant cumulé le plus d'heures de formation continue à destination des enseignants ? [Autre]", 
                  "Sur quel rythme se base la formation continue ?", "Quels sont les trois sujets les plus fréquemment abordés ? [Comment aborder l'éducation à la vie affective et sexuelle en tant qu'enseignant]", 
                  "Quels sont les trois sujets les plus fréquemment abordés ? [Les infections sexuellement transmissibles]", 
                  "Quels sont les trois sujets les plus fréquemment abordés ? [La contraception et la contraception d'urgence]", 
                  "Quels sont les trois sujets les plus fréquemment abordés ? [Le consentement]", 
                  "Quels sont les trois sujets les plus fréquemment abordés ? [L'anatomie]", 
                  "Quels sont les trois sujets les plus fréquemment abordés ? [La législation]", 
                  "Quels sont les trois sujets les plus fréquemment abordés ? [Les violences sexistes et sexuelles]", 
                  "Quels sont les trois sujets les plus fréquemment abordés ? [L'hygiène]", 
                  "Quels sont les trois sujets les plus fréquemment abordés ? [La sexualité LGBTQIA+]", 
                  "Quels sont les trois sujets les plus fréquemment abordés ? [Le genre]", 
                  "Quels sont les trois sujets les plus fréquemment abordés ? [La pornographie]", 
                  "Quels sont les trois sujets les plus fréquemment abordés ? [La cybersexualité (nudes, sexting, revenge porn, etc.)]", 
                  "Quels sont les trois sujets les plus fréquemment abordés ? [L'égalité femme - homme]", 
                  "Quels sont les trois sujets les plus fréquemment abordés ? [Culture, religion et sexualité]", 
                  "Quels sont les trois sujets les plus fréquemment abordés ? [Autre]", 
                  "Quel est le facteur limitant principal à la mise en place d'une formation continue à l'éducation à la vie affective et sexuelle ?", 
                  "Quel est le facteur limitant principal à la mise en place d'une formation continue à l'éducation à la vie affective et sexuelle ? [Autre]"
)

factors.sup = c("ID de la réponse", "Date de soumission", "Dernière page", 
                "Langue de départ", "Tête de série", "Nom de l'établissement : ", 
                "Nombre d'élèves inscrits dans l'établissement en 2022 : ", 
                "Nombre de professeurs ayant suivi une formation initiale et/ou continue sur l'éducation à la vie affective et sexuelle en 2022 au sein du lycée : ", 
                "Nombre de professeurs stagiaires dans votre établissement en 2022 : ", 
                "Nombre total d'enseignants dans l'établissement en 2022 : ", 
                "Existe-t-il des interventions prévues sur l'éducation à la vie affective et sexuelle durant le cursus d'un(e) élève dans l'établissement ?", 
                "S'agissant des interventions ayant lieu dans le cadre d'un cours :   Existe-t-il des interventions sur l'éducation à la vie affective et sexuelle ayant lieu dans un cours dans le cadre du parcours éducatif de santé ?", 
                "Nombre de professeurs proposant ces cours ? ", "Dans quelle matière ? [Mathématiques]", 
                "Dans quelle matière ? [Physique Chimie]", "Dans quelle matière ? [Sciences de la Vie et de la Terre]", 
                "Dans quelle matière ? [Français]", "Dans quelle matière ? [Histoire Géographie]", 
                "Dans quelle matière ? [Sciences Economiques et Sociales]", 
                "Dans quelle matière ? [Education Physique et Sportive]", "Dans quelle matière ? [Enseignement Moral et Civique]", 
                "Dans quelle matière ? [Sciences numériques et technologie]", 
                "Dans quelle matière ? [Langues vivantes]", "Dans quelle matière ? [Arts]", 
                "Dans quelle matière ? [Langues et cultures de l'Antiquité : Latin]", 
                "Dans quelle matière ? [Langues et cultures de l'Antiquité : Grec]", 
                "Dans quelle matière ? [Biotechnologies]", "Dans quelle matière ? [Création et culture design]", 
                "Dans quelle matière ? [Sciences de l'ingénieur]", "Dans quelle matière ? [Création et innovation technologiques]", 
                "Dans quelle matière ? [Principes fondamentaux de l'économie et de la gestion]", 
                "Dans quelle matière ? [Santé et social]", "Dans quelle matière ? [Sciences et laboratoire]", 
                "Dans quelle matière ? [Philosophie]", "Dans quelle matière ? [Humanités scientifiques et numériques]", 
                "Dans quelle matière ? [Humanités, littérature et philosophie]", 
                "Dans quelle matière ? [Numérique et sciences informatiques]", 
                "Dans quelle matière ? [Biologie - écologie ]", "Dans quelle matière ? [Autre]", 
                "Combien d'interventions de ce type sont prévues sur l'ensemble des trois années de cursus d'un(e) élève ? [Seconde]...40", 
                "Combien d'interventions de ce type sont prévues sur l'ensemble des trois années de cursus d'un(e) élève ? [Première]...41", 
                "Combien d'interventions de ce type sont prévues sur l'ensemble des trois années de cursus d'un(e) élève ? [Terminale]...42", 
                "S'agissant des interventions ayant lieu dans un cadre spécifiquement prévu pour l'éducation à la vie affective et sexuelle :  Existe-t-il des séances sans intervenants extérieurs à l'établissement ?", 
                "Qui y participe ?  [Enseignants]", "Qui y participe ?  [Médecin scolaire, infirmière scolaire, CPE ou AES]", 
                "Qui y participe ?  [Enseignants ET médecin scolaire, infirmière scolaire, CPE ou AES]", 
                "Qui y participe ?  [Autre]", "Nombre d'interventions de ce type prévues durant les trois années du cursus d'un(e) élève ? [Seconde]", 
                "Nombre d'interventions de ce type prévues durant les trois années du cursus d'un(e) élève ? [Première]", 
                "Nombre d'interventions de ce type prévues durant les trois années du cursus d'un(e) élève ? [Terminale]", 
                "Nombre de professeurs de votre établissement participant à ces interventions ?", 
                "Dans quelle matière enseignent-ils ? [Mathématiques]...52", 
                "Dans quelle matière enseignent-ils ? [Physique Chimie]...53", 
                "Dans quelle matière enseignent-ils ? [Sciences de la Vie et de la Terre]...54", 
                "Dans quelle matière enseignent-ils ? [Français]...55", "Dans quelle matière enseignent-ils ? [Histoire Géographie]...56", 
                "Dans quelle matière enseignent-ils ? [Sciences Economiques et Sociales]...57", 
                "Dans quelle matière enseignent-ils ? [Education Physique et Sportive]...58", 
                "Dans quelle matière enseignent-ils ? [Enseignement Moral et Civique]...59", 
                "Dans quelle matière enseignent-ils ? [Sciences numériques et technologie]...60", 
                "Dans quelle matière enseignent-ils ? [Langues vivantes]...61", 
                "Dans quelle matière enseignent-ils ? [Arts]...62", "Dans quelle matière enseignent-ils ? [Langues et cultures de l'Antiquité : Latin]...63", 
                "Dans quelle matière enseignent-ils ? [Langues et cultures de l'Antiquité : Grec]...64", 
                "Dans quelle matière enseignent-ils ? [Biotechnologies]...65", 
                "Dans quelle matière enseignent-ils ? [Création et culture design]...66", 
                "Dans quelle matière enseignent-ils ? [Sciences de l'ingénieur]...67", 
                "Dans quelle matière enseignent-ils ? [Création et innovation technologiques]...68", 
                "Dans quelle matière enseignent-ils ? [Principes fondamentaux de l'économie et de la gestion]...69", 
                "Dans quelle matière enseignent-ils ? [Santé et social]...70", 
                "Dans quelle matière enseignent-ils ? [Sciences et laboratoire]...71", 
                "Dans quelle matière enseignent-ils ? [Philosophie]...72", "Dans quelle matière enseignent-ils ? [Humanités scientifiques et numériques]...73", 
                "Dans quelle matière enseignent-ils ? [Humanités, littérature et philosophie]...74", 
                "Dans quelle matière enseignent-ils ? [Numérique et sciences informatiques]...75", 
                "Dans quelle matière enseignent-ils ? [Biologie - écologie ]...76", 
                "Dans quelle matière enseignent-ils ? [Autre]...77", "Existe-t-il des interventions avec intervenants extérieurs à l'établissement ?", 
                "Quels types de partenaires interviennent lors de ces séances ?  [Le Centre de Planification et d'Education Familiale]", 
                "Quels types de partenaires interviennent lors de ces séances ?  [Planning Familial]", 
                "Quels types de partenaires interviennent lors de ces séances ?  [Autres associations agréées ]", 
                "Quels types de partenaires interviennent lors de ces séances ?  [Professionnels de santé (médecin, infirmière, etc.)]", 
                "Quels types de partenaires interviennent lors de ces séances ?  [Psychologue, Sexologue]", 
                "Quels types de partenaires interviennent lors de ces séances ?  [Autre]", 
                "Combien d'interventions de ce type sont prévues sur l'ensemble des trois années de cursus d'un(e) élève ? [Seconde]...85", 
                "Combien d'interventions de ce type sont prévues sur l'ensemble des trois années de cursus d'un(e) élève ? [Première]...86", 
                "Combien d'interventions de ce type sont prévues sur l'ensemble des trois années de cursus d'un(e) élève ? [Terminale]...87", 
                "Combien de professeurs de votre établissement participent à ces interventions ?", 
                "Dans quelle matière enseignent-ils ? [Mathématiques]...89", 
                "Dans quelle matière enseignent-ils ? [Physique Chimie]...90", 
                "Dans quelle matière enseignent-ils ? [Sciences de la Vie et de la Terre]...91", 
                "Dans quelle matière enseignent-ils ? [Français]...92", "Dans quelle matière enseignent-ils ? [Histoire Géographie]...93", 
                "Dans quelle matière enseignent-ils ? [Sciences Economiques et Sociales]...94", 
                "Dans quelle matière enseignent-ils ? [Education Physique et Sportive]...95", 
                "Dans quelle matière enseignent-ils ? [Enseignement Moral et Civique]...96", 
                "Dans quelle matière enseignent-ils ? [Sciences numériques et technologie]...97", 
                "Dans quelle matière enseignent-ils ? [Langues vivantes]...98", 
                "Dans quelle matière enseignent-ils ? [Arts]...99", "Dans quelle matière enseignent-ils ? [Langues et cultures de l'Antiquité : Latin]...100", 
                "Dans quelle matière enseignent-ils ? [Langues et cultures de l'Antiquité : Grec]...101", 
                "Dans quelle matière enseignent-ils ? [Biotechnologies]...102", 
                "Dans quelle matière enseignent-ils ? [Création et culture design]...103", 
                "Dans quelle matière enseignent-ils ? [Sciences de l'ingénieur]...104", 
                "Dans quelle matière enseignent-ils ? [Création et innovation technologiques]...105", 
                "Dans quelle matière enseignent-ils ? [Principes fondamentaux de l'économie et de la gestion]...106", 
                "Dans quelle matière enseignent-ils ? [Santé et social]...107", 
                "Dans quelle matière enseignent-ils ? [Sciences et laboratoire]...108", 
                "Dans quelle matière enseignent-ils ? [Philosophie]...109", 
                "Dans quelle matière enseignent-ils ? [Humanités scientifiques et numériques]...110", 
                "Dans quelle matière enseignent-ils ? [Humanités, littérature et philosophie]...111", 
                "Dans quelle matière enseignent-ils ? [Numérique et sciences informatiques]...112", 
                "Dans quelle matière enseignent-ils ? [Biologie - écologie ]...113", 
                "Dans quelle matière enseignent-ils ? [Autre]...114", "Quel est le facteur limitant principal à la mise en place de séance d'éducation à la vie affective et sexuelle dans votre établissement ?", 
                "Quel est le facteur limitant principal à la mise en place de séance d'éducation à la vie affective et sexuelle dans votre établissement ? [Autre]", 
                "Est-il prévu une formation initiale sur les sujets liés à l'éducation à la vie affective et sexuelle pour les enseignants de votre établissement ?", 
                "Est-elle proposée à tous les enseignants à leur arrivée dans l'établissement ?", 
                "Sous quelles conditions proposez-vous la formation initiale ? [A la demande de l'enseignant]", 
                "Sous quelles conditions proposez-vous la formation initiale ? [Selon la discipline enseignée]", 
                "Sous quelles conditions proposez-vous la formation initiale ? [Autre]", 
                "Qui a été le dernier formateur en date ayant cumulé le plus d'heures de formation initiale à destination des enseignants ?", 
                "Qui a été le dernier formateur en date ayant cumulé le plus d'heures de formation initiale à destination des enseignants ? [Autre]", 
                "Combien de temps dure la formation initiale ? (en heures)", 
                "Comment sont abordé les sujets durant cette formation initiale ?", 
                "Quels sont ces sujets ? [Comment aborder l'éducation à la vie affective et sexuelle en tant qu'enseignant]", 
                "Quels sont ces sujets ? [Les infections sexuellement transmissibles]", 
                "Quels sont ces sujets ? [La contraception et la contraception d'urgence]", 
                "Quels sont ces sujets ? [Le consentement]", "Quels sont ces sujets ? [L'anatomie]", 
                "Quels sont ces sujets ? [La législation]", "Quels sont ces sujets ? [Les violences sexistes et sexuelles]", 
                "Quels sont ces sujets ? [L'hygiène]", "Quels sont ces sujets ? [La sexualité LGBTQIA+]", 
                "Quels sont ces sujets ? [Le genre]", "Quels sont ces sujets ? [La pornographie]", 
                "Quels sont ces sujets ? [La cybersexualité (nudes, sexting, revenge porn, etc.)]", 
                "Quels sont ces sujets ? [L'égalité femme - homme]", "Quels sont ces sujets ? [Culture, religion et sexualité]", 
                "Quels sont ces sujets ? [Autre]", "Quel est le facteur limitant principal à la mise en place d'une formation initiale à l'éducation à la vie affective et sexuelle dans votre établissement ?", 
                "Quel est le facteur limitant principal à la mise en place d'une formation initiale à l'éducation à la vie affective et sexuelle dans votre établissement ? [Autre]", 
                "Existe-t-il une formation continue concernant l'éducation à la vie affective et sexuelle dispensée aux professeurs enseignant dans l'établissement ?", 
                "Est-elle proposée à tous les enseignants de l'établissement ?", 
                "Sous quelles conditions proposez-vous la formation continue ? [A la demande de l'enseignant]", 
                "Sous quelles conditions proposez-vous la formation continue ? [Selon la discipline enseignée]", 
                "Sous quelles conditions proposez-vous la formation continue ? [Suite à une formation initiale ]", 
                "Sous quelles conditions proposez-vous la formation continue ? [Autre]", 
                "Qui a été le dernier formateur en date ayant cumulé le plus d'heures de formation continue à destination des enseignants ?", 
                "Qui a été le dernier formateur en date ayant cumulé le plus d'heures de formation continue à destination des enseignants ? [Autre]", 
                "Sur quel rythme se base la formation continue ?", "Quels sont les trois sujets les plus fréquemment abordés ? [Comment aborder l'éducation à la vie affective et sexuelle en tant qu'enseignant]", 
                "Quels sont les trois sujets les plus fréquemment abordés ? [Les infections sexuellement transmissibles]", 
                "Quels sont les trois sujets les plus fréquemment abordés ? [La contraception et la contraception d'urgence]", 
                "Quels sont les trois sujets les plus fréquemment abordés ? [Le consentement]", 
                "Quels sont les trois sujets les plus fréquemment abordés ? [L'anatomie]", 
                "Quels sont les trois sujets les plus fréquemment abordés ? [La législation]", 
                "Quels sont les trois sujets les plus fréquemment abordés ? [Les violences sexistes et sexuelles]", 
                "Quels sont les trois sujets les plus fréquemment abordés ? [L'hygiène]", 
                "Quels sont les trois sujets les plus fréquemment abordés ? [La sexualité LGBTQIA+]", 
                "Quels sont les trois sujets les plus fréquemment abordés ? [Le genre]", 
                "Quels sont les trois sujets les plus fréquemment abordés ? [La pornographie]", 
                "Quels sont les trois sujets les plus fréquemment abordés ? [La cybersexualité (nudes, sexting, revenge porn, etc.)]", 
                "Quels sont les trois sujets les plus fréquemment abordés ? [L'égalité femme - homme]", 
                "Quels sont les trois sujets les plus fréquemment abordés ? [Culture, religion et sexualité]", 
                "Quels sont les trois sujets les plus fréquemment abordés ? [Autre]", 
                "Quel est le facteur limitant principal à la mise en place d'une formation continue à l'éducation à la vie affective et sexuelle ?", 
                "Quel est le facteur limitant principal à la mise en place d'une formation continue à l'éducation à la vie affective et sexuelle ? [Autre]"
)

descriptive = CreateTableOne(vars = variables.sup, data = prof, includeNA = TRUE,
                             factorVars = factors.sup)
print(descriptive, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)