#Charger des packages importants:
chooseCRANmirror()
install.packages("tidyverse")
library(tidyverse)
install.packages("Rcmdr")
library(Rcmdr)
install.packages("Rcpp")
library(Rcpp)
install.packages("ggplot2",dependencies = TRUE)
library(ggplot2)
library(survival)
library(dplyr)
install.packages("forestmodel")
library(forestmodel)
install.packages("randomForest")
install.packages("ggplot2")
install.packages("gtsummary")
library(gtsummary)
install.packages("GGally")
library(GGally)
install.packages("glmtoolbox")
library(glmtoolbox)
install.packages("performance")
library(performance)
install.packages("nom du package")
library(nom du package)

BronchioOHD=read.table("BronchioOHD.csv",sep=";",dec=",",head=TRUE,quote="",fill=TRUE)

# Visualisation des donnees:
 
str(BronchioOHD)
head(BronchioOHD)
summary(BronchioOHD)

#Categorisation des variables
BronchioOHD$Ageb=as.factor(BronchioOHD$Ageb)
BronchioOHD$Spo2b=as.factor(BronchioOHD$Spo2b)
BronchioOHD$FR.1=as.factor(BronchioOHD$FR.1)
BronchioOHD$TransfertREA=as.factor(BronchioOHD$TransfertREA)
BronchioOHD$Centre=as.factor(BronchioOHD$Centre)
BronchioOHD$USC=as.factor(BronchioOHD$USC)
BronchioOHD$Genre=as.factor(BronchioOHD$Genre)
BronchioOHD$Terme=as.factor(BronchioOHD$Terme)
BronchioOHD$PDN =as.factor(BronchioOHD$PDN)
BronchioOHD$PremiereBronchio =as.factor(BronchioOHD$PremiereBronchio)
BronchioOHD$DBP =as.factor(BronchioOHD$DBP)
BronchioOHD$Asthme =as.factor(BronchioOHD$Asthme)
BronchioOHD$Tabagisme =as.factor(BronchioOHD$Tabagisme)
BronchioOHD$Gemellaire =as.factor(BronchioOHD$Gemellaire)
BronchioOHD$CDP=as.factor(BronchioOHD$CDP)
BronchioOHD$atcdSV=as.factor(BronchioOHD$atcdSV)
BronchioOHD$AutreATCD =as.factor(BronchioOHD$AutreATCD)
BronchioOHD$Fievre =as.factor(BronchioOHD$Fievre)
BronchioOHD$ApneesMaison =as.factor(BronchioOHD$ApneesMaison)
BronchioOHD$CyanoseMaison =as.factor(BronchioOHD$CyanoseMaison)
BronchioOHD$MalaiseMaison =as.factor(BronchioOHD$MalaiseMaison)
BronchioOHD$FR =as.factor(BronchioOHD$FR)
BronchioOHD$FC =as.factor(BronchioOHD$FC)
BronchioOHD$PA=as.numeric(BronchioOHD$PA)
BronchioOHD$Temperature=as.factor(BronchioOHD$Temperature)
BronchioOHD$Auscultation =as.factor(BronchioOHD$Auscultation)
BronchioOHD$SDL =as.factor(BronchioOHD$SDL)
BronchioOHD$apneeHOP =as.factor(BronchioOHD$apneeHOP)
BronchioOHD$AEG =as.factor(BronchioOHD$AEG)
BronchioOHD$MalaiseHOP =as.factor(BronchioOHD$MalaiseHOP)
BronchioOHD$Spo2 =as.factor(BronchioOHD$Spo2)
BronchioOHD$CRP=as.factor(BronchioOHD$CRP)
BronchioOHD$virus =as.factor(BronchioOHD$virus)
BronchioOHD$Coinfection =as.factor(BronchioOHD$Coinfection)
BronchioOHD$NomVirus =as.factor(BronchioOHD$NomVirus)
BronchioOHD$RP =as.factor(BronchioOHD$RP)
BronchioOHD$RPb=as.factor(BronchioOHD$RPb)
BronchioOHD$WANG =as.factor(BronchioOHD$WANG)
BronchioOHD$Woodm =as.factor(BronchioOHD$Woodm)
BronchioOHD$SupportO2 =as.factor(BronchioOHD$SupportO2)
BronchioOHD$CTCsyst =as.factor(BronchioOHD$CTCsyst)
BronchioOHD$CTC =as.factor(BronchioOHD$CTC)
BronchioOHD$B2mim =as.factor(BronchioOHD$B2mim)
BronchioOHD$Adrenaline =as.factor(BronchioOHD$Adrenaline)
BronchioOHD$SHH =as.factor(BronchioOHD$SHH)
BronchioOHD$Kine =as.factor(BronchioOHD$Kine)
BronchioOHD$ATB =as.factor(BronchioOHD$ATB)
BronchioOHD$Remplissage =as.factor(BronchioOHD$Remplissage)
BronchioOHD$Jeun =as.factor(BronchioOHD$Jeun)
BronchioOHD$HydratTYPE=as.factor(BronchioOHD$HydratTYPE)
BronchioOHD$HIV =as.factor(BronchioOHD$HIV)
BronchioOHD$Alimentation =as.factor(BronchioOHD$Alimentation)
BronchioOHD$AlimentationOrale=as.factor(BronchioOHD$AlimentationOrale)
BronchioOHD$volume =as.factor(BronchioOHD$volume)
BronchioOHD$autre =as.factor(BronchioOHD$autre)
BronchioOHD$FC.2=as.factor(BronchioOHD$FC.2)
BronchioOHD$temperatureb =as.factor(BronchioOHD$temperatureb)
BronchioOHD$Auscultationb =as.factor(BronchioOHD$Auscultationb)
BronchioOHD$SDLb =as.factor(BronchioOHD$SDLb)
BronchioOHD$apneeb =as.factor(BronchioOHD$apneeb)
BronchioOHD$AEGb =as.factor(BronchioOHD$AEGb)
BronchioOHD$malaiseb =as.factor(BronchioOHD$malaiseb)
BronchioOHD$GDS =as.factor(BronchioOHD$GDS)
BronchioOHD$CRPb=as.factor(BronchioOHD$CRPb)
BronchioOHD$RPb =as.factor(BronchioOHD$RPb)
BronchioOHD$WangB =as.factor(BronchioOHD$WangB)
BronchioOHD$Woodb=as.factor(BronchioOHD$Woodb)
BronchioOHD$ROXI =as.numeric(BronchioOHD$ROXI)
BronchioOHD$Modif=as.factor(BronchioOHD$Modif)
BronchioOHD$CTCsystb =as.factor(BronchioOHD$CTCsystb)
BronchioOHD$CTCb =as.factor(BronchioOHD$CTCb)
BronchioOHD$B2mimb =as.factor(BronchioOHD$B2mimb)
BronchioOHD$Adrenalineb =as.factor(BronchioOHD$Adrenalineb)
BronchioOHD$SHHb =as.factor(BronchioOHD$SHHb)
BronchioOHD$Kineb =as.factor(BronchioOHD$Kineb)
BronchioOHD$ATBb =as.factor(BronchioOHD$ATBb)
BronchioOHD$Remplissageb =as.factor(BronchioOHD$Remplissageb)
BronchioOHD$Jeunb =as.factor(BronchioOHD$Jeunb)
BronchioOHD$HIVb =as.factor(BronchioOHD$HIVb)
BronchioOHD$Alimentationb =as.factor(BronchioOHD$Alimentationb)
BronchioOHD$AlimentationOraleb=as.factor(BronchioOHD$AlimentationOraleb)
BronchioOHD$HydratTYPEb =as.factor(BronchioOHD$HydratTYPEb)
BronchioOHD$volumeb =as.factor(BronchioOHD$volumeb)
BronchioOHD$Caffeine =as.factor(BronchioOHD$Caffeine)
BronchioOHD$OHD1L =as.factor(BronchioOHD$OHD1L)
BronchioOHD$OHD3L =as.factor(BronchioOHD$OHD3L)
BronchioOHD$Autre =as.factor(BronchioOHD$Autre)
BronchioOHD$HydratTYPE =as.factor(BronchioOHD$HydratTYPE)
BronchioOHD$AppelREA =as.factor(BronchioOHD$AppelREA)
BronchioOHD$MotifREA =as.factor(BronchioOHD$MotifREA)
BronchioOHD$Escalade =as.factor(BronchioOHD$Escalade)
BronchioOHD$Relai =as.factor(BronchioOHD$Relai)
BronchioOHD$ModificationTH =as.factor(BronchioOHD$ModificationTH)
BronchioOHD$CTCsystc =as.factor(BronchioOHD$CTCsystc)
BronchioOHD$CTCc =as.factor(BronchioOHD$CTCc)
BronchioOHD$B2mimc =as.factor(BronchioOHD$B2mimc)
BronchioOHD$Adrenalinec =as.factor(BronchioOHD$Adrenalinec)
BronchioOHD$SHHc =as.factor(BronchioOHD$SHHc)
BronchioOHD$Kinec =as.factor(BronchioOHD$Kinec)
BronchioOHD$ATBc =as.factor(BronchioOHD$ATBc)
BronchioOHD$Remplissagec =as.factor(BronchioOHD$Remplissagec)
BronchioOHD$Jeunc=as.factor(BronchioOHD$Jeunc)
BronchioOHD$HIVc=as.factor(BronchioOHD$HIVc)
BronchioOHD$Alimentationc=as.factor(BronchioOHD$Alimentationc)
BronchioOHD$AlimentationOralec=as.factor(BronchioOHD$AlimentationOralec)
BronchioOHD$HydratTYPEc=as.factor(BronchioOHD$HydratTYPEc)
BronchioOHD$volumec=as.factor(BronchioOHD$volumec)
BronchioOHD$poursuiteOHD=as.factor(BronchioOHD$poursuiteOHD)
BronchioOHD$VNI=as.factor(BronchioOHD$VNI)
BronchioOHD$VNIrea=as.factor(BronchioOHD$VNIrea)
BronchioOHD$modaliteVNI=as.factor(BronchioOHD$modaliteVNI)
BronchioOHD$escaladeVNI=as.factor(BronchioOHD$escaladeVNI)
BronchioOHD$intubation=as.factor(BronchioOHD$intubation)
BronchioOHD$Relai=as.factor(BronchioOHD$Relai)
BronchioOHD$dureedureeTOTOHD=as.numeric(BronchioOHD$dureeTOTOHD)
BronchioOHD$dureeVNI=as.numeric(BronchioOHD$dureeVNI)
BronchioOHD$dureeVI=as.numeric(BronchioOHD$dureeVI)
BronchioOHD$dureedureeTOTVent=as.numeric(BronchioOHD$dureeTOTVent)
BronchioOHD$dureeJEUN=as.numeric(BronchioOHD$dureeJEUN)
BronchioOHD$dureeHIV=as.numeric(BronchioOHD$dureeHIV)
BronchioOHD$dureeSNG =as.numeric(BronchioOHD$dureeSNG)
BronchioOHD$dureeSejour=as.numeric(BronchioOHD$dureeSejour)
BronchioOHD$VSMAX=as.factor(BronchioOHD$VSMAX)


summary(BronchioOHD)

#7e etape: utilisation du jeu de donnees et tests statistiques
#( BronchioOHD$Centre)
tt=table(BronchioOHD$Centre)
tt/sum(tt)*100
table(BronchioOHD$Centre,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Centre,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$Centre,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Centre,BronchioOHD$TransfertREA))

#(BronchioOHD$USC)
tt=table(BronchioOHD$USC)
tt/sum(tt)*100
table(BronchioOHD$USC,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$USC,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$USC,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$USC,BronchioOHD$TransfertREA))

# BronchioOHD$Genre
tt=table(BronchioOHD$Genre)
tt/sum(tt)*100
table(BronchioOHD$Genre,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Genre,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Genre,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Genre,BronchioOHD$TransfertREA))

# BronchioOHD$Terme
tt=table(BronchioOHD$Terme)
tt/sum(tt)*100
table(BronchioOHD$Terme,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Terme,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Terme,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Terme,BronchioOHD$TransfertREA))

# BronchioOHD$PDN
tt=table(BronchioOHD$PDN)
tt/sum(tt)*100
table(BronchioOHD$PDN,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$PDN,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$PDN,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$PDN,BronchioOHD$TransfertREA))

# BronchioOHD$PremiereBronchio 
tt=table(BronchioOHD$PremiereBronchio)
tt/sum(tt)*100
table(BronchioOHD$PremiereBronchio,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$PremiereBronchio,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$PremiereBronchio,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$PremiereBronchio,BronchioOHD$TransfertREA

# BronchioOHD$DBP
tt=table(BronchioOHD$DBP)
tt/sum(tt)*100
table(BronchioOHD$DBP,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$DBP,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$DBP,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$DBP,BronchioOHD$TransfertREA))

# BronchioOHD$Asthme tt=table(BronchioOHD$comorbid_respiratoire)
tt/sum(tt)*100
table(BronchioOHD$Asthme,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Asthme,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Asthme,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Asthme,BronchioOHD$TransfertREA))

# BronchioOHD$Tabagisme)
tt=table(BronchioOHD$Tabagisme)
tt/sum(tt)*100
table(BronchioOHD$Tabagisme,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Tabagisme,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Tabagisme,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Tabagisme,BronchioOHD$TransfertREA))

# BronchioOHD$Gemellaire 
tt=table(BronchioOHD$Gemellaire)
tt/sum(tt)*100
table(BronchioOHD$Gemellaire,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Gemellaire,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Gemellaire,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Gemellaire,BronchioOHD$TransfertREA))

# BronchioOHD$CDP
tt=table(BronchioOHD$CDP)
tt/sum(tt)*100
table(BronchioOHD$CDP,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$CDP,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$CDP,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$CDP,BronchioOHD$TransfertREA))

# BronchioOHD$atcdSV 
tt=table(BronchioOHD$atcdSV)
tt/sum(tt)*100
table(BronchioOHD$atcdSV,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$atcdSV,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$atcdSV,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$atcdSV,BronchioOHD$TransfertREA))

# BronchioOHD$AutreATCD
tt=table(BronchioOHD$AutreATCD)
tt/sum(tt)*100
table(BronchioOHD$AutreATCD,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$comorbid_neuro,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$AutreATCD,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$AutreATCD,BronchioOHD$TransfertREA))

# BronchioOHD$Fievre
tt=table(BronchioOHD$Fievre)
tt/sum(tt)*100
table(BronchioOHD$Fievre,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Fievre,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Fievre,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Fievrev,BronchioOHD$TransfertREA))

# BronchioOHD$ApneesMaison
tt=table(BronchioOHD$ApneesMaison)
tt/sum(tt)*100
table(BronchioOHD$ApneesMaison,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$ApneesMaison,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$ApneesMaison,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$ApneesMaison,BronchioOHD$TransfertREA))

# BronchioOHD$CyanoseMaison
tt=table(BronchioOHD$CyanoseMaison)
tt/sum(tt)*100
table(BronchioOHD$CyanoseMaison,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$CyanoseMaison,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$CyanoseMaison,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$CyanoseMaison,BronchioOHD$TransfertREA))

# BronchioOHD$MalaiseMaison
tt=table(BronchioOHD$MalaiseMaison)
tt/sum(tt)*100
table(BronchioOHD$MalaiseMaison,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$MalaiseMaison,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$MalaiseMaison,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$MalaiseMaison,BronchioOHD$TransfertREA))

# BronchioOHD$FR
tt=table(BronchioOHD$FR)
tt/sum(tt)*100
table(BronchioOHD$FR,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$FR,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$FR,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$FR,BronchioOHD$TransfertREA))

# BronchioOHD$FC 
tt=table(BronchioOHD$FC)
tt/sum(tt)*100
table(BronchioOHD$FC,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$FC,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$FC,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$FC,BronchioOHD$TransfertREA))

# BronchioOHD$Temperature
tt=table(BronchioOHD$Temperature)
tt/sum(tt)*100
table(BronchioOHD$Temperature,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Temperature,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Temperature,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Temperature,BronchioOHD$TransfertREA))

# BronchioOHD$Auscultation
tt=table(BronchioOHD$Auscultation)
tt/sum(tt)*100
table(BronchioOHD$Auscultation,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Auscultation,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Auscultation,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Auscultation,BronchioOHD$TransfertREA))

# BronchioOHD$SDL 
tt=table(BronchioOHD$SDL)
tt/sum(tt)*100
table(BronchioOHD$SDL,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$SDL,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$SDL,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$SDL,BronchioOHD$TransfertREA))

# BronchioOHD$apneeHOP
tt=table(BronchioOHD$apneeHOP)
tt/sum(tt)*100
table(BronchioOHD$apneeHOP,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$apneeHOP,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$apneeHOP,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$apneeHOP,BronchioOHD$TransfertREA))

# BronchioOHD$AEG
tt=table(BronchioOHD$AEG)
tt/sum(tt)*100
table (BronchioOHD$AEG,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$AEG,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$AEG,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$AEG,BronchioOHD$TransfertREA))

# BronchioOHD$MalaiseHOP
tt=table(BronchioOHD$MalaiseHOP)
tt/sum(tt)*100
table(BronchioOHD$MalaiseHOP,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$MalaiseHOP,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)

# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$MalaiseHOP,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$MalaiseHOP,BronchioOHD$TransfertREA))

# BronchioOHD$Spo2*
tt=table(BronchioOHD$Spo2)
tt/sum(tt)*100
table(BronchioOHD$Spo2,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Spo2,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Spo2,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Spo2,BronchioOHD$TransfertREA))

# BronchioOHD$CRP
tt=table(BronchioOHD$CRP)
tt/sum(tt)*100
table(BronchioOHD$CRP,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$CRP,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$CRP,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$CRP,BronchioOHD$TransfertREA))

# BronchioOHD$virus 
tt=table(BronchioOHD$virus)
tt/sum(tt)*100
table(BronchioOHD$virus,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$virus,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
r# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$virus,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$virus,BronchioOHD$TransfertREA))

# BronchioOHD$Coinfection
tt=table(BronchioOHD$Coinfection)
tt/sum(tt)*100
table(BronchioOHD$Coinfection,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Coinfection,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Coinfection,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Coinfection,BronchioOHD$TransfertREA))

# BronchioOHD$RP
tt=table(BronchioOHD$RP)
tt/sum(tt)*100
table(BronchioOHD$RP,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$RP,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$RP,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$RP,BronchioOHD$TransfertREA))

# BronchioOHD$RPb
tt=table(BronchioOHD$RPb)
tt/sum(tt)*100
table(BronchioOHD$RPb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$RPb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$RPb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$RPb,BronchioOHD$TransfertREA))


# BronchioOHD$WANG
tt=table(BronchioOHD$WANG)
tt/sum(tt)*100
table(BronchioOHD$WANG,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$WANG,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$WANG,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$WANG,BronchioOHD$TransfertREA)) 



#( BronchioOHD$Woodm)
tt=table(BronchioOHD$Woodm)
tt/sum(tt)*100
table(BronchioOHD$Woodm,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Woodm,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Woodm,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Woodm,BronchioOHD$TransfertREA))

#( BronchioOHD$SupportO2)
tt=table(BronchioOHD$SupportO2)
tt/sum(tt)*100
table(BronchioOHD$SupportO2,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$SupportO2,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$SupportO2,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$SupportO2,BronchioOHD$TransfertREA))

# BronchioOHD$CTCsyst
tt=table(BronchioOHD$CTCsyst)
tt/sum(tt)*100
table(BronchioOHD$CTCsyst,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$CTCsyst,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$CTCsyst,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$CTCsyst,BronchioOHD$TransfertREA))

# BronchioOHD$CTC
tt=table(BronchioOHD$CTC)
tt/sum(tt)*100
table(BronchioOHD$CTC,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$CTC,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$CTC,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$CTC,BronchioOHD$TransfertREA))

# BronchioOHD$B2mim
tt=table(BronchioOHD$B2mim)
tt/sum(tt)*100
table(BronchioOHD$B2mim,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$B2mim,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$B2mim,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$B2mim,BronchioOHD$TransfertREA))

# BronchioOHD$Adrenaline
tt=table(BronchioOHD$Adrenaline)
tt/sum(tt)*100
table(BronchioOHD$Adrenaline,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Adrenaline,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Adrenaline,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Adrenaline,BronchioOHD$TransfertREA

# BronchioOHD$SHH
tt=table(BronchioOHD$SHH)
tt/sum(tt)*100
table(BronchioOHD$SHH,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$SHH,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$SHH,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$SHH,BronchioOHD$TransfertREA))

# BronchioOHD$Kine 
tt=table(BronchioOHD$Kine)
tt/sum(tt)*100
table(BronchioOHD$Kine,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Kine,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Kine,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Kine,BronchioOHD$TransfertREA))

# BronchioOHD$ATB)
tt=table(BronchioOHD$ATB)
tt/sum(tt)*100
table(BronchioOHD$ATB,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$ATB,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$ATB,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$ATB,BronchioOHD$TransfertREA))

# BronchioOHD$Remplissage
tt=table(BronchioOHD$Remplissage)
tt/sum(tt)*100
table(BronchioOHD$Remplissage,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Remplissage,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Remplissage,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Remplissage,BronchioOHD$TransfertREA))

# BronchioOHD$Jeun
tt=table(BronchioOHD$Jeun)
tt/sum(tt)*100
table(BronchioOHD$Jeun,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Jeun,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Jeun,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Jeun,BronchioOHD$TransfertREA))

# BronchioOHD$HIV
tt=table(BronchioOHD$HIV)
tt/sum(tt)*100
table(BronchioOHD$HIV,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$HIV,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$HIV,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$HIV,BronchioOHD$TransfertREA))

# BronchioOHD$HydratTYPE
tt=table(BronchioOHD$HydratTYPE)
tt/sum(tt)*100
table(BronchioOHD$HydratTYPE,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$HydratTYPE,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$HydratTYPE,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$HydratTYPE,BronchioOHD$TransfertREA))

# BronchioOHD$Alimentation
tt=table(BronchioOHD$Alimentation)
tt/sum(tt)*100
table(BronchioOHD$Alimentation,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Alimentation,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table BronchioOHD$Alimentation,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Alimentation,BronchioOHD$TransfertREA))

# BronchioOHD$AlimentationOrale
tt=table(BronchioOHD$AlimentationOrale)
tt/sum(tt)*100
table(BronchioOHD$AlimentationOrale,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$AlimentationOrale,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table BronchioOHD$AlimentationOrale,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$AlimentationOrale,BronchioOHD$TransfertREA))

# BronchioOHD$volume
tt=table(BronchioOHD$volume)
tt/sum(tt)*100
table(BronchioOHD$volume,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$volume,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$volume,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$volume,BronchioOHD$TransfertREA))

#(BronchioOHD$HydratTYPE)
tt=table(BronchioOHD$HydratTYPE)
tt/sum(tt)*100
table(BronchioOHD$HydratTYPE,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$HydratTYPE,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$HydratTYPE,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$HydratTYPE,BronchioOHD$TransfertREA))

# BronchioOHD$autre
tt=table(BronchioOHD$autre)
tt/sum(tt)*100
table(BronchioOHD$autre,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$autre,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$autre,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$autre,BronchioOHD$TransfertREA))

# BronchioOHD$FC.2 
tt=table(BronchioOHD$FC.2)
tt/sum(tt)*100
table(BronchioOHD$FC.2,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$FC.2,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$FC.2,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$FC.2,BronchioOHD$TransfertREA))

# BronchioOHD$temperatureb)
tt=table(BronchioOHD$temperatureb)
tt/sum(tt)*100
table(BronchioOHD$temperatureb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$temperatureb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$temperatureb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$temperatureb,BronchioOHD$TransfertREA))

# BronchioOHD$Auscultationb
tt=table(BronchioOHD$Auscultationb)
tt/sum(tt)*100
table(BronchioOHD$Auscultationb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Auscultationb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Auscultationb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Auscultationb,BronchioOHD$TransfertREA))

# BronchioOHD$SDLb 
tt=table(BronchioOHD$SDLb)
tt/sum(tt)*100
table(BronchioOHD$SDLb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$SDLb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$SDLb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$SDLb,BronchioOHD$TransfertREA))

# BronchioOHD$apneeb
tt=table(BronchioOHD$apneeb)
tt/sum(tt)*100
table(BronchioOHD$apneeb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$apneeb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$apneeb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$apneeb,BronchioOHD$TransfertREA))

# BronchioOHD$AEGb
tt=table(BronchioOHD$AEGb)
tt/sum(tt)*100
table (BronchioOHD$AEGb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$AEGb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$AEGb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$AEGb,BronchioOHD$TransfertREA))

# BronchioOHD$malaiseb
tt=table(BronchioOHD$malaiseb)
tt/sum(tt)*100
table(BronchioOHD$malaiseb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$malaiseb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$malaiseb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$malaiseb,BronchioOHD$TransfertREA))

# BronchioOHD$GDS*
tt=table(BronchioOHD$ GDS)
tt/sum(tt)*100
table(BronchioOHD$ GDS,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$GDS,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$GDS,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$GDS,BronchioOHD$TransfertREA))

# BronchioOHD$CRPb
tt=table(BronchioOHD$CRPb)
tt/sum(tt)*100
table(BronchioOHD$CRPb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$CRPb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$CRPb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$CRPb,BronchioOHD$TransfertREA))

# BronchioOHD$RPOHD
tt=table(BronchioOHD$RPOHD)
tt/sum(tt)*100
table(BronchioOHD$RPOHD,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$RPOHD,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$RPOHD,BronchioOHD$TransfertREA))

# BronchioOHD$RPOHDb
tt=table(BronchioOHD$RPOHDb)
tt/sum(tt)*100
table(BronchioOHD$RPOHDb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$RPOHDb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$RPOHDb,BronchioOHD$TransfertREA))

# BronchioOHD$WangB
tt=table(BronchioOHD$WangB)
tt/sum(tt)*100
table(BronchioOHD$WangB,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$WangB,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$WangB,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$WangB,BronchioOHD$TransfertREA))

#( BronchioOHD$Woodb)
tt=table(BronchioOHD$Woodb)
tt/sum(tt)*100
table(BronchioOHD$Woodb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Woodb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Woodb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Woodb,BronchioOHD$TransfertREA))

# BronchioOHD$RPOHD
tt=table(BronchioOHD$RPOHD)
tt/sum(tt)*100
table(BronchioOHD$RPOHD,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$RPOHD,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$RPOHD,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$RP,BronchioOHD$TransfertREA))

# BronchioOHD$RPOHDb
tt=table(BronchioOHD$RPOHDb)
tt/sum(tt)*100
table(BronchioOHD$RPOHDb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$RPOHDb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$RPOHDb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$RPOHDb,BronchioOHD$TransfertREA))


#( BronchioOHD$Modif)
tt=table(BronchioOHD$Modif)
tt/sum(tt)*100
table(BronchioOHD$Modif,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Modif,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Modif,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Modif,BronchioOHD$TransfertREA))

# BronchioOHD$CTCsystb
tt=table(BronchioOHD$CTCsystb)
tt/sum(tt)*100
table(BronchioOHD$CTCsystb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$CTCsystb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$CTCsystb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$CTCsystb,BronchioOHD$TransfertREA))

# BronchioOHD$CTCb
tt=table(BronchioOHD$CTCb)
tt/sum(tt)*100
table(BronchioOHD$CTCb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$CTCb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$CTCb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$CTCb,BronchioOHD$TransfertREA))

# BronchioOHD$B2mimb
tt=table(BronchioOHD$B2mimb)
tt/sum(tt)*100
table(BronchioOHD$B2mimb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$B2mimb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$B2mimb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$B2mimb,BronchioOHD$TransfertREA))

# BronchioOHD$Adrenalineb
tt=table(BronchioOHD$Adrenalineb)
tt/sum(tt)*100
table(BronchioOHD$Adrenalineb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Adrenalineb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Adrenalineb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Adrenalineb,BronchioOHD$TransfertREA

# BronchioOHD$SHHb
tt=table(BronchioOHD$SHHb)
tt/sum(tt)*100
table(BronchioOHD$SHHb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$SHHb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$SHHb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$SHHb,BronchioOHD$TransfertREA))

# BronchioOHD$Kineb 
tt=table(BronchioOHD$Kineb)
tt/sum(tt)*100
table(BronchioOHD$Kineb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Kineb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Kineb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Kineb,BronchioOHD$TransfertREA))

# BronchioOHD$ATBb)
tt=table(BronchioOHD$ATBb)
tt/sum(tt)*100
table(BronchioOHD$ATBb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$ATBb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$ATBb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$ATBb,BronchioOHD$TransfertREA))

# BronchioOHD$Remplissageb
tt=table(BronchioOHD$Remplissageb)
tt/sum(tt)*100
table(BronchioOHD$Remplissageb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Remplissageb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Remplissageb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Remplissageb,BronchioOHD$TransfertREA))

# BronchioOHD$Jeunb
tt=table(BronchioOHD$Jeunb)
tt/sum(tt)*100
table(BronchioOHD$Jeunb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Jeunb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Jeunb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Jeunb,BronchioOHD$TransfertREA))

# BronchioOHD$HIVb
tt=table(BronchioOHD$HIVb)
tt/sum(tt)*100
table(BronchioOHD$HIVb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$HIVb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$HIVb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$HIVb,BronchioOHD$TransfertREA))

# BronchioOHD$Alimentationb
tt=table(BronchioOHD$Alimentationb)
tt/sum(tt)*100
table(BronchioOHD$Alimentationb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Alimentationb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table BronchioOHD$Alimentationb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Alimentationb,BronchioOHD$TransfertREA))

# BronchioOHD$AlimentationOraleb
tt=table(BronchioOHD$AlimentationOraleb)
tt/sum(tt)*100
table(BronchioOHD$AlimentationOraleb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$AlimentationOraleb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table BronchioOHD$AlimentationOraleb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$AlimentationOraleb,BronchioOHD$TransfertREA))


# BronchioOHD$volumeb
tt=table(BronchioOHD$volumeb)
tt/sum(tt)*100
table(BronchioOHD$volumeb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$volumeb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$volumeb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$volumeb,BronchioOHD$TransfertREA))

#(BronchioOHD$HydratTYPEb)
tt=table(BronchioOHD$HydratTYPEb)
tt/sum(tt)*100
table(BronchioOHD$HydratTYPEb,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$HydratTYPEb,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$HydratTYPEb,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$HydratTYPEb,BronchioOHD$TransfertREA))

#( BronchioOHD$Caffeine)
tt=table(BronchioOHD$Caffeine)
tt/sum(tt)*100
table(BronchioOHD$Caffeine,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Caffeine,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$Caffeine,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Caffeine,BronchioOHD$TransfertREA))

#( BronchioOHD$OHD1L)
tt=table(BronchioOHD$OHD1L)
tt/sum(tt)*100
table(BronchioOHD$OHD1L,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$OHD1L,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$OHD1L,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$OHD1L,BronchioOHD$TransfertREA))

#( BronchioOHD$OHD3L)
tt=table(BronchioOHD$OHD3L)
tt/sum(tt)*100
table(BronchioOHD$OHD3L,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$OHD3L,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$OHD3L,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$OHD3L,BronchioOHD$TransfertREA))

# BronchioOHD$Autre
tt=table(BronchioOHD$Autre)
tt/sum(tt)*100
table(BronchioOHD$Autre,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Autre,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$Autre,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Autre,BronchioOHD$TransfertREA))


#variable quantitative ou continue:

# BronchioOHD$Dureesymptomes
shapiro.test(BronchioOHD$Dureesymptomes) 
hist(BronchioOHD$Dureesymptomes)
aggregate(BronchioOHD$Dureesymptomes~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$Dureesymptomes~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$Age
shapiro.test(BronchioOHD$Age) 
hist(BronchioOHD$Age)
aggregate(BronchioOHD$Age~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$Age~BronchioOHD$TransfertREA, alternative = 'greater')

#BronchioOHD$Poids
shapiro.test(BronchioOHD$Poids) 
hist(BronchioOHD$Poids)
aggregate(BronchioOHD$Poids~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$Poids~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$PA
shapiro.test(BronchioOHD$PA) 
hist(BronchioOHD$PA)
aggregate(BronchioOHD$PA~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$PA~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$pH
shapiro.test(BronchioOHD$pH) 
hist(BronchioOHD$pH)
aggregate(BronchioOHD$pH~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$pH~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$PCO2
shapiro.test(BronchioOHD$PCO2) 
hist(BronchioOHD$PCO2)
aggregate(BronchioOHD$PCO2~BronchioOHD$TransfertREA,FUN=summary)
t.test (BronchioOHD$PCO2~BronchioOHD$TransfertREA, alternative = 'greater')
aggregate(BronchioOHD$PCO2~BronchioOHD$TransfertREA,FUN=sd)
sd(BronchioOHD$PCO2,na.rm=TRUE)


# BronchioOHD$HCO3
shapiro.test(BronchioOHD$HCO3) 
hist(BronchioOHD$HCO3)
aggregate(BronchioOHD$HCO3~BronchioOHD$TransfertREA,FUN=sd)
t.test(BronchioOHD$HCO3~BronchioOHD$TransfertREA, alternative = 'greater')
sd(BronchioOHD$HCO3,na.rm=TRUE)

# BronchioOHD$lactates
shapiro.test(BronchioOHD$lactates) 
hist(BronchioOHD$lactates)
aggregate(BronchioOHD$lactates~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$lactates~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$Na
shapiro.test(BronchioOHD$Na) 
hist(BronchioOHD$Na)
aggregate(BronchioOHD$Na~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$Na~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$PCT
shapiro.test(BronchioOHD$PCT) 
hist(BronchioOHD$PCT)
aggregate(BronchioOHD$PCT~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$PCT~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$Hb
shapiro.test(BronchioOHD$Hb) 
hist(BronchioOHD$Hb)
aggregate(BronchioOHD$Hb~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$Hb~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$leucocytes
shapiro.test(BronchioOHD$leucocytes) 
hist(BronchioOHD$leucocytes)
aggregate(BronchioOHD$leucocytes~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$leucocytes~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$leucocytes
shapiro.test(BronchioOHD$leucocytes) 
hist(BronchioOHD$leucocytes)
aggregate(BronchioOHD$leucocytes~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$leucocytes~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$PNN
shapiro.test(BronchioOHD$PNN) 
hist(BronchioOHD$PNN)
aggregate(BronchioOHD$PNN~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$PNN~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$lymphocytes
shapiro.test(BronchioOHD$lymphocytes) 
hist(BronchioOHD$lymphocytes)
aggregate(BronchioOHD$lymphocytes~BronchioOHD$TransfertREA,FUN=summary)
t.test (BronchioOHD$lymphocytes~BronchioOHD$TransfertREA, alternative = 'greater')
sd(BronchioOHD$lymphocytes,na.rm=TRUE)
aggregate(BronchioOHD$lymphocytes~BronchioOHD$TransfertREA,FUN=sd)

# BronchioOHD$FR.1
shapiro.test(BronchioOHD$FR.1) 
hist(BronchioOHD$FR.1)
aggregate(BronchioOHD$FR.1~BronchioOHD$TransfertREA,FUN=sd)
t.test (BronchioOHD$FR.1~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$Spo2b
shapiro.test(BronchioOHD$Spo2b) 
hist(BronchioOHD$Spo2b)
aggregate(BronchioOHD$Spo2b~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$Spo2b~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$Fi02
shapiro.test(BronchioOHD$Fi02) 
hist(BronchioOHD$Fi02)
aggregate(BronchioOHD$Fi02~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$Fi02~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$pHb
shapiro.test(BronchioOHD$pHb) 
hist(BronchioOHD$pHb)
aggregate(BronchioOHD$pHb~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$pHb~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$PCO2b
shapiro.test(BronchioOHD$PCO2b) 
hist(BronchioOHD$PCO2b)
aggregate(BronchioOHD$PCO2b~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$PCO2b~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$HCO3b
shapiro.test(BronchioOHD$HCO3b) 
hist(BronchioOHD$HCO3b)
aggregate(BronchioOHD$HCO3b~BronchioOHD$TransfertREA,FUN=sd)
t.test(BronchioOHD$HCO3b~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$lactatesb
shapiro.test(BronchioOHD$lactatesb) 
hist(BronchioOHD$lactatesb)
aggregate(BronchioOHD$lactatesb~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$lactatesb~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$Nab
shapiro.test(BronchioOHD$Nab) 
hist(BronchioOHD$Nab)
aggregate(BronchioOHD$Nab~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$Nab~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$PCTb
shapiro.test(BronchioOHD$PCTb) 
hist(BronchioOHD$PCTb)
aggregate(BronchioOHD$PCTb~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$PCTb~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$Hbb
shapiro.test(BronchioOHD$Hbb) 
hist(BronchioOHD$Hbb)
aggregate(BronchioOHD$Hbb~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$Hbb~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$leucocytesb
shapiro.test(BronchioOHD$leucocytesb) 
hist(BronchioOHD$leucocytesb)
aggregate(BronchioOHD$leucocytesb~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$leucocytesb~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$leucocytesb
shapiro.test(BronchioOHD$leucocytesb) 
hist(BronchioOHD$leucocytesb)
aggregate(BronchioOHD$leucocytesb~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$leucocytesb~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$PNNb
shapiro.test(BronchioOHD$PNNb) 
hist(BronchioOHD$PNNb)
aggregate(BronchioOHD$PNNb~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$PNNb~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$lymphocytes
shapiro.test(BronchioOHD$lymphocytesb) 
hist(BronchioOHD$lymphocytesb)
aggregate(BronchioOHD$lymphocytesb~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$lymphocytesb~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$DelaiOHD
shapiro.test(BronchioOHD$DelaiOHD) 
hist(BronchioOHD$DelaiOHD)
aggregate(BronchioOHD$DelaiOHD~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$DelaiOHD~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$ROXI
shapiro.test(BronchioOHD$ROXI) 
hist(BronchioOHD$ROXI)
aggregate(BronchioOHD$ROXI~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$ROXI~BronchioOHD$TransfertREA, alternative = 'greater')

#TABLEAU 4

BronchioOHD$AppelREA =as.factor(BronchioOHD$AppelREA)
BronchioOHD$MotifREA =as.factor(BronchioOHD$MotifREA)
BronchioOHD$Escalade =as.factor(BronchioOHD$Escalade)
BronchioOHD$Relai =as.factor(BronchioOHD$Relai)
BronchioOHD$ModificationTH =as.factor(BronchioOHD$ModificationTH)
BronchioOHD$CTCsystc =as.factor(BronchioOHD$CTCsystc)
BronchioOHD$CTCc =as.factor(BronchioOHD$CTCc)
BronchioOHD$B2mimc =as.factor(BronchioOHD$B2mimc)
BronchioOHD$Adrenalinec =as.factor(BronchioOHD$Adrenalinec)
BronchioOHD$SHHc =as.factor(BronchioOHD$SHHc)
BronchioOHD$Kinec =as.factor(BronchioOHD$Kinec)
BronchioOHD$ATBc =as.factor(BronchioOHD$ATBc)
BronchioOHD$Remplissagec =as.factor(BronchioOHD$Remplissagec)
BronchioOHD$Jeunc=as.factor(BronchioOHD$Jeunc)
BronchioOHD$HIVc=as.factor(BronchioOHD$HIVc)
BronchioOHD$Alimentationc=as.factor(BronchioOHD$Alimentationc)
BronchioOHD$AlimentationOralec=as.factor(BronchioOHD$AlimentationOralec)
BronchioOHD$HydratTYPEc=as.factor(BronchioOHD$HydratTYPEc)
BronchioOHD$volumec=as.factor(BronchioOHD$volumec)
BronchioOHD$poursuiteOHD=as.factor(BronchioOHD$poursuiteOHD)
BronchioOHD$VNI=as.factor(BronchioOHD$VNI)
BronchioOHD$VNIrea=as.factor(BronchioOHD$VNIrea)
BronchioOHD$modaliteVNI=as.factor(BronchioOHD$modaliteVNI)
BronchioOHD$escaladeVNI=as.factor(BronchioOHD$escaladeVNI)
BronchioOHD$intubation=as.factor(BronchioOHD$intubation)
BronchioOHD$Relai=as.factor(BronchioOHD$Relai)
BronchioOHD$dureedureeTOTOHD=as.numeric(BronchioOHD$dureeTOTOHD)
BronchioOHD$dureeVNI=as.numeric(BronchioOHD$dureeVNI)
BronchioOHD$dureeVI=as.numeric(BronchioOHD$dureeVI)
BronchioOHD$dureedureeTOTVent=as.numeric(BronchioOHD$dureeTOTVent)
BronchioOHD$dureeJEUN=as.numeric(BronchioOHD$dureeJEUN)
BronchioOHD$dureeHIV=as.numeric(BronchioOHD$dureeHIV)
BronchioOHD$dureeSNG =as.numeric(BronchioOHD$dureeSNG)
BronchioOHD$dureeSejour=as.numeric(BronchioOHD$dureeSejour)
BronchioOHD$VSMAX=as.factor(BronchioOHD$VSMAX)

# BronchioOHD$AppelREA
tt=table(BronchioOHD$AppelREA)
tt/sum(tt)*100
table(BronchioOHD$AppelREA,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$AppelREA,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$AppelREA,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$AppelREA,BronchioOHD$TransfertREA))

# BronchioOHD$MotifREA
tt=table(BronchioOHD$MotifREA)
tt/sum(tt)*100
table(BronchioOHD$MotifREA,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$MotifREA,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$MotifREA,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$MotifREA,BronchioOHD$TransfertREA))

# BronchioOHD$Escalade
tt=table(BronchioOHD$Escalade)
tt/sum(tt)*100
table(BronchioOHD$Escalade,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Escalade,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table BronchioOHD$Escalade,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Escalade,BronchioOHD$TransfertREA))

# BronchioOHD$Relai 
tt=table(BronchioOHD$Relai)
tt/sum(tt)*100
table(BronchioOHD$Relai,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Relai,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$Relai,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Relai,BronchioOHD$TransfertREA))

# BronchioOHD$CTCsystc
tt=table(BronchioOHD$CTCsystc)
tt/sum(tt)*100
table(BronchioOHD$CTCsystc,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$CTCsystc,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$CTCsystc,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$CTCsystc,BronchioOHD$TransfertREA))

# BronchioOHD$CTCc
tt=table(BronchioOHD$CTCc)
tt/sum(tt)*100
table(BronchioOHD$CTCc,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$CTCc,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$CTCc,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$CTCc,BronchioOHD$TransfertREA))

# BronchioOHD$B2mimc
tt=table(BronchioOHD$B2mimc)
tt/sum(tt)*100
table(BronchioOHD$B2mimc,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$B2mimc,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$B2mimc,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$B2mimc,BronchioOHD$TransfertREA))

# BronchioOHD$Adrenalinec
tt=table(BronchioOHD$Adrenalinec)
tt/sum(tt)*100
table(BronchioOHD$Adrenalinec,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Adrenalinec,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$Adrenalinec,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Adrenalinec,BronchioOHD$TransfertREA

# BronchioOHD$SHHc
tt=table(BronchioOHD$SHHc)
tt/sum(tt)*100
table(BronchioOHD$SHHc,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$SHHc,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$SHHc,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$SHHc,BronchioOHD$TransfertREA))

# BronchioOHD$Kinec
tt=table(BronchioOHD$Kinec)
tt/sum(tt)*100
table(BronchioOHD$Kinec,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Kinec,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$Kinec,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Kinec,BronchioOHD$TransfertREA))

# BronchioOHD$ATBc)
tt=table(BronchioOHD$ATBc)
tt/sum(tt)*100
table(BronchioOHD$ATBc,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$ATBc,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$ATBc,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$ATBc,BronchioOHD$TransfertREA))

# BronchioOHD$Remplissagec
tt=table(BronchioOHD$Remplissagec)
tt/sum(tt)*100
table(BronchioOHD$Remplissagec,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Remplissagec,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$Remplissagec,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Remplissagec,BronchioOHD$TransfertREA))

# BronchioOHD$Jeunc
tt=table(BronchioOHD$Jeunc)
tt/sum(tt)*100
table(BronchioOHD$Jeunc,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Jeunc,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$Jeunc,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Jeunc,BronchioOHD$TransfertREA))

# BronchioOHD$HIVc
tt=table(BronchioOHD$HIVc)
tt/sum(tt)*100
table(BronchioOHD$HIVc,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$HIVc,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)

# BronchioOHD$Alimentationc
tt=table(BronchioOHD$Alimentationc)
tt/sum(tt)*100
table(BronchioOHD$Alimentationc,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Alimentationc,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table BronchioOHD$Alimentationc,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Alimentationc,BronchioOHD$TransfertREA))

# BronchioOHD$AlimentationOralec
tt=table(BronchioOHD$AlimentationOralec)
tt/sum(tt)*100
table(BronchioOHD$AlimentationOralec,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$AlimentationOralec,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table BronchioOHD$AlimentationOralec,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$AlimentationOralec,BronchioOHD$TransfertREA))

# BronchioOHD$volumebc
tt=table(BronchioOHD$volumec)
tt/sum(tt)*100
table(BronchioOHD$volumec,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$volumec,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates realisee pour les petits echantillons
fisher.test(table(BronchioOHD$volumec,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$volumec,BronchioOHD$TransfertREA))

#(BronchioOHD$HydratTYPEc)
tt=table(BronchioOHD$HydratTYPEc)
tt/sum(tt)*100
table(BronchioOHD$HydratTYPEc,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$HydratTYPEc,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table(BronchioOHD$HydratTYPEc,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$HydratTYPEc,BronchioOHD$TransfertREA))

# BronchioOHD$intubation
tt=table(BronchioOHD$intubation)
tt/sum(tt)*100
table(BronchioOHD$intubation,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$intubation,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)

# escaladeVNI
tt=table(BronchioOHD$escaladeVNI)
tt/sum(tt)*100
table(BronchioOHD$escaladeVNI,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$escaladeVNI,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)

# BronchioOHD$Alimentationc
tt=table(BronchioOHD$Alimentationc)
tt/sum(tt)*100
table(BronchioOHD$Alimentationc,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Alimentationc,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)
# la mention correct = F annule la correction de Yates r�alis�e pour les petits �chantillons
fisher.test(table BronchioOHD$Alimentationc,BronchioOHD$TransfertREA))
odds.ratio(table(BronchioOHD$Alimentationc,BronchioOHD$TransfertREA))

# modaliteVNI
tt=table(BronchioOHD$modaliteVNI)
tt/sum(tt)*100
table(BronchioOHD$modaliteVNI,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$modaliteVNI,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)

# VNI
tt=table(BronchioOHD$VNI)
tt/sum(tt)*100
table(BronchioOHD$VNI,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$VNI,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)

#(poursuiteOHD)
tt=table(poursuiteOHD)
tt/sum(tt)*100
table(poursuiteOHD,BronchioOHD$TransfertREA)
chisq.test(table(poursuiteOHD,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)

#BronchioOHD$VSMAX
tt=table(BronchioOHD$VSMAX)
tt/sum(tt)*100
table(BronchioOHD$VSMAX,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$VSMAX,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)

# BronchioOHD$Relai 
tt=table(BronchioOHD$Relai)
tt/sum(tt)*100
table(BronchioOHD$Relai,BronchioOHD$TransfertREA)
chisq.test(table(BronchioOHD$Relai,BronchioOHD$TransfertREA),correct=F,simulate.p.value=T,B=2000)


# BronchioOHD$DelaiAppel
shapiro.test(BronchioOHD$DelaiAppel) 
hist(BronchioOHD$DelaiAppel)
aggregate(BronchioOHD$DelaiAppel~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$DelaiAppel~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$dureeTOTOHD
shapiro.test(BronchioOHD$dureeTOTOHD) 
hist(BronchioOHD$dureeTOTOHD)
aggregate(BronchioOHD$dureeTOTOHD~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$dureeTOTOHD~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$dureeVNI
shapiro.test(BronchioOHD$dureeVNI) 
hist(BronchioOHD$dureeVNI)
aggregate(BronchioOHD$dureeVNI~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$dureeVNI~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$dureeVI
shapiro.test(BronchioOHD$dureeVI) 
hist(BronchioOHD$dureeVI)
aggregate(BronchioOHD$dureeVI~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$dureeVI~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$dureeTOTVent
shapiro.test(BronchioOHD$dureeTOTVent) 
hist(BronchioOHD$dureeTOTVent)
aggregate(BronchioOHD$dureeTOTVent~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$dureeTOTVent~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$dureeJEUN
shapiro.test(BronchioOHD$dureeJEUN) 
hist(BronchioOHD$dureeJEUN)
aggregate(BronchioOHD$dureeJEUN~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$dureeJEUN~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$dureeHIV
shapiro.test(BronchioOHD$dureeHIV) 
hist(BronchioOHD$dureeHIV)
aggregate(BronchioOHD$dureeHIV~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$dureeHIV~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$dureeSNG
shapiro.test(BronchioOHD$dureeSNG) 
hist(BronchioOHD$dureeSNG)
aggregate(BronchioOHD$dureeSNG~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$dureeSNG~BronchioOHD$TransfertREA, alternative = 'greater')

# BronchioOHD$dureeSejour
shapiro.test(BronchioOHD$dureeSejour) 
hist(BronchioOHD$dureeSejour)
aggregate(BronchioOHD$dureeSejour~BronchioOHD$TransfertREA,FUN=summary)
wilcox.test(BronchioOHD$dureeSejour~BronchioOHD$TransfertREA, alternative = 'greater')

# plusieurs autres variables : R�gression logistique multivariable
		# Charger les packages utiles
		install.packages("questionr")
		library(questionr)
		install.packages("broom")
		library(broom)
		install.packages("MASS")
		library(MASS)
summary(BronchioOHD)

# Regression logistique transfert en REA
#Modèle à l'admission
# Terme
BronchioOHD$Terme<-relevel(BronchioOHD$Terme,">37")
# Ageb
BronchioOHD$Ageb<-relevel(BronchioOHD$Ageb,">3mois")
# Poids
BronchioOHD$Poidsb=cut(BronchioOHD$Poids,c(2,4,5,6,11))
levels(BronchioOHD$Poidsb)=c("A","B","C","D")
summary(BronchioOHD$Poidsb)
BronchioOHD$Poidsb<-relevel(BronchioOHD$Poidsb,"D")
# PremiereBronchio
BronchioOHD$PremiereBronchio<-relevel(BronchioOHD$PremiereBronchio,"non")
#Dureesymptomes
BronchioOHD$Dureesymptomesb=cut(BronchioOHD$Dureesymptomes,c(0,5,13))
levels(BronchioOHD$Dureesymptomesb)=c("A","B")
BronchioOHD$Dureesymptomesb<-relevel(BronchioOHD$Dureesymptomesb,"B")
summary(BronchioOHD$Dureesymptomesb)
#virus
BronchioOHD$virus<-relevel(BronchioOHD$virus,"non identifie")
odds.ratio(table(BronchioOHD$virus,BronchioOHD$TransfertREA))
#Coinfection
BronchioOHD$Coinfection<-relevel(BronchioOHD$Coinfection,"non")
odds.ratio(table(BronchioOHD$Coinfection,BronchioOHD$TransfertREA))
# FC
BronchioOHD$FC<-relevel(BronchioOHD$FC,"normocarde")
odds.ratio(table(BronchioOHD$FC,BronchioOHD$TransfertREA))
# Spo2
BronchioOHD$Spo2<-relevel(BronchioOHD$Spo2,">92%")
odds.ratio(table(BronchioOHD$Spo2,BronchioOHD$TransfertREA))
#ApneesMaison
BronchioOHD$ApneesMaison<-relevel(BronchioOHD$ApneesMaison,"non")
odds.ratio(table(BronchioOHD$ApneesMaison,BronchioOHD$TransfertREA))
#MalaiseHOP
BronchioOHD$MalaiseHOP<-relevel(BronchioOHD$MalaiseHOP,"non")
odds.ratio(table(BronchioOHD$MalaiseHOP,BronchioOHD$TransfertREA))
#pHCAT
BronchioOHD$pHCAT=cut(BronchioOHD$pH,c(7.15,7.3,7.38,7.5))
levels(BronchioOHD$pHCAT)=c("A","B","C")
BronchioOHD$pHCAT<-relevel(BronchioOHD$pHCAT,"C")
summary(BronchioOHD$pHCAT)
BronchioOHD$pHCAT<-relevel(BronchioOHD$pHCAT,"C")
# RP
BronchioOHD$RP<-relevel(BronchioOHD$RP,"normale")
summary(BronchioOHD$RP)
reg <- glm(TransfertREA~Terme+Ageb+virus+Spo2,data = BronchioOHD, family = binomial(logit))
		reg
		summary(reg)

		exp(cbind(coef(reg), confint(reg)))
		odds.ratio(reg)
		warnings(reg)
		# Sélection de variables
		reg2 <- step(reg)
		reg3 <- stepAIC(reg)
		print(reg$fitted.values)
		# Evaluation de la significativité du modèle
		chi2<-reg$null.deviance-reg$deviance
		print(chi2)
		ddl<-reg$df.null-reg$df.residual
		print(ddl)
		pvalue<-pchisq(chi2,ddl,lower.tail=F)
		print(pvalue)
		# test de la colinéarité des variables
		install.packages("performance")
		library(performance)
		performance::check_collinearity(reg)
		# 
		#Le Hosmer-Lemeshow :
		install.packages("glmtoolbox")
		library(glmtoolbox)
		glmtoolbox::hltest(reg)
		#pseudo-R2 de MacFadden :
		# complete model
		mod <- glm(y ~ x1 + x2 + x3, data = data, family="binomial")
		# null model 
		nullmod <- glm(TransfertREA ~ 1,data = BronchioOHD, family = binomial)
		#PSEUDO-R2
		1-logLik(reg)/logLik(nullmod)
tbl_regression(reg, exponentiate = TRUE)
ggcoef_model(reg, exponentiate = TRUE)
forest_model(reg)

install.packages("effects")
		library(effects)
		plot(allEffects(reg))
		install.packages("tidyverse")
		library(tidyverse)
		library(broom)
	library(broom.helpers)
		tidy(reg, conf.int = TRUE, exponentiate = TRUE)
		tmp <- tidy(reg, conf.int = TRUE, exponentiate = TRUE)
		str(tmp)
		install.packages("ggplot2")
		library(ggplot2)
		ggplot(tmp) + aes(x = estimate, y = term, xmin = conf.low, 
 		xmax = conf.high) + geom_vline(xintercept = 1) + 
  		geom_errorbarh() + geom_point() + scale_x_log10()



summary(BronchioOHD)
#Modèle à la mise sous OHD
# FR.1
BronchioOHD$FR.1<-relevel(BronchioOHD$FR.1,"eupneique")
Spo2b
BronchioOHD$Spo2b<-relevel(BronchioOHD$Spo2b,">92%")
# SDLb
BronchioOHD$SDLb<-relevel(BronchioOHD$SDLb,"absent")
#WangB
BronchioOHD$WangB<-relevel(BronchioOHD$WangB,"0-3 sans gravite")
# Woodb
BronchioOHD$Woodb<-relevel(BronchioOHD$Woodb,"0-2,5")
# apneeb	
BronchioOHD$apneeb<-relevel(BronchioOHD$apneeb,"non")
# AEGb
BronchioOHD$AEGb<-relevel(BronchioOHD$AEGb,"non")
# malaiseb
BronchioOHD$malaiseb<-relevel(BronchioOHD$AEGb,"non")
# ROXI
BronchioOHD$ROXIb=cut(BronchioOHD$ROXI,c(0,6,30))
levels(BronchioOHD$ROXIb)=c("<6",">6")
BronchioOHD$ROXIb<-relevel(BronchioOHD$ROXIb,">6")
summary(BronchioOHD$ROXIb)

reg <- glm(TransfertREA~Woodb+apneeb+malaiseb+ROXIb,data = BronchioOHD, family = binomial(logit))
		reg
		summary(reg)
		exp(cbind(coef(reg), confint(reg)))
		odds.ratio(reg)
		
		# Sélection de variables
		reg2 <- step(reg)
		reg3 <- stepAIC(reg)
		print(reg$fitted.values)
		# Evaluation de la significativité du modèle
		chi2<-reg$null.deviance-reg$deviance
		print(chi2)
		ddl<-reg$df.null-reg$df.residual
		print(ddl)
		pvalue<-pchisq(chi2,ddl,lower.tail=F)
		print(pvalue)
		# test de la colinéarité des variables
		install.packages("performance")
		library(performance)
		performance::check_collinearity(reg)
		# 
		#Le Hosmer-Lemeshow :
		install.packages("glmtoolbox")
		library(glmtoolbox)
		glmtoolbox::hltest(reg)
		#pseudo-R2 de MacFadden :
		# complete model
		mod <- glm(y ~ x1 + x2 + x3, data = data, family="binomial")
		# null model 
		nullmod <- glm(TransfertREA ~ 1,data = BronchioOHD, family = binomial)
		#PSEUDO-R2
		1-logLik(reg)/logLik(nullmod)
tbl_regression(reg, exponentiate = TRUE)
ggcoef_model(reg, exponentiate = TRUE)
forest_model(reg)

reg <- glm(TransfertREA~Terme+Ageb+virus+Spo2+Woodb+malaiseb+ROXIb,data = BronchioOHD, family = binomial(logit))
reg
		summary(reg)
		exp(cbind(coef(reg), confint(reg)))
		odds.ratio(reg)
		
		# Sélection de variables
		reg2 <- step(reg)
		reg3 <- stepAIC(reg)
		print(reg$fitted.values)
		# Evaluation de la significativité du modèle
		chi2<-reg$null.deviance-reg$deviance
		print(chi2)
		ddl<-reg$df.null-reg$df.residual
		print(ddl)
		pvalue<-pchisq(chi2,ddl,lower.tail=F)
		print(pvalue)
		# test de la colinéarité des variables
		install.packages("performance")
		library(performance)
		performance::check_collinearity(reg)
		# 
		#Le Hosmer-Lemeshow :
		install.packages("glmtoolbox")
		library(glmtoolbox)
		glmtoolbox::hltest(reg)
		#pseudo-R2 de MacFadden :
		# complete model
		mod <- glm(y ~ x1 + x2 + x3, data = data, family="binomial")
		# null model 
		nullmod <- glm(TransfertREA ~ 1,data = BronchioOHD, family = binomial)
		#PSEUDO-R2
		1-logLik(reg)/logLik(nullmod)
tbl_regression(reg, exponentiate = TRUE)
ggcoef_model(reg, exponentiate = TRUE)
forest_model(reg)

install.packages("effects")
		library(effects)
		plot(allEffects(reg))
		install.packages("tidyverse")
		library(tidyverse)
		library(broom)
	library(broom.helpers)
		tidy(reg, conf.int = TRUE, exponentiate = TRUE)
		tmp <- tidy(reg, conf.int = TRUE, exponentiate = TRUE)
		str(tmp)
		install.packages("ggplot2")
		library(ggplot2)
		ggplot(tmp) + aes(x = estimate, y = term, xmin = conf.low, 
 		xmax = conf.high) + geom_vline(xintercept = 1) + 
  		geom_errorbarh() + geom_point() + scale_x_log10()


# Regression logistique besoin de VNI
#Modèle à l'admission
# Terme
BronchioOHD$Terme<-relevel(BronchioOHD$Terme,">37")
# Ageb
BronchioOHD$Ageb<-relevel(BronchioOHD$Ageb,">3mois")
# Poids
BronchioOHD$Poidsb=cut(BronchioOHD$Poids,c(2,4,5,6,11))
levels(BronchioOHD$Poidsb)=c("A","B","C","D")
summary(BronchioOHD$Poidsb)
BronchioOHD$Poidsb<-relevel(BronchioOHD$Poidsb,"D")
# PremiereBronchio
BronchioOHD$PremiereBronchio<-relevel(BronchioOHD$PremiereBronchio,"non")
#Dureesymptomes
BronchioOHD$Dureesymptomesb=cut(BronchioOHD$Dureesymptomes,c(0,5,13))
levels(BronchioOHD$Dureesymptomesb)=c("A","B")
BronchioOHD$Dureesymptomesb<-relevel(BronchioOHD$Dureesymptomesb,"B")
summary(BronchioOHD$Dureesymptomesb)
#virus
BronchioOHD$virus<-relevel(BronchioOHD$virus,"non identifie")
odds.ratio(table(BronchioOHD$virus,BronchioOHD$TransfertREA))
#Coinfection
BronchioOHD$Coinfection<-relevel(BronchioOHD$Coinfection,"non")
odds.ratio(table(BronchioOHD$Coinfection,BronchioOHD$TransfertREA))
# FC
BronchioOHD$FC<-relevel(BronchioOHD$FC,"normocarde")
odds.ratio(table(BronchioOHD$FC,BronchioOHD$TransfertREA))
# Spo2
BronchioOHD$Spo2<-relevel(BronchioOHD$Spo2,">92%")
odds.ratio(table(BronchioOHD$Spo2,BronchioOHD$TransfertREA))
#ApneesMaison
BronchioOHD$ApneesMaison<-relevel(BronchioOHD$ApneesMaison,"non")
odds.ratio(table(BronchioOHD$ApneesMaison,BronchioOHD$TransfertREA))
#MalaiseHOP
BronchioOHD$MalaiseHOP<-relevel(BronchioOHD$MalaiseHOP,"non")
odds.ratio(table(BronchioOHD$MalaiseHOP,BronchioOHD$TransfertREA))
#pHCAT
BronchioOHD$pHCAT=cut(BronchioOHD$pH,c(7.15,7.3,7.38,7.5))
levels(BronchioOHD$pHCAT)=c("A","B","C")
BronchioOHD$pHCAT<-relevel(BronchioOHD$pHCAT,"C")
summary(BronchioOHD$pHCAT)
BronchioOHD$pHCAT<-relevel(BronchioOHD$pHCAT,"C")
# RP
BronchioOHD$RP<-relevel(BronchioOHD$RP,"normale")
summary(BronchioOHD$RP)
reg <- glm(VNI~Terme+Ageb+virus+Spo2,data = BronchioOHD, family = binomial(logit))
		reg
		summary(reg)

		exp(cbind(coef(reg), confint(reg)))
		odds.ratio(reg)
		warnings(reg)
		# Sélection de variables
		reg2 <- step(reg)
		reg3 <- stepAIC(reg)
		print(reg$fitted.values)
		# Evaluation de la significativité du modèle
		chi2<-reg$null.deviance-reg$deviance
		print(chi2)
		ddl<-reg$df.null-reg$df.residual
		print(ddl)
		pvalue<-pchisq(chi2,ddl,lower.tail=F)
		print(pvalue)
		# test de la colinéarité des variables
		install.packages("performance")
		library(performance)
		performance::check_collinearity(reg)
		# 
		#Le Hosmer-Lemeshow :
		install.packages("glmtoolbox")
		library(glmtoolbox)
		glmtoolbox::hltest(reg)
		#pseudo-R2 de MacFadden :
		# complete model
		mod <- glm(y ~ x1 + x2 + x3, data = data, family="binomial")
		# null model 
		nullmod <- glm(TransfertREA ~ 1,data = BronchioOHD, family = binomial)
		#PSEUDO-R2
		1-logLik(reg)/logLik(nullmod)
tbl_regression(reg, exponentiate = TRUE)
ggcoef_model(reg, exponentiate = TRUE)
forest_model(reg)

install.packages("effects")
		library(effects)
		plot(allEffects(reg))
		install.packages("tidyverse")
		library(tidyverse)
		library(broom)
	library(broom.helpers)
		tidy(reg, conf.int = TRUE, exponentiate = TRUE)
		tmp <- tidy(reg, conf.int = TRUE, exponentiate = TRUE)
		str(tmp)
		install.packages("ggplot2")
		library(ggplot2)
		ggplot(tmp) + aes(x = estimate, y = term, xmin = conf.low, 
 		xmax = conf.high) + geom_vline(xintercept = 1) + 
  		geom_errorbarh() + geom_point() + scale_x_log10()



summary(BronchioOHD)
#Modèle à la mise sous OHD
# FR.1
BronchioOHD$FR.1<-relevel(BronchioOHD$FR.1,"eupneique")
Spo2b
BronchioOHD$Spo2b<-relevel(BronchioOHD$Spo2b,">92%")
# SDLb
BronchioOHD$SDLb<-relevel(BronchioOHD$SDLb,"absent")
#WangB
BronchioOHD$WangB<-relevel(BronchioOHD$WangB,"0-3 sans gravite")
# Woodb
BronchioOHD$Woodb<-relevel(BronchioOHD$Woodb,"0-2,5")
# apneeb	
BronchioOHD$apneeb<-relevel(BronchioOHD$apneeb,"non")
# AEGb
BronchioOHD$AEGb<-relevel(BronchioOHD$AEGb,"non")
# malaiseb
BronchioOHD$malaiseb<-relevel(BronchioOHD$AEGb,"non")
# ROXI
BronchioOHD$ROXIb=cut(BronchioOHD$ROXI,c(0,6,30))
levels(BronchioOHD$ROXIb)=c("<6",">6")
BronchioOHD$ROXIb<-relevel(BronchioOHD$ROXIb,">6")
summary(BronchioOHD$ROXIb)

reg <- glm(VNI~Woodb+apneeb+malaiseb+ROXIb,data = BronchioOHD, family = binomial(logit))
		reg
		summary(reg)
		exp(cbind(coef(reg), confint(reg)))
		odds.ratio(reg)
		
		# Sélection de variables
		reg2 <- step(reg)
		reg3 <- stepAIC(reg)
		print(reg$fitted.values)
		# Evaluation de la significativité du modèle
		chi2<-reg$null.deviance-reg$deviance
		print(chi2)
		ddl<-reg$df.null-reg$df.residual
		print(ddl)
		pvalue<-pchisq(chi2,ddl,lower.tail=F)
		print(pvalue)
		# test de la colinéarité des variables
		install.packages("performance")
		library(performance)
		performance::check_collinearity(reg)
		# 
		#Le Hosmer-Lemeshow :
		install.packages("glmtoolbox")
		library(glmtoolbox)
		glmtoolbox::hltest(reg)
		#pseudo-R2 de MacFadden :
		# complete model
		mod <- glm(y ~ x1 + x2 + x3, data = data, family="binomial")
		# null model 
		nullmod <- glm(TransfertREA ~ 1,data = BronchioOHD, family = binomial)
		#PSEUDO-R2
		1-logLik(reg)/logLik(nullmod)
tbl_regression(reg, exponentiate = TRUE)
ggcoef_model(reg, exponentiate = TRUE)
forest_model(reg)

reg <- glm(VNI~Terme+Ageb+virus+Spo2+Woodb+malaiseb+ROXIb,data = BronchioOHD, family = binomial(logit))
reg
		summary(reg)
		exp(cbind(coef(reg), confint(reg)))
		odds.ratio(reg)
		
		# Sélection de variables
		reg2 <- step(reg)
		reg3 <- stepAIC(reg)
		print(reg$fitted.values)
		# Evaluation de la significativité du modèle
		chi2<-reg$null.deviance-reg$deviance
		print(chi2)
		ddl<-reg$df.null-reg$df.residual
		print(ddl)
		pvalue<-pchisq(chi2,ddl,lower.tail=F)
		print(pvalue)
		# test de la colinéarité des variables
		install.packages("performance")
		library(performance)
		performance::check_collinearity(reg)
		# 
		#Le Hosmer-Lemeshow :
		install.packages("glmtoolbox")
		library(glmtoolbox)
		glmtoolbox::hltest(reg)
		#pseudo-R2 de MacFadden :
		# complete model
		mod <- glm(y ~ x1 + x2 + x3, data = data, family="binomial")
		# null model 
		nullmod <- glm(TransfertREA ~ 1,data = BronchioOHD, family = binomial)
		#PSEUDO-R2
		1-logLik(reg)/logLik(nullmod)
tbl_regression(reg, exponentiate = TRUE)
ggcoef_model(reg, exponentiate = TRUE)
forest_model(reg)

install.packages("effects")
		library(effects)
		plot(allEffects(reg))
		install.packages("tidyverse")
		library(tidyverse)
		library(broom)
	library(broom.helpers)
		tidy(reg, conf.int = TRUE, exponentiate = TRUE)
		tmp <- tidy(reg, conf.int = TRUE, exponentiate = TRUE)
		str(tmp)
		install.packages("ggplot2")
		library(ggplot2)
		ggplot(tmp) + aes(x = estimate, y = term, xmin = conf.low, 
 		xmax = conf.high) + geom_vline(xintercept = 1) + 
  		geom_errorbarh() + geom_point() + scale_x_log10()
	


# Courbes de survie

# Analyse de survie
	# charger le package survival
	install.packages("survival")
	library(survival)
	# donner un nouveau nom � la fonction de survie: nom du document.surv
	nom du document.surv<- Surv(nom du document$nom de la variable de temps,nom du document$nom de la variable de status==valeur de la variable de status consid�r�e)
	nom du document.survfit <- survfit(nom du document.surv)
	plot(nom du document.survfit,xlab="axe des abscisses",ylab="axe des ordonn�es",lty=c(,),main="titre du graphique")
	legend(c("nom du groupe1","nom du groupe2",lty=c(,))

	# comparaison entre 2 groupes
	nom du document.survfit2 <- survfit(nom du document.surv~nom de la variable de comparaison))
	plot(nom du document.survfit2,col=c(1,2))
	# test de la diff�rence entre les 2 courbes:Test du log-rank	
	survdiff(Surv(nom du document$nom de la variable de temps,nom du document$nom de la variable de status==valeur de la variable de status consid�r�e)~nom de la variable de comparaison)
	survdiff(nom du document.surv~nom du document~nom de la variable de comparaison)

	legend(3000,0.4,c("F","H"),lty=c(1,1),col=c(1,2))

library(survival)
	# Probabilit� de discharge du service
		AMMONUSURV.surv <- Surv(AMMONUSURV$SEJOUR,AMMONUSURV$STATUTb==1)
		AMMONUSURV.surv
		AMMONUSURV.survfit <- survfit(AMMONUSURV.surv~AMMONUSURV$Groupe)
		plot(AMMONUSURV.survfit,xlab="PICU Stay(days)",ylab="PICU stay probability(100%)",lty=c(1,3),main="PICU stay")
		legend(c("Control","Protocol"),lty=c(1,3))
		survdiff(AMMONUSURV.surv~AMMONUSURV$Groupe)
	
	#Diagnostic-prescription
		AMMONUSURV.surv <- Surv(AMMONUSURV$AMMODelai_resultat_presciption,AMMONUSURV$STATUT==1)
		AMMONUSURV.surv
		AMMONUSURV.survfit <- survfit(AMMONUSURV.surv~AMMONUSURV$Groupe)
		plot(AMMONUSURV.survfit,xlab="Delay to prescription (minutes)",ylab="free from prescription(100%)",lty=c(1,3),main="Delay from diagnosis to prescription")
		legend(c("Control","Protocol"),lty=c(1,3))
		survdiff(AMMONUSURV.surv~AMMONUSURV$Groupe)

