# DATA
getwd()
setwd("P:/CONSULTATION/Lhermitte_Amaury/DATA") # On PC
setwd("/Users/damianocerasuolo/Desktop/UBRC/2021_CONSULT/Lhermitte_Amaury/DATA") # On Mac

#-------------------------------------------------------------------------------

# PACKAGES
#install.packages("readxl")
library("readxl")

la <- read_excel("la.xlsx", na="NA")

# View(la)
names(la)

str(la$Vrec_VteaPEEP15)
la$Vrec_VteaPEEP15[la$Vrec_VteaPEEP15==".1"] <- "-1"
la$Vrec_VteaPEEP15[la$Vrec_VteaPEEP15==".120"] <- "-120"
la$Vrec_VteaPEEP15 <- as.numeric(as.character(la$Vrec_VteaPEEP15))
table(la$Vrec_VteaPEEP15)

################################################################################

install.packages("tidyverse")
library("tidyverse")
install.packages("dplyr")
library("dplyr")

# ADD LABELS TO VARIABLES
la = apply_labels(la,
                    ID = "ID",
                    IEP= "IEP",
                    Age = "Age",
                    Homme = "Homme 1 / Femme 0",
                    Taille = "Taille",
                    Poids = "Poids",
                    IMC = "IMC",
                    entreerea = "Date Entrée en Réa",
                    diagSDRA = "Date Diagnostic de SDRA",
                    IOT = "Date IOT",
                    inclusion = "Date inclusion",
                    dela_reainclusion = "Délai Entrée Réa-inclusion",
                    delai_IOTinclusion = "Délai IOT-inclusion",
                    delai_SDRAinclusion = "Délai Dg SDRA-inclusion",
                    seancesDV = "Nb séances DV total",
                    seancesDV_avantinclusion01 = "Séances de DV avant l'inclusion",
                    seancesDV_avantinclusion = "Si DV avant l'inclusion Nbr de séances",
                    etiologieSDRApulmonaire = "Etiologie SDRA pulmunaire",
                    covid01 = "Covid 19+",
                    delai_covidsympt_inclusion = "Délai Sympt. COVID/Inclusion",
                    delai_COVID_Inclusion = "Délai Df COVID/Inclusion",
                    delai_COVID_Inclusion7j = "Délai Dg COVID/Inclusion -7j",
                    delai_COVID_Inclusion7j14j = "Délai Dg COVID/Inclusion 7-14j",
                    delai_COVID_Inclusion14j = "Délai dg COVID/Inclusion >14j",
                    FDR_SDRA_pneumopathie = "FDR SDRA Pneumopathie (hors COVID)",
                    FDR_SDRA_sepsis = "FDR SDRA Sepsis",
                    FDR_SDRA_choc = "FDR SDRA Etat de choc",
                    FDR_SDRA_contusionspulm = "FDR SDRA Contusions pulmonaires",
                    FDR_SDRA_inhalation = "FDR SDRA Inhalation",
                    FDR_SDRA_pancréatiteaigue = "FDR SDRA Pancréatite aigue",
                    FDR_SDRA_péritonite = "FDR SDRA Péritoneite",
                    FDR_SDRA_ischémiedigestive = "FDR SDRA Ischémie digestive",
                    FDR_SDRA_traumatisegrave = "FDR SDRA Traumatisé grave",
                    FDR_SDRA_transfusionmassive = "FDR SDRA Transfusion massive",
                    FDR_SDRA_postCEC = "FDR SDRA Post-CEC",
                    FDR_SDRA_autres = "FDR SDRA Autres",
                    FDR_SDRA_siautre = "FDR si autre",
                    SOFA = "SOFA TOTAL",
                    IGSII = "IGS II",
                    charlsonscore = "Score Charlson",
                    hypnovel01 = "Hypnovel",
                    hypnovel_mgh = "Posologie (mg/h)",
                    propofol01 = "Propofol",
                    propofol_mgh = "Posologie (mg/h)",
                    sufenta01 = "Sufenta",
                    sufenta_migrogh = "Posologie (μg/h)",
                    cisatracurium01 = "Cisatracurium",
                    cisatracurium_mgh = "Posologie (mg/h)",
                    atracurium01 = "Atracurium",
                    atracurium_mgh = "Posologie (mg/h)",
                    rocuronium01 = "Rocuronium",
                    rocuronium_mgh = "Posologie (mg/h)",
                    NAD01 = "NAD",
                    NAD_microgkgmin = "Posologie (μg/kg/min)",
                    NAD_sup02 = "Poso NAD > 0,2 μg/kg/min",
                    NAD_sup05 = "Poso NAD > 0,5 μg/kg/min",
                    dobutamine01 = "Dobutamine",
                    NO01 = "NO",
                    NO_ppm = "Posologie (ppm)",
                    NOinf5 = "NO <5 ppm",
                    NO_5_10 = "NO [5-10] ppm",
                    NO_sup10 = "NO >10 ppm",
                    T0_Vt_mlkg = "T0 Vt (ml/kg)",
                    T0_Vt_ml = "T0 Soit Vt (ml)",
                    T0_FR_cpm = "T0 FR (cpm)",
                    T0_debit_Lmin = "T0 Débit (L/min)",
                    T0_TpsInpsi_sec = "T0 Tps inpsi (sec)",
                    T0_I_E = "T0 I/E",
                    T0_Pmax_cmH2O = "T0 Pmax (cmH2O)",
                    T0_Pplat_cmH2O = "T0 Pplat (cmH2O)",
                    T0_Pplat_inf20cmH2O = "T0 Pplat < 20 cmH2O",
                    T0_Pplat_20_24cmH2O = "T0 Pplat [20-24] cmH2O",
                    T0_Pplat_25_30_cmH2O = "T0 Pplat [25-30] cmH2O",
                    T0_Pplat_sup30cmH2O = "T0 Pplat > 30 cmH2O",
                    T0_PEEP_cmH2O = "T0 PEEP (cmH2O)",
                    T0_PEEP_5_9_cmH2O = "T0 PEEP [5-9] cmH2O",
                    T0_PEEP_10_15_cmH2O = "T0 PEEP [10-15] cmH2O",
                    T0_Pmotrice_cmH2O = "T0 Pmotrice (cmH2O)",
                    T0_Pmotrice_inf10cmH2O = "T0 Pmotrice < 10 cmH2O",
                    T0_Pmotrice_10_15cmH2O = "T0 Pmotrice [10-15] cmH2O",
                    T0_Pmotrice_sup15cmH2O = "T0 Pmotrice > 15 cmH2O",
                    T0_Compliance_mlcmH2O = "T0 Compliance (ml/cmH2O)",
                    T0_Compliance_inf20mlcmH2O = "T0 Compliance < 20ml/cmH2O",
                    T0_Compliance_20_40_mlcmH2O = "T0 Compliance [20-40] ml/cmH2O",
                    T0_Compliance_sup40mlcmH2O = "T0 Compliance  > 40 ml/cmH2O",
                    T0_Ventmin_Lmin = "T0 Vent min (L/min)",
                    T0_FiO2 = "T0 FiO2",
                    FC = "FC",
                    PAS = "PAS",
                    PAD = "PAD",
                    PAM = "PAM",
                    SpO2 = "SpO2",
                    SpO2_sup96p100 = "SpO2 > 96%",
                    SpO2_92_96 = "SpO2 92-96%",
                    SpO2_inf92 = "SpO2 < 92%",
                    ETT_faite = "ETT faite",
                    FEVG_pourc = "FEVG (%)",
                    FEVG_sup50 = "FEVG > 50%",
                    FEVG_30_50 = "FEVG 30-50%",
                    FEVG_inf30 = "FEVG < 30%",
                    PRVG_elevees = "PRVG Élevées",
                    PRVG_basses = "PRVG Basses",
                    PRVG_zonegrise = "PRVG Zone grise",
                    dysfonction_VD = "Dysfonction VD",
                    TAPSE_mm = "TAPSE (mm)",
                    TAPSE_inf12mm = "TAPSE <12 mm",
                    PAPS_mmHg = "PAPS Valeur (mmHg)",
                    PAPS_inf40mmHg = "PAPS - de 40 mmHg",
                    PAPS_sup40mmHg = "PAPS + de 40 mmHg",
                    T0_pH = "T0 pH",
                    T0_PaCO2 = "T0 PaCO2",
                    T0_PaO2 = "T0 PaO2",
                    T0_P_F = "T0 P/F",
                    T0_A_FiO2 = "T0 A FiO2",
                    T0_HCO3minus = "T0 HCO3-",
                    T0_Lactate = "T0 Lactate",
                    dureeMERPplusMRA_min = "Durée MERP+MRA (min)",
                    dureeMERPplusMRA_inf10min = "Durée MERP+MRA - de 10 min",
                    dureeMERPplusMRA_10_15min = "Durée MERP+MRA [10-15] min",
                    dureeMERPplusMRA_plus15min = "Durée MERP+MRA + de 15 min",
                    MERP_PEEP_15Vte = "MERP PEEP 15 Vte",
                    MERP_PEEP_15_Pplat = "MERP PEEP 15 Pplat",
                    VteCo = "VteCo",
                    MERP_PEEP_5_Vte = "MERP PEEP 5 Vte",
                    MERP_PEEP_5_Pplat = "MERP PEEP 5 Pplat",
                    MRA_PEEP_20_Pplat = "MRA PEEP 20 Pplat",
                    MRA_PEEP_20_Pmotrice = "MRA PEEP 20 Pmotrice",
                    MRA_PEEP_18_Pplat = "MRA PEEP 18 Pplat",
                    MRA_PEEP_18_Pmotrice = "MRA PEEP 18 Pmotrice",
                    MRA_PEEP_16_Pplat = "MRA PEEP 16 Pplat",
                    MRA_PEEP_16_Pmotrice = "MRA PEEP 16 Pmotrice",
                    MRA_PEEP_14_Pplat = "MRA PEEP 14 Pplat",
                    MRA_PEEP_14_Pmotrice = "MRA PEEP 14 Pmotrice",
                    MRA_PEEP_12_Pplat = "MRA PEEP 12 Pplat",
                    MRA_PEEP_12_Pmotrice = "MRA PEEP 12 Pmotrice",
                    MRA_PEEP_10_Pplat = "MRA PEEP 10 Pplat",
                    MRA_PEEP_10_Pmotrice = "MRA PEEP 10 Pmotrice",
                    T15_pH = "T15’ pH",
                    T15_PaCO2 = "T15’ PaCO2",
                    T15_PaO2 = "T15’ PaO2",
                    T15_P_F = "T15’P/F",
                    T15_A_FiO2 = "T15’ À FiO2",
                    T15_HCO3minus = "T15’ HCO3-",
                    T15_Lactate = "T15’ Lactate",
                    Best_PEEP = "Best PEEP",
                    BestPEEP_soitPmotrice = "Soit Pmotrice",
                    Vrec_VteaPEEP15 = "Vrec (Calculé avec le Vte à PEEP 15)",
                    Vrec_inf50ml = "Vrec (VTe) < 50 ml",
                    Vrec_50_99ml = "Vrec (VTe)  [50-99] ml",
                    Vrec_100_150ml = "Vrec (VTe) [100-150] ml",
                    Vrecsup150ml = "Vrec (VTe)> 150 ml",
                    Delta_P_F = "Delta P/F",
                    Delta_P_F_sup20 = "Delta P/F > 20%",
                    Delta_P_F_neg = "Delta P/F Négatif",
                    T0_apresMERPplusMRA_Vt_mlkg = "T0 après MERP+MRA Vt (ml/kg)",
                    T0_apresMERPplusMRA_Vt_ml = "T0 après MERP+MRA Soit Vt (ml)",
                    T0_apresMERPplusMRA_FRcpm = "T0 après MERP+MRA FR (cpm)",
                    T0_apresMERPplusMRA_CompliancemlcmH2O = "T0 après MERP+MRA Compliance (ml/cmH2O)",
                    T0_apresMERPplusMRA_Compliance_inf20mlcmH2O = "T0 après MERP+MRA Compliance < 20ml/cmH2O",
                    T0_apresMERPplusMRA_Compliance_20_40mlcmH2O = "T0 après MERP+MRA Compliance [20-40]ml/cmH2O",
                    T0_apresMERPplusMRA_Compliance_sup40mlcmH2O = "T0 après MERP+MRA Compliance > 40ml/cmH2O",
                    T0_apresMERPplusMRA_FiO2 = "T0 après MERP+MRA FiO2",
                    T0_apresMERPplusMRA_Ventmin_Lmin = "T0 après MERP+MRA Vent min (L/min)",
                    T0_apresMERPplusMRA_debit_Lmin = "T0 après MERP+MRA Débit (L/min)",
                    T0_apresMERPplusMRA_TpsInpsi_sec = "T0 après MERP+MRA Tps inpsi (sec)",
                    T0_apresMERPplusMRA_I_E = "T0 après MERP+MRA I/E",
                    T0_apresMERPplusMRA_Pplat_cmH2O = "T0 après MERP+MRA Pplat (cmH2O)",
                    T0_apresMERPplusMRA_Pplat_inf20cmH2O = "T0 après MERP+MRA Pplat <20 cmH2O",
                    T0_apresMERPplusMRA_Pplat_20_25_cmH2O = "T0 après MERP+MRA Pplat [20-25] cmH2O",
                    T0_apresMERPplusMRA_Pplat_25_30_cmH2O = "T0 après MERP+MRA Pplat [25-30] cmH2O",
                    T0_apresMERPplusMRA_Pplatsup30cmH2O = "T0 après MERP+MRA Pplat >30 cmH2O",
                    T0_apresMERPplusMRA_PEEP_cmH2O = "T0 après MERP+MRA PEEP (cmH2O)",
                    T0_apresMERPplusMRA_PEEP_5_10_cmH2O = "T0 après MERP+MRA PEEP [5-10] cmH2O",
                    T0_apresMERPplusMRA_PEEP_10_15cmH2O = "T0 après MERP+MRA PEEP [10-15] cmH2O",
                    T0_apresMERPplusMRA_PEEPsup15cmH2O = "T0 après MERP+MRA PEEP > 15 cmH2O",
                    T0_apresMERPplusMRA_PmotricecmH2O = "T0 après MERP+MRA Pmotrice (cmH2O)",
                    T0_apresMERPplusMRA_Pmotrice_inf10cmH2O = "T0 après MERP+MRA Pmotrice <10 cmH2O",
                    T0_apresMERPplusMRA_Pmotrice_10_15cmH2O = "T0 après MERP+MRA Pmotrice [10-15] cmH2O",
                    T0_apresMERPplusMRA_Pmotrice_sup15cmH2O = "T0 après MERP+MRA Pmotrice >15 cmH2O",
                    apresGdS15_postMRA_Vt_mlkg = "Après GdS 15’ post-MRA Vt (ml/kg)",
                    apresGdS15_postMRA_SoitVt_ml = "Après GdS 15’ post-MRA Soit Vt (ml)",
                    apresGdS15_postMRA_FR_cpm = "Après GdS 15’ post-MRA FR (cpm)",
                    apresGdS15_postMRA_Compliance_mlcmH2O = "Après GdS 15’ post-MRA Compliance (ml/cmH2O)",
                    apresGdS15_postMRA_Compliance_inf20mlcmH2O = "Après GdS 15’ post-MRA Compliance < 20ml/cmH2O",
                    apresGdS15_postMRA_Compliance_20_40mlcmH2O = "Après GdS 15’ post-MRA Compliance [20-40]ml/cmH2O",
                    apresGdS15_postMRA_Compliance_sup40mlcmH2O = "Après GdS 15’ post-MRA Compliance > 40ml/cmH2O",
                    apresGdS15_postMRA_FiO2 = "Après GdS 15’ post-MRA FiO2",
                    apresGdS15_postMRA_Ventmin_Lmin = "Après GdS 15’ post-MRA Vent min (L/min)",
                    apresGdS15_postMRA_debit_Lmin = "Après GdS 15’ post-MRA Débit (L/min)",
                    apresGdS15_postMRA_TpsInpsi_sec = "Après GdS 15’ post-MRA Tps inpsi (sec)",
                    apresGdS15_postMRA_I_E = "Après GdS 15’ post-MRA I/E",
                    apresGdS15_postMRA_PplatcmH2O = "Après GdS 15’ post-MRA Pplat (cmH2O)",
                    apresGdS15_postMRA_Pplat_inf20cmH2O = "Après GdS 15’ post-MRA Pplat <20 cmH2O",
                    apresGdS15_postMRA_Pplat_20_25cmH2O = "Après GdS 15’ post-MRA Pplat [20-25] cmH2O",
                    apresGdS15_postMRA_Pplat_25_30cmH2O = "Après GdS 15’ post-MRA Pplat [25-30] cmH2O",
                    apresGdS15_postMRA_Pplat_sup30cmH2O = "Après GdS 15’ post-MRA  Pplat >30 cmH2O",
                    apresGdS15_postMRA_PEEP_cmH2O = "Après GdS 15’ post-MRA PEEP (cmH2O)",
                    apresGdS15_postMRA_PEEP_5_10cmH2O = "Après GdS 15’ post-MRA PEEP [5-10] cmH2O",
                    apresGdS15_postMRA_PEEP_10_15cmH2O = "Après GdS 15’ post-MRA PEEP [10-15] cmH2O",
                    apresGdS15_postMRA_PEEP_sup15cmH2O = "Après GdS 15’ post-MRA PEEP > 15 cmH2O",
                    apresGdS15_postMRA_PmotricecmH2O = "Après GdS 15’ post-MRA Pmotrice (cmH2O)",
                    apresGdS15_postMRA_Pmotrice_inf10cmH2O = "Après GdS 15’ post-MRA Pmotrice <10 cmH2O",
                    apresGdS15_postMRA_Pmotrice_10_15cmH2O = "Après GdS 15’ post-MRA Pmotrice [10-15] cmH2O",
                    apresGdS15_postMRA_Pmotrice_sup15cmH2O = "Après GdS 15’ post-MRA Pmotrice >15 cmH2O",
                    arretMERP = "Arrêt de la MERP",
                    arretMRA = "Arrêt de la MRA",
                    PEEPMAX = "PEEP MAX",
                    Bradycardie = "Bradycardie",
                    Fcminimale = "FC la + basse",
                    Tachycardie = "Tachycardie",
                    Fcmaximale = "FC la + haute",
                    troublerythme = "Trouble du rythme",
                    ACR = "ACR",
                    hypotension_arterielle = "Hypotension artérielle",
                    PASminimale = "PAS (Valeurs les + basses)",
                    PADminimales = "PAD (Valeurs les + basses)",
                    PAMminimales = "PAM (Valeurs les + basses)",
                    SpO2_inf85p100 = "SpO2 < 85%",
                    SpO2minimale = "SpO2 la + basse",
                    Pneumothorax = "Pneumothorax",
                    Autres = "Autres",
                    sortieUSI_vivant = "Sortie USI Vivant",
                    dureeUSI_j = "Date sortie USI",
                    sortiehosp_vivant01 = "Durée séjour USI (j)",
                    sejourpostrea_j = "Sortie Hospit Vivant",
                    sejourtotal_j = "Date sortie Hospit",
                    vivant_j28 = "Vivant à J28",
                    dureeVM_j = "Durée VM (j)",
                    jourssansVM_sur28j = "Soit nbr jour sans VM (/28j)",
                    dureeP_F_inf200j = "Durée P/F<200 (j)",
                    jourscurarises = "Nb jours curarisés",
                    curarisation_infegal48h = "Curarisation < ou = 48h",
                    necessiteNO = "Nécessité NO",
                    jourssousNO = "Nbr jours sous NO",
                    necessiteAlmitrine = "Nécessité Almitrine",
                    necessiteECMO = "Nécessité ECMO",
                    jourssousECMO = "Nbr jours sous ECMO"
                    )

var_lab(la[12])

################################################################################

# vcrecdata <- la[c("Vrec_VteaPEEP15")]
# View(vcrecdata)

table(la$MERP_PEEP_5_Vte)
table(la$MERP_PEEP_5_Pplat)
table(la$MRA_PEEP_20_Pplat)
table(la$MRA_PEEP_20_Pmotrice)
table(la$MRA_PEEP_18_Pplat)
table(la$MRA_PEEP_18_Pmotrice)
table(la$MRA_PEEP_16_Pplat)
table(la$MRA_PEEP_16_Pmotrice)
table(la$MRA_PEEP_14_Pplat)
table(la$MRA_PEEP_14_Pmotrice)
table(la$MRA_PEEP_12_Pplat)
table(la$vMRA_PEEP_12_Pmotrice)
table(la$MRA_PEEP_10_Pplat)
table(la$MRA_PEEP_10_Pmotrice)

################################################################################

# PROC
# Reference code: Millot, p 885 

# colonisation_num == x (classe reelle)
# pcr_cy			== predictor 

# variable binaire = Delta_P_F_sup20 (y)  
# variable qualitative = Vrec_VteaPEEP15 (x)

str(la$Delta_P_F_sup20)
la$Delta_P_F_sup20 = as.factor(la$Delta_P_F_sup20)
str(la$Vrec_VteaPEEP15)
la$Vrec_VteaPEEP15 = as.numeric(as.character(la$Vrec_VteaPEEP15))

#chargement des packages
library("ggplot2")
library("pROC")

y = la$Delta_P_F_sup20
x = la$Vrec_VteaPEEP15

roc.sup20 <- roc(la$Delta_P_F_sup20, la$Vrec_VteaPEEP15)

#courbe roc
par(xaxs = "i", yaxs = "i", xpd = TRUE, cex.axis = 1.5, cex.lab = 1.5)
plot.roc(roc.sup20 , legacy.axes = TRUE, identity = FALSE, col = "orange", lwd = 2, 
         print.thres = "all", print.thres.pattern = "", print.thres.cex = 1.5, print.thres.col = "brown")
par(xpd = TRUE)
segments(1, 0, 0, 1)

coords(roc.sup20, x="best", best.method="youden", transpose = TRUE)

#-------------------------------------------------------------------------------

# SENS AND SPE

mycoords <- coords(roc.sup20, "all", transpose = TRUE)
plot(mycoords["threshold",], mycoords["specificity",], type="l", 
     col="red", xlab="Cutoff", ylab="Performance")
lines(mycoords["threshold",], mycoords["sensitivity",], type="l", 
      col="blue")
legend(100, 0.4, c("Specificity", "Sensitivity"), 
       col=c("red", "blue"), lty=1)

#-------------------------------------------------------------------------------

roc.sup20 <- roc(la$Delta_P_F_sup20, la$Vrec_VteaPEEP15)
plot(roc.sup20)

plot(smooth(roc.sup20), add=TRUE, col="blue")
legend("bottomright", legend=c("Empirical", "Smoothed"),
       col=c(par("fg"), "blue"), lwd=2)

plot(roc.sup20, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="lightblue", print.thres=TRUE)

has.partial.auc(roc.sup20)

# To plot a different partial AUC, we need to ignore the existing value with reuse.auc=FALSE
plot(roc.sup20, print.auc=TRUE, auc.polygon=TRUE, partial.auc=c(1, 0.8),
     partial.auc.focus="se", grid=c(0.1, 0.2), grid.col=c("green", "red"),
     max.auc.polygon=TRUE, auc.polygon.col="lightblue",
     print.thres=TRUE, print.thres.adj = c(1, -1),
     reuse.auc=FALSE)

plot(roc.sup20, print.thres="best", print.thres.best.method="youden")

plot(roc.sup20, print.thres="best", print.thres.best.method="youden",
     print.thres.best.weights=c(50, 0.2),
     print.thres.adj = c(1.1, 1.25),add = TRUE)

#-------------------------------------------------------------------------------

# Start a ROC plotrocobj <- plot.roc(aSAH$outcome, aSAH$s100b)
plot(roc.sup20)
# Thresholds
ci.thresolds.obj <- ci.thresholds(roc.sup20)
plot(ci.thresolds.obj)

# Specificities
plot(roc.sup20) 
ci.sp.obj <- ci.sp(roc.sup20, boot.n=500)
plot(ci.sp.obj)

# Sensitivities
plot(roc.sup20) 
ci.se.obj <- ci(roc.sup20, of="se", boot.n=500)
plot(ci.se.obj)

# plotting a shape
ci.sp.obj <- ci.sp(roc.sup20, sensitivities=seq(0, 1, .01), boot.n=1000)
plot(roc.sup20)
plot(ci.sp.obj, type="shape", col="blue")

# Direct syntax (response, predictor)
# plot.roc(la$Delta_P_F_sup20, la$Vrec_VteaPEEP15,ci=TRUE, of="thresholds")

################################################################################

### AUC
# Reference code: https://statinfer.com/203-4-3-roc-and-auc/

library("pROC")
library("ROCR")

logitfit_auc<-glm(y~x, data=la, family="binomial")
coef(logitfit_auc)

predicted_prob<-predict(logitfit_auc,type="response")
roccurve <- roc(logitfit_auc$y, predicted_prob)
plot(roccurve)
auc(roccurve)
auc(logitfit_auc$y, predicted_prob)

################################################################################

library(tableone)
dput(names(la))

# TRANSFORM VARIABLES
la$Age = as.numeric(as.character(la$Age))
la$Taille = as.numeric(as.character(la$Taille))
la$Poids = as.numeric(as.character(la$Poids))
la$dela_reainclusion = as.numeric(as.character(la$dela_reainclusion ))
la$IOT = as.numeric(as.character(la$IOT))
la$delai_IOTinclusion = as.numeric(as.character(la$delai_IOTinclusion))
la$delai_SDRAinclusion = as.numeric(as.character(la$delai_SDRAinclusion))
la$delai_covidsympt_inclusion = as.numeric(as.character(la$delai_covidsympt_inclusion))
la$delai_COVID_Inclusion = as.numeric(as.character(la$delai_COVID_Inclusion))
la$delai_COVID_Inclusion14j = as.numeric(as.character(la$delai_COVID_Inclusion14j))
la$delai_COVID_Inclusion7j14j = as.numeric(as.character(la$delai_COVID_Inclusion7j14j))
la$SOFA = as.numeric(as.character(la$SOFA))
la$charlsonscore = as.numeric(as.character(la$charlsonscore))
la$IMC = as.numeric(as.character(la$IMC))
la$seancesDV = as.numeric(as.character(la$seancesDV))
la$IGSII = as.numeric(as.character(la$IGSII))
la$hypnovel_mgh = as.numeric(as.character(la$hypnovel_mgh))
la$propofol_mgh = as.numeric(as.character(la$propofol_mgh))
la$sufenta_migrogh = as.numeric(as.character(la$sufenta_migrogh))
la$cisatracurium_mgh = as.numeric(as.character(la$cisatracurium_mgh))
la$atracurium_mgh = as.numeric(as.character(la$atracurium_mgh))
la$NAD_microgkgmin = as.numeric(as.character(la$NAD_microgkgmin))
la$T0_Vt_mlkg = as.numeric(as.character(la$T0_Vt_mlkg))
la$T0_TpsInpsi_sec = as.numeric(as.character(la$T0_TpsInpsi_sec))
la$T0_Pmax_cmH2O = as.numeric(as.character(la$T0_Pmax_cmH2O))
la$T0_Compliance_mlcmH2O = as.numeric(as.character(la$T0_Compliance_mlcmH2O))
la$T0_Ventmin_Lmin = as.numeric(as.character(la$T0_Ventmin_Lmin))
la$T0_pH = as.numeric(as.character(la$T0_pH))
la$T0_PaCO2 = as.numeric(as.character(la$T0_PaCO2))
la$T0_PaO2 = as.numeric(as.character(la$T0_PaO2))
la$T0_P_F = as.numeric(as.character(la$T0_P_F))
la$T0_HCO3minus = as.numeric(as.character(la$T0_HCO3minus))
la$T0_Lactate = as.numeric(as.character(la$T0_Lactate))
la$dureeMERPplusMRA_min = as.numeric(as.character(la$dureeMERPplusMRA_min))
la$T15_pH = as.numeric(as.character(la$T15_pH))
la$T15_PaO2 = as.numeric(as.character(la$T15_PaO2))
la$T15_P_F = as.numeric(as.character(la$T15_P_F))
la$T15_HCO3minus = as.numeric(as.character(la$T15_HCO3minus))
la$T15_Lactate = as.numeric(as.character(la$T15_Lactate))
la$Delta_P_F = as.numeric(as.character(la$Delta_P_F))
la$T0_apresMERPplusMRA_Vt_mlkg = as.numeric(as.character(la$T0_apresMERPplusMRA_Vt_mlkg))
la$rocuronium_mgh = as.numeric(as.character(la$rocuronium_mgh))
la$T0_FR_cpm = as.numeric(as.character(la$T0_FR_cpm))
la$T0_Vt_ml  = as.numeric(as.character(la$T0_Vt_ml))
la$T0_debit_Lmin = as.numeric(as.character(la$T0_debit_Lmin))
la$T0_Pplat_cmH2O = as.numeric(as.character(la$T0_Pplat_cmH2O)) 
la$T0_PEEP_cmH2O = as.numeric(as.character(la$T0_PEEP_cmH2O))
la$T0_Pmotrice_cmH2O = as.numeric(as.character(la$T0_Pmotrice_cmH2O))
la$FC = as.numeric(as.character(la$FC))
la$PAS = as.numeric(as.character(la$PAS))
la$PAD = as.numeric(as.character(la$PAD))
la$PAM = as.numeric(as.character(la$PAM))
la$SpO2 = as.numeric(as.character(la$SpO2))
la$TAPSE_mm = as.numeric(as.character(la$TAPSE_mm))
la$MERP_PEEP_15_Pplat = as.numeric(as.character(la$MERP_PEEP_15_Pplat))
la$VteCo = as.numeric(as.character(la$VteCo))
la$MERP_PEEP_5_Vte = as.numeric(as.character(la$MERP_PEEP_5_Vte))
la$MERP_PEEP_5_Pplat = as.numeric(as.character(la$MERP_PEEP_5_Pplat))
la$MRA_PEEP_20_Pplat = as.numeric(as.character(la$MRA_PEEP_20_Pplat))
la$MRA_PEEP_20_Pmotrice = as.numeric(as.character(la$MRA_PEEP_20_Pmotrice))
la$MRA_PEEP_18_Pplat = as.numeric(as.character(la$MRA_PEEP_18_Pplat))
la$MRA_PEEP_18_Pmotrice = as.numeric(as.character(la$MRA_PEEP_18_Pmotrice))
la$MRA_PEEP_16_Pplat = as.numeric(as.character(la$MRA_PEEP_16_Pplat))
la$MRA_PEEP_16_Pmotrice = as.numeric(as.character(la$MRA_PEEP_16_Pmotrice))
la$MRA_PEEP_14_Pplat = as.numeric(as.character(la$MRA_PEEP_14_Pplat))
la$MRA_PEEP_14_Pmotrice = as.numeric(as.character(la$MRA_PEEP_14_Pmotrice))
la$MRA_PEEP_12_Pplat = as.numeric(as.character(la$MRA_PEEP_12_Pplat))
la$MRA_PEEP_12_Pmotrice = as.numeric(as.character(la$MRA_PEEP_12_Pmotrice))
la$MRA_PEEP_10_Pplat = as.numeric(as.character(la$MRA_PEEP_10_Pplat))
la$MRA_PEEP_10_Pmotrice = as.numeric(as.character(la$MRA_PEEP_10_Pmotrice))
la$T15_PaCO2 = as.numeric(as.character(la$T15_PaCO2)) 
la$T15_A_FiO2 = as.numeric(as.character(la$T15_A_FiO2))
la$Vrec_VteaPEEP15 = as.numeric(as.character(la$Vrec_VteaPEEP15))
la$T0_apresMERPplusMRA_Vt_ml = as.numeric(as.character(la$T0_apresMERPplusMRA_Vt_ml))
la$T0_apresMERPplusMRA_FRcpm = as.numeric(as.character(la$T0_apresMERPplusMRA_FRcpm))
la$T0_apresMERPplusMRA_CompliancemlcmH2O = as.numeric(as.character(la$T0_apresMERPplusMRA_CompliancemlcmH2O))
la$T0_apresMERPplusMRA_FiO2 = as.numeric(as.character(la$T0_apresMERPplusMRA_FiO2))
la$T0_apresMERPplusMRA_Ventmin_Lmin = as.numeric(as.character(la$T0_apresMERPplusMRA_Ventmin_Lmin))
la$T0_apresMERPplusMRA_debit_Lmin = as.numeric(as.character(la$T0_apresMERPplusMRA_debit_Lmin))
la$T0_apresMERPplusMRA_TpsInpsi_sec = as.numeric(as.character(la$T0_apresMERPplusMRA_TpsInpsi_sec))
la$T0_apresMERPplusMRA_I_E = as.numeric(as.character(la$T0_apresMERPplusMRA_I_E))
la$T0_apresMERPplusMRA_Pplat_cmH2O = as.numeric(as.character(la$T0_apresMERPplusMRA_Pplat_cmH2O))
la$T0_apresMERPplusMRA_PEEP_cmH2O = as.numeric(as.character(la$T0_apresMERPplusMRA_PEEP_cmH2O))
la$T0_apresMERPplusMRA_PmotricecmH2O = as.numeric(as.character(la$T0_apresMERPplusMRA_PmotricecmH2O))
la$apresGdS15_postMRA_Vt_mlkg = as.numeric(as.character(la$apresGdS15_postMRA_Vt_mlkg))
la$apresGdS15_postMRA_SoitVt_ml = as.numeric(as.character(la$apresGdS15_postMRA_SoitVt_ml))
la$apresGdS15_postMRA_FR_cpm = as.numeric(as.character(la$apresGdS15_postMRA_FR_cpm))
la$apresGdS15_postMRA_Compliance_mlcmH2O = as.numeric(as.character(la$apresGdS15_postMRA_Compliance_mlcmH2O))

la$apresGdS15_postMRA_FiO2 = as.numeric(as.character(la$apresGdS15_postMRA_FiO2))
la$apresGdS15_postMRA_Ventmin_Lmin = as.numeric(as.character(la$apresGdS15_postMRA_Ventmin_Lmin))
la$apresGdS15_postMRA_debit_Lmin = as.numeric(as.character(la$apresGdS15_postMRA_debit_Lmin))
la$apresGdS15_postMRA_TpsInpsi_sec = as.numeric(as.character(la$apresGdS15_postMRA_TpsInpsi_sec))
la$apresGdS15_postMRA_I_E = as.numeric(as.character(la$apresGdS15_postMRA_I_E))
la$apresGdS15_postMRA_PplatcmH2O = as.numeric(as.character(la$apresGdS15_postMRA_PplatcmH2O))
la$apresGdS15_postMRA_PEEP_cmH2O = as.numeric(as.character(la$apresGdS15_postMRA_PEEP_cmH2O))
la$apresGdS15_postMRA_PmotricecmH2O = as.numeric(as.character(la$apresGdS15_postMRA_PmotricecmH2O))

la$dureeUSI_j = as.numeric(as.character(la$dureeUSI_j))
la$sejourpostrea_j = as.numeric(as.character(la$sejourpostrea_j))
la$sejourtotal_j = as.numeric(as.character(la$sejourtotal_j)) 
la$dureeVM_j = as.numeric(as.character(la$dureeVM_j))
la$jourssansVM_sur28j = as.numeric(as.character(la$jourssansVM_sur28j)) 
la$dureeP_F_inf200j = as.numeric(as.character(la$dureeP_F_inf200j)) 
la$jourscurarises = as.numeric(as.character(la$jourscurarises)) 
la$jourssousNO = as.numeric(as.character(la$jourssousNO)) 
la$jourssousECM0 = as.numeric(as.character(la$jourssousECM0)) 
la$dureeUSI_j = as.numeric(as.character(la$dureeUSI_j))
la$sejourpostrea_j = as.numeric(as.character(la$sejourpostrea_j))
la$sejourtotal_j = as.numeric(as.character(la$sejourtotal_j))
la$dureeVM_j = as.numeric(as.character(la$dureeVM_j))
la$jourssansVM_sur28j = as.numeric(as.character(la$jourssansVM_sur28j))
la$jourscurarises = as.numeric(as.character(la$jourscurarises)) 

 

# CREATE THE TABLEONE OBJECT
CreateTableOne(data = la) 

variables = c("Age", "Homme", "Taille", "Poids", "IMC",  
               "dela_reainclusion", "delai_IOTinclusion", 
              "delai_SDRAinclusion", "seancesDV", "seancesDV_avantinclusion01", 
              "seancesDV_avantinclusion", "etiologieSDRApulmonaire", "covid01", 
              "delai_covidsympt_inclusion", "delai_COVID_Inclusion", "delai_COVID_Inclusion7j", 
              "delai_COVID_Inclusion7j14j", "delai_COVID_Inclusion14j", "FDR_SDRA_pneumopathie", 
              "FDR_SDRA_sepsis", "FDR_SDRA_choc", "FDR_SDRA_contusionspulm", 
              "FDR_SDRA_inhalation", "FDR_SDRA_pancréatiteaigue", "FDR_SDRA_péritonite", 
              "FDR_SDRA_ischémiedigestive", "FDR_SDRA_traumatisegrave", "FDR_SDRA_transfusionmassive", 
              "FDR_SDRA_postCEC", "FDR_SDRA_autres", "FDR_SDRA_siautre", "SOFA", 
              "IGSII", "charlsonscore", "hypnovel01", "hypnovel_mgh", "propofol01", "Delta_P_F_sup20")
              
variables2 = c("propofol_mgh", "sufenta01", "sufenta_migrogh", "cisatracurium01", 
              "cisatracurium_mgh", "atracurium01", "atracurium_mgh", "rocuronium01", 
              "rocuronium_mgh", "NAD01", "NAD_microgkgmin", "NAD_sup02", "NAD_sup05", 
              "dobutamine01", "NO01", "NO_ppm", "NOinf5", "NO_5_10", "NO_sup10", 
              "T0_Vt_mlkg", "T0_Vt_ml", "T0_FR_cpm", "T0_debit_Lmin", "T0_TpsInpsi_sec", 
              "T0_I_E", "T0_Pmax_cmH2O", "T0_Pplat_cmH2O", "T0_Pplat_inf20cmH2O", 
              "T0_Pplat_20_24cmH2O", "T0_Pplat_25_30_cmH2O", "T0_Pplat_sup30cmH2O", 
              "T0_PEEP_cmH2O", "T0_PEEP_5_9_cmH2O", "T0_PEEP_10_15_cmH2O", 
              "T0_Pmotrice_cmH2O", "T0_Pmotrice_inf10cmH2O", "T0_Pmotrice_10_15cmH2O", 
              "T0_Pmotrice_sup15cmH2O", "T0_Compliance_mlcmH2O", "T0_Compliance_inf20mlcmH2O", 
              "T0_Compliance_20_40_mlcmH2O", "T0_Compliance_sup40mlcmH2O", "Delta_P_F_sup20")

variables3 = c("T0_Ventmin_Lmin", "T0_FiO2", "FC", "PAS", "PAD", "PAM", "SpO2", 
              "SpO2_sup96p100", "SpO2_92_96", "SpO2_inf92", "ETT_faite", "FEVG_pourc", 
              "FEVG_sup50", "FEVG_30_50", "FEVG_inf30", "PRVG_elevees", "PRVG_basses", 
              "PRVG_zonegrise", "dysfonction_VD", "TAPSE_mm", "TAPSE_inf12mm", 
              "PAPS_mmHg", "PAPS_inf40mmHg", "PAPS_sup40mmHg", "T0_pH", "T0_PaCO2", 
              "T0_PaO2", "T0_P_F", "T0_A_FiO2", "T0_HCO3minus", "T0_Lactate", "Delta_P_F_sup20")

variables3bis = c("dureeMERPplusMRA_min", "dureeMERPplusMRA_inf10min", "dureeMERPplusMRA_10_15min", 
              "dureeMERPplusMRA_plus15min", "MERP_PEEP_15Vte", "MERP_PEEP_15_Pplat", 
              "VteCo", "MERP_PEEP_5_Vte", "MERP_PEEP_5_Pplat", "MRA_PEEP_20_Pplat", 
              "MRA_PEEP_20_Pmotrice", "MRA_PEEP_18_Pplat", "MRA_PEEP_18_Pmotrice", 
              "MRA_PEEP_16_Pplat", "MRA_PEEP_16_Pmotrice", "MRA_PEEP_14_Pplat", 
              "MRA_PEEP_14_Pmotrice", "MRA_PEEP_12_Pplat", "MRA_PEEP_12_Pmotrice", "Delta_P_F_sup20")

variables4 = c("MRA_PEEP_10_Pplat", "MRA_PEEP_10_Pmotrice", "T15_pH", "T15_PaCO2", 
              "T15_PaO2", "T15_P_F", "T15_A_FiO2", "T15_HCO3minus", "T15_Lactate", 
              "Best_PEEP", "BestPEEP_soitPmotrice", "Vrec_VteaPEEP15", "Vrec_inf50ml", 
              "Vrec_50_99ml", "Vrec_100_150ml", "Vrecsup150ml", "Delta_P_F", 
              "Delta_P_F_sup20", "Delta_P_F_neg", "T0_apresMERPplusMRA_Vt_mlkg")

variables4bis = c("T0_apresMERPplusMRA_Vt_ml", "T0_apresMERPplusMRA_FRcpm", "T0_apresMERPplusMRA_CompliancemlcmH2O", 
              "T0_apresMERPplusMRA_Compliance_inf20mlcmH2O", "T0_apresMERPplusMRA_Compliance_20_40mlcmH2O", 
              "T0_apresMERPplusMRA_Compliance_sup40mlcmH2O", "T0_apresMERPplusMRA_FiO2", 
              "T0_apresMERPplusMRA_Ventmin_Lmin", "T0_apresMERPplusMRA_debit_Lmin", 
              "T0_apresMERPplusMRA_TpsInpsi_sec", "T0_apresMERPplusMRA_I_E", "Delta_P_F_sup20")
              
variables5 = c("T0_apresMERPplusMRA_Pplat_cmH2O", "T0_apresMERPplusMRA_Pplat_inf20cmH2O", 
              "T0_apresMERPplusMRA_Pplat_20_25_cmH2O", "T0_apresMERPplusMRA_Pplat_25_30_cmH2O", 
              "T0_apresMERPplusMRA_Pplatsup30cmH2O", "T0_apresMERPplusMRA_PEEP_cmH2O", 
              "T0_apresMERPplusMRA_PEEP_5_10_cmH2O", "T0_apresMERPplusMRA_PEEP_10_15cmH2O", 
              "T0_apresMERPplusMRA_PEEPsup15cmH2O", "T0_apresMERPplusMRA_PmotricecmH2O", 
              "T0_apresMERPplusMRA_Pmotrice_inf10cmH2O", "T0_apresMERPplusMRA_Pmotrice_10_15cmH2O", 
              "T0_apresMERPplusMRA_Pmotrice_sup15cmH2O", "apresGdS15_postMRA_Vt_mlkg", 
              "apresGdS15_postMRA_SoitVt_ml", "apresGdS15_postMRA_FR_cpm", 
              "apresGdS15_postMRA_Compliance_mlcmH2O", "apresGdS15_postMRA_Compliance_inf20mlcmH2O", "Delta_P_F_sup20")

variables6 = c("apresGdS15_postMRA_Compliance_20_40mlcmH2O", "apresGdS15_postMRA_Compliance_sup40mlcmH2O", 
              "apresGdS15_postMRA_FiO2", "apresGdS15_postMRA_Ventmin_Lmin", 
              "apresGdS15_postMRA_debit_Lmin", "apresGdS15_postMRA_TpsInpsi_sec", 
              "apresGdS15_postMRA_I_E", "apresGdS15_postMRA_PplatcmH2O", "apresGdS15_postMRA_Pplat_inf20cmH2O", 
              "apresGdS15_postMRA_Pplat_20_25cmH2O", "apresGdS15_postMRA_Pplat_25_30cmH2O", 
              "apresGdS15_postMRA_Pplat_sup30cmH2O", "apresGdS15_postMRA_PEEP_cmH2O", 
              "apresGdS15_postMRA_PEEP_5_10cmH2O", "apresGdS15_postMRA_PEEP_10_15cmH2O", 
              "apresGdS15_postMRA_PEEP_sup15cmH2O", "apresGdS15_postMRA_PmotricecmH2O", 
              "apresGdS15_postMRA_Pmotrice_inf10cmH2O", "apresGdS15_postMRA_Pmotrice_10_15cmH2O", "Delta_P_F_sup20")

variables7 = c("arretMERP", "arretMRA", 
              "PEEPMAX", "Bradycardie", "Fcminimale", "Tachycardie", "Fcmaximale", 
              "troublerythme", "ACR", "hypotension_arterielle", "PASminimale", 
              "PADminimales", "PAMminimales", "SpO2_inf85p100", "SpO2minimale", 
              "Pneumothorax", "Autres", "sortieUSI_vivant", "dureeUSI_j", "sortiehosp_vivant01", 
              "sejourpostrea_j", "sejourtotal_j", "vivant_j28", "dureeVM_j", 
              "jourssansVM_sur28j", "dureeP_F_inf200j", "jourscurarises", "curarisation_infegal48h", 
              "necessiteNO", "jourssousNO", "necessiteAlmitrine", "Delta_P_F_sup20", "necessiteECMO")

categorical = c( "Homme",
                "seancesDV", "seancesDV_avantinclusion01", 
                "seancesDV_avantinclusion", "etiologieSDRApulmonaire", "covid01", 
                "delai_COVID_Inclusion7j14j", "FDR_SDRA_pneumopathie", 
                "FDR_SDRA_sepsis", "FDR_SDRA_choc", "FDR_SDRA_contusionspulm", 
                "FDR_SDRA_inhalation", "FDR_SDRA_pancréatiteaigue", "FDR_SDRA_péritonite", 
                "FDR_SDRA_ischémiedigestive", "FDR_SDRA_traumatisegrave", "FDR_SDRA_transfusionmassive", 
                "FDR_SDRA_postCEC", "FDR_SDRA_autres", "FDR_SDRA_siautre",  
                "hypnovel01", "propofol01", "Delta_P_F_sup20")

categorical2 = c("sufenta01", "cisatracurium01", 
                "atracurium01", "rocuronium01", 
                "NAD01", "NAD_sup02", "NAD_sup05", 
                "dobutamine01", "NO01", "NO_ppm", "NOinf5", "NO_5_10", "NO_sup10", 
                "T0_I_E", "T0_Pplat_inf20cmH2O", 
                "T0_Pplat_20_24cmH2O", "T0_Pplat_25_30_cmH2O", "T0_Pplat_sup30cmH2O", 
                "T0_PEEP_5_9_cmH2O", "T0_PEEP_10_15_cmH2O", 
                "T0_Pmotrice_inf10cmH2O", "T0_Pmotrice_10_15cmH2O", 
                "T0_Pmotrice_sup15cmH2O", "T0_Compliance_inf20mlcmH2O", 
                "T0_Compliance_20_40_mlcmH2O", "T0_Compliance_sup40mlcmH2O", "Delta_P_F_sup20")

categorical3 = c("T0_FiO2",  
                "SpO2_sup96p100", "SpO2_92_96", "SpO2_inf92", "ETT_faite", "FEVG_pourc", 
                "FEVG_sup50", "FEVG_30_50", "FEVG_inf30", "PRVG_elevees", "PRVG_basses", 
                "PRVG_zonegrise", "dysfonction_VD", "TAPSE_inf12mm", 
                "PAPS_mmHg", "PAPS_inf40mmHg", "PAPS_sup40mmHg",  
                "T0_A_FiO2", "Delta_P_F_sup20")

categorical3bis = c("dureeMERPplusMRA_inf10min", "dureeMERPplusMRA_10_15min", 
                "dureeMERPplusMRA_plus15min", "Delta_P_F_sup20")

categorical4 = c("Best_PEEP", "BestPEEP_soitPmotrice", "Vrec_inf50ml", 
                "Vrec_50_99ml", "Vrec_100_150ml", "Vrecsup150ml",  
                "Delta_P_F_sup20", "Delta_P_F_neg")

categorical4bis = c("T0_apresMERPplusMRA_Compliance_inf20mlcmH2O", "T0_apresMERPplusMRA_Compliance_20_40mlcmH2O", 
                "T0_apresMERPplusMRA_Compliance_sup40mlcmH2O", "Delta_P_F_sup20")

categorical5 = c("T0_apresMERPplusMRA_Pplat_inf20cmH2O", 
                "T0_apresMERPplusMRA_Pplat_20_25_cmH2O", "T0_apresMERPplusMRA_Pplat_25_30_cmH2O", 
                "T0_apresMERPplusMRA_Pplatsup30cmH2O", 
                "T0_apresMERPplusMRA_PEEP_5_10_cmH2O", "T0_apresMERPplusMRA_PEEP_10_15cmH2O", 
                "T0_apresMERPplusMRA_PEEPsup15cmH2O",
                "T0_apresMERPplusMRA_Pmotrice_inf10cmH2O", "T0_apresMERPplusMRA_Pmotrice_10_15cmH2O", 
                "T0_apresMERPplusMRA_Pmotrice_sup15cmH2O", 
                "apresGdS15_postMRA_Compliance_inf20mlcmH2O", "Delta_P_F_sup20")

categorical6 = c("apresGdS15_postMRA_Compliance_20_40mlcmH2O", "apresGdS15_postMRA_Compliance_sup40mlcmH2O", 
                "apresGdS15_postMRA_Pplat_inf20cmH2O", 
                "apresGdS15_postMRA_Pplat_20_25cmH2O", "apresGdS15_postMRA_Pplat_25_30cmH2O", 
                "apresGdS15_postMRA_Pplat_sup30cmH2O",  
                "apresGdS15_postMRA_PEEP_5_10cmH2O", "apresGdS15_postMRA_PEEP_10_15cmH2O", 
                "apresGdS15_postMRA_PEEP_sup15cmH2O", 
                "apresGdS15_postMRA_Pmotrice_inf10cmH2O", "apresGdS15_postMRA_Pmotrice_10_15cmH2O",
                "Delta_P_F_sup20")

categorical7 = c("arretMERP", "arretMRA", 
                "PEEPMAX", "Bradycardie", "Fcminimale", "Tachycardie", "Fcmaximale", 
                "troublerythme", "ACR", "hypotension_arterielle", "PASminimale", 
                "PADminimales", "PAMminimales", "SpO2_inf85p100", "SpO2minimale", 
                "Pneumothorax", "Autres", "sortieUSI_vivant", "sortiehosp_vivant01", 
                 "vivant_j28",  
                  "curarisation_infegal48h", 
                "necessiteAlmitrine", "necessiteECMO", "Delta_P_F_sup20")


# CREATE THE DESCRIPTIVE TABLE
tab1 = CreateTableOne(vars = variables, data = la, factorVars = categorical)
print(tab1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

tab2 = CreateTableOne(vars = variables2, data = la, factorVars = categorical2)
print(tab2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

tab3 = CreateTableOne(vars = variables3, data = la, factorVars = categorical3)
print(tab3, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

        tab3bis = CreateTableOne(vars = variables3bis, data = la, factorVars = categorical3bis)
        print(tab3bis, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

tab4 = CreateTableOne(vars = variables4, data = la, factorVars = categorical4)
print(tab4, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

        tab4bis = CreateTableOne(vars = variables4bis, data = la, factorVars = categorical4bis)
        print(tab4bis, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

tab5 = CreateTableOne(vars = variables5, data = la, factorVars = categorical5)
print(tab5, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

tab6 = CreateTableOne(vars = variables6, data = la, factorVars = categorical6)
print(tab6, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

tab7 = CreateTableOne(vars = variables7, data = la, factorVars = categorical7)
print(tab7, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

la$Delta_P_F_sup20 <- as.factor(la$Delta_P_F_sup20)

# CREATE THE UNIVARIATE TABLE 
tab1bv = CreateTableOne(vars = variables, data = la, factorVars = categorical, test = TRUE,
                      strata = "Delta_P_F_sup20")
print(tab1bv, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

tab2bv = CreateTableOne(vars = variables2, data = la, factorVars = categorical2, test = TRUE,
                        strata = "Delta_P_F_sup20")
print(tab2bv, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

tab3bv = CreateTableOne(vars = variables3, data = la, factorVars = categorical3, test = TRUE,
                        strata = "Delta_P_F_sup20")
print(tab3bv, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

        tab3bvbis = CreateTableOne(vars = variables3bis, data = la, factorVars = categorical3bis, test = TRUE,
                                strata = "Delta_P_F_sup20")
        print(tab3bvbis, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
        
tab4bv = CreateTableOne(vars = variables4, data = la, factorVars = categorical4, test = TRUE,
                                strata = "Delta_P_F_sup20")
print(tab4bv, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

        tab4bvbis = CreateTableOne(vars = variables4bis, data = la, factorVars = categorical4bis, test = TRUE,
                                strata = "Delta_P_F_sup20")
        print(tab4bvbis, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
        
tab5bv = CreateTableOne(vars = variables5, data = la, factorVars = categorical5, test = TRUE,
                                strata = "Delta_P_F_sup20")
print(tab5bv, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

tab6bv = CreateTableOne(vars = variables6, data = la, factorVars = categorical6, test = TRUE,
                        strata = "Delta_P_F_sup20")
print(tab6bv, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

tab7bv = CreateTableOne(vars = variables7, data = la, factorVars = categorical7, test = TRUE,
                        strata = "Delta_P_F_sup20")
print(tab7bv, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

####################


# BEST PEEP
 la$Best_PEEP = as.numeric(as.character(la$Best_PEEP))
 aggregate(la$Best_PEEP, list(la$Delta_P_F_sup20), FUN=mean) 
 aggregate(la$Best_PEEP, list(la$Delta_P_F_sup20), FUN=sd) 
 t.test(la$Best_PEEP, la$Delta_P_F_sup20, conf.level = 0.95)

 # (T0_PEEP_cmH2O) - (apresGdS15_postMRA_PEEP_cmH2O)
la$dcmh2o = as.numeric(as.character(la$T0_PEEP_cmH2O)) -  as.numeric(as.character(la$apresGdS15_postMRA_PEEP_cmH2O))
aggregate(la$dcmh2o, list(la$Delta_P_F_sup20), FUN=mean) 
aggregate(la$dcmh2o, list(la$Delta_P_F_sup20), FUN=sd) 
t.test(la$dcmh2o, la$Delta_P_F_sup20, conf.level = 0.95)

 # (T0_Pplat_cmH2O) - (apresGdS15_postMRA_PplatcmH2O)
 la$dpplat = as.numeric(as.character(la$T0_Pplat_cmH2O)) - as.numeric(as.character(la$apresGdS15_postMRA_PplatcmH2O))
aggregate(la$dpplat, list(la$Delta_P_F_sup20), FUN=mean) 
aggregate(la$dpplat, list(la$Delta_P_F_sup20), FUN=sd) 
t.test(la$dpplat, la$Delta_P_F_sup20, conf.level = 0.95)

# (T0_Pmotrice_cmH2O) - (apresGdS15_postMRA_PmotricecmH2O)
 la$motrice = as.numeric(as.character(la$T0_Pmotrice_cmH2O)) - as.numeric(as.character(la$apresGdS15_postMRA_PmotricecmH2O))
aggregate(la$motrice, list(la$Delta_P_F_sup20), FUN=mean) 
 aggregate(la$motrice, list(la$Delta_P_F_sup20), FUN=sd) 
 t.test(la$motrice, la$Delta_P_F_sup20, conf.level = 0.95)

# (T0_Compliance_mlcmH2O) - (apresGdS15_postMRA_Compliance_mlcmH2O)
la$compliance = as.numeric(as.character(la$T0_Compliance_mlcmH2O)) - as.numeric(as.character(la$apresGdS15_postMRA_Compliance_mlcmH2O))      
aggregate(la$compliance, list(la$Delta_P_F_sup20), FUN=mean) 
aggregate(la$compliance, list(la$Delta_P_F_sup20), FUN=sd) 
t.test(la$compliance, la$Delta_P_F_sup20, conf.level = 0.95)

# QUALI :
str(la$Delta_P_F_sup20)
la$Delta_P_F_sup20 = as.factor(la$Delta_P_F_sup20)

 # Plat decreased
 la$dpplat_change[la$T0_Pplat_cmH2O < la$apresGdS15_postMRA_PplatcmH2O] = "incr"
 la$dpplat_change[la$T0_Pplat_cmH2O >  la$apresGdS15_postMRA_PplatcmH2O] = "decr"
 table(la$dpplat_change)
 chisq.test(la$dpplat_change, la$Delta_P_F_sup20, correct = FALSE)
 fisher.test(la$dpplat_change, la$Delta_P_F_sup20)

 # Pmotrice
la$motrice_change[la$T0_Pmotrice_cmH2O < la$apresGdS15_postMRA_PmotricecmH2O] = "incr"
la$motrice_change[la$T0_Pmotrice_cmH2O  > la$apresGdS15_postMRA_PmotricecmH2O] = "decr"
table(la$motrice_change)
fisher.test(la$motrice_change, la$Delta_P_F_sup20)

# Compliance
la$compl_change[la$T0_Compliance_mlcmH2O < la$apresGdS15_postMRA_Compliance_mlcmH2O] = "incr"
la$compl_change[la$T0_Compliance_mlcmH2O >  la$apresGdS15_postMRA_Compliance_mlcmH2O] = "decr"
str(la$compl_change)
table(la$compl_change)
fisher.test(la$compl_change, la$Delta_P_F_sup20)
