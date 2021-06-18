# DATA
getwd()
setwd("P:/CONSULTATION/Lhermitte_Amaury/DATA") # On PC
setwd("/Users/damianocerasuolo/Desktop/UBRC/2021_CONSULT/Lhermitte_Amaury/DATA") # On Mac

#-------------------------------------------------------------------------------

# PACKAGES
#install.packages("readxl")
library("readxl")

la <- read_excel("la.xlsx", na="NA")

View(la)
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
                    sufenta_migrogh = "Posologie (??g/h)",
                    cisatracurium01 = "Cisatracurium",
                    cisatracurium_mgh = "Posologie (mg/h)",
                    atracurium01 = "Atracurium",
                    atracurium_mgh = "Posologie (mg/h)",
                    rocuronium01 = "Rocuronium",
                    rocuronium_mgh = "Posologie (mg/h)",
                    NAD01 = "NAD",
                    NAD_microgkgmin = "Posologie (??g/kg/min)",
                    NAD_sup02 = "Poso NAD > 0,2 ??g/kg/min",
                    NAD_sup05 = "Poso NAD > 0,5 ??g/kg/min",
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
                    T15_pH = "T15' pH",
                    T15_PaCO2 = "T15' PaCO2",
                    T15_PaO2 = "T15' PaO2",
                    T15_P_F = "T15'P/F",
                    T15_A_FiO2 = "T15' À FiO2",
                    T15_HCO3minus = "T15' HCO3-",
                    T15_Lactate = "T15' Lactate",
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
                    apresGdS15_postMRA_Vt_mlkg = "Après GdS 15' post-MRA Vt (ml/kg)",
                    apresGdS15_postMRA_SoitVt_ml = "Après GdS 15' post-MRA Soit Vt (ml)",
                    apresGdS15_postMRA_FR_cpm = "Après GdS 15' post-MRA FR (cpm)",
                    apresGdS15_postMRA_Compliance_mlcmH2O = "Après GdS 15' post-MRA Compliance (ml/cmH2O)",
                    apresGdS15_postMRA_Compliance_inf20mlcmH2O = "Après GdS 15' post-MRA Compliance < 20ml/cmH2O",
                    apresGdS15_postMRA_Compliance_20_40mlcmH2O = "Après GdS 15' post-MRA Compliance [20-40]ml/cmH2O",
                    apresGdS15_postMRA_Compliance_sup40mlcmH2O = "Après GdS 15' post-MRA Compliance > 40ml/cmH2O",
                    apresGdS15_postMRA_FiO2 = "Après GdS 15' post-MRA FiO2",
                    apresGdS15_postMRA_Ventmin_Lmin = "Après GdS 15' post-MRA Vent min (L/min)",
                    apresGdS15_postMRA_debit_Lmin = "Après GdS 15' post-MRA Débit (L/min)",
                    apresGdS15_postMRA_TpsInpsi_sec = "Après GdS 15' post-MRA Tps inpsi (sec)",
                    apresGdS15_postMRA_I_E = "Après GdS 15' post-MRA I/E",
                    apresGdS15_postMRA_PplatcmH2O = "Après GdS 15' post-MRA Pplat (cmH2O)",
                    apresGdS15_postMRA_Pplat_inf20cmH2O = "Après GdS 15' post-MRA Pplat <20 cmH2O",
                    apresGdS15_postMRA_Pplat_20_25cmH2O = "Après GdS 15' post-MRA Pplat [20-25] cmH2O",
                    apresGdS15_postMRA_Pplat_25_30cmH2O = "Après GdS 15' post-MRA Pplat [25-30] cmH2O",
                    apresGdS15_postMRA_Pplat_sup30cmH2O = "Après GdS 15' post-MRA  Pplat >30 cmH2O",
                    apresGdS15_postMRA_PEEP_cmH2O = "Après GdS 15' post-MRA PEEP (cmH2O)",
                    apresGdS15_postMRA_PEEP_5_10cmH2O = "Après GdS 15' post-MRA PEEP [5-10] cmH2O",
                    apresGdS15_postMRA_PEEP_10_15cmH2O = "Après GdS 15' post-MRA PEEP [10-15] cmH2O",
                    apresGdS15_postMRA_PEEP_sup15cmH2O = "Après GdS 15' post-MRA PEEP > 15 cmH2O",
                    apresGdS15_postMRA_PmotricecmH2O = "Après GdS 15' post-MRA Pmotrice (cmH2O)",
                    apresGdS15_postMRA_Pmotrice_inf10cmH2O = "Après GdS 15' post-MRA Pmotrice <10 cmH2O",
                    apresGdS15_postMRA_Pmotrice_10_15cmH2O = "Après GdS 15' post-MRA Pmotrice [10-15] cmH2O",
                    apresGdS15_postMRA_Pmotrice_sup15cmH2O = "Après GdS 15' post-MRA Pmotrice >15 cmH2O",
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
                    jourssousECM0 = "Nbr jours sous ECM0"
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

coords(roc.sup20, x="best", best.method="youden")

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

# CREATE THE TABLEONE OBJECT
CreateTableOne(data = la) 

variables = c("URGn", "KTTINIn", "EPOINIn", 
              "nephgp", "METHOn", "techn", "MODALn", "VAVn", 
              "traitement", "PDS", "TAIL", "IRCn", "O2n", "ICn", "ICOROn", 
              "IDMn", "RYTHMn", "ANEVn", "AMIn", "AVCAITn", "KCn", "VHBn", 
              "VHCn", "CIRHn", "VIHn", "SIDAn", "HANDn", "AMPn", "PLEGn", "CECITEn", 
              "COMPORTn", "TYPDIABn", "STADICn", "STDAMIn", "STDCIRHn", "TABACn", 
              "bmi", "tabac2", "iresp", "sero", "coro", "foie", "comCV", "comcvcl", 
              "comcvcl2", "sex", "age", "ETAT_DERNOUV2019", "delai_IRT", "delai_DC", 
              "delai_TX", "delai_SVR", "delai_PDV", "delai_DERNOUV2019", "groupes6", 
              "categories18", "groupes6_CA1", "categories18_CA1", "groupes6_CA2", 
              "categories18_CA2", "MOTIF_An", "CPKMEDn", "REFUSn", "SOR_ANN", "grouping01")

categorical = c("URGn", "KTTINIn", "EPOINIn", 
                "nephgp", "METHOn", "techn", "MODALn", "VAVn", 
                "traitement", "IRCn", "O2n", "ICn", "ICOROn", 
                "IDMn", "RYTHMn", "ANEVn", "AMIn", "AVCAITn", "KCn", "VHBn", 
                "VHCn", "CIRHn", "VIHn", "SIDAn", "HANDn", "AMPn", "PLEGn", "CECITEn", 
                "COMPORTn", "TYPDIABn", "STADICn", "STDAMIn", "STDCIRHn", "TABACn", 
                "tabac2", "iresp", "sero", "coro", "foie", "comCV", "comcvcl", 
                "comcvcl2", "sex", "groupes6", 
                "categories18", "groupes6_CA1", "categories18_CA1", "groupes6_CA2", 
                "categories18_CA2", "MOTIF_An", "CPKMEDn", "REFUSn", "SOR_ANN", "grouping01")


# CREATE THE DESCRIPTIVE TABLE
tab1 = CreateTableOne(vars = variables, data = apkd, factorVars = categorical)
print(tab1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

# CREATE THE UNIVARIATE TABLE 
tab2 = CreateTableOne(vars = variables, data = apkd, factorVars = categorical, test = TRUE,
                      strata = "grouping01")
print(tab2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)
