################################################
################################################
################################################
### Progress of intra-annual growth dynamics from VS model outputs
### {JT, October 2019}
################################################
################################################
################################################



################################################
### Reading outputs of VS model
################################################

library(readxl)


### Vystupy z VS - Ecuador
GrM <- data.frame(t(read.table("e:/JJC_VS/#Ecuador/SAVE/gM.txt")))
zeros <- rep(0, 12)
GrM <- rbind(zeros, GrM); rownames(GrM) <- c(1990:2015)

GrT <- data.frame(t(read.table("e:/JJC_VS/#Ecuador/SAVE/gT.txt")))
GrT <- rbind(zeros, GrT); rownames(GrT) <- c(1990:2015)

SM <- data.frame(t(read.table("e:/JJC_VS/#Ecuador/SAVE/moist.txt")))
SM <- rbind(zeros, SM); rownames(SM) <- c(1990:2015)
temp <- data.frame(t(read.table("e:/JJC_VS/#Ecuador/SAVE/temp.txt")))
temp <- rbind(zeros, temp); rownames(temp) <- c(1990:2015)

PAR <- read.table("e:/JJC_VS/#Ecuador/SAVE/Par.txt", quote="\""); rownames(PAR) <- "Ecuador"; colnames(PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 

GrE <- data.frame(t(read.table("e:/JJC_VS/#Ecuador/SAVE/gE.txt")))

ObsMod <- read.table("e:/JJC_VS/#Ecuador/SAVE/ObsMod.txt"); colnames(ObsMod) <- c("YEAR", "OBS", "MOD"); ObsMod$YEAR <- c(1991:2015)
ObsMod$OBS <- (ObsMod$OBS - mean(ObsMod$OBS)) / sd(ObsMod$OBS) 

### Vystupy z VS - declining and nondeclining pines from Spain
MD_gT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/MDgT.txt", quote="\"")
MD_gM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/MDgM.txt", quote="\"")
MD_gINT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/MDgINT.txt", quote="\"")
MD_gE <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/MDgE.txt", quote="\"")
MD_OBSMOD <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/MD_OBSMOD.txt", quote="\""); colnames(MD_OBSMOD) <- c("YEAR", "OBS", "MOD")
MD_PAR <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/MD_PAR.txt", quote="\""); rownames(MD_PAR) <- "MD"; colnames(MD_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
MD_SM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/MD_sm.txt", quote="\"")
MD_Temp <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/MD_t.txt", quote="\"")

MN_gT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/MNgT.txt", quote="\"")
MN_gM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/MNgM.txt", quote="\"")
MN_gINT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/MNgINT.txt", quote="\"")
MN_gE <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/MNgE.txt", quote="\"")
MN_OBSMOD <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/MN_OBSMOD.txt", quote="\""); colnames(MN_OBSMOD) <- c("YEAR", "OBS", "MOD")
MN_PAR <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/MN_PAR.txt", quote="\""); rownames(MN_PAR) <- "MN"; colnames(MN_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
MN_SM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/MN_sm.txt", quote="\"")
MN_Temp <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/MN_t.txt", quote="\"")

OD_gT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ODgT.txt", quote="\"")
OD_gM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ODgM.txt", quote="\"")
OD_gINT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ODgINT.txt", quote="\"")
OD_gE <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ODgE.txt", quote="\"")
OD_OBSMOD <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/OD_OBSMOD.txt", quote="\""); colnames(OD_OBSMOD) <- c("YEAR", "OBS", "MOD")
OD_PAR <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/OD_PAR.txt", quote="\""); rownames(OD_PAR) <- "OD"; colnames(OD_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
OD_SM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/OD_sm.txt", quote="\"")
OD_Temp <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/OD_t.txt", quote="\"")

ON_gT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ONgT.txt", quote="\"")
ON_gM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ONgM.txt", quote="\"")
ON_gINT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ONgINT.txt", quote="\"")
ON_gE <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ONgE.txt", quote="\"")
ON_OBSMOD <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ON_OBSMOD.txt", quote="\""); colnames(ON_OBSMOD) <- c("YEAR", "OBS", "MOD")
ON_PAR <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ON_PAR.txt", quote="\""); rownames(ON_PAR) <- "ON"; colnames(ON_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
ON_SM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ON_sm.txt", quote="\"")
ON_Temp <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ON_t.txt", quote="\"")

OH_gT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/OHgT.txt", quote="\"")
OH_gM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/OHgM.txt", quote="\"")
OH_gINT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/OHgINT.txt", quote="\"")
OH_gE <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/OHgE.txt", quote="\"")
OH_OBSMOD <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/OH_OBSMOD.txt", quote="\""); colnames(OH_OBSMOD) <- c("YEAR", "OBS", "MOD")
OH_PAR <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/OH_PAR.txt", quote="\""); rownames(OH_PAR) <- "OH"; colnames(OH_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
OH_SM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/OH_sm.txt", quote="\"")
OH_Temp <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/OH_t.txt", quote="\"")

ED_gT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/EDgT.txt", quote="\"")
ED_gM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/EDgM.txt", quote="\"")
ED_gINT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/EDgINT.txt", quote="\"")
ED_gE <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/EDgE.txt", quote="\"")
ED_OBSMOD <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ED_OBSMOD.txt", quote="\""); colnames(ED_OBSMOD) <- c("YEAR", "OBS", "MOD")
ED_PAR <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ED_PAR.txt", quote="\""); rownames(ED_PAR) <- "ED"; colnames(ED_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
ED_SM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ED_sm.txt", quote="\"")
ED_Temp <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ED_t.txt", quote="\"")

EN_gT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ENgT.txt", quote="\"")
EN_gM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ENgM.txt", quote="\"")
EN_gINT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ENgINT.txt", quote="\"")
EN_gE <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/ENgE.txt", quote="\"")
EN_OBSMOD <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/EN_OBSMOD.txt", quote="\""); colnames(EN_OBSMOD) <- c("YEAR", "OBS", "MOD")
EN_PAR <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/EN_PAR.txt", quote="\""); rownames(EN_PAR) <- "EN"; colnames(EN_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
EN_SM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/EN_sm.txt", quote="\"")
EN_Temp <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/EN_t.txt", quote="\"")

PD_gT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PDgT.txt", quote="\"")
PD_gM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PDgM.txt", quote="\"")
PD_gINT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PDgINT.txt", quote="\"")
PD_gE <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PDgE.txt", quote="\"")
PD_OBSMOD <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PD_OBSMOD.txt", quote="\""); colnames(PD_OBSMOD) <- c("YEAR", "OBS", "MOD")
PD_PAR <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PD_PAR.txt", quote="\""); rownames(PD_PAR) <- "PD"; colnames(PD_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
PD_SM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PD_sm.txt", quote="\"")
PD_Temp <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PD_t.txt", quote="\"")

PN_gT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PNgT.txt", quote="\"")
PN_gM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PNgM.txt", quote="\"")
PN_gINT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PNgINT.txt", quote="\"")
PN_gE <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PNgE.txt", quote="\"")
PN_OBSMOD <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PN_OBSMOD.txt", quote="\""); colnames(PN_OBSMOD) <- c("YEAR", "OBS", "MOD")
PN_PAR <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PN_PAR.txt", quote="\""); rownames(PN_PAR) <- "PN"; colnames(PN_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
PN_SM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PN_sm.txt", quote="\"")
PN_Temp <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PN_t.txt", quote="\"")

PH_gT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PHgT.txt", quote="\"")
PH_gM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PHgM.txt", quote="\"")
PH_gINT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PHgINT.txt", quote="\"")
PH_gE <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PHgE.txt", quote="\"")
PH_OBSMOD <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PH_OBSMOD.txt", quote="\""); colnames(PH_OBSMOD) <- c("YEAR", "OBS", "MOD")
PH_PAR <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PH_PAR.txt", quote="\""); rownames(PH_PAR) <- "PH"; colnames(PH_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
PH_SM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PH_sm.txt", quote="\"")
PH_Temp <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/PH_t.txt", quote="\"")

CD_gT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CDgT.txt", quote="\"")
CD_gM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CDgM.txt", quote="\"")
CD_gINT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CDgINT.txt", quote="\"")
CD_gE <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CDgE.txt", quote="\"")
CD_OBSMOD <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CD_OBSMOD.txt", quote="\""); colnames(CD_OBSMOD) <- c("YEAR", "OBS", "MOD")
CD_PAR <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CD_PAR.txt", quote="\""); rownames(CD_PAR) <- "CD"; colnames(CD_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
CD_SM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CD_sm.txt", quote="\"")
CD_Temp <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CD_t.txt", quote="\"")

CN_gT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CNgT.txt", quote="\"")
CN_gM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CNgM.txt", quote="\"")
CN_gINT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CNgINT.txt", quote="\"")
CN_gE <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CNgE.txt", quote="\"")
CN_OBSMOD <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CN_OBSMOD.txt", quote="\""); colnames(CN_OBSMOD) <- c("YEAR", "OBS", "MOD")
CN_PAR <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CN_PAR.txt", quote="\""); rownames(CN_PAR) <- "CN"; colnames(CN_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
CN_SM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CN_sm.txt", quote="\"")
CN_Temp <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CN_t.txt", quote="\"")

CH_gT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CHgT.txt", quote="\"")
CH_gM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CHgM.txt", quote="\"")
CH_gINT <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CHgINT.txt", quote="\"")
CH_gE <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CHgE.txt", quote="\"")
CH_OBSMOD <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CH_OBSMOD.txt", quote="\""); colnames(CH_OBSMOD) <- c("YEAR", "OBS", "MOD")
CH_PAR <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CH_PAR.txt", quote="\""); rownames(CH_PAR) <- "CH"; colnames(CH_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
CH_SM <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CH_sm.txt", quote="\"")
CH_Temp <- read.table("e:/JJC_VS/_DeclNondecl/Vystupy/CH_t.txt", quote="\"")

### Vystupy z VS - Juniperus communis
ALI_gT <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/ALIgT.txt", quote="\"")
ALI_gM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/ALIgM.txt", quote="\"")
ALI_gE <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/ALIgE.txt", quote="\"")
ALI_OBSMOD <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/ALI_OBSMOD.txt", quote="\""); colnames(ALI_OBSMOD) <- c("YEAR", "OBS", "MOD")
ALI_PAR <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/ALI_PAR.txt", quote="\""); rownames(ALI_PAR) <- "ALI"; colnames(ALI_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
ALI_SM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/ALI_sm.txt", quote="\"")
ALI_Temp <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/ALI_t.txt", quote="\"")

FIN_gT <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/FINgT.txt", quote="\"")
FIN_gM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/FINgM.txt", quote="\"")
FIN_gE <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/FINgE.txt", quote="\"")
FIN_OBSMOD <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/FIN_OBSMOD.txt", quote="\""); colnames(FIN_OBSMOD) <- c("YEAR", "OBS", "MOD")
FIN_PAR <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/FIN_PAR.txt", quote="\""); rownames(FIN_PAR) <- "FIN"; colnames(FIN_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
FIN_SM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/FIN_sm.txt", quote="\"")
FIN_Temp <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/FIN_t.txt", quote="\"")

KIR_gT <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KIRgT.txt", quote="\"")
KIR_gM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KIRgM.txt", quote="\"")
KIR_gE <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KIRgE.txt", quote="\"")
KIR_OBSMOD <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KIR_OBSMOD.txt", quote="\""); colnames(KIR_OBSMOD) <- c("YEAR", "OBS", "MOD")
KIR_PAR <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KIR_PAR.txt", quote="\""); rownames(KIR_PAR) <- "KIR"; colnames(KIR_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
KIR_SM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KIR_sm.txt", quote="\"")
KIR_Temp <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KIR_t.txt", quote="\"")

KOB_gT <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KOBgT.txt", quote="\"")
KOB_gM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KOBgM.txt", quote="\"")
KOB_gE <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KOBgE.txt", quote="\"")
KOB_OBSMOD <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KOB_OBSMOD.txt", quote="\""); colnames(KOB_OBSMOD) <- c("YEAR", "OBS", "MOD")
KOB_PAR <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KOB_PAR.txt", quote="\""); rownames(KOB_PAR) <- "KOB"; colnames(KOB_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
KOB_SM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KOB_sm.txt", quote="\"")
KOB_Temp <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KOB_t.txt", quote="\"")

PUR_gT <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/PURgT.txt", quote="\"")
PUR_gM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/PURgM.txt", quote="\"")
PUR_gE <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/PURgE.txt", quote="\"")
PUR_OBSMOD <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/PUR_OBSMOD.txt", quote="\""); colnames(PUR_OBSMOD) <- c("YEAR", "OBS", "MOD")
PUR_PAR <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/PUR_PAR.txt", quote="\""); rownames(PUR_PAR) <- "PUR"; colnames(PUR_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
PUR_SM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/PUR_sm.txt", quote="\"")
PUR_Temp <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/PUR_t.txt", quote="\"")

NUR_gT <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/NURgT.txt", quote="\"")
NUR_gM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/NURgM.txt", quote="\"")
NUR_gE <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/NURgE.txt", quote="\"")
NUR_OBSMOD <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/NUR_OBSMOD.txt", quote="\""); colnames(NUR_OBSMOD) <- c("YEAR", "OBS", "MOD")
NUR_PAR <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/NUR_PAR.txt", quote="\""); rownames(NUR_PAR) <- "NUR"; colnames(NUR_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
NUR_SM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/NUR_sm.txt", quote="\"")
NUR_Temp <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/NUR_t.txt", quote="\"")

SUR_gT <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/SURgT.txt", quote="\"")
SUR_gM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/SURgM.txt", quote="\"")
SUR_gE <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/SURgE.txt", quote="\"")
SUR_OBSMOD <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/SUR_OBSMOD.txt", quote="\""); colnames(SUR_OBSMOD) <- c("YEAR", "OBS", "MOD")
SUR_PAR <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/SUR_PAR.txt", quote="\""); rownames(SUR_PAR) <- "SUR"; colnames(SUR_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth")  
SUR_SM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/SUR_sm.txt", quote="\"")
SUR_Temp <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/SUR_t.txt", quote="\"")

PEN_gT <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/PENgT.txt", quote="\"")
PEN_gM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/PENgM.txt", quote="\"")
PEN_gE <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/PENgE.txt", quote="\"")
PEN_OBSMOD <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/PEN_OBSMOD.txt", quote="\""); colnames(PEN_OBSMOD) <- c("YEAR", "OBS", "MOD")
PEN_PAR <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/PEN_PAR.txt", quote="\""); rownames(PEN_PAR) <- "PEN"; colnames(PEN_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
PEN_SM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/PEN_sm.txt", quote="\"")
PEN_Temp <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/PEN_t.txt", quote="\"")

POL_gT <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/POLgT.txt", quote="\"")
POL_gM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/POLgM.txt", quote="\"")
POL_gE <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/POLgE.txt", quote="\"")
POL_OBSMOD <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/POL_OBSMOD.txt", quote="\""); colnames(POL_OBSMOD) <- c("YEAR", "OBS", "MOD")
POL_PAR <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/POL_PAR.txt", quote="\""); rownames(POL_PAR) <- "POL"; colnames(POL_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
POL_SM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/POL_sm.txt", quote="\"")
POL_Temp <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/POL_t.txt", quote="\"")

SEL_gT <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/SELgT.txt", quote="\"")
SEL_gM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/SELgM.txt", quote="\"")
SEL_gE <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/SELgE.txt", quote="\"")
SEL_OBSMOD <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/SEL_OBSMOD.txt", quote="\""); colnames(SEL_OBSMOD) <- c("YEAR", "OBS", "MOD")
SEL_PAR <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/SEL_PAR.txt", quote="\""); rownames(SEL_PAR) <- "SEL"; colnames(SEL_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
SEL_SM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/SEL_sm.txt", quote="\"")
SEL_Temp <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/SEL_t.txt", quote="\"")

VIL_gT <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/VILgT.txt", quote="\"")
VIL_gM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/VILgM.txt", quote="\"")
VIL_gE <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/VILgE.txt", quote="\"")
VIL_OBSMOD <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/VIL_OBSMOD.txt", quote="\""); colnames(VIL_OBSMOD) <- c("YEAR", "OBS", "MOD")
VIL_PAR <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/VIL_PAR.txt", quote="\""); rownames(VIL_PAR) <- "VIL"; colnames(VIL_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth")  
VIL_SM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/VIL_sm.txt", quote="\"")
VIL_Temp <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/VIL_t.txt", quote="\"")

RHE_gT <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/RHEgT.txt", quote="\"")
RHE_gM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/RHEgM.txt", quote="\"")
RHE_gE <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/RHEgE.txt", quote="\"")
RHE_OBSMOD <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/RHE_OBSMOD.txt", quote="\""); colnames(RHE_OBSMOD) <- c("YEAR", "OBS", "MOD")
RHE_PAR <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/RHE_PAR.txt", quote="\""); rownames(RHE_PAR) <- "RHE"; colnames(RHE_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth")  
RHE_SM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/RHE_sm.txt", quote="\"")
RHE_Temp <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/RHE_t.txt", quote="\"")
################################

KEV_gT <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KEVgT.txt", quote="\"")
KEV_gM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KEVgM.txt", quote="\"")
KEV_gE <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KEVgE.txt", quote="\"")
KEV_OBSMOD <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KEV_OBSMOD.txt", quote="\""); colnames(KEV_OBSMOD) <- c("YEAR", "OBS", "MOD")
KEV_PAR <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KEV_PAR.txt", quote="\""); rownames(KEV_PAR) <- "KEV"; colnames(KEV_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth") 
KEV_SM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KEV_sm.txt", quote="\"")
KEV_Temp <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/KEV_t.txt", quote="\"")

ABI_gT <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/ABIgT.txt", quote="\"")
ABI_gM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/ABIgM.txt", quote="\"")
ABI_gE <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/ABIgE.txt", quote="\"")
ABI_OBSMOD <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/ABI_OBSMOD.txt", quote="\""); colnames(ABI_OBSMOD) <- c("YEAR", "OBS", "MOD")
ABI_PAR <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/ABI_PAR.txt", quote="\""); rownames(ABI_PAR) <- "ABI"; colnames(ABI_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth")  
ABI_SM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/ABI_sm.txt", quote="\"")
ABI_Temp <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/ABI_t.txt", quote="\"")

VEN_gT <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/VENgT.txt", quote="\"")
VEN_gM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/VENgM.txt", quote="\"")
VEN_gE <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/VENgE.txt", quote="\"")
VEN_OBSMOD <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/VEN_OBSMOD.txt", quote="\""); colnames(VEN_OBSMOD) <- c("YEAR", "OBS", "MOD")
VEN_PAR <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/VEN_PAR.txt", quote="\""); rownames(VEN_PAR) <- "VEN"; colnames(VEN_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth")  
VEN_SM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/VEN_sm.txt", quote="\"")
VEN_Temp <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/VEN_t.txt", quote="\"")

CDL_gT <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/CDLgT.txt", quote="\"")
CDL_gM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/CDLgM.txt", quote="\"")
CDL_gE <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/CDLgE.txt", quote="\"")
CDL_OBSMOD <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/CDL_OBSMOD.txt", quote="\""); colnames(CDL_OBSMOD) <- c("YEAR", "OBS", "MOD")
CDL_PAR <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/CDL_PAR.txt", quote="\""); rownames(CDL_PAR) <- "CDL"; colnames(CDL_PAR) <- c("T1", "T2", "T3", "T4", "M1", "M2", "M3", "M4", "roots", "AC1", "ACmonth")  
CDL_SM <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/CDL_sm.txt", quote="\"")
CDL_Temp <- read.table("E:/JuniperNetwork/Octave/VYSTUPY/CDL_t.txt", quote="\"")
##################################

################################################
################################################
### Plotting functions - contour graphs
### {JT, October 2019}
################################################
################################################
################################################

PLOT.VSLITE <- function(input_gT, input_gM, input_gE, input_gINT = NULL, input_sm, input_temp, par, name=NULL){ 
  
  library(RColorBrewer); library(plot.matrix); library(reshape2); library(ggplot2)
  
  
  # What is a driving limiting factor of each month?
  LIMIT <- input_gT
  LIMIT[(input_gM == 0 | input_gT == 0)] <- "white" # Dormancy
  LIMIT[((input_gM > 0 & input_gM < input_gT) & input_sm < par$M2)] <- "blue" # Drought
  LIMIT[((input_gM > 0 & input_gM < input_gT) & input_sm > par$M3)] <- "grey" # Soil moisture oversaturation
  LIMIT[((input_gT > 0 & input_gT < input_gM) & input_temp < par$T2)] <- "red" # Cold
  LIMIT[((input_gT > 0 & input_gT < input_gM) & input_temp > par$T3)] <- "purple" # Hot
  LIMIT[(input_gM == 1 & input_gT == 1)] <- "green" # Optimal
  colvec<- c("blue" = "#0000FF", "grey" = "#3C3D42", "red" = "#FF0000", "purple" = "#7A1E73", "green" = "#006D2C", "white" = "#FFFFFF")
  
  matrix <- as.matrix(t(LIMIT[c(1:12)]))
  colnames(matrix) <- c(1940:(1939+nrow(input_gT)))
  rownames(matrix) <- c(1:12)
  
  if (is.null(input_gINT)){
    
    # What is integral growth rate? 
    GR <- input_gT
    GR[(input_gM < input_gT)] <- input_gM[(input_gM < input_gT)] # Table GR stores smaller from gT and gM
  
    for (i in c(1:12)){
      GR[i] <- GR[,i] * input_gE[1,i] # GR is scaled by gE
    }
  }
  
  else {GR <- input_gINT}
  
  matrix.2 <- as.matrix(t(GR))
  colnames(matrix.2) <- c(1940:(1939+nrow(input_gT)))
  rownames(matrix.2) <- c(1:12)
  
  matrix.2melt <- melt(matrix.2)
  matrix.1melt <- melt(matrix)
  
  INPUT <- cbind(matrix.2melt, matrix.1melt$value); colnames(INPUT) <- c("MONTH", "YEAR", "Gr", "Lim")
  maxGR <- max(INPUT$Gr)
  
  library(RColorBrewer); library(plot.matrix)
  
   jpeg(paste("e:/JJC_VS/Obrazky/Matice/DecNondec/", name,".jpg", sep=""), width = (600/72)*(500*7/4), height = (600/72)*500, units = "px", res = 600)
  # jpeg(paste("e:/JuniperNetwork/Obrazky/Gr/", name,".jpg", sep=""), width = (600/72)*(500*7/4), height = (600/72)*500, units = "px", res = 600)
  
  par(mar = c(5,5,1,3))
  
  mat <- ggplot(INPUT, aes(x = YEAR, y = MONTH)) + 
    geom_raster(aes(fill=Lim, alpha = Gr)) + 
    scale_fill_manual(values=colvec)+
    scale_alpha_continuous(range = c(0.15, (maxGR+0.15))) +
    scale_y_continuous(name = "Month\n", breaks = c(1:12), labels = c("Jan", "", "", "", "", "", "", "", "", "", "", "Dec"))+
    scale_x_continuous(name = "Year\n", limits = c(1940, 2020), breaks = c(1940, 1960, 1980, 2000), labels = c(1940, 1960, 1980, 2000))+
    theme_bw() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(),
                       panel.border = element_rect(fill = NA, colour = "grey20"),
                       axis.line = element_line(colour = "black"),
                       axis.text = element_text(size = 45, colour = "black"),
                       axis.title = element_text(size = 45, colour = "black"),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),
                       axis.ticks.length=unit(.3, "cm"),
                       legend.position = "none")
  
  
  print(mat)
  
  
  dev.off()
  
 # if (is.null(input_gINT)) {return(GR)}
  return(INPUT)
}



# GrINT <- PLOT.VSLITE(GrT, GrM, GrE, SM, temp, PAR, "Ecuador")

ALI_gINT <- PLOT.VSLITE(ALI_gT[,c(2:13)], ALI_gM[,c(2:13)], ALI_gE,  ALI_SM[,c(2:13)], ALI_Temp[,c(2:13)], ALI_PAR, "ALI")
PEN_gINT <- PLOT.VSLITE(PEN_gT[,c(2:13)], PEN_gM[,c(2:13)], PEN_gE,  PEN_SM[,c(2:13)], PEN_Temp[,c(2:13)], PEN_PAR, "PEN")
POL_gINT <- PLOT.VSLITE(POL_gT[,c(2:13)], POL_gM[,c(2:13)], POL_gE,  POL_SM[,c(2:13)], POL_Temp[,c(2:13)], POL_PAR, "POL")
VIL_gINT <- PLOT.VSLITE(VIL_gT[,c(2:13)], VIL_gM[,c(2:13)], VIL_gE,  VIL_SM[,c(2:13)], VIL_Temp[,c(2:13)], VIL_PAR, "VIL")
ABI_gINT <- PLOT.VSLITE(ABI_gT[,c(2:13)], ABI_gM[,c(2:13)], ABI_gE,  ABI_SM[,c(2:13)], ABI_Temp[,c(2:13)], ABI_PAR, "ABI")
FIN_gINT <- PLOT.VSLITE(FIN_gT[,c(2:13)], FIN_gM[,c(2:13)], FIN_gE,  FIN_SM[,c(2:13)], FIN_Temp[,c(2:13)], FIN_PAR, "FIN")
KEV_gINT <- PLOT.VSLITE(KEV_gT[,c(2:13)], KEV_gM[,c(2:13)], KEV_gE,  KEV_SM[,c(2:13)], KEV_Temp[,c(2:13)], KEV_PAR, "KEV")
KIR_gINT <- PLOT.VSLITE(KIR_gT[,c(2:13)], KIR_gM[,c(2:13)], KIR_gE,  KIR_SM[,c(2:13)], KIR_Temp[,c(2:13)], KIR_PAR, "KIR")
KOB_gINT <- PLOT.VSLITE(KOB_gT[,c(2:13)], KOB_gM[,c(2:13)], KOB_gE,  KOB_SM[,c(2:13)], KOB_Temp[,c(2:13)], KOB_PAR, "KOB")
PUR_gINT <- PLOT.VSLITE(PUR_gT[,c(2:13)], PUR_gM[,c(2:13)], PUR_gE,  PUR_SM[,c(2:13)], PUR_Temp[,c(2:13)], PUR_PAR, "PUR")
NUR_gINT <- PLOT.VSLITE(NUR_gT[,c(2:13)], NUR_gM[,c(2:13)], NUR_gE,  NUR_SM[,c(2:13)], NUR_Temp[,c(2:13)], NUR_PAR, "NUR")
SUR_gINT <- PLOT.VSLITE(SUR_gT[,c(2:13)], SUR_gM[,c(2:13)], SUR_gE,  SUR_SM[,c(2:13)], SUR_Temp[,c(2:13)], SUR_PAR, "SUR")
RHE_gINT <- PLOT.VSLITE(RHE_gT[,c(2:13)], RHE_gM[,c(2:13)], RHE_gE,  RHE_SM[,c(2:13)], RHE_Temp[,c(2:13)], RHE_PAR, "RHE")
VEN_gINT <- PLOT.VSLITE(VEN_gT[,c(2:13)], VEN_gM[,c(2:13)], VEN_gE,  VEN_SM[,c(2:13)], VEN_Temp[,c(2:13)], VEN_PAR, "VEN")
SEL_gINT <- PLOT.VSLITE(SEL_gT[,c(2:13)], SEL_gM[,c(2:13)], SEL_gE,  SEL_SM[,c(2:13)], SEL_Temp[,c(2:13)], SEL_PAR, "SEL")
CDL_gINT <- PLOT.VSLITE(CDL_gT[,c(2:13)], CDL_gM[,c(2:13)], CDL_gE,  CDL_SM[,c(2:13)], CDL_Temp[,c(2:13)], CDL_PAR, "CDL")

##############################
### Klimagraf
##############################

Climchart <- function(grM, grT, grE, gr, param, name){
  
  ###########################################
  ### Part 1 - traditional climate chart
  ###########################################
  
  res <- data.frame(DOY = c(1:12), Temp = NA, Moist = NA, Int=NA, Solar = NA, Intplus = NA, Intminus = NA)
  for (i in c(1:12)){
    res[i,"Temp"] <- mean(grT[,i])
    res[i,"Moist"] <- mean(grM[,i])
    res[i,"Int"] <- mean(gr[,i])
    res[i,"Intminus"] <- mean(gr[,i]) - sd(gr[,i]) 
    res[i,"Intplus"] <- mean(gr[,i]) + sd(gr[,i]) 
    res[i,"Int"] <- mean(gr[,i])
    res[i,"Solar"] <- grE[1,i]
    
  }
  res[res[,"Intminus"] < 0, "Intminus"] <- 0
  res[res[,"Intplus"] > 1, "Intplus"] <- 1
  

  library(RColorBrewer); library(plot.matrix); library(ggplot2)
  
  p <- ggplot(data=res, aes(x=c(1:12), group=1)) +
    geom_ribbon(aes(ymin=Intminus, ymax=Intplus), alpha = 0.25, fill = "black")+
    geom_line(aes(y=Temp), col = "red", linetype = "solid", size = 0.75)+
    geom_line(aes(y=Moist), col = "blue", linetype = "solid", size = 0.75)+
    geom_line(aes(y=Solar), col = "orange", linetype = "solid", size = 0.75)+
    geom_line(aes(y=Int), col = "black", linetype = "solid", size = 0.85)+
    ylim(0,1)+
    ylab("Partial and integral growth rates\n")+
    scale_x_continuous(breaks = c(1:12), labels = c())+
    ggtitle("")+
    scale_color_grey()+ 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(fill = NA, colour = "grey20"),
            axis.title.x=element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text = element_text(size = 24, colour = "black"),
            axis.title = element_text(size = 24, colour = "black"),
            plot.margin = margin(0, 0, 0, 0, "cm"),
            axis.ticks.length=unit(.3, "cm"))
  
  
  ###########################################
  ### Part 2 - growth trends
  ###########################################
  # 1] Output file
  Output <- data.frame(T1=NA, T2=NA, T3=NA, T4=NA, T5=NA, T6=NA, T7=NA, T8=NA, T9=NA, T10=NA, T11=NA, T12=NA,
                       M1=NA, M2=NA, M3=NA, M4=NA, M5=NA, M6=NA, M7=NA, M8=NA, M9=NA, M10=NA, M11=NA, M12=NA,
                       GR1=NA, GR2=NA, GR3=NA, GR4=NA, GR5=NA, GR6=NA, GR7=NA, GR8=NA, GR9=NA, GR10=NA, GR11=NA, GR12=NA)
  
  # 2a] Calculation of trends
  for (i in c(1:12)){
    DATA <- data.frame(ROK=c(1940:(1939+nrow(grT))), T=grT[(i)], M=grM[(i)], GR=gr[(i)]); colnames(DATA) <- c("ROK", "T", "M", "GR")
    
    modelT <- summary(lm(DATA$T ~ DATA$ROK))
    Output[3,i] <- modelT$coefficients[2,4] # p-value of slope
    Output[2,i] <- modelT$coefficients[2,1] # Estimate of slope
    Output[1,i] <- colMeans(grT[(i)])
    
    modelM <- summary(lm(DATA$M ~ DATA$ROK))
    Output[3,i+12] <- modelM$coefficients[2,4] # p-value of slope
    Output[2,i+12] <- modelM$coefficients[2,1] # Estimate of slope
    Output[1,i+12] <- colMeans(grM[(i)])
    
    modelGR <- summary(lm(DATA$GR ~ DATA$ROK))
    Output[3,i+24] <- modelGR$coefficients[2,4] # p-value of slope
    Output[2,i+24] <- modelGR$coefficients[2,1] # Estimate of slope
    Output[1,i+24] <- colMeans(gr[(i)])
    
    
    rm(modelM, modelT, DATA)
    
  }
  # 3a] File with results
  Output[3,is.na(Output[3,])] <- 0
  
  rownames(Output) <- c(paste(name,"MEAN", sep="_"), paste(name, "I1", sep="_"), paste(name, "p1", sep="_"))
  Output.2 <- data.frame(t(Output))
  Output.2[Output.2[3]<0.05,3] <- 1; Output.2[Output.2[3]<1,3] <- 0.5
  Output.2[c(1:12),"COL"] <- "red"; Output.2[c(13:24),"COL"] <- "blue"; Output.2[c(25:36),"COL"] <- "black"
  Output.2$MONTH <- rep(c(1:12), 3)
  colnames(Output.2) <- c("MEAN", "SLOPE", "P", "COL", "MONTH")
  
  # 2b+3b] Calculating and storing percentual trend in GrINT
  chron <- data.frame(YEAR = c(1:nrow(gr)), GR_year = NA)
  AC <- as.numeric(param[1, 10])
  I0 <- as.numeric(param[1, 11])
  
  for (i in c(2:nrow(chron))){
    chron[i, "GR_year"] <- sum(gr[i,c(1:12)]) + (AC * sum(gr[(i-1), c(I0:12)]))
  }
  
  chron$GR_year <- 100*chron$GR_year/mean(chron[c(2:nrow(chron)), "GR_year"])
  
  slope <- round(summary(lm(chron[,"GR_year"] ~ chron[,"YEAR"]))$coefficients[2,1], 2)
  sig <- summary(lm(chron[,"GR_year"] ~ chron[,"YEAR"]))$coefficients[2,4]
  if (sig < 0.05){mark <- "*"} else (mark <- "n.s.")
  
  # 4] Plotting
  library(ggplot2); library(cowplot)
  
  q <- ggplot(data=Output.2, aes(fill = COL, color = COL, alpha = P)) +
    geom_line(aes(x=MONTH, y=rep(0, 36)), linetype = "solid", size = 0.5)+
    geom_bar(aes(y=100*SLOPE, x=MONTH), stat = "identity", position=position_dodge())+
    xlab("\nMonth")+
    scale_x_continuous(breaks = c(1:12), labels = c("Jan", " ", "Mar", " ", "May", " ", "Jul", " ", "Sep", " ", "Nov", " "))+
    scale_y_continuous(name = "Trend per 100 year\n", limits = c(-0.5, 0.5), breaks = c(-0.5, 0, 0.5))+
    scale_color_manual(values=c("black", "blue", "red"))+
    scale_fill_manual(values=c("black", "blue", "red"))+
    annotate("text", x = 11, y=0.4, size = 9, label = paste(slope, " % ", mark, sep=""))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, colour = "grey20"),
          legend.position = "none",
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size = 24, colour = "black"),
          axis.title = element_text(size = 24, colour = "black"),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          axis.ticks.length=unit(.3, "cm"))
  
  
  # jpeg(paste("e:/JuniperNetwork/Obrazky/Klimagraf/", name,".jpg", sep=""), width = (600/72)*(500), height = (800/72)*500, units = "px", res = 600)
  jpeg(paste("e:/JJC_VS/Obrazky/Klimagraf/DecNondec/", name,".jpg", sep=""), width = (600/72)*(500), height = (800/72)*500, units = "px", res = 600)
  
  print(
    plot_grid(p, q, nrow = 2, ncol = 1, rel_heights = c(2,1), align = "hv"))
  dev.off()
  
}

# Climchart(GrM[c(2:26),], GrT[c(2:26),], GrE,GrINT[c(2:26),], "Ecuador")

# Climchart(MD_gM[,c(2:13)], MD_gT[,c(2:13)], MD_gE, MD_gINT, MD_PAR, "MD")
# Climchart(MN_gM[,c(2:13)], MN_gT[,c(2:13)], MN_gE, MN_gINT, MN_PAR, "MN")
Climchart(OD_gM[,c(2:13)], OD_gT[,c(2:13)], OD_gE, OD_gINT[,c(2:13)], OD_PAR, "PIPID")
Climchart(ON_gM[,c(2:13)], ON_gT[,c(2:13)], ON_gE, ON_gINT[,c(2:13)], ON_PAR, "PIPIN")
Climchart(OH_gM[,c(2:13)], OH_gT[,c(2:13)], OH_gE, OH_gINT[,c(2:13)], OH_PAR, "PIPIH")
# Climchart(ED_gM[,c(2:13)], ED_gT[,c(2:13)], ED_gE, ED_gINT, ED_PAR, "ED")
# Climchart(EN_gM[,c(2:13)], EN_gT[,c(2:13)], EN_gE, EN_gINT, EN_PAR, "EN")
Climchart(CD_gM[,c(2:13)], CD_gT[,c(2:13)], CD_gE, CD_gINT[,c(2:13)], CD_PAR, "PISYD")
Climchart(CN_gM[,c(2:13)], CN_gT[,c(2:13)], CN_gE, CN_gINT[,c(2:13)], CN_PAR, "PISYN")
Climchart(CH_gM[,c(2:13)], CH_gT[,c(2:13)], CH_gE, CH_gINT[,c(2:13)], CH_PAR, "PISYH")
Climchart(PD_gM[,c(2:13)], PD_gT[,c(2:13)], PD_gE, PD_gINT[,c(2:13)], PD_PAR, "PIHAD")
Climchart(PN_gM[,c(2:13)], PN_gT[,c(2:13)], PN_gE, PN_gINT[,c(2:13)], PN_PAR, "PIHAN")
Climchart(PH_gM[,c(2:13)], PH_gT[,c(2:13)], PH_gE, PH_gINT[,c(2:13)], PH_PAR, "PIHAH")

Climchart(ALI_gM[,c(2:13)], ALI_gT[,c(2:13)], ALI_gE, ALI_gINT, ALI_PAR, "1ALI")
Climchart(PEN_gM[,c(2:13)], PEN_gT[,c(2:13)], PEN_gE, PEN_gINT, PEN_PAR, "1PEN")
Climchart(POL_gM[,c(2:13)], POL_gT[,c(2:13)], POL_gE, POL_gINT, POL_PAR, "1POL")
Climchart(VIL_gM[,c(2:13)], VIL_gT[,c(2:13)], VIL_gE, VIL_gINT, VIL_PAR, "1VIL")
Climchart(ABI_gM[,c(2:13)], ABI_gT[,c(2:13)], ABI_gE, ABI_gINT, ABI_PAR, "2ABI")
Climchart(FIN_gM[,c(2:13)], FIN_gT[,c(2:13)], FIN_gE, FIN_gINT, FIN_PAR, "2FIN")
Climchart(KEV_gM[,c(2:13)], KEV_gT[,c(2:13)], KEV_gE, KEV_gINT, KEV_PAR, "2KEV")
Climchart(KIR_gM[,c(2:13)], KIR_gT[,c(2:13)], KIR_gE, KIR_gINT, KIR_PAR, "2KIR")
Climchart(KOB_gM[,c(2:13)], KOB_gT[,c(2:13)], KOB_gE, KOB_gINT, KOB_PAR, "2KOB")
Climchart(PUR_gM[,c(2:13)], PUR_gT[,c(2:13)], PUR_gE, PUR_gINT, PUR_PAR, "3PUR")
Climchart(NUR_gM[,c(2:13)], NUR_gT[,c(2:13)], NUR_gE, NUR_gINT, NUR_PAR, "3NUR")
Climchart(SUR_gM[,c(2:13)], SUR_gT[,c(2:13)], SUR_gE, SUR_gINT, SUR_PAR, "3SUR")
Climchart(RHE_gM[,c(2:13)], RHE_gT[,c(2:13)], RHE_gE, RHE_gINT, RHE_PAR, "4RHE")
Climchart(VEN_gM[,c(2:13)], VEN_gT[,c(2:13)], VEN_gE, VEN_gINT, VEN_PAR, "4VEN")
Climchart(SEL_gM[,c(2:13)], SEL_gT[,c(2:13)], SEL_gE, SEL_gINT, SEL_PAR, "4SEL")
Climchart(CDL_gM[,c(2:13)], CDL_gT[,c(2:13)], CDL_gE, CDL_gINT, CDL_PAR, "4CDL")

##############################
### ObsMod
##############################

ObsModplot <- function(chronos, name){
  
  n <- nrow(chronos)
  
  cor <- round(cor(na.omit(chronos[,"OBS"]), na.omit(chronos[,"MOD"])), 2)
  cor.old <- round(cor(na.omit(chronos[c(1:40),"OBS"]), na.omit(chronos[c(1:40),"MOD"])), 2)
  cor.young <- round(cor(na.omit(chronos[c(41:n),"OBS"]), na.omit(chronos[c(41:n),"MOD"])), 2)
  
  
  # Splines
  ObsSpline <- smooth.spline(x = chronos$YEAR, y=chronos$OBS, df = 10) 
  ModSpline <- smooth.spline(x = chronos$YEAR, y=chronos$MOD, df = 10)
  
  chronos$OBS_SPL <- predict(ObsSpline)$y 
  chronos$MOD_SPL <- predict(ModSpline)$y 
  
  cor.spl <- round(cor(na.omit(chronos[,"OBS_SPL"]), na.omit(chronos[,"MOD_SPL"])), 2)

  ###
  
  library(RColorBrewer); library(plot.matrix); library(ggplot2)
  
  jpeg(paste("e:/JJC_VS/Obrazky/ObsMod/DecNondec/", name,".jpg", sep=""), width = (600/72)*(500*7/4), height = (600/72)*500, units = "px", res = 600)
  # jpeg(paste("e:/JuniperNetwork/Obrazky/ObsMod/", name,".jpg", sep=""), width = (600/72)*(500*7/4), height = (600/72)*500*1.25, units = "px", res = 600)
  
  
  p <- ggplot(data=chronos) +
    geom_line(aes(y=rep(0, n), x=YEAR), col = "grey20", linetype = "dotted", size = 1.1)+
    geom_line(aes(y=OBS, x=YEAR), col = "red", linetype = "solid", size = 1.0)+
    geom_line(aes(y=MOD, x=YEAR), col = "blue", linetype = "solid", size = 1.0)+
    geom_vline(xintercept = 1980, col = "orange", linetype = "solid", size = 0.65, alpha = 0.5)+
    # geom_line(aes(y=OBS_SPL, x=YEAR), col = "red", linetype = "dashed", size = 0.75, alpha = 0.75)+
    # geom_line(aes(y=MOD_SPL, x=YEAR), col = "blue", linetype = "dashed", size = 0.75, alpha = 0.75)+
    ylim(-3.5,3.5)+
    xlab("\nYear")+
    ylab("Index")+
    # ggtitle(substr(name, 2,5))+
    xlim(1940, 2020)+
    annotate("text", x = 2000, y=2.75, label = paste(cor," (", cor.old, "/", cor.young, ")", sep=""), size = 14)

  
  print(
    p + scale_color_grey()+ 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(fill = NA, colour = "grey20"),
            axis.line = element_line(colour = "black"),
            axis.text = element_text(size = 38, colour = "black"),
            axis.title = element_text(size = 38, colour = "black"),
            axis.ticks.length=unit(.3, "cm"),
            plot.title = element_text(color = "black", hjust = 0.5, vjust = -6, face = "bold", size = 50)))
  
  dev.off()
  
  
  return(list(chronos = chronos, cor = data.frame(FULL = cor, YOUNG = cor.young, OLD = cor.old)))
  
  
}


ObsModplot(ObsMod, "Ecuador")

ObsModplot(ALI_OBSMOD, "1ALI")
ObsModplot(PEN_OBSMOD, "1PEN")
ObsModplot(POL_OBSMOD, "1POL")
ObsModplot(VIL_OBSMOD, "1VIL")
ObsModplot(ABI_OBSMOD, "2ABI")
ObsModplot(FIN_OBSMOD, "2FIN")
ObsModplot(KEV_OBSMOD, "2KEV")
ObsModplot(KIR_OBSMOD, "2KIR")
ObsModplot(KOB_OBSMOD, "2KOB")
ObsModplot(PUR_OBSMOD, "3PUR")
ObsModplot(NUR_OBSMOD, "3NUR")
ObsModplot(SUR_OBSMOD, "3SUR")
ObsModplot(RHE_OBSMOD, "4RHE")
ObsModplot(SEL_OBSMOD, "4SEL")
ObsModplot(VEN_OBSMOD, "4VEN")
ObsModplot(CDL_OBSMOD, "4CDL")

##############################
### Phenology estimates
##############################

GS <- function(input){
  

  DATA <- input
  DATA[DATA>0 & DATA<=1] <- 1
  DATA.res <- data.frame(YEAR = c(1940:(1939+nrow(DATA))), GS = rowSums(DATA))
  return(DATA.res)
  
}

GrowSeas <- Reduce(function(x, y) merge(x, y, by="YEAR", all=T), list(GS(ALI_gINT), GS(PEN_gINT), GS(POL_gINT), GS(VIL_gINT),
                                                                      GS(ABI_gINT), GS(FIN_gINT), GS(KEV_gINT), GS(KIR_gINT), GS(KOB_gINT),
                                                                      GS(PUR_gINT), GS(NUR_gINT), GS(SUR_gINT),
                                                                      GS(SEL_gINT), GS(CDL_gINT), GS(RHE_gINT), GS(VEN_gINT)))
colnames(GrowSeas) <- c("YEAR", "ALI", "PEN", "POL", "VIL", "ABI", "FIN", "KEV", "KIR", "KOB", "PUR", "NUR", "SUR", "SEL", "CDL", "RHE", "VEN")
write.table(GrowSeas, "e:/JuniperNetwork/Octave/VYSTUPY/GrowSeas/GrowSeas.txt", row.names = F)

GrowSeas[c(2:17)] <- 30* GrowSeas[c(2:17)]

summary(lm(GrowSeas$ALI ~ GrowSeas$YEAR))
summary(lm(GrowSeas$PEN ~ GrowSeas$YEAR))
summary(lm(GrowSeas$POL ~ GrowSeas$YEAR))
summary(lm(GrowSeas$VIL ~ GrowSeas$YEAR))

summary(lm(GrowSeas$ABI ~ GrowSeas$YEAR))
summary(lm(GrowSeas$FIN ~ GrowSeas$YEAR))
summary(lm(GrowSeas$KEV ~ GrowSeas$YEAR))
summary(lm(GrowSeas$KIR ~ GrowSeas$YEAR))
summary(lm(GrowSeas$KOB ~ GrowSeas$YEAR))

summary(lm(GrowSeas$PUR ~ GrowSeas$YEAR))
summary(lm(GrowSeas$NUR ~ GrowSeas$YEAR))
summary(lm(GrowSeas$SUR ~ GrowSeas$YEAR))

summary(lm(GrowSeas$SEL ~ GrowSeas$YEAR))
summary(lm(GrowSeas$CDL ~ GrowSeas$YEAR))
summary(lm(GrowSeas$RHE ~ GrowSeas$YEAR))
summary(lm(GrowSeas$VEN ~ GrowSeas$YEAR))

##############################
### Comparison of growth deviations in extreme years
##############################

DROUGHT <- function(D_gINT, N_gINT, H_gINT, year = 2000, name = "000"){

  rownames(D_gINT) <- c(1940:(1939+nrow(D_gINT)))
  rownames(N_gINT) <- c(1940:(1939+nrow(N_gINT)))
  rownames(H_gINT) <- c(1940:(1939+nrow(H_gINT)))

  DATA <- data.frame(DmeanINT = colMeans(D_gINT[c(1:40),]), NmeanINT = colMeans(N_gINT[c(1:40),]), HmeanINT = colMeans(H_gINT[c(1:40),]),
                     DdroughtINT = (t(D_gINT[rownames(D_gINT) %in% year, ])), NdroughtINT = (t(N_gINT[rownames(N_gINT) %in% year, ])), HdroughtINT = (t(H_gINT[rownames(H_gINT) %in% year, ])),
                     SPE = name, YEAR = year, MONTH = c(1:12))

  colnames(DATA) <- c("DmeanINT", "NmeanINT", "HmeanINT", 
                      "DexINT", "NexINT", "HexINT")

  library(ggplot2)

  plot <- ggplot(data=DATA, aes(x = c(1:12))) +
            geom_line(aes(y=rep(0, 12)), col = "grey20", linetype = "solid", size = 0.6)+
            geom_line(aes(y=(DexINT-DmeanINT)/sum(DmeanINT)), col = "red", linetype = "solid", size = 1.1)+
            geom_line(aes(y=(NexINT-NmeanINT)/sum(NmeanINT)), col = "orange", linetype = "solid", size = 1.1)+
            geom_line(aes(y=(HexINT-HmeanINT)/sum(HmeanINT)), col = "green", linetype = "solid", size = 1.1)+
            scale_x_continuous(breaks = c(1:12), labels = c("Jan", " ", "Mar", " ", "May", " ", "Jul", " ", "Sep", " ", "Nov", " "))+
            ylim(-0.14,0.07)+
            xlab("\nMonth")+
            ylab("Growth rate relative decline (%)\n")+ 
                scale_color_grey()+ 
                theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(fill = NA, colour = "grey20"),
                axis.line = element_line(colour = "black"),
                axis.text = element_text(size = 28, colour = "black"),
                axis.title = element_text(size = 28, colour = "black"),
                axis.ticks.length=unit(.3, "cm"),
                plot.title = element_text(color = "black", hjust = 0.5, vjust = -6, face = "bold", size = 50))

  jpeg(paste("e:/JJC_VS/Obrazky/Drought/", name,"_", year,".jpg", sep=""), width = (600/72)*(500*5/4), height = (600/72)*500, units = "px", res = 600)
  
  print(plot)
  
  dev.off()
  
  return(DATA)
}


##########################################################################################

### Pokus o clusterovani
MEANgT <- cbind(ALI=colMeans(ALI_gT[c(2:13)]),PEN=colMeans(PEN_gT[c(2:13)]),POL=colMeans(POL_gT[c(2:13)]),VIL=colMeans(VIL_gT[c(2:13)]),
                ABI=colMeans(ABI_gT[c(2:13)]),FIN=colMeans(FIN_gT[c(2:13)]),KEV=colMeans(KEV_gT[c(2:13)]),KIR=colMeans(KIR_gT[c(2:13)]),KOB=colMeans(KOB_gT[c(2:13)]),
                PUR=colMeans(PUR_gT[c(2:13)]),NUR=colMeans(NUR_gT[c(2:13)]),SUR=colMeans(SUR_gT[c(2:13)]),
                SEL=colMeans(SEL_gT[c(2:13)]),VEN=colMeans(VEN_gT[c(2:13)]), RHE=colMeans(RHE_gT[c(2:13)]), CDL=colMeans(CDL_gT[c(2:13)]))
MEANgM <- cbind(ALI=colMeans(ALI_gM[c(2:13)]),PEN=colMeans(PEN_gM[c(2:13)]),POL=colMeans(POL_gM[c(2:13)]),VIL=colMeans(VIL_gM[c(2:13)]),
                ABI=colMeans(ABI_gM[c(2:13)]),FIN=colMeans(FIN_gM[c(2:13)]),KEV=colMeans(KEV_gM[c(2:13)]),KIR=colMeans(KIR_gM[c(2:13)]),KOB=colMeans(KOB_gM[c(2:13)]),
                PUR=colMeans(PUR_gM[c(2:13)]),NUR=colMeans(NUR_gM[c(2:13)]),SUR=colMeans(SUR_gM[c(2:13)]),
                SEL=colMeans(SEL_gM[c(2:13)]),VEN=colMeans(VEN_gM[c(2:13)]), RHE=colMeans(RHE_gM[c(2:13)]), CDL=colMeans(CDL_gM[c(2:13)]))
rownames(MEANgT) <- c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12")
rownames(MEANgM) <- c("M1","M2","M3","M4","M5","M6","M7","M8","M9","M10","M11","M12")

MEAN <- rbind(MEANgT, MEANgM)

library(ape)
hc <- hclust(dist(t(MEAN[,c(1:16)]))^2, method = "ward.D2")
plot.phylo(as.phylo(hc), label.offset = 0.1, type = "phylogram", edge.lty = 1, direction = "downwards",
           tip.color = c(rep("red", 4), rep("blue", 5), rep("orange", 3), rep("green", 4)))
