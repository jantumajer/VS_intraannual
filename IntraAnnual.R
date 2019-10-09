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

DailyGr <- read.table("e:/Xylo_Model/MODEL/FINAL result/krk rate 1961 - 2017.dat", header = T) # Output of VS on daily scale (rate)
AnnualGr <- read.table("e:/Xylo_Model/MODEL/FINAL result/krk 1961 - 2017.dat", header = T) # Output of VS on annual scale

################################################
### Variable indicating presence/absence of cambial activity (from BG-EG span)
### Variable indicating limitation (temperature, moisture or optimal)
################################################

DailyGr$CambialActivity <- NA
DailyGr$Limit <- NA

for (i in c(1:nrow(DailyGr))){
  year <- DailyGr[i, "year"] # Current year
  
  BG <- AnnualGr[AnnualGr$year == year, "BG1"] # Estimated dates of beggining and ending of growing season
  EG <- AnnualGr[AnnualGr$year == year, "EG1"]
  
  if (DailyGr[i, "t"] >= BG & DailyGr[i, "t"] <= EG) {DailyGr[i, "CambialActivity"] <- TRUE}
    else {DailyGr[i, "CambialActivity"] <- FALSE}
  
  # 0=no growth (Gr=Gt=0), 1=temperature limitation(Gt<Gw), 2=moisture limitation (Gw<Gt), 3=optimal growth (Gt=Gw=1) 
  if (DailyGr[i, "CambialActivity"] == FALSE) {DailyGr[i, "Limit"] <- 0} 
  else
    {if (DailyGr[i, "Grw"] == 1 & DailyGr[i, "GrT"] == 1) {DailyGr[i, "Limit"] <- 3}
     if (DailyGr[i, "Grw"] > DailyGr[i, "GrT"]) {DailyGr[i, "Limit"] <- 1}
     if (DailyGr[i, "Grw"] < DailyGr[i, "GrT"]) {DailyGr[i, "Limit"] <- 2}
      
    }
}

################################################
### Total sum of Gint for individual years (~ total tree-ring width proxy)
################################################

subset <- DailyGr[DailyGr$CambialActivity == T,] # Because some Gr is modelled also for dormancy (e.g. when T>T1, but cummulative temperature<Tbeg), I need to cut off days of dormancy
GRyear <- aggregate(subset[,c("Gr")], by = list(year=subset$year), FUN = sum)
rm(subset)

################################################
### Cumulative sum of Gint during each year and its conversion to percents
################################################

DailyGr$GrCumul <- 0
DailyGr$GrCumul_perc <- 0

for (i in c(1:nrow(DailyGr))){
  
  year <- DailyGr[i, "year"] # Current year
  
  if(DailyGr[i, "t"] > 1 & DailyGr[i, "CambialActivity"] == TRUE) #GrCumul is simply sum of current year Gr and previous day GrCumul
    {DailyGr[i, "GrCumul"] <- DailyGr[(i-1), "GrCumul"] + DailyGr[i, "Gr"]
     DailyGr[i, "GrCumul_perc"] <- DailyGr[i, "GrCumul"] / GRyear[GRyear$year == year, "x"]}
}

rm(GRyear)

################################################
################################################
### Table of tree-ring formation progress
################################################
################################################

Formation <- data.frame(year=AnnualGr$year, BG=NA, P05=NA, P10=NA, P15=NA, P20=NA, P25=NA, P30=NA, P35=NA, P40=NA, P45=NA, P50=NA,
                                                   P55=NA, P60=NA, P65=NA, P70=NA, P75=NA, P80=NA, P85=NA, P90=NA, P95=NA, EG=NA)

for (i in c(2:nrow(DailyGr))){
  
  year <- DailyGr[i, "year"] # Current year
  
  # For each year finds the first day, when standardized cumulative growth (GrCumul_perc) exceeds specific threshold
  if (DailyGr[i, "GrCumul_perc"] > 0.05 & DailyGr[(i-1), "GrCumul_perc"] < 0.05) {Formation[Formation$year == year, "P05"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.10 & DailyGr[(i-1), "GrCumul_perc"] < 0.10) {Formation[Formation$year == year, "P10"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.15 & DailyGr[(i-1), "GrCumul_perc"] < 0.15) {Formation[Formation$year == year, "P15"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.20 & DailyGr[(i-1), "GrCumul_perc"] < 0.20) {Formation[Formation$year == year, "P20"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.25 & DailyGr[(i-1), "GrCumul_perc"] < 0.25) {Formation[Formation$year == year, "P25"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.30 & DailyGr[(i-1), "GrCumul_perc"] < 0.30) {Formation[Formation$year == year, "P30"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.35 & DailyGr[(i-1), "GrCumul_perc"] < 0.35) {Formation[Formation$year == year, "P35"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.40 & DailyGr[(i-1), "GrCumul_perc"] < 0.40) {Formation[Formation$year == year, "P40"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.45 & DailyGr[(i-1), "GrCumul_perc"] < 0.45) {Formation[Formation$year == year, "P45"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.50 & DailyGr[(i-1), "GrCumul_perc"] < 0.50) {Formation[Formation$year == year, "P50"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.55 & DailyGr[(i-1), "GrCumul_perc"] < 0.55) {Formation[Formation$year == year, "P55"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.60 & DailyGr[(i-1), "GrCumul_perc"] < 0.60) {Formation[Formation$year == year, "P60"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.65 & DailyGr[(i-1), "GrCumul_perc"] < 0.65) {Formation[Formation$year == year, "P65"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.70 & DailyGr[(i-1), "GrCumul_perc"] < 0.70) {Formation[Formation$year == year, "P70"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.75 & DailyGr[(i-1), "GrCumul_perc"] < 0.75) {Formation[Formation$year == year, "P75"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.80 & DailyGr[(i-1), "GrCumul_perc"] < 0.80) {Formation[Formation$year == year, "P80"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.85 & DailyGr[(i-1), "GrCumul_perc"] < 0.85) {Formation[Formation$year == year, "P85"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.90 & DailyGr[(i-1), "GrCumul_perc"] < 0.90) {Formation[Formation$year == year, "P90"] <- DailyGr[i, "t"]} 
  if (DailyGr[i, "GrCumul_perc"] > 0.95 & DailyGr[(i-1), "GrCumul_perc"] < 0.95) {Formation[Formation$year == year, "P95"] <- DailyGr[i, "t"]} 

}

Formation$BG <- AnnualGr$BG1
Formation$EG <- AnnualGr$EG1

################################################
### Reading xylogenesis data (estimation of tree-ring formation progress)
################################################

XyloDynamics <- read.table("e:/Xylo_Model/XYLODATA/VYSLEDKY/DynamicsIntoR.csv", header = T, sep = ";") 

subFormation <- Formation[Formation$year > 2009, c(1:21)]

residual <- XyloDynamics - subFormation






################################################
################################################
################################################
### Plotting functions - contour graphs
### {JT, October 2019}
################################################
################################################
################################################

# Reformating data into matrixes
matrix <- matrix(0, ncol=57, nrow = 366)
  colnames(matrix) <- AnnualGr$year
  rownames(matrix) <- c(1:366)
matrix.2 <- matrix

for (i in c(1:nrow(DailyGr))) {
    
    year <- DailyGr[i, "year"]
    day <- DailyGr[i, "t"]
    
    if (DailyGr[i, "CambialActivity"] == TRUE) {
                      matrix[day, (year-1960)] <- DailyGr[i, "Gr"]
                      matrix.2[day, (year-1960)] <- DailyGr[i, "Limit"]
                                               }
    
}  

################################################
### Plotting the image of Gint
################################################

image(x=as.numeric(colnames(matrix)), y=c(1:nrow(matrix)), z=t(matrix), zlim=c(0.001, 1.001), ylim=c(100, 320), col = rev(heat.colors(1000)), axes = F, xlab = NA, ylab="DOY")

axis(1, at = seq(1960, 2020, by = 5))
axis(2, at = seq(0, 365, by = 30))
axis(4, at = seq(0, 365, by = 30))

# Estimated dates of the start and the end of growing season (BG, EG)
lines(AnnualGr$BG1 ~ AnnualGr$year, lw=2, lty=3, col="black")
lines(AnnualGr$EG1 ~ AnnualGr$year, lw=2, lty=3, col="black")

# Lineat trends in BG and EG
lines(predict(lm(AnnualGr$BG1 ~ AnnualGr$year)) ~ AnnualGr$year, lw=2, lty=3, col="blue")
lines(predict(lm(AnnualGr$EG1 ~ AnnualGr$year)) ~ AnnualGr$year, lw=2, lty=3, col="blue")


################################################
### Plotting the image of dominant limiting factors
################################################

image(x=as.numeric(colnames(matrix.2)), y=c(1:nrow(matrix.2)), z=t(matrix.2), ylim=c(100, 320), col = c("white", "red", "blue", "grey"), axes = F, xlab = NA, ylab="DOY")

axis(1, at = seq(1960, 2020, by = 5))
axis(2, at = seq(0, 365, by = 30))
axis(4, at = seq(0, 365, by = 30))