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


### Vystupy z kambialniho bloku ze skirptu K. Anchukaitise

# Growth rates
PHgr <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/Gr_PH.txt"); colnames(PHgr) <- c(1960:2012)
PUgr <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/Gr_PU.txt"); colnames(PUgr) <- c(1960:2014)
JTgr <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/Gr_JT.txt"); colnames(JTgr) <- c(1960:2012)
JCgr <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/Gr_JC.txt"); colnames(JCgr) <- c(1960:2014)
JTgrT <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/GrT_JT.txt"); colnames(JTgrT) <- c(1960:2012)
JTgrM <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/GrM_JT.txt"); colnames(JTgrM) <- c(1960:2012)
JCgrT <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/GrT_JC.txt"); colnames(JCgrT) <- c(1960:2014)
JCgrM <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/GrM_JC.txt"); colnames(JCgrM) <- c(1960:2014)
PHgrT <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/GrT_PH.txt"); colnames(PHgrT) <- c(1960:2012)
PHgrM <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/GrM_PH.txt"); colnames(PHgrM) <- c(1960:2012)
PUgrT <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/GrT_PU.txt"); colnames(PUgrT) <- c(1960:2014)
PUgrM <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/GrM_PU.txt"); colnames(PUgrM) <- c(1960:2014)

# Cells in cambial and differentiation zone
JTxyl <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/Xylem_JT.txt"); JTxyl <- JTxyl[c(1:365),]; colnames(JTxyl) <- c(1960:2012)
JCxyl <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/Xylem_JC.txt"); JCxyl <- JCxyl[c(1:365),]; colnames(JCxyl) <- c(1960:2014)
PHxyl <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/Xylem_PH.txt"); PHxyl <- PHxyl[c(1:365),]; colnames(PHxyl) <- c(1960:2012)
PUxyl <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/Xylem_PU.txt"); PUxyl <- PUxyl[c(1:365),]; colnames(PUxyl) <- c(1960:2014)
JTcamb <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/Cambium_JT.txt"); JTcamb <- JTcamb[c(1:365),]; colnames(JTcamb) <- c(1960:2012)
JCcamb <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/Cambium_JC.txt"); JCcamb <- JCcamb[c(1:365),]; colnames(JCcamb) <- c(1960:2014)
PHcamb <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/Cambium_PH.txt"); PHcamb <- PHcamb[c(1:365),]; colnames(PHcamb) <- c(1960:2012)
PUcamb <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/Cambium_PU.txt"); PUcamb <- PUcamb[c(1:365),]; colnames(PUcamb) <- c(1960:2014)

# Observed and simulated chronologies
JTom <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/ObsMod_JT.txt"); colnames(JTom) <- c("obs", "env", "cam"); rownames(JTom) <- c(1960:2012)
JCom <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/ObsMod_JC.txt"); colnames(JCom) <- c("obs", "env", "cam"); rownames(JCom) <- c(1960:2014)
PHom <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/ObsMod_PH.txt"); colnames(PHom) <- c("obs", "env", "cam"); rownames(PHom) <- c(1960:2012)
PUom <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/ObsMod_PU.txt"); colnames(PUom) <- c("obs", "env", "cam"); rownames(PUom) <- c(1960:2014)

# Cambial phenology
PHph <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/Phenol_PH.txt"); colnames(PHph) <- c("Cstart", "Estart", "Eend", "Cend"); rownames(PHph) <- c(1960:2012)
JTph <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/Phenol_JT.txt"); colnames(JTph) <- c("Cstart", "Estart", "Eend", "Cend"); rownames(JTph) <- c(1960:2012)
PUph <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/Phenol_PU.txt"); colnames(PUph) <- c("Cstart", "Estart", "Eend", "Cend"); rownames(PUph) <- c(1960:2014)
JCph <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/Phenol_JC.txt"); colnames(JCph) <- c("Cstart", "Estart", "Eend", "Cend"); rownames(JCph) <- c(1960:2014)

# Temperature and soil moisture
PHsm <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/moist_PH.txt"); colnames(PHsm) <- c(1960:2012)
JTsm <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/moist_JT.txt"); colnames(JTsm) <- c(1960:2012)
PUsm <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/moist_PU.txt"); colnames(PUsm) <- c(1960:2014)
JCsm <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/moist_JC.txt"); colnames(JCsm) <- c(1960:2014)
PHtemp <- read.table("e:/JJC_VS/MODEL/Klima/EOBS_010_OSCI/Penaflor_T.txt")[c(1:365),c(1:53)]; colnames(PHtemp) <- c(1960:2012)
JTtemp <- read.table("e:/JJC_VS/MODEL/Klima/EOBS_010_OSCI/Penaflor_T.txt")[c(1:365),c(1:53)]; colnames(JTtemp) <- c(1960:2012)
PUtemp <- read.table("e:/JJC_VS/MODEL/Klima/EOBS_010_OSCI/Pennaroya_T.txt")[c(1:365),]; colnames(PUtemp) <- c(1960:2014)
JCtemp <- read.table("e:/JJC_VS/MODEL/Klima/EOBS_010_OSCI/Villaroya_T.txt")[c(1:365),]; colnames(JCtemp) <- c(1960:2014)

# Parameters
JTpar <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/JTpar.txt")
JCpar <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/JCpar.txt")
PHpar <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/PHpar.txt")
PUpar <- read.table("e:/JJC_VS/vsm_cambial_JJC/SAVE/PUpar.txt")


################################################
################################################
### Plotting functions - contour graphs
### {JT, October 2019}
################################################
################################################
################################################


###########################
#### Function to correct Gr outside of growing season
###########################
CORRECT <- function(gr, ph){
  gr.out <-gr
  
  for (i in c(1:nrow(ph))){
    eos <- ph[i, 4]
    sos <- ph[i, 1]
    
    gr.out[(!(rownames(gr.out) %in% c(sos:eos))),i] <- 0
  }
  return(gr.out)
}

################################################
################################################
### Plotting functions - contour graphs
### {JT, April 2021}
################################################
################################################
################################################

PLOT.VS <- function(Gr, GrT, GrM, sm, temp, par, name){
  
  miss.file<- data.frame(DOY = c(1:365), A=0, B=0, C=0, D=0, E=0, F=0, G=0, H=0, I=0, J=0, K=0, L=0)
  
  miss <- 2015 - as.numeric(max(colnames(GrT)))
  
  Gr <- cbind(Gr, miss.file[,c(2:(1+miss))]); colnames(Gr) <- c(1960:2015)
  GrT <- cbind(GrT, miss.file[,c(2:(1+miss))]); colnames(GrT) <- c(1960:2015)
  GrM <- cbind(GrM, miss.file[,c(2:(1+miss))]); colnames(GrM) <- c(1960:2015)
  sm <- cbind(sm, miss.file[,c(2:(1+miss))]); colnames(sm) <- c(1960:2015)
  temp <- cbind(temp, miss.file[,c(2:(1+miss))]); colnames(temp) <- c(1960:2015)
  
  # Reformating data into matrixes
  matrix <- matrix(0, ncol=length(Gr), nrow = 365)
  colnames(matrix) <- colnames(Gr)
  rownames(matrix) <- c(1:365)
  matrix.2 <- matrix
  
  matrix <- as.matrix(Gr)
  
  matrix.2[((GrT > GrM) & sm < par$V6)] <- "blue" # Drought
  matrix.2[((GrT > GrM) & sm > par$V7)] <- "grey" # Soil moisture oversaturation
  matrix.2[((GrT < GrM) & temp < par$V2)] <- "red" # Cold
  matrix.2[((GrT < GrM) & temp > par$V3)] <- "purple" # Warm
  matrix.2[GrT == 1 & GrM == 1] <- "green" # Optimal
  colvec<- c("blue" = "#0000FF", "grey" = "#3C3D42", "red" = "#FF0000", "purple" = "#7A1E73", "green" = "#006D2C", "white" = "#FFFFFF")
  
  
  library(RColorBrewer); library(plot.matrix); library(reshape2); library(ggplot2)
  
  matrix.2melt <- melt(matrix.2)
  matrix.1melt <- melt(matrix)
  
  INPUT <- cbind(matrix.1melt, matrix.2melt$value); colnames(INPUT) <- c("DOY", "YEAR", "Gr", "Lim")
  INPUT[INPUT$Lim == 0, "Lim"] <- "white"
  maxGR <- max(INPUT$Gr)
  
  jpeg(paste("e:/JJC_VS/Obrazky/Matice/", name,".jpg", sep=""), width = (600/72)*(500*7/4), height = (600/72)*500, units = "px", res = 600)
  
  par(mar = c(5,5,1,3))
  
  mat <- ggplot(INPUT, aes(x = YEAR, y = DOY)) + 
    geom_raster(aes(fill=Lim, alpha = Gr)) + 
    scale_fill_manual(values=colvec)+
    ylab("DOY\n")+
    scale_alpha_continuous(range = c(0.15, (maxGR+0.15))) +
    scale_x_continuous(name = "Year\n", limits = c(1960, 2015), breaks = c(1940, 1960, 1980, 2000), labels = c(1940, 1960, 1980, 2000))+
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
  
}

PLOT.VS(CORRECT(JTgr, JTph), CORRECT(JTgrT, JTph), CORRECT(JTgrM, JTph), JTsm, JTtemp, JTpar, "1JT")
PLOT.VS(CORRECT(PHgr, PHph), CORRECT(PHgrT, PHph), CORRECT(PHgrM, PHph), PHsm, PHtemp, PHpar, "1PH")
PLOT.VS(CORRECT(PUgr, PUph), CORRECT(PUgrT, PUph), CORRECT(PUgrM, PUph), PUsm, PUtemp, PUpar, "2PU")
PLOT.VS(CORRECT(JCgr, JCph), CORRECT(JCgrT, JCph), CORRECT(JCgrM, JCph), JCsm, JCtemp, JCpar, "2JC")


##############################
### Klimagraf
##############################


Climchart <- function(gr, grT, grM, name){
res <- data.frame(DOY = c(1:365), Temp = NA, Moist = NA, Int=NA, Intplus = NA, Intminus = NA, MINxyl = NA, MAXxyl = NA, MEANxyl = NA, MINcam = NA, MAXcam = NA, MENAcam = NA)
  for (i in c(1:365)){
    res[i,"Temp"] <- rowMeans(grT[i,])
    res[i,"Moist"] <- rowMeans(grM[i,])
    res[i,"Int"] <- rowMeans(gr[i,])
    res[i,"Intminus"] <- rowMeans(gr[i,]) - sd(gr[i,]) 
    res[i,"Intplus"] <- rowMeans(gr[i,]) + sd(gr[i,]) 

  }
  res[res[,"Intminus"] < 0, "Intminus"] <- 0

  
  library(RColorBrewer); library(plot.matrix); library(ggplot2); library(cowplot)
  
  clim <- ggplot(data=res, aes(x=c(1:365), group=1)) +
    geom_ribbon(aes(ymin=Intminus, ymax=Intplus), alpha = 0.25, fill = "black")+
    geom_line(aes(y=Temp), col = "red", linetype = "solid", size = 0.75)+
    geom_line(aes(y=Moist), col = "blue", linetype = "solid", size = 0.75)+
    geom_line(aes(y=Int), col = "black", linetype = "solid", size = 0.85)+
    ylim(0,1)+
    xlim(0,365)+
    ylab("Partial and integral growth rates\n")+
    xlab("\nDOY")+
   # ggtitle(name)
      
  jpeg(paste("e:/JJC_VS/Obrazky/Klimagraf/", name,".jpg", sep=""), width = (600/72)*(500), height = (600/72)*500, units = "px", res = 600)
  
  print(clim + scale_color_grey()+ 
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(fill = NA, colour = "grey20"),
                axis.line = element_line(colour = "black"),
                axis.text = element_text(size = 24, colour = "black"),
                axis.title = element_text(size = 24, colour = "black"),
                axis.ticks.length=unit(.3, "cm"),  
                plot.title = element_text(color = "black", hjust = 0.1, vjust = -8, face = "bold.italic", size = 26)))
  dev.off()
  
  
}

Climchart(CORRECT(JTgr, JTph), JTgrT, JTgrM, "1JT")
Climchart(CORRECT(JCgr, JCph), JCgrT, JCgrM, "2JC")
Climchart(CORRECT(PHgr, PHph), PHgrT, PHgrM, "1PH")
Climchart(CORRECT(PUgr, PUph), PUgrT, PUgrM, "2PU")


Climchart.Change <- function(gr, camb, xyl, name){
  res <- data.frame(DOY = c(1:365), Cambium.old = NA, Cambium.young = NA, Xylem.old = NA, Xylem.young = NA)
  res.dif <- data.frame(DOY = c(1:365), Cambium = NA, Xylem = NA, Gr = NA, Cambium.p = .75, Xylem.p = .75, Gr.p = .75)
  
  for (i in c(1:365)){
    res[i,"Cambium.old"] <- rowMeans(camb[i,c(1:20)])
    res[i,"Cambium.young"] <- rowMeans(camb[i,c((ncol(camb)-19):ncol(camb))])
    res[i,"Gr.old"] <- rowMeans(gr[i,c(1:20)])
    res[i,"Gr.young"] <- rowMeans(gr[i,c((ncol(gr)-19):ncol(gr))])
    res[i,"Xylem.old"] <- rowMeans(xyl[i,c(1:20)])
    res[i,"Xylem.young"] <- rowMeans(xyl[i,c((ncol(xyl)-19):ncol(xyl))])
  }
  
  for (i in c(1:365)){
    sub.Gr.Old <- gr[i,c(1:20)]
    sub.Gr.Young <- gr[i,c((ncol(gr)-19):ncol(gr))]
    sub.Cambium.Old <- camb[i,c(1:20)]
    sub.Cambium.Young <- camb[i,c((ncol(camb)-19):ncol(camb))]    
    sub.Xylem.Old <- xyl[i,c(1:20)]
    sub.Xylem.Young <- xyl[i,c((ncol(xyl)-19):ncol(xyl))]
    
    res.dif[i, "Cambium"] <- res[i,"Cambium.young"] - res[i,"Cambium.old"]
    res.dif[i, "Gr"] <- res[i,"Gr.young"] - res[i,"Gr.old"]
    res.dif[i, "Xylem"] <- res[i,"Xylem.young"] - res[i,"Xylem.old"]
    
    if(!(res.dif[i, "Gr"]==0)) 
    {if ((t.test(x = sub.Gr.Old-sub.Gr.Young))$p.value < 0.05){res.dif[i, "Gr.p"] <- 1}} 
    
    if(!(res.dif[i, "Cambium"]==0))       
      {if ((t.test(x = sub.Cambium.Old-sub.Cambium.Young))$p.value < 0.05){res.dif[i, "Cambium.p"] <- 1}} 

    if(!(res.dif[i, "Xylem"]==0))       
      {if ((t.test(x = sub.Xylem.Old-sub.Xylem.Young))$p.value < 0.05){res.dif[i, "Xylem.p"] <- 1}} 
  }
  

  library(RColorBrewer); library(plot.matrix); library(ggplot2); library(cowplot)
  
  # 1] Plotting a chart for climate
  jpeg(paste("e:/JJC_VS/Obrazky/Klimagraf_change/GRint_", name,".jpg", sep=""), width = (600/72)*(500), height = (600/72)*500*1.4, units = "px", res = 600)
  
  p <- ggplot(data=res, aes(x=c(1:365), group=1)) +
    geom_line(aes(y=Gr.old), col = "black", linetype = "solid", size = 0.75, alpha = 0.4)+
    geom_line(aes(y=Gr.young), col = "black", linetype = "solid", size = 0.75, alpha = 1.0)+
    ylim(0,1)+
    xlab("\nDOY")+
    ylab("Integral growth rates\n")+
    ggtitle(name)+
     scale_color_grey()+ 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border = element_rect(fill = NA, colour = "grey20"),
              axis.line = element_line(colour = "black"),
              axis.text = element_text(size = 20, colour = "black"),
              axis.title = element_text(size = 20, colour = "black"),
              axis.ticks.length=unit(.3, "cm"),
              plot.title = element_text(color = "black", hjust = 0.1, vjust = -8, face = "bold.italic", size = 28))
  
  q <- ggplot(data=res.dif) +
    geom_line(aes(x=c(1:365), y=rep(0, 365)), linetype = "solid", size = 0.5)+
    geom_bar(aes(y=Gr, x=DOY, alpha = Gr.p), fill = "black", stat = "identity", position=position_dodge())+
    xlab("\nDOY")+
    ylab("Difference between subperiods\n")+
    scale_y_continuous(limits = c(-0.45, 0.45), breaks=c(-0.45,0,0.45))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, colour = "grey20"),
          legend.position = "none",
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size = 20, colour = "black"),
          axis.title = element_text(size = 20, colour = "black"),
          axis.ticks.length=unit(.3, "cm"),
          plot.title = element_text(color = "black", hjust = 0.5, vjust = 0, face = "bold", size = 28))
  
  
  
    print(
      plot_grid(p, q, nrow = 2, ncol = 1, rel_heights = c(0.9, 0.5), align = "hv"))
    dev.off()
  
 # 2] Plotting a chart for xylem
    jpeg(paste("e:/JJC_VS/Obrazky/Klimagraf_change/Xylem_", name,".jpg", sep=""), width = (600/72)*(500), height = (600/72)*500*1.4, units = "px", res = 600)
    
    p <- ggplot(data=res, aes(x=c(1:365), group=1)) +
      geom_line(aes(y=Xylem.old), col = "green", linetype = "solid", size = 0.75, alpha = 0.4)+
      geom_line(aes(y=Xylem.young), col = "green", linetype = "solid", size = 0.75, alpha = 1.0)+
      ylim(0,80)+
      xlab("\nDOY")+
      ylab("Number of xylem cells\n")+
      ggtitle(name)+
      scale_color_grey()+ 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(fill = NA, colour = "grey20"),
            axis.line = element_line(colour = "black"),
            axis.text = element_text(size = 20, colour = "black"),
            axis.title = element_text(size = 20, colour = "black"),
            axis.ticks.length=unit(.3, "cm"),
            plot.title = element_text(color = "black", hjust = 0.1, vjust = -8, face = "bold.italic", size = 28))
    
    q <- ggplot(data=res.dif) +
      geom_line(aes(x=c(1:365), y=rep(0, 365)), linetype = "solid", size = 0.5)+
      geom_bar(aes(y=Xylem, x=DOY, alpha = Xylem.p), fill = "green", stat = "identity", position=position_dodge())+
      xlab("\nDOY")+
      ylab("Difference between subperiods\n")+
      scale_y_continuous(limits = c(-25, 25), breaks=c(-25,0,25))+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(fill = NA, colour = "grey20"),
            legend.position = "none",
            axis.line = element_line(colour = "black"),
            axis.text = element_text(size = 20, colour = "black"),
            axis.title = element_text(size = 20, colour = "black"),
            axis.ticks.length=unit(.3, "cm"),
            plot.title = element_text(color = "black", hjust = 0.5, vjust = 0, face = "bold", size = 28))
    
    
    
    print(
      plot_grid(p, q, nrow = 2, ncol = 1, rel_heights = c(0.9, 0.5), align = "hv"))
    dev.off()
  
  # 3] Plotting a chart for cambium
    jpeg(paste("e:/JJC_VS/Obrazky/Klimagraf_change/Cambium_", name,".jpg", sep=""), width = (600/72)*(500), height = (600/72)*500*1.4, units = "px", res = 600)
    
    p <- ggplot(data=res, aes(x=c(1:365), group=1)) +
      geom_line(aes(y=Cambium.old), col = "orange", linetype = "solid", size = 0.75, alpha = 0.4)+
      geom_line(aes(y=Cambium.young), col = "orange", linetype = "solid", size = 0.75, alpha = 1.0)+
      xlab("\nDOY")+
      ylab("Cambial cells\n")+
      ggtitle(name)+
      scale_color_grey()+ 
      scale_y_continuous(limits = c(0, 10), breaks=c(0,5,10))+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(fill = NA, colour = "grey20"),
            axis.line = element_line(colour = "black"),
            axis.text = element_text(size = 20, colour = "black"),
            axis.title = element_text(size = 20, colour = "black"),
            axis.ticks.length=unit(.3, "cm"),
            plot.title = element_text(color = "black", hjust = 0.1, vjust = -8, face = "bold.italic", size = 28))
    
    q <- ggplot(data=res.dif) +
      geom_line(aes(x=c(1:365), y=rep(0, 365)), linetype = "solid", size = 0.5)+
      geom_bar(aes(y=Cambium, x=DOY, alpha = Cambium.p), fill = "orange", stat = "identity", position=position_dodge())+
      xlab("\nDOY")+
      ylab("Difference between subperiods\n")+
      scale_y_continuous(limits = c(-4, 4), breaks=c(-4,0,4))+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(fill = NA, colour = "grey20"),
            legend.position = "none",
            axis.line = element_line(colour = "black"),
            axis.text = element_text(size = 20, colour = "black"),
            axis.title = element_text(size = 20, colour = "black"),
            axis.ticks.length=unit(.3, "cm"),
            plot.title = element_text(color = "black", hjust = 0.5, vjust = 0, face = "bold", size = 28))
    
    
    
    print(
      plot_grid(p, q, nrow = 2, ncol = 1, rel_heights = c(0.9, 0.5), align = "hv"))
    dev.off()
    
  
}

Climchart.Change(CORRECT(JTgr, JTph), JTcamb, JTxyl, "1JT")
Climchart.Change(CORRECT(JCgr, JCph), JCcamb, JCxyl, "2JC")
Climchart.Change(CORRECT(PHgr, PHph), PHcamb, PHxyl, "1PH")
Climchart.Change(CORRECT(PUgr, PUph), PUcamb,  PUxyl, "2PU")


##############################
### Zero-growth probability
##############################

ZeroGrowth <- function(gr, xyl, camb, name = "aaa"){
  res <- data.frame(DOY = c(1:365), zeroGR = 0, zeroXyl = 0, zeroCamb = 0)
  res.xyl <- gr; res.xyl[] <- 0
  res.camb <- res.xyl
  
  for (i in c(1:365)){
    res[i,"zeroGR"] <- length(gr[i,gr[i,]>0]) / ncol(gr)
    
    for (j in c(1:ncol(gr))){
      basecamb <- 3 # min(camb[1, j], camb[365, j])
      
      if (camb[i, j] > basecamb) {res.camb[i, j] <- 1}
      
      if(i %in% c(1:350)){
        
      mincell <- xyl[i, j] # The number of cells at the considered day
      maxcell <- xyl[i+14, j] # The number of cells at the considered day + 2 weeks
      
      if (maxcell - mincell > 0) {res.xyl[i, j] <- 1}}
      
      
    }
    res[i,"zeroXyl"] <- sum(res.xyl[i,]) / (ncol(res.xyl))
    res[i,"zeroCamb"] <- sum(res.camb[i,]) / (ncol(res.camb))
    
    
  }
  
  library(RColorBrewer); library(plot.matrix); library(ggplot2); library(cowplot)

  jpeg(paste("e:/JJC_VS/Obrazky/ZeroGrowth/", name,".jpg", sep=""), width = (600/72)*(500), height = (600/72)*500, units = "px", res = 600)
  
  z <- ggplot(data=res, aes(x=c(1:365), group=1)) +
    geom_line(aes(y=zeroGR), col = "black", linetype = "solid", size = 0.75, alpha = 1.0)+
    geom_line(aes(y=zeroXyl), col = "green", linetype = "solid", size = 0.75, alpha = 1.0)+
    geom_line(aes(y=zeroCamb), col = "orange", linetype = "solid", size = 0.75, alpha = 1.0)+
    ylim(0,1)+
    xlab("\nDOY")+
    ylab("Probability of simulated growth\n")
    
  
  
  print(z + scale_color_grey()+ 
          theme(panel.grid.major = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(fill = NA, colour = "grey20"),
                axis.line = element_line(colour = "black"),
                axis.text = element_text(size = 24, colour = "black"),
                axis.title = element_text(size = 24, colour = "black"),
                axis.ticks.length=unit(.3, "cm")))
  dev.off()
  
}

ZeroGrowth(CORRECT(JTgr, JTph), JTxyl, JTcamb, "1JT")
ZeroGrowth(CORRECT(JCgr, JCph), JCxyl, JCcamb, "2JC")
ZeroGrowth(CORRECT(PHgr, PHph), PHxyl, PHcamb, "1PH")
ZeroGrowth(CORRECT(PUgr, PUph), PUxyl, PUcamb, "2PU")

##############################
### ObsMod
##############################

ObsMod <- function(chronos, name){
  
  corEnv <- round(cor(na.omit(chronos$obs), na.omit(chronos$env)), 2)
  corCam <- round(cor(na.omit(chronos$obs), na.omit(chronos$cam)), 2)
  
  
  library(RColorBrewer); library(plot.matrix); library(ggplot2)
  
  jpeg(paste("e:/JJC_VS/Obrazky/ObsMod/", name,".jpg", sep=""), width = (600/72)*(500*8/4), height = (600/72)*500*1.25, units = "px", res = 600)
  
  
  p <- ggplot(data=chronos, aes(x=c(1960:2012), group=1)) +
    geom_line(aes(y=rep(1, 53)), col = "grey20", linetype = "dotted", size = 0.5)+
    geom_line(aes(y=obs), col = "red", linetype = "solid", size = 0.85)+
    geom_line(aes(y=cam), col = "blue", linetype = "solid", size = 0.85)+
    # geom_line(aes(y=env), col = "green", linetype = "solid", size = 0.85)+
    ylim(0.0001,2.8)+
    xlab("\nYear")+
    ylab("Index\n")+
    ggtitle(name)+
    annotate("text", x = 1986, y=2.5, label = paste("r = ", corCam, sep=""), size = 12)
    
  
  print(
    p + scale_color_grey()+ 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, colour = "grey20"),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size = 24, colour = "black"),
          axis.title = element_text(size = 24, colour = "black"),
          axis.ticks.length=unit(.3, "cm"),
          plot.title = element_text(color = "black", hjust = 0.5, vjust = -8, face = "bold.italic", size = 32)))
  

    dev.off()
  
  }


ObsMod(JTom, "J. thurifera")
ObsMod(JCom[c(1:53),], "J. communis")
ObsMod(PHom, "P. halepensis")
ObsMod(PUom[c(1:53),], "P. uncinata")

##############################
### JT+PH - effect of uni/bimodality on the intra-annual pattern of cell number
##############################

#### JT ####
JT.xyl.uni <-rownames(JTph[JTph$Eend<250,])
JT.xyl.bi <-rownames(JTph[JTph$Eend>249,])

JTxyl.uni.sub <- JTxyl[,colnames(JTxyl) %in% JT.xyl.uni]
JTxyl.bi.sub <- JTxyl[,colnames(JTxyl) %in% JT.xyl.bi]
JTcam.uni.sub <- JTcamb[,colnames(JTxyl) %in% JT.xyl.uni]
JTcam.bi.sub <- JTcamb[,colnames(JTxyl) %in% JT.xyl.bi]
JTgrm.uni.sub <- JTgrM[c(1:365),colnames(JTxyl) %in% JT.xyl.uni]
JTgrm.bi.sub <- JTgrM[c(1:365),colnames(JTxyl) %in% JT.xyl.bi]
JTgrt.uni.sub <- JTgrT[c(1:365),colnames(JTxyl) %in% JT.xyl.uni]
JTgrt.bi.sub <- JTgrT[c(1:365),colnames(JTxyl) %in% JT.xyl.bi]

res <- data.frame(DOY = c(1:365), uni.mean = rowMeans(JTxyl.uni.sub), bi.mean = rowMeans(JTxyl.bi.sub),
                                  Cuni.mean = rowMeans(JTcam.uni.sub), Cbi.mean = rowMeans(JTcam.bi.sub),
                                  Muni.mean = rowMeans(JTgrm.uni.sub), Mbi.mean = rowMeans(JTgrm.bi.sub),
                                  Tuni.mean = rowMeans(JTgrt.uni.sub), Tbi.mean = rowMeans(JTgrt.bi.sub))

for (i in c(1:365)){
  res[i, "uni.max"] <- res[i, "uni.mean"] + sd(JTxyl.uni.sub[i,])
  res[i, "uni.min"] <- res[i, "uni.mean"] - sd(JTxyl.uni.sub[i,])
  res[i, "bi.max"] <- res[i, "bi.mean"] + sd(JTxyl.bi.sub[i,])
  res[i, "bi.min"] <- res[i, "bi.mean"] - sd(JTxyl.bi.sub[i,])
  
}

#### PH ####
PH.xyl.uni <-rownames(PHph[PHph$Eend<250,])
PH.xyl.bi <-rownames(PHph[PHph$Eend>249,])

PHxyl.uni.sub <- PHxyl[,colnames(PHxyl) %in% PH.xyl.uni]
PHxyl.bi.sub <- PHxyl[,colnames(PHxyl) %in% PH.xyl.bi]
PHcam.uni.sub <- PHcamb[,colnames(PHxyl) %in% PH.xyl.uni]
PHcam.bi.sub <- PHcamb[,colnames(PHxyl) %in% PH.xyl.bi]
PHgrm.uni.sub <- PHgrM[c(1:365),colnames(PHxyl) %in% PH.xyl.uni]
PHgrm.bi.sub <- PHgrM[c(1:365),colnames(PHxyl) %in% PH.xyl.bi]
PHgrt.uni.sub <- PHgrT[c(1:365),colnames(PHxyl) %in% PH.xyl.uni]
PHgrt.bi.sub <- PHgrT[c(1:365),colnames(PHxyl) %in% PH.xyl.bi]

res <- data.frame(DOY = c(1:365), uni.mean = rowMeans(PHxyl.uni.sub), bi.mean = rowMeans(PHxyl.bi.sub),
                  Cuni.mean = rowMeans(PHcam.uni.sub), Cbi.mean = rowMeans(PHcam.bi.sub),
                  Muni.mean = rowMeans(PHgrm.uni.sub), Mbi.mean = rowMeans(PHgrm.bi.sub),
                  Tuni.mean = rowMeans(PHgrt.uni.sub), Tbi.mean = rowMeans(PHgrt.bi.sub))

for (i in c(1:365)){
  res[i, "uni.max"] <- res[i, "uni.mean"] + sd(PHxyl.uni.sub[i,])
  res[i, "uni.min"] <- res[i, "uni.mean"] - sd(PHxyl.uni.sub[i,])
  res[i, "bi.max"] <- res[i, "bi.mean"] + sd(PHxyl.bi.sub[i,])
  res[i, "bi.min"] <- res[i, "bi.mean"] - sd(PHxyl.bi.sub[i,])
  
}


library(RColorBrewer); library(plot.matrix); library(ggplot2); library(cowplot)
jpeg("e:/JJC_VS/Obrazky/Cells/BiUniJT.jpg", width = (600/72)*(500)*1, height = (600/72)*500*3, units = "px", res = 600)


xyl <- ggplot(data=res, aes(x=c(1:365), group=1)) +
  # geom_ribbon(aes(ymin=uni.min, ymax=uni.max), alpha = 0.25, fill = "red")+
  # geom_ribbon(aes(ymin=bi.min, ymax=bi.max), alpha = 0.25, fill = "blue")+
  geom_line(aes(y=uni.mean), col = "red", linetype = "solid", size = 0.75)+
  geom_line(aes(y=bi.mean), col = "blue", linetype = "solid", size = 0.75)+
  ylim(0,80)+
  xlim(0,365)+
  ylab("Produced tracheids\n")+
  xlab("\nDOY")+
  scale_color_grey()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey20"),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 24, colour = "black"),
        axis.title = element_text(size = 24, colour = "black"),
        axis.ticks.length=unit(.3, "cm"),  
        plot.title = element_text(color = "black", hjust = 0.1, vjust = -8, face = "bold.italic", size = 26))

camb <- ggplot(data=res, aes(x=c(1:365), group=1)) +
  # geom_ribbon(aes(ymin=uni.min, ymax=uni.max), alpha = 0.25, fill = "red")+
  # geom_ribbon(aes(ymin=bi.min, ymax=bi.max), alpha = 0.25, fill = "blue")+
  geom_line(aes(y=Cuni.mean), col = "red", linetype = "solid", size = 0.75)+
  geom_line(aes(y=Cbi.mean), col = "blue", linetype = "solid", size = 0.75)+
  ylim(0,10)+
  xlim(0,365)+
  ylab("Cambial cell\n")+
  xlab("\nDOY")+
  scale_color_grey()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey20"),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 24, colour = "black"),
        axis.title = element_text(size = 24, colour = "black"),
        axis.ticks.length=unit(.3, "cm"),  
        plot.title = element_text(color = "black", hjust = 0.1, vjust = -8, face = "bold.italic", size = 26))

moist <- ggplot(data=res, aes(x=c(1:365), group=1)) +
  geom_line(aes(y=Muni.mean), col = "red", linetype = "solid", size = 0.75)+
  geom_line(aes(y=Mbi.mean), col = "blue", linetype = "solid", size = 0.75)+
  ylim(0,1)+
  xlim(0,365)+
  ylab("Growth rate to soil moisture\n")+
  xlab("\nDOY")+
  scale_color_grey()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey20"),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 24, colour = "black"),
        axis.title = element_text(size = 24, colour = "black"),
        axis.ticks.length=unit(.3, "cm"),  
        plot.title = element_text(color = "black", hjust = 0.1, vjust = -8, face = "bold.italic", size = 26))

temp <- ggplot(data=res, aes(x=c(1:365), group=1)) +
  geom_line(aes(y=Tuni.mean), col = "red", linetype = "solid", size = 0.75)+
  geom_line(aes(y=Tbi.mean), col = "blue", linetype = "solid", size = 0.75)+
  ylim(0,1)+
  xlim(0,365)+
  ylab("Growth rate to temperature\n")+
  xlab("\nDOY")+
  scale_color_grey()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey20"),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 24, colour = "black"),
        axis.title = element_text(size = 24, colour = "black"),
        axis.ticks.length=unit(.3, "cm"),  
        plot.title = element_text(color = "black", hjust = 0.1, vjust = -8, face = "bold.italic", size = 26))


print(plot_grid(xyl, camb, moist, temp, nrow = 4, ncol = 1, align = "hv") )
dev.off()


##############################
### Phenol
##############################

Phenol <- function(phenology, name){
  means <- colMeans(phenology, na.rm = T)
  means <- round(means, 0)
  
  library(RColorBrewer); library(plot.matrix); library(ggplot2)
  
  jpeg(paste("e:/JJC_VS/Obrazky/Phenol/", name,".jpg", sep=""), width = (600/72)*(500*7/4), height = (600/72)*500, units = "px", res = 600)
  
  p <- ggplot(data=phenology, aes(x=c(1960:2012), group=1)) +
    geom_line(aes(y=Cstart), col = "grey", linetype = "solid", size = 0.5)+
    geom_line(aes(y=Estart), col = "red", linetype = "solid", size = 0.65)+
    geom_line(aes(y=Eend), col = "red", linetype = "solid", size = 0.65)+
    geom_line(aes(y=Cend), col = "grey", linetype = "solid", size = 0.5)+
    ylim(0,366)+
    xlab("\nYear")+
    ylab("DOY\n")+
    ggtitle(name)
  
  print(
    p + scale_color_grey()+ 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(fill = NA, colour = "grey20"),
            axis.line = element_line(colour = "black"),
            axis.text = element_text(size = 14, colour = "black"),
            axis.title = element_text(size = 14, colour = "black"),
            axis.ticks.length=unit(.3, "cm"),
            plot.title = element_text(color = "black", hjust = 0.5, vjust = -8, face = "bold", size = 22)))
  
  dev.off()
  
}


Phenol(JTph, "1JT")
Phenol(JCph[c(1:53),], "1JC")
Phenol(PHph, "1PH")
Phenol(PUph[c(1:53),], "1PU")

#### Phenol do clanku

dry <- as.data.frame(cbind(YEAR = as.numeric(rownames(PHph)), Pstart = PHph$Estart, Jstart = JTph$Estart, Pend = PHph$Eend, Jend = JTph$Eend))
cold <- as.data.frame(cbind(YEAR = as.numeric(rownames(PUph[c(1:53),])), Pstart = PUph[c(1:53),"Estart"], Jstart = JCph[c(1:53), "Estart"], Pend = PUph[c(1:53), "Eend"], Jend = JCph[c(1:53), "Eend"]))

dry.mean <- as.data.frame(cbind(P.start.mean = mean(dry$Pstart), P.start.sd = sd(dry$Pstart), P.end.mean = mean(dry$Pend), P.end.sd = sd(dry$Pend),
                                J.start.mean = mean(dry$Jstart), J.start.sd = sd(dry$Jstart), J.end.mean = mean(dry$Jend), J.end.sd = sd(dry$Jend)))

cold.mean <- as.data.frame(cbind(P.start.mean = mean(cold$Pstart), P.start.sd = sd(cold$Pstart), P.end.mean = mean(cold$Pend), P.end.sd = sd(cold$Pend),
                                J.start.mean = mean(cold$Jstart), J.start.sd = sd(cold$Jstart), J.end.mean = mean(cold$Jend), J.end.sd = sd(cold$Jend)))

site.dry <- ggplot(data=dry, aes(x=c(1960:2012), group=1)) +
  geom_line(aes(y=Pstart), col = "orange", linetype = "solid", size = 0.65)+
  geom_line(aes(y=Pend), col = "orange", linetype = "solid", size = 0.65)+
  geom_line(aes(y=Jstart), col = "brown", linetype = "solid", size = 0.65)+
  geom_line(aes(y=Jend), col = "brown", linetype = "solid", size = 0.65)+
  ylim(0,366)+
  xlab("\nYear")+
  ylab("DOY\n")+
  scale_color_grey()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "white"),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 18, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"),
        axis.ticks.length=unit(.3, "cm"),
        plot.title = element_text(color = "black", hjust = 0.5, vjust = -8, face = "bold", size = 22))

mean.dry <- ggplot(data=dry.mean) +
  geom_point(aes(y=P.start.mean, x = 1.05), col = "orange",  size = 6)+
  geom_point(aes(y=P.end.mean, x = 1.05), col = "orange",  size = 6)+
  geom_point(aes(y=J.start.mean, x = 0.95), col = "brown",  size = 6)+
  geom_point(aes(y=J.end.mean, x = 0.95), col = "brown",  size = 6)+
  geom_errorbar(aes(ymin=P.start.mean-P.start.sd, ymax=P.start.mean+P.start.sd, x=1.05), width=0.4, colour="orange", alpha=0.9, size=1.3)+
  geom_errorbar(aes(ymin=P.end.mean-P.end.sd, ymax=P.end.mean+P.end.sd, x=1.05), width=0.4, colour="orange", alpha=0.9, size=1.3)+
  geom_errorbar(aes(ymin=J.start.mean-J.start.sd, ymax=J.start.mean+J.start.sd, x=.95), width=0.4, colour="brown", alpha=0.9, size=1.3)+
  geom_errorbar(aes(ymin=J.end.mean-J.end.sd, ymax=J.end.mean+J.end.sd, x=.95), width=0.4, colour="brown", alpha=0.9, size=1.3)+
  scale_color_grey()+
  xlim(0.8,1.2)+
  scale_y_continuous(position = "right", name = "DOY\n", limits = c(0, 366))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "white"),
        axis.line.x = element_line(colour = "white"),
        axis.text.x = element_text(size = 18, colour = "white"),
        axis.title.x = element_text(size = 18, colour = "white"),
        axis.ticks.length.x= unit(0.3, "cm"),
        axis.ticks.x = element_line(colour = "white"),
        axis.line.y = element_line(colour = "black"),
        axis.text.y = element_text(size = 18, colour = "black"),
        axis.title.y = element_text(size = 18, colour = "black"),
        axis.ticks.length.y=unit(.3, "cm"),
        plot.title = element_text(color = "black", hjust = 0.5, vjust = -8, face = "bold", size = 22))

site.cold <- ggplot(data=cold, aes(x=c(1960:2012), group=1)) +
  geom_line(aes(y=Pstart), col = "orange", linetype = "solid", size = 0.65)+
  geom_line(aes(y=Pend), col = "orange", linetype = "solid", size = 0.65)+
  geom_line(aes(y=Jstart), col = "brown", linetype = "solid", size = 0.65)+
  geom_line(aes(y=Jend), col = "brown", linetype = "solid", size = 0.65)+
  ylim(0,366)+
  xlab("\nYear")+
  ylab("DOY\n")+
  scale_color_grey()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "white"),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 18, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"),
        axis.ticks.length=unit(.3, "cm"),
        plot.title = element_text(color = "black", hjust = 0.5, vjust = -8, face = "bold", size = 22))

mean.cold <- ggplot(data=cold.mean) +
  geom_point(aes(y=P.start.mean, x = 1.05), col = "orange",  size = 6)+
  geom_point(aes(y=P.end.mean, x = 1.05), col = "orange",  size = 6)+
  geom_point(aes(y=J.start.mean, x = 0.95), col = "brown",  size = 6)+
  geom_point(aes(y=J.end.mean, x = 0.95), col = "brown",  size = 6)+
  geom_errorbar(aes(ymin=P.start.mean-P.start.sd, ymax=P.start.mean+P.start.sd, x=1.05), width=0.4, colour="orange", alpha=0.9, size=1.3)+
  geom_errorbar(aes(ymin=P.end.mean-P.end.sd, ymax=P.end.mean+P.end.sd, x=1.05), width=0.4, colour="orange", alpha=0.9, size=1.3)+
  geom_errorbar(aes(ymin=J.start.mean-J.start.sd, ymax=J.start.mean+J.start.sd, x=.95), width=0.4, colour="brown", alpha=0.9, size=1.3)+
  geom_errorbar(aes(ymin=J.end.mean-J.end.sd, ymax=J.end.mean+J.end.sd, x=.95), width=0.4, colour="brown", alpha=0.9, size=1.3)+
  scale_color_grey()+
  xlim(0.8,1.2)+
  scale_y_continuous(position = "right", name = "DOY\n", limits = c(0, 366))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "white"),
        axis.line.x = element_line(colour = "white"),
        axis.text.x = element_text(size = 18, colour = "white"),
        axis.title.x = element_text(size = 18, colour = "white"),
        axis.ticks.length.x= unit(0.3, "cm"),
        axis.ticks.x = element_line(colour = "white"),
        axis.line.y = element_line(colour = "black"),
        axis.text.y = element_text(size = 18, colour = "black"),
        axis.title.y = element_text(size = 18, colour = "black"),
        axis.ticks.length.y=unit(.3, "cm"),
        plot.title = element_text(color = "black", hjust = 0.5, vjust = -8, face = "bold", size = 22))

library(RColorBrewer); library(plot.matrix); library(ggplot2)

jpeg("e:/JJC_VS/Obrazky/Phenol/dryMean.jpg", width = (600/72)*500*6/4*0.2, height = (600/72)*500, units = "px", res = 600)
  print(mean.dry)
dev.off()

jpeg("e:/JJC_VS/Obrazky/Phenol/dryChron.jpg", width = (600/72)*500*6/4, height = (600/72)*500, units = "px", res = 600)
  print(site.dry)
dev.off()

jpeg("e:/JJC_VS/Obrazky/Phenol/coldMean.jpg", width = (600/72)*500*6/4*0.2, height = (600/72)*500, units = "px", res = 600)
  print(mean.cold)
dev.off()

jpeg("e:/JJC_VS/Obrazky/Phenol/coldChron.jpg", width = (600/72)*500*6/4, height = (600/72)*500, units = "px", res = 600)
  print(site.cold)
dev.off()

##############################
### Intra-annual pattern of cell number
##############################

library(readxl)
JTobsxyl <- read_excel("E:/JJC_VS/Data/Xylo/xylogenesis junipers_trees2.xlsx", sheet = "JT")
JCobsxyl <- read_excel("E:/JJC_VS/Data/Xylo/xylogenesis junipers_trees2.xlsx", sheet = "JC")
PUobsxyl <- read_excel("E:/JJC_VS/Data/Xylo/xylogenesis junipers_trees2.xlsx", sheet = "PU")
PHobsxyl <- read_excel("E:/JJC_VS/Data/Xylo/xylogenesis junipers_trees2.xlsx", sheet = "PH")

JTobscam <- read_excel("E:/JJC_VS/Data/Xylo/xylogenesis junipers_trees2.xlsx", sheet = "JTcamb")
JCobscam <- read_excel("E:/JJC_VS/Data/Xylo/xylogenesis junipers_trees2.xlsx", sheet = "JCcamb")
PUobscam <- read_excel("E:/JJC_VS/Data/Xylo/xylogenesis junipers_trees2.xlsx", sheet = "PUcamb")
PHobscam <- read_excel("E:/JJC_VS/Data/Xylo/xylogenesis junipers_trees2.xlsx", sheet = "PHcamb")
                                     

CELL.VERIFICATION <- function(input.MOD, input.OBS, name, year, type="raw"){
  data.to.plot <- data.frame(DOY = c(1:365), MOD = NA, OBS.MIN = NA, OBS.MEAN = NA, OBS.MAX = NA)
  
  if (type == "raw"){span <- c(0,40) # 0-15 cambial zone, 0-40 differentiation zone
                     data.to.plot[,"MOD"] <- input.MOD[,colnames(input.MOD)==year] 
  
                     for (i in c(1:nrow(input.OBS))){
                      doy <- as.numeric(input.OBS[i, "DOY"])
    
                      data.to.plot[data.to.plot$DOY == doy,"OBS.MIN"] <- input.OBS[i, "EWM"] - (input.OBS[i, "EWMsd"])
                      data.to.plot[data.to.plot$DOY == doy,"OBS.MEAN"] <- input.OBS[i, "EWM"]
                      data.to.plot[data.to.plot$DOY == doy,"OBS.MAX"] <- input.OBS[i, "EWM"] + (input.OBS[i, "EWMsd"])
                     }}
  
  if (type == "std"){span <- c(0,1)
                    data.to.plot[,"MOD"] <- input.MOD[,colnames(input.MOD)==year]  / input.MOD[365, colnames(input.MOD)==year]
  
                    for (i in c(1:nrow(input.OBS))){
                     doy <- as.numeric(input.OBS[i, "DOY"])
    
                     data.to.plot[data.to.plot$DOY == doy,"OBS.MIN"] <- input.OBS[i, "PERC"] - 2.262*(input.OBS[i, "PERCsd"]/sqrt(10))
                     data.to.plot[data.to.plot$DOY == doy,"OBS.MEAN"] <- input.OBS[i, "PERC"]
                     data.to.plot[data.to.plot$DOY == doy,"OBS.MAX"] <- input.OBS[i, "PERC"] + 2.262*(input.OBS[i, "PERCsd"]/sqrt(10))
                     
                     data.to.plot[!(is.na(data.to.plot[,"OBS.MIN"])) & data.to.plot[,"OBS.MIN"] < 0, "OBS.MIN"] <- 0
                     data.to.plot[!(is.na(data.to.plot[,"OBS.MAX"])) & data.to.plot[,"OBS.MAX"] > 1, "OBS.MAX"] <- 1
                    }}
  
    print((cor.test(~ OBS.MEAN + MOD, data = subset(data.to.plot, !is.na(data.to.plot$OBS.MEAN))))$estimate)
    
    data.Wilc <- subset(data.to.plot, !is.na(data.to.plot$OBS.MEAN))
    print(wilcox.test(x = data.Wilc$OBS.MEAN, y = data.Wilc$MOD, paired = T, mu = 0))
  
  

  library(RColorBrewer); library(plot.matrix); library(ggplot2)
  
  jpeg(paste("e:/JJC_VS/Obrazky/CellsVerif/", name,".jpg", sep=""), width = (600/72)*(500), height = (600/72)*500, units = "px", res = 600)
  
  p <- ggplot() +
    geom_line(data=subset(data.to.plot, !is.na(data.to.plot$OBS.MEAN)), aes(y=OBS.MEAN, x=DOY), col = "red", linetype = "dashed", size = 0.75)+
    geom_point(data=subset(data.to.plot, !is.na(data.to.plot$OBS.MEAN)), aes(y=OBS.MEAN, x=DOY), col = "red", size = 4)+
    geom_ribbon(data=subset(data.to.plot, !is.na(data.to.plot$OBS.MEAN)), aes(ymin=OBS.MIN, ymax=OBS.MAX, x=DOY), alpha = 0.25, fill = "red")+
    geom_line(data=data.to.plot, aes(y=MOD, x=DOY), col = "blue", linetype = "dashed", size = 0.75)+
    geom_point(data=subset(data.to.plot, !is.na(data.to.plot$OBS.MEAN)), aes(y=MOD, x=DOY), col = "blue", size = 4)+
    ylim(span)+
    xlab("\nDOY")+
    ylab("Number of cells\n")+
    ggtitle(name)
  

  print(
    p + scale_color_grey()+ 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(fill = NA, colour = "grey20"),
            axis.line = element_line(colour = "black"),
            axis.text = element_text(size = 18, colour = "black"),
            axis.title = element_text(size = 18, colour = "black"),
            axis.ticks.length=unit(.3, "cm"),
            plot.title = element_text(color = "black", hjust = 0.5, vjust = -8, face = "bold.italic", size = 26)))
  
  dev.off()
  
 # return(data.Wilc)
  
}

CELL <- function(input.xyl, input.cam, name){
  data.to.plot <- data.frame(DOY = c(1:365), MINxyl = NA, MEANxyl = NA, MAXxyl = NA, MINcam = NA, MENAcam = NA, MAXcam = NA)
  
  for (i in c(1:365)){
    data.to.plot[i,"MINxyl"] <- rowMeans(input.xyl[i,]) - sd(input.xyl[i,])
    data.to.plot[i,"MEANxyl"] <- rowMeans(input.xyl[i,])
    data.to.plot[i,"MAXxyl"] <- rowMeans(input.xyl[i,]) + sd(input.xyl[i,])
    
    data.to.plot[i,"MINcam"] <- rowMeans(input.cam[i,]) - sd(input.cam[i,])
    data.to.plot[i,"MEANcam"] <- rowMeans(input.cam[i,])
    data.to.plot[i,"MAXcam"] <- rowMeans(input.cam[i,]) + sd(input.cam[i,])
  }
  data.to.plot[data.to.plot[,"MINxyl"] < 0, "MINxyl"] <- 0
  
  
  library(RColorBrewer); library(plot.matrix); library(ggplot2)
  
  jpeg(paste("e:/JJC_VS/Obrazky/Cells/", name,".jpg", sep=""), width = (600/72)*(500), height = (600/72)*500, units = "px", res = 600)
  
  p <- ggplot(data=data.to.plot, aes(x=c(1:365), group=1)) +
    geom_line(aes(y=8*MEANcam), col = "orange", linetype = "solid", size = 0.75)+
    geom_ribbon(aes(ymin=8*MINcam, ymax=8*MAXcam), alpha = 0.25, fill = "orange")+
    geom_line(aes(y=MEANxyl), col = "green", linetype = "solid", size = 0.75)+
    geom_ribbon(aes(ymin=MINxyl, ymax=MAXxyl), alpha = 0.25, fill = "green")+
    xlim(0,365)+
    xlab("\nDOY")+
    # ggtitle(name)+
    scale_y_continuous(name = "Number of xylem cells\n", limits=c(0, 100), sec.axis = sec_axis( trans=~./8, name="Number of cambial cells\n", breaks = c(0,5,10,15)))
  
  
  print(
    p + scale_color_grey()+ 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(fill = NA, colour = "grey20"),
            axis.line = element_line(colour = "black"),
            axis.text = element_text(size = 24, colour = "black"),
            axis.title = element_text(size = 24, colour = "black"),
            axis.ticks.length=unit(.3, "cm"),
            plot.title = element_text(color = "black", hjust = 0.5, vjust = -8, face = "bold.italic", size = 26)))
  
  dev.off()
  
}


#######################

CELL.VERIFICATION(JTxyl, JTobsxyl, "J. thurifera", 2006, "raw")
CELL.VERIFICATION(JCxyl, JCobsxyl, "J. communis", 2014, "raw")
CELL.VERIFICATION(PHxyl, PHobsxyl, "P. halepensis", 2006, "raw")
CELL.VERIFICATION(PUxyl, PUobsxyl, "P. uncinata", 2014, "raw")

CELL.VERIFICATION(JTcamb, JTobscam, "J. thurifera_Cambial zone", 2006, "raw")
CELL.VERIFICATION(JCcamb, JCobscam, "J. communis_Cambial zone", 2014, "raw")
CELL.VERIFICATION(PHcamb, PHobscam, "P. halepensis_Cambial zone", 2006, "raw")
CELL.VERIFICATION(PUcamb, PUobscam, "P. uncinata_Cambial zone", 2014, "raw")

CELL(JTxyl, JTcamb, "J. thurifera")
CELL(JCxyl, JCcamb, "J. communis")
CELL(PUxyl, PUcamb, "P. uncinata")
CELL(PHxyl, PHcamb, "P. halepensis")

########################################
### Statisticke restovani rozdilu ve fenologii
########################################


t.test(PHph[,1], JTph[,1], paired = T)
t.test(PHph[,2], JTph[,2], paired = T)
t.test(PHph[,3], JTph[,3], paired = T)
t.test(PHph[,4], JTph[,4], paired = T)

t.test(PUph[,1], JCph[,1], paired = T)
t.test(PUph[,2], JCph[,2], paired = T)
t.test(PUph[,3], JCph[,3], paired = T)
t.test(PUph[,4], JCph[,4], paired = T)

summary(lm(PHph[,1]~c(1960:2012)))
summary(lm(PHph[,2]~c(1960:2012)))
summary(lm(PHph[,3]~c(1960:2012)))
summary(lm(PHph[,4]~c(1960:2012)))

summary(lm(JTph[,1]~c(1960:2012)))
summary(lm(JTph[,2]~c(1960:2012)))
summary(lm(JTph[,3]~c(1960:2012)))
summary(lm(JTph[,4]~c(1960:2012)))

summary(lm(PUph[,1]~c(1960:2012)))
summary(lm(PUph[,2]~c(1960:2012)))
summary(lm(PUph[,3]~c(1960:2012)))
summary(lm(PUph[,4]~c(1960:2012)))

summary(lm(JCph[,1]~c(1960:2012)))
summary(lm(JCph[,2]~c(1960:2012)))
summary(lm(JCph[,3]~c(1960:2012)))
summary(lm(JCph[,4]~c(1960:2012)))

mean(PHph[PHph$Eend < 251, "Eend"]) # 7
mean(JTph[JTph$Eend < 251, "Eend"]) # 11

mean(PHph[PHph$Eend > 250, "Eend"]) # 46
mean(JTph[JTph$Eend > 250, "Eend"]) # 42

########################################
### Prirust v ruznych klimatickych limitacich
########################################

KLIMA <- function(Gr, GrT, GrM){
  OUTPUT <- data.frame(YEAR = c(1960:2012), Temp = NA, Moist = NA, Opt = NA, ALL = NA)
  
  for (i in c(1:ncol(GrT))){
    subT <- GrT[c(1:365),i]; subM <- GrM[c(1:365),i]; subGr <- Gr[c(1:365),i]
    
    OUTPUT[i,"Moist"] <- sum(subGr[subT > subM])
    OUTPUT[i,"Temp"] <- sum(subGr[subT < subM])
    OUTPUT[i,"Opt"] <- sum(subGr[subT ==1 & subM == 1])
    OUTPUT[i,"ALL"] <- sum(subGr)
    
  }
  return(OUTPUT)
}

limitaceJT <- KLIMA(CORRECT(JTgr, JTph), JTgrT, JTgrM)
limitaceJC <- KLIMA(CORRECT(JCgr, JCph), JCgrT, JCgrM)
limitacePH <- KLIMA(CORRECT(PHgr, PHph), PHgrT, PHgrM)
limitacePU <- KLIMA(CORRECT(PUgr, PUph), PUgrT, PUgrM)



write.table(limitaceJT, "e:/JJC_VS/Rskripty/vysledky/limitaceJT.txt", row.names = F)
write.table(limitaceJC, "e:/JJC_VS/Rskripty/vysledky/limitaceJC.txt", row.names = F)
write.table(limitacePH, "e:/JJC_VS/Rskripty/vysledky/limitacePH.txt", row.names = F)
write.table(limitacePU, "e:/JJC_VS/Rskripty/vysledky/limitacePU.txt", row.names = F)


########################################
### Trendy v chronologiich
########################################
library(Kendall)

MannKendall(PHom$obs)
MannKendall(PHom$cam)
MannKendall(PUom$obs)
MannKendall(PUom$cam)
MannKendall(JTom$obs)
MannKendall(JTom$cam)
MannKendall(JCom$obs)
MannKendall(JCom$cam)


