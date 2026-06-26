##########################################################################################
#Scripts that produce the figure 4 as shown in the EJA paper
#
#Authors: JG Adiele and AGT Schut
#PPS, December 2020
#
#Adiele, J. G., A. G. T. Schut, K. S. Ezui, P. Pypers and K. E. Giller (2021). 
#   A recalibrated and tested LINTUL-Cassava simulation model provides insight into the high 
#   yield potential of cassava under rainfed conditions. European Journal of Agronomy 124: paper 126242.
#
##########################################################################################

#Names of resulting figures
Fig4.TIF <- "./Figures/Adiele et al. EJA Figure 4.tif"
Fig4.PDF <- "./Figures/Adiele et al. EJA Figure 4.pdf"

#Data output from LINTUL-cASSAVA simulations
#Analysing Simulated and observed yield 
#Read in the data needed
Pot_Edo16  <-read.csv('./Results/LINTUL_CASSAVA_potential_growth_EDO_2016.csv')
Wlim_Edo16 <-read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_EDO_2016.csv')
Pot_CRS16  <-read.csv('./Results/LINTUL_CASSAVA_potential_growth_CRS_2016.csv')
Wlim_CRS16 <-read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_CRS_2016.csv')
Pot_BEN16  <-read.csv('./Results/LINTUL_CASSAVA_potential_growth_BENUE_2016.csv')
Wlim_BEN16 <-read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_BENUE_2016.csv')

Pot_Edo17  <-read.csv('./Results/LINTUL_CASSAVA_potential_growth_EDO_2017.csv')
Wlim_Edo17 <-read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_EDO_2017.csv')
Pot_CRS17  <-read.csv('./Results/LINTUL_CASSAVA_potential_growth_CRS_2017.csv')
Wlim_CRS17 <-read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_CRS_2017.csv')
Pot_BEN17  <-read.csv('./Results/LINTUL_CASSAVA_potential_growth_BENUE_2017.csv')
Wlim_BEN17 <-read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_BENUE_2017.csv')

BIOMASS    <- read.csv('./Data/Biomass_NPKuptake_plantpart_totals_per_treatment.csv', header = T) #plant parts g DM/m2
BIOM_Edo16 <- BIOMASS[which(BIOMASS$Location == 'Edo' & BIOMASS$Year == 2016 & BIOMASS$Treatment == "NfPfKf"),]
BIOM_Edo17 <- BIOMASS[which(BIOMASS$Location == 'Edo' & BIOMASS$Year == 2017 & BIOMASS$Treatment == "NfPfKf"),]
BIOM_BEN16 <- BIOMASS[which(BIOMASS$Location == 'Benue' & BIOMASS$Year == 2016 & BIOMASS$Treatment == "NfPfKf"),]
BIOM_BEN17 <- BIOMASS[which(BIOMASS$Location == 'Benue' & BIOMASS$Year == 2017 & BIOMASS$Treatment == "NfPfKf"),]
BIOM_CRS16 <- BIOMASS[which(BIOMASS$Location == 'CRS' & BIOMASS$Year == 2016 & BIOMASS$Treatment == "NfPfKf"),]
BIOM_CRS17 <- BIOMASS[which(BIOMASS$Location == 'CRS' & BIOMASS$Year == 2017 & BIOMASS$Treatment == "NfPfKf"),]

LeavesStemRootDynamics<-function(Series1=Potential,
                                 Series2=NULL,
                                 BIOMASS=NA,TITLE=title,
                                 LEG=TRUE,LEGTEXT=c('Potential', 'Water lim.', 'Nutrient lim.', "Observed")){
  
  # leaves
  
  ColS1 <- rgb(red = 0, green = 0, blue = 0, alpha = 1) #black line
  ColS2 <- rgb(red = 1, green = 0, blue = 0, alpha = 0.8)#red line
  colBIOM <- "black" #'#22BB22'
  
  plot( x    = Series1$time, 
        y    = Series1$WLVG, type="l", 
        col  = ColS1, 
        xlab = "time (days)", 
        ylab = expression('Green leaves, g DM m'^-2),
        ylim = c(0,500), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1.25, #1.0
        cex.axis= 1.1, #0.85
        mgp=c(2,0.5,0))
  text(x=median(Series1$time), y=490,labels=TITLE,cex=1.5)
  colLEG <- ColS1
  colLWD <- 2
  colLTY <- 1
  colPCH <- NA
  if(length(Series2)>1){
    lines(x    = Series2$time, 
          y    = Series2$WLVG, 
          col  = ColS2, 
          lwd  = 2,
          lty  = 2)
    colLEG <- c(colLEG,ColS2)
    colLWD <- c(colLWD, 2)
    colLTY <- c(colLTY, 2)
    colPCH <- c(colPCH, NA)
  }
  if(length(BIOMASS)>1){
    arrows(BIOMASS$time, BIOMASS$DMLeaves.g.DM.m2-BIOMASS$std_DMLeaves.g.DM.m2, 
           BIOMASS$time, BIOMASS$DMLeaves.g.DM.m2+BIOMASS$std_DMLeaves.g.DM.m2, 
           length=0.05, angle=90, code=3)
    points(x   = BIOMASS$time,
           y   = BIOMASS$DMLeaves.g.DM.m2,
           col = colBIOM,
           lwd = 2.5,
           pch = 19)
    colLEG <- c(colLEG,colBIOM)
    colLWD <- c(colLWD, 2.5)
    colLTY <- c(colLTY, NA)
    colPCH <- c(colPCH, 19)
  }
  
  ##############################################
  
  #STEMS
  
  #par(mfrow=c(1,1), oma = c(4,1,1,1), mar = c(0.6,5,0,0))
  plot( x    = Series1$time, 
        y    = Series1$WST, type="l", 
        col  = ColS1, 
        xlab = "time (days)", 
        ylab = expression('Stems, g DM m'^-2),
        ylim = c(0,2000), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1.25, #1.0
        cex.axis= 1.1, #0.85
        mgp=c(2,0.5,0))
  
  if(length(Series2)>1){
    lines(x    = Series2$time, 
          y    = Series2$WST, 
          col  = ColS2, 
          lwd  = 2,
          lty  = 2)
  }
if(length(BIOMASS)>1){
    arrows(BIOMASS$time, BIOMASS$DMstems.g.DM.m2-BIOMASS$std_DMstems.g.DM.m2, 
           BIOMASS$time, BIOMASS$DMstems.g.DM.m2+BIOMASS$std_DMstems.g.DM.m2, 
           length=0.05, angle=90, code=3)
    points(x   = BIOMASS$time,
           y   = BIOMASS$DMstems.g.DM.m2,
           col = colBIOM,
           lwd = 2.5,
           pch = 19)
  }
  if(LEG){
    leg_x <- 150   # higher is more to right
    leg_y <- 1100  # higher is more to upper
    legend("topleft", legend=LEGTEXT, col = colLEG, lwd = colLWD, lty = colLTY, pch = colPCH, bty = "n")
  }
  
  ###################################################################
  
  #Storage roots
  
  plot( x    = Series1$time, 
        y    = Series1$WSO, type="l", 
        col  = ColS1, 
        xlab = "time (days)", 
        ylab = expression('Storage roots, g DM m'^-2),
        ylim = c(0,4000), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1.25, #1.0
        cex.axis= 1.1, #0.85
        mgp=c(2,0.5,0))
  if(length(Series2)>1){
    lines(x    = Series2$time, 
          y    = Series2$WSO, 
          col  = ColS2, 
          lwd  = 2,
          lty  = 2)
  }
  if(length(BIOMASS)>1){
    arrows(BIOMASS$time, BIOMASS$DMRoots.g.DM.m2 - BIOMASS$std_DMRoots.g.DM.m2, 
           BIOMASS$time, BIOMASS$DMRoots.g.DM.m2 + BIOMASS$std_DMRoots.g.DM.m2, 
           length=0.05, angle=90, code=3)
    points(x   = BIOMASS$time,
           y   = BIOMASS$DMRoots.g.DM.m2,
           col = colBIOM,
           lwd = 2.5,
           pch = 19)
  }
}




Figure4 <- function(){
  par(mfcol=c(3,5),oma=c(3,3,1,1),mar=c(1.5,1.5,0,0))
  LeavesStemRootDynamics(Series1=Pot_Edo17,
                         Series2=Wlim_Edo17,
                         BIOMASS=BIOM_Edo17,TITLE="Edo '17",
                         LEG=TRUE,LEGTEXT=c('Potential', 'Water limited',"Observed"))
  LeavesStemRootDynamics(Series1=Pot_CRS16,
                         Series2=Wlim_CRS16,
                         BIOMASS=BIOM_CRS16,TITLE="CRS '16",
                         LEG=FALSE,LEGTEXT=c('Potential', 'Water lim.',"Observed"))
  LeavesStemRootDynamics(Series1=Pot_CRS17,
                         Series2=Wlim_CRS17,
                         BIOMASS=BIOM_CRS17,TITLE="CRS '17",
                         LEG=FALSE,LEGTEXT=c('Potential', 'Water lim.',"Observed"))
  LeavesStemRootDynamics(Series1=Pot_BEN16,
                         Series2=Wlim_BEN16,
                         BIOMASS=BIOM_BEN16,TITLE="BEN '16",
                         LEG=FALSE,LEGTEXT=c('Potential', 'Water lim.',"Observed"))
  LeavesStemRootDynamics(Series1=Pot_BEN17,
                         Series2=Wlim_BEN17,
                         BIOMASS=BIOM_BEN17,TITLE="BEN '17",
                         LEG=FALSE,LEGTEXT=c('Potential', 'Water lim.',"Observed"))
  
  mtext("Days after planting", side = 1, line = 1, outer = TRUE, at = NA,
        adj = NA, padj = NA, cex = 1, col = "black", font = NA)
  
  mtext("Roots, gDM/m2", 
        side = 2, line = 1, outer = TRUE, at = 0.2,
        adj = NA, padj = NA, cex = 1, col = "black", font = NA)
  mtext("Stems, gDM/m2", 
        side = 2, line = 1, outer = TRUE, at = 0.52,
        adj = NA, padj = NA, cex = 1, col = "black", font = NA)
  mtext("Leaves, gDM/m2 ", 
        side = 2, line = 1, outer = TRUE, at = 0.86,
        adj = NA, padj = NA, cex = 1, col = "black", font = NA)
}

tiff(Fig4.TIF, height = 13, width = 24, units = 'cm', 
     compression = "lzw", res = 600)
  Figure4()
dev.off()

pdf(Fig4.PDF)
  Figure4()
dev.off()