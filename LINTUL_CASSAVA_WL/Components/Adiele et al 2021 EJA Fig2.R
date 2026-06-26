##########################################################################################
#Script that produces figure 2 as shown in the EJA paper
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
Fig2.TIF <- "./Figures/Adiele et al. EJA Figure 2.tif"
Fig2.PDF <- "./Figures/Adiele et al. EJA Figure 2.pdf"

#READ IN THE DATA
DATA <- read.csv("./Data/fPAR selected treatments Benue Edo 2016 and 2017.csv", header = T)

#################################################################

DAP_vs_IPAR<-function(Series1=TRT1,Series2=NULL,Series3=NULL,
                     LOCYR=LOCYR,
                     LEG=TRUE,LEGTEXT=c('NfPfKf', 'NfPfKf', 'NfPfKf', "Observed")){
  
  # leaves
  
  ColS1 <- rgb(red = 0, green = 0, blue = 0, alpha = 1) #black line
  ColS2 <- rgb(red = 1, green = 0, blue = 0, alpha = 0.8)#red line
  ColS3 <- rgb(red = 0, green = 0, blue = 1, alpha = 0.8)#blue line
  
  plot( x    = Series1$DAP, 
        y    = Series1$fPAR, type="l", 
        col  = ColS1, 
        xlab = "Days after planting", 
        ylab = "IPAR, f",
        ylim = c(0,1), 
        xlim=c(0,450),
        lwd  = 1.5,
        lty  = 1,
        cex.lab  = 1.25, #1.0
        cex.axis= 1.1, #0.85
        mgp=c(2,0.5,0))
  text(x=100, y=0.05,labels=LOCYR,cex=1.5)
  arrows(Series1$DAP, Series1$fPAR-Series1$fPAR_sd, 
         Series1$DAP, Series1$fPAR+Series1$fPAR_sd, 
           length=0.05, angle=90, code=3,col = ColS1)
    points(x   = Series1$DAP,
           y   = Series1$fPAR,
           col = ColS1,
           pch = 2)
    colLEG <- ColS1
    colLWD <- 1.5
    colLTY <- 1
    colPCH <- 2

  if(length(Series2)>1){
    lines(x    = Series2$DAP, 
          y    = Series2$fPAR, 
          col  = ColS2, 
          lwd  = 1.5,
          lty  = 2)
    arrows(Series2$DAP+3, Series2$fPAR-Series2$fPAR_sd, 
           Series2$DAP+3, Series2$fPAR+Series2$fPAR_sd, 
           length=0.05, angle=90, code=3,col = ColS2)
    points(x   = Series1$DAP,
           y   = Series1$fPAR,
           col = ColS1,
           pch = 5)
    colLEG <- c(colLEG,ColS2)
    colLWD <- c(colLWD, 1.5)
    colLTY <- c(colLTY, 2)
    colPCH <- c(colPCH, 5)
  }
  if(length(Series3)>1){
      lines(x    = Series3$DAP, 
            y    = Series3$fPAR, 
            col  = ColS3, 
            lwd  = 1.5,
            lty  = 4)
      arrows(Series3$DAP-3, Series3$fPAR-Series3$fPAR_sd, 
             Series3$DAP-3, Series3$fPAR+Series3$fPAR_sd, 
             length=0.05, angle=90, code=3,col = ColS3)
      points(x   = Series3$DAP,
             y   = Series3$fPAR,
             col = ColS3,
             pch = 19)
      colLEG <- c(colLEG,ColS3)
      colLWD <- c(colLWD, 1.5)
      colLTY <- c(colLTY, 4)
      colPCH <- c(colPCH, 19)
    }    
  if(LEG){
    legend("bottomright", legend=LEGTEXT, col = colLEG, lwd = colLWD, lty = colLTY, pch = colPCH, bty = "n")
  }
}

Figure2 <- function(DATA){
  par(mfrow=c(2,2),oma=c(3,3,1,1),mar=c(1.5,1.5,0,0))
  ii<-which(DATA[,"Location"]=="Edo" & DATA[,"Year"]=="2016" & DATA[,"Treatment"]=="NfPfK1")
  ij<-which(DATA[,"Location"]=="Edo" & DATA[,"Year"]=="2016" & DATA[,"Treatment"]=="NfPfKf")
  ik<-which(DATA[,"Location"]=="Edo" & DATA[,"Year"]=="2016" & DATA[,"Treatment"]=="NfPfKfMn")
  DAP_vs_IPAR(Series1=DATA[ii,],Series2=DATA[ij,],Series3=DATA[ik,],
              LOCYR="Edo, 2016",
              LEG=TRUE,LEGTEXT=c('NfPfK240', 'NfPfKf', 'NfPfKfMN'))
  ii<-which(DATA[,"Location"]=="Benue" & DATA[,"Year"]=="2016" & DATA[,"Treatment"]=="NfPfK1")
  ij<-which(DATA[,"Location"]=="Benue" & DATA[,"Year"]=="2016" & DATA[,"Treatment"]=="NfPfKf")
  ik<-which(DATA[,"Location"]=="Benue" & DATA[,"Year"]=="2016" & DATA[,"Treatment"]=="NfPfKfMn")
  DAP_vs_IPAR(Series1=DATA[ii,],Series2=DATA[ij,],Series3=DATA[ik,],
              LOCYR="Benue, 2016",
              LEG=FALSE,LEGTEXT=c('NfPfK240', 'NfPfKf', 'NfPfKfMN'))
  ii<-which(DATA[,"Location"]=="Edo" & DATA[,"Year"]=="2017" & DATA[,"Treatment"]=="NfPfK1")
  ij<-which(DATA[,"Location"]=="Edo" & DATA[,"Year"]=="2017" & DATA[,"Treatment"]=="NfPfKf")
  ik<-which(DATA[,"Location"]=="Edo" & DATA[,"Year"]=="2017" & DATA[,"Treatment"]=="NfPfKfMn")
  DAP_vs_IPAR(Series1=DATA[ii,],Series2=DATA[ij,],Series3=DATA[ik,],
              LOCYR="Edo, 2017",
              LEG=FALSE,LEGTEXT=c('NfPfK240', 'NfPfKf', 'NfPfKfMN'))
  ii<-which(DATA[,"Location"]=="Benue" & DATA[,"Year"]=="2017" & DATA[,"Treatment"]=="NfPfK1")
  ij<-which(DATA[,"Location"]=="Benue" & DATA[,"Year"]=="2017" & DATA[,"Treatment"]=="NfPfKf")
  ik<-which(DATA[,"Location"]=="Benue" & DATA[,"Year"]=="2017" & DATA[,"Treatment"]=="NfPfKfMn")
  DAP_vs_IPAR(Series1=DATA[ii,],Series2=DATA[ij,],Series3=DATA[ik,],
              LOCYR="Benue, 2017",
              LEG=FALSE,LEGTEXT=c('NfPfK240', 'NfPfKf', 'NfPfKfMN'))
  mtext("Days after planting", side = 1, line = 1, outer = TRUE, at = NA,
        adj = NA, padj = NA, cex = 1, col = "black", font = NA)
  
  mtext("IPAR, f", 
        side = 2, line = 1, outer = TRUE, at = 0.25,
        adj = NA, padj = NA, cex = 1, col = "black", font = NA)
  mtext("IPAR, f", 
        side = 2, line = 1, outer = TRUE, at = 0.75,
        adj = NA, padj = NA, cex = 1, col = "black", font = NA)
}

tiff(Fig2.TIF, height = 13, width = 20, units = 'cm', 
       compression = "lzw", res = 600)
  Figure2(DATA)  
dev.off()

pdf(Fig2.PDF)
  Figure2(DATA)  
dev.off()
