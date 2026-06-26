##########################################################################################
#Scripts that produce the figures 1 and 1A as shown in the EJA paper
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
Fig1A.TIF <- "./Figures/Adiele et al. EJA Figure 1A.tif"
Fig1A.PDF <- "./Figures/Adiele et al. EJA Figure 1A.pdf"
Fig1.TIF <- "./Figures/Adiele et al. EJA Figure 1.tif"
Fig1.PDF <- "./Figures/Adiele et al. EJA Figure 1.pdf"

#Data measured with an AccuPar below and above canopy
DATA_ACCUPAR <- read.csv("./Data/AccuPAR measurements and LAI and k calculations for 2016+2017.csv", header = T)


#Figure 1A.
Adiele_etal_EJA_Fig_1A <- function(DATA){
  Modk <- lm(DATA$ln_FLI ~ -1 + DATA$LAI)
  
  uLY<-unique(DATA[,"Loc_yr"])
  ii<-DATA[,"Loc_yr"]=="Benue_2017"
  plot(x=DATA[ii,"LAI"],y=DATA[ii,"ln_FLI"],
       cex.lab=1.5, cex = 1.0,
       xlab = 'Leaf area index (m²/m²)',
       ylab = '-ln (1-fLI)',
       xlim = c(0,7), ylim = c(0,5),
       type="p",
       pch=15,col="black")
  ii<-DATA[,"Loc_yr"]=="Benue_2016"
  points(x=DATA[ii,"LAI"],y=DATA[ii,"ln_FLI"], pch=0,col="black",cex=1.0)
  ii<-DATA[,"Loc_yr"]=="Edo_2016"
  points(x=DATA[ii,"LAI"],y=DATA[ii,"ln_FLI"], pch=1,col="black",cex=1.0)
  ii<-DATA[,"Loc_yr"]=="Edo_2017"
  points(x=DATA[ii,"LAI"],y=DATA[ii,"ln_FLI"], pch=16,col="black",cex=1.0)

  legend("bottomright", c("Benue, 2016" ,"Benue, 2017","Edo, 2016", "Edo, 2017"),
         cex=1.0,
         pch = c(0, 15, 1, 16), bty="n") 
  
  print(summary(Modk))
  print(anova(Modk))
  abline(Modk)
  legend('topleft', c('R² = 0.99', 'y = 0.67x'), bty = 'n')
}

tiff(Fig1A.TIF, height = 13, width = 16, units = 'cm', 
     compression = "lzw", res = 600)
  par(oma=c(2,2,2,2),mar=c(5,5,0,0))
  Adiele_etal_EJA_Fig_1A(DATA = DATA_ACCUPAR)
dev.off()

pdf(Fig1A.PDF)
  par(oma=c(2,2,2,2),mar=c(5,5,0,0))
  Adiele_etal_EJA_Fig_1A(DATA = DATA_ACCUPAR)
dev.off()


Adiele_etal_EJA_Fig_1 <- function(DATA){

  Modk <- lm(DATA$ln_FLI ~ -1 + DATA$LAI)
  
  plot(x=DATA[,"LAI"],y=DATA[,"ln_FLI"],
       cex.lab=1.5, cex = 1.0,
       xlab = 'Leaf area index (m²/m²)',
       ylab = '-ln (1-fLI)',
       xlim = c(0,7), ylim = c(0,5),
       type="p",
       pch=1,col="black")

  print(summary(Modk))
  print(anova(Modk))
  abline(Modk)
  legend('topleft', c('R² = 0.99', 'y = 0.67x'), bty = 'n')
}
tiff(Fig1.TIF, height = 13, width = 16, units = 'cm', 
     compression = "lzw", res = 600)
  par(oma=c(2,2,2,2),mar=c(5,5,0,0))
  Adiele_etal_EJA_Fig_1(DATA = DATA_ACCUPAR)
dev.off()

pdf(Fig1.PDF)
  par(oma=c(2,2,2,2),mar=c(5,5,0,0))
  Adiele_etal_EJA_Fig_1(DATA = DATA_ACCUPAR)
dev.off()
