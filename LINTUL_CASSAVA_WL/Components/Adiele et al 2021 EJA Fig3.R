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
Fig3.TIF <- "./Figures/Adiele et al. EJA Figure 3.tif"
Fig3.PDF <- "./Figures/Adiele et al. EJA Figure 3.pdf"

#Data on cumulative intercepted PAR, combining measure wheater data and interpolated measurements of LI from AccurPAR data
Bio_cumIPAR <- read.csv("./Data/Bio+cumIPAR_NFPFKF_BenueEdo_Locations+years.csv", header = T)

#Figure 3.
Adiele_etal_EJA_Fig_3 <- function(Bio_cumIPAR){
  
  Bio_cumIPAR$Loc_yr <- factor(Bio_cumIPAR$Loc_yr)
  #par(mfrow=c(1,2))
  ii<-which(Bio_cumIPAR[,"Loc_yr"]=="Edo_2016" )
  plot(Bio_cumIPAR[ii,"Cum_PAR.MJm.2"], Bio_cumIPAR[ii,"BiomassDM.gm.2"], xlab = "Cumulative IPAR (MJ/m²)", ylab = "Biomass (g/m²)",
       cex.lab=1.3, cex = 1.0, pch =1, xlim = c(0,2600), ylim = c(0,9000))
  ii<-which(Bio_cumIPAR[,"Loc_yr"]=="Edo_2017" )
  points(Bio_cumIPAR[ii,"Cum_PAR.MJm.2"], Bio_cumIPAR[ii,"BiomassDM.gm.2"], pch =16, cex = 1.0)
  
  ii<-which(Bio_cumIPAR[,"Loc_yr"]=="Benue_2016" )
  points(Bio_cumIPAR[ii,"Cum_PAR.MJm.2"], Bio_cumIPAR[ii,"BiomassDM.gm.2"], pch =2, cex = 1.0)
  ii<-which(Bio_cumIPAR[,"Loc_yr"]=="Benue_2016" )
  points(Bio_cumIPAR[ii,"Cum_PAR.MJm.2"], Bio_cumIPAR[ii,"BiomassDM.gm.2"], pch =17, cex = 1.0)
  legend('bottomright', c('Edo, 2016', 'Edo, 2017', 'Benue, 2016', 'Benue, 2017'), 
         pch = c(1,16,2,17), bty = 'n')
  
  #c(20,15,1,0)
  ii<-which(Bio_cumIPAR[,"Loc_yr"]=="Edo_2016" )
  ModBE <- lm(BiomassDM.gm.2 ~ -1 + Cum_PAR.MJm.2,Bio_cumIPAR[ii,])
  summary(ModBE)
  anova(ModBE)
  abline(ModBE)
  legend('topleft', c('Edo 2016', 'R² = 0.98, P< .001', 'y = 2.76x'), bty = 'n')
}

tiff(Fig3.TIF, height = 16, width = 16, units = 'cm', 
     compression = "lzw", res = 600)
par(oma=c(2,2,2,2),mar=c(5,5,0,0))
Adiele_etal_EJA_Fig_3(Bio_cumIPAR=Bio_cumIPAR)
dev.off()

pdf(Fig3.PDF)
par(oma=c(2,2,2,2),mar=c(5,5,0,0))
Adiele_etal_EJA_Fig_3(Bio_cumIPAR=Bio_cumIPAR)
dev.off()
