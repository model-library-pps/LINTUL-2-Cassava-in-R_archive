##########################################################################################
#Scripts that produce the figures 5 as shown in the EJA paper
#
#Authors: JG Adiele and AGT Schut
#PPS, December 2020
#
#Adiele, J. G., A. G. T. Schut, K. S. Ezui, P. Pypers and K. E. Giller (2021). 
#   A recalibrated and tested LINTUL-Cassava simulation model provides insight into the high 
#   yield potential of cassava under rainfed conditions. European Journal of Agronomy 124: paper 126242.
#
##########################################################################################

##################################################################
#Ploting simulated versus observed storage root yield            #
#NfPfKf for all years in Benue, Cross River and Edo  (Fig. 5 in thesis chapter 3)            #
##################################################################

#Names of resulting figures
Fig5.TIF <- "./Figures/Adiele et al. EJA Figure 5.tif"
Fig5.PDF <- "./Figures/Adiele et al. EJA Figure 5.pdf"

#Get the data
MEASURED_DATA <- read.csv("./Data/Biomass_NPKuptake_plantpart_totals_per_treatment.csv", header = T)
MEASURED_DATA <- subset(MEASURED_DATA,Treatment == "NfPfKf", select=c("Year","Location","time","DMRoots.g.DM.m2"))
MEASURED_DATA[,"DMRoots.t.ha"]<-MEASURED_DATA[,"DMRoots.g.DM.m2"]*1E4*1E-6

#Get the sumulated results
BEN16 <- data.frame(Location="Benue",subset(read.csv("./Results/LINTUL_CASSAVA_water_limited_growth_BENUE_2016.csv"),select=c("time","WSOTHA")))
BEN17 <- data.frame(Location="Benue",subset(read.csv("./Results/LINTUL_CASSAVA_water_limited_growth_BENUE_2017.csv"),select=c("time","WSOTHA")))
CRS16 <- data.frame(Location="CRS",subset(read.csv("./Results/LINTUL_CASSAVA_water_limited_growth_CRS_2016.csv"),select=c("time","WSOTHA")))
CRS17 <- data.frame(Location="CRS",subset(read.csv("./Results/LINTUL_CASSAVA_water_limited_growth_CRS_2017.csv"),select=c("time","WSOTHA")))
EDO17 <- data.frame(Location="Edo",subset(read.csv("./Results/LINTUL_CASSAVA_water_limited_growth_EDO_2017.csv"),select=c("time","WSOTHA")))
#Add sowing year
BEN16[,"Year"]<-2016
CRS16[,"Year"]<-2016
BEN17[,"Year"]<-2017
CRS17[,"Year"]<-2017
EDO17[,"Year"]<-2017
MODELLED <- rbind(BEN16, BEN17, CRS16, CRS17, EDO17)

COMBINED_DATA=NULL
for(year in 2016:2017){
  for(loc in c("Edo","Benue","CRS")){
    ii<-which(MEASURED_DATA[,"Location"] == loc & MEASURED_DATA[,"Year"] == year)
    jj<-which(MODELLED[,"Location"] == loc & MODELLED[,"Year"] == year)
    combdata<-merge(MEASURED_DATA[ii,],MODELLED[jj,],by="time", suffixes=c(".measured",".modelled"))
    COMBINED_DATA = rbind(COMBINED_DATA, combdata)
  }
}


fit <- lm(WSOTHA ~ -1 + DMRoots.t.ha, COMBINED_DATA)
sfit = summary(fit)
RMSE=sqrt(mean(sfit$residuals^2))
anova(fit)
print(paste0("Fitted linear regression. Slope =", sfit$coefficients[1], ", R2= ",sfit$r.squared ,", RMSE =", RMSE))

Figure_5 <- function(COMBINED_DATA){
  ii<-which(COMBINED_DATA[,"Year.measured"]==2017 & COMBINED_DATA[,"Location.measured"]=="Benue")
  plot(x=COMBINED_DATA[ii,"DMRoots.t.ha"],y=COMBINED_DATA[ii,"WSOTHA"],
       xlim = c(0,40), ylim = c(0,40),
       cex.lab=1.5, cex = 1.5,
       xlab = 'Observed storage roots (t DM/ha)',
       ylab = 'Simulated storage roots (t DM/ha)',
       type="p",
       pch=15,col="black")
  ii<-which(COMBINED_DATA[,"Year.measured"]==2016 & COMBINED_DATA[,"Location.measured"]=="Benue")
  points( x=COMBINED_DATA[ii,"DMRoots.t.ha"],y=COMBINED_DATA[ii,"WSOTHA"], pch=0,col="black",cex=1.5)
  ii<-which(COMBINED_DATA[,"Year.measured"]==2017 & COMBINED_DATA[,"Location.measured"]=="Edo")
  points( x=COMBINED_DATA[ii,"DMRoots.t.ha"],y=COMBINED_DATA[ii,"WSOTHA"], pch=16,col="black",cex=1.5)
  ii<-which(COMBINED_DATA[,"Year.measured"]==2016 & COMBINED_DATA[,"Location.measured"]=="CRS")
  points( x=COMBINED_DATA[ii,"DMRoots.t.ha"],y=COMBINED_DATA[ii,"WSOTHA"], pch=2,col="black",cex=1.5)
  ii<-which(COMBINED_DATA[,"Year.measured"]==2017 & COMBINED_DATA[,"Location.measured"]=="CRS")
  points( x=COMBINED_DATA[ii,"DMRoots.t.ha"],y=COMBINED_DATA[ii,"WSOTHA"], pch=17,col="black",cex=1.5)
  lines(c(-100,100),c(-100,100),col="black",lty=1,lwd=2)
  lines(c(-100,100),c(-100,100)*sfit$coefficients[1],col="red",lty=3,lwd=3)
  
  legend("bottomright", c("Edo, 2017", "CRS, 2016", "CRS, 2017", "Benue, 2016" ,"Benue, 2017"),
         cex=1.2,
         pch = c(16, 2,17,0,15), bty="n") 
  #text(x=10,y=35,labels=paste0("slope = ",round(sfit$coefficients[1],2),", RMSE=",round(RMSE,2),", R²=",round(sfit$r.squared,2)))
  legend('topleft', c( paste0('y = ',round(sfit$coefficients[1],2),'x'),
                       paste0('R² = ', round(sfit$r.squared,2)),
                       paste0('RMSE = ',round(RMSE,2))), 
         cex=1.2, bty = 'n')
}

tiff(Fig5.TIF, height = 15, width = 15, units = 'cm', 
     compression = "lzw", res = 600)
  Figure_5(COMBINED_DATA)
dev.off()

pdf(Fig5.PDF)
  Figure_5(COMBINED_DATA)
dev.off()

