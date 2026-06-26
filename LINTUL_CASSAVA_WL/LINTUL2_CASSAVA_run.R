#-------------------------------------------------------------------------------------------------#
# LINTUL2-CASSAVA run script
#
# Author:       Rob van den Beuken
# Adapted:      A.G.T. Schut
# Copyright:    Copyright 2019, PPS
# Email:        tom.schut@wur.nl
# Date:         19-12-2019
#
# Original LINTUL-Cassava: Ezui, K. S. et al. (2018). Simulating drought impact and mitigation in 
# cassava using the LINTUL model. Field Crops Research, 219, 256-272.
# 
# Most of the code is a direct translation by Rob van den Beuken from the FST version 
# as developed by Guillaume Ezui under supervision of Peter Leffelaar.
# Joy Adiele used this original version to calibrate the model to Nigerian conditions.
# For this, the RUE parameter was set to the measured value 
# and LAI was forced using measured data from Edo 2016 to estimate the assimilate partitioning values for the used cultivar.
# 
# AGT Schut checked, simplified and adapted the code to its current format.
# Calibration for Nigeria: 
# Adiele, J. G., A. G. T. Schut, K. S. Ezui, P. Pypers and K. E. Giller (2021). "A recalibrated and tested LINTUL-Cassava simulation model provides insight into the high yield potential of cassava under rainfed conditions " European Journal of Agronomy 124: paper 126242.
#
# This file is used to run the LINTUL2_CASSAVA model and all its related components. 
# Parameters used are from  Adiele et al.
#
#--------------------------------------------------------------------------------------------------#
# BEFORE START
# It is important before running this script to set the working directory to source file location: 
# This is done as follows: Go to 'Session' -> 'Set Working Directory' -> To Source File Location
#--------------------------------------------------------------------------------------------------#

# GENERAL SETTINGS
rm(list=ls())
source('./Components/LINTUL2_CASSAVA_iniSTATES.R')
source("./Components/LINTUL2_CASSAVA_PARAMETERS_EZUI.r")
source('./Components/LINTUL2_CASSAVA_PARAMETERS_ADIELE.R')
source('./Components/LINTUL2_CASSAVA_Help.R')
source('./Components/LINTUL2_CASSAVA_Weather.R')
source('./Components/LINTUL2_CASSAVA_field_management.R')
source('./Components/LINTUL2_CASSAVA.r')	

require('deSolve')              # used for solving ODEs
# install.packages('deSolve')   # uncomment if the deSolve package is not yet installed

#Read in data from the site 
SiteInfo <- read.csv("./Data/Field_characteristics_field_management.csv")

country   <- "nig"

for(year in c(2016, 2017)){
  #-EDO----------------------------------------------------------------------------------------------------#
  wdata <- get_weather('./Weather/', country=country, station='1', year=substr(toString(year),2,4), endtime = 506)
  
  #Use default parameters as defined by Ezui et al. and re-calibrated by Adiele et al. EJA 2021
  pars_iri  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT(SiteInfo = subset(SiteInfo, Location == "Edo" & Year_of_planting == year),
                                               MODEL_PARAM = LINTUL2_CASSAVA_PARAMETERS_ADIELE(irri = TRUE))
  
  Pot_Edo <- LINTUL2_CASSAVA_RUN(wdata = wdata,  pars = pars_iri, year = year, starttime = pars_iri[["DOYPL"]]-100, endtime = pars_iri[["DOYHAR"]])
  write.csv(Pot_Edo, paste0("./Results/LINTUL_CASSAVA_potential_growth_EDO_", year, ".csv"))
  
  pars_rf  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT(SiteInfo = subset(SiteInfo, Location == "Edo" & Year_of_planting == year),
                                               MODEL_PARAM = LINTUL2_CASSAVA_PARAMETERS_ADIELE(irri = FALSE))
  Wlim_Edo <- LINTUL2_CASSAVA_RUN(wdata = wdata,  pars = pars_rf, year = year, starttime = pars_rf[["DOYPL"]]-100, endtime = pars_rf[["DOYHAR"]])
  write.csv(Wlim_Edo,paste0("./Results/LINTUL_CASSAVA_water_limited_growth_EDO_", year, ".csv"))
  
  #-CRS, cross river---------------------------------------------------------------------------------------#
  if(year == 2016){
    wdata <- get_weather('./Weather/', country=country, station='3', year=substr(toString(year),2,4), endtime = 500)
    pars_iri  <-  LINTUL2_CASSAVA_FIELD_MANAGEMENT(SiteInfo = subset(SiteInfo, Location == "Cross River1" & Year_of_planting == year),
                                                   MODEL_PARAM = LINTUL2_CASSAVA_PARAMETERS_ADIELE(irri = TRUE))
    pars_rf  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT(SiteInfo = subset(SiteInfo, Location == "Cross River1" & Year_of_planting == year),
                                                 MODEL_PARAM = LINTUL2_CASSAVA_PARAMETERS_ADIELE(irri = FALSE))
  }else if(year == 2017){
    wdata <- get_weather('./Weather/', country=country, station='4', year=substr(toString(year),2,4), endtime = 500)
    pars_iri  <-  LINTUL2_CASSAVA_FIELD_MANAGEMENT(SiteInfo = subset(SiteInfo, Location == "Cross River2" & Year_of_planting == year),
                                                   MODEL_PARAM = LINTUL2_CASSAVA_PARAMETERS_ADIELE(irri = TRUE))
    pars_rf  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT(SiteInfo = subset(SiteInfo, Location == "Cross River2" & Year_of_planting == year),
                                                 MODEL_PARAM = LINTUL2_CASSAVA_PARAMETERS_ADIELE(irri = FALSE))
  }
  
  Pot_CRS <- LINTUL2_CASSAVA_RUN(wdata = wdata,  pars = pars_iri, year = year, starttime = pars_iri[["DOYPL"]]-100, endtime = pars_iri[["DOYHAR"]])
  write.csv(Pot_CRS,paste0("./Results/LINTUL_CASSAVA_potential_growth_CRS_", year, ".csv"))
  
  
  Wlim_CRS <- LINTUL2_CASSAVA_RUN(wdata = wdata,  pars = pars_rf, year = year, starttime = pars_rf[["DOYPL"]]-100, endtime = pars_rf[["DOYHAR"]])
  write.csv(Wlim_CRS,paste0("./Results/LINTUL_CASSAVA_water_limited_growth_CRS_", year, ".csv"))
  
  #-Benue-------------------------------------------------------------------------------------------------#
  pars_iri  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT(SiteInfo = subset(SiteInfo, Location == "Benue2" & Year_of_planting == year),
                                                      MODEL_PARAM = LINTUL2_CASSAVA_PARAMETERS_ADIELE(irri = TRUE))
  pars_rf  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT(SiteInfo = subset(SiteInfo, Location == "Benue2" & Year_of_planting == year),
                                               MODEL_PARAM = LINTUL2_CASSAVA_PARAMETERS_ADIELE(irri = FALSE))
  wdata <- get_weather('./Weather/', country=country, station='2', year=substr(toString(year),2,4), endtime = pars_iri[["DOYHAR"]])
  
  Pot_Ben <- LINTUL2_CASSAVA_RUN(wdata = wdata,  pars = pars_iri, year = year, starttime = pars_iri[["DOYPL"]]-100, endtime = pars_iri[["DOYHAR"]])
  write.csv(Pot_Ben,paste0("./Results/LINTUL_CASSAVA_potential_growth_BENUE_", year, ".csv"))
  
  
  Wlim_Ben <- LINTUL2_CASSAVA_RUN(wdata = wdata,  pars = pars_rf, year = year, starttime = pars_rf[["DOYPL"]]-100, endtime = pars_rf[["DOYHAR"]])
  write.csv(Wlim_Ben,paste0("./Results/LINTUL_CASSAVA_water_limited_growth_BENUE_", year, ".csv"))
}

#===========================================================================================================
#PLOTTING figures as in the EJA paper
#===========================================================================================================
source("./Components/Adiele et al 2021 EJA Fig1.R")
source("./Components/Adiele et al 2021 EJA Fig2.R")
source("./Components/Adiele et al 2021 EJA Fig3.R")
source("./Components/Adiele et al 2021 EJA Fig4.R")
source("./Components/Adiele et al 2021 EJA Fig5.R")
