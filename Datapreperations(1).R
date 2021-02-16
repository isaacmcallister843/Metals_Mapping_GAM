#Datapreperation 

#------------------------------------ Packages
setwd("C:/Users/dmcallister/Desktop/LitReview/Data")
library(carData)
library(car)
library(MASS)
library(ppcor)
library(zoo)
library(lmtest)
library(tidyverse)
#For plots
library(ggplot2)
library(ggpubr)
#For spatial 

library (ncf)
library (ade4)
library (gstat)
library (geoR)
library (sp)
library (Hmisc)
library (nlme)
library(mgcv)

#AIC Packages
library(bbmle)
library(lme4)

#For raster
library(sp)
library(raster)
library(spatialEco)
library(rgdal)
library(tiff)
library(ecospat)
library(biomod2)
library(raster)
library(ctmcmove)
library(fields)
library(rasterVis)
#other
library(installr)

#--------------------------------------------------------- West min soils
west <- read.csv("filename1.csv",na.strings = c(""))
#Digital Elecvation moidel 
DEM <- raster("DEM.tif")
DEM <- projectRaster(DEM, crs="+init=epsg:26909")

#Adding elevations data
west_slope1 <- read.csv("filename2.csv")
west_slope2 <- read.csv("filename2.csv")

west_slope <- data.frame(west_slope1,west_slope2)

west_slope[is.na(west_slope$slope.1),]$slope.1 <- west_slope[is.na(west_slope$slope.1),]$slope
west_slope[is.na(west_slope$aspect.1),]$aspect.1 <- west_slope[is.na(west_slope$aspect.1),]$aspect

west_slope <- west_slope[,c(5,6)]

#Setting up the df
west <- west[,c(2:4,6:10,12,22,ncol(west))]

west <- cbind(west,west_slope)

colnames(west) <- c("sample_id", "e", "n", "Au", "Ag", "Cu", "Pb", "Zn", "grid","SUB","elev", "slope","aspect")

west <- west[,c(1,9,10,11,12,13,2,3,4,5,6,7,8)]

for(i in 4:ncol(west)){
  west[,i] <- as.numeric(west[,i])
}

#There are significant outliers in all metals some sites have 10k + mg/kg, which is probably a sampling
#error. So we will apply heavy outlier anaylis
#There as more significant issues in this data
## Most metals are normally distributed but has significant tails. i.e pb spikes around
##                            an accepted mean but contains values of up to 40k mg/kg. Which is basically just a lead pipe

#-------------------------- Filtering

#ix this use sent liteture. 
#BOX PLOT VALUES 

west_mod <- west[west$Ag < boxplot.stats(west$Ag)$stats[5] &
                 west$Pb < boxplot.stats(west$Pb)$stats[5] &
                 west$Zn < boxplot.stats(west$Zn)$stats[5] &
                 west$Au < boxplot.stats(west$Au)$stats[5] &
                 west$Cu < boxplot.stats(west$Cu)$stats[5],]

#Reseting dataframe
rownames(west_mod) <- c(1:nrow(west_mod))


#------------------------------------ Fixing NA
#Now that the key is prepared we go about subbing the NA values

for(i in 1:nrow(west_mod)){
  if(is.na(west_mod$SUB[i]) == TRUE){
    valmin = 10^10
    correction = 1
    for(j in 1:nrow(key_mod)){
        x1 <- west_mod$e[i]
        y1 <- west_mod$n[i]
        x2 <- key_mod$e[j]
        y2 <- key_mod$n[j]
        
        val <- ((x1-x2)^2+(y1-y2)^2)^.5
        
        if(val<valmin){
          valmin = val
          correction = j 
        }
      }
    west_mod$SUB[i] <- key_mod$SURFM_1[correction]
  }
}

rm(i,j,x1,x2,y1,y2,val,valmin,correction, modify,key,a,ph,)
