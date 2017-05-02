library(raster)
library(plyr)
library(ggplot2)
library(reshape2)
# read in model outputs that were extracted

ED2.CO2 <- readRDS(file = "Data/ED_montly_CO2.RDS")
ED2.GPP <- readRDS(file = "Data/ED_montly_gpp.RDS")
ED2.gwbi <- readRDS(file = "Data/ED_montly_gwbi.RDS")
#test<- raster(ED2.CO2)

ed.co2<- ED2.CO2$CO2
Year <- yearno+850

yr <- "850"
dens1850 <- ed.co2[,,yr]
tab <- melt(dens1850)

colnames(tab) <- c("pft", "lon", "lat", "Dens")

#test <- adply(ED2.CO2$CO2, c(1,2,3))
plot(ED2.GPP$GPP, ED2.CO2$CO2)
plot(ED2.gwbi$GWBI, ED2.CO2$CO2)
