library(raster)
library(plyr)
library(ggplot2)
library(reshape2)
# load in the lat lon of all the sites
load("Data/PalEON_siteInfo_all.RData")

# function read in model outputs that were extracted
ED2.CO2 <- readRDS(file = "Data/ED2/ED2.CO2.rds")
ED2.GPP <- readRDS(file = "Data/ED2/ED2.GPP.rds")
ED2.gwbi <- readRDS(file = "Data/ED2/ED2.GWBI.rds")


colnames(ED2.CO2) <- paleon$latlon


plot(ED2.CO2[,1], ED2.GPP[,1])
plot(ED2.CO2[,1], ED2.gwbi[,1])

ed.co2<- ED2.CO2$CO2
Year <- yearno+850

yr <- "850"
dens1850 <- ed.co2[,,yr]
tab <- melt(dens1850)

colnames(tab) <- c("pft", "lon", "lat", "Dens")

#test <- adply(ED2.CO2$CO2, c(1,2,3))
plot(ED2.GPP$GPP, ED2.CO2$CO2)
plot(ED2.gwbi$GWBI, ED2.CO2$CO2)
