# lets get the variables that we are interested in from LPJ-GUESS:

# We need:
#    1. model -- the name of the model you're interested in; this should correspond to the PREFIX of the model directory
#    2. model.dir -- the file path to where you extracted the compressed model directory; DO NOT MOVE FILES AROUND INSIDE THAT DIRECTORY!
#    3. vars  -- a vector of the variables you wish to extract for each site
#    4. xmin  -- the minimum (western0most) longitude of interest
#    5. xmax  -- the maximum (eastern-most) longitude of interest
#    6. ymin  -- the minimum (southern-most) latitude of interest
#    7. ymax  -- the maximum (northern-most) latitude of interest
#    8. yrmin -- the first year of interest; defaults to 850 (first year of simulations)
#    9. yrmax -- the last year of interest; defaults to 2010 (last year of simulations)
#
# Note: This will by default return the entire time series at the raw time step provided by each model
mod <- "ED2"
#mdir <- "C:/Users/JMac/Documents/Kelly/MIP/WUE_MIP/WUE_MIP/Data/ED2.v1.2016-05-03.tar/ED2.v1.2016-05-03/ED2.v1.2016-05-03/"
mdir <- paste0(getwd(), "/Data/ED2.v1.2016-05-03.tar/ED2.v1.2016-05-03/ED2.v1.2016-05-03/")
#vector of variables
vars <- c("CO2", "NPP", "Dens", "Fire", "PFT", "Fcomp", "GWBI", "tair")
vars <- "Dens"
# bounding box info:
xmin <- -100
xmax <- -70
ymin <- 35
ymax <- 50
yrmin <- 850
yrmax <- 2010

source(paste0(getwd(),'/R/extract_output_region.R')) # for external harddrive
#source('D:/Kelly/WUE_MIP/R/extract_output_region_pft.R') # for external harddrive

ED2.npp <- extract.paleon.site(model = mod, model.dir = mdir, vars = vars, xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
saveRDS(ED2.npp, file = "Data/ED_monthly_npp.RDS")

ED2.gpp <- extract.paleon.site(model = mod, model.dir = mdir, vars = "GPP", xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
saveRDS(ED2.gpp, file = "Data/ED_montly_gpp.RDS")

ED2.transp <- extract.paleon.site(model = mod, model.dir = mdir, vars = "Transp", xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
saveRDS(ED2.transp, file = "Data/ED_montly_transp.RDS")

ED2.evap <- extract.paleon.site(model = mod, model.dir = mdir, vars = "Evap", xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
saveRDS(ED2.evap, file = "Data/ED_montly_evap.RDS")

# split up because we dont have enough memory to extract the whole spatial extent
# look at western density
ED2.denswest <- extract.paleon.site(model = mod, model.dir = mdir, vars = "Dens", xmin=-100, xmax=-90, ymin=35, ymax=48, yrmin=850, yrmax=2010)
saveRDS(ED2.denswest, file = "Data/ED_montly_dens.RDS")



ED2.gwbi <- extract.paleon.site(model = mod, model.dir = mdir, vars = "GWBI", xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
saveRDS(ED2.gwbi, file = "Data/ED_montly_gwbi.RDS")

ED2.pft <- extract.paleon.site(model = mod, model.dir = mdir, vars = "PFT", xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
saveRDS(ED2.pft, file = "Data/ED_montly_pft.RDS")

ED2.Fcomp <- mod.out
ED2.Fcomp <- extract.paleon.site(model = mod, model.dir = mdir, vars = "Fcomp", xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
saveRDS(ED2.Fcomp, file = "Data/ED_montly_Fcomp.RDS")

ED2.Fire <- extract.paleon.site(model = mod, model.dir = mdir, vars = "Fire", xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
saveRDS(ED2.Fire, file = "Data/ED_montly_Fire.RDS")

ED2.tair <- extract.paleon.site(model = mod, model.dir = mdir, vars = "tair", xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
saveRDS(ED2.tair, file = "Data/ED_montly_tair.RDS")

ED2.qair <- extract.paleon.site(model = mod, model.dir = mdir, vars = "qair", xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
saveRDS(ED2.qair, file = "Data/ED_montly_qair.RDS")

ED2.precip <- extract.paleon.site(model = mod, model.dir = mdir, vars = "precipf", xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
saveRDS(ED2.precip, file = paste0(getwd(),"/Data/ED_montly_precip.RDS"))

ED2.soilmoist <- extract.paleon.site(model = mod, model.dir = mdir, vars = "SoilMoist", xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
saveRDS(ED2.soilmoist, file =paste0(getwd(),"/Data/ED_montly_soilmoist.RDS"))

ED2.LAI <- extract.paleon.site(model = mod, model.dir = mdir, vars = "LAI", xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
saveRDS(ED2.LAI, file = "Data/ED_montly_LAI.RDS")

ED2.Estab <- extract.paleon.site(model = mod, model.dir = mdir, vars = "Estab", xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
saveRDS(ED2.Estab, file = "Data/ED_montly_Estab.RDS")

ED2.Mort <- extract.paleon.site(model = mod, model.dir = mdir, vars = "Mort", xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
saveRDS(ED2.Mort, file = "Data/ED_montly_Mort.RDS")

ED2.CO2 <- extract.paleon.site(model = mod, model.dir = mdir, vars = "CO2", xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
saveRDS(ED2.CO2, file = "Data/ED_montly_CO2.RDS")

# the extractions are very slow. I Need to be able to work with the timeseries data

plotlatlon <- function(Dens, lat,lon){
  #Year <- yearno+850
  dens1850 <- ED2.evap$Evap[1,1,]
  #dens1850$evg <- rowSums
  tab <- melt(dens1850)
  
  colnames(tab) <- c( "year", "Dens")
  tab <- merge(tab, pft, by = "pft")
  # plot the density of each PFT for 1850
  ggplot(tab, aes(x = year, y = Dens))+geom_point()+theme_bw()+ggtitle(paste0("Total Density ","lat = ", lat,"lon" =lon))+ 
    facet_wrap(~names) + scale_color_manual(name = "PFT", values = )
  #ggplot(tab, aes(x = lon, y = lat, fill = Dens))+geom_raster()+facet_wrap(~pft)
}
