# This script will look at sensitivity of WUE to precipitation, CO2, and tair
# Author: Kelly Heilman
library(reshape2)
library(ggplot2)
library(tidyr)

# load the necessary data:
IWUE <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_IWUE.RDS"))
WUEi <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_WUEi.RDS"))
WUEt <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_WUEt.RDS"))
CO2 <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_CO2.RDS"))
precip <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_precip.RDS"))
tair <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_tair.RDS"))
gwbi <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_GWBI.RDS"))
lai <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_lai.RDS"))

# load the pft specific data:
dens <- readRDS("Data/ED_monthly_Dens_nona.RDS")
Fcomp <- readRDS("Data/ED_monthly_Fcomp_nona.RDS")



# try unlist ot convert Fcomp to a df
#df <- data.frame(matrix(unlist(Fcomp), nrow=13932, byrow=T),stringsAsFactors=FALSE)
#test <- do.call(rbind, lapply(Fcomp, data.frame, stringsAsFactors=FALSE))

#convert list to array
Fcomp <- Fcomp$Fcomp
dens <- dens$Dens

# because the data was in an array, we have alot of lat longs with no values--can we get rid of these?
Fcomp[,,,]
Fcomp[,,"850",pfts]

# plot pfts that occurred in ED runs:
pft.list <- dimnames(Fcomp)$pft

# reduce to the actual pfts present:
pfts <- c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   "temp.decid.late", "grass.c3.temp" )
Fcomp.r <- Fcomp[,,,pfts]
Dens.r <- dens[,,,pfts]

# extract only the lats
Fcompnona <- which(!is.na(Fcomp.r))
# as.data.frame.table converts array s really quickly
df0 <- as.data.frame.table(Fcomp.r)
Fcomp.df <- df0[!is.na(df0$Freq),]
colnames(Fcomp.df) <- c("lat", 'lon', 'time', 'pft', 'Fcomp')

# get rid of the factores
Fcomp.df$lat <- as.numeric(as.character(Fcomp.df$lat))
Fcomp.df$lon <- as.numeric(as.character(Fcomp.df$lon))
Fcomp.df$time <- as.numeric(as.character(Fcomp.df$time))
Fcomp.df$pft <- as.character(Fcomp.df$pft)

saveRDS(Fcomp.df, paste0(getwd(), "/Data/extracted/Ed2_fcomp.df")) # save

#-------------------- do the same conversion for density:
df1 <- as.data.frame.table(Dens.r, fun = sum) # since there should only be 1 dens value this should be okay
#df1 <- array2df(Dens.r)
#df2 <- adply(Dens.r)
Dens.df <- df1[!is.na(df1$Freq),]
colnames(Dens.df) <- c("lat", 'lon', 'time', 'pft', 'Dens')

# get rid of the factores
Dens.df$lat <- as.numeric(as.character(Dens.df$lat))
Dens.df$lon <- as.numeric(as.character(Dens.df$lon))
Dens.df$time <- as.numeric(as.character(Dens.df$time))
Dens.df$pft <- as.character(Dens.df$pft)

saveRDS(Dens.df, paste0(getwd(), "/Data/extracted/Ed2_dens.df")) # save



# create a column IDentifier
datain <- read.csv("Data/ED_site_list_lat_lon.csv")
Fcomp.df <- merge(Fcomp.df, datain[,c('lat','lon','ID','site.name')], by = c('lat', 'lon'))
Dens.df <- merge(Dens.df, datain[,c('lat','lon','ID','site.name')], by = c('lat', 'lon'))



#linecolor <- c('Earlypine' = "red", 'lateconifer'="orange", 'earlydeciduous'="blue",
 #              'middeciduous' ="grey", 'latedeciduous'='forestgreen')



#---------------------- make a plot of fcomp at each site-------------------
sites <- unique(Fcomp.df$site.name)
Fcomp.df$site.name <- as.character(Fcomp.df$site.name) # make site name a character
Fcomp.df <- Fcomp.df[order(Fcomp.df$ID),]


png(height = 13, width = 13, units = 'in', res= 100, "outputs/preliminaryplots/ED_Fcomp_yearly_timeseries.png")
ggplot(Fcomp.df, aes(x = time, y = Fcomp, colour = pft)) + geom_line()+theme_bw()+facet_wrap(~site.name, ncol=5)
dev.off()

#------------------ plot density of each pft at each site--------------------------


Dens.df$site.name <- as.character(Dens.df$site.name) # make site name a character
Dens.df <- Dens.df[order(Dens.df$ID),]

png(height = 13, width = 13, units = 'in', res= 200, "outputs/preliminaryplots/ED_Dens_yearly_timeseries.png")
ggplot(Dens.df, aes(x = time, y = Dens, colour = pft)) + geom_line()+theme_bw()+facet_wrap(~site.name, ncol=5)
dev.off()

# note: density values seem very high--are these values really stems per hectare?


#---------------Plotting non-pft variables:
# preliminary crappy plots for 1 site
plot( CO2[,2], IWUE[,2] )
plot( precip[,2], IWUE[,2] )
plot( tair[,2], IWUE[,2] )
plot( gwbi[,2], IWUE[,2] )
plot( lai[,2], IWUE[,2] )


#-------------------get dataframes aggregated to yearly timestep---------

# we may need to rethink this function, but it is a start
get.yrmeans <- function(df, var){
  m <- melt(df, id.vars=c("Year", "Month", "mo"))
  yrmeans<-dcast(m, Year ~ variable, mean, na.rm=TRUE)
  m2 <- melt(yrmeans, id.vars= "Year")
  m2$Year <- as.numeric(m2$Year)
  
  #yrmeans
  colnames(m2) <- c("Year", "Site", var)
  m2$Site <- as.character(m2$Site)
  m2
}

IWUE.y <- get.yrmeans(IWUE, "IWUE")
WUEi.y <- get.yrmeans(WUEi, "WUEi")
WUEt.y <- get.yrmeans(WUEt, "WUEt")
precip.y <- get.yrmeans(precip, "precip")
CO2.y <- get.yrmeans(CO2, "CO2")
lai.y <- get.yrmeans(lai, "LAI")
gwbi.y <- get.yrmeans(gwbi, "GWBI")
tair.y <- get.yrmeans(tair, "Tair")

# use reduce to merge all these into one dataframe
all.y <- Reduce(function(x, y) merge(x, y, by = ,all=TRUE), list(IWUE.y, WUEi.y, WUEt.y, CO2.y,
                                                       gwbi.y,tair.y, precip.y, lai.y))


# write for future use
saveRDS(all.y, paste0(getwd(), "/Data/extracted/ED_yearly_allnonpft.RDS"))


# make plots of variables by sites

plot.sens <- function(df, xname, yname){
  df <- df[,c("Year", "Site", xname, yname)]
  colnames(df) <- c("Year", "Site", "x", "y")
  lim <- quantile(df$x, .99) # so we dont plot the outliers
  
  png(height = 12, width = 12, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/sensitivity/ED2_", xname,"_", yname,"_yr_sens_site.png"))
  print(ggplot(data = df, aes(x = x, y = y, color = Site))+geom_point()+xlim(0,lim)+
       ylab(yname)+ xlab(xname)+stat_smooth(color = "black")+facet_wrap(~Site, ncol = 5)+theme_bw())
  dev.off()
}

# for WUEt
plot.sens(all.y, "WUEt", "CO2")
plot.sens(all.y, "Year", "CO2")
plot.sens(all.y, "Year", "CO2")
plot.sens(all.y, "WUEt", "GWBI")
plot.sens(all.y, "WUEt", "Tair")
plot.sens(all.y, "WUEt", "precip")
plot.sens(all.y, "WUEt", "LAI")
# for WUEi
plot.sens(all.y, "WUEi", "CO2")
plot.sens(all.y, "WUEi", "GWBI")
plot.sens(all.y, "WUEi", "Tair")
plot.sens(all.y, "WUEi", "precip")
plot.sens(all.y, "WUEi", "LAI")
#for IWUE
plot.sens(all.y, "IWUE", "CO2")
plot.sens(all.y, "IWUE", "GWBI")
plot.sens(all.y, "IWUE", "Tair")
plot.sens(all.y, "IWUE", "precip")
plot.sens(all.y, "IWUE", "LAI")

#sensitivty of other parameters:
plot.sens(all.y, "GWBI", "CO2")
plot.sens(all.y, "GWBI", "IWUE")
plot.sens(all.y, "GWBI", "Tair")
plot.sens(all.y, "GWBI", "precip")
plot.sens(all.y, "GWBI", "LAI")

#------------------Plot sensitivty of WUE against pft-specific variables----------------
fcomp.pft<- ddply(Fcomp.df, .variables = c("lat", 'lon', 'time','ID', 'pft', 'site.name'), FUN = sum, value.var = "Fcomp")

fcomp.wide <- spread(Fcomp.df, pft, Fcomp) # uses tidyr
colnames(fcomp.wide) <- c("lat" , "lon", "Year", "ID","Site" ,"conifer.late",  "grass.c3.temp","pine.north","temp.decid.early","temp.decid.late","temp.decid.mid") 

  
dens.wide <- spread(Dens.df, pft, Dens) # uses tidyr
dens.wide$total <- rowSums(dens.wide[,6:11], na.rm=TRUE)
colnames(dens.wide) <- c("lat" , "lon", "Year", "ID","Site" ,"conifer.late",  "grass.c3.temp","pine.north","temp.decid.early","temp.decid.late","temp.decid.mid","total.density") 


dens.all <- merge(dens.wide, all.y, by=c("Year", "Site"))
fcomp.all <- merge(fcomp.wide, all.y, by = c("Year", "Site"))

saveRDS(fcomp.all, paste0(getwd(), "/Data/extracted/ED2_full_fcomp.RDS"))
saveRDS(dens.all, paste0(getwd(), "/Data/extracted/ED2_full_dens.RDS"))

#-------------------density histograms-----------------------------------

hist(dens.all$pine.north, breaks = 50)
hist(dens.all$conifer.late, breaks = 50)
hist(dens.all$temp.decid.early, breaks = 50)
hist(dens.all$temp.decid.late, breaks = 50)
hist(dens.all$temp.decid.mid, breaks = 50)
hist(dens.all$total.density, breaks = 50)

#-------------------make preliminary plots of density vs WUE-----------------------

plot(dens.all$WUEi, dens.all$total.density)
plot(dens.all$WUEt, dens.all$total.density)
plot(dens.all$IWUE, dens.all$total.density)

plot.sens(dens.all, "total.density", "WUEi")
plot.sens(dens.all, "total.density", "WUEt")
plot.sens(dens.all, "total.density", "IWUE")
plot.sens(dens.all, "CO2", "total.density")
plot.sens(dens.all, "precip", "total.density")
plot.sens(dens.all, "total.density", "GWBI")

plot.sens(dens.all, "temp.decid.early", "WUEi")
plot.sens(dens.all, "temp.decid.mid", "WUEi")
plot.sens(dens.all, "temp.decid.late", "WUEi")
plot.sens(dens.all, "pine.north", "WUEi")
plot.sens(dens.all, "conifer.late", "WUEi")
plot.sens(dens.all, "precip", "total.density")
plot.sens(dens.all, "total.density", "GWBI")
plot.sens(dens.all, "total.density", "LAI")
plot.sens(dens.all, "total.density", "Tair")

# Look at BA in ED
# how is fire related to density in the models?
# fire-density relationship through time?

# ----------------------Fcomp vs WUE and other params---------------
# need to fix this! this will name over the density plots!!
plot.sens(fcomp.all, "temp.decid.early", "WUEi")
plot.sens(fcomp.all, "temp.decid.mid", "WUEi")
plot.sens(fcomp.all, "temp.decid.late", "WUEi")
plot.sens(fcomp.all, "pine.north", "WUEi")
plot.sens(fcomp.all, "conifer.late", "WUEi")


plot.sens(fcomp.all, "temp.decid.early", "WUEi")
plot.sens(fcomp.all, "temp.decid.mid", "WUEi")
plot.sens(fcomp.all, "temp.decid.late", "WUEi")
plot.sens(densfcomp.all, "pine.north", "WUEi")
plot.sens(fcomp.all, "conifer.late", "WUEi")


#-----------------Model Sensitivity of WUE-----------------

require(mgcv)

# there are a couple of "inf" values for some of the WUE. Remove them here:
all.clean <- do.call(data.frame,lapply(all.y, function(x) replace(x, is.infinite(x),NA)))

testlm <- bam(WUEi ~ s(precip) + s(Tair) +s(CO2), data = all.clean)

summary(testlm) # 31.3% of variance

plot(testlm, select=1)
plot(testlm, select=2)
plot(testlm, select=3)


gwbi.mod <- bam(GWBI ~ s(precip) + s(Tair) + s(CO2) + s(WUEi), data = all.clean)

summary(gwbi.mod) # 31.3% of variance

plot(gwbi.mod, select=1)
plot(gwbi.mod, select=2)
plot(gwbi.mod, select=3)
plot(gwbi.mod, select=4)

# check out gavin simpsons blog to generate ci for gams and modeling through time:
#http://www.fromthebottomoftheheap.net/2011/06/12/additive-modelling-and-the-hadcrut3v-global-mean-temperature-series/

lai.mod <- bam(LAI ~ s(WUEi), data = all.clean)
summary(lai.mod)
plot(lai.mod)

#-----------------------------------------------
# We want to model:
# 1. WUE ~ temp, precip, CO2
# 2. GWBI ~ temp, precip, WUE (and co2)
# 3. Density ~ temp, precip, wUE (with PFT effect)
# 4. fcomp ~ temp, precip, wue (with pft effects)

# but we also want to know how the effects of temp, precip, etc changes over time?
# potentially use some of the tools idenified on the "from the bottom of the heap" blog