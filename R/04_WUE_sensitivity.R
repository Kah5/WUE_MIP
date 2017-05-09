# This script will look at sensitivity of WUE to precipitation, CO2, and tair
# Author: Kelly Heilman
library(reshape2)
library(ggplot2)

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

# create a column IDentifier
datain <- read.csv("Data/ED_site_list_lat_lon.csv")
Fcomp.df <- merge(Fcomp.df, datain[,c('lat','lon','ID','site.name')], by = c('lat', 'lon'))

############################### 

linecolor <- c('Earlypine' = "red", 'lateconifer'="orange", 'earlydeciduous'="blue",
               'middeciduous' ="grey", 'latedeciduous'='forestgreen')

datain <- read.csv( paste0(getwd(), "/Data/ED_site_list_lat_lon.csv") )


#---------------------- make a plot of fcomp at each site-------------------
sites <- unique(Fcomp.df$site.name)


  ggplot(Fcomp.df[Fcomp.df$site.name %in% sites[i],], aes(x = time, y = Fcomp, colour = pft)) + geom_line()+theme_bw()+



ggplot()+ geom_line(aes(x = as.numeric(Fcomp.df$time), y = Fcomp[datain[i,"latrow"],datain[i,"lonrow"],,pft.list[6]],colour = "Earlypine" ))+theme_bw()+
  geom_line(aes(x = as.numeric(dimnames(Fcomp)$time), y = Fcomp[datain[i,"latrow"],datain[i,"lonrow"],,pft.list[8]], colour = "lateconifer"))+
  geom_line(aes(x = as.numeric(dimnames(Fcomp)$time), y = Fcomp[datain[i,"latrow"],datain[i,"lonrow"],,pft.list[9]], colour = "earlydeciduous"))+
  geom_line(aes(x = as.numeric(dimnames(Fcomp)$time), y =Fcomp[datain[i,"latrow"],datain[i,"lonrow"],,pft.list[10]], colour = "middeciduous"))+
  geom_line(aes(x = as.numeric(dimnames(Fcomp)$time), y = Fcomp[datain[i,"latrow"],datain[i,"lonrow"],,pft.list[11]], colour = "latedeciduous")) + ylab("Fcomp") + xlab("time")+ggtitle("title")+
  scale_colour_manual("PFTs",
                      values= c('Earlypine' = "red", 'lateconifer'="orange", 'earlydeciduous'="blue",
                                'middeciduous' ="grey", 'latedeciduous'='forestgreen'))

  }
# note this only works for point lat= 33.25 lon = -99.75
#------------------ plot density of each pft at each site--------------------------

ggplot()+ geom_line(aes(x = as.numeric(dimnames(dens)$time), y = dens[1,1,,pft.list[6]],colour = "Earlypine" ))+theme_bw()+
  geom_line(aes(x = as.numeric(dimnames(dens)$time), y = dens[1,1,,pft.list[8]], colour = "lateconifer"))+
  geom_line(aes(x = as.numeric(dimnames(dens)$time), y = dens[1,1,,pft.list[9]], colour = "earlydeciduous"))+
  geom_line(aes(x = as.numeric(dimnames(dens)$time), y = dens[1,1,,pft.list[10]], colour = "middeciduous"))+
  geom_line(aes(x = as.numeric(dimnames(dens)$time), y = dens[1,1,,pft.list[11]], colour = "latedeciduous")) + ylab("Density (1/ha)") + xlab("time")+ggtitle("title")+
  scale_colour_manual("PFTs",
                      values= c('Earlypine' = "red", 'lateconifer'="orange", 'earlydeciduous'="blue",
                                'middeciduous' ="grey", 'latedeciduous'='forestgreen'))


# note: density values seem very high--are these values really stems per hectare?

# preliminary crappy plots
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