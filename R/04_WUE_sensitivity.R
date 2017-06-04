# This script will look at sensitivity of WUE to precipitation, CO2, and tair
# Author: Kelly Heilman
library(reshape2)
library(ggplot2)
library(tidyr)

# load the necessary data:
#IWUE <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_IWUE.RDS"))
#WUEi <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_WUEi.RDS"))
#WUEt <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_WUEt.RDS"))
#CO2 <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_CO2.RDS"))
#precip <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_precip.RDS"))
#tair <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_tair.RDS"))
#gwbi <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_GWBI.RDS"))
#lai <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_lai.RDS"))

# load the pft specific data:
ED2.Dens <- readRDS("D:/Kelly/WUE_MIP/Data/ED2/ED2.Dens.rds")
ED2.Fcomp <- readRDS("D:/Kelly/WUE_MIP/Data/ED2/ED2.Fcomp.RDS")

load("D:/Kelly/WUE_MIP/Data/PalEON_siteInfo_all.RData")

timevec <- 1:13932
month <- rep(1:12, 1161)
yearsince  <- rep(0:1160, each =12)
year <- yearsince + 850
# try unlist ot convert Fcomp to a df
#df <- data.frame(matrix(unlist(Fcomp), nrow=13932, byrow=T),stringsAsFactors=FALSE)
#test <- do.call(rbind, lapply(Fcomp, data.frame, stringsAsFactors=FALSE))

#convert list to array
pft.lab=c("grass.c4", "tropic.early", "tropic.mid", "tropic.late", "grass.c3.temp", "pine.north", "pine.south", "conifer.late", "temp.decid.early", "temp.decid.mid", "temp.decid.late","ag1", "ag2", "ag3", "ag4","grass.c3.subtrop","Araucaria")

Fcomp<- ED2.Fcomp
Dens <- ED2.Dens


dimnames(Fcomp) <- list(timevec, paleon$num, pft.lab)
dimnames(Dens) <- list(timevec, paleon$num, pft.lab)
#grass <- ED2.Fcomp[,,"grass.c4"]
#dens <- dens$Dens



# plot pfts that occurred in ED runs:


# reduce to the actual pfts present:
pfts <- c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
          "temp.decid.late", "grass.c3.temp" )
Fcomp.r <- Fcomp[,,pfts]
Dens.r <- Dens[,,pfts]

# plots of Fcomp at each site (for ED2):

for(i in 1:length(paleon$num)){
png(height=7, width = 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Fcomp/ED2_Fcomp_",paleon[i,]$latlon, ".png"))
plot(Fcomp.r[,i,"pine.north"], ylim = c(0,1.5), col = 'red', ylab = "Fcomp", xlab = "Months since 850")
points(Fcomp.r[,i,"conifer.late"], col = "forestgreen")
points(Fcomp.r[,i,"temp.decid.early"], col = "lightblue")
points(Fcomp.r[,i,"temp.decid.mid"], col = 'blue')
points(Fcomp.r[,i,"temp.decid.late"], col = "orange")
points(Fcomp.r[,i,"grass.c3.temp"], col = "black")
legend('topleft',legend=c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
         "temp.decid.late", "grass.c3.temp" ), 
       col = c('red', 'forestgreen', 'lightblue', 'blue', 'orange', 'black'), pch=16)
dev.off()
}


# make the plot for density:
for(i in 1:length(paleon$num)){
  png(height=7, width = 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Dens/ED2_Dens_",paleon[i,]$latlon, ".png"))
  plot(Dens.r[,i,"pine.north"] , ylim=c(10000, 1000000), col = 'red', ylab = "Fcomp", xlab = "Months since 850")
  points(Dens.r[,i,"conifer.late"], col = "forestgreen")
  points(Dens.r[,i,"temp.decid.early"], col = "lightblue")
  points(Dens.r[,i,"temp.decid.mid"], col = 'blue')
  points(Dens.r[,i,"temp.decid.late"], col = "orange")
  points(Dens.r[,i,"grass.c3.temp"], col = "black")
  legend('topleft',legend=c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
                            "temp.decid.late", "grass.c3.temp" ), 
         col = c('red', 'forestgreen', 'lightblue', 'blue', 'orange', 'black'), pch=16)
  dev.off()
}

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



# note: density values seem very high--are these values really stems per hectare?



#-------------------get dataframes aggregated to yearly timestep---------
WUEt <- readRDS(paste0(getwd(), "/Data/ED2/ED2.WUEt.rds"))
WUEi <- readRDS(paste0(getwd(), "/Data/ED2/ED2.WUEi.rds"))
IWUE <- readRDS(paste0(getwd(), "/Data/ED2/ED2.IWUE.rds"))


# we may need to rethink this function, but it is a start
get.yrmeans <- function(df, var){
  df <- data.frame(df)
  df$Year <- year
  df$Month <- month
  m <- melt(df, id.vars=c("Year", "Month"))
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
precip.y <- get.yrmeans(ED2.precipf, "precip")
CO2.y <- get.yrmeans(ED2.CO2, "CO2")
lai.y <- get.yrmeans(ED2.LAI, "LAI")
gwbi.y <- get.yrmeans(ED2.GWBI, "GWBI")
tair.y <- get.yrmeans(ED2.tair, "Tair")

# use reduce to merge all these into one dataframe
all.y <- Reduce(function(x, y) merge(x, y, by = ,all=TRUE), list(IWUE.y, WUEi.y, WUEt.y, CO2.y,
                                                       gwbi.y,tair.y, precip.y, lai.y))


# write for future use
saveRDS(all.y, paste0(getwd(), "/Data/extracted/ED_yearly_allnonpft.RDS"))


# make plots of variables by sites
all.y <- readRDS(paste0(getwd(), "/Data/extracted/ED_yearly_allnonpft.RDS"))

plot.sens <- function(df, xname, yname){
  df <- df[,c("Year", "Site", xname, yname)]
  colnames(df) <- c("Year", "Site", "x", "y")
  lim <- quantile(df$x, .99, na.rm=T) # so we dont plot the outliers
  
  png(height = 12, width = 12, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/sensitivity/ED2_", xname,"_", yname,"_yr_sens_site.png"))
  print(ggplot(data = df, aes(x = x, y = y, color = Site))+geom_point()+xlim(0,lim)+
       ylab(yname)+ xlab(xname)+stat_smooth(color = "black")+theme_bw()+ theme(legend.position="none") )
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


#---------------Since there are many grid cells, lets classifiy them by mean annual precip--------
# then we can look at sensitivity of the variables within different moisture classes:

# find the mean annual precip rate for each grid cell:
ED2.precip <- readRDS(paste0(getwd(), "/Data/ED2/ED2.precipf.rds"))
pr <- data.frame(ED2.precip)
pr$Year <- year
pr$Month <- month
m <- melt(pr, id.vars=c("Year", "Month"))
yrsums <- dcast(m, Year ~ variable, sum, na.rm=TRUE) # get a precipitaiton rate sum for each year
means <- colSums(yrsums[,2:255])
colnames(means)<- paleon$latlon
paleon$meanprecipf <-means

#X11(width = 12)
ggplot(paleon, aes(x=lon, y=lat, fill=meanprecipf))+geom_raster()

summary(means)
summary(means[!means == 0])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.1751  0.3016  0.3745  0.3744  0.4519  0.5788 

# get the gridcells where the mean precipitation rate is less than 25th percentile:

pct25 <- paleon[paleon$meanprecipf <= quantile(means[!means == 0], .25),]$num
pct50 <- paleon[paleon$meanprecipf >= quantile(means[!means == 0], .25) & paleon$meanprecipf <= quantile(means[!means == 0], .50),]$num
pct75 <- paleon[paleon$meanprecipf >= quantile(means[!means == 0], .50) & paleon$meanprecipf <= quantile(means[!means == 0], .75),]$num
pct100 <- paleon[paleon$meanprecipf >= quantile(means[!means == 0], .75)& paleon$meanprecipf <= quantile(means[!means == 0], 1),]$num

# add the X before the number to match all.y
pct25<- paste0("X", pct25)
pct50<- paste0("X", pct50)
pct75<- paste0("X", pct75)
pct100<- paste0("X", pct100)

# make plots for only the low precipitation sites and save in preliminaryplots/sensitivity/by_precip:

plot.sens.site <- function(df, xname, yname, pct, percentile){
  df <- df[,c("Year", "Site", xname, yname)]
  df <- df[df$Site %in% pct, ]
  colnames(df) <- c("Year", "Site", "x", "y")
  lim <- quantile(df$x, .99, na.rm=T) # so we dont plot the outliers
  
  png(height = 12, width = 12, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/sensitivity/by_precip/",xname,"/ED2_", xname,"_", yname,"_",percentile,"_yr_sens.png"))
  print(ggplot(data = df, aes(x = x, y = y, color = Site))+geom_point()+xlim(0,lim)+
          ylab(yname)+ xlab(xname)+stat_smooth(color = "black")+theme_bw()+ theme(legend.position="none") + ggtitle(paste0(percentile,"precip") ))
  dev.off()
  
  png(height = 12, width = 12, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/sensitivity/by_precip/",xname,"/ED2_", xname,"_", yname,"_",percentile,"_yr_sens_site.png"))
  print(ggplot(data = df, aes(x = x, y = y, color = Site))+geom_point()+xlim(0,lim)+
          ylab(yname)+ xlab(xname)+stat_smooth(color = "black")+facet_wrap(~Site)+theme_bw()+ theme(legend.position="none") + ggtitle(paste0(percentile,"precip") ))
  dev.off()
}

# for WUEi:
plot.sens.site(all.y, "WUEi", "CO2", pct25, "25th_percentile")
plot.sens.site(all.y, "WUEi", "CO2", pct50, "50th_percentile")
plot.sens.site(all.y, "WUEi", "CO2", pct75, "75th_percentile")
plot.sens.site(all.y, "WUEi", "CO2", pct100, "100th_percentile")

plot.sens.site(all.y, "WUEi", "GWBI", pct25, "25th_percentile")
plot.sens.site(all.y, "WUEi", "GWBI", pct50, "50th_percentile")
plot.sens.site(all.y, "WUEi", "GWBI", pct75, "75th_percentile")
plot.sens.site(all.y, "WUEi", "GWBI", pct100, "100th_percentile")

plot.sens.site(all.y, "WUEi", "Tair", pct25, "25th_percentile")
plot.sens.site(all.y, "WUEi", "Tair", pct50, "50th_percentile")
plot.sens.site(all.y, "WUEi", "Tair", pct75, "75th_percentile")
plot.sens.site(all.y, "WUEi", "Tair", pct100, "100th_percentile")

plot.sens.site(all.y, "WUEi", "precip", pct25, "25th_percentile")
plot.sens.site(all.y, "WUEi", "precip", pct50, "50th_percentile")
plot.sens.site(all.y, "WUEi", "precip", pct25, "75th_percentile")
plot.sens.site(all.y, "WUEi", "precip", pct100, "100th_percentile")

plot.sens.site(all.y, "WUEi", "LAI", pct25, "25th_percentile")
plot.sens.site(all.y, "WUEi", "LAI", pct50, "50th_percentile")
plot.sens.site(all.y, "WUEi", "LAI", pct75, "75th_percentile")
plot.sens.site(all.y, "WUEi", "LAI", pct100, "100th_percentile")

# for WUEi:
plot.sens.site(all.y, "IWUE", "CO2", pct25, "25th_percentile")
plot.sens.site(all.y, "IWUE", "CO2", pct50, "50th_percentile")
plot.sens.site(all.y, "IWUE", "CO2", pct75, "75th_percentile")
plot.sens.site(all.y, "IWUE", "CO2", pct100, "100th_percentile")

plot.sens.site(all.y, "IWUE", "GWBI", pct25, "25th_percentile")
plot.sens.site(all.y, "IWUE", "GWBI", pct50, "50th_percentile")
plot.sens.site(all.y, "IWUE", "GWBI", pct75, "75th_percentile")
plot.sens.site(all.y, "IWUE", "GWBI", pct100, "100th_percentile")

plot.sens.site(all.y, "IWUE", "Tair", pct25, "25th_percentile")
plot.sens.site(all.y, "IWUE", "Tair", pct50, "50th_percentile")
plot.sens.site(all.y, "IWUE", "Tair", pct75, "75th_percentile")
plot.sens.site(all.y, "IWUE", "Tair", pct100, "100th_percentile")

plot.sens.site(all.y, "IWUE", "precip", pct25, "25th_percentile")
plot.sens.site(all.y, "IWUE", "precip", pct50, "50th_percentile")
plot.sens.site(all.y, "IWUE", "precip", pct25, "75th_percentile")
plot.sens.site(all.y, "IWUE", "precip", pct100, "100th_percentile")

plot.sens.site(all.y, "IWUE", "LAI", pct25, "25th_percentile")
plot.sens.site(all.y, "IWUE", "LAI", pct50, "50th_percentile")
plot.sens.site(all.y, "IWUE", "LAI", pct75, "75th_percentile")
plot.sens.site(all.y, "IWUE", "LAI", pct100, "100th_percentile")

# for WUEt:
plot.sens.site(all.y, "WUEt", "CO2", pct25, "25th_percentile")
plot.sens.site(all.y, "WUEt", "CO2", pct50, "50th_percentile")
plot.sens.site(all.y, "WUEt", "CO2", pct75, "75th_percentile")
plot.sens.site(all.y, "WUEt", "CO2", pct100, "100th_percentile")

plot.sens.site(all.y, "WUEt", "GWBI", pct25, "25th_percentile")
plot.sens.site(all.y, "WUEt", "GWBI", pct50, "50th_percentile")
plot.sens.site(all.y, "WUEt", "GWBI", pct75, "75th_percentile")
plot.sens.site(all.y, "WUEt", "GWBI", pct100, "100th_percentile")

plot.sens.site(all.y, "WUEt", "Tair", pct25, "25th_percentile")
plot.sens.site(all.y, "WUEt", "Tair", pct50, "50th_percentile")
plot.sens.site(all.y, "WUEt", "Tair", pct75, "75th_percentile")
plot.sens.site(all.y, "WUEt", "Tair", pct100, "100th_percentile")

plot.sens.site(all.y, "WUEt", "precip", pct25, "25th_percentile")
plot.sens.site(all.y, "WUEt", "precip", pct50, "50th_percentile")
plot.sens.site(all.y, "WUEt", "precip", pct25, "75th_percentile")
plot.sens.site(all.y, "WUEt", "precip", pct100, "100th_percentile")

plot.sens.site(all.y, "WUEt", "LAI", pct25, "25th_percentile")
plot.sens.site(all.y, "WUEt", "LAI", pct50, "50th_percentile")
plot.sens.site(all.y, "WUEt", "LAI", pct75, "75th_percentile")
plot.sens.site(all.y, "WUEt", "LAI", pct100, "100th_percentile")





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