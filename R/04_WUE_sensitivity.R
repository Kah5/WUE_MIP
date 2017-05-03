# This script will look at sensitivity of WUE to precipitation, CO2, and tair
# Author: Kelly Heilman
library(reshape2)
library(ggplot2)

# load the necessary data:
IWUE <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_IWUE.RDS"))
WUEi <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_WUEi.RDS"))
WUEt <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_WUEt.RDS"))
CO2 <- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_CO2.RDS"))
precip<- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_precip.RDS"))
tair<- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_tair.RDS"))
gwbi<- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_GWBI.RDS"))
lai<- readRDS( paste0(getwd(),"/Data/extracted/ED_monthly_lai.RDS"))

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