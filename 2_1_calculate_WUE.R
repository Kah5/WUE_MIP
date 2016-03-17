library(ggplot2)
library(reshape)
library(plantecophys)

#for calculating iWUE GPP/T*VPD
summary(qair[[1]][,"ed2"])
summary(tair[[1]][,"ed2"])

qair2rh <- function(qair, temp, press = 101.325){
  es <-  6.112 * exp((17.67 * (temp-273.15))/((temp-273.15) +243.5 ))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
 return(rh)
}

#check vpd calculation
qair2vpd <- function(qair, temp, press = 101.325){
  es <-  6.112 * exp((17.67 * (temp-273.15))/((temp-273.15) +243.5 ))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0

vpd <- (es- (rh * es))
return(vpd)
}


RHtoVPD()
# calculate a VPD list
RH <- VPD <- list()
for(s in 1:length(site.list)){
RH[[s]] <- qair2rh(qair[[s]],tair[[s]], press = 101.325)
VPD[[s]] <- qair2vpd(qair[[s]],tair[[s]], press = 101.325)
}

for(s in 1:length(site.list)){
  VPD[[s]] <- RHtoVPD(RH[[s]], tair[[s]]-273.15, Pa = 101)
}
names(RH)<- names(VPD)<- site.list



# calculate canopy conductance 
# need to correct this/make sure it is right
canconduct <- function(tair, Transp, LAI, VPD){
((115.8 + 0.4226*(tair-273.15))*((Transp*1000/LAI)/VPD)*0.0001)
}

models <- c("clm45","clm.bgc", "ed2", "ed.lu", "sibcasa")

Gc <- list()
for (s in 1:length(site.list)){
  Gc[[s]] <- canconduct(tair[[s]][,models], Transp[[s]][,models], LAI.m[[s]][,models], VPD[[s]][,models])
}

plot(Gc[[4]][,"clm.bgc"])

trans.models <- c("clm45", "clm.bgc", "ed2", "ed.lu",  "sibcasa")
#WUE weighted by VPD
IWUE <- WUEt<- WUEi<- list()
for (s in 1:length(site.list)){
  IWUE[[s]] <- (GPP[[s]][,trans.models]*1000/Transp[[s]][,trans.models])*(VPD[[s]][,trans.models])
  WUEt[[s]] <- GPP[[s]][,trans.models]*1000/Transp[[s]][,trans.models]
  WUEi[[s]] <- GPP[[s]][,trans.models]*1000/Gc[[s]][trans.models] # convert to kg/m2/s
}






#create dataframes for sites
HA.WUEt <- WUEt[[4]]
HA.WUEi <- WUEi[[4]]
HA.IWUE <- IWUE[[4]]
HA.CO2  <- CO2[[4]]
HA.VPD  <- VPD[[4]]

HA.SWE <- SWE[[4]]
HA.Trans <- SWE[[4]]

#crappy prelim plots
plot(HA.VPD$ed2, HA.Trans$ed2)
plot(HA.VPD$clm45, HA.Trans$clm45)
plot(HA.VPD$sibcasa, HA.Trans$sibcasa)




pdf("BL_Longterm_WUEplots.pdf")

HF.wuet <- data.frame(Year = Year,
                     Month = Month,
                     HA.WUEt)
q <- aggregate(.~Year, data = HF.wuet, FUN = mean)
q <- melt(q[,-2], id.vars = c( "Year"))
#q <- transform(q, new_date = as.Date(paste(Year,Month,'01',sep="-")))

ggplot(q, aes(Year,value, col=variable)) + 
  geom_line()  + ylab("GPP/Transpiration")+ 
  labs(title = " Yearly Avg. WUE Harvard Forest")

#for intrinsic WUE (GPP/Gc)
HF.wuei <- data.frame(Year = Year,
                      Month = Month,
                      HA.WUEi)
q <- aggregate(.~Year, data = HF.wuei, FUN = mean)
q <- melt(q[,-2], id.vars = c( "Year"))
#q <- transform(q, new_date = as.Date(paste(Year,Month,'01',sep="-")))

ggplot(q, aes(Year,value, col=variable)) + 
  geom_line() + ylab("GPP/Canopy Conductance")+ 
  labs(title = " Yearly Avg. WUE Harvard Forest")


#for inherent WUE (GPP/Transp*VPD)
HF.Iwue <- data.frame(Year = Year,
                      Month = Month,
                      HA.IWUE)
q <- aggregate(.~Year, data = HF.Iwue, FUN = mean)
q <- melt(q[,-2], id.vars = c( "Year"))
#q <- transform(q, new_date = as.Date(paste(Year,Month,'01',sep="-")))

ggplot(q, aes(Year,value, col=variable)) + 
  geom_line() + ylab("GPP/Transp*VPD)")+ 
  labs(title = " Yearly Avg. WUE Harvard Forest")


#compare yearly seasonal cycle of WUE

qm<- aggregate(.~Month, data = HF.wuei, FUN = mean)
qm <- melt(qm[,-2], id.vars = c( "Month"))
#q <- transform(q, new_date = as.Date(paste(Year,Month,'01',sep="-")))

ggplot(qm, aes(Month,value, col=variable)) + 
  geom_line() +geom_point() + ylab("GPP/Transpiration*VPD")+ 
  labs(title = "Monthly Avg. WUE Harvard Forest")

qm.t<- aggregate(.~Month, data = HF.wuet, FUN = mean)
qm.t <- melt(qm.t[,-2], id.vars = c( "Month"))
#q <- transform(q, new_date = as.Date(paste(Year,Month,'01',sep="-")))

ggplot(qm.t, aes(Month,value, col=variable)) + 
  geom_line() +geom_point() + ylab("GPP/Transpiration")+ 
  labs(title = "Monthly Avg. WUE Harvard Forest")

dev.off()



HF.20wuei <- HF.wuei[HF.wuei$Year %in% last20, ]
HF.20wuet <- HF.wuet[HF.wuet$Year %in% last20, ]

wuei.y <- aggregate(.~Year, data = HF.20wuei, FUN = mean)
wuet.y <- aggregate(.~Year, data = HF.20wuei, FUN = mean)






#qm <- melt(qm[,-2], id.vars = c( "Month"))
#q <- transform(q, new_date = as.Date(paste(Year,Month,'01',sep="-")))

ggplot(qm, aes(Month,value, col=variable)) + 
  geom_line() +geom_point() + ylab("GPP/Transpiration")+ 
  labs(title = "Monthly Avg. WUE Harvard Forest")








plot(CO2[[1]][,"ed2"], WUEi[[1]][,"ed2"])

HF.wuei <- data.frame(Year = Year,
                      Month = Month,
                      HA.WUEi, 
                      CO2= HA.CO2[,"ed2"])


q <- melt(HF.wuei[,-c(1:2)], id.vars = c( "CO2"))

ggplot(q, aes(CO2,value, col=variable)) + 
  geom_smooth() + ylab("GPP/Transpiration*VPD")+ 
  labs(title = " Yearly Avg. WUE Harvard Forest")


