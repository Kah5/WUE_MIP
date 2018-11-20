#------------What is the relationship between density and Biomass in the models------------

# read in density files for ED2:

Dens <- readRDS("/Users/kah/Documents/WUE_MIP/Data/ED2/ED2.Dens.rds")

load("/Users/kah/Documents/WUE_MIP/Data/PalEON_siteInfo_all.RData")

# make plots for ED2:
timevec <- 1:13932
month <- rep(1:12, 1161)
yearsince  <- rep(0:1160, each =12)
year <- yearsince + 850

# try unlist ot convert Fcomp to a df
#df <- data.frame(matrix(unlist(Fcomp), nrow=13932, byrow=T),stringsAsFactors=FALSE)
#test <- do.call(rbind, lapply(Fcomp, data.frame, stringsAsFactors=FALSE))

#convert list to array
pft.lab=c("grass.c4", "tropic.early", "tropic.mid", "tropic.late", "grass.c3.temp", "pine.north", "pine.south", "conifer.late", "temp.decid.early", "temp.decid.mid", "temp.decid.late","ag1", "ag2", "ag3", "ag4","grass.c3.subtrop","Araucaria")

dimnames(Dens) <- list(year, paleon$num, pft.lab)

pfts <- c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
          "temp.decid.late", "grass.c3.temp" )

Dens.r <- Dens[,,pfts]
CO2<- readRDS("/Users/kah/Documents/WUE_MIP/Data/ED2/ED2.CO2.rds")
TotalDens <- CO2

# get total density:
for(i in 1:length(paleon$num)){
  
  dens.site <- data.frame(Dens.r[,i,])
  TotalDens[,i] <- rowSums(dens.site, na.rm=TRUE)
  
}

dimnames(TotalDens) <- list(year, paleon$num)
saveRDS(TotalDens, "/Users/kah/Documents/WUE_MIP/outputs/data/ED2/TotalDens.rds")

# read in AGB
AGB <- readRDS("/Users/kah/Documents/WUE_MIP/Data/ED2/ED2.AGB.rds")
dimnames(AGB) <- list(year, paleon$num)

# get the yearly mean of AGB and Dens
source("/Users/kah/Documents/WUE_MIP/R/get.yrmeans.R")
AGB.y <- get.yrmeans(AGB, "AGB")
TotalDens.y <- get.yrmeans(TotalDens, "TotalDens")

AGBdens <- merge(AGB.y, TotalDens.y, by = c("Year", "Site"))

saveRDS(AGBdens, "/Users/kah/Documents/WUE_MIP/outputs/data/ED2/AGBDens.rds")


# read in AGBI
AGBI <- readRDS("/Users/kah/Documents/WUE_MIP/outputs/data/ED2/ED2.agbi.rds")
#dimnames(AGBI) <- list(year, paleon$num)
AGBI.m<- melt(AGBI, id.vars = "Year")
colnames(AGBI.m) <- c("Year", "Site", "AGBI")

AGBdens<- merge(AGBdens, AGBI.m, by =c("Year", "Site"))

AVGdens <- AGBdens[,c("Year", "Site", "TotalDens")]
test <- dcast( AVGdens, TotalDens ~ Site, fun.aggregate = "mean")
#---------------plot total density vs aboveground biomass (by site?)-------------

# check timesereies ot make sure they look okay
ggplot(AGBdens, aes(Year, AGB))+geom_point()
ggplot(AGBdens, aes(Year, AGBI))+geom_point()
ggplot(AGBdens, aes(Year, TotalDens))+geom_point()

png(height= 5, width= 7, units = "in", res=300, "/Users/kah/Documents/WUE_MIP/outputs/preliminaryplots/Dens/ED2_AGB_Dens_full.png")
ggplot(AGBdens, aes(AGB, TotalDens, color = Site))+geom_point()+
  theme(legend.position = "none")+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+theme(legend.position = "none")
dev.off()

png(height= 5, width= 7, units = "in", res=300, "/Users/kah/Documents/WUE_MIP/outputs/preliminaryplots/Dens/ED2_AGBI_Dens_full.png")

ggplot(AGBdens, aes(AGBI, TotalDens, color = Site))+geom_point()+
  theme(legend.position = "none")+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass increment (kgC/m2)")+theme(legend.position = "none")
dev.off()
# from this figure, it is apparent that at intermediate AGB, you can have highish and lowish densities


# for LPJ-GUESS:
#------------What is the relationship between density and Biomass in the models------------

# read in density files for ED2:

Dens <- readRDS("/Users/kah/Documents/WUE_MIP/Data/LPJ-GUESS/LPJ-GUESS.Dens.rds")


load("/Users/kah/Documents/WUE_MIP/Data/PalEON_siteInfo_all.RData")

# make plots for ED2:
timevec <- 1:13932
month <- rep(1:12, 1161)
yearsince  <- rep(0:1160, each =12)
year <- yearsince + 850

# try unlist ot convert Fcomp to a df
#df <- data.frame(matrix(unlist(Fcomp), nrow=13932, byrow=T),stringsAsFactors=FALSE)
#test <- do.call(rbind, lapply(Fcomp, data.frame, stringsAsFactors=FALSE))

#convert list to array

dimnames(Dens) <- list(year, paleon$num)

pfts <- c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
          "temp.decid.late", "grass.c3.temp" )

Dens.r <- Dens[,,pfts]
CO2<- readRDS("/Users/kah/Documents/WUE_MIP/Data/ED2/ED2.CO2.rds")
TotalDens <- CO2

# get total density:
for(i in 1:length(paleon$num)){
  
  dens.site <- data.frame(Dens.r[,i,])
  TotalDens[,i] <- rowSums(dens.site, na.rm=TRUE)
  
}

dimnames(TotalDens) <- list(year, paleon$num)
saveRDS(TotalDens, "/Users/kah/Documents/WUE_MIP/outputs/data/ED2/TotalDens.rds")

# read in AGB
AGB <- readRDS("/Users/kah/Documents/WUE_MIP/Data/ED2/ED2.AGB.rds")
dimnames(AGB) <- list(year, paleon$num)

# get the yearly mean of AGB and Dens
source("/Users/kah/Documents/WUE_MIP/R/get.yrmeans.R")
AGB.y <- get.yrmeans(AGB, "AGB")
TotalDens.y <- get.yrmeans(TotalDens, "TotalDens")

AGBdens <- merge(AGB.y, TotalDens.y, by = c("Year", "Site"))

saveRDS(AGBdens, "/Users/kah/Documents/WUE_MIP/outputs/data/ED2/AGBDens.rds")


# read in AGBI
AGBI <- readRDS("/Users/kah/Documents/WUE_MIP/outputs/data/ED2/ED2.agbi.rds")
#dimnames(AGBI) <- list(year, paleon$num)
AGBI.m<- melt(AGBI, id.vars = "Year")
colnames(AGBI.m) <- c("Year", "Site", "AGBI")

AGBdens<- merge(AGBdens, AGBI.m, by =c("Year", "Site"))

AVGdens <- AGBdens[,c("Year", "Site", "TotalDens")]
test <- dcast( AVGdens, TotalDens ~ Site, fun.aggregate = "mean")
#---------------plot total density vs aboveground biomass (by site?)-------------

# check timesereies ot make sure they look okay
ggplot(AGBdens, aes(Year, AGB))+geom_point()
ggplot(AGBdens, aes(Year, AGBI))+geom_point()
ggplot(AGBdens, aes(Year, TotalDens))+geom_point()

png(height= 5, width= 7, units = "in", res=300, "/Users/kah/Documents/WUE_MIP/outputs/preliminaryplots/Dens/ED2_AGB_Dens_full.png")
ggplot(AGBdens, aes(AGB, TotalDens, color = Site))+geom_point()+
  theme(legend.position = "none")+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+theme(legend.position = "none")
dev.off()

png(height= 5, width= 7, units = "in", res=300, "/Users/kah/Documents/WUE_MIP/outputs/preliminaryplots/Dens/ED2_AGBI_Dens_full.png")

ggplot(AGBdens, aes(AGBI, TotalDens, color = Site))+geom_point()+
  theme(legend.position = "none")+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass increment (kgC/m2)")+theme(legend.position = "none")
dev.off()
# from this figure, it is apparent that at intermediate AGB, you can have highish and lowish densities




# ------------look at the relationship a few different ways:----------------------
# 1. Does the curver look the same Before and after 1800?
# 2. Does climate mediate the agb-dens curve?
# 3. Does Fcomp mediate the agb-dens curve?


# 1. -------Does the curver look the same Before and after 1800?---------------
post1800 <- 1800:2010

# make the post 1800
png(height= 5, width= 7, units = "in", res=300, "/Users/kah/Documents/WUE_MIP/outputs/preliminaryplots/Dens/ED2_AGB_Dens_post1800.png")
ggplot(AGBdens[AGBdens$Year %in% post1800,], aes(AGB, TotalDens, color = Site))+geom_point()+
  theme(legend.position = "none")+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+theme(legend.position = "none")
dev.off()
# make the post 1800

png(height= 5, width= 7, units = "in", res=300, "/Users/kah/Documents/WUE_MIP/outputs/preliminaryplots/Dens/ED2_AGBI_Dens_post1800.png")

ggplot(AGBdens[AGBdens$Year %in% post1800,], aes(AGBI, TotalDens, color = Site))+geom_point()+
  theme(legend.position = "none")+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass Increment (kgC/m2/yr)")+theme(legend.position = "none")
dev.off()

# make the pre 1800
png(height= 5, width= 7, units = "in", res=300, "/Users/kah/Documents/WUE_MIP/outputs/preliminaryplots/Dens/ED2_AGB_Dens_pre1800.png")
ggplot(AGBdens[!AGBdens$Year %in% post1800,], aes(AGB, TotalDens, color = Site))+geom_point()+
  theme(legend.position = "none")+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+theme(legend.position = "none")
dev.off()

png(height= 5, width= 7, units = "in", res=300, "/Users/kah/Documents/WUE_MIP/outputs/preliminaryplots/Dens/ED2_AGBI_Dens_pre1800.png")
ggplot(AGBdens[!AGBdens$Year %in% post1800,], aes(AGBI, TotalDens, color = Site))+geom_point()+
  theme(legend.position = "none")+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass Increment (kgC/m2/yr)")+theme(legend.position = "none")
dev.off()
# ANSWER: Yes it is about the same relationship

# 2. ---------------Does Climate mediate the AGB-Density curve?------------------------
precip <- readRDS("/Users/kah/Documents/WUE_MIP/Data/ED2/ED2.precipf.rds")
tair <- readRDS("/Users/kah/Documents/WUE_MIP/Data/ED2/ED2.tair.rds")

# get the year means
precip.y <- get.yrmeans(precip, "precip")
tair.y <- get.yrmeans(tair, "tair")

AGBdens <- merge(AGBdens, precip.y, by = c("Year", "Site"))
AGBdens <- merge(AGBdens, tair.y, by = c("Year", "Site"))

# plot the same AGB vs density plot, but color by precip
png(height= 5, width= 7, units = "in", res=300, "/Users/kah/Documents/WUE_MIP/outputs/preliminaryplots/Dens/ED2_AGB_Dens_by_precip.png")
ggplot(AGBdens, aes(AGB, TotalDens, color = precip))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+theme(legend.position = "none")
dev.off()

png(height= 5, width= 7, units = "in", res=300, "/Users/kah/Documents/WUE_MIP/outputs/preliminaryplots/Dens/ED2_AGBI_Dens_by_precip.png")
ggplot(AGBdens, aes(AGBI, AGB, color = precip))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+theme(legend.position = "none")
dev.off()

png(height= 5, width= 7, units = "in", res=300, "/Users/kah/Documents/WUE_MIP/outputs/preliminaryplots/Dens/ED2_AGBI_Precip_by_Dens.png")
ggplot(AGBdens, aes(AGBI, precip, color = TotalDens))+geom_point()+theme_bw()+ylab("Precipitation Rate")+xlab("Aboveground biomass Increment (kgC/m2)")+theme(legend.position = "none")
dev.off()

# split it up and plot next to each other:
ph <- ggplot(AGBdens[AGBdens$precip >= mean(AGBdens$precip, na.rm=TRUE),], aes(AGB, TotalDens, color = precip))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+xlim(0,45)+scale_color_gradient(low = "red", high = "blue", limits=c(range(AGBdens$precip, na.rm=TRUE)))+ggtitle("ED2 higher than average precip")
pl <- ggplot(AGBdens[AGBdens$precip < mean(AGBdens$precip, na.rm=TRUE),], aes(AGB, TotalDens, color = precip))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+xlim(0,45)+scale_color_gradient(low = "red", high = "blue", limits=c(range(AGBdens$precip, na.rm=TRUE)))+ggtitle("ED2 lower than average precip")

source("R/grid_arrange_shared_legend.R")
png(height= 7, width = 7, units = "in", res=300, "/Users/kah/Documents/WUE_MIP/outputs/preliminaryplots/Dens/ED2_AGB_Dens_by_precip2.png")
grid_arrange_shared_legend(ph, pl, nrow=2, ncol=1)
dev.off()


# plot AGB vs. density, but color by tair
png(height= 5, width= 7, units = "in", res=300, "/Users/kah/Documents/WUE_MIP/outputs/preliminaryplots/Dens/ED2_AGB_Dens_by_tair.png")
ggplot(AGBdens, aes(AGB, TotalDens, color = tair))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+theme(legend.position = "none")
dev.off()

# split it up and plot next to each other:
th<- ggplot(AGBdens[AGBdens$tair >= mean(AGBdens$tair, na.rm=TRUE),], aes(AGB, TotalDens, color = tair))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+scale_color_gradient(low = "red", high = "blue", limits=c(range(AGBdens$tair, na.rm=TRUE)))+ggtitle("ED2 higher than average tair")
tl<- ggplot(AGBdens[AGBdens$tair < mean(AGBdens$tair, na.rm=TRUE),], aes(AGB, TotalDens, color = tair))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+scale_color_gradient(low = "red", high = "blue", limits=c(range(AGBdens$tair, na.rm=TRUE)))+ggtitle("ED2 lower than average tair")

png(height= 7, width = 7, units = "in", res=300, "/Users/kah/Documents/WUE_MIP/outputs/preliminaryplots/Dens/ED2_AGB_Dens_by_precip2.png")
grid_arrange_shared_legend(th, tl, nrow=2, ncol=1)
dev.off()

# ED ANSWER: interannual climate vars don't seem to mediate the AGB-Dens relationship

# 3.-----------------------Does Fcomp/Dominant density mediate the agb-dens curve?-----------------
Fcomp <- readRDS("/Users/kah/Documents/WUE_MIP/Data/ED2/ED2.Fcomp.rds")
pft.lab=c("grass.c4", "tropic.early", "tropic.mid", "tropic.late", "grass.c3.temp", "pine.north", "pine.south", "conifer.late", "temp.decid.early", "temp.decid.mid", "temp.decid.late","ag1", "ag2", "ag3", "ag4","grass.c3.subtrop","Araucaria")

dimnames(Fcomp) <- list(year, paleon$num, pft.lab)

pfts <- c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
          "temp.decid.late", "grass.c3.temp" )

Fcomp.r <- Fcomp[,,pfts]

# get the year means for each fcomp
pine.north.y <- get.yrmeans(Fcomp.r[,,"pine.north"], "pine.north")
conifer.late.y <- get.yrmeans(Fcomp.r[,,"conifer.late"], "conifer.late")
temp.decid.early.y <- get.yrmeans(Fcomp.r[,,"temp.decid.early"], "temp.decid.early")
temp.decid.mid.y <- get.yrmeans(Fcomp.r[,,"temp.decid.mid"], "temp.decid.mid")
temp.decid.late.y <- get.yrmeans(Fcomp.r[,,"temp.decid.late"], "temp.decid.late")
grass.c3.temp.y <- get.yrmeans(Fcomp.r[,,"grass.c3.temp"], "grass.c3.temp")

all.y <- Reduce(function(x, y) merge(x, y, by = ,all=TRUE), 
                list(AGBdens, pine.north.y,conifer.late.y, temp.decid.early.y,
                     temp.decid.mid.y, temp.decid.late.y, grass.c3.temp.y))

source("outputs/preliminaryplots/ED_grid_map.png")
a <- ggplot(all.y, aes(AGB, TotalDens, color = pine.north))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+scale_colour_gradientn(colours = rev(terrain.colors(7)), limits = c(0,1))+ggtitle("Pine.North")
b <- ggplot(all.y, aes(AGB, TotalDens, color = conifer.late))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+scale_colour_gradientn(colours = rev(terrain.colors(7)), limits = c(0,1))+ggtitle("Conifer.Late")
c <- ggplot(all.y, aes(AGB, TotalDens, color = temp.decid.early))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+scale_colour_gradientn(colours = rev(terrain.colors(7)), limits = c(0,1))+ggtitle("Temp.Decid.early")
d <- ggplot(all.y, aes(AGB, TotalDens, color = temp.decid.mid))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+scale_colour_gradientn(colours = rev(terrain.colors(7)), limits = c(0,1))+ggtitle("Temp.Decid.mid")
e <- ggplot(all.y, aes(AGB, TotalDens, color = temp.decid.late))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+scale_colour_gradientn(colours = rev(terrain.colors(7)), limits = c(0,1))+ggtitle("Temp.Decid.late")
#ggplot(all.y, aes(AGB, TotalDens, color = grass.c3.temp))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+theme(legend.position = "none")

png(height = 12, width = 7, units = "in", res=300,"outputs/preliminaryplots/Dens/ED2_Dens_AGB_by_fcomp.png")
grid.arrange(a,b,c,d,e, nrow=5,ncol=1)
dev.off()

a <- ggplot(all.y, aes(AGBI, TotalDens, color = pine.north))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+scale_colour_gradientn(colours = rev(terrain.colors(7)), limits = c(0,1))+ggtitle("Pine.North")
b <- ggplot(all.y, aes(AGBI, TotalDens, color = conifer.late))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+scale_colour_gradientn(colours = rev(terrain.colors(7)), limits = c(0,1))+ggtitle("Conifer.Late")
c <- ggplot(all.y, aes(AGBI, TotalDens, color = temp.decid.early))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+scale_colour_gradientn(colours = rev(terrain.colors(7)), limits = c(0,1))+ggtitle("Temp.Decid.early")
d <- ggplot(all.y, aes(AGBI, TotalDens, color = temp.decid.mid))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+scale_colour_gradientn(colours = rev(terrain.colors(7)), limits = c(0,1))+ggtitle("Temp.Decid.mid")
e <- ggplot(all.y, aes(AGBI, TotalDens, color = temp.decid.late))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+scale_colour_gradientn(colours = rev(terrain.colors(7)), limits = c(0,1))+ggtitle("Temp.Decid.late")
#ggplot(all.y, aes(AGB, TotalDens, color = grass.c3.temp))+geom_point()+theme_bw()+ylab("Total Density (trees/ha)")+xlab("Aboveground biomass (kgC/m2)")+theme(legend.position = "none")

png(height = 12, width = 7, units = "in", res=300,"outputs/preliminaryplots/Dens/ED2_Dens_AGB_by_fcomp.png")
grid_arrange_shared_legend(a,b,c,d,e, nrow=5,ncol=1)
dev.off()
