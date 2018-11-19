# This script will look at Fcomp, Density, possibly BA? trends through time
# Author: Kelly Heilman
library(reshape2)
library(ggplot2)
library(tidyr)

# load the necessary data:
rm(list=ls()) # clear envt

# load the pft specific data:
ED2.Dens <- readRDS("Data/ED2/ED2.Dens.rds")
ED2.Fcomp <- readRDS("Data/ED2/ED2.Fcomp.RDS")
ED2.CO2 <- readRDS('Data/ED2/ED2.CO2.rds')
load("Data/PalEON_siteInfo_all.RData")

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


Fcomp <- ED2.Fcomp
Dens <- ED2.Dens
CO2 <- ED2.CO2


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
  plot(Dens.r[,i,"pine.north"] , ylim=c(10000, 1000000), col = 'red', ylab = "Dens", xlab = "Months since 850")
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


#save Dens.r and Fcomp.r
saveRDS(Dens.r, "Data/ED2.Dens.pftonly.rds")
saveRDS(Fcomp.r, "Data/ED2.Fcomp.pftonly.rds")

# --------------------Identify sites where Fractional composition shifts occur over time----------
fcomp.m <- melt(Fcomp.r)
colnames(fcomp.m) <- c("months", "num", "PFT", "fcomp")
fcomp.long <- left_join(fcomp.m, paleon[,c("num", "lon", "lat")], by = "num")
ggplot(fcomp.long[fcomp.long$num == 23,], aes(months,fcomp, color = PFT))+geom_line()+theme_bw()

# find the dominant fractional composition for each grid cell at each time point:
fcomp.wide <- fcomp.long %>% spread(key = PFT, value = fcomp)
DF <- fcomp.wide[2000:10000,5:ncol(fcomp.wide)]

# find the species with the maximum fractional composition:
MaxComp <- colnames(DF)[apply(DF,1,which.max)]

#----------------------Are there sites where density is bimodal?----------------

# make histograms of density by site and model
# plot histograms for each grid cell based on the model:
make.hists<- function(model){
if(model == "ED2"){
  Dens <- readRDS("Data/ED2/ED2.Dens.rds")
  CO2 <- ED2.CO2
  dimnames(Dens) <- list(timevec, paleon$num, pft.lab)
  pfts <- c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
          "temp.decid.late", "grass.c3.temp" )
  Dens.r <- Dens[,,pfts]
  
  # loop to calculate the total density and output histograms
  for(i in 1:length(paleon$num)){
    png(height=7, width = 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Dens/hists/",model,"_Dens_",paleon[i,]$latlon, ".png"))
    dens.site <- data.frame(Dens.r[,i,])
    dens.site$Totaldens <- rowSums(dens.site, na.rm=TRUE)
    print(ggplot(dens.site, aes(Totaldens))+geom_histogram()+theme_bw())
    dev.off()
    
    site.m <- melt(dens.site, id.vars = c("Totaldens"))
    png(height = 4, width= 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Dens/hists/",model,"_Dens_byPFT", paleon[i,]$latlon, '.png'))
    print(ggplot(site.m, aes(value, fill= variable))+geom_histogram()+facet_wrap(~variable)+theme_bw())
    dev.off()
  }
  
}else{
  Dens <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.Dens.rds")
  CO2 <- ED2.CO2
  yr <- 850:2010 # guess density is yearly
  pft.guess=c("BNE", "BINE", "BNS", "BIBS", "TeBS", "BeIBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G", "Total")
  dimnames(Dens) <- list(yr, paleon$num, pft.guess)
  
  pfts <- c("BNE" ,"BINE","BNS", "BIBS",   
            "TeBS","BeIBS","TeBE", "C3G", "Total" )
  Dens.r <- Dens[,,pfts]
  # loop that takes total 
  for(i in 1:length(paleon$num)){
    png(height=7, width = 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Dens/hists/",model,"_Dens_",paleon[i,]$latlon, ".png"))
    dens.site <- data.frame(Dens.r[,i,])
    #dens.site$Totaldens <- rowSums(dens.site, na.rm=TRUE)
    print(ggplot(dens.site, aes(Total))+geom_histogram()+theme_bw())
    dev.off()
    
    site.m <- melt(dens.site, id.vars = c("Total"))
    png(height = 4, width= 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Dens/hists/",model,"_Dens_byPFT", paleon[i,]$latlon, '.png'))
    print(ggplot(site.m, aes(value, fill= variable))+geom_histogram()+facet_wrap(~variable)+theme_bw())
    dev.off()
  }
  
}
}

make.hists(model = "ED2")
make.hists(model = "GUESS")

# there are some sites that have bimodal distn of tree density, but 
# these may be due to different species distributions


# -----------------What grid cells are significantly "bimodal"?--------------

# make one big function that finds the mean density and SD nd maps these out 
# make a dataframe of total density and density sd to get an idea of how much each grid cell varies:
# this function also outputs the df of mean density and sd that is used in the map.fires function below

make.dens.maps <- function(model) {
  if(model == "ED2"){
  library(modes)
  Dens <- readRDS("Data/ED2/ED2.Dens.rds")
  CO2 <- ED2.CO2
  dimnames(Dens) <- list(timevec, paleon$num, pft.lab)
  pfts <- c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
            "temp.decid.late", "grass.c3.temp" )
  Dens.r <- Dens[,,pfts]
  
  Dens.mean <- data.frame(num = paleon$num, 
                        lon = paleon$lon, 
                        lat = paleon$lat, 
                        latlon = paleon$latlon,
                        mean = NA, 
                        sd = NA, 
                        pval= NA, 
                        BC = NA)

  for(i in 1:length(paleon$num)){
    dens.site <- data.frame(Dens.r[,i,])
    dens.site$Totaldens <- rowSums(dens.site, na.rm=TRUE)
    Dens.mean[i,]$mean <- mean(dens.site$Totaldens, na.rm=TRUE)
    Dens.mean[i,]$sd <- sd(dens.site$Totaldens, na.rm=TRUE)
    Dens.mean[i,]$pval <- diptest::dip.test(na.omit(density(dens.site$Totaldens)$y))$p
    Dens.mean[i,]$BC <- bimodality_coefficient(na.omit(dens.site$Totaldens)) 
  }

  bimodal <- ifelse(Dens.mean$BC > 0.55 & Dens.mean$pval <= 0.05, "Bimodal", 'Unimodal')
  Dens.mean$bimodal <- bimodal
  # save the file for future use:
  saveRDS(Dens.mean, paste0(getwd(), "/outputs/data/ED2/ED2.meandens.rds"))
  
  # plot the mean density out on a map:
  states <- map_data("state")
  states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
  coordinates(states)<-~long+lat
  class(states)
  proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
  states <- spTransform(states,CRSobj = '+init=epsg:4326')
  mapdata <- data.frame(states)
  
  cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
  
  # map of mean density:
  png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_mean_dens_map.png"))
  print(ggplot(Dens.mean, aes(x = lon, y = lat, fill = mean))+geom_raster()+
    scale_fill_gradientn(colours = cbpalette, limits = c(0,3000000), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey')+
    geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw()+ ggtitle('Mean total density'))
  dev.off()
  
  rbpalette<- c('#67001f',
    '#b2182b',
    '#d6604d',
    '#f4a582',
    '#fddbc7',
    '#d1e5f0',
    '#92c5de',
    '#4393c3',
    '#2166ac',
    '#053061')
  # map of sd of density:
  png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_sd_dens_map.png"))
  print(ggplot(Dens.mean, aes(x = lon, y = lat, fill = sd))+geom_raster()+
    scale_fill_gradientn(colours = rev(rbpalette), limits = c(0,500000), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey')+
    geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('SD of total density'))
  dev.off()
  
  # map of places with significantly bimodal distribution in tree density over time:
  
  
  
  png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_bimodal_time_map.png"))
  print(ggplot(Dens.mean, aes(x = lon, y = lat, fill = bimodal))+geom_raster()+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Places where density has bimodal distn.'))
  dev.off()
  
  }else{ 
    
    ## if the model is LPJ-GUESS, then...
    # read in LPJ density again
    Dens <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.Dens.rds")
    CO2 <- ED2.CO2
    yr <- 850:2010 # guess density is yearly
    pft.guess=c("BNE", "BINE", "BNS", "BIBS", "TeBS", "BeIBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G", "Total")
    dimnames(Dens) <- list(yr, paleon$num, pft.guess)
    
    pfts <- c("BNE" ,"BINE","BNS", "BIBS",   
              "TeBS","BeIBS","TeBE", "C3G", "Total" )
    Dens.r <- Dens[,,pfts]
    
    
    library(modes)
    Dens.mean <- data.frame(num = paleon$num, 
                            lon = paleon$lon, 
                            lat = paleon$lat, 
                            latlon = paleon$latlon,
                            mean = NA, 
                            sd = NA, 
                            pval= NA, 
                            BC = NA)
    
    for(i in 1:length(paleon$num)){
      dens.site <- data.frame(Dens.r[,i,])
      Dens.mean[i,]$mean <- mean(dens.site$Total, na.rm=TRUE)
      Dens.mean[i,]$sd <- sd(dens.site$Total, na.rm=TRUE)
      Dens.mean[i,]$pval <- diptest::dip.test(na.omit(density(dens.site$Total)$y))$p
      Dens.mean[i,]$BC <- bimodality_coefficient(na.omit(dens.site$Total)) 
    }
    
    bimodal <- ifelse(Dens.mean$BC > 0.55 & Dens.mean$pval <= 0.05, "Bimodal", 'Unimodal')
    Dens.mean$bimodal <- bimodal
    
    # save the file for future use:
    saveRDS(Dens.mean, paste0(getwd(), "/outputs/data/GUESS/GUESS.meandens.rds"))
    
    # plot the mean density out on a map:
    states <- map_data("state")
    states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
    coordinates(states)<-~long+lat
    class(states)
    proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
    states <- spTransform(states,CRSobj = '+init=epsg:4326')
    mapdata <- data.frame(states)
    
    cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
    
    # map of mean density:
    png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/GUESS_mean_dens_map.png"))
    print( ggplot(Dens.mean, aes(x = lon, y = lat, fill = mean))+geom_raster()+
      scale_fill_gradientn(colours = cbpalette, limits = c(700,3000), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey')+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw()+ ggtitle('Mean total density'))
    dev.off()
    
    rbpalette<- c('#67001f',
                  '#b2182b',
                  '#d6604d',
                  '#f4a582',
                  '#fddbc7',
                  '#d1e5f0',
                  '#92c5de',
                  '#4393c3',
                  '#2166ac',
                  '#053061')
    # map of sd of density:
    png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/GUESS_sd_dens_map.png"))
    print( ggplot(Dens.mean, aes(x = lon, y = lat, fill = sd))+geom_raster()+
      scale_fill_gradientn(colours = rev(rbpalette), limits = c(100,900), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey')+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('SD of total density'))
    dev.off()
    
    # map of places with significantly bimodal distribution in tree density over time:
    
    bimodal <- ifelse(Dens.mean$BC > 0.55 & Dens.mean$pval <= 0.05, "Bimodal", 'Unimodal')
    Dens.mean$bimodal <- bimodal
    
    png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/GUESS_bimodal_time_map.png"))
    print(ggplot(Dens.mean, aes(x = lon, y = lat, fill = bimodal))+geom_raster()+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Places where density has bimodal distn.'))
    dev.off()
  }
}

# saves maps in preliminaryplots/Dens/maps
# note the color scale differences in tree density between models
make.dens.maps(model = "ED2")
make.dens.maps(model = "GUESS")

# ---------------------How does fire frequency vary across space?-----------------
# also how does it relate to mean density?

# need to fix this function:
#Error in eval(expr, envir, enclos) : object 'bimodal' not found 

map.fires <- function(model){
  
  ## for ED2:
  if(model == "ED2"){
    
    # compare this to a map of fire frequency:
    fire <- readRDS(paste0(getwd(),'/Data/ED2/ED2.Fire.rds'))
    dimnames(fire) <- list(timevec, paleon$num)
    df.fire <- data.frame(fire)
    fire.tots <- readRDS(paste0(getwd(), "/outputs/data/ED2/ED2.meandens.rds"))
    fire.tots$Fire.tots <- colSums(fire, na.rm=TRUE) # find the total number of fires at each grid cell
    fire.tots$countfires <- colSums(fire != 0, na.rm=TRUE)
    
    # plot the mean density out on a map:
    states <- map_data("state")
    states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
    coordinates(states)<-~long+lat
    class(states)
    proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
    states <- spTransform(states,CRSobj = '+init=epsg:4326')
    mapdata <- data.frame(states)
    
    cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
    
    
    # plot a map of total fire emmissions across space:
    png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_Fire_emmissions_map.png"))
      print(ggplot(fire.tots, aes(x = lon, y = lat, fill = Fire.tots))+geom_raster()+
      scale_fill_gradient(low = "#fee090", high = '#99000d',name ="Total Fire Emmissions kgC/m2/s", na.value = 'darkgrey')+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Total Fire emmissions 850-2011'))
    dev.off()
    
    # plot a map of the total instances of fire across space
    png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_fire_counts_map.png"))
      print(ggplot(fire.tots, aes(x = lon, y = lat, fill = countfires))+geom_raster()+
      scale_fill_gradient(low = "#fee090", high = '#99000d',name ="Total number of Fires", na.value = 'darkgrey')+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Total number of fires 850-2011'))
    dev.off()
    
    # basic plots of fire emmissions vs mean tree density and sd throught time:
    pdf(paste0("outputs/preliminaryplots/Dens/", model,"_density_vs_fire.pdf"))
      print(ggplot(fire.tots, aes(x = Fire.tots, y = mean, color = bimodal))+geom_point()+theme_bw()+xlab("Mean total density")+ylab("Total fire emmissions 850-2011"))
      print(ggplot(fire.tots, aes(x = Fire.tots, y = sd, color = bimodal))+geom_point()+theme_bw()+xlab("SD total density")+ylab("Total fire emmissions 850-2011"))
      
      # plots of fire counts vs. mean density and sd through time:
      print(ggplot(fire.tots, aes(x = countfires, y = mean, color = bimodal))+geom_point()+theme_bw()+xlab("Mean total density")+ylab("Number of fires 850-2011"))
      print(ggplot(fire.tots, aes(x = countfires, y = sd, color = bimodal))+geom_point()+theme_bw()+xlab("SD total density")+ylab("Number of fires 850-2011"))
    dev.off()
    
  }else{
    ## For LPJ-GUESS:
    
    # compare this to a map of fire frequency:
    fire <- readRDS(paste0(getwd(),'/Data/LPJ-GUESS/LPJ-GUESS.Fire.rds'))
    dimnames(fire) <- list(yr, paleon$num)
    df.fire <- data.frame(fire)
    fire.tots <- readRDS(paste0(getwd(), "/outputs/data/GUESS/GUESS.meandens.rds"))
    fire.tots$Fire.tots <- colSums(fire, na.rm=TRUE) # find the total number of fires at each grid cell
    fire.tots$countfires <- colSums(fire != 0, na.rm=TRUE)
    
    # plot the mean density out on a map:
    states <- map_data("state")
    states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
    coordinates(states)<-~long+lat
    class(states)
    proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
    states <- spTransform(states,CRSobj = '+init=epsg:4326')
    mapdata <- data.frame(states)
    
    cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
    
    
    # plot a map of total fire emmissions across space:
    png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/", model,"_Fire_emmissions_map.png"))
    print(ggplot(fire.tots, aes(x = lon, y = lat, fill = Fire.tots))+geom_raster()+
      scale_fill_gradient(low = "#fee090", high = '#99000d',name ="Total Fire Emmissions kgC/m2/s", na.value = 'darkgrey')+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Total Fire emmissions 850-2011'))
    dev.off()
    
    # plot a map of the total instances of fire across space
    png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/",model,"_fire_counts_map.png"))
    print(ggplot(fire.tots, aes(x = lon, y = lat, fill = countfires))+geom_raster()+
      scale_fill_gradient(low = "#fee090", high = '#99000d',name ="Total number of Fires", na.value = 'darkgrey')+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Total number of fires 850-2011'))
    dev.off()
    
    # basic plots of fire emmissions vs mean tree density and sd throught time:
    pdf(paste0("outputs/preliminaryplots/Dens/", model,"_density_vs_fire.pdf"))
    print(ggplot(fire.tots, aes(x = Fire.tots, y = mean, color = bimodal))+geom_point()+theme_bw()+xlab("Mean total density")+ylab("Total fire emmissions 850-2011"))
    print(ggplot(fire.tots, aes(x = Fire.tots, y = sd, color = bimodal))+geom_point()+theme_bw()+xlab("SD total density")+ylab("Total fire emmissions 850-2011"))
    
    # plots of fire counts vs. mean density and sd through time:
    print(ggplot(fire.tots, aes(x = countfires, y = mean, color = bimodal))+geom_point()+theme_bw()+xlab("Mean total density")+ylab("Number of fires 850-2011"))
    print(ggplot(fire.tots, aes(x = countfires, y = sd, color = bimodal))+geom_point()+theme_bw()+xlab("SD total density")+ylab("Number of fires 850-2011"))
    dev.off()
  }
}

map.fires(model = "ED2")
map.fires(model = "GUESS")


#----------- What is the sensitivity of total density to CO2 in ED2?-------------
# use ED2 CO2 for CO2 (can't find the output for LPJ-GUESS)

WUE.cor.co2 <- function(model){
  if(model == "ED2"){
  atm.co2 <- CO2[,1]
  # df for saving sensitiviey
  sens.mean <- data.frame(num = paleon$num, 
                        lon = paleon$lon, 
                        lat = paleon$lat, 
                        latlon = paleon$latlon,
                        corCO2 = NA, 
                        mean=NA,
                        sd = NA
                        )

  for(i in 1:length(paleon$num)){
    Dens.r <- readRDS(paste0(getwd(), "/Data/ED2/ED2.Dens.rds"))
    dens.site <- data.frame(Dens.r[,i,])
    dens.site$Totaldens <- rowSums(dens.site, na.rm=TRUE)
    sens.mean[i,]$corCO2 <- cor(dens.site$Totaldens, atm.co2)
    sens.mean[i,]$mean <- mean(dens.site$Totaldens, na.rm= TRUE)
    sens.mean[i,]$sd <- sd(dens.site$Totaldens, na.rm=TRUE)
  }

  rbpalette<- c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#d1e5f0',
                '#92c5de','#4393c3','#2166ac','#053061')
  
# it looks like in ED, the west is more highly positively corrllated with CO2:
  png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_dens_co2_cor_map.png"))
  ggplot(sens.mean, aes(x = lon, y = lat, fill = corCO2))+geom_raster()+
    scale_fill_gradientn(colours = rev(rbpalette), limits = c(-1,1), name ="correlation coefficient", na.value = 'darkgrey')+
    geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Correlation of Total Density with CO2')
  dev.off()

  }else{
    
    # for GUESS
    atm.co2 <- CO2[,1]
    # df for saving sensitiviey
    sens.mean <- data.frame(num = paleon$num, 
                            lon = paleon$lon, 
                            lat = paleon$lat, 
                            latlon = paleon$latlon,
                            corCO2 = NA, 
                            mean=NA,
                            sd = NA
    )
    
    for(i in 1:length(paleon$num)){
      Dens.r <- readRDS(paste0(getwd(), "/Data/LPJ-GUESS/LPJ-GUESS.Dens.rds"))
      dens.site <- data.frame(Dens.r[,i,])
      dens.site$Total <- dens.site[,,13] 
      #dens.site$Totaldens <- rowSums(dens.site, na.rm=TRUE)
      sens.mean[i,]$corCO2 <- cor(dens.site$Total, atm.co2)
      sens.mean[i,]$mean <- mean(dens.site$Total, na.rm= TRUE)
      sens.mean[i,]$sd <- sd(dens.site$Total, na.rm=TRUE)
    }
    
    rbpalette<- c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#d1e5f0',
                  '#92c5de','#4393c3','#2166ac','#053061')
    
    # it looks like in ED, the west is more highly positively corrllated with CO2:
    png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/",model,"_dens_co2_cor_map.png"))
    ggplot(sens.mean, aes(x = lon, y = lat, fill = corCO2))+geom_raster()+
      scale_fill_gradientn(colours = rev(rbpalette), limits = c(-1,1), name ="correlation coefficient", na.value = 'darkgrey')+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Correlation of Total Density with CO2')
    dev.off()
  }
}
WUE.cor.co2(model = "ED2")
WUE.cor.co2(model = "GUESS")



# from here on out, this is test code:

#------what is the sensitivity of Relativized Density to precip and Tair?--------------

#atm.co2 <- CO2[,1]

# read in the saved mean density file
mean.dens <- readRDS("outputs/data/ED2/ED2.meandens.rds")

# need to relativize the the density data:
# making the Relative Density file for ED2:
TotalDens <- CO2
RelDens <- CO2 
Dens.r <- readRDS(paste0(getwd(), "/Data/ED2/ED2.Dens.rds"))


for(i in 1:length(paleon$num)){
  
  dens.site <- data.frame(Dens.r[,i,])
  TotalDens[,i] <- rowSums(dens.site, na.rm=TRUE)
  RelDens[,i] <- TotalDens[,i]/mean(TotalDens[,i], na.rm=TRUE)
}

saveRDS(RelDens ,"outputs/data/ED2/ED2.RelDens.rds")


# making the Relative Density file for GUESS:
RelDens <- CO2
Dens.r <- readRDS(paste0(getwd(), "/Data/LPJ-GUESS/LPJ-GUESS.Dens.rds"))

for(i in 1:length(paleon$num)){
  dens.site <- data.frame(Dens.r[,i,])
  TotalDens[,i] <- dens.site[,13]
  RelDens[,i] <- TotalDens[,i]/mean(TotalDens[,i], na.rm=TRUE)
}

saveRDS(RelDens, "outputs/data/GUESS/GUESS.RelDens.rds")

# okay now lets look at sensitivity of "Relative Density" to Tair and Precip and WUE

# for model == "ED2"

ED.reldens <- readRDS("outputs/data/ED2/ED2.RelDens.rds")
ED.tair <- readRDS("Data/ED2/ED2.tair.rds")
ED.precip <- readRDS("Data/ED2/ED2.precipf.rds")
ED.IWUE <- readRDS("Data/ED2/ED2.IWUE.rds")
ED.WUEi <- readRDS("Data/ED2/ED2.WUEi.rds")
ED.WUEt <- readRDS("Data/ED2/ED2.WUEt.rds")
ED.CO2 <- readRDS("Data/ED2/ED2.CO2.rds")

# get the mean relative density (not sure if this is right--double check)
sec2yr <- 1*60*60*24*365.25
source("R/get.yrmeans.R")

reldens.y <- get.yrmeans(ED.reldens, "Rel.Dens")
tair.y <- get.yrmeans(ED.tair, "Tair")
tair.y$Tair.C <- tair.y$Tair - 273.15
precipf.y <- get.yrmeans(ED.precip, "precip")
precipf.y$precip.mm <- precipf.y$precip*sec2yr # convert to mm

IWUE.y <- get.yrmeans(ED.IWUE, "IWUE")
WUEi.y <- get.yrmeans(ED.WUEi, "WUEi")
WUEt.y <- get.yrmeans(ED.WUEt, "WUEt")
CO2.y <- get.yrmeans(ED.CO2, "CO2")


# use reduce to merge these all together
all.y <- Reduce(function(x, y) merge(x, y, by = c("Year", "Site"),all=TRUE), list(reldens.y, IWUE.y, WUEi.y, WUEt.y, CO2.y,
                                                                 tair.y, precipf.y))

# save the all.y
saveRDS(all.y, "outputs/data/ED2/ED2.alldat.yrmeans.rds")


# this function currently takes awhile
plot.sens.subset <- function(df, xname, yname, yrs){
  df <- df[,c("Year", "Site", xname, yname)]
  colnames(df) <- c("Year", "Site", "x", "y")
  df <- df[df$Year %in% yrs, ]
  lim <- quantile(df$x, .99, na.rm=T) # so we dont plot the outliers
  
  png(height = 12, width = 12, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/sensitivity/ED2_", xname,"_", yname,"_",yrs[1],"_",yrs[length(yrs)],"_sens.png"))
  print(ggplot(data = df, aes(x = x, y = y, color = Site))+geom_point()+xlim(0,lim)+
          ylab(yname)+ xlab(xname)+stat_smooth(color = "black") +theme_bw()+ theme(legend.position="none") )
  dev.off()
  
# write site level data to a pdf:
  pdf(paste0(getwd(),"/outputs/preliminaryplots/sensitivity/ED2_", xname,"_", yname,"_",yrs[1],"_",yrs[length(yrs)],"_sens_site.pdf"), 7, 5)
  for (i in 1:length(unique(df$Site))) {
    print(ggplot(df[df$Site %in% df$Site[i:(i+24)], ], 
                 aes(x, y)) + 
            geom_point() +
            facet_wrap(~ Site, ncol = 5, nrow = 5) +
            xlim(0,lim)+
            ylab(yname)+ xlab(xname)+stat_smooth(color = "black") +theme_bw())
  }
  dev.off()
}


plot.sens(all.y, "CO2", "Rel.Dens")
plot.sens(all.y, "Rel.Dens", "IWUE")
plot.sens(all.y, "Rel.Dens", "WUEi")
plot.sens(all.y, "Rel.Dens", "WUEt")
plot.sens(all.y, "Rel.Dens", "Tair")
plot.sens(all.y, "Rel.Dens", "precip")

# making the plots for the CO2 dominated era:

plot.sens.subset(all.y, "Rel.Dens", "CO2", 850:1800)
plot.sens.subset(all.y, "Rel.Dens", "CO2", 1800:2010)
plot.sens.subset(all.y, "Rel.Dens", "IWUE", 850:1800)
plot.sens.subset(all.y, "Rel.Dens", "WUEi", 850:1800)
plot.sens.subset(all.y, "Rel.Dens", "WUEt", 850:1800)
plot.sens.subset(all.y, "Rel.Dens", "Tair", 850:1800)
plot.sens.subset(all.y, "Rel.Dens", "precip", 850:1800)

# The above plots are all interesteing since precip, tair, WUE, density all vary by site. 
# may be helpful to determine a site specific sensitivity to CO2, precip, WUE
# OR relativize by mean of all sites at all time periods?



#-----------------Plot site level sensitivities to climat-------------
png(height = 20, width = 20, units = "in", res = 200, "outputs/preliminaryplots/sensitivity/ED2_TairC_relDens.png")
ggplot(all.y, aes(Tair.C, Rel.Dens))+geom_point()+facet_wrap(~Site)
dev.off()

png(height = 20, width = 20, units = "in", res = 200, "outputs/preliminaryplots/sensitivity/ED2_precipmm_relDens.png")
ggplot(all.y, aes(precip.mm, Rel.Dens))+geom_point()+facet_wrap(~Site)
dev.off()

png(height = 20, width = 20, units = "in", res = 200, "outputs/preliminaryplots/sensitivity/ED2_TairC_relDens.png")
ggplot(all.y, aes(CO2, Rel.Dens))+geom_point()+facet_wrap(~Site)
dev.off()

# read in the agbi + total dens as well:
dens.agbi.site <- readRDS( "outputs/data/ED2/ED2.agbi.dens.site.rds")
#dens.agbi <- readRDS( "outputs/data/ED2/ED2.agbi.rds")

all.df <- left_join(all.y, dens.agbi.site, by = c("Year", "Site"))

# in general, CO2, precip, and Tair all affect agbi at sites:
ggplot(all.df, aes(CO2, agbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes(precip.mm, agbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes(Tair.C, agbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes(CO2, GS_agb, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes( GS_agb, agbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")

ggplot(all.df, aes(Tair.C, GS_gwbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes(precip.mm, GS_gwbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes(CO2, GS_gwbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes( GS_gwbi, Dens, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes( GS_gwbi, GS_agb, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")


ggplot(all.df, aes(CO2, Dens))+geom_point()+stat_smooth()+stat_smooth()+theme(legend.position = "none")
library(mgcv)

agbi.p.gam <- gam(agbi ~ s(precip.mm), data = all.df)
agbi.p.t.gam <- gam(agbi ~ s(precip.mm) + s(Tair.C), data = all.df)
agbi.p.t.c.gam <- gam(agbi ~ s(precip.mm) + s(Tair.C) + s(CO2) + s(Year), data = all.df)
summary(agbi.p.t.c.gam)
plot(agbi.p.t.c.gam)

dens.p.t.c.gam <- gam(Dens ~ s(precip.mm) + s(Tair.C) + s(CO2) + s(Year), data = all.df)
summary(dens.p.t.c.gam)

agb.p.t.c.gam <- gam(GS_agb ~ s(precip.mm) + s(Tair.C) + s(CO2) + s(Year) + s(Dens), data = all.df)
summary(agb.p.t.c.gam)
plot(agb.p.t.c.gam)

rel.dens.p.t.c.gam <- gam(Rel.Dens ~ s(precip.mm) + s(Tair.C) + s(CO2) + s(agbi), data = all.df)
summary(rel.dens.p.t.c.gam)
plot(rel.dens.p.t.c.gam)

agbi.p.t.c.glm <- lm(agbi ~ precip.mm + Tair.C + CO2 + Site, data = all.df)
summary(agbi.p.t.c.glm )
saveRDS(all.df, "outputs/data/ED2/dens_agbi_climate_ED2.rds")



#-----What is sensitivity to climate when we relativize the climate data----

# for model == "ED2"

ED.reldens <- readRDS("outputs/data/ED2/ED2.RelDens.rds")
ED.tair <- readRDS("Data/ED2/ED2.tair.rds")
ED.precip <- readRDS("Data/ED2/ED2.precipf.rds")
ED.IWUE <- readRDS("Data/ED2/ED2.IWUE.rds")
ED.WUEi <- readRDS("Data/ED2/ED2.WUEi.rds")
ED.WUEt <- readRDS("Data/ED2/ED2.WUEt.rds")
ED.CO2 <- readRDS("Data/ED2/ED2.CO2.rds")

# get the mean relative density (not sure if this is right--double check)
# function to map out WUE increase across space:
map.WUE.inc <- function(WUEtype, var){
  
  png(height = 5, width = 8, units = "in", res=300, paste0(getwd(),"/outputs/preliminaryplots/WUE/ED2_",var,"inc_rel_pre1800.png"))
  print(ggplot(IWUEinc, aes(Year, IWUE, color = Site))+geom_point()+theme(legend.position = "none")+theme_bw()+theme(legend.position="none"))
  dev.off()
  
  a <- dcast(IWUEinc, Year ~ Site)
  
  
  slope.table <- data.frame(site = colnames(a[,2:length(a)]),
                            pval = NA, 
                            slope = NA)
  
  # find the slopes for WUE for 
  for(i in 1:length(paleon$num)){
    if(is.na(a[,i+1])){
      pval <- NA
      slope <- NA
    }else{
      mod <- summary( lm(a[,i+1] ~ Year,data = a) )
      pval <- mod$coefficients[2,4]
      slope <- mod$coefficients[2,1]
    }
    slope.table[i,]$pval <- pval
    slope.table[i,]$slope <- slope
  }
  
  paleon$site <- paste0("X", paleon$num)
  
  # merge paleon to site to plot:
  slope.xy <- merge(paleon, slope.table, by = "site")
  
  ggplot(slope.xy, aes(x = lon, y=lat, fill= slope))+geom_raster()
  
  
  states <- map_data("state")
  states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
  coordinates(states)<-~long+lat
  class(states)
  proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
  states <- spTransform(states,CRSobj = '+init=epsg:4326')
  mapdata <- data.frame(states)
  
  cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
  
  
  # map out the correlations with WUE
  png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_",var,"_inc_map.png"))
  print(ggplot(slope.xy, aes(x = lon, y=lat, fill= slope))+geom_raster()+
          scale_fill_gradient(low = "blue", high = "red", name ="slope (WUE increase/year)", na.value = 'darkgrey')+
          geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Slope of relative WUE increase/year'))
  dev.off()
}

map.WUE.inc(IWUEinc, "IWUE")
map.WUE.inc(WUEiinc, "WUEi")
map.WUE.inc(WUEtinc, "WUEt")

# get the relative jjameans for all the datavariables
source("R/get.yrmeans.R")
get.relative.jjameans <- function (model){
  if(model == "ED2"){
    ED.reldens <- readRDS("outputs/data/ED2/ED2.RelDens.rds")
    ED.tair <- readRDS("Data/ED2/ED2.tair.rds")
    ED.precip <- readRDS("Data/ED2/ED2.precipf.rds")
    ED.IWUE <- readRDS("Data/ED2/ED2.IWUE.rds")
    ED.WUEi <- readRDS("Data/ED2/ED2.WUEi.rds")
    ED.WUEt <- readRDS("Data/ED2/ED2.WUEt.rds")
    ED.CO2 <- readRDS("Data/ED2/ED2.CO2.rds")
    ED.LAI <- readRDS("Data/ED2/ED2.LAI.rds")
    ED.AGB <- readRDS("Data/ED2/ED2.AGB.rds")
    
    
    
    reldens.y <- get.JJAmeans(ED.reldens, "Rel.Dens")
    tair.y <- get.JJAmeans(ED.tair, "Tair")
    precipf.y <- get.JJAmeans(ED.precip, "precip")
    IWUE.y <- get.JJAmeans(ED.IWUE, "IWUE")
    WUEi.y <- get.JJAmeans(ED.WUEi, "WUEi")
    WUEt.y <- get.JJAmeans(ED.WUEt, "WUEt")
    CO2.y <- get.JJAmeans(ED.CO2, "CO2")# save the all.y
    LAI.y <- get.JJAmeans(ED.LAI, "LAI")# save the all.y
    AGB.y <- get.JJAmeans(ED.AGB, "AGB")
    
    all.jja <- Reduce(function(x, y) merge(x, y, by = ,all=TRUE), list(reldens.y, IWUE.y, WUEi.y, WUEt.y, CO2.y,
                                                                     tair.y, precipf.y, LAI.y, AGB.y))
    
    saveRDS(all.jja, "outputs/data/ED2/ED2.alldat.jjameans.rds")
    
    
    
    
    #relativize <- function(df, var){
     # test <- dcast(df, Year ~ Site)
      #Relvalue <- test
      
    #  for(i in 1:length(paleon$num)){
     #   Relvalue[,i+1] <- test[,i+1]/mean(test[,i+1], na.rm=TRUE)
      #}
      #m3 <- melt(Relvalue, id.vars = "Year")
      #colnames(m3) <- c("Year", "Site", var)
      #m3
    #}
    
   # tair.r <- relativize(tair.y, "tair")
  #  precipf.r <- relativize(precipf.y, "precipf")
   # IWUE.r <- relativize(IWUE.y, "IWUE")
  #  WUEi.r <- relativize(WUEi.y, "WUEi")
   # WUEt.r <- relativize(WUEt.y, "WUEt")
    
    # use reduce to merge these all together
    #jja.y <- Reduce(function(x, y) merge(x, y, by = ,all=TRUE), list(reldens.y,  CO2.y, IWUE.r, WUEi.r, WUEt.r,
     #                                                                tair.r, precipf.r))
    
    
    # sensitivity analyses still need some work:
    
    #------------- Are WUE increases higher in the West then?-------------------
    # look at increases in WUE relative to 850-1800 mean:
    # using the WUEi.y summer growing season averages
    
    relativize.period <- function(df,period, var){
    
      test <- dcast(df, Year ~ Site)
      Relvalue <- test
      
      for(i in 1:length(paleon$num)){
        Relvalue[,i+1] <- test[,i+1]/mean(test[test$Year %in% period ,i+1], na.rm=TRUE)
      }
      m3 <- melt(Relvalue, id.vars = "Year")
      colnames(m3) <- c("Year", "Site", paste0("rel_",var))
      m3
    }
    
    IWUEinc <- relativize.period(IWUE.y, 850:1800, "IWUE")
    WUEiinc <- relativize.period(WUEi.y, 850:1800, "WUEi")
    WUEtinc <- relativize.period(WUEt.y, 850:1800, "WUEt")
    # rename variables:
    
    
    # save the jja relative wue, density and overall precipitation to a single df:
    all.jja.rel <-  Reduce(function(x, y) merge(x, y, by =,all=TRUE), list(reldens.y, IWUEinc, WUEiinc, WUEtinc,reldens.y, IWUE.y, WUEi.y, WUEt.y, CO2.y,
                                                                           tair.y, precipf.y, LAI.y, AGB.y))
    
    saveRDS(all.jja.rel, "outputs/data/ED2/ED2.all.jja.rel.rds")
    
    }else{
    
    # if the model == LPJ.GUESS
    G.reldens <- readRDS("outputs/data/GUESS/GUESS.RelDens.rds")
    G.tair <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.tair.rds")
    G.precip <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.precipf.rds")
    G.IWUE <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.IWUE.rds")
    G.WUEi <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.WUEi.rds")
    G.WUEt <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.WUEt.rds")
    G.CO2 <- readRDS("Data/ED2/ED2.CO2.rds")
    G.LAI <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.LAI.rds")
    G.AGB <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.AGB.rds")
    
    reldens.y <- get.JJAmeans(G.reldens, "Rel.Dens")
    tair.y <- get.JJAmeans(G.tair, "Tair")
    precipf.y <- get.JJAmeans(G.precip, "precip")
    IWUE.y <- get.JJAmeans(G.IWUE, "IWUE")
    WUEi.y <- get.JJAmeans(G.WUEi, "WUEi")
    WUEt.y <- get.JJAmeans(G.WUEt, "WUEt")
    CO2.y <- get.JJAmeans(G.CO2, "CO2")# save the all.y
    LAI.y <- get.JJAmeans(G.LAI, "LAI")
    #AGB.y <- get.JJAmeans(G.AGB, "AGB")
    dimnames(G.AGB) <- list(yr, site.list, pft.guess)
    totAGB.y <- data.frame(G.AGB[,,"Total"])
    totAGB.y$Year <- yr
    AGB.y <- melt(totAGB.y, id.vars = c("Year"))
    colnames(AGB.y) <- c("Year", "Site", "AGB")
    
    all.jja <- Reduce(function(x, y) merge(x, y, by = ,all=TRUE), list(reldens.y, IWUE.y, WUEi.y, WUEt.y, CO2.y,
                                                                       tair.y, precipf.y, LAI.y, AGB.y))
    
    saveRDS(all.jja, "outputs/data/GUESS/GUESS.alldat.jjameans.rds")
    
  
    #relativize <- function(df, var){
      #test <- dcast(df, Year ~ Site)
      #Relvalue <- test
      
    #  for(i in 1:length(paleon$num)){
     #   Relvalue[,i+1] <- test[,i+1]/mean(test[,i+1], na.rm=TRUE)
      #}
    #  m3 <- melt(Relvalue, id.vars = "Year")
     # colnames(m3) <- c("Year", "Site", var)
      #m3
    #}
    
    #tair.r <- relativize(tair.y, "tair")
    #precipf.r <- relativize(precipf.y, "precipf")
    #IWUE.r <- relativize(IWUE.y, "IWUE")
    #WUEi.r <- relativize(WUEi.y, "WUEi")
    #WUEt.r <- relativize(WUEt.y, "WUEt")
    
    # use reduce to merge these all together
    #jja.y <- Reduce(function(x, y) merge(x, y, by = ,all=TRUE), list(reldens.y,  CO2.y, IWUE.r, WUEi.r, WUEt.r,
     #                                                                tair.r, precipf.r))
    
    
    # sensitivity analyses still need some work:
    
    #------------- Are WUE increases higher in the West then?-------------------
    # look at increases in WUE relative to 850-1800 mean:
    # using the WUEi.y summer growing season averages
    
    relativize.period <- function(df,period, var){
      
      test <- dcast(df, Year ~ Site)
      Relvalue <- test
      
      for(i in 1:length(paleon$num)){
        Relvalue[,i+1] <- test[,i+1]/mean(test[test$Year %in% period ,i+1], na.rm=TRUE)
      }
      m3 <- melt(Relvalue, id.vars = "Year")
      colnames(m3) <- c("Year", "Site", paste0("rel_",var))
      m3
    }
    
    IWUEinc <- relativize.period(IWUE.y, 850:1800, "IWUE")
    WUEiinc <- relativize.period(WUEi.y, 850:1800, "WUEi")
    WUEtinc <- relativize.period(WUEt.y, 850:1800, "WUEt")
    # rename variables:
    
    
    # save the jja relative wue, density and overall precipitation to a single df:
    all.jja.rel <-  Reduce(function(x, y) merge(x, y, by =,all=TRUE), list(reldens.y, IWUEinc, WUEiinc, WUEtinc,reldens.y, IWUE.y, WUEi.y, WUEt.y, CO2.y,
                                                                           tair.y, precipf.y, LAI.y, AGB.y))
    
    saveRDS(all.jja.rel, "outputs/data/GUESS/GUESS.all.jja.rel.rds")
  }
}

get.relative.jjameans(model = "ED2")
get.relative.jjameans(model = "GUESS")


#-------------- Is the increase in WUE related to changes in density?-----------
# also are these increases linked to spatial patterns in precip and temperature

# Main Question: Is climate or stand structure more important in determining WUE increase?

# Steps for analysis:

# 1. Get a value for each growing season/year at each point (above)
# 2. Take all the points together and model of WUE based on tree density (relative), CO2, temp, precip


# for jja mean data:
#jja.y <- readRDS("outputs/data/ED2/ED2.alldat.yrmeans.rds")

make_post_1800_plots <- function(model){
  if(model == "ED2"){

  jja.y <- readRDS("outputs/data/ED2/ED2.all.jja.rel.rds")
  
  # get the years from 1800 - 2010:
  jja.subset <- jja.y[jja.y$Year %in% 1800:2010, ]
  
  
  
  # Plot basic Trends through time
  
  
  # maybe plot these by mean precipf, mean tair, and mean WUE?
  
  
  # Q: What is the effect of WUE, precip, tair on rel. density?
  # basic plots over the whole domain
  
  # find the mean precip rate
  precip <- jja.subset[,c("Site", "precip", "Year")]
  pr.means <- aggregate(precip ~ Site, data = precip, FUN = mean)
  colnames(pr.means) <- c("Site", "mean.precipf")
  jja.subset <- merge(jja.subset, pr.means, by = "Site")# add site means to the jja.subset df
  
  # print these all to a pdf:
  pdf("outputs/preliminaryplots/post_1800_changes/ED2/post_1800_timeseries.pdf")
  
  ggplot(jja.subset, aes(Rel.Dens, Tair, color = Site))+geom_point()+theme(legend.position = "none")
  ggplot(jja.subset, aes(Rel.Dens, IWUE, color = Site))+geom_point()+theme(legend.position = "none")
  ggplot(jja.subset, aes(Rel.Dens, precip, color = Site))+geom_point()+theme(legend.position = "none")
  ggplot(jja.subset, aes(LAI, precip, color = Site))+geom_point()+theme(legend.position = "none")
  ggplot(jja.subset, aes(AGB, precip, color = Site))+geom_point()+theme(legend.position = "none")
  
  
  
  # now lets make some prelimary plots colored by mean precip
  
  ggplot(jja.subset, aes(Rel.Dens, Tair, color = mean.precipf ))+geom_point()
  ggplot(jja.subset, aes(Rel.Dens, precip, color = mean.precipf))+geom_point()
  ggplot(jja.subset, aes(Rel.Dens, CO2, color = mean.precipf))+geom_point()
  ggplot(jja.subset, aes(Rel.Dens, IWUE, color = mean.precipf))+geom_point()
  dev.off()
  
  
  # Plot basic Trends through time colored by mean precipf
  pdf("outputs/preliminaryplots/post_1800_changes/ED2/post_1800_timeseries_by_precip.pdf")
  
  ggplot(jja.subset, aes(Year, Tair, color = mean.precipf))+geom_point()
  ggplot(jja.subset, aes(Year, precip, color = mean.precipf))+geom_point()
  ggplot(jja.subset, aes(Year, CO2, color = mean.precipf))+geom_point()
  ggplot(jja.subset, aes(Year, Rel.Dens, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  
  # based on these plots, it looks like places with lower mean precipf over 1800-2010 show larger inc in density:
  # are these also places with increases in WUE
  # these plots are omitting several outliers:
  
  ggplot(jja.subset, aes(Year, IWUE, color = mean.precipf))+ylim(0,100)+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(Year, WUEt, color = mean.precipf))+ylim(0,100)+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(Year, WUEi, color = mean.precipf))+ylim(0,100)+geom_point()+scale_color_gradient(low = "red", high = "blue")
  
  ggplot(jja.subset, aes(Year, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(Year, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(Year, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  
  dev.off()
  
  
  
  ggplot(jja.subset, aes(Rel.Dens, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(Rel.Dens, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(Rel.Dens, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(AGB, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(AGB, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(AGB, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  
  
  # lets also look at the places the increase relative to mean annual temperature
  Tair <- jja.subset[,c("Site", "Tair", "Year")]
  tair.means <- aggregate(Tair ~ Site, data = Tair, FUN = mean)
  colnames(tair.means) <- c("Site", "mean.tair")
  jja.subset <- merge(jja.subset, tair.means, by = "Site")# add site means to the jja.subset df
  
  
  pdf("outputs/preliminaryplots/post_1800_changes/ED2/post_1800_timeseries_by_tair.pdf")
  
  # plot relative increase in WUE jja temp subset:
  ggplot(jja.subset, aes(Year, rel_IWUE, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(Year, rel_WUEt, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(Year, rel_WUEi, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  
  # plot relative increase in density with jja temp subset:
  ggplot(jja.subset, aes(Year, Tair, color = mean.tair))+geom_point()
  ggplot(jja.subset, aes(Year, precip, color = mean.tair))+geom_point()
  ggplot(jja.subset, aes(Year, Rel.Dens, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  
  dev.off()
  
  # lets make these outputs better and on the same png file:
  source("R/grid_arrange_shared_legend.R")
  
  rd <- ggplot(jja.subset, aes(Year, Rel.Dens, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
  pr <- ggplot(jja.subset, aes(Year, precip, color = mean.precipf))+geom_point()+theme_bw()
  IWUEp <- ggplot(jja.subset, aes(Year, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
  WUEtp <- ggplot(jja.subset, aes(Year, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
  WUEip <- ggplot(jja.subset, aes(Year, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
  
  #X11(width =12)
  library(grid)
  library(gridExtra)
  
  png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/ED2/post1800_dens_timeserise_by_precip.png")
  grid_arrange_shared_legend(rd, pr, WUEtp, nrow=3, ncol=1, position = "right")
  dev.off()
  
  # now lets subset the reldens and WUE increases by precip
  rdhigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, Rel.Dens, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ylim(0,4)+ggtitle("Sites with higher than average precip")+ylab("Relative Density")
  rdlow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, Rel.Dens, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ylim(0,4)+ggtitle("Sites with lower than average precip")+ylab("Relative Density")
  
  png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/ED2/post1800_dens_timeseries_highlo_precip.png")
  grid_arrange_shared_legend(rdhigh, rdlow, nrow=2, ncol=1, position = "right")
  dev.off()
  
  # plot ts of AGB increases colored by precip 
  ahigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, AGB, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with higher than average precip")+ylab("Relative Density")+ylim(0,45)
  alow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, AGB, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with lower than average precip")+ylab("Relative Density")+ylim(0,45)
  
  png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/ED2/post1800_AGB_timeseries_highlo_precip.png")
  grid_arrange_shared_legend(ahigh, alow, nrow=2, ncol=1, position = "right")
  dev.off()
  
  # plot ts of LAI increases colored by precip
  lhigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, LAI, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with higher than average precip")+ylab("Relative Density")+ylim(0,12)
  llow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, LAI, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with lower than average precip")+ylab("Relative Density")+ylim(0,12)
  
  png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/ED2/post1800_LAI_timeseries_highlo_precip.png")
  grid_arrange_shared_legend(lhigh, llow, nrow=2, ncol=1, position = "right")
  dev.off()
  
  # plot high and lows for LAI
  lhigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, LAI, color = mean.precipf))+geom_point()+ylim(0,11)+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with higher than average precip")+ylab("LAI")
  llow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, LAI, color = mean.precipf))+geom_point()+ylim(0, 11)+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with lower than average precip")+ylab("LAI")
  
  png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/ED2/post1800_dens_timeseries_highlo_precip.png")
  grid_arrange_shared_legend(lhigh, llow, nrow=2, ncol=1, position = "right")
  dev.off()
  
  # plot high and lows for AGB:
  ahigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, AGB, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with higher than average precip")+ylab("Aboveground Biomass")
  alow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, AGB, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with lower than average precip")+ylab("Aboveground Biomass")
  
  png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/ED2/post1800_AGB_timeseries_highlo_precip.png")
  grid_arrange_shared_legend(ahigh, alow, nrow=2, ncol=1, position = "right")
  dev.off()
  
  # do the same thing for WUE
  IWUEphigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative IWUE at high precip sites")+ylim(0,4)
  WUEtphigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative WUEt at high precip sites")+ylim(0,4)
  WUEiphigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative WUEi at high precip sites")+ylim(0,20)
  
  IWUEplow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative IWUE at low precip sites")+ylim(0,4)
  WUEtplow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative WUEt at low precip sites")+ylim(0,4)
  WUEiplow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative WUEi at low precip sites")+ylim(0,20)
  
  png(height = 12, width = 12, units = "in",res=200,"outputs/preliminaryplots/post_1800_changes/ED2/post1800_WUE_timeseries_by_precip.png")
  grid_arrange_shared_legend(IWUEphigh, IWUEplow, WUEtphigh, WUEtplow, WUEiphigh, WUEiplow, ncol=2, nrow=3, position = "right")
  dev.off()
  
  
  # plot out the temperatures:
  rdt <- ggplot(jja.subset, aes(Year, Rel.Dens, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
  prt <- ggplot(jja.subset, aes(Year, precip, color = mean.tair))+geom_point()+theme_bw()
  IWUEt <- ggplot(jja.subset, aes(Year, rel_IWUE, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
  WUEtt <- ggplot(jja.subset, aes(Year, rel_WUEt, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
  WUEit <- ggplot(jja.subset, aes(Year, rel_WUEi, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
  
  png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/ED2/post1800_dens_timeserise_by_tair.png")
  grid_arrange_shared_legend(rdt, prt, WUEtt, nrow=3, ncol=1, position = "right")
  dev.off()
  
  saveRDS(jja.subset, "outputs/data/ED2/jja.subset.rds")
  
  }else{
    
    # for LPJ-GUESS:
    
    jja.y <- readRDS("outputs/data/GUESS/GUESS.all.jja.rel.rds")
    
    # get the years from 1800 - 2010:
    jja.subset <- jja.y[jja.y$Year %in% 1800:2010, ]
    
    
    # Q: What is the effect of WUE, precip, tair on rel. density?
    # basic plots over the whole domain
    #paleon$Site <- paste0("X", paleon$num)
    #GUESS.jja<- merge(jja.subset, paleon, by = "Site")
    #png("GUESS.map.png")
    #ggplot(GUESS.jja, aes(x = lon, y = lat, fill=mean.precipf))+geom_raster()
    #dev.off()
    #saveRDS(GUESS.jja, "GUESS.jja.rds")
    #jja.ED <- readRDS("outputs/data/ED2/ED2.all.jja.rel.rds")
    
    # get the years from 1800 - 2010:
   # jja.subset <- jja.y[jja.ED$Year %in% 1800:2010, ]
    
    
    
    # print these all to a pdf:
    pdf("outputs/preliminaryplots/post_1800_changes/GUESS/post_1800_timeseries.pdf")
    
    ggplot(jja.subset, aes(Rel.Dens, Tair, color = Site))+geom_point()+theme(legend.position = "none")
    ggplot(jja.subset, aes(Rel.Dens, IWUE, color = Site))+geom_point()+theme(legend.position = "none")
    ggplot(jja.subset, aes(Rel.Dens, precip, color = Site))+geom_point()+theme(legend.position = "none")
    ggplot(jja.subset, aes(LAI, precip, color = Site))+geom_point()+theme(legend.position = 'none')
    ggplot(jja.subset, aes(AGB, IWUE, color = Site))+geom_point()+theme(legend.position = "none")
    
    dev.off()
    
    precip <- jja.subset[,c("Site", "precip", "Year")]
    pr.means <- aggregate(precip ~ Site, data = precip, FUN = mean)
    colnames(pr.means) <- c("Site", "mean.precipf")
    jja.subset <- merge(jja.subset, pr.means, by = "Site")# add site means to the jja.subset df
    
    
    # just merging by had here because it wasn't working above
    # add the relative increases in WUE for ED
    
    # now lets make some prelimary plots of 
    
    ggplot(jja.subset, aes(Rel.Dens, Tair, color = mean.precipf ))+geom_point()
    ggplot(jja.subset, aes(Rel.Dens, precip, color = mean.precipf))+geom_point()
    ggplot(jja.subset, aes(Rel.Dens, CO2, color = mean.precipf))+geom_point()
    ggplot(jja.subset, aes(Rel.Dens, IWUE, color = mean.precipf))+geom_point()
    
    # Plot basic Trends through time colored by mean precipf
    pdf("outputs/preliminaryplots/post_1800_changes/GUESS/post_1800_timeseries_by_precip.pdf")
    
    ggplot(jja.subset, aes(Year, Tair, color = mean.precipf))+geom_point()
    ggplot(jja.subset, aes(Year, precip, color = mean.precipf))+geom_point()
    ggplot(jja.subset, aes(Year, CO2, color = mean.precipf))+geom_point()
    ggplot(jja.subset, aes(Year, Rel.Dens, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
    ggplot(jja.subset, aes(Year, AGB, color = mean.precipf))+geom_point()
    
    # based on these plots, it looks like places with lower mean precipf over 1800-2010 show larger inc in density:
    # are these also places with increases in WUE
    # these plots are omitting several outliers:
    
    ggplot(jja.subset, aes(Year, IWUE, color = mean.precipf))+ylim(0,100)+geom_point()+scale_color_gradient(low = "red", high = "blue")
    ggplot(jja.subset, aes(Year, WUEt, color = mean.precipf))+ylim(0,100)+geom_point()+scale_color_gradient(low = "red", high = "blue")
    ggplot(jja.subset, aes(Year, WUEi, color = mean.precipf))+ylim(0,100)+geom_point()+scale_color_gradient(low = "red", high = "blue")
    
    ggplot(jja.subset, aes(Year, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
    ggplot(jja.subset, aes(Year, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
    ggplot(jja.subset, aes(Year, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
    
    dev.off()
    
   
    # lets also look at the places the increase relative to mean annual temperature
    Tair <- jja.subset[,c("Site", "Tair", "Year")]
    tair.means <- aggregate(Tair ~ Site, data = Tair, FUN = mean)
    colnames(tair.means) <- c("Site", "mean.tair")
    jja.subset <- merge(jja.subset, tair.means, by = "Site")# add site means to the jja.subset df
    
    
    pdf("outputs/preliminaryplots/post_1800_changes/GUESS/post_1800_timeseries_by_tair.pdf")
    
    # plot relative increase in WUE jja temp subset:
    ggplot(jja.subset, aes(Year, rel_IWUE, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")
    ggplot(jja.subset, aes(Year, rel_WUEt, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")
    ggplot(jja.subset, aes(Year, rel_WUEi, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")
    
    # plot relative increase in density with jja temp subset:
    ggplot(jja.subset, aes(Year, Tair, color = mean.tair))+geom_point()
    ggplot(jja.subset, aes(Year, precip, color = mean.tair))+geom_point()
    ggplot(jja.subset, aes(Year, Rel.Dens, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")
    ggplot(jja.subset, aes(Year, AGB, color = mean.tair))+geom_point()
    dev.off()
    
    # lets make these outputs better and on the same png file:
    source("R/grid_arrange_shared_legend.R")
    
    rd <- ggplot(jja.subset, aes(Year, Rel.Dens, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
    pr <- ggplot(jja.subset, aes(Year, precip, color = mean.precipf))+geom_point()+theme_bw()
    IWUEp <- ggplot(jja.subset, aes(Year, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
    WUEtp <- ggplot(jja.subset, aes(Year, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
    WUEip <- ggplot(jja.subset, aes(Year, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
    
    
    #X11(width =12)
    library(grid)
    library(gridExtra)
    
    png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/GUESS/post1800_dens_timeserise_by_precip.png")
    grid_arrange_shared_legend(rd, pr, WUEtp, nrow=3, ncol=1, position = "right")
    dev.off()
    
    # now lets subset the reldens and WUE increases by precip
    rdhigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, Rel.Dens, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ylim(0,4)+ggtitle("Sites with higher than average precip")+ylab("Relative Density")
    rdlow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, Rel.Dens, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ylim(0,4)+ggtitle("Sites with lower than average precip")+ylab("Relative Density")
    
    png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/GUESS/post1800_dens_timeseries_highlo_precip.png")
    grid_arrange_shared_legend(rdhigh, rdlow, nrow=2, ncol=1, position = "right")
    dev.off()
    
    # now lets subset the AGB and WUE increases by precip
    ahigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, AGB, color = mean.precipf))+geom_point()+ylim(0,13)+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with higher than average precip")+ylab("Aboveground Biomass")
    alow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, AGB, color = mean.precipf))+geom_point()+ylim(0,13)+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with lower than average precip")+ylab("Aboveground Biomass")
    
    png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/GUESS/post1800_AGB_timeseries_highlo_precip.png")
    grid_arrange_shared_legend(ahigh, alow, nrow=2, ncol=1, position = "right")
    dev.off()
    
    lhigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, LAI, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+ylim(0,6)+theme_bw()+ggtitle("Sites with higher than average precip")+ylab("LAI")
    llow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, LAI, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+ylim(0,6)+theme_bw()+ggtitle("Sites with lower than average precip")+ylab("LAI")
    
    png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/GUESS/post1800_LAI_timeseries_highlo_precip.png")
    grid_arrange_shared_legend(lhigh, llow, nrow=2, ncol=1, position = "right")
    dev.off()
    
    # do the same thing for WUE
    IWUEphigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative IWUE at high precip sites")+ylim(0,4)
    WUEtphigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative WUEt at high precip sites")+ylim(0,4)
    WUEiphigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative WUEi at high precip sites")+ylim(0,20)
    
    IWUEplow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative IWUE at low precip sites")+ylim(0,4)
    WUEtplow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative WUEt at low precip sites")+ylim(0,4)
    WUEiplow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative WUEi at low precip sites")+ylim(0,20)
    
    png(height = 12, width = 12, units = "in",res=200,"outputs/preliminaryplots/post_1800_changes/GUESS/post1800_WUE_timeseries_by_precip.png")
    grid_arrange_shared_legend(IWUEphigh, IWUEplow, WUEtphigh, WUEtplow, WUEiphigh, WUEiplow, ncol=2, nrow=3, position = "right")
    dev.off()
    
    
    # plot out the temperatures:
    rdt <- ggplot(jja.subset, aes(Year, Rel.Dens, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
    prt <- ggplot(jja.subset, aes(Year, precip, color = mean.tair))+geom_point()+theme_bw()
    IWUEt <- ggplot(jja.subset, aes(Year, rel_IWUE, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
    WUEtt <- ggplot(jja.subset, aes(Year, rel_WUEt, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
    WUEit <- ggplot(jja.subset, aes(Year, rel_WUEi, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
    
    png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/GUESS/post1800_dens_timeserise_by_tair.png")
    grid_arrange_shared_legend(rdt, prt, WUEtt, nrow=3, ncol=1, position = "right")
    dev.off()
    
    saveRDS(jja.subset, "outputs/data/GUESS/jja.subset.rds")
    
  }
  
  

  
  
  
  
  
# now to quantitativily analysize theses
# fit a gam?
# relative density increases over time
denst <- gam(Rel.Dens ~ s(Year), data = jja.subset)
plot(time)

tairt <- gam( Tair ~ s(Year), data = jja.subset )
plot(tairt)

precipt <- gam(precip ~ s(Year) + random(list(Site)), data = jja.subset)
plot(precipt)

# using a gam
g <- gam(Rel.Dens ~ s(Year) + s(WUEt) + s(precip) + s(Tair), data = jja.subset)
summary(g)
plot(g)
