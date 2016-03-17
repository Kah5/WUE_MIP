
# Sites
site.list <- c("PHA", "PHO", "PUN", "PBL", "PDL", "PMB")

clm.bgc.pft <- Fcomp.clm.bgc<- Tranp.clm.bgc <- CWDI.clm.bgc<-CPOOL_TO_GRESP_PFT.clm.bgc <- NPP.clm.bgc <- list() 
for(s in 1:length(site.list)){
  
  dir.clm.bgc <- file.path(model.dir, "CLM45BGC", paste0(site.list[s], ".CLM45BGC"))
  files.clm.bgc <- dir(dir.clm.bgc)
  
  #files.clm.bgc <- dir(dir.clm.bgc)
  clm.var.diversity.list <- list()
  clm.var.diversity <- c("Fcomp","Tranp", "CWDI", "CPOOL_TO_GRESP_PFT","LEAFC_LOSS_PFT", "FROOTC_LOSS_PFT", "CPOOL_TO_LIVESTEM_PFT","NPP_PFT" )
  div.list<- list()
  #-----------------------------------
  # File loop extracting time series by variable group
  for(i in 1:length(files.clm.bgc)){
    ncMT <- nc_open(file.path(dir.clm.bgc, files.clm.bgc[i]))
    for(v in 1:length(site.list)){
           Fcomp.clm.bgc[[v]] <- data.frame(ncvar_get(ncMT, clm.var.diversity[1]))
           Tranp.clm.bgc[[v]] <- data.frame(ncvar_get(ncMT, clm.var.diversity[2]))
           CWDI.clm.bgc[[v]] <- data.frame(ncvar_get(ncMT, clm.var.diversity[3]))
           CPOOL_TO_GRESP_PFT.clm.bgc[[v]] <- data.frame(ncvar_get(ncMT, clm.var.diversity[4]))
           NPP.clm.bgc[[v]] <- data.frame(ncvar_get(ncMT, clm.var.diversity[8]))
            }}
    nc_close(ncMT)      
  }
names(Fcomp.clm.bgc) <- names(Tranp.clm.bgc) <- 
  names(CWDI.clm.bgc)<- names(CPOOL_TO_GRESP_PFT.clm.bgc) <- names(NPP.clm.bgc) <- site.list


clm.pft <- c("bare", "TeNE", "BNE", "BNS", "TrBE", "TeBE", "TrBS", "TeBS", "BBS", "SBD", "TeSBS", "BSBS", "C3_arctic", "C3", "C4", "corn", "wheat")


# Adding pft labels to each sites
#names(clm.bgc) <- c(clm.bgc.var)
for(i in 1:length(site.list)){
  names(Tranp.clm.bgc[[i]]) <- clm.pft
}




###other way
clm.bgc.pft <- Fcomp.clm.bgc<- Tranp.clm.bgc <- CWDI.clm.bgc<-CPOOL_TO_GRESP_PFT.clm.bgc <- NPP.clm.bgc <- list() 
for(s in 1:length(site.list)){
  
  dir.clm.bgc <- file.path(model.dir, "CLM45BGC", paste0(site.list[s], ".CLM45BGC"))
  files.clm.bgc <- dir(dir.clm.bgc)
  
  #files.clm.bgc <- dir(dir.clm.bgc)
  clm.var.diversity.list <- list()
  clm.var.diversity <- c("Fcomp","Tranp", "CWDI", "CPOOL_TO_GRESP_PFT","LEAFC_LOSS_PFT", "FROOTC_LOSS_PFT", "CPOOL_TO_LIVESTEM_PFT","NPP_PFT" )
  div.list<- list()

  
  
###################good code
#this code works for fcomp and transp
clm.pft <- c("bare", "TeNE", "BNE", "BNS", "TrBE", "TeBE", "TrBS", "TeBS", "BBS", "SBD", "TeSBS", "BSBS", "C3_arctic", "C3", "C4", "corn", "wheat")

clm.fcomp <- list()
for(s in 1:length(site.list)){
  dir.clm.bgc <- file.path(model.dir, "CLM45BGC", paste0(site.list[s], ".CLM45BGC"))
  files.clm.bgc <- dir(dir.clm.bgc)
for(i in 1:length(files.clm.bgc)){
  ncMT <- nc_open(file.path(dir.clm.bgc, files.clm.bgc[i]))
  npft <- nrow(ncvar_get(ncMT, "Fcomp"))
  if(i == 1) clm.fcomp[[s]] <- as.data.frame(ncvar_get(ncMT, "Fcomp"))
  else clm.fcomp[[s]] <- rbind(clm.fcomp[[s]], ncvar_get(ncMT, "Fcomp"))
  nc_close(ncMT)      
}}

clm.Tranp <- list()
for(s in 1:length(site.list)){
  dir.clm.bgc <- file.path(model.dir, "CLM45BGC", paste0(site.list[s], ".CLM45BGC"))
  files.clm.bgc <- dir(dir.clm.bgc)
  
  for(i in 1:length(files.clm.bgc)){
    ncMT <- nc_open(file.path(dir.clm.bgc, files.clm.bgc[i]))
    npft <- nrow(ncvar_get(ncMT, "Fcomp"))
    if(i == 1) clm.Tranp[[s]] <- as.data.frame(ncvar_get(ncMT, "Tranp"))
    else clm.Tranp[[s]] <- rbind(clm.Tranp[[s]], ncvar_get(ncMT, "Tranp"))
    nc_close(ncMT)  
    
  } 
}

clm.CPOOL_GRESP <- list()
for(s in 1:length(site.list)){
  dir.clm.bgc <- file.path(model.dir, "CLM45BGC", paste0(site.list[s], ".CLM45BGC"))
  files.clm.bgc <- dir(dir.clm.bgc)
  
  for(i in 1:length(files.clm.bgc)){
    ncMT <- nc_open(file.path(dir.clm.bgc, files.clm.bgc[i]))
    npft <- nrow(ncvar_get(ncMT, "Fcomp"))
    if(i == 1) clm.CPOOL_GRESP[[s]] <- as.data.frame(ncvar_get(ncMT, "CPOOL_TO_GRESP_PFT"))
    else clm.CPOOL_GRESP[[s]] <- rbind(clm.CPOOL_GRESP[[s]], ncvar_get(ncMT, "CPOOL_TO_GRESP_PFT"))
    nc_close(ncMT)  
  } 
}

clm.NPP <- list()
for(s in 1:length(site.list)){
  dir.clm.bgc <- file.path(model.dir, "CLM45BGC", paste0(site.list[s], ".CLM45BGC"))
  files.clm.bgc <- dir(dir.clm.bgc)
  
  for(i in 1:length(files.clm.bgc)){
    ncMT <- nc_open(file.path(dir.clm.bgc, files.clm.bgc[i]))
    npft <- nrow(ncvar_get(ncMT, "Fcomp"))
    if(i == 1) clm.NPP[[s]] <- as.data.frame(ncvar_get(ncMT, "NPP_PFT"))
    else clm.NPP[[s]] <- rbind(clm.NPP[[s]], ncvar_get(ncMT, "NPP_PFT"))
    nc_close(ncMT)  
    
  } 
}


names(clm.Tranp)<- names(clm.fcomp)<- names(clm.NPP) <-names(clm.CPOOL_GRESP) <- site.list
for(i in 1:length(site.list)){
  names(clm.Tranp[[i]]) <- clm.pft
  names(clm.fcomp[[i]]) <- clm.pft
  names(clm.CPOOL_GRESP[[i]]) <- clm.pft
  names(clm.NPP[[i]])<- clm.pft
}



x11(width =11)
par(mfrow=c(2,1))
plot(clm.fcomp[[4]]$TeNE, col = "red")
plot(WUEi[[4]][,"clm.bgc"]*10, col = "red")
lines(WUEt[[4]][,"clm.bgc"], col = "blue")


#make billy's lake WUE a datafram
BL.wuei<- data.frame(Month = Month,
           Year = Year,
           wuei = WUEi[[4]][,"clm.bgc"])

BL.wuet<- data.frame(Month = Month,
                     Year = Year,
                     wuet = WUEt[[4]][,"clm.bgc"])
#make fcomp a dataframe
BL.fcomp<- data.frame(Month = Month,
                     Year = Year,
                     fcomp = clm.fcomp[[4]])
BL.Gc <- data.frame(Month = Month, 
                    Year = Year, 
                    Gc[[4]])
BL.co2 <- data.frame(Month = Month, 
                     Year = Year ,
                     CO2[[4]])

wuei.y <- aggregate(.~Year, data = BL.wuei, FUN = mean)
wuet.y <- aggregate(.~Year, data = BL.wuet, FUN = mean)
fcomp.y <- aggregate(.~Year, data = BL.fcomp, FUN = mean)
Gc.y <- aggregate(.~Year, data = BL.Gc, FUN = mean)
CO2.y <- aggregate(.~Year, data = BL.co2, FUN = mean)

plot(Gc.y$Year, Gc.y$ed.lu, type = "l", xlim= c(1750,2011))
lines(CO2.y$Year, CO2.y$ed.lu, xlim = c(1750,2011))

plot(Gc.y$ed.lu, wuei.y$wuei)
