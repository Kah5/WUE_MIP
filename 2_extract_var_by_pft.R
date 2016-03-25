
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
names(Fcomp.clm.bgc) <- names(Tranp.clm.bgc) <- site.list
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

}
  
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
pdf("clm_pft_WUEallsites.pdf")
for(s in 1:length(site.list)){
plot(clm.fcomp[[s]]$TeNE, col = "red", ylim = c(0, 1), ylab = "PFT abundance", 
     xlab= "Time (months)", 
     main = paste("PFT over time at", site.list[s]))
points(clm.fcomp[[s]]$TeBS, col = "green")
legend("topleft", pch = 15,col = c("red", "green"), c("TeNE", "TeBS"))

plot(WUEi[[s]][,"clm.bgc"]*10, type = "l", ylim= c(0, 40),col = "red", ylab = "WUE",  xlab= "Time (months)",
     main = paste("Metrics of WUE over time", site.list[s]))
lines(WUEt[[s]][,"clm.bgc"], col = "blue")
lines(IWUE[[s]][,"clm.bgc"], col = "green")
legend("topright", pch = "-", col = c("red", "blue", "green"), c("WUEi", "WUEt", "IWUE"))
}
dev.off()




#this loop doesn't work because some sites have no veg pft shift
pdf("wue_veg_change_boxplots.pdf")
for(s in 1:length(site.list)){
s <- 4
pdf("BL_veg_change_boxplots.pdf")
#create an index for when pft is only TeBS
TeBSonly<- clm.fcomp[[s]]$TeBS > 0.99
#index for when pft is a "stable" mix of TeBS and TeNE (roughly)
TeBS.TeNE <- clm.fcomp[[s]]$TeBS < 0.65
#fix the shift pwart
#indext when pft is shifting, but not yet just TeBS
TeBS.shift <-  clm.fcomp[[s]]$TeBS >= 0.7 & clm.fcomp[[s]]$TeBS <  0.99 


##Get the WUEi and WUEt for each of these time periods 
WUEii<- WUEi[[s]][,"clm.bgc"] 
WUEi.TeBS <- WUEii[TeBSonly]
WUEi.TeBS.TeNE <- WUEii[TeBS.TeNE]
WUEi.shift <- WUEii[clm.fcomp[[s]]$TeBS >= 0.65 & clm.fcomp[[s]]$TeBS <  0.99 ]

WUEtt<- WUEt[[s]][,"clm.bgc"] 
WUEt.TeBS <- WUEtt[TeBSonly]
WUEt.TeBS.TeNE <- WUEtt[TeBS.TeNE]
WUEt.shift <- WUEtt[clm.fcomp[[s]]$TeBS >= 0.65 & clm.fcomp[[s]]$TeBS <  0.99 ]

IIWUE<- IWUE[[s]][,"clm.bgc"] 
IWUE.TeBS <- IIWUE[TeBSonly]
IWUE.TeBS.TeNE <- IIWUE[TeBS.TeNE]
IWUE.shift <- IIWUE[clm.fcomp[[s]]$TeBS >= 0.65 & clm.fcomp[[s]]$TeBS <  0.99 ]

#make a boxplots for each of these
#for WUEt
a <- data.frame(group = "TeBS only", value = WUEt.TeBS)
b <- data.frame(group = "TeBS + TeNE coexisting", value = WUEt.TeBS.TeNE)
c <- data.frame(group = "vegetation shift", value = WUEt.shift)

WUEt.df <- rbind(a, b, c)

ggplot(WUEt.df, aes(x = group, y = value, fill = group)) + 
  geom_boxplot() + ylim(0, 7) + ggtitle(paste(site.list[s],"WUEt"))


#for WUEi
#make a boxplots for each of these
a <- data.frame(group = "TeBS only", value = WUEi.TeBS)
b <- data.frame(group = "TeBS + TeNE coexisting", value = WUEi.TeBS.TeNE)
c <- data.frame(group = "vegetation shift", value = WUEi.shift)

WUEi.df <- rbind(a, b, c)

ggplot(WUEi.df, aes(x = group, y = value, fill = group)) + 
  geom_boxplot() + ylim(0, 7) + ggtitle(paste(site.list[s],"WUEi"))

#for IWUE

#make a boxplots for each of these
a <- data.frame(group = "TeBS only", value = IWUE.TeBS)
b <- data.frame(group = "TeBS + TeNE coexisting", value = IWUE.TeBS.TeNE)
c <- data.frame(group = "vegetation shift", value = IWUE.shift)

IWUE.df <- rbind(a, b, c)

ggplot(IWUE.df, aes(x = group, y = value, fill = group)) + 
  geom_boxplot() + ylim(0, 7) + ggtitle(paste(site.list[s],"IWUE"))

#boxplot( WUEi.TeBS.TeNE, WUEi.shift, WUEi.TeBS, main = paste(site.list[s]))
#boxplot( WUEt.TeBS.TeNE, WUEt.shift, WUEt.TeBS, ylim = c(0, 10),main = paste(site.list[s]))
#boxplot( IWUE.TeBS.TeNE, IWUE.shift, IWUE.TeBS, ylim = c(0, 10),main = paste(site.list[s]))

##another way of looking at these dynamics
TeBS <- clm.fcomp[[s]]$TeBS

difference <- diff(TeBS, lag = 1)

#boxplots for IWUE differences

IWUE.pos<- IIWUE[difference > 0.00003 ]
IWUE.neg <- IIWUE[difference < 0]
IWUE.shift <- IIWUE[difference >= -0.00003 & difference <= 0]

a <- data.frame(group = "TeBS increasing", value = IWUE.pos)
b <- data.frame(group = "TeBS decreasing", value = IWUE.neg)
c <- data.frame(group = "TeBS same", value = IWUE.shift)

IWUE.df <- rbind(a, b, c)

ggplot(IWUE.df, aes(x = group, y = value, fill = group)) + 
  geom_boxplot() + ylim(0, 10) + ggtitle(paste(site.list[s],"IWUE"))

#boxplots for WUEi difference


WUEi.pos<- WUEii[difference > 0.00003 ]
WUEi.neg <- WUEii[difference < 0]
WUEi.shift <- WUEii[difference >= -0.00003 & difference <= 0]


a <- data.frame(group = "TeBS increasing", value = WUEi.pos)
b <- data.frame(group = "TeBS decreasing", value = WUEi.neg)
c <- data.frame(group = "TeBS same", value = WUEi.shift)

WUEi.df <- rbind(a, b, c)

ggplot(WUEi.df, aes(x = group, y = value, fill = group)) + 
  geom_boxplot() + ylim(0, 2.0) + ggtitle(paste(site.list[s],"WUEi"))


#boxplots for WUEt differeces
WUEt.pos<- WUEtt[difference > 0.00003 ]
WUEt.neg <- WUEtt[difference <  0]
WUEt.shift <- WUEtt[difference >= -0.00003 & difference <= 0]

a <- data.frame(group = "TeBS increasing", value = WUEt.pos)
b <- data.frame(group = "TeBS decreasing", value = WUEt.neg)
c <- data.frame(group = "TeBS same", value = WUEt.shift)

WUEt.df <- rbind(a, b, c)

ggplot(WUEt.df, aes(x = group, y = value, fill = group)) + 
  geom_boxplot() + ylim(0, 7) + ggtitle(paste(site.list[s],"WUEt"))



}
dev.off()

#plain boxplots
boxplot(IWUE.pos, IWUE.shift, IWUE.neg, ylim = c(0, 10), notch = TRUE)
boxplot(WUEi.pos,  WUEi.shift, WUEi.neg, notch = TRUE)
boxplot(WUEt.pos,  WUEt.shift, WUEt.neg,ylim = c(0, 10), notch = TRUE)





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
