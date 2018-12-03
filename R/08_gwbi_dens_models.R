# Script to run mixed effects models on agbi & density:
library(rjags)
library(ggplot2)
library(caTools)
library(ggridges)
library(tidyr)
library(reshape2)
library(dplyr)


# ---------------read in model agbi, dens summaries for ED2
all.df.yr <- readRDS("outputs/data/ED2/dens_agbi_climate_ED2.rds")
#dens.agbi <- readRDS( "outputs/data/ED2/ED2.agbi.dens.site.rds")


sec2yr <- 1*60*60*24*365.25
JJAmeans.ED <- readRDS("outputs/data/ED2/ED2.alldat.jjameans.rds")
JJAmeans.ED$Tair.C.jja <- JJAmeans.ED$Tair - 273.15
JJAmeans.ED$precip.mm.jja <- JJAmeans.ED$precip*(sec2yr*3/12)
colnames(JJAmeans.ED)

ED.all <- left_join(all.df.yr, JJAmeans.ED[,c("Year", "Site", "Tair.C.jja", "precip.mm.jja")], by = c("Year", "Site"))

min.totals <- ED.all %>% group_by(Site) %>% summarise(min.gwbi = min(GS_gwbi),
                                                      mean.gwbi = mean(GS_gwbi))
GWBI.ED.mins <- merge(ED.all, min.totals, by = "Site")
rel.ed.gwbi <- GWBI.ED.mins %>% group_by(Site, Year) %>% summarise(rel.gwbi = GS_gwbi - (min.gwbi-0.15),
                                                                   rel.gwbi.raw = GS_gwbi - (min.gwbi),
                                                                   mean.diff = GS_gwbi - mean.gwbi)

ED.all <- merge(ED.all, rel.ed.gwbi)


# ------------read in model agbi, dens summaries for LPJ-GUESS
all.df.yr.GUESS <- readRDS("outputs/data/GUESS/GUESS.alldat.yrmeans.rds")



sec2yr <- 1*60*60*24*365.25
JJAmeans.GUESS <- readRDS("outputs/data/GUESS/GUESS.alldat.jjameans.rds")
JJAmeans.GUESS$Tair.C.jja <- JJAmeans.GUESS$Tair - 273.15
JJAmeans.GUESS$precip.mm.jja <- JJAmeans.GUESS$precip*(sec2yr*3/12)
colnames(JJAmeans.GUESS)

GUESS.all <- left_join(all.df.yr.GUESS, JJAmeans.GUESS[,c("Year", "Site", "Tair.C.jja", "precip.mm.jja")], by = c("Year", "Site"))

# for GUESS, we also have density, and gwbi at PFT scale, so lets read them in here:
GWBI.GUESS <- readRDS("Data/GUESS.gwbi.pft.wide.rds")
GWBI.GUESS$Site <- paste0("X", GWBI.GUESS$Site)
head(GWBI.GUESS)

# get the minimum for each site so we can relativize by the min:
min.totals <- GWBI.GUESS %>% group_by(Site) %>% summarise(min.gwbi = min(Total.gwbi), 
                                                          mean.gwbi = mean(Total.gwbi))
GWBI.GUESS.mins <- merge(GWBI.GUESS, min.totals, by = "Site")
rel.guess.gwbi <- GWBI.GUESS.mins %>% group_by(Site, Year) %>% summarise(rel.gwbi = Total.gwbi - (min.gwbi-0.15),
                                                                         rel.gwbi.raw = Total.gwbi - (min.gwbi), 
                                                                         mean.diff = Total.gwbi - mean.gwbi)
                                                                         
ggplot(rel.guess.gwbi, aes(Year, mean.diff, color = Site))+geom_point()+theme(legend.position = "none")

GWBI.GUESS <- merge(GWBI.GUESS, rel.guess.gwbi)

Dens.GUESS <- readRDS("Data/GUESS.Dens.pft.wide.rds")
Dens.GUESS$Site <- paste0("X", Dens.GUESS$Site)

GUESS.all.y <- left_join(GUESS.all, GWBI.GUESS, by =c("Year", "Site"))
GUESS.all <- left_join(GUESS.all.y, Dens.GUESS, by =c("Year", "Site"))

# make a DF paralell to ED2:
colnames(ED.all)
colnames(GUESS.all)

GUESS.totals <- GUESS.all[,c("Year", "Site", "Rel.Dens", "IWUE", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                             "Total.Dens", "Total.gwbi","mean.diff", "Tair.C.jja", "precip.mm.jja")]

GUESS.totals$Model <- "GUESS"
colnames(GUESS.totals) <- c("Year", "Site", "Rel.Dens", "IWUE", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                         "Dens", "gwbi","rel.gwbi", "Tair.C.jja", "precip.mm.jja", "Model")


# now subset ED.all by because we dont have all the WUE

ED.totals <- ED.all[,c("Year", "Site", "Rel.Dens", "IWUE", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                             "Dens", "GS_gwbi", "mean.diff", "Tair.C.jja", "precip.mm.jja")]
ED.totals$Model <- "ED2"
colnames(ED.totals) <- c("Year", "Site", "Rel.Dens", "IWUE", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                         "Dens", "gwbi",  "rel.gwbi","Tair.C.jja", "precip.mm.jja", "Model")



# combine together (in case we want to model together):
ED.GUESS <- rbind(ED.totals, GUESS.totals)

# some prelimiary plots to visualize the data:
ggplot(ED.GUESS, aes( precip.mm, rel.gwbi,  color = Model))+geom_point()+stat_smooth()+theme(legend.position = "none")+theme_bw()
ggplot(ED.GUESS, aes( Tair.C.jja, rel.gwbi, color = Model))+geom_point()+stat_smooth()+theme(legend.position = "none")+theme_bw()
ggplot(ED.GUESS, aes( CO2, rel.gwbi, color = Model))+geom_point()+stat_smooth()+theme(legend.position = "none")+theme_bw()
ggplot(ED.GUESS, aes( precip.mm.jja, rel.gwbi, color = Model))+geom_point()+stat_smooth()+theme(legend.position = "none")+theme_bw()



#----------------------------------------
# Separate Testing and Training Datasets:

# 1. initial data cleaning & checking
# 2. create dummy variables for CO2 classes: "modern", "past", "pre-industrial"
# 3. calculate agbi lagged
# 4. remove NA values for lagged agbi
# 5. scale climate parameters
# 6. Split into testing and training data:

# remove NAS: 
ED <- ED.totals[!is.na(ED.totals$rel.gwbi) & !is.na(ED.totals$precip.mm),]
GUESS <- GUESS.totals[!is.na(GUESS.totals$rel.gwbi) & !is.na(GUESS.totals$precip.mm),]

# create classes for modern, past and pre-industrial:
ED$period <- ifelse(ED$Year <= 1850, "pre-industrial", 
                ifelse(ED$Year <= 1950 & ED$Year > 1850,"industrial-past",
                    ifelse(ED$Year >= 1950, "modern-industrial", "NA")))

ED$period_cd <- ifelse(ED$Year <= 1850, "3", 
                    ifelse(ED$Year <= 1950 & ED$Year > 1850,"2",
                           ifelse(ED$Year >= 1950, "1", "NA")))

# create classes for modern, past and pre-industrial:
GUESS$period <- ifelse(GUESS$Year <= 1850, "pre-industrial", 
                    ifelse(GUESS$Year <= 1950 & GUESS$Year > 1850,"industrial-past",
                           ifelse(GUESS$Year >= 1950, "modern-industrial", "NA")))

GUESS$period_cd <- ifelse(GUESS$Year <= 1850, "3", 
                       ifelse(GUESS$Year <= 1950 & GUESS$Year > 1850,"2",
                              ifelse(GUESS$Year >= 1950, "1", "NA")))

# calculate lagged agbi:

# get previous years growth for ED
uni.Sites <- unique(ED$Site)
ED.sort <- ED[with(ED, order(Site, Year)),]

ED.sort.wide <- ED.sort[,c("Year", "Site", "rel.gwbi")] %>% spread(key = "Site", value = "rel.gwbi")
ED.sort_1.wide <- ED.sort.wide[1:(length(ED.sort.wide$Year)-1),]
ED.sort_1.wide$Year <- ED.sort.wide[2:(length(ED.sort.wide$Year)),]$Year

ED.sort_2.wide <- ED.sort.wide[1:(length(ED.sort.wide$Year)-2),]
ED.sort_2.wide$Year <- ED.sort.wide[3:(length(ED.sort.wide$Year)),]$Year

ED.sort_3.wide <- ED.sort.wide[1:(length(ED.sort.wide$Year)-3),]
ED.sort_3.wide$Year <- ED.sort.wide[4:(length(ED.sort.wide$Year)),]$Year

ED.sort_4.wide <- ED.sort.wide[1:(length(ED.sort.wide$Year)-4),]
ED.sort_4.wide$Year <- ED.sort.wide[5:(length(ED.sort.wide$Year)),]$Year

ED.sort_5.wide <- ED.sort.wide[1:(length(ED.sort.wide$Year)-5),]
ED.sort_5.wide$Year <- ED.sort.wide[6:(length(ED.sort.wide$Year)),]$Year


ED.sort_1 <- melt(ED.sort_1.wide, id.vars = c("Year"))
colnames(ED.sort_1) <- c("Year", "Site", "rel.gwbi_1")
ED.sort_2 <- melt(ED.sort_2.wide, id.vars = c("Year"))
colnames(ED.sort_2) <- c("Year", "Site", "rel.gwbi_2")

ED.sort_3 <- melt(ED.sort_3.wide, id.vars = c("Year"))
colnames(ED.sort_3) <- c("Year", "Site", "rel.gwbi_3")

ED.sort_4 <- melt(ED.sort_4.wide, id.vars = c("Year"))
colnames(ED.sort_4) <- c("Year", "Site", "rel.gwbi_4")

ED.sort_5 <- melt(ED.sort_5.wide, id.vars = c("Year"))
colnames(ED.sort_5) <- c("Year", "Site", "rel.gwbi_5")

ED.sort_lag <- left_join(ED.sort_1, ED.sort_2, by = c("Year", "Site"))
ED.sort_lag34 <- left_join(ED.sort_3, ED.sort_4, by = c("Year", "Site"))
ED.sort_lag <- left_join(ED.sort_lag, ED.sort_lag34, by = c("Year", "Site"))
ED.sort_lag <- left_join(ED.sort_lag, ED.sort_5, by = c("Year", "Site"))

ED.sort_lag <- left_join(ED.sort, ED.sort_lag, by = c("Year", "Site"))

# remove NAs from agbi_1 and agbi_2:

ED.sort_lag <- ED.sort_lag[!is.na(ED.sort_lag$rel.gwbi_1) & !is.na(ED.sort_lag$rel.gwbi_2) & !is.na(ED.sort_lag$rel.gwbi_3) & !is.na(ED.sort_lag$rel.gwbi_4) & !is.na(ED.sort_lag$rel.gwbi_5),]

# scale the climate parameters:

ED.sort_lag$Precip.scaled = as.vector(scale(ED.sort_lag$precip.mm, center = TRUE, scale = TRUE))
ED.sort_lag$Temp.jja.scaled = as.vector(scale(ED.sort_lag$Tair.C.jja, center = TRUE, scale = TRUE))
ED.sort_lag$Temp.scaled = as.vector(scale(ED.sort_lag$Tair.C, center = TRUE, scale = TRUE))

# scale the CO2 parameters:
ED.sort_lag$CO2.scaled = as.vector(scale(ED.sort_lag$CO2, center = TRUE, scale = TRUE))
#ED.sort_lag$site_num <- 

#splits <- unlist(strsplit(unique(ED.sort_lag$Site), "X"))
covert_site_codes <- data.frame(site_num = unique(as.numeric(sapply(strsplit(ED.sort_lag$Site,"X"), `[`, 2))),
           site_code = 1:length(unique(as.numeric(sapply(strsplit(ED.sort_lag$Site,"X"), `[`, 2)))))

ED.sort_lag$site_num <- as.numeric(sapply(strsplit(ED.sort_lag$Site,"X"), `[`, 2))
ED.sort_lag <- left_join(ED.sort_lag, covert_site_codes, by = "site_num")


# get previous years growth for GUESS:
# get previous years growth
uni.Sites <- unique(GUESS$Site)
GUESS.sort <- GUESS[with(GUESS, order(Site, Year)),]

GUESS.sort.wide <- GUESS.sort[,c("Year", "Site", "rel.gwbi")] %>% spread(key = "Site", value = "rel.gwbi")
GUESS.sort_1.wide <- GUESS.sort.wide[1:(length(GUESS.sort.wide$Year)-1),]
GUESS.sort_1.wide$Year <- GUESS.sort.wide[2:(length(GUESS.sort.wide$Year)),]$Year

GUESS.sort_2.wide <- GUESS.sort.wide[1:(length(GUESS.sort.wide$Year)-2),]
GUESS.sort_2.wide$Year <- GUESS.sort.wide[3:(length(GUESS.sort.wide$Year)),]$Year

GUESS.sort_3.wide <- GUESS.sort.wide[1:(length(GUESS.sort.wide$Year)-3),]
GUESS.sort_3.wide$Year <- GUESS.sort.wide[4:(length(GUESS.sort.wide$Year)),]$Year

GUESS.sort_4.wide <- GUESS.sort.wide[1:(length(GUESS.sort.wide$Year)-4),]
GUESS.sort_4.wide$Year <- GUESS.sort.wide[5:(length(GUESS.sort.wide$Year)),]$Year

GUESS.sort_5.wide <- GUESS.sort.wide[1:(length(GUESS.sort.wide$Year)-5),]
GUESS.sort_5.wide$Year <- GUESS.sort.wide[6:(length(GUESS.sort.wide$Year)),]$Year


GUESS.sort_1 <- melt(GUESS.sort_1.wide, id.vars = c("Year"))
colnames(GUESS.sort_1) <- c("Year", "Site", "rel.gwbi_1")
GUESS.sort_2 <- melt(GUESS.sort_2.wide, id.vars = c("Year"))
colnames(GUESS.sort_2) <- c("Year", "Site", "rel.gwbi_2")

GUESS.sort_3 <- melt(GUESS.sort_3.wide, id.vars = c("Year"))
colnames(GUESS.sort_3) <- c("Year", "Site", "rel.gwbi_3")

GUESS.sort_4 <- melt(GUESS.sort_4.wide, id.vars = c("Year"))
colnames(GUESS.sort_4) <- c("Year", "Site", "rel.gwbi_4")

GUESS.sort_5 <- melt(GUESS.sort_5.wide, id.vars = c("Year"))
colnames(GUESS.sort_5) <- c("Year", "Site", "rel.gwbi_5")

GUESS.sort_lag <- left_join(GUESS.sort_1, GUESS.sort_2, by = c("Year", "Site"))
GUESS.sort_lag34 <- left_join(GUESS.sort_3, GUESS.sort_4, by = c("Year", "Site"))
GUESS.sort_lag <- left_join(GUESS.sort_lag, GUESS.sort_lag34, by = c("Year", "Site"))
GUESS.sort_lag <- left_join(GUESS.sort_lag, GUESS.sort_5, by = c("Year", "Site"))

GUESS.sort_lag <- left_join(GUESS.sort, GUESS.sort_lag, by = c("Year", "Site"))

# remove NAs from agbi_1 and agbi_2:

GUESS.sort_lag <- GUESS.sort_lag[!is.na(GUESS.sort_lag$rel.gwbi_1) & !is.na(GUESS.sort_lag$rel.gwbi_2) & !is.na(GUESS.sort_lag$rel.gwbi_3) & !is.na(GUESS.sort_lag$rel.gwbi_4) & !is.na(GUESS.sort_lag$rel.gwbi_5),]

# scale the climate parameters:

GUESS.sort_lag$Precip.scaled = as.vector(scale(GUESS.sort_lag$precip.mm, center = TRUE, scale = TRUE))
GUESS.sort_lag$Temp.jja.scaled = as.vector(scale(GUESS.sort_lag$Tair.C.jja, center = TRUE, scale = TRUE))
GUESS.sort_lag$Temp.scaled = as.vector(scale(GUESS.sort_lag$Tair.C, center = TRUE, scale = TRUE))

# scale the CO2 parameters:
GUESS.sort_lag$CO2.scaled = as.vector(scale(GUESS.sort_lag$CO2, center = TRUE, scale = TRUE))
#GUESS.sort_lag$site_num <- 

#splits <- unlist(strsplit(unique(GUESS.sort_lag$Site), "X"))
covert_site_codes <- data.frame(site_num = unique(as.numeric(sapply(strsplit(GUESS.sort_lag$Site,"X"), `[`, 2))),
                                site_code = 1:length(unique(as.numeric(sapply(strsplit(GUESS.sort_lag$Site,"X"), `[`, 2)))))

GUESS.sort_lag$site_num <- as.numeric(sapply(strsplit(GUESS.sort_lag$Site,"X"), `[`, 2))
GUESS.sort_lag <- left_join(GUESS.sort_lag, covert_site_codes, by = "site_num")

# combine together (in case we want to model together):
full.df <- rbind(GUESS.sort_lag, ED.sort_lag)


# split training and testing datasets:
# split training and testing datasets:
msk <- caTools::sample.split( ED.sort_lag, SplitRatio = 3/4, group = NULL )

train.full <- full.df[msk,]
test.full <- full.df[!msk,]

train.mod <- train.full[train.full$period %in% c("modern-industrial", "industrial-past"),]
test.mod <- test.full[test.full$period %in% c("modern-industrial", "industrial-past"),]

train.ED <- train.mod[train.mod$Model %in% "ED2",]
test.ED <- test.mod[test.mod$Model %in% "ED2",]

train.full.ED <- train.full
test.full.ED <- test.full
#------------------------------
# split training and testing for GUESS:
# 
msk2 <- caTools::sample.split( GUESS.sort_lag, SplitRatio = 3/4, group = NULL )

train.GUESS.full <- GUESS.sort_lag[msk2,]
test.GUESS.full <- GUESS.sort_lag[!msk2,]

train.GUESS  <- train.GUESS.full[train.GUESS.full$period %in% c("modern-industrial", "industrial-past"),]
test.GUESS  <- test.GUESS.full[test.GUESS.full$period %in% c("modern-industrial", "industrial-past"),]

train.full.ED <- train.GUESS.full
test.full.ED <- test.GUESS.full

class(train.ED$site_code)

#---------------------read in TR data and relativise to compare----------------------
full.ghcn <- read.csv("/Users/kah/Documents/TreeRings/outputs/data/rwi_age_dbh_ghcn.df")
summary(full.ghcn)

# get all records that have all RWI and don't have negative diams or NA diams
#full.ghcn <- full.ghcn[!is.na(full.ghcn$RWI) & full.ghcn$DBH > 0 & !is.na(full.ghcn$DBH),]

rwi.ghcn <- read.csv("/Users/kah/Documents/TreeRings/outputs/full_ghcn_all_months_rwi.csv")
rwi.prism <- read.csv("/Users/kah/Documents/TreeRings/outputs/full_prism_all_months_rwi.csv")

# get rel.rwi:
min.totals <- full.ghcn %>% group_by(site) %>% summarise(min.gwbi = min(RWI, na.rm = TRUE), 
                                                          mean.gwbi = mean(RWI, na.rm = TRUE))
RWI.mins <- merge(full.ghcn, min.totals, by = "site")
rel.TR.gwbi <- RWI.mins %>% group_by(site,ID, year) %>% summarise( mean.diff = RWI - mean.gwbi)

ggplot(rel.TR.gwbi , aes(year, mean.diff, color = site))+geom_point()+theme(legend.position = "none")

full.ghcn.rel <- merge( full.ghcn, rel.TR.gwbi, by = c("ID", "site","year"))
# calculate JJA VPD for each year
rwi.prism$jja.VPDmax <- rowMeans(rwi.prism[,c("Month_vpdmax_6", "Month_vpdmax_7", "Month_vpdmax_8")])
rwi.prism$jja.VPDmin <- rowMeans(rwi.prism[,c("Month_vpdmin_6", "Month_vpdmin_7", "Month_vpdmin_8")])

# calculate JJA BAL for each year
rwi.prism$jja.BAL <- rowMeans(rwi.prism[,c("Month_BAL_6", "Month_BAL_7", "Month_BAL_8")])

# calculate total precip for each year
rwi.ghcn$MAP.prism <- rowSums(rwi.prism[,c("Month_pcp_1","Month_pcp_2", "Month_pcp_3","Month_pcp_4","Month_pcp_5","Month_pcp_6", "Month_pcp_7", "Month_pcp_8", "Month_pcp_9", "Month_pcp_10", "Month_pcp_11", "Month_pcp_12")])

rwi.ghcn$MAP.ghcn <- rowSums(rwi.ghcn[,c("Month_pcp_1","Month_pcp_2", "Month_pcp_3","Month_pcp_4","Month_pcp_5","Month_pcp_6", "Month_pcp_7", "Month_pcp_8", "Month_pcp_9", "Month_pcp_10", "Month_pcp_11", "Month_pcp_12")])

# make sure the prism pcp, temp values are specified in columns, so when we merge df, it won't be a huge deal:
colnames(rwi.prism)[12:59] <- paste0("prism_",colnames(rwi.prism)[12:59])
rwi.prism.sub <- rwi.prism[,!colnames(rwi.prism) %in% c("DBH", "dbhclass", "ageclass", "SpecCode", "RWI", "RWI_1", "RWI_2", "RWI_3")]
rwi.ghcn.sub <- rwi.ghcn[,!colnames(rwi.ghcn) %in% c("DBH", "dbhclass", "ageclass", "SpecCode", "RWI", "RWI_1", "RWI_2", "RWI_3")]
# merge rwi.ghcn and rwi.prism
full.clim <- merge(rwi.ghcn.sub, rwi.prism.sub, by = c("year", "site", "ID"))

# merge to full.ghcn
full.ghcn <- merge(full.ghcn.rel, full.clim, by = c("ID", "year", "site" ))

full.ghcn$site_age <- paste0(full.ghcn$site, "-", full.ghcn$ageclass)
full.ghcn$site_age.code <- as.numeric(as.factor(full.ghcn$site_age))



full.ghcn$DBH.scaled = as.vector(scale(full.ghcn$DBH, center = TRUE, scale = TRUE))


# standardise predictor variables to have mean 0 and sd = 1
DI.scaled = scale(full.ghcn$JJA.pdsi, center= TRUE, scale=TRUE)
DBH.scaled = scale(full.ghcn$DBH, center= TRUE, scale=TRUE)
full.ghcn$T.scaled = as.vector(scale(full.ghcn$JUNTmax, center= TRUE, scale=TRUE))
full.ghcn$DI.scaled = as.vector(scale(full.ghcn$JJA.pdsi, center = TRUE, scale = TRUE))
full.ghcn$DBH.scaled = as.vector(scale(full.ghcn$DBH, center = TRUE, scale = TRUE))
full.ghcn$SP6.scaled = as.vector(scale(full.ghcn$SP06_6, center = TRUE, scale = TRUE))
full.ghcn$SP6.scaled = as.vector(scale(full.ghcn$SP06_6, center = TRUE, scale = TRUE))
full.ghcn$SP1.scaled = as.vector(scale(full.ghcn$SP01_6, center = TRUE, scale = TRUE))

full.ghcn$jja.VPDmax.scaled <- as.vector(scale(full.ghcn$jja.VPDmax, center = TRUE, scale = TRUE))
full.ghcn$jja.BAL.scaled <- as.vector(scale(full.ghcn$jja.BAL, center = TRUE, scale = TRUE))
full.ghcn$MAP.scaled = as.vector(scale(full.ghcn$MAP.prism, center = TRUE, scale = TRUE))

MAP.scaled <- scale(full.ghcn$MAP.prism, center = TRUE, scale = TRUE)
T.scaled <- scale(full.ghcn$JUNTmax, center= TRUE, scale=TRUE)

SP1.scaled <- scale(full.ghcn$SP01_6, center = TRUE, scale = TRUE)
SP6.scaled <- scale(full.ghcn$SP06_6, center = TRUE, scale = TRUE)



# need to define site level structures, if not already defined:

if(! "structure" %in% colnames(full.ghcn)){
  
  structure <- data.frame(site = c("AVO","BON","COR",  "ENG",  "GLA",  "GLL1", "GLL2", "GLL3","GLL4", "HIC",  "MOU",  "PLE",  "PVC",  "STC",  "TOW", "UNC" ),
                          structure = c("Forest", "Savanna", "Forest", "Forest", "Savanna", "Forest", "Savanna", "Savanna", "Forest", "Savanna", "Forest","Savanna", "Savanna", "Savanna", "Forest", "Savanna"))
  full.ghcn <- merge(full.ghcn, structure, by = "site")
}



#struct.cohort and struct.cohort.code (if it doesnt alread exist)

if(! "struct.cohort" %in% colnames(full.ghcn)){
  
  full.ghcn$struct.cohort <- paste0(full.ghcn$ageclass,"-", full.ghcn$structure)
  full.ghcn$struct.cohort.code <- ifelse(full.ghcn$struct.cohort %in% "Past-Forest", 1,
                                         ifelse(full.ghcn$struct.cohort %in% "Modern-Forest", 2, 
                                                ifelse(full.ghcn$struct.cohort %in% "Past-Savanna", 3,
                                                       ifelse(full.ghcn$struct.cohort %in% "Modern-Savanna", 4,"NA" ))))
  
}


# get lagged relative RWI for tree ring data:
full.ghcn.sort <- full.ghcn[with(full.ghcn, order(ID, year)),]

full.ghcn.sort.wide <- full.ghcn.sort[,c("year", "ID", "mean.diff")] %>% spread(key = "ID", value = "mean.diff")
full.ghcn.sort_1.wide <- full.ghcn.sort.wide[1:(length(full.ghcn.sort.wide$year)-1),]
full.ghcn.sort_1.wide$year <- full.ghcn.sort.wide[2:(length(full.ghcn.sort.wide$year)),]$year

full.ghcn.sort_2.wide <- full.ghcn.sort.wide[1:(length(full.ghcn.sort.wide$year)-2),]
full.ghcn.sort_2.wide$year <- full.ghcn.sort.wide[3:(length(full.ghcn.sort.wide$year)),]$year

full.ghcn.sort_3.wide <- full.ghcn.sort.wide[1:(length(full.ghcn.sort.wide$year)-3),]
full.ghcn.sort_3.wide$year <- full.ghcn.sort.wide[4:(length(full.ghcn.sort.wide$year)),]$year

full.ghcn.sort_4.wide <- full.ghcn.sort.wide[1:(length(full.ghcn.sort.wide$year)-4),]
full.ghcn.sort_4.wide$year <- full.ghcn.sort.wide[5:(length(full.ghcn.sort.wide$year)),]$year

full.ghcn.sort_5.wide <- full.ghcn.sort.wide[1:(length(full.ghcn.sort.wide$year)-5),]
full.ghcn.sort_5.wide$year <- full.ghcn.sort.wide[6:(length(full.ghcn.sort.wide$year)),]$year


full.ghcn.sort_1 <- melt(full.ghcn.sort_1.wide, id.vars = c("year"))
colnames(full.ghcn.sort_1) <- c("year", "ID", "rel.diff_1")

full.ghcn.sort_2 <- melt(full.ghcn.sort_2.wide, id.vars = c("year"))
colnames(full.ghcn.sort_2) <- c("year", "ID", "rel.diff_2")

full.ghcn.sort_3 <- melt(full.ghcn.sort_3.wide, id.vars = c("year"))
colnames(full.ghcn.sort_3) <- c("year","ID", "rel.diff_3")

full.ghcn.sort_4 <- melt(full.ghcn.sort_4.wide, id.vars = c("year"))
colnames(full.ghcn.sort_4) <- c("year", "ID","rel.diff_4")

full.ghcn.sort_5 <- melt(full.ghcn.sort_5.wide, id.vars = c("year"))
colnames(full.ghcn.sort_5) <- c("year", "ID", "rel.diff_5")

full.ghcn.sort_lag <- left_join(full.ghcn.sort_1, full.ghcn.sort_2, by = c("year", "ID"))
full.ghcn.sort_lag34 <- left_join(full.ghcn.sort_3, full.ghcn.sort_4, by = c("year", "ID"))
full.ghcn.sort_lag <- left_join(full.ghcn.sort_lag, full.ghcn.sort_lag34, by = c("year", "ID"))
full.ghcn.sort_lag <- left_join(full.ghcn.sort_lag, full.ghcn.sort_5, by = c("year", "ID"))

full.ghcn.sort_lag <- left_join(full.ghcn.sort, full.ghcn.sort_lag, by = c("year", "ID"))



# omit NA values for RWI - 1:
ghcn.clean <- full.ghcn.sort_lag[!is.na(full.ghcn.sort_lag$mean.diff) & !is.na(full.ghcn.sort_lag$rel.diff_1) & !is.na(full.ghcn.sort_lag$rel.diff_2) & !is.na(full.ghcn.sort_lag$rel.diff_3) & !is.na(full.ghcn.sort_lag$rel.diff_4)  & !is.na(full.ghcn.sort_lag$rel.diff_5) & !is.na(full.ghcn.sort_lag$DBH),]
ghcn.clean <- ghcn.clean[,c("site", "ID", "year", "RWI", "MAP.prism","MAP.scaled","JUNTmax","T.scaled", "DBH","mean.diff","rel.diff_1", "rel.diff_2",         
                            "rel.diff_3", "rel.diff_4","rel.diff_5" , "ageclass")]
full.ghcn <- ghcn.clean
# split training and testing datasets:
msk <- caTools::sample.split( ghcn.clean, SplitRatio = 3/4, group = NULL )

train.RWI.full <- ghcn.clean[msk,]
test.RWI.full <- ghcn.clean[!msk,]

# create a dataste the elimates yrs 1900-1950 for the modern and 1950-present for past:

mod.post <- full.ghcn[full.ghcn$ageclass %in% "Modern" & full.ghcn$Year >= 1950,]
past.pre <- full.ghcn[full.ghcn$ageclass %in% "Past" & full.ghcn$Year < 1950,]

sub.ghcn <- rbind(mod.post, past.pre)

msk <- caTools::sample.split( sub.ghcn, SplitRatio = 3/4, group = NULL )

train.RWI <- ghcn.clean[msk,]
test.RWI <- ghcn.clean[!msk,]



#------------------------------------------------------------------------------------
# create probe data to get posterior predictive estimates from ED and GUESS
# generate probe dataset:
DIprobe <- round(seq(range(train.full$Precip.scaled)[1], range(train.full$Precip.scaled)[2], by = 2), 3)
Tempprobe <- round(seq(range(train.full$Temp.jja.scaled)[1], range(train.full$Temp.jja.scaled)[2], by = 2), 3)
RWI1probe <- round(seq(range(train.full$rel.gwbi_1)[1], range(train.full$rel.gwbi_1)[2], by = 2), 2)
RWI2probe <- round(seq(range(train.full$rel.gwbi_2)[1], range(train.full$rel.gwbi_2)[2], by = 2), 2)
RWI3probe <- round(seq(range(train.full$rel.gwbi_3)[1], range(train.full$rel.gwbi_3)[2], by = 2), 2)
RWI4probe <- round(seq(range(train.full$rel.gwbi_4)[1], range(train.full$rel.gwbi_4)[2], by = 2), 2)

# expand into full probe
probe.ED <- expand.grid(DI.scaled = DIprobe,  T.scaled = Tempprobe,
                     rel.gwbi_1 = 1, rel.gwbi_2= 1, rel.gwbi_3= 1,rel.gwbi_4= 1,
                     site_num = 1:221,struct.cohort.code= 1:2)


# -------------------------------------------
# Develop bayesian mixed models for ED
#--------------------------------------------
library(mgcv)
mod <- lm(rel.gwbi ~ Precip.scaled + Temp.jja.scaled + CO2 + rel.gwbi_1 + rel.gwbi_2 +rel.gwbi_3 +rel.gwbi_4+rel.gwbi_5 + Site, data = train.ED)
summary(mod)
#plot(mod)
preds <- predict(mod, test.ED)
plot(test.ED$gwbi, preds)
abline(a = 0, b = 1, col= "red")

# basic lm with no re explais ~ 50 percent of variance

# model gwbi as a function of Temp, Precip, CO2, with random slops for time period & site random intercept
ED_re_site_time_period <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # Y is agbi

# function g()
gfunc[i] <- alpha[sites[i]] + beta1[period[i]]*Precip.scaled[i] + beta2[period[i]]*Temp.jja.scaled[i] + beta3[period[i]]*agbi_1[i] + beta4[period[i]]*agbi_2[i] + beta5[period[i]]*agbi_3[i] + beta6[period[i]]*agbi_4[i] # use Drought index as a scaled variable 

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(c in 1:length(C)){
beta1[c] ~ dnorm(mu_beta1, inv_beta1)
beta2[c] ~ dnorm(mu_beta2, inv_beta2)
beta3[c] ~ dnorm(mu_beta3, inv_beta3)
beta4[c] ~ dnorm(mu_beta4, inv_beta4)
beta5[c] ~ dnorm(mu_beta5, inv_beta5)
beta6[c] ~ dnorm(mu_beta6, inv_beta6)
}

for(s in 1:length(S)){
alpha[s] ~ dnorm(mu_alpha, inv_alpha)
}

# use normal hyperpriors for each hyperparamters 
mu_alpha ~ dunif(-2, 2)
mu_beta1 ~ dunif(-2, 2)
mu_beta2 ~ dunif(-2, 2)
mu_beta3 ~ dunif(-2, 2)
mu_beta4 ~ dunif(-2, 2)
mu_beta5 ~ dunif(-2, 2)
mu_beta6 ~ dunif(-2, 2)

inv_alpha   ~ dgamma(0.001, 0.001)
sigma_alpha <- 1/sqrt(inv_alpha)
inv_beta1   ~ dgamma(0.001, 0.001)
sigma_beta1 <- 1/sqrt(inv_beta1)
inv_beta2   ~ dgamma(0.001, 0.001)
sigma_beta2 <- 1/sqrt(inv_beta2)
inv_beta3   ~ dgamma(0.001, 0.001)
sigma_beta3 <- 1/sqrt(inv_beta3)
inv_beta4   ~ dgamma(0.001, 0.001)
sigma_beta4 <- 1/sqrt(inv_beta4)
inv_beta5   ~ dgamma(0.001, 0.001)
sigma_beta5 <- 1/sqrt(inv_beta5)
inv_beta6   ~ dgamma(0.001, 0.001)
sigma_beta6 <- 1/sqrt(inv_beta6)


# Non-informative Prior for the inverse population variances

#alpha_ref ~ dnorm(0,0.1)
inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# Predictions
for(i in 1:np){
# process model
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is gwbbi

# function g()
gfunc.p[i] <- alpha[sites.p[i]] + beta1[period.p[i]]*Precip.scaled.p[i] + beta2[period.p[i]]*Temp.jja.scaled.p[i] + beta3[period.p[i]]*agbi_1.p[i] + beta4[period.p[i]]*agbi_2.p[i] + beta5[period.p[i]]*agbi_3.p[i] + beta6[period.p[i]]*agbi_4.p[i]# use Drought index as a scaled variable 

}


# Probe
for(i in 1:nprobe){
 #process model
Yprobe[i]   ~ dnorm(gfunc.probe[i], inv.var) # Y is agbi

 #function g()
gfunc.probe[i] <- alpha[sites.probe[i]] + beta1[period.probe[i]]*Precip.scaled.probe[i] + beta2[period.probe[i]]*Temp.jja.scaled.probe[i] + beta3[period.probe[i]]*agbi_1.probe[i] + beta4[period.probe[i]]*agbi_2.probe[i] + beta5[period.probe[i]]*agbi_3.probe[i] + beta6[period.probe[i]]*agbi_4.probe[i]# use Drought index as a scaled variable 

}

}"






reg.EDel.by_period <- jags.model(textConnection(ED_re_site_time_period), 
                               data = list(Y = train.ED$rel.gwbi, n=length(train.ED$rel.gwbi), Precip.scaled = train.ED$Precip.scaled, Temp.jja.scaled = train.ED$Temp.jja.scaled, agbi_1 = train.ED$rel.gwbi_1,agbi_2 = train.ED$rel.gwbi_2, agbi_3 = train.ED$rel.gwbi_3, agbi_4 = train.ED$rel.gwbi_4,
                                           period = as.numeric(train.ED$period_cd), S = unique(train.ED$site_code),  C = unique(train.ED$period_cd), sites = as.numeric(train.ED$site_code), np=length(test.ED$period_cd), 
                                           sites.p = test.ED$site_code, Precip.scaled.p = test.ED$Precip.scaled, Temp.jja.scaled.p = test.ED$Temp.jja.scaled, agbi_1.p = test.ED$rel.gwbi_1, agbi_2.p = test.ED$rel.gwbi_2, agbi_3.p = test.ED$rel.gwbi_3, agbi_4.p = test.ED$rel.gwbi_4,
                                           period.p = as.numeric(test.ED$period_cd),
                                           
                                           nprobe=length(probe.ED$struct.cohort.code), 
                                           sites.probe = probe.ED$site_num, Precip.scaled.probe = probe.ED$DI.scaled, Temp.jja.scaled.probe = probe.ED$T.scaled, agbi_1.probe = probe.ED$rel.gwbi_1, agbi_2.probe = probe.ED$rel.gwbi_2, agbi_3.probe = probe.ED$rel.gwbi_3, agbi_4.probe = probe.ED$rel.gwbi_4,
                                           period.probe = as.numeric(probe.ED$struct.cohort.code)), n.chains = 3, n.adapt = 100)


update(reg.EDel.by_period, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.ED.period <- coda.samples(reg.EDel.by_period, 
 #                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
  #                          n.chains = 3, n.iter=2000, thin = 10)
samp.ED.period <- coda.samples(reg.EDel.by_period, 
                              variable.names=c("alpha", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6" ), 
                              n.chains = 3, n.iter=5000, thin = 1)

samp.ED.ypred <- coda.samples(reg.EDel.by_period, 
                               variable.names=c("Ypred" ), 
                               n.chains = 3, n.iter=5000, thin = 1)

samp.ED.yprobe <- coda.samples(reg.EDel.by_period, 
                              variable.names=c("Yprobe" ), 
                              n.chains = 3, n.iter=5000, thin = 1)


saveRDS(samp.ED.period, "outputs/gwbi_model/Lag4_cohort_re_clim/ED_parameter_samps.rds")
saveRDS(samp.ED.ypred, "outputs/gwbi_model/Lag4_cohort_re_clim/ED_Ypred_samps.rds")
saveRDS(samp.ED.yprobe, "outputs/gwbi_model/Lag4_cohort_re_clim/ED_Yprobe_samps.rds")
samp.ED.yprobe <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/ED_Yprobe_samps.rds")

saveRDS(test.ED, "outputs/gwbi_model/Lag4_cohort_re_clim/ED_testdata.rds")
saveRDS(train.ED, "outputs/gwbi_model/Lag4_cohort_re_clim/ED_traindata.rds")

# Extract the samples for each parameter

samps       <- samp.ED.period[[1]]
Yp.samps    <- samp.ED.ypred [[1]]
Yprobe.sampes <- samp.ED.yprobe [[1]]
alpha.samps <- samps[,1:length(unique(test.ED$site_num))]
beta1.samps  <- samps[,(length(unique(test.ED$site_num))+1):(length(unique(test.ED$site_num))+2)]
beta2.samps  <- samps[,(length(unique(test.ED$site_num))+3):(length(unique(test.ED$site_num))+4)]
beta3.samps  <- samps[,(length(unique(test.ED$site_num))+5):(length(unique(test.ED$site_num))+6)]
beta4.samps  <- samps[,(length(unique(test.ED$site_num))+7):(length(unique(test.ED$site_num))+8)]
beta5.samps  <- samps[,(length(unique(test.ED$site_num))+9):(length(unique(test.ED$site_num))+10)]
beta6.samps  <- samps[,(length(unique(test.ED$site_num))+11):(length(unique(test.ED$site_num))+12)]


# check for convergence:


# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.ED$rel.gwbi

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.ED$rel.gwbi))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+ylim(-0.35, 1.5)+xlim(-0.35,1.5)+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/gwbi_model/basic_reg/pred_vs_obs.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.ED$rel.gwbi)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.ED$rel.gwbi)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# from here we want to get sensitivities
# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- unique(train.ED$site_num)
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <-unique(train.ED$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b1$num <- rownames(b1)
b1.m <- melt(b1, id.vars=c("num"))
b1.mplots <- ggplot(b1.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")


b2 <- data.frame(beta2.samps)
colnames(b2) <-unique(train.ED$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <- ggplot(b2.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Temperature slope")

b3 <- data.frame(beta3.samps)
colnames(b3) <-unique(train.ED$period)
#colnames(b3) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <- ggplot(b3.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-1 Index slope")


b4 <- data.frame(beta4.samps)
colnames(b4) <-unique(train.ED$period)

#colnames(b4) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b4$num <- rownames(b4)
b4.m <- melt(b4, id.vars=c("num"))
b4.mplots <- ggplot(b4.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-2 Index slope")

b5 <- data.frame(beta5.samps)
colnames(b5) <-unique(train.ED$period)
#colnames(b5) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b5$num <- rownames(b5)
b5.m <- melt(b5, id.vars=c("num"))
b5.mplots <- ggplot(b5.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-3 Index slope")

b6 <- data.frame(beta6.samps)
colnames(b6) <-unique(train.ED$period)
#
#colnames(b6) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b6$num <- rownames(b6)
b6.m <- melt(b6, id.vars=c("num"))
b6.mplots <- ggplot(b6.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-4 Index slope")

#>>>>>>>> plot dot plots from guess model:
a.m$variable2 <- paste0("X",a.m$variable)

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b1.sum <- b1.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b
b2.sum <- b2.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b2.sum$variable <- factor(b2.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))


b3.sum <- b3.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b3.sum$variable <- factor(b3.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))


b4.sum <- b4.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b4.sum$variable <- factor(b4.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

b5.sum <- b5.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b5.sum$variable <- factor(b5.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

b6.sum <- b6.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b6.sum$variable <- factor(b6.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

# write out all the dotplots
# want to order the sites by mean annual precip and/or mean annual temperatuere and then plot:
# summarize site map and site mat:
ED.site.clim <- ED.sort_lag %>% group_by(Site) %>% summarise(MAP = mean(precip.mm, na.rm=TRUE), 
                                                                   MAT = mean(Tair.C, na.rm=TRUE))

a1.sum$Site <- paste0("X", a1.sum$variable)
a1.clim <- merge(ED.site.clim, a1.sum, by = "Site")
int.dot.MAP <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAP, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+theme(legend.position = "none")
int.dot.Tmean <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAT, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+theme(legend.position = "none")


b1.dot <-ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Precipitation sensitivity)")+xlim(-0.15, 0.25)+ geom_vline(xintercept = 0, linetype = "dashed")


b2.dot <-ggplot(data.frame(b2.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated JJA temperauture sensitivity")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b3.dot <-ggplot(data.frame(b3.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -1 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b4.dot <-ggplot(data.frame(b4.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -2 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b5.dot <-ggplot(data.frame(b5.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -3 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b6.dot <- ggplot(data.frame(b6.sum), aes(x = mean.val, y = variable, color = variable))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 0.4,height = 0))+
  geom_point( size = 0.5)+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -4 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

png(height = 12, width = 5, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/ED_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot,b2.dot, b3.dot, b4.dot, b5.dot, b6.dot, ncol = 1)
dev.off()


#-------------- plot posterior predictions:--------------------
# now look at predictions:

Yprobe.samps <- data.frame(Yprobe.sampes) 
Yp.m <- melt(Yprobe.samps)
#Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
 #                                                              ci.hi = quantile(value,0.975),
  #                                                             ci.lo = quantile(value,0.025))

colnames(Yprobe.samps) <- 1:length(Yprobe.samps)
probe.m <- melt(Yprobe.samps)
colnames(probe.m) <- c("num", "gwbi_pred")

probe.ED$num <- 1:length(probe.ED[,1])
#colnames(test.GUESS) <- c("Drought", "DBH","Tmax", "RWI_1","RWI_2","struct.cohort", "num")

# summarize by cohort class only:
probe.ED$num <- as.factor(as.character(probe.ED$num))
full.p <- probe.ED

probtest <- dplyr::inner_join(probe.m, full.p, by=c("num"))
colnames(probtest) <- c("num", "gwbi_pred", "MAP_scaled", "JJA.T.scaled", "gwbi_1", "gwbi_2", "gwbi_3", "gwbi_4", "site_num", "period")
saveRDS(probtest, "outputs/gwbi_model/ED_probtest.rds")


cohort.summary.pr <- probtest %>% group_by(period, MAP_scaled) %>% dplyr::summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                   gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                   gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.summary.tm <- probtest %>% group_by(period, JJA.T.scaled) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                             gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                             gwbi.high = quantile(gwbi_pred, 0.975)) 


# need to covert the periods to factors:
cohort.summary.pr$period <- as.factor(cohort.summary.pr$period)
cohort.summary.tm$period <- as.factor(cohort.summary.tm$period)

ggplot(cohort.summary.pr, aes(MAP_scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)

ggplot(data = cohort.summary.tm, aes(JJA.T.scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)

# compare drought sensitivity at low temperatures and high temperatures:
cohort.summary.tm.pr <- probtest %>% group_by(period, JJA.T.scaled, MAP_scaled) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                               gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                               gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.summary.tm.pr$period <- as.factor(cohort.summary.tm.pr$period)
ggplot(data = cohort.summary.tm.pr, aes(MAP_scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.tm.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~JJA.T.scaled)

#------------------------------------------------------------------------------
# Run the model for GUESS:
#-------------------------------------------------------------------------------



library(mgcv)
mod <- lm(rel.gwbi ~ Precip.scaled + Temp.jja.scaled  + rel.gwbi_1 + rel.gwbi_2 +rel.gwbi_3 +rel.gwbi_4 + rel.gwbi_5 + Site, data = train.GUESS)
summary(mod)

# basic lm with no re explais ~ 50 percent of variance

# model gwbi as a function of Temp, Precip, CO2, with random slops for time period & site random intercept
GUESS_re_site_time_period <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # Y is agbi

# function g()
gfunc[i] <- alpha[sites[i]] + beta1[period[i]]*Precip.scaled[i] + beta2[period[i]]*Temp.jja.scaled[i] + beta3[period[i]]*agbi_1[i] + beta4[period[i]]*agbi_2[i] + beta5[period[i]]*agbi_3[i] + beta6[period[i]]*agbi_4[i] # use Drought index as a scaled variable 

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(c in 1:length(C)){
beta1[c] ~ dnorm(mu_beta1, inv_beta1)
beta2[c] ~ dnorm(mu_beta2, inv_beta2)
beta3[c] ~ dnorm(mu_beta3, inv_beta3)
beta4[c] ~ dnorm(mu_beta4, inv_beta4)
beta5[c] ~ dnorm(mu_beta5, inv_beta5)
beta6[c] ~ dnorm(mu_beta6, inv_beta6)
}

for(s in 1:length(S)){
alpha[s] ~ dnorm(mu_alpha, inv_alpha)
}

# use normal hyperpriors for each hyperparamters 
mu_alpha ~ dunif(-2, 2)
mu_beta1 ~ dunif(-2, 2)
mu_beta2 ~ dunif(-2, 2)
mu_beta3 ~ dunif(-2, 2)
mu_beta4 ~ dunif(-2, 2)
mu_beta5 ~ dunif(-2, 2)
mu_beta6 ~ dunif(-2, 2)

inv_alpha   ~ dgamma(0.001, 0.001)
sigma_alpha <- 1/sqrt(inv_alpha)
inv_beta1   ~ dgamma(0.001, 0.001)
sigma_beta1 <- 1/sqrt(inv_beta1)
inv_beta2   ~ dgamma(0.001, 0.001)
sigma_beta2 <- 1/sqrt(inv_beta2)
inv_beta3   ~ dgamma(0.001, 0.001)
sigma_beta3 <- 1/sqrt(inv_beta3)
inv_beta4   ~ dgamma(0.001, 0.001)
sigma_beta4 <- 1/sqrt(inv_beta4)
inv_beta5   ~ dgamma(0.001, 0.001)
sigma_beta5 <- 1/sqrt(inv_beta5)
inv_beta6   ~ dgamma(0.001, 0.001)
sigma_beta6 <- 1/sqrt(inv_beta6)


# Non-informative Prior for the inverse population variances

#alpha_ref ~ dnorm(0,0.1)
inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# Predictions
for(i in 1:np){
# process model
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is agbi

# function g()
gfunc.p[i] <- alpha[sites.p[i]] + beta1[period.p[i]]*Precip.scaled.p[i] + beta2[period.p[i]]*Temp.jja.scaled.p[i] + beta3[period.p[i]]*agbi_1.p[i] + beta4[period.p[i]]*agbi_2.p[i] + beta5[period.p[i]]*agbi_3.p[i] + beta6[period.p[i]]*agbi_4.p[i]# use Drought index as a scaled variable 

}

# Probe
for(i in 1:nprobe){
#process model
Yprobe[i]   ~ dnorm(gfunc.probe[i], inv.var) # Y is agbi

#function g()
gfunc.probe[i] <- alpha[sites.probe[i]] + beta1[period.probe[i]]*Precip.scaled.probe[i] + beta2[period.probe[i]]*Temp.jja.scaled.probe[i] + beta3[period.probe[i]]*agbi_1.probe[i] + beta4[period.probe[i]]*agbi_2.probe[i] + beta5[period.probe[i]]*agbi_3.probe[i] + beta6[period.probe[i]]*agbi_4.probe[i]# use Drought index as a scaled variable 

}


}"



DIprobe <- round(seq(range(train.GUESS.full$Precip.scaled)[1], range(train.GUESS.full$Precip.scaled)[2], by = 2), 3)
Tempprobe <- round(seq(range(train.GUESS.full$Temp.jja.scaled)[1], range(train.GUESS.full$Temp.jja.scaled)[2], by = 2), 3)
GUESS1probe <- round(seq(range(train.GUESS.full$rel.gwbi_1)[1], range(train.GUESS.full$rel.gwbi_1)[2], by = 2), 2)
GUESS2probe <- round(seq(range(train.GUESS.full$rel.gwbi_2)[1], range(train.GUESS.full$rel.gwbi_2)[2], by = 2), 2)
GUESS3probe <- round(seq(range(train.GUESS.full$rel.gwbi_3)[1], range(train.GUESS.full$rel.gwbi_3)[2], by = 2), 2)
GUESS4probe <- round(seq(range(train.GUESS.full$rel.gwbi_4)[1], range(train.GUESS.full$rel.gwbi_4)[2], by = 2), 2)

# expand into full probe
probe.GUESS <- expand.grid(DI.scaled = DIprobe,  T.scaled = Tempprobe,
                        rel.gwbi_1 = 1, rel.gwbi_2= 1, rel.gwbi_3= 1,rel.gwbi_4= 1,
                        site_num = 1:16,struct.cohort.code= 1:2)




reg.model.by_period <- jags.model(textConnection(GUESS_re_site_time_period), 
                                data = list(Y = train.GUESS$rel.gwbi, n=length(train.GUESS$rel.gwbi), Precip.scaled = train.GUESS$Precip.scaled, Temp.jja.scaled = train.GUESS$Temp.jja.scaled, agbi_1 = train.GUESS$rel.gwbi_1,agbi_2 = train.GUESS$rel.gwbi_2, agbi_3 = train.GUESS$rel.gwbi_3, agbi_4 = train.GUESS$rel.gwbi_4,
                                            period = as.numeric(train.GUESS$ageclass), S = unique(train.GUESS$site),  C = unique(train.GUESS$ageclass), sites = train.GUESS$site, np=length(test.GUESS$ageclass), 
                                            sites.p = test.GUESS$site, Precip.scaled.p = test.GUESS$Precip.scaled, Temp.jja.scaled.p = test.GUESS$Temp.jja.scaled, agbi_1.p = test.GUESS$rel.gwbi_1, agbi_2.p = test.GUESS$rel.gwbi_2, agbi_3.p = test.GUESS$rel.gwbi_3, agbi_4.p = test.GUESS$rel.gwbi_4,
                                            period.p = as.numeric(test.GUESS$ageclass),
                                           
                                            nprobe=length(probe.GUESS$struct.cohort.code), 
                                            sites.probe = probe.GUESS$site_num, Precip.scaled.probe = probe.GUESS$DI.scaled, Temp.jja.scaled.probe = probe.GUESS$T.scaled, agbi_1.probe = probe.GUESS$rel.gwbi_1, agbi_2.probe = probe.GUESS$rel.gwbi_2, agbi_3.probe = probe.GUESS$rel.gwbi_3, agbi_4.probe = probe.GUESS$rel.gwbi_4,
                                            period.probe = as.numeric(probe.GUESS$struct.cohort.code)), n.chains = 3, n.adapt = 100)


update(reg.model.by_period, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.GUESS.period <- coda.samples(reg.GUESSel.by_period, 
#                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
#                          n.chains = 3, n.iter=2000, thin = 10)
samp.GUESS.period <- coda.samples(reg.model.by_period, 
                               variable.names=c("alpha", "beta1", "beta2","beta3","beta4","beta5","beta6" ), 
                               n.chains = 3, n.iter=5000, thin = 1)

samp.GUESS.ypred <- coda.samples(reg.model.by_period, 
                              variable.names=c("Ypred" ), 
                              n.chains = 3, n.iter=5000, thin = 1)

samp.GUESS.yprobe <- coda.samples(reg.model.by_period, 
                                 variable.names=c("Yprobe" ), 
                                 n.chains = 3, n.iter=5000, thin = 1)

saveRDS(samp.GUESS.period, "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_parameter_samps.rds")
saveRDS(samp.GUESS.ypred, "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_Ypred_samps.rds")
saveRDS(samp.GUESS.yprobe, "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_Yprobe_samps.rds")
samp.GUESS.yprobe<- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_Yprobe_samps.rds")

saveRDS(test.GUESS, "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_testdata.rds")
saveRDS(train.GUESS, "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_traindata.rds")


#Extract the samples for each parameter

samps       <- samp.GUESS.period[[1]]
Yp.samps    <- samp.GUESS.ypred [[1]]
Yprobe.samps <- samp.GUESS.yprobe[[1]]
alpha.samps <- samps[,1:length(unique(test.GUESS$site_num))]
beta1.samps  <- samps[,(length(unique(test.GUESS$site_num))+1):(length(unique(test.GUESS$site_num))+2)]
beta2.samps  <- samps[,(length(unique(test.GUESS$site_num))+3):(length(unique(test.GUESS$site_num))+4)]
beta3.samps  <- samps[,(length(unique(test.GUESS$site_num))+5):(length(unique(test.GUESS$site_num))+6)]
beta4.samps  <- samps[,(length(unique(test.GUESS$site_num))+7):(length(unique(test.GUESS$site_num))+8)]
beta5.samps  <- samps[,(length(unique(test.GUESS$site_num))+9):(length(unique(test.GUESS$site_num))+10)]
beta6.samps  <- samps[,(length(unique(test.GUESS$site_num))+11):(length(unique(test.GUESS$site_num))+12)]

#sigma.samps <- samps[,(length(unique(test.GUESS$site_num))+6):(length(test.GUESS$site_num)+length(unique(test.GUESS$site_num))+12)]



# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.GUESS$rel.gwbi

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.GUESS$rel.gwbi))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+ylim(-0.35, 1.5)+xlim(-0.35,1.5)+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
#png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/basic_reg/pred_vs_obs.png")
p.o.plot
#dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.GUESS$rel.gwbi)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.GUESS$rel.gwbi)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# plot distributions of the paramters:

# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- unique(train.GUESS$site_num)
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <-unique(train.GUESS$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b1$num <- rownames(b1)
b1.m <- melt(b1, id.vars=c("num"))
b1.mplots <- ggplot(b1.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")


b2 <- data.frame(beta2.samps)
colnames(b2) <-unique(train.GUESS$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <- ggplot(b2.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Temperature slope")

b3 <- data.frame(beta3.samps)
colnames(b3) <-unique(train.GUESS$period)
#colnames(b3) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <- ggplot(b3.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-1 Index slope")


b4 <- data.frame(beta4.samps)
colnames(b4) <-unique(train.GUESS$period)

#colnames(b4) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b4$num <- rownames(b4)
b4.m <- melt(b4, id.vars=c("num"))
b4.mplots <- ggplot(b4.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-2 Index slope")

b5 <- data.frame(beta5.samps)
colnames(b5) <-unique(train.GUESS$period)
#colnames(b5) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b5$num <- rownames(b5)
b5.m <- melt(b5, id.vars=c("num"))
b5.mplots <- ggplot(b5.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-3 Index slope")

b6 <- data.frame(beta6.samps)
colnames(b6) <-unique(train.GUESS$period)
#
#colnames(b6) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b6$num <- rownames(b6)
b6.m <- melt(b6, id.vars=c("num"))
b6.mplots <- ggplot(b6.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-4 Index slope")

#>>>>>>>> plot dot plots from GUESS model:
a.m$variable2 <- paste0("X",a.m$variable)

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b1.sum <- b1.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b
b2.sum <- b2.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b2.sum$variable <- factor(b2.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))


b3.sum <- b3.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b3.sum$variable <- factor(b3.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))


b4.sum <- b4.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b4.sum$variable <- factor(b4.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

b5.sum <- b5.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b5.sum$variable <- factor(b5.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

b6.sum <- b6.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b6.sum$variable <- factor(b6.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

# write out all the dotplots
# want to order the sites by mean annual precip and/or mean annual temperatuere and then plot:
# summarize site map and site mat:
GUESS.site.clim <- GUESS.sort_lag %>% group_by(Site) %>% summarise(MAP = mean(precip.mm, na.rm=TRUE), 
                                                MAT = mean(Tair.C, na.rm=TRUE))

a1.sum$Site <- paste0("X", a1.sum$variable)
a1.clim <- merge(GUESS.site.clim, a1.sum, by = "Site")
int.dot.MAP <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAP, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+theme(legend.position = "none")
int.dot.Tmean <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAT, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+theme(legend.position = "none")


b1.dot <- ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Precipitation sensitivity)")+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")


b2.dot <- ggplot(data.frame(b2.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated JJA temperauture sensitivity")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

b3.dot <- ggplot(data.frame(b3.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -1 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

b4.dot <- ggplot(data.frame(b4.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -2 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

b5.dot <- ggplot(data.frame(b5.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -3 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

b6.dot <- ggplot(data.frame(b6.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -4 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")


# combine all the plots together and save to output:
png(height = 12, width = 5, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot,b2.dot, b3.dot, b4.dot, b5.dot, b6.dot, ncol = 1)
dev.off()


#-------------- plot posterior predictions:--------------------
# now look at predictions:

Yprobe.samps <- data.frame(Yprobe.samps) 
Yp.m <- melt(Yprobe.samps)
#Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
#                                                              ci.hi = quantile(value,0.975),
#                                                             ci.lo = quantile(value,0.025))

colnames(Yprobe.samps) <- 1:length(Yprobe.samps)
probe.m <- melt(Yprobe.samps)
colnames(probe.m) <- c("num", "gwbi_pred")

probe.GUESS$num <- 1:length(probe.GUESS[,1])
#colnames(test.GUESS) <- c("Drought", "DBH","Tmax", "GUESS_1","GUESS_2","struct.cohort", "num")

# summarize by cohort class only:
probe.GUESS$num <- as.factor(as.character(probe.GUESS$num))
full.p <- probe.GUESS

probtest <- dplyr::inner_join(probe.m, full.p, by=c("num"))
colnames(probtest) <- c("num", "gwbi_pred", "MAP_scaled", "JJA.T.scaled", "gwbi_1", "gwbi_2", "gwbi_3", "gwbi_4", "site_num", "period")

saveRDS(probtest, "outputs/gwbi_model/GUESS_probtest.rds")
cohort.summary.pr <- probtest %>% group_by(period, MAP_scaled) %>% dplyr::summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                    gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                    gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.summary.tm <- probtest %>% group_by(period, JJA.T.scaled) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                               gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                               gwbi.high = quantile(gwbi_pred, 0.975)) 


# need to covert the periods to factors:
cohort.summary.pr$period <- as.factor(cohort.summary.pr$period)
cohort.summary.tm$period <- as.factor(cohort.summary.tm$period)

ggplot(cohort.summary.pr, aes(MAP_scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)

ggplot(data = cohort.summary.tm, aes(JJA.T.scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)

# compare drought sensitivity at low temperatures and high temperatures:
cohort.summary.tm.pr <- probtest %>% group_by(period, JJA.T.scaled, MAP_scaled) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                              gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                              gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.summary.tm.pr$period <- as.factor(cohort.summary.tm.pr$period)
ggplot(data = cohort.summary.tm.pr, aes(MAP_scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.tm.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~JJA.T.scaled)

# -------------- read in tree ring data and model tree ring data ---------------------------
colnames(train.GUESS) <- c("site"  ,     "ID" ,        "year" ,      "RWI"  ,      "MAP.prism" , "Precip.scaled" ,"JUNTmax" ,  
                           "Temp.jja.scaled",  "DBH",  "rel.gwbi" , "rel.gwbi_1" ,"rel.gwbi_2", "rel.gwbi_3", "rel.gwbi_4",
                           "rel.gwbi_5", "ageclass")
train.GUESS.full <- train.GUESS

colnames(test.GUESS) <- c("site"  ,     "ID" ,        "year" ,      "RWI"  ,      "MAP.prism" , "Precip.scaled" ,"JUNTmax" ,  
                          "Temp.jja.scaled",  "DBH",  "rel.gwbi" , "rel.gwbi_1" ,"rel.gwbi_2", "rel.gwbi_3", "rel.gwbi_4",
                          "rel.gwbi_5", "ageclass")
test.GUESS.full <- test.GUESS


# model gwbi as a function of Temp, Precip, CO2, with random slops for time period & site random intercept
rwi_re_site_time_period <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # Y is agbi

# function g()
gfunc[i] <- alpha[sites[i]] + beta1[period[i]]*Precip.scaled[i] + beta2[period[i]]*Temp.jja.scaled[i] + beta3[period[i]]*agbi_1[i] + beta4[period[i]]*agbi_2[i] + beta5[period[i]]*agbi_3[i] + beta6[period[i]]*agbi_4[i] # use Drought index as a scaled variable 

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(c in 1:length(C)){
beta1[c] ~ dnorm(mu_beta1, inv_beta1)
beta2[c] ~ dnorm(mu_beta2, inv_beta2)
beta3[c] ~ dnorm(mu_beta3, inv_beta3)
beta4[c] ~ dnorm(mu_beta4, inv_beta4)
beta5[c] ~ dnorm(mu_beta5, inv_beta5)
beta6[c] ~ dnorm(mu_beta6, inv_beta6)
}

for(s in 1:length(S)){
alpha[s] ~ dnorm(mu_alpha, inv_alpha)
}

# use normal hyperpriors for each hyperparamters 
mu_alpha ~ dunif(-2, 2)
mu_beta1 ~ dunif(-2, 2)
mu_beta2 ~ dunif(-2, 2)
mu_beta3 ~ dunif(-2, 2)
mu_beta4 ~ dunif(-2, 2)
mu_beta5 ~ dunif(-2, 2)
mu_beta6 ~ dunif(-2, 2)

inv_alpha   ~ dgamma(0.001, 0.001)
sigma_alpha <- 1/sqrt(inv_alpha)
inv_beta1   ~ dgamma(0.001, 0.001)
sigma_beta1 <- 1/sqrt(inv_beta1)
inv_beta2   ~ dgamma(0.001, 0.001)
sigma_beta2 <- 1/sqrt(inv_beta2)
inv_beta3   ~ dgamma(0.001, 0.001)
sigma_beta3 <- 1/sqrt(inv_beta3)
inv_beta4   ~ dgamma(0.001, 0.001)
sigma_beta4 <- 1/sqrt(inv_beta4)
inv_beta5   ~ dgamma(0.001, 0.001)
sigma_beta5 <- 1/sqrt(inv_beta5)
inv_beta6   ~ dgamma(0.001, 0.001)
sigma_beta6 <- 1/sqrt(inv_beta6)


# Non-informative Prior for the inverse population variances

#alpha_ref ~ dnorm(0,0.1)
inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# Predictions
for(i in 1:np){
# process model
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is agbi

# function g()
gfunc.p[i] <- alpha[sites.p[i]] + beta1[period.p[i]]*Precip.scaled.p[i] + beta2[period.p[i]]*Temp.jja.scaled.p[i] + beta3[period.p[i]]*agbi_1.p[i] + beta4[period.p[i]]*agbi_2.p[i] + beta5[period.p[i]]*agbi_3.p[i] + beta6[period.p[i]]*agbi_4.p[i]# use Drought index as a scaled variable 

}

# Probe
for(i in 1:nprobe){
#process model
Yprobe[i]   ~ dnorm(gfunc.probe[i], inv.var) # Y is agbi

#function g()
gfunc.probe[i] <- alpha[sites.probe[i]] + beta1[period.probe[i]]*Precip.scaled.probe[i] + beta2[period.probe[i]]*Temp.jja.scaled.probe[i] + beta3[period.probe[i]]*agbi_1.probe[i] + beta4[period.probe[i]]*agbi_2.probe[i] + beta5[period.probe[i]]*agbi_3.probe[i] + beta6[period.probe[i]]*agbi_4.probe[i]# use Drought index as a scaled variable 

}


}"



DIprobe <- round(seq(range(train.rwi.full$Precip.scaled)[1], range(train.rwi.full$Precip.scaled)[2], by = 2), 3)
Tempprobe <- round(seq(range(train.rwi.full$Temp.jja.scaled)[1], range(train.rwi.full$Temp.jja.scaled)[2], by = 2), 3)
RWI1probe <- round(seq(range(train.rwi.full$rel.gwbi_1)[1], range(train.rwi.full$rel.gwbi_1)[2], by = 2), 2)
RWI2probe <- round(seq(range(train.rwi.full$rel.gwbi_2)[1], range(train.rwi.full$rel.gwbi_2)[2], by = 2), 2)
RWI3probe <- round(seq(range(train.rwi.full$rel.gwbi_3)[1], range(train.rwi.full$rel.gwbi_3)[2], by = 2), 2)
RWI4probe <- round(seq(range(train.rwi.full$rel.gwbi_4)[1], range(train.rwi.full$rel.gwbi_4)[2], by = 2), 2)

# expand into full probe
probe.rwi <- expand.grid(DI.scaled = DIprobe,  T.scaled = Tempprobe,
                           rel.gwbi_1 = 1, rel.gwbi_2= 1, rel.gwbi_3= 1,rel.gwbi_4= 1,
                           site_num = 1:221,struct.cohort.code= 1:2)




reg.model.by_period <- jags.model(textConnection(rwi_re_site_time_period), 
                                  data = list(Y = train.rwi$rel.gwbi, n=length(train.rwi$rel.gwbi), Precip.scaled = train.rwi$Precip.scaled, Temp.jja.scaled = train.rwi$Temp.jja.scaled, agbi_1 = train.rwi$rel.gwbi_1,agbi_2 = train.rwi$rel.gwbi_2, agbi_3 = train.rwi$rel.gwbi_3, agbi_4 = train.rwi$rel.gwbi_4,
                                              period = as.numeric(train.rwi$period_cd), S = unique(train.rwi$site_code),  C = unique(train.rwi$period_cd), sites = train.rwi$site_code, np=length(test.RWI$period_cd), 
                                              sites.p = test.RWI$site_code, Precip.scaled.p = test.RWI$Precip.scaled, Temp.jja.scaled.p = test.RWI$Temp.jja.scaled, agbi_1.p = test.RWI$rel.gwbi_1, agbi_2.p = test.RWI$rel.gwbi_2, agbi_3.p = test.RWI$rel.gwbi_3, agbi_4.p = test.RWI$rel.gwbi_4,
                                              period.p = as.numeric(test.RWI$period_cd),
                                              
                                              nprobe=length(probe.rwi$struct.cohort.code), 
                                              sites.probe = probe.rwi$site_num, Precip.scaled.probe = probe.rwi$DI.scaled, Temp.jja.scaled.probe = probe.rwi$T.scaled, agbi_1.probe = probe.rwi$rel.gwbi_1, agbi_2.probe = probe.rwi$rel.gwbi_2, agbi_3.probe = probe.rwi$rel.gwbi_3, agbi_4.probe = probe.rwi$rel.gwbi_4,
                                              period.probe = as.numeric(probe.rwi$struct.cohort.code)), n.chains = 3, n.adapt = 100)


update(reg.model.by_period, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.rwi.period <- coda.samples(reg.rwiel.by_period, 
#                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
#                          n.chains = 3, n.iter=2000, thin = 10)
samp.rwi.period <- coda.samples(reg.model.by_period, 
                                  variable.names=c("alpha", "beta1", "beta2","beta3","beta4","beta5","beta6" ), 
                                  n.chains = 3, n.iter=5000, thin = 1)

samp.rwi.ypred <- coda.samples(reg.model.by_period, 
                                 variable.names=c("Ypred" ), 
                                 n.chains = 3, n.iter=5000, thin = 1)

samp.rwi.yprobe <- coda.samples(reg.model.by_period, 
                                  variable.names=c("Yprobe" ), 
                                  n.chains = 3, n.iter=5000, thin = 1)

saveRDS(samp.rwi.period, "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_parameter_samps.rds")
saveRDS(samp.rwi.ypred, "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_Ypred_samps.rds")
saveRDS(samp.rwi.yprobe, "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_Yprobe_samps.rds")
samp.rwi.yprobe<- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_Yprobe_samps.rds")

saveRDS(test.RWI, "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_testdata.rds")
saveRDS(train.RWI, "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_traindata.rds")


#Extract the samples for each parameter

samps       <- samp.rwi.period[[1]]
Yp.samps    <- samp.rwi.ypred [[1]]
Yprobe.samps <- samp.rwi.yprobe[[1]]
alpha.samps <- samps[,1:length(unique(test.RWI$site_num))]
beta1.samps  <- samps[,(length(unique(test.RWI$site_num))+1):(length(unique(test.RWI$site_num))+2)]
beta2.samps  <- samps[,(length(unique(test.RWI$site_num))+3):(length(unique(test.RWI$site_num))+4)]
beta3.samps  <- samps[,(length(unique(test.RWI$site_num))+5):(length(unique(test.RWI$site_num))+6)]
beta4.samps  <- samps[,(length(unique(test.RWI$site_num))+7):(length(unique(test.RWI$site_num))+8)]
beta5.samps  <- samps[,(length(unique(test.RWI$site_num))+9):(length(unique(test.RWI$site_num))+10)]
beta6.samps  <- samps[,(length(unique(test.RWI$site_num))+11):(length(unique(test.RWI$site_num))+12)]

#sigma.samps <- samps[,(length(unique(test.GUESS$site_num))+6):(length(test.GUESS$site_num)+length(unique(test.GUESS$site_num))+12)]



# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.RWI$rel.gwbi

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.RWI$rel.gwbi))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_pred_vs_obs.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.RWI$rel.gwbi)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.RWI$rel.gwbi)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# plot distributions of the paramters:

# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- unique(train.RWI$site_num)
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <-unique(train.RWI$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b1$num <- rownames(b1)
b1.m <- melt(b1, id.vars=c("num"))
b1.mplots <- ggplot(b1.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")


b2 <- data.frame(beta2.samps)
colnames(b2) <-unique(train.RWI$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <- ggplot(b2.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Temperature slope")

b3 <- data.frame(beta3.samps)
colnames(b3) <-unique(train.RWI$period)
#colnames(b3) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <- ggplot(b3.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-1 Index slope")


b4 <- data.frame(beta4.samps)
colnames(b4) <-unique(train.RWI$period)

#colnames(b4) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b4$num <- rownames(b4)
b4.m <- melt(b4, id.vars=c("num"))
b4.mplots <- ggplot(b4.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-2 Index slope")

b5 <- data.frame(beta5.samps)
colnames(b5) <-unique(train.RWI$period)
#colnames(b5) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b5$num <- rownames(b5)
b5.m <- melt(b5, id.vars=c("num"))
b5.mplots <- ggplot(b5.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-3 Index slope")

b6 <- data.frame(beta6.samps)
colnames(b6) <-unique(train.RWI$period)
#
#colnames(b6) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b6$num <- rownames(b6)
b6.m <- melt(b6, id.vars=c("num"))
b6.mplots <- ggplot(b6.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-4 Index slope")

#>>>>>>>> plot dot plots from RWI model:
a.m$variable2 <- paste0("X",a.m$variable)

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b1.sum <- b1.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b
b2.sum <- b2.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b2.sum$variable <- factor(b2.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))


b3.sum <- b3.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b3.sum$variable <- factor(b3.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))


b4.sum <- b4.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b4.sum$variable <- factor(b4.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

b5.sum <- b5.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b5.sum$variable <- factor(b5.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

b6.sum <- b6.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b6.sum$variable <- factor(b6.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

# write out all the dotplots
# want to order the sites by mean annual precip and/or mean annual temperatuere and then plot:
# summarize site map and site mat:
RWI.site.clim <- RWI.sort_lag %>% group_by(Site) %>% summarise(MAP = mean(precip.mm, na.rm=TRUE), 
                                                                   MAT = mean(Tair.C, na.rm=TRUE))

a1.sum$Site <- paste0("X", a1.sum$variable)
a1.clim <- merge(RWI.site.clim, a1.sum, by = "Site")
int.dot.MAP <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAP, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+theme(legend.position = "none")
int.dot.Tmean <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAT, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+theme(legend.position = "none")


b1.dot <- ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Precipitation sensitivity)")+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")


b2.dot <- ggplot(data.frame(b2.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated JJA temperauture sensitivity")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

b3.dot <- ggplot(data.frame(b3.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -1 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

b4.dot <- ggplot(data.frame(b4.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -2 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

b5.dot <- ggplot(data.frame(b5.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -3 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

b6.dot <- ggplot(data.frame(b6.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -4 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")


# combine all the plots together and save to output:
png(height = 12, width = 5, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/RWI_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot,b2.dot, b3.dot, b4.dot, b5.dot, b6.dot, ncol = 1)
dev.off()


#-------------- plot posterior predictions:--------------------
# now look at predictions:

Yprobe.samps <- data.frame(Yprobe.samps) 
Yp.m <- melt(Yprobe.samps)
#Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
#                                                              ci.hi = quantile(value,0.975),
#                                                             ci.lo = quantile(value,0.025))

colnames(Yprobe.samps) <- 1:length(Yprobe.samps)
probe.m <- melt(Yprobe.samps)
colnames(probe.m) <- c("num", "gwbi_pred")

probe.rwi$num <- 1:length(probe.rwi[,1])
#colnames(test.RWI) <- c("Drought", "DBH","Tmax", "RWI_1","RWI_2","struct.cohort", "num")

# summarize by cohort class only:
probe.rwi$num <- as.factor(as.character(probe.rwi$num))
full.p <- probe.rwi

probtest <- dplyr::inner_join(probe.m, full.p, by=c("num"))
colnames(probtest) <- c("num", "gwbi_pred", "MAP_scaled", "JJA.T.scaled", "gwbi_1", "gwbi_2", "gwbi_3", "gwbi_4", "site_num", "period")

saveRDS(probtest, "outputs/gwbi_model/RWI_probtest.rds")
cohort.summary.pr <- probtest %>% group_by(period, MAP_scaled) %>% dplyr::summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                    gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                    gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.summary.tm <- probtest %>% group_by(period, JJA.T.scaled) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                               gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                               gwbi.high = quantile(gwbi_pred, 0.975)) 


# need to covert the periods to factors:
cohort.summary.pr$period <- as.factor(cohort.summary.pr$period)
cohort.summary.tm$period <- as.factor(cohort.summary.tm$period)

ggplot(cohort.summary.pr, aes(MAP_scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)

ggplot(data = cohort.summary.tm, aes(JJA.T.scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)

# compare drought sensitivity at low temperatures and high temperatures:
cohort.summary.tm.pr <- probtest %>% group_by(period, JJA.T.scaled, MAP_scaled) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                              gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                              gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.summary.tm.pr$period <- as.factor(cohort.summary.tm.pr$period)
ggplot(data = cohort.summary.tm.pr, aes(MAP_scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.tm.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~JJA.T.scaled)



# ------------- read in both GUESS and ED and plot posteriors together to compare ---------

GUESS.probe <- readRDS("outputs/gwbi_model/GUESS_probtest.rds")
GUESS.probe$model <- "LPJ-GUESS"
ED.probe <- readRDS("outputs/gwbi_model/ED_probtest.rds")
ED.probe$model <- "ED2"


model.probe <- rbind(GUESS.probe, ED.probe)

cohort.summary.pr <- model.probe %>% group_by(period, MAP_scaled, model) %>% dplyr::summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                    gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                    gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.summary.tm <- model.probe %>% group_by(period, JJA.T.scaled, model) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                               gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                               gwbi.high = quantile(gwbi_pred, 0.975)) 


# need to covert the periods to factors:
cohort.summary.pr$period <- as.factor(cohort.summary.pr$period)
cohort.summary.tm$period <- as.factor(cohort.summary.tm$period)

# compare sensitivities of the models
ggplot(cohort.summary.pr, aes(MAP_scaled, gwbi, color = model))+geom_line()+geom_ribbon(data = cohort.summary.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = model), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~period)

ggplot(cohort.summary.tm, aes(JJA.T.scaled, gwbi, color = model))+geom_line()+geom_ribbon(data = cohort.summary.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = model), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~period)



#---------------WUE response to climate in models and in data-----------------
mod <- lm(IWUE ~ gwbi + Precip.scaled + Temp.jja.scaled  + gwbi_1 +gwbi_2 +gwbi_3 +gwbi_4+gwbi_5 + Site, data = train.GUESS)
summary(mod)




mod <- lm(WUEt ~ Precip.scaled + Temp.jja.scaled+ CO2+ Site, data = train.GUESS)
summary(mod)

modED <- lm(IWUE ~  Precip.scaled + Temp.jja.scaled + CO2 + Site, data = train.ED)
summary(modED)

ggplot(train.ED[train.ED$WUEt <= 100,], aes(Year, IWUE))+geom_point()
ggplot(train.GUESS, aes(Year, IWUE))+geom_point()


#---------------Density response to climate & models-----------------
mod <- lm(Dens ~ gwbi + Precip.scaled + Temp.jja.scaled  + gwbi_1 + gwbi_2 +gwbi_3 +gwbi_4+gwbi_5 + Site, data = train.GUESS)
summary(mod)

modED <- lm(Dens ~ gwbi + Precip.scaled + Temp.jja.scaled  + gwbi_1 +gwbi_2 +gwbi_3 +gwbi_4+gwbi_5 + Site, data = train.ED)
summary(modED)



# model density as a function of gwbi only:
# model gwbi as a function of Temp, Precip, CO2, with random slops for time period & site random intercept
dens_gwbi_only <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # Y is agbi

# function g()
gfunc[i] <- alpha[sites[i]] + beta1[period[i]]*gwbi[i] 

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(c in 1:length(C)){
beta1[c] ~ dnorm(mu_beta1, inv_beta1)

}

for(s in 1:length(S)){
alpha[s] ~ dnorm(mu_alpha, inv_alpha)
}

# use normal hyperpriors for each hyperparamters 
mu_alpha ~ dunif(-2, 2)
mu_beta1 ~ dunif(-2, 2)


inv_alpha   ~ dgamma(0.001, 0.001)
sigma_alpha <- 1/sqrt(inv_alpha)
inv_beta1   ~ dgamma(0.001, 0.001)
sigma_beta1 <- 1/sqrt(inv_beta1)


# Non-informative Prior for the inverse population variances

#alpha_ref ~ dnorm(0,0.1)
inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# Predictions
for(i in 1:np){
# process model
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is agbi

# function g()
gfunc.p[i] <- alpha[sites.p[i]] + beta1[period.p[i]]*gwbi.p[i] 

}

}"


guess.dens.gwbi <- jags.model(textConnection(dens_gwbi_only), 
                                  data = list(Y = train.GUESS$Dens, n=length(train.GUESS$gwbi), gwbi = train.GUESS$gwbi, 
                                              period = as.numeric(train.GUESS$period_cd), S = unique(train.GUESS$site_code),  C = unique(train.GUESS$period_cd), sites = train.GUESS$site_code, np=length(test.GUESS$period_cd), 
                                              sites.p = test.GUESS$site_code, gwbi.p = test.GUESS$gwbi,
                                              period.p = as.numeric(test.GUESS$period_cd)),
                                              
                                                n.chains = 3, n.adapt = 100)


update(guess.dens.gwbi, 1000); # Burnin for 1000 samples to start, then go higher later

# get parameter estimates
dens.GUESS.period <- coda.samples(guess.dens.gwbi, 
                                  variable.names=c("alpha", "beta1"), 
                                  n.chains = 3, n.iter=5000, thin = 1)

dens.GUESS.ypred <- coda.samples(guess.dens.gwbi, 
                                 variable.names=c("Ypred" ), 
                                 n.chains = 3, n.iter=5000, thin = 1)

#dens.GUESS.yprobe <- coda.samples(guess.dens.gwbi, 
 #                                 variable.names=c("Yprobe" ), 
  #                                n.chains = 3, n.iter=5000, thin = 1)

saveRDS(dens.GUESS.period, "outputs/density_gwbi_model/GUESS_parameter_samps.rds")
saveRDS(dens.GUESS.ypred, "outputs/density_gwbi_model/GUESS_Ypred_samps.rds")
#saveRDS(dens.GUESS.yprobe, "outputs/gwbi_model/density_gwbi_model/GUESS_Yprobe_samps.rds")
#dens.GUESS.yprobe<- readRDS( "outputs/gwbi_model/density_gwbi_model/GUESS_Yprobe_samps.rds")
saveRDS(test.GUESS, "outputs/density_gwbi_model/GUESS_testdata.rds")
saveRDS(train.GUESS, "outputs/density_gwbi_model/GUESS_traindata.rds")

#Extract the samples for each parameter

samps       <- dens.GUESS.period[[1]]
Yp.samps    <- dens.GUESS.ypred [[1]]
#Yprobe.samps <- samp.GUESS.yprobe[[1]]
alpha.samps <- samps[,1:length(unique(test.GUESS$site_num))]
beta1.samps  <- samps[,(length(unique(test.GUESS$site_num))+1):(length(unique(test.GUESS$site_num))+2)]

#sigma.samps <- samps[,(length(unique(test.GUESS$site_num))+6):(length(test.GUESS$site_num)+length(unique(test.GUESS$site_num))+12)]



# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.GUESS$Dens

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.GUESS$Dens))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")#+ylim(-0.35, 1.5)+xlim(-0.35,1.5)+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/density_gwbi_model/GUESSpred_vs_obs.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.GUESS$Dens)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.GUESS$Dens)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# plot distributions of the paramters:

# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- unique(train.GUESS$site_num)
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <-unique(train.GUESS$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b1$num <- rownames(b1)
b1.m <- melt(b1, id.vars=c("num"))
b1.mplots <- ggplot(b1.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")


#>>>>>>>> plot dot plots from guess model:
a.m$variable2 <- paste0("X",a.m$variable)

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b1.sum <- b1.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))

# write out all the dotplots
# want to order the sites by mean annual precip and/or mean annual temperatuere and then plot:
# summarize site map and site mat:
GUESS.site.clim <- GUESS.sort_lag %>% group_by(Site) %>% summarise(MAP = mean(precip.mm, na.rm=TRUE), 
                                                                   MAT = mean(Tair.C, na.rm=TRUE))

a1.sum$Site <- paste0("X", a1.sum$variable)
a1.clim <- merge(GUESS.site.clim, a1.sum, by = "Site")
int.dot.MAP <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAP, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")
int.dot.Tmean <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAT, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")


b1.dot <- ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi sensitivity)")+ geom_vline(xintercept = 0, linetype = "dashed")




# combine all the plots together and save to output:
png(height = 12, width = 5, units = "in", res = 300, "outputs/density_gwbi_model/GUESS_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot, ncol = 1)
dev.off()


# ---------------------------- now run the density model for ED2:------------------------


ED.dens.gwbi <- jags.model(textConnection(dens_gwbi_only), 
                              data = list(Y = train.ED$Dens, n=length(train.ED$gwbi), gwbi = train.ED$gwbi, 
                                          period = as.numeric(train.ED$period_cd), S = unique(train.ED$site_code),  C = unique(train.ED$period_cd), sites = train.ED$site_code, np=length(test.ED$period_cd), 
                                          sites.p = test.ED$site_code, gwbi.p = test.ED$gwbi,
                                          period.p = as.numeric(test.ED$period_cd)),
                              
                              n.chains = 3, n.adapt = 100)


update(ED.dens.gwbi, 1000); # Burnin for 1000 samples to start, then go higher later

# get parameter estimates
dens.ED.period <- coda.samples(ED.dens.gwbi, 
                                  variable.names=c("alpha", "beta1"), 
                                  n.chains = 3, n.iter=5000, thin = 1)

dens.ED.ypred <- coda.samples(ED.dens.gwbi, 
                                 variable.names=c("Ypred" ), 
                                 n.chains = 3, n.iter=5000, thin = 1)

#dens.ED.yprobe <- coda.samples(ED.dens.gwbi, 
 #                                 variable.names=c("Yprobe" ), 
  #                                n.chains = 3, n.iter=5000, thin = 1)

saveRDS(samp.ED.period, "outputs/density_gwbi_model/ED_parameter_samps.rds")
saveRDS(samp.ED.ypred, "outputs/density_gwbi_model/ED_Ypred_samps.rds")
saveRDS(samp.ED.yprobe, "outputs/density_gwbi_model/ED_Yprobe_samps.rds")
samp.ED.yprobe<- readRDS( "outputs/density_gwbi_model/ED_Yprobe_samps.rds")
saveRDS(test.ED, "outputs/density_gwbi_model/ED_testdata.rds")
saveRDS(train.ED, "outputs/density_gwbi_model/ED_traindata.rds")

#Extract the samples for each parameter

samps       <- dens.ED.period[[1]]
Yp.samps    <- dens.ED.ypred [[1]]
#Yprobe.samps <- samp.ED.yprobe[[1]]
alpha.samps <- samps[,1:length(unique(test.ED$site_num))]
beta1.samps  <- samps[,(length(unique(test.ED$site_num))+1):(length(unique(test.ED$site_num))+2)]

#sigma.samps <- samps[,(length(unique(test.ED$site_num))+6):(length(test.ED$site_num)+length(unique(test.ED$site_num))+12)]



# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.ED$gwbi

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.ED$Dens))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/density_gwbi_model/EDpred_vs_obs.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.ED$Dens)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.ED$Dens)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# plot distributions of the paramters:

# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- unique(train.ED$site_num)
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <-unique(train.ED$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b1$num <- rownames(b1)
b1.m <- melt(b1, id.vars=c("num"))
b1.mplots <- ggplot(b1.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")


#>>>>>>>> plot dot plots from ED model:
a.m$variable2 <- paste0("X",a.m$variable)

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b1.sum <- b1.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))

# write out all the dotplots
# want to order the sites by mean annual precip and/or mean annual temperatuere and then plot:
# summarize site map and site mat:
ED.site.clim <- ED.sort_lag %>% group_by(Site) %>% summarise(MAP = mean(precip.mm, na.rm=TRUE), 
                                                                   MAT = mean(Tair.C, na.rm=TRUE))

a1.sum$Site <- paste0("X", a1.sum$variable)
a1.clim <- merge(ED.site.clim, a1.sum, by = "Site")
int.dot.MAP <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAP, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")
int.dot.Tmean <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAT, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")


b1.dot <- ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Precipitation sensitivity)")+ geom_vline(xintercept = 0, linetype = "dashed")




# combine all the plots together and save to output:
png(height = 12, width = 5, units = "in", res = 300, "outputs/density_gwbi_model/ED_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot, ncol = 1)
dev.off()




#------------------Models of density by climate--------------------
# model gwbi as a function of Temp, Precip, CO2, with random slops for time period & site random intercept
GUESS_dens_climate <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # Y is agbi

# function g()
gfunc[i] <- alpha[sites[i]] + beta1[period[i]]*Precip.scaled[i] + beta2[period[i]]*Temp.jja.scaled[i] 

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(c in 1:length(C)){
beta1[c] ~ dnorm(mu_beta1, inv_beta1)
beta2[c] ~ dnorm(mu_beta2, inv_beta2)

}

for(s in 1:length(S)){
alpha[s] ~ dnorm(mu_alpha, inv_alpha)
}

# use normal hyperpriors for each hyperparamters 
mu_alpha ~ dunif(-2, 2)
mu_beta1 ~ dunif(-2, 2)
mu_beta2 ~ dunif(-2, 2)

inv_alpha   ~ dgamma(0.001, 0.001)
sigma_alpha <- 1/sqrt(inv_alpha)
inv_beta1   ~ dgamma(0.001, 0.001)
sigma_beta1 <- 1/sqrt(inv_beta1)
inv_beta2   ~ dgamma(0.001, 0.001)
sigma_beta2 <- 1/sqrt(inv_beta2)


# Non-informative Prior for the inverse population variances

#alpha_ref ~ dnorm(0,0.1)
inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# Predictions
for(i in 1:np){
# process model
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is agbi

# function g()
gfunc.p[i] <- alpha[sites.p[i]] + beta1[period.p[i]]*Precip.scaled.p[i] + beta2[period.p[i]]*Temp.jja.scaled.p[i] 

}

# Probe
for(i in 1:nprobe){
#process model
Yprobe[i]   ~ dnorm(gfunc.probe[i], inv.var) # Y is agbi

#function g()
gfunc.probe[i] <- alpha[sites.probe[i]] + beta1[period.probe[i]]*Precip.scaled.probe[i] + beta2[period.probe[i]]*Temp.jja.scaled.probe[i] 

}


}"



DIprobe <- round(seq(range(train.GUESS.full$Precip.scaled)[1], range(train.GUESS.full$Precip.scaled)[2], by = 2), 3)
Tempprobe <- round(seq(range(train.GUESS.full$Temp.jja.scaled)[1], range(train.GUESS.full$Temp.jja.scaled)[2], by = 2), 3)

# expand into full probe
probe.GUESS <- expand.grid(DI.scaled = DIprobe,  T.scaled = Tempprobe,
                           
                           site_num = 1:221,struct.cohort.code= 1:2)




reg.model.by_period <- jags.model(textConnection(GUESS_dens_climate), 
                                  data = list(Y = train.GUESS$Dens, n=length(train.GUESS$Dens), Precip.scaled = train.GUESS$Precip.scaled, Temp.jja.scaled = train.GUESS$Temp.jja.scaled, 
                                              period = as.numeric(train.GUESS$period_cd), S = unique(train.GUESS$site_code),  C = unique(train.GUESS$period_cd), sites = train.GUESS$site_code, np=length(test.GUESS$period_cd), 
                                              sites.p = test.GUESS$site_code, Precip.scaled.p = test.GUESS$Precip.scaled, Temp.jja.scaled.p = test.GUESS$Temp.jja.scaled, 
                                              period.p = as.numeric(test.GUESS$period_cd),
                                              
                                              nprobe=length(probe.GUESS$struct.cohort.code), 
                                              sites.probe = probe.GUESS$site_num, Precip.scaled.probe = probe.GUESS$DI.scaled, Temp.jja.scaled.probe = probe.GUESS$T.scaled, 
                                              period.probe = as.numeric(probe.GUESS$struct.cohort.code)), n.chains = 3, n.adapt = 100)


update(reg.model.by_period, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.GUESS.period <- coda.samples(reg.GUESSel.by_period, 
#                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
#                          n.chains = 3, n.iter=2000, thin = 10)
dens.clim.GUESS.period <- coda.samples(reg.model.by_period, 
                                  variable.names=c("alpha", "beta1", "beta2" ), 
                                  n.chains = 3, n.iter=5000, thin = 1)

dens.clim.GUESS.ypred <- coda.samples(reg.model.by_period, 
                                 variable.names=c("Ypred" ), 
                                 n.chains = 3, n.iter=5000, thin = 1)

dens.clim.GUESS.yprobe <- coda.samples(reg.model.by_period, 
                                  variable.names=c("Yprobe" ), 
                                  n.chains = 3, n.iter=5000, thin = 1)

saveRDS(dens.clim.GUESS.period, "outputs/density_model_climate/GUESS_parameter_samps.rds")
saveRDS(dens.clim.GUESS.ypred, "outputs/density_model_climate/GUESS_Ypred_samps.rds")
saveRDS(dens.clim.GUESS.yprobe, "outputs/density_model_climate/GUESS_Yprobe_samps.rds")
dens.clim.GUESS.yprobe <- readRDS( "outputs/density_model_climate/GUESS_Yprobe_samps.rds")

saveRDS(test.GUESS, "outputs/density_model_climate/GUESS_testdata.rds")
saveRDS(train.GUESS, "outputs/density_model_climate/GUESS_traindata.rds")


#Extract the samples for each parameter

samps       <- dens.clim.GUESS.period[[1]]
Yp.samps    <- dens.clim.GUESS.ypred [[1]]
Yprobe.samps <- dens.clim.GUESS.yprobe[[1]]
alpha.samps <- samps[,1:length(unique(test.GUESS$site_num))]
beta1.samps  <- samps[,(length(unique(test.GUESS$site_num))+1):(length(unique(test.GUESS$site_num))+2)]
beta2.samps  <- samps[,(length(unique(test.GUESS$site_num))+3):(length(unique(test.GUESS$site_num))+4)]


# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.GUESS$Dens

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.GUESS$Dens))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/density_model_climate/GUESSpred_vs_obs.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.GUESS$Dens)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.GUESS$Dens)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# plot distributions of the paramters:

# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- unique(train.ED$site_num)
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <-unique(train.ED$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b1$num <- rownames(b1)
b1.m <- melt(b1, id.vars=c("num"))
b1.mplots <- ggplot(b1.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")


b2 <- data.frame(beta2.samps)
colnames(b2) <-unique(train.ED$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <- ggplot(b2.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")

#>>>>>>>> plot dot plots from ED model:
a.m$variable2 <- paste0("X",a.m$variable)

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b1.sum <- b1.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))


#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b2.sum <- b2.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))

# write out all the dotplots
# want to order the sites by mean annual precip and/or mean annual temperatuere and then plot:
# summarize site map and site mat:
GUESS.site.clim <- GUESS.sort_lag %>% group_by(Site) %>% summarise(MAP = mean(precip.mm, na.rm=TRUE), 
                                                             MAT = mean(Tair.C, na.rm=TRUE))

a1.sum$Site <- paste0("X", a1.sum$variable)
a1.clim <- merge(GUESS.site.clim, a1.sum, by = "Site")
int.dot.MAP <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAP, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")
int.dot.Tmean <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAT, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")


b1.dot <- ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Precipitation sensitivity)")+ geom_vline(xintercept = 0, linetype = "dashed")


b2.dot <- ggplot(data.frame(b2.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Summer Temperature sensitivity)")+ geom_vline(xintercept = 0, linetype = "dashed")



# combine all the plots together and save to output:
png(height = 12, width = 5, units = "in", res = 300, "outputs/density_model_climate/GUESS_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot,b2.dot, ncol = 1)
dev.off()


#------------------------------Run the density-climate model for ED2---------------------

DIprobe <- round(seq(range(train.ED.full$Precip.scaled)[1], range(train.ED.full$Precip.scaled)[2], by = 2), 3)
Tempprobe <- round(seq(range(train.ED.full$Temp.jja.scaled)[1], range(train.ED.full$Temp.jja.scaled)[2], by = 2), 3)

# expand into full probe
probe.ED <- expand.grid(DI.scaled = DIprobe,  T.scaled = Tempprobe,
                           
                           site_num = 1:221,struct.cohort.code= 1:2)




reg.model.by_period <- jags.model(textConnection(GUESS_dens_climate), 
                                  data = list(Y = train.ED$Dens, n=length(train.ED$Dens), Precip.scaled = train.ED$Precip.scaled, Temp.jja.scaled = train.ED$Temp.jja.scaled, 
                                              period = as.numeric(train.ED$period_cd), S = unique(train.ED$site_code),  C = unique(train.ED$period_cd), sites = train.ED$site_code, np=length(test.ED$period_cd), 
                                              sites.p = test.ED$site_code, Precip.scaled.p = test.ED$Precip.scaled, Temp.jja.scaled.p = test.ED$Temp.jja.scaled, 
                                              period.p = as.numeric(test.ED$period_cd),
                                              
                                              nprobe=length(probe.ED$struct.cohort.code), 
                                              sites.probe = probe.ED$site_num, Precip.scaled.probe = probe.ED$DI.scaled, Temp.jja.scaled.probe = probe.ED$T.scaled, 
                                              period.probe = as.numeric(probe.ED$struct.cohort.code)), n.chains = 3, n.adapt = 100)


update(reg.model.by_period, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.ED.period <- coda.samples(reg.EDel.by_period, 
#                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
#                          n.chains = 3, n.iter=2000, thin = 10)
dens.clim.ED.period <- coda.samples(reg.model.by_period, 
                                       variable.names=c("alpha", "beta1", "beta2" ), 
                                       n.chains = 3, n.iter=5000, thin = 1)

dens.clim.ED.ypred <- coda.samples(reg.model.by_period, 
                                      variable.names=c("Ypred" ), 
                                      n.chains = 3, n.iter=5000, thin = 1)

dens.clim.ED.yprobe <- coda.samples(reg.model.by_period, 
                                       variable.names=c("Yprobe" ), 
                                       n.chains = 3, n.iter=5000, thin = 1)

saveRDS(samp.ED.period, "outputs/density_model_climate/ED_parameter_samps.rds")
saveRDS(samp.ED.ypred, "outputs/density_model_climate/ED_Ypred_samps.rds")
saveRDS(samp.ED.yprobe, "outputs/density_model_climate/ED_Yprobe_samps.rds")
samp.ED.yprobe <- readRDS( "outputs/density_model_climate/ED_Yprobe_samps.rds")

saveRDS(test.ED, "outputs/density_model_climate/ED_testdata.rds")
saveRDS(train.ED, "outputs/density_model_climate/ED_traindata.rds")


#Extract the samples for each parameter

samps       <- samp.ED.period[[1]]
Yp.samps    <- samp.ED.ypred [[1]]
Yprobe.samps <- samp.ED.yprobe[[1]]
alpha.samps <- samps[,1:length(unique(test.ED$site_num))]
beta1.samps  <- samps[,(length(unique(test.ED$site_num))+1):(length(unique(test.ED$site_num))+2)]
beta2.samps  <- samps[,(length(unique(test.ED$site_num))+3):(length(unique(test.ED$site_num))+4)]


# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.ED$gwbi

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.ED$Dens))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/density_model_climate/EDpred_vs_obs.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.ED$Dens)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.ED$Dens)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# plot distributions of the paramters:

# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- unique(train.ED$site_num)
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <-unique(train.ED$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b1$num <- rownames(b1)
b1.m <- melt(b1, id.vars=c("num"))
b1.mplots <- ggplot(b1.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")


b2 <- data.frame(beta2.samps)
colnames(b2) <-unique(train.ED$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <- ggplot(b2.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")

#>>>>>>>> plot dot plots from ED model:
a.m$variable2 <- paste0("X",a.m$variable)

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b1.sum <- b1.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))


#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b2.sum <- b2.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))

# write out all the dotplots
# want to order the sites by mean annual precip and/or mean annual temperatuere and then plot:
# summarize site map and site mat:
ED.site.clim <- ED.sort_lag %>% group_by(Site) %>% summarise(MAP = mean(precip.mm, na.rm=TRUE), 
                                                                   MAT = mean(Tair.C, na.rm=TRUE))

a1.sum$Site <- paste0("X", a1.sum$variable)
a1.clim <- merge(ED.site.clim, a1.sum, by = "Site")
int.dot.MAP <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAP, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")
int.dot.Tmean <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAT, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")


b1.dot <- ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Precipitation sensitivity)")+ geom_vline(xintercept = 0, linetype = "dashed")


b2.dot <- ggplot(data.frame(b2.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Summer Temperature sensitivity)")+ geom_vline(xintercept = 0, linetype = "dashed")



# combine all the plots together and save to output:
png(height = 12, width = 5, units = "in", res = 300, "outputs/density_model_climate/ED_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot, b2.dot,ncol = 1)
dev.off()