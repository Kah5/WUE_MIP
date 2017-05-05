# Code to extract the data from Netcdfs
library(ncdf4); library(abind); library(car)
library(reshape2)
mod <- "ED2"
#mdir <- "C:/Users/JMac/Documents/Kelly/MIP/WUE_MIP/WUE_MIP/Data/ED2.v1.2016-05-03.tar/ED2.v1.2016-05-03/ED2.v1.2016-05-03/"
model.dir <- paste0(getwd(), "/Data/ED2.v1.2016-05-03.tar/ED2.v1.2016-05-03/ED2.v1.2016-05-03/")
var <- "Fcomp"


# PFT labels for ED:
pft.lab=c("grass.c4", "tropic.early", "tropic.mid", "tropic.late", "grass.c3.temp", "pine.north", "pine.south", "conifer.late", "temp.decid.early", "temp.decid.mid", "temp.decid.late","ag1", "ag2", "ag3", "ag4","grass.c3.subtrop","Araucaria")
soil=c(4.00, 3.00, 2.17, 1.50, 1.10, 0.80, 0.60, 0.45, 0.30, 0.20, 0.12, 0.06)

# labels for the files in ED model directory
files.all <- dir(model.dir, ".nc")
# Translating yrmin and yrmax into the rounded file names we asked for in paleon
file.min <- ifelse(substr(yrmin,1,1)==8, 850, ifelse(substr(yrmin, 1,1)==9, 900, as.numeric(paste0(substr(yrmin,1,2),"00"))))
file.max <- ifelse(substr(yrmax,1,1)==8, 850, ifelse(substr(yrmax, 1,1)==9, 900, as.numeric(paste0(substr(yrmax,1,2),"00"))))



files.use <- vector()



if(model=="LPJ-GUESS"){ # LPJ-GUESS did not conform to the convention we wanted, so it requires a special case
  dir.month <- dir(model.dir, "month")
  dir.month <- dir.month[!substr(dir.month, nchar(dir.month)-2, nchar(dir.month))==".gz"] # exclude any compressed files
  files.all <- dir(file.path(model.dir, dir.month), ".nc")
  
  # figuring out which files to use
  files.use <- vector()
  for(i in 1:length(files.all)){
    if(as.numeric(strsplit(files.all[i], "_")[[1]][2])>=file.min & as.numeric(strsplit(files.all[i], "_")[[1]][2])<=file.max ){
      files.use <- c(files.use, i) 
    }
  }
} else { # For everything else, the format shoudl be [MODEL]_[start year].nc
  # get list of available .nc files
  files.all <- dir(model.dir, ".nc")
  
  # Subset the files based on the years we're interested in
  files.use <- which(as.numeric(substr(files.all,nchar(files.all)-6, nchar(files.all)-3))>=file.min & as.numeric(substr(files.all,nchar(files.all)-6, nchar(files.all)-3))<=file.max)
} # End file listing 

if(!length(files.use)>0){ stop("No files meet extraction criteria") }




mod.out <- list()
full.out <- list()
files.all<- files.all[1]
#files.use <- 1:2

for(i in files.use){

if(model=="LPJ-GUESS"){
  ncT <- nc_open(file.path(model.dir, dir.month, files.all[i]))
} else {
  ncT <- nc_open(file.path(model.dir, files.all[i]))
} # End opening .nc file

# finding out the frist year in the file
if(model=="LPJ-GUESS"){ # LPJ-GUESS isn't following our convention so it requries a special case
  yr.file <- as.numeric(strsplit(files.all[i], "_")[[1]][2])
} else {
  yr.file <- as.numeric(substr(files.all[i],nchar(files.all[i])-6, nchar(files.all[i])-3))
}


# Figure out our time indices
lat <- ncvar_get(ncT, "lat")
lon <- ncvar_get(ncT, "lon")



# open the ncdf file:
ncT <- nc_open(file.path(model.dir, files.all[i]))

# grab lat long values 
lat <- ncvar_get(ncT, "lat")
lon <- ncvar_get(ncT, "lon")


# extract variable of interest
dat.all <- ncvar_get(ncT, var)
dims.dat <- dim(dat.all)

# figure out the dimensions
# Figuring out some of the dimension & stats of the data
# NOTE: there should be 30 latitudes & 80 longitudes
# NOTE: we have to do this by variable because pft location throws things off
dims.dat <- dim(dat.all)
if(length(dims.dat)<3 | length(dims.dat)>4){stop("Dimensions of output are weird! Can't continue")}
pft <- ifelse(length(dims.dat)==4, TRUE, FALSE) # Note: I'm using PFT as a general wrapper here for pft or soil
dim.lat <- which(dims.dat==30)
dim.lon <- which(dims.dat==80)

if(length(dim.lat)>1 | length(dim.lon)>1){ stop("2 dimensions meet lat or lon criteria! Need to stop and fix things!")}

# Finding our time dimension
if(pft == FALSE){
  # If there are no pfts, time is our remaining dimension
  dim.time <- c(1:length(dims.dat))[!c(1:length(dims.dat)) %in% c(dim.lat, dim.lon)]
} else {
  # The PFT dims should match the length of PFT names I have
  dim.pft <- which(dims.dat==length(pft.lab) | dims.dat==length(soil))  
  if(!length(dim.pft)==1) stop("PFT/Soil dimension doesn't line up with our PFT/soils lists.  Stop and clarify (and update function)")
  
  dim.time <- c(1:length(dims.dat))[!c(1:length(dims.dat)) %in% c(dim.lat, dim.lon, dim.pft)]
} # End Time dimension define

# Reshape to a common dimensions format
# Note: I have latitude in rows and longitude in columns so that if you take 1 slice it should look right spatially

  dat.all <- aperm(dat.all, c(dim.lat, dim.lon, dim.time, dim.pft)) 
# end data rearranging

# Figure out which times we want
# NOTE: This will require figuring out if the time step is annual or monthly
#       and we're going to do this a clunky way because the time step variables have not
#       been reliable

# Start index
if(dim(dat.all)[3]%%12 == 0){ # if the dimension is evenly divisible by 12, we have monthly data!
  ind.min <- (max(yrmin, yr.file) - yr.file)*12+1 
} else {
  ind.min <- (max(yrmin, yr.file) - yr.file)+1
} # end find start index

# End index
if(i < files.use[length(files.use)]){ # if we're going to need another file after this, take everything
  ind.max <- dim(dat.all)[3]
} else {
  if(dim(dat.all)[3]%%12==0){ # if the dimension is evenly divisible by 12, we have monthly data!
    ind.max <- (yrmax - yr.file + 1)*12
  } else {
    ind.max <- (yrmax - yr.file + 1)
  }
} # end find end index

  
  # create a dataframe with lat and lon values for lookup later
  lats <- data.frame(lat = lat,
                     latrow = 1:30)
  
  lons <- data.frame(lon = lon,
                     lonrow = 1:80)
  
  mod.out[[var]] <- dat.all # add dimnames...need to fix the time vector to change time for each file
  dimnames(dat.all) <- list(lat=lat, lon=lon, time=lab.time, pft=pft.lab)
  #lab.time <- 1:600
  if(dim(mod.out[[var]])[3]%%12==0){ 
    lab.time <- round(yrmin + (1:dim(mod.out[[var]])[3] - 1)/12, 3)
  } else {
    lab.time <- yrmin + (1:dim(mod.out[[var]])[3] - 1)
  }
  
  
  
  
 
  
  # only get the grid cells that the model has been run at
  #dimnames(!is.na(dat.all[,,1,1]))
  
  test <- mod.out$Fcomp
  
  # pull out all grid cells at time point 1 and 1 PFT melt to find the lat lons with data
  test2 <- test[,,1,1]
  tab <- melt(test2)
  
  datain <- tab[!is.na(tab$value),]
  colnames(datain) <- c('latrow', 'lonrow', 'fcomp')
  datain<- merge(datain, lats, by = "latrow")
  datain <- merge(datain, lons, by = "lonrow")
  datain$ID <- 1:40 
  datain$site.name <- paste0("site", datain$ID)
  

  
  #x <- is.na(dat.all[,,,])
  #huh <- which(!x, TRUE)
  
  
  #mod.out$Fcomp
  
  #ind.lat <- which(lat>=ymin & lat<=ymax)
  #ind.lon <- which(lon>=xmin & lon<=xmax)
  # creat indices of the lat lons that we have data for
  ind.lat <- unique(datain$latrow)
  ind.lon <- unique(datain$lonrow)
  
  # select only those grid cells that have data:
  dat.temp <- array(dat.all[ind.lat,ind.lon,ind.min:ind.max,], dim=c(length(ind.lat), length(ind.lon), length(ind.min:ind.max), dim(dat.all)[4]))
  
  
  # now combine all in mod out:
  
  if(i == files.use[1]){ # if this is our first time through start a new layer in the list
    full.out[[var]] <- dat.temp
  } else { # If we already have a list going, add the output to what we already have
    full.out[[var]] <- abind(full.out[[var]], dat.temp, along=3)
  }
  
}
  
  
  
  