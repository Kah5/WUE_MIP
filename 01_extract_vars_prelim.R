# lets get the variables that we are interested in from LPJ-GUESS:

# We need:
#    1. model -- the name of the model you're interested in; this should correspond to the PREFIX of the model directory
#    2. model.dir -- the file path to where you extracted the compressed model directory; DO NOT MOVE FILES AROUND INSIDE THAT DIRECTORY!
#    3. vars  -- a vector of the variables you wish to extract for each site
#    4. xmin  -- the minimum (western0most) longitude of interest
#    5. xmax  -- the maximum (eastern-most) longitude of interest
#    6. ymin  -- the minimum (southern-most) latitude of interest
#    7. ymax  -- the maximum (northern-most) latitude of interest
#    8. yrmin -- the first year of interest; defaults to 850 (first year of simulations)
#    9. yrmax -- the last year of interest; defaults to 2010 (last year of simulations)
#
# Note: This will by default return the entire time series at the raw time step provided by each model
mod <- "LPJ-GUESS"
mdir <- "C:/Users/JMac/Documents/Kelly/MIP/WUE_MIP/WUE_MIP/Data/LPJ-GUESS/"

#vector of variables
vars <- c("CO2", "NPP", "Dens", "Fire", "PFT", "Fcomp", "GWBI", "tair")

# bounding box info:
xmin <- -100
xmax <- -70
ymin <- 35
ymax <- 50
yrmin <- 850
yrmax <- 2010

source("extract_output_region.R")

extract.paleon.site(model = mod, model.dir = mdir, vars = vars, xmin=-100, xmax=-60, ymin=35, ymax=50, yrmin=850, yrmax=2010)
  