# script to read testing and training data, fit species level climate responses with no random effects:

#library(dplyr)
#install.packages("nimble", lib="~/myRlibs",repos='http://cran.us.r-project.org')
library(igraph)
library(nimble)

library(coda)

# get the unique spec.codes:
#rwl.itrdb.clim.nona <- readRDS( paste0(getwd(),"/Data/ITRDB/full.clim.prism.rds"))
rwl.itrdb.clim.nona <- readRDS( paste0(getwd(),"/Data/full.clim.prism.rds"))
rwl.full <- rwl.itrdb.clim.nona[!is.na(rwl.itrdb.clim.nona$RWI_1) & !is.na(rwl.itrdb.clim.nona$RWI_2)  ,]
spec.list  <- as.character( unique(rwl.full$SPEC.CODE))


# specify the nimble model:

clim.reg.re.period <- nimbleCode({ 
  
  alpha ~ dnorm(0, sd = 1000) 
  #beta1 ~ dnorm(0, sd = 1000) 
  #beta2 ~ dnorm(0, sd = 1000) 
  #beta3 ~ dnorm(0, sd = 1000) 
  #beta4 ~ dnorm(0, sd = 1000)
  #beta5 ~ dnorm(0, sd = 1000)
  sigmaY  ~ dunif(0, 100)
  #mu_alpha ~ dunif(-2, 2)
  
  
  #inv_alpha   ~ dgamma(0.001, 0.001)
  #sigma_alpha <- 1/sqrt(inv_alpha)
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
  
  mu_beta1 ~ dunif(-2, 2)
  mu_beta2 ~ dunif(-2, 2)
  mu_beta3 ~ dunif(-2, 2)
  mu_beta4 ~ dunif(-2, 2)
  mu_beta5 ~ dunif(-2, 2)
  mu_beta6 ~ dunif(-2, 2)

    for(c in 1:C){
      
      beta1[c] ~ dnorm(mu_beta1, inv_beta1)
      beta2[c] ~ dnorm(mu_beta2, inv_beta2)
      beta3[c] ~ dnorm(mu_beta3, inv_beta3)
      beta4[c] ~ dnorm(mu_beta4, inv_beta4)
      beta5[c] ~ dnorm(mu_beta5, inv_beta5)
      beta6[c] ~ dnorm(mu_beta6, inv_beta6)
    
    }
    for(i in 1:N) {
      predictedY[i] <- alpha + beta1[period[i]]*Precip.scaled[i] + beta2[period[i]]*Temp.jja.scaled[i]+ beta3[period[i]]*RWI_1[i] + beta4[period[i]]*RWI_2[i] + beta5[period[i]]*Age[i]+ beta6[period[i]]*(Temp.jja.scaled[i]*Precip.scaled[i])
      Y[i] ~ dnorm(predictedY[i], sigmaY)
      
    
    }
  
})

# params we are interested in:
params <- c("alpha",  "beta1", "beta2", "beta3", "beta4", "beta5", "beta6","sigmaY")


run.nimble.spec <- function(spec, train.test.dir,niter, burnin, thin, nchains){
if(file.exists(paste0("outputs/ITRDB_models/ITRDB_species_time_re_tp_int/samps_", spec,niter,"iter_", burnin,"burnin_",thin,"thin" ,".rds"))){
  cat(paste("model for ....", spec, "exists already"))
  }else{cat(paste("Running model for....", spec))
      # readin testing data:
      train.data <-  readRDS(paste0(train.test.dir, "/train_",spec,"_nimble.rds"))
      train.data$RWI_log <- log(train.data$RWI)
      train.data$timeclass <- ifelse(train.data$year <= 1945, "Pre-1945", "Post-1945" )
      train.data$time_num <- ifelse(train.data$year <= 1945, 1,2)
      
      # specify the species-specific parameters:
      SPECIES.data <- list(
        
        Y = train.data$RWI_log, Precip.scaled = train.data$Precip.scaled, 
        Temp.jja.scaled = train.data$Temp.jun.scaled, RWI_1 = train.data$RWI_1, 
        RWI_2 = train.data$RWI_2, Age = train.data$Age, period = as.numeric(train.data$time_num) )
      
      
      SPECIES.constants <- list( N=length(train.data$RWI),
                                 C = length(unique(train.data$time_num)))
      

      # need to specify inits because sometimes it makes them NA values:
      inits <- function() list(beta1 = rnorm(SPECIES.constants$C),
                               beta2 = rnorm(SPECIES.constants$C),
                               beta3 = rnorm(SPECIES.constants$C),
                               beta4 = rnorm(SPECIES.constants$C),
                               beta5 = rnorm(SPECIES.constants$C),
                               beta6 = rnorm(SPECIES.constants$C))
      
      
      system.time(SPECIES.samples <- nimbleMCMC(
        code = clim.reg.re.period,
        constants = SPECIES.constants,
        data = SPECIES.data, ## provide the combined data & constants as constants
        inits = inits,
        monitors = params,
        niter = niter,
        nburnin = burnin,
        thin = thin, 
        nchains = nchains))
      
      
      if(nchains <= 1){
      coda.samples <- as.mcmc(SPECIES.samples)
      summary(coda.samples)
      }else{
        coda.samples <- as.mcmc(SPECIES.samples[[1]])
        summary(coda.samples)
      }
      #traceplot(coda.samples)
      #acfplot(coda.samples)
      
      
      # save outputs:
      saveRDS(SPECIES.samples, paste0("outputs/ITRDB_models/ITRDB_species_time_re_tp_int/samps_", spec,niter,"iter_", burnin,"burnin_",thin,"thin" ,".rds"))
}
      }


# test on a species with small # of data:

# run.nimble.spec(spec = "PIVI", 
#                 train.test.dir = "outputs/ITRDB_models/train_test_data/",
#                 niter = 25000, burnin= 200, thin = 20, nchains = 3)
# 
# run.nimble.spec(spec = "FAGR", 
#                 train.test.dir = "outputs/ITRDB_models/train_test_data/",
#                 niter = 25000, burnin= 200, thin = 20, nchains = 3)
# 
# sessionInfo()

for(i in 1:length(spec.list)){
run.nimble.spec(spec = spec.list[i], 
                train.test.dir = "outputs/ITRDB_models/train_test_data/",
                niter = 25000, burnin= 200, thin = 20, nchains = 3)
  
  


}


# FAGR.samps <- readRDS("outputs/ITRDB_models/ITRDB_species_time_re/samps_PIVI15000iter_200burnin_20thin.rds")
# coda.samples <- as.mcmc(FAGR.samps[[1]])
# summary(coda.samples)
# traceplot(coda.samples)
# acfplot(coda.samples)

