library(ggplot2)
library(dplyr)
library(tidyr)
library(coda)
library(reshape2)
library(cowplot)

# read in all the samples:
filenames <- list.files(path = "outputs/ITRDB_models/ITRDB_species_no_re", pattern = "*.rds")
full.filenames <- paste0("outputs/ITRDB_models/ITRDB_species_no_re/", filenames)

spec <- substring(filenames, 7, 10)
# use lapply to read in the rds files:
all.params <- lapply(full.filenames, readRDS)


# get summaries of all the parameters:
all.params.chain1 <- lapply(all.params, function(x){as.mcmc(x[[1]])})
all.summaries <- lapply(all.params.chain1, summary)

names(all.params.chain1) <- spec

# save traceplots
for(i in 1:length(spec)){
  png(height = 6, width = 12, units = "in", res = 200, paste0("outputs/ITRDB_models/ITRDB_species_no_re/traceplot_", spec[i],".png"))
  par(mfrow = c(2,4))
  traceplot(all.params.chain1[[i]])
  dev.off()
}

# save acfplots:
for(i in 1:length(spec)){
  png(height = 6, width = 12, units = "in", res = 200, paste0("outputs/ITRDB_models/ITRDB_species_no_re/acfplot_", spec[i],".png"))
  acfplot(all.params.chain1[[i]])
  dev.off()
}


# plot paramter estimates alltogether
for(i in 1:length(spec)){
  
  ests <- data.frame(all.params.chain1[[i]])
  ests.summary <- data.frame(mean = apply(ests, 2, mean), 
             Ci.low = apply(ests, 2, function(x){quantile(x, 0.025)}), 
             Ci.high = apply(ests, 2, function(x){quantile(x, 0.975)}))
  
  ests.summary$params <- rownames(ests.summary)                                                                 
  
 ggsave( paste0("outputs/ITRDB_models/ITRDB_species_no_re/parameter_ests_", spec[i],".png"))
  ggplot(ests.summary[!ests.summary$params %in% "sigmaY",], aes(params, mean ))+
    geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
      ylab("Parameter Estimates")+xlab("parameter")+theme_bw(base_size = 15)+theme(panel.grid = element_blank())


}

# now save and plot all the paramter estimates together to compare across species:

all.param.ests <- do.call(rbind, all.params.chain1)

all.param.ests <- data.frame(all.param.ests)
all.param.ests$species <- rep(names(all.params.chain1), sapply(all.params.chain1, nrow)) # add the species code names


# then make boxplots or 95% quantile plots of the parameter estimates
all.param.ests.m <- melt(all.param.ests)
all.param.summary <- all.param.ests.m %>% group_by(species, variable) %>% dplyr::summarise(mean = mean(value), 
                                                                    Ci.low = quantile(value, 0.025), 
                                                                    Ci.high = quantile(value, 0.975))


# still need to: assign colors based on Paleon species

INTERCEPTS <- ggplot(all.param.summary[all.param.summary$variable %in% "alpha",], aes(species, mean))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Species intercept Alpha Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

MAPS <- ggplot(all.param.summary[all.param.summary$variable %in% "beta1",], aes(species, mean))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Precipitation Sensitivity(Beta1) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))


JUNTMAX <- ggplot(all.param.summary[all.param.summary$variable %in% "beta2",], aes(species, mean))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Jun Tmax Sensitivity (Beta2) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

PREVRWI_1 <- ggplot(all.param.summary[all.param.summary$variable %in% "beta3",], aes(species, mean))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("PrevRWI_1 (Beta3) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

PREVRWI_2 <- ggplot(all.param.summary[all.param.summary$variable %in% "beta4",], aes(species, mean))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("PrevRWI_1 (Beta4) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

AGE <- ggplot(all.param.summary[all.param.summary$variable %in% "beta5",], aes(species, mean))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Age (Beta5) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

png(height = 18, width = 16, units = "in", res = 300, "outputs/ITRDB_models/ITRDB_species_no_re/All_spec_parameters.png")
cowplot::plot_grid(INTERCEPTS, MAPS, JUNTMAX, PREVRWI_1, PREVRWI_2, AGE, align = "hv",ncol = 2, labels = "AUTO")
dev.off()


# ----------------------Plot predicted vs. observed for each test.dataset----------------------------------
# use apply to read in all the 
test.data.names <- list.files(path = "outputs/ITRDB_models/train_test_data/", pattern = "*_nimble.rds")
index.first <- substring(test.data.names, first = 1, last = 4)
test.dfs <- test.data.names[index.first %in% "test"]
test.dfs.full <- paste0("/Users/kah/Documents/WUE_MIP/WUE_MIP/outputs/ITRDB_models/train_test_data/", test.dfs)
test.list <- lapply(test.dfs.full, readRDS)

test.data.df <- do.call(rbind, test.list)


# get predictions from the betas for each model:

pred.get.rsq <- function(x){
      cat("*")
      meanMAP.sim <- x
      
      int.mcmc <- as.mcmc(all.params.chain1[[1]])
      int.mcmc.mat <- as.matrix(int.mcmc)
      int.mcmc.dat <- data.frame(int.mcmc.mat)
      
      int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(meanMAP.sim$Temp.jun.scaled)), nrow = nrow(int.mcmc.dat))
      
      
      # use betas to generate pp given a value for site, structure, dbh, rwi1, rwi2, and varying T and MAP:
      for(i in 1:length(meanMAP.sim$Temp.jun.scaled)){
        
        
        int.1[,i] <- int.mcmc.dat[,paste0("alpha")]+
          int.mcmc.dat[,paste0("beta1")]*meanMAP.sim[i,]$Precip.scaled+    
          int.mcmc.dat[,paste0("beta2")]*meanMAP.sim[i,]$Temp.jun.scaled + 
          int.mcmc.dat[,paste0("beta3")]*meanMAP.sim[i,]$RWI_1  + 
          int.mcmc.dat[,paste0("beta4")]*meanMAP.sim[i,]$RWI_2 +
          int.mcmc.dat[,paste0("beta5")]*meanMAP.sim[i,]$Age 
          #int.mcmc.dat[,paste0("beta7.", meanMAP.sim[i,"struct.cohort.code"], ".")] * (meanMAP.sim[i,"MAP.scaled"] *meanMAP.sim[i,"T.scaled"])
        
        
      }
      
      meanMAP.sim$idval <- 1:length(meanMAP.sim$Longitude)
      
      # rows are the mcmc values
      colnames(int.1) <- 1:length(meanMAP.sim$Longitude)
      test.m <- melt(int.1)
      colnames(test.m) <- c("MCMC", "idval", "Ypred")
      full.pred <- left_join(test.m, meanMAP.sim, by = "idval")
      full.pred$RWI.pred <- exp(full.pred$Ypred)
      
      # indiv.summary <- full.pred %>% group_by(studyCode, year, ID) %>% dplyr::summarise(mean.pred = mean(RWI.pred, na.rm =TRUE),
      #                                                                              ci.low.pred=quantile(RWI.pred, 0.025, na.rm =TRUE),
      #                                                                              ci.high.pred=quantile(RWI.pred, 0.975, na.rm =TRUE),
      #                                                                              mean.obs = mean(RWI, na.rm =TRUE), 
      #                                                                              ci.low.obs=quantile(RWI, 0.025, na.rm =TRUE),
      #                                                                              ci.high.obs=quantile(RWI, 0.975, na.rm =TRUE))
      # 
      # 
      # 
      indiv.summary <- full.pred %>% group_by( SPEC.CODE,year) %>% dplyr::summarise(mean.pred = mean(RWI.pred, na.rm =TRUE),
                                                                               ci.low.pred=quantile(RWI.pred, 0.025, na.rm =TRUE),
                                                                               ci.high.pred=quantile(RWI.pred, 0.975, na.rm =TRUE),
                                                                               mean.obs = mean(RWI, na.rm =TRUE), 
                                                                               ci.low.obs=quantile(RWI, 0.025, na.rm =TRUE),
                                                                               ci.high.obs=quantile(RWI, 0.975, na.rm =TRUE))
      
      
      summary.lm <- summary(lm(RWI.pred ~ RWI ,data = full.pred))
      
      site.model.fit <- data.frame(spec = unique(indiv.summary$SPEC.CODE),
        rsq = summary.lm$r.squared)
      
      site.model.fit
}

rsq.site <- lapply(test.list, pred.get.rsq)
#ggplot(indiv.summary , aes(mean.obs, mean.pred))+geom_point()+geom_abline(intercept = 0, 1, color = "red")+ylim(0,10)+xlim(0,7)+theme(legend.position = "none")+facet_wrap(~studyCode)

# pred.obs.time <- ggplot(indiv.summary , aes(year, mean.pred))+geom_line()+geom_ribbon(aes(ymin = ci.low.pred, ymax = ci.high.pred, fill = site), alpha = 0.25, linetype = "dashed", colour = NA)+
#   geom_point(data = indiv.summary , aes(year, mean.obs), color = "black", size = 0.05)+geom_line(data = indiv.summary , aes(year, mean.obs), color = "red", linetype = "dashed")+theme(legend.position = "none")+
#   facet_wrap(~site, scales = "free_y")+ylab("Predicted Tree Growth (mm)")+xlab("Year")



# ---------Plot posterior predictive response of each species to temperature & precipitation pre & post:

