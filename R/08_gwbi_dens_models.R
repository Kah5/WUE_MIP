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

Dens.GUESS <- readRDS("Data/GUESS.Dens.pft.wide.rds")
Dens.GUESS$Site <- paste0("X", Dens.GUESS$Site)

GUESS.all.y <- left_join(GUESS.all, GWBI.GUESS, by =c("Year", "Site"))
GUESS.all <- left_join(GUESS.all.y, Dens.GUESS, by =c("Year", "Site"))

# make a DF paralell to ED2:
colnames(ED.all)
colnames(GUESS.all)

GUESS.totals <- GUESS.all[,c("Year", "Site", "Rel.Dens", "IWUE", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                             "Total.Dens", "Total.gwbi", "Tair.C.jja", "precip.mm.jja")]

GUESS.totals$Model <- "GUESS"
colnames(GUESS.totals) <- c("Year", "Site", "Rel.Dens", "IWUE", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                         "Dens", "gwbi", "Tair.C.jja", "precip.mm.jja", "Model")


# now subset ED.all by because we dont have all the WUE

ED.totals <- ED.all[,c("Year", "Site", "Rel.Dens", "IWUE", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                             "Dens", "GS_gwbi", "Tair.C.jja", "precip.mm.jja")]
ED.totals$Model <- "ED2"
colnames(ED.totals) <- c("Year", "Site", "Rel.Dens", "IWUE", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                         "Dens", "gwbi", "Tair.C.jja", "precip.mm.jja", "Model")



# combine together (in case we want to model together):
ED.GUESS <- rbind(ED.totals, GUESS.totals)

# some prelimiary plots to visualize the data:
ggplot(ED.GUESS, aes( precip.mm, gwbi,  color = Model))+geom_point()+stat_smooth()+theme(legend.position = "none")+theme_bw()
ggplot(ED.GUESS, aes( Tair.C.jja,gwbi, color = Model))+geom_point()+stat_smooth()+theme(legend.position = "none")+theme_bw()
ggplot(ED.GUESS, aes( CO2, gwbi, color = Model))+geom_point()+stat_smooth()+theme(legend.position = "none")+theme_bw()
ggplot(ED.GUESS, aes( precip.mm.jja, gwbi, color = Model))+geom_point()+stat_smooth()+theme(legend.position = "none")+theme_bw()



#----------------------------------------
# Separate Testing and Training Datasets:

# 1. initial data cleaning & checking
# 2. create dummy variables for CO2 classes: "modern", "past", "pre-industrial"
# 3. calculate agbi lagged
# 4. remove NA values for lagged agbi
# 5. scale climate parameters
# 6. Split into testing and training data:

# remove NAS: 
ED <- ED.totals[!is.na(ED.totals$gwbi) & !is.na(ED.totals$precip.mm),]
GUESS <- GUESS.totals[!is.na(GUESS.totals$gwbi) & !is.na(GUESS.totals$precip.mm),]

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

ED.sort.wide <- ED.sort[,c("Year", "Site", "gwbi")] %>% spread(key = "Site", value = "gwbi")
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
colnames(ED.sort_1) <- c("Year", "Site", "gwbi_1")
ED.sort_2 <- melt(ED.sort_2.wide, id.vars = c("Year"))
colnames(ED.sort_2) <- c("Year", "Site", "gwbi_2")

ED.sort_3 <- melt(ED.sort_3.wide, id.vars = c("Year"))
colnames(ED.sort_3) <- c("Year", "Site", "gwbi_3")

ED.sort_4 <- melt(ED.sort_4.wide, id.vars = c("Year"))
colnames(ED.sort_4) <- c("Year", "Site", "gwbi_4")

ED.sort_5 <- melt(ED.sort_5.wide, id.vars = c("Year"))
colnames(ED.sort_5) <- c("Year", "Site", "gwbi_5")

ED.sort_lag <- left_join(ED.sort_1, ED.sort_2, by = c("Year", "Site"))
ED.sort_lag34 <- left_join(ED.sort_3, ED.sort_4, by = c("Year", "Site"))
ED.sort_lag <- left_join(ED.sort_lag, ED.sort_lag34, by = c("Year", "Site"))
ED.sort_lag <- left_join(ED.sort_lag, ED.sort_5, by = c("Year", "Site"))

ED.sort_lag <- left_join(ED.sort, ED.sort_lag, by = c("Year", "Site"))

# remove NAs from agbi_1 and agbi_2:

ED.sort_lag <- ED.sort_lag[!is.na(ED.sort_lag$gwbi_1) & !is.na(ED.sort_lag$gwbi_2) & !is.na(ED.sort_lag$gwbi_3) & !is.na(ED.sort_lag$gwbi_4) & !is.na(ED.sort_lag$gwbi_5),]

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

GUESS.sort.wide <- GUESS.sort[,c("Year", "Site", "gwbi")] %>% spread(key = "Site", value = "gwbi")
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
colnames(GUESS.sort_1) <- c("Year", "Site", "gwbi_1")
GUESS.sort_2 <- melt(GUESS.sort_2.wide, id.vars = c("Year"))
colnames(GUESS.sort_2) <- c("Year", "Site", "gwbi_2")

GUESS.sort_3 <- melt(GUESS.sort_3.wide, id.vars = c("Year"))
colnames(GUESS.sort_3) <- c("Year", "Site", "gwbi_3")

GUESS.sort_4 <- melt(GUESS.sort_4.wide, id.vars = c("Year"))
colnames(GUESS.sort_4) <- c("Year", "Site", "gwbi_4")

GUESS.sort_5 <- melt(GUESS.sort_5.wide, id.vars = c("Year"))
colnames(GUESS.sort_5) <- c("Year", "Site", "gwbi_5")

GUESS.sort_lag <- left_join(GUESS.sort_1, GUESS.sort_2, by = c("Year", "Site"))
GUESS.sort_lag34 <- left_join(GUESS.sort_3, GUESS.sort_4, by = c("Year", "Site"))
GUESS.sort_lag <- left_join(GUESS.sort_lag, GUESS.sort_lag34, by = c("Year", "Site"))
GUESS.sort_lag <- left_join(GUESS.sort_lag, GUESS.sort_5, by = c("Year", "Site"))

GUESS.sort_lag <- left_join(GUESS.sort, GUESS.sort_lag, by = c("Year", "Site"))

# remove NAs from agbi_1 and agbi_2:

GUESS.sort_lag <- GUESS.sort_lag[!is.na(GUESS.sort_lag$gwbi_1) & !is.na(GUESS.sort_lag$gwbi_2) & !is.na(GUESS.sort_lag$gwbi_3) & !is.na(GUESS.sort_lag$gwbi_4) & !is.na(GUESS.sort_lag$gwbi_5),]

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
#------------------------------
# split training and testing for GUESS:
# 
msk2 <- caTools::sample.split( GUESS.sort_lag, SplitRatio = 3/4, group = NULL )

train.GUESS.full <- GUESS.sort_lag[msk2,]
train.GUESS.full <- GUESS.sort_lag[!msk2,]

train.GUESS  <- train.GUESS.full[train.GUESS.full$period %in% c("modern-industrial", "industrial-past"),]
train.GUESS  <- train.GUESS.full[train.GUESS.full$period %in% c("modern-industrial", "industrial-past"),]

class(train.ED$site_code)


# -------------------------------------------
# Develop bayesian mixed models for ED
#--------------------------------------------
library(mgcv)
mod <- lm(gwbi ~ Precip.scaled + Temp.jja.scaled + CO2 + gwbi_1 +gwbi_2 +gwbi_3 +gwbi_4+gwbi_5 + Site, data = train.ED)
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
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is agbi

# function g()
gfunc.p[i] <- alpha[sites.p[i]] + beta1[period.p[i]]*Precip.scaled.p[i] + beta2[period.p[i]]*Temp.jja.scaled.p[i] + beta3[period.p[i]]*agbi_1.p[i] + beta4[period.p[i]]*agbi_2.p[i] + beta5[period.p[i]]*agbi_3.p[i] + beta6[period.p[i]]*agbi_4.p[i]# use Drought index as a scaled variable 

}

}"






reg.EDel.by_period <- jags.model(textConnection(ED_re_site_time_period), 
                               data = list(Y = train.ED$gwbi, n=length(train.ED$gwbi), Precip.scaled = train.ED$Precip.scaled, Temp.jja.scaled = train.ED$Temp.jja.scaled, agbi_1 = train.ED$gwbi_1,agbi_2 = train.ED$gwbi_2, agbi_3 = train.ED$gwbi_3, agbi_4 = train.ED$gwbi_4,
                                           period = as.numeric(train.ED$period_cd), S = unique(train.ED$site_code),  C = unique(train.ED$period_cd), sites = as.numeric(train.ED$site_code), np=length(test.ED$period_cd), 
                                           sites.p = test.ED$site_code, Precip.scaled.p = test.ED$Precip.scaled, Temp.jja.scaled.p = test.ED$Temp.jja.scaled, agbi_1.p = test.ED$gwbi_1, agbi_2.p = test.ED$gwbi_2, agbi_3.p = test.ED$gwbi_3, agbi_4.p = test.ED$gwbi_4,
                                           period.p = as.numeric(test.ED$period_cd)), n.chains = 3, n.adapt = 100)


update(reg.EDel.by_period, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.ED.period <- coda.samples(reg.EDel.by_period, 
 #                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
  #                          n.chains = 3, n.iter=2000, thin = 10)
samp.ED.period <- coda.samples(reg.EDel.by_period, 
                              variable.names=c("alpha", "beta1", "beta2" ), 
                              n.chains = 3, n.iter=5000, thin = 1)

samp.ED.ypred <- coda.samples(reg.EDel.by_period, 
                               variable.names=c("Ypred" ), 
                               n.chains = 3, n.iter=5000, thin = 1)

#Extract the samples for each parameter

samps       <- samp.ED.period[[1]]
Yp.samps    <- samp.ED.ypred [[1]]
alpha.samps <- samps[,(length(test.ED$site_num)+1):(length(test.ED$site_num)+length(unique(test.ED$site_num)))]
beta.samps  <- samps[,(length(test.ED$site_num)+length(unique(test.ED$site_num))+1):(length(test.ED$site_num)+length(unique(test.ED$site_num))+3)]
sigma.samps <- samps[,(length(test.ED$site_num)+length(unique(test.ED$site_num))+4):(length(test.ED$site_num)+length(unique(test.ED$site_num))+7)]


# caluculate 95% CI for alpha and beta samps 
alpha.025 <- quantile(alpha.samps, 0.025)
alpha.975 <- quantile(alpha.samps, 0.975)
beta1.025 <- quantile(beta.samps[,1], 0.025)
beta1.975 <- quantile(beta.samps[,1], 0.975)
beta2.025 <- quantile(beta.samps[,2], 0.025)
beta2.975 <- quantile(beta.samps[,2], 0.975)



a <- data.frame('alpha' = alpha.samps, 'beta.precip' = beta.samps[,1], 'beta.temp' = beta.samps[,2])
colnames(a)<- c("alpha", "beta.precip", "beta.temp")
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))

# make dot + 95% CI plots for each param
a.mplots <- ggplot(a.m, aes(value, color = variable))+geom_density(alpha = 0.5)+theme_bw()

# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.ED$gwbi

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.ED$gwbi))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+ylim(-0.35, 1.5)+xlim(-0.35,1.5)+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
#png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/basic_reg/pred_vs_obs.png")
p.o.plot
#dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.ED$gwbi)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.ED$gwbi)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# from here we want to get sensitivities
# also plot posteriors by sites:


#------------------------------------------------------------------------------
# Run the model for GUESS:
#-------------------------------------------------------------------------------
library(mgcv)
mod <- lm(gwbi ~ Precip.scaled + Temp.jja.scaled + CO2 + gwbi_1 +gwbi_2 +gwbi_3 +gwbi_4+gwbi_5 + Site, data = train.GUESS)
summary(mod)
#plot(mod)
preds <- predict(mod, test.GUESS)
plot(test.GUESS$gwbi, preds)
abline(a = 0, b = 1, col= "red")

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

}"






reg.model.by_period <- jags.model(textConnection(GUESS_re_site_time_period), 
                                data = list(Y = train.GUESS$gwbi, n=length(train.GUESS$gwbi), Precip.scaled = train.GUESS$Precip.scaled, Temp.jja.scaled = train.GUESS$Temp.jja.scaled, agbi_1 = train.GUESS$gwbi_1,agbi_2 = train.GUESS$gwbi_2, agbi_3 = train.GUESS$gwbi_3, agbi_4 = train.GUESS$gwbi_4,
                                            period = as.numeric(train.GUESS$period_cd), S = unique(train.GUESS$site_code),  C = unique(train.GUESS$period_cd), sites = train.GUESS$site_code, np=length(test.GUESS$period_cd), 
                                            sites.p = test.GUESS$site_code, Precip.scaled.p = test.GUESS$Precip.scaled, Temp.jja.scaled.p = test.GUESS$Temp.jja.scaled, agbi_1.p = test.GUESS$gwbi_1, agbi_2.p = test.GUESS$gwbi_2, agbi_3.p = test.GUESS$gwbi_3, agbi_4.p = test.GUESS$gwbi_4,
                                            period.p = as.numeric(test.GUESS$period_cd)), n.chains = 3, n.adapt = 100)


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

#Extract the samples for each parameter

samps       <- samp.GUESS.period[[1]]
Yp.samps    <- samp.GUESS.ypred [[1]]
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
Yp.summary$Observed <- test.GUESS$gwbi

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.GUESS$gwbi))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+ylim(-0.35, 1.5)+xlim(-0.35,1.5)+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
#png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/basic_reg/pred_vs_obs.png")
p.o.plot
#dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.GUESS$gwbi)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.GUESS$gwbi)

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

#>>>>>>>> plot dot plots from guess model:


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
int.dot <- ggplot(data.frame(a1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+scale_color_manual(values = c("Past-Savanna"='#a6611a',
                                                                                                                                                                                                                                        "Modern-Savanna"='#dfc27d',
                                                                                                                                                                                                                                        "Modern-Forest"='#c7eae5',
                                                                                                                                                                                                                                        "Past-Forest"='#018571'))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Intercept (alpha)")+xlim(-0.3, 0.8) + geom_vline(xintercept = 0, linetype = "dashed")


ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Precipitation sensitivity)")+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")


ggplot(data.frame(b2.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated JJA temperauture sensitivity")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

ggplot(data.frame(b3.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -1 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

ggplot(data.frame(b4.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -2 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

ggplot(data.frame(b5.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -3 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

ggplot(data.frame(b6.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -4 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")


# combine all the plots together and save to output:
