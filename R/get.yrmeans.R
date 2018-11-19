library(reshape2)
library(ggplot2)
library(tidyr)

get.yrmeans <- function(df, var){
  df <- data.frame(df)
  df$Year <- year
  df$Month <- month
  m <- melt(df, id.vars=c("Year", "Month"))
  yrmeans<-dcast(m, Year ~ variable, mean, na.rm=TRUE)
  m2 <- melt(yrmeans, id.vars= "Year")
  m2$Year <- as.numeric(m2$Year)
  
  #yrmeans
  colnames(m2) <- c("Year", "Site", var)
  m2$Site <- as.character(m2$Site)
  m2
}

# funciton for getting the JJA yearly means
get.JJAmeans <- function(df, var){
  df <- data.frame(df)
  df$Year <- year
  df$Month <- month
  m <- melt(df, id.vars=c("Year", "Month"))
  m <- m[m$Month %in% c(6,7,8),]
  yrmeans <- dcast(m, Year ~ variable, mean)
  m2 <- melt(yrmeans, id.vars= "Year")
  m2$Year <- as.numeric(m2$Year)
  
  colnames(m2) <- c("Year", "Site", var)
  m2$Site <- as.character(m2$Site)
  m2
}

# funciton for getting the growing season 5-10 yearly means
get.GSmeans <- function(df, var){
  df <- data.frame(df)
  df$Year <- year
  df$Month <- month
  m <- melt(df, id.vars=c("Year", "Month"))
  m <- m[m$Month %in% c(5,6,7,8, 9,10),]
  yrmeans <- dcast(m, Year ~ variable, mean)
  m2 <- melt(yrmeans, id.vars= "Year")
  m2$Year <- as.numeric(m2$Year)
  
  colnames(m2) <- c("Year", "Site", var)
  m2$Site <- as.character(m2$Site)
  m2
}

# funciton for getting the JJA yearly means
get.mo.means <- function(df, mo, var){
  df <- data.frame(df)
  df$Year <- year
  df$Month <- month
  m <- melt(df, id.vars=c("Year", "Month"))
  m <- m[m$Month %in% mo,]
  yrmeans <- dcast(m, Year ~ variable, mean)
  m2 <- melt(yrmeans, id.vars= "Year")
  m2$Year <- as.numeric(m2$Year)
  
  colnames(m2) <- c("Year", "Site", var)
  m2$Site <- as.character(m2$Site)
  m2
}

# basic function to relativize data:
relativize <- function(df){
  test <- dcast(df, Year ~ Site)
  Relvalue <- test
  
  for(i in 1:length(paleon$num)){
    Relvalue[,i+1] <- test[,i+1]/mean(test[,i+1], na.rm=TRUE)
  }
}