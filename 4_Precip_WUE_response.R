#WUE response to precip
#now look at WUE responses to precip

#this is a strange unit (not ppm), but could convert it to ppm
for (s in 1:length(site.list)){
  s <- 5
precip.df <- precip[[s]][, "ed.lu"]
WUEii<- WUEi[[s]][,"clm.bgc"]


#pdf of strange precip trends and units of clm.bgc
pdf(paste0(site.list[s],"precip_trends.pdf"))
plot(precip[[s]][,"ed.lu"], type = "l",ylab = "precip ppm", xlab = "Time (months since 1-1-850",
     main = "ED.lu")
plot(precip[[s]][,"ed2"],type = "l", ylab = "precip ppm", xlab = "Time (months since 1-1-850",
     main = "ED.lu")
plot(precip[[s]][,"clm.bgc"],type = "l", ylab = "precip units ??", xlab = "Time (months since 1-1-850",
     main = "clm.bgc")
plot(precip[[s]][,"clm45"],type = "l", ylab = "precip units ??", xlab = "Time (months since 1-1-850",
     main = "clm.45")
plot(precip[[s]][,"sibcasa"],type = "l", ylab = "precip ppm", xlab = "Time (months since 1-1-850",
     main = "sibcasa")
dev.off()


#use precip from ed.lu?
#create index for the growing season only
index <- Month< 10 & Month >4

sec2month <- (30*24*60*60)
precip.df <- precip[[s]][, "ed.lu"]*sec2month
WUEii<- WUEi[[s]][index,]
WUEii <- WUEt[[s]][index,]
WUEii <- IWUE[[s]][index,]

below20 <- WUEii[precip.df < 20,] #cut off WUEii over 10
above20.40 <- WUEii[precip.df > 20 & precip.df <40, ]
above40.60 <- WUEii[precip.df > 40 & precip.df <60,]
above60.80 <- WUEii[precip.df > 60 & precip.df <80 ,]
above80.100 <- WUEii[precip.df > 80 & precip.df< 100 ,]
above100.120 <- WUEii[precip.df > 100 & precip.df< 120 ,]
above120 <- WUEii[precip.df >= 120,]

a <- data.frame(group = "<20 kg/m2/mo", below20)
b <- data.frame(group = "20-40", above20.40)
c <- data.frame(group = "40-60", above40.60)
d <- data.frame(group = "60-80", above60.80)
e <- data.frame(group = "80-100", above80.100)
f <- data.frame(group = "100-120", above100.120)
g <- data.frame(group = ">120", above120)

df <- rbind(a, b, c, e, f, g)
df.m <- melt(data = df, id = c("group"))

pdf(paste0(site.list[s],"precip_WUEt_response_ha.pdf"))
ggplot(df.m, aes(x = variable, y = value, fill = group )) + 
  geom_boxplot() + ylim(0,7.5)+ ggtitle(paste(site.list[s],"WUEt"))
dev.off()
}


#boxplot(below280, above280.300, above300.320, above320.340,
#    above340.360, above360.380, above380) #maybe slightly higher for higher precip
