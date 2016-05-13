#now look at WUE responses to CO2

#this is a strange unit (not ppm), but could convert it to ppm
s <- 4
CO2.df <- CO2[[s]][, "ed.lu"]
WUEii<- WUEi[[s]][,"clm.bgc"]


#pdf of strange CO2 trends and units of clm.bgc
pdf("CO2_trends_BL.pdf")
plot(CO2[[s]][,"ed.lu"], type = "l",ylab = "CO2 ppm", xlab = "Time (months since 1-1-850",
     main = "ED.lu")
plot(CO2[[s]][,"ed2"],type = "l", ylab = "CO2 ppm", xlab = "Time (months since 1-1-850",
     main = "ED.lu")
plot(CO2[[s]][,"clm.bgc"],type = "l", ylab = "CO2 units ??", xlab = "Time (months since 1-1-850",
     main = "clm.bgc")
plot(CO2[[s]][,"clm45"],type = "l", ylab = "CO2 units ??", xlab = "Time (months since 1-1-850",
     main = "clm.45")
plot(CO2[[s]][,"sibcasa"],type = "l", ylab = "CO2 ppm", xlab = "Time (months since 1-1-850",
     main = "sibcasa")
dev.off()

#create index for the growing season only
index <- Month< 9 & Month >4

#use CO2 from ed.lu?
CO2.df <- CO2[[s]][index, "ed.lu"]
WUEii<- WUEi[[s]][index,]
#WUEii <- WUEt[[s]][index,]
#WUEii <- IWUE[[s]][index,]
WUEii <- WUEet[[s]][index,]
below280 <- WUEii[CO2.df < 280,] #cut off WUEii over 10
above280.300 <- WUEii[CO2.df < 300 & CO2.df >280, ]
above300.320 <- WUEii[CO2.df < 320 & CO2.df >300,]
above320.340 <- WUEii[CO2.df > 320 & CO2.df <340 ,]
above340.360 <- WUEii[CO2.df > 340 & CO2.df< 360 ,]
above360.380 <- WUEii[CO2.df > 360 & CO2.df< 380 ,]
above380 <- WUEii[CO2.df >= 380 &  WUEii <10,]

a <- data.frame(group = "<280ppm", below280)
b <- data.frame(group = "280-300", above280.300)
c <- data.frame(group = "300-320", above300.320)
d <- data.frame(group = "320-340", above320.340)
e <- data.frame(group = "340-360", above340.360)
f <- data.frame(group = "360-380", above360.380)
g <- data.frame(group = ">380", above380)

df <- rbind(a, b, c, e, f, g)
df.m <- melt(data = df, id = c("group"))

pdf("CO2_IWUE_response.pdf")
ggplot(df.m, aes(x = variable, y = value, fill = group )) + 
  geom_boxplot() + ylim(0,7.5)+ ggtitle(paste(site.list[s],"WUEi"))
dev.off()
#plots for all models --ggplot?

df <- data.frame(f1=factor(rbinom(100, 1, 0.45), label=c("m","w")), 
                 f2=factor(rbinom(100, 1, 0.45), label=c("young","old")),
                 boxthis=rnorm(100))
#boxplot(below280, above280.300, above300.320, above320.340,
    #    above340.360, above360.380, above380) #maybe slightly higher for higher CO2
