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


#use CO2 from ed.lu?
CO2.df <- CO2[[s]][, "ed.lu"]
WUEii<- WUEi[[s]]


below280 <- WUEii[CO2.df < 280 & WUEii <10,] #cut off WUEii over 10
above280.300 <- WUEii[CO2.df > 280 & CO2.df <300 & WUEii <10,]
above300.320 <- WUEii[CO2.df >= 300 & CO2.df <320 & WUEii <10,]
above320.340 <- WUEii[CO2.df >= 320 & CO2.df <340 & WUEii <10,]
above340.360 <- WUEii[CO2.df >= 340 & CO2.df< 360 & WUEii <10,]
above360.380 <- WUEii[CO2.df >= 360 & CO2.df< 380 & WUEii <10,]
above380 <- WUEii[CO2.df >= 380&  WUEii <10,]


#plots for all models --ggplot?


#boxplot(below280, above280.300, above300.320, above320.340,
    #    above340.360, above360.380, above380) #maybe slightly higher for higher CO2
