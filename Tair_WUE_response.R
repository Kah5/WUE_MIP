#Temperature response
#WUE response to tair
#now look at WUE responses to tair

#this is a strange unit (not ppm), but could convert it to ppm
s <- 4
tair.df <- tair[[s]][, "ed.lu"] - 273.15
WUEii<- WUEi[[s]][,"clm.bgc"]


#pdf of strange tair trends and units of clm.bgc
pdf("tair_trends_BL.pdf")
plot(tair[[s]][,"ed.lu"], type = "l",ylab = "tair ppm", xlab = "Time (months since 1-1-850",
     main = "ED.lu")
plot(tair[[s]][,"ed2"],type = "l", ylab = "tair ppm", xlab = "Time (months since 1-1-850",
     main = "ED.lu")
plot(tair[[s]][,"clm.bgc"],type = "l", ylab = "tair units ??", xlab = "Time (months since 1-1-850",
     main = "clm.bgc")
plot(tair[[s]][,"clm45"],type = "l", ylab = "tair units ??", xlab = "Time (months since 1-1-850",
     main = "clm.45")
plot(tair[[s]][,"sibcasa"],type = "l", ylab = "tair ppm", xlab = "Time (months since 1-1-850",
     main = "sibcasa")
dev.off()


#use tair from ed.lu?
#sec2month <- (30*24*60*60)
tair.df <- tair[[s]][, "ed.lu"]- 273.15
WUEii<- WUEi[[s]]
WUEii <- WUEt[[s]]
WUEii <- IWUE[[s]]

below0 <- WUEii[tair.df < 0,] #cut off WUEii over 10
above0.6 <- WUEii[tair.df > 0 & tair.df <6, ]
above6.12 <- WUEii[tair.df > 6 & tair.df <12,]
above12.14 <- WUEii[tair.df > 12 & tair.df <14 ,]
above14.16 <- WUEii[tair.df > 14 & tair.df< 16 ,]
above16.18 <- WUEii[tair.df > 16 & tair.df< 18 ,]
above18.20 <- WUEii[tair.df > 18 & tair.df <20,]
above20.22 <- WUEii[tair.df > 20 & tair.df <22,]
above22 <- WUEii[tair.df > 22,]

a <- data.frame(group = "<0", below0)
b <- data.frame(group = "0-6", above0.6 )
c <- data.frame(group = "6-12", above6.12 )
d <- data.frame(group = "12-14", above12.14)
e <- data.frame(group = "14-16", above14.16)
f <- data.frame(group = "16-18", above16.18)
g <- data.frame(group = "18-20", above18.20 )
h <- data.frame(group = "20-22", above20.22)
j <- data.frame(group = ">22", above22)

df <- rbind(a, b, c, e, f, g, h, j)
df.m <- melt(data = df, id = c("group"))

pdf("tair_IWUE_response.pdf")
ggplot(df.m, aes(x = variable, y = value, fill = group )) + 
  geom_boxplot() + ylim(0,7.5)+ ggtitle(paste(site.list[s],"IWUE"))
dev.off()
#plots for all models --ggplot?

df <- data.frame(f1=factor(rbinom(100, 1, 0.45), label=c("m","w")), 
                 f2=factor(rbinom(100, 1, 0.45), label=c("young","old")),
                 boxthis=rnorm(100))
#boxplot(below280, above280.300, above300.320, above320.340,
#    above340.360, above360.380, above380) #maybe slightly higher for higher tair
