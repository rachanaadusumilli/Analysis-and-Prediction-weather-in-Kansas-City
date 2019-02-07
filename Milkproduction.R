library(TTR)
milkprodTS<-ts(milkprod$Pounds_per_Cow,frequency = 12,start=c(1962,1))
plot.ts(milkprodTS)
sTS3 = SMA(milkprodTS,n=3)
sTS5 = SMA(milkprodTS,n=5)
sTS8  = SMA(milkprodTS,n=8)
lines(milkprodTS, col="black")
lines(sTS3, col="darkgoldenrod1")
lines(sTS5, col="blue")
lines(sTS8,col="brown1")
milkprodTS<-ts(milkprod$Pounds_per_Cow,frequency = 12,start=c(1962,1))
plot.ts(milkprodTS)

# simple exponential smoothing without seasonal and trend component
sHW = HoltWinters(milkprodTS,beta=FALSE, gamma=FALSE)

# Holt-Winters exponential smoothing with trend and additive seasonal component
sHW1 = HoltWinters(milkprodTS)
sHW
lines(sHW$fitted[,1], col= "green")

sHW1
lines(sHW1$fitted[,1], col= "red")



