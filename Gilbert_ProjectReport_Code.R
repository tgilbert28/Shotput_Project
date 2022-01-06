library(mosaic)
library(stats)
library(ggplot2)
library(MASS)
library(leaps)
library(olsrr)
library(ggplot2)

shot.data = read.csv("shotput.csv", header = T)
head(shot.data)

qqnorm(shot.data$rVel, main = "QQPlot of Velocity")
qqline(shot.data$rVel, main = "QQPlot of Velocity")
qqnorm(shot.data$rAng, main = "QQPlot of Angle")
qqline(shot.data$rAng, main = "QQPlot of Angle")
qqnorm(shot.data$rHei, main = "QQPlot of Height")
qqline(shot.data$rHei, main = "QQPlot of Height")

ggplot(shot.data, aes(x = rHei)) + geom_density() + ggtitle("Density plot of release height") + xlab("Release Height") + theme(plot.title = element_text(hjust = 0.5))
ggplot(shot.data, aes(x = rVel)) + geom_density() + ggtitle("Density plot of release velocity") + xlab("Release Velocity") + theme(plot.title = element_text(hjust = 0.5))
ggplot(shot.data, aes(x = rAng)) + geom_density() + ggtitle("Density plot of release angle") + xlab("Release Angle") + theme(plot.title = element_text(hjust = 0.5))
pairs(~(shot.data$distance) + (shot.data$rVel) + (shot.data$rAng) + shot.data$rHei,pch = 19, lower.panel = NULL)

topfive = subset(shot.data, distance >= 21.66)
others = subset(shot.data, distance < 21.66)

topfivevel = mean(topfive$rVel)
topfiveang = mean(topfive$rAng)
topfivehei = mean(topfive$rHei)

veltest = t.test(others$rVel, mu = topfivevel)
veltest 
angtest = t.test(others$rAng, mu = topfiveang)
angtest 
heitest = t.test(others$rHei, mu = topfivehei)
heitest 

shot.lm = lm(shot.data$distance ~ shot.data$rVel + shot.data$rAng + shot.data$rHei)
summary(shot.lm)
plot(shot.lm, which = 1)

par(mfrow=c(3,1))
shot.lev = hatvalues(shot.lm)
shot.resid = rstandard(shot.lm)
shot.cook = cooks.distance(shot.lm)
plot(shot.cook, type = 'o')
plot(shot.resid, type = 'h')
abline(h = 0, lwd = 2, col = "red")
plot(shot.lev, type = 'p')
par(mfrow=c(1,1))

shot.updated = shot.data[-c(5,22,24,31),]

shot.lm.2 = lm(shot.updated$distance ~ shot.updated$rVel + shot.updated$rAng + shot.updated$rHei)
summary(shot.lm.2)
plot(shot.lm.2, which = 1)

shot.forward = ols_step_forward_p(shot.lm.2)
shot.forward
plot(shot.forward)















