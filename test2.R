#test 2
library(datasets)
library(ggplot2)
library(RColorBrewer)
library(combinat)
library(knitr)
library(kableExtra)
library(corrplot)
library(ggpubr)
source("http://www.sthda.com/upload/rquery_cormat.r")

mt <- mtcars
mt$am <- as.factor(ifelse(mt$am==0,"automatic","manual"))
mt$vs <- as.factor(ifelse(mt$vs==0,"v-shaped","straight"))
mt$cyl <- as.factor(mt$cyl)
mt$gear <- as.factor(mt$gear)
mt$carb <- as.factor(mt$carb)

ggplot(data=mt, aes(x=am,y=mpg,fill=am)) +
  geom_boxplot(outlier.colour="black", 
               outlier.size=2,position=position_dodge(1)) + 
  labs(title = "Motor Trend Car Road Tests",
       subtitle = "",
       y = "MPG", x = "Transmission") +
  scale_fill_brewer(name = "Cylinder",palette="Oranges")  +
  theme(plot.title = element_text(hjust = 0.5)) 

lm.am <- glm(mpg~am,data=mt)

mydata <- mt[, c(1,3,4,5,6,7)]
head(mydata)

rquery.cormat(mydata)

lm.wt <-glm(mpg~wt,data=mt)
resid.wt <- resid(lm.wt)  
mt$resid.wt <- resid.wt

ggplot(data=mt, aes(x=am,y=resid.wt,fill=am)) +
  geom_boxplot(outlier.colour="black", 
               outlier.size=2,position=position_dodge(1)) + 
  labs(title = "Motor Trend Car Road Tests",
       subtitle = "",
       y = "MPG", x = "Transmission") +
  scale_fill_brewer(name = "Cylinder",palette="Oranges")  +
  theme(plot.title = element_text(hjust = 0.5)) 


lm.disp <-glm(mpg~disp,data=mt)
resid.disp <- resid(lm.disp)  
mt$resid.disp <- resid.disp

lm.qsec <-glm(mpg~qsec,data=mt)
resid.qsec <- resid(lm.qsec)  
mt$resid.qsec <- resid.qsec

lm.hp <-glm(mpg~hp,data=mt)
resid.hp <- resid(lm.hp)  
mt$resid.hp <- resid.hp

lm.drat <-glm(mpg~drat,data=mt)
resid.drat <- resid(lm.drat)  
mt$resid.drat <- resid.drat

lm.cyl <- glm(mpg~cyl,data=mt)
resid.cyl <- resid(lm.cyl)
mt$resid.cyl <- resid.cyl

mt$cyl8 <- as.factor(ifelse(mt$cyl==8,"yes","no"))

lm.wt.cyl <- glm(mpg~cyl8*wt,data=mt)
resid.wt.cyl <- resid(lm.wt.cyl)
mt$resid.wt.cyl <- resid.wt.cyl

mt$resid.am.wt <- resid(lm(wt~am,data=mt))
#with(mt,plot(resid.am.wt,resid.wt))
with(mt,points(resid.am.wt,resid.wt, pch = 21, col = "black", bg = "lightseagreen", cex = 2))
abline(h=0, lwd = 3)

with(mt,cor(resid.am.wt,resid.wt))
summary(lm(resid.wt~resid.am.wt,data=mt))

mt$resid.am.cyl <- resid(glm(cyl~am,data=mt,family="binomial"))
with(mt,plot(resid.am.cyl,resid.cyl))
summary(lm(resid.cyl~resid.am.cyl,data=mt))



plot1 <- ggplot(data=mt, aes(x=am,y=wt,fill=am)) +
  geom_boxplot(outlier.colour="black", 
               outlier.size=2,position=position_dodge(1)) + 
  labs(title = "Motor Trend Car Road Tests",
       subtitle = "",
       y = "Weight", x = "Transmission") +
  scale_fill_brewer(name = "Cylinder",palette="OrRd")  +
  theme(plot.title = element_text(hjust = 0.5)) 


plot2 <- ggplot(data=mt, aes(x=am,y=disp,fill=am)) +
  geom_boxplot(outlier.colour="black", 
               outlier.size=2,position=position_dodge(1)) + 
  labs(title = "Motor Trend Car Road Tests",
       subtitle = "",
       y = "Displacement", x = "Transmission") +
  scale_fill_brewer(name = "Transmission",palette="Oranges")  +
  theme(plot.title = element_text(hjust = 0.5)) 

plot3 <- ggplot(data=mt, aes(x=am,y=drat,fill=am)) +
  geom_boxplot(outlier.colour="black", 
               outlier.size=2,position=position_dodge(1)) + 
  labs(title = "Motor Trend Car Road Tests",
       subtitle = "",
       y = "Rear Axle Ratio", x = "Transmission") +
  scale_fill_brewer(name = "Transmission",palette="Oranges")  +
  theme(plot.title = element_text(hjust = 0.5)) 

plot4 <- ggplot(data=mt, aes(x=am,y=hp,fill=am)) +
  geom_boxplot(outlier.colour="black", 
               outlier.size=2,position=position_dodge(1)) + 
  labs(title = "Motor Trend Car Road Tests",
       subtitle = "",
       y = "Gross Horesepower", x = "Transmission") +
  scale_fill_brewer(name = "Transmission",palette="Oranges")  +
  theme(plot.title = element_text(hjust = 0.5)) 

figure <- ggarrange(plot1, plot2, plot3, plot4,
                    labels = c("Weight", "Displacement", "Rear Axle Ratio","Gross HP"),
                    ncol = 2, nrow = 2)
figure


mydata <- mt[, c(1,3,4,5,6,7,12,13)]
#head(mydata)

rquery.cormat(mydata)

par(mfrow=c(2,2))

n <- nrow(mpg)
with(mt,plot(wt, mpg, xlab="Weight",ylab="Miles per Gallon", main="Red = Automatic, Blue = Manual", type = "n", frame = FALSE))
with(mt,abline(lm(mpg ~ wt), lwd = 2))
with(mt,abline(h = mean(mpg[1 : (n/2)]), lwd = 3))
with(mt,abline(h = mean(mpg[(n/2 + 1) : n]), lwd = 3))
fit <- lm(mpg ~ wt + am,data=mt)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
with(mt[mt$am=="manual",],points(wt, mpg, pch = 21, col = "black", bg = "lightblue", cex = 2))
with(mt[mt$am=="automatic",],points(wt, mpg, pch = 21, col = "black", bg = "salmon", cex = 2))
#with(mt,points(wt[(n/2 + 1) : n], mpg[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2))

with(mt,plot(disp, mpg, xlab="Displacement",ylab="Miles per Gallon", main="Red = Automatic, Blue = Manual", type = "n", frame = FALSE))
with(mt,abline(lm(mpg ~ disp), lwd = 2))
with(mt,abline(h = mean(mpg[1 : (n/2)]), lwd = 3))
with(mt,abline(h = mean(mpg[(n/2 + 1) : n]), lwd = 3))
fit <- lm(mpg ~ disp + am,data=mt)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
with(mt[mt$am=="manual",],points(disp, mpg, pch = 21, col = "black", bg = "lightblue", cex = 2))
with(mt[mt$am=="automatic",],points(disp, mpg, pch = 21, col = "black", bg = "salmon", cex = 2))

with(mt,plot(hp, mpg, xlab="Gross Horesepower",ylab="Miles per Gallon", main="Red = Automatic, Blue = Manual", type = "n", frame = FALSE))
with(mt,abline(lm(mpg ~ hp), lwd = 2))
with(mt,abline(h = mean(mpg[1 : (n/2)]), lwd = 3))
with(mt,abline(h = mean(mpg[(n/2 + 1) : n]), lwd = 3))
fit <- lm(mpg ~ hp + am,data=mt)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
with(mt[mt$am=="manual",],points(hp, mpg, pch = 21, col = "black", bg = "lightblue", cex = 2))
with(mt[mt$am=="automatic",],points(hp, mpg, pch = 21, col = "black", bg = "salmon", cex = 2))

with(mt,plot(drat, mpg, xlab="Rear Axle Ratio",ylab="Miles per Gallon", main="Red = Automatic, Blue = Manual", type = "n", frame = FALSE))
with(mt,abline(lm(mpg ~ drat), lwd = 2))
with(mt,abline(h = mean(mpg[1 : (n/2)]), lwd = 3))
with(mt,abline(h = mean(mpg[(n/2 + 1) : n]), lwd = 3))
fit <- lm(mpg ~ drat + am,data=mt)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
with(mt[mt$am=="manual",],points(drat, mpg, pch = 21, col = "black", bg = "lightblue", cex = 2))
with(mt[mt$am=="automatic",],points(drat, mpg, pch = 21, col = "black", bg = "salmon", cex = 2))

with(mt,plot(qsec, mpg, xlab="1/4 Mile Time",ylab="Miles per Gallon", main="Red = Automatic, Blue = Manual", type = "n", frame = FALSE))
with(mt,abline(lm(mpg ~ qsec), lwd = 2))
with(mt,abline(h = mean(mpg[1 : (n/2)]), lwd = 3))
with(mt,abline(h = mean(mpg[(n/2 + 1) : n]), lwd = 3))
fit <- lm(mpg ~ qsec + am,data=mt)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
with(mt[mt$am=="manual",],points(qsec, mpg, pch = 21, col = "black", bg = "lightblue", cex = 2))
with(mt[mt$am=="automatic",],points(qsec, mpg, pch = 21, col = "black", bg = "salmon", cex = 2))

