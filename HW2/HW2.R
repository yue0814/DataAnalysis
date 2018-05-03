# Homework 2
setwd("D:/Google Drive/Fall2017/PHP2550/HW2")
baseseg <- read.csv("baseseg.csv")
potential_predictors <- c("bascre", "sbase", "dbase", "baseu", "AGE", "SEX", "black")
#eliminate NAs in gfr
baseseg <- baseseg[-which(is.na(baseseg$gfr)),]

# extract the significant predictors which have p-values < 0.05
potentials <- c()
for (i in potential_predictors){
  if (summary(lm(paste0("baseseg$gfr~baseseg$",i)))[[4]][8] < 0.05){
    potentials <- c(potentials, i)
  }
}
potentials
par(mfrow=c(2,3))
plot(baseseg$bascre, baseseg$gfr)
plot(baseseg$sbase, baseseg$gfr)
plot(baseseg$dbase, baseseg$gfr)
plot(baseseg$baseu, baseseg$gfr)
plot(baseseg$AGE, baseseg$gfr)
# fit all the potentials
model1 <- lm(gfr~bascre+sbase+dbase+baseu+AGE+I(bascre^2)+I(bascre^3)+I(bascre^4),data = baseseg)
summary(model1, cor=T) # sbase and dbase has negative pairwise correlation 
library(leaps)
leaps <- regsubsets(gfr~bascre+sbase+dbase+baseu+AGE+I(bascre^2)+I(bascre^3)+I(bascre^4),data = baseseg,nbest = 4)
plot(leaps,scale = "adjr2")
# drop sbase
leaps2 <- regsubsets(gfr~bascre*sbase*dbase*baseu*AGE*I(bascre^2)+I(bascre^3),data = baseseg,nbest = 1)
plot(leaps2,scale = "adjr2")
leaps2$xnames[c(1,2,3,4,6,7,9,12)]

leaps3 <- regsubsets(gfr~bascre*dbase*baseu*I(bascre^2)+I(bascre^3),data = baseseg,nbest = 1)
plot(leaps3,scale = "adjr2")
# drop AGE
model22 <- lm(gfr~bascre*dbase*baseu-1,data = baseseg)
summary(model22)
plot(fitted(model22), residuals(model22),xlab = "Fitted", ylab = "Residuals")
abline(h=0)


model2 <- lm(gfr~-1+(bascre+dbase+baseu)^3+I(bascre^2)+I(bascre^3),data = baseseg)
summary(model2) # The coefficients themselves do not change residual SE does not change
# constant variance
plot(fitted(model2), residuals(model2),xlab = "Fitted", ylab = "Residuals")
abline(h=0) # Nonlinear which indicates some change in the model is necessary


full_model <- lm(gfr~bascre*dbase*baseu*I(bascre^2)*I(bascre^3)*I(bascre^4),data = baseseg)
reduced_model <- step(full_model,direction = "backward")

m1 <- lm(gfr~bascre,data = baseseg) # nonlinearity
plot(fitted(m1), residuals(m1),xlab = "Fitted", ylab = "Residuals")
abline(h=0)
m2 <- lm(gfr~dbase,data = baseseg)
plot(fitted(m2), residuals(m2),xlab = "Fitted", ylab = "Residuals")
abline(h=0)
m3 <- lm(gfr~baseu,data = baseseg)
plot(fitted(m3), residuals(m3),xlab = "Fitted", ylab = "Residuals")
abline(h=0)
# Look like Nike function, add reciprocal term (respect the hierarchy)

m22 <- lm(gfr~bascre+I(bascre^(2)), data = baseseg)
plot(fitted(m22), residuals(m22),xlab = "Fitted", ylab = "Residuals")
abline(h=0)
summary(m22)

m23 <- lm(gfr~bascre+I(bascre^2)+I(bascre^3), data = baseseg)
summary(m23)
plot(fitted(m23), residuals(m23),xlab = "Fitted", ylab = "Residuals")
abline(h=0)
AIC(m22)
AIC(m23)
AIC(m24)
m24 <- lm(gfr~bascre+I(bascre^2)+I(bascre^3)+I(bascre^4), data = baseseg)
summary(m24)
plot(fitted(m24), residuals(m24),xlab = "Fitted", ylab = "Residuals")
abline(h=0)
m25 <- lm(gfr~bascre+I(bascre^2)+I(bascre^3)+I(bascre^4)+I(bascre^5), data = baseseg)
summary(m25)
#add polynomial term of order 4
model3 <- lm(gfr~-1+(dbase+baseu+bascre)^3+I(bascre^-1)+I(bascre^2)+I(bascre^3),data = baseseg)
summary(model3)
plot(fitted(model3), residuals(model3),xlab = "Fitted", ylab = "Residuals")
abline(h=0)

model4 <- lm(gfr~-1+(dbase+baseu+bascre)^3+I(bascre^-1)+I(bascre^-2)+I(bascre^2)+I(bascre^3),data = baseseg)
summary(model4)
plot(fitted(model4), residuals(model4),xlab = "Fitted", ylab = "Residuals")
abline(h=0)

model5 <- lm(gfr~-1+(dbase+baseu+bascre)+I(bascre^-1)+I(bascre^-2)+I(bascre^2)+I(bascre^3),data = baseseg)
summary(model5)
plot(fitted(model5), residuals(model5),xlab = "Fitted", ylab = "Residuals")
abline(h=0)

model6 <- lm(gfr~-1+(bascre+baseu)+I(bascre^-1)+I(bascre^-2)+I(bascre^2),data = baseseg)
summary(model6)
plot(fitted(model6), residuals(model6),xlab = "Fitted", ylab = "Residuals")
abline(h=0)

plot(model6)

model8 <- lm(gfr~-1+(bascre*baseu)+I(bascre^-1)+I(bascre^-2)+I(bascre^2),data = baseseg)
summary(model8)

plot(fitted(model8), residuals(model8),xlab = "Fitted", ylab = "Residuals")
abline(h=0)
######
model7 <- lm(gfr~bascre+dbase+I(bascre^2)+I(bascre^3)+bascre:dbase+dbase:I(bascre^2)+bascre:I(bascre^3)+
               dbase:I(bascre^3)+I(bascre^2):I(bascre^3)+bascre:dbase:I(bascre^3)+bascre:I(bascre^2):I(bascre^3)+
               dbase:I(bascre^2):I(bascre^3)+bascre:dbase:I(bascre^2):I(bascre^3)+I(bascre^-1),data = baseseg)
summary(model7)

plot(fitted(model7), residuals(model7),xlab = "Fitted", ylab = "Residuals")
abline(h=0)
# Normality
qqnorm(residuals(model5), ylab = "Residuals")
qqline(residuals(model5))

qqnorm(residuals(model8), ylab = "Residuals")
qqline(residuals(model8))
shapiro.test(residuals(model8))
#outlier
library(car)
outlierTest(model8)

plot(cooks.distance(model8))
# PART B
# 1
y <- rnorm(100, mean = 10, sd=2)
x <- rnorm(100, mean = 3, sd=1)
model_B <- lm(y~x)
summary(model_B)[[4]][2,4]
p_values <- c()
for (i in c(1:1000)){
  y <- rnorm(100, mean = 10, sd=2)
  x <- rnorm(100, mean = 3, sd=1)
  model_B1 <- lm(y~x)
  p_values <- c(p_values,summary(model_B1)[[4]][2,4])
}
sum(p_values<0.05)/1000

#
p_values2 <- c()
for (i in 1:1000){
  y <- rep(0, 100)
  x <- rnorm(100,mean = 3,sd=1)
  for (j in 1:100){
    y[j] <- rnorm(1, x[j]+10,1)
  }
  model_B2 <- lm(y~x)
  p_values2 <- c(p_values2, summary(model_B2)[[4]][2,4])
  
}
sum(p_values2<0.05)/1000


#2
library(MASS)
y <- rnorm(100,10,4)
X <- mvrnorm(n=100,c(1,2,3),diag(3))
set.seed(4)
p_values3 <- c()
for (i in 1:1000){
  y <- rnorm(100,10,4)
  X <- mvrnorm(n=100,c(1,2,3),diag(3))
  model_B3 <- lm(y~X)
  p_values3 <- rbind(p_values3, summary(model_B3)$coefficients[-1,4])
}
sum(p_values3[,1] <0.05)
sum(p_values3[,2] <0.05)
sum(p_values3[,3] <0.05)

sum(apply(p_values3,1,min)<0.05)


significant <- c()
for (n in c(3,5,10,20,50,90)){
  p_values4 <- c()
  for (i in 1:1000){
    y <- rnorm(100,10,4)
    X <- mvrnorm(n=100,c(1:n),diag(n))
    model_B4 <- lm(y~X)
    p_values4 <- rbind(p_values4, summary(model_B4)$coefficients[-1,4])
  }
  significant <- c(significant, sum(apply(p_values4,1,min)<0.05))
}

plot(x=c(3,5,10,20,50,90), y=significant)



significant2 <- c()
for (n in c(1:200)){
  p_values5 <- c()
  for (i in 1:2000){
    y <- rnorm(100,10,4)
    X <- mvrnorm(n=100,c(1:n),diag(n))
    model_B5 <- lm(y~X)
    p_values5 <- rbind(p_values5, summary(model_B5)$coefficients[-1,4])
  }
  significant2 <- c(significant2, sum(apply(p_values5,1,min)<0.05))
}

plot(x=c(1:200), y=significant2)
