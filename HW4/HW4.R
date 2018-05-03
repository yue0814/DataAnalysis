# Homework 4
# Read the dataset
setwd("D:/Dropbox (Brown)/Fall2017/PHP2550/HW4")
dat <- read.table("wells.txt", sep = " ", header = T)
# switch is a binary indicator for whether the household switched wells
dat$switch <- as.factor(dat$switch)
# arsenic is the level of arsenic in the well in hundreds of micrograms per liter

# dist is the distance to the nearest safe well in meters

# assoc is whether household members are active in community organizations
dat$assoc <- as.factor(dat$assoc)
# educ is the number of years of education of the head of household.
str(dat)
# Separate into train and test set
train_dat <- dat[1:2520,]
test_dat <- dat[2521:3020, ]

### a.
# Test each predictor
summary(glm(switch ~ arsenic, family = binomial(link = logit), data = train_dat))
summary(glm(switch ~ assoc, family = binomial(link = logit), data = train_dat))
summary(glm(switch ~ educ, family = binomial(link = logit), data = train_dat))
summary(glm(switch ~ dist, family = binomial(link = logit), data = train_dat))

m0 <- glm(switch ~ arsenic + assoc + educ + dist, family = binomial(link = logit), data = train_dat)
summary(m0)

m1 <- glm(switch ~ -1 + arsenic + educ + dist + assoc, family = binomial(link = logit), data = train_dat)
summary(m1)

m2 <- glm(switch ~ -1 + arsenic + educ + dist, family = binomial(link = logit), data = train_dat)
summary(m2)
anova(m1, m2,test = "Chisq")

m3 <- glm(switch ~ -1 + arsenic * educ + dist, family = binomial(link = logit), data = train_dat)
summary(m3)

m4 <- glm(switch ~ -1 + arsenic +  educ * dist, family = binomial(link = logit), data = train_dat)
summary(m4)

m5 <- glm(switch ~ -1 + arsenic * dist + educ, family = binomial(link = logit), data = train_dat)
summary(m5) # not significant

m6 <- glm(switch ~ -1 + arsenic +  educ * dist +educ*arsenic, family = binomial(link = logit), data = train_dat)
summary(m6)
anova(m2, m6,test = "Chisq")

model <- glm(switch ~ -1 + arsenic +  educ * dist, family = binomial(link = logit), data = train_dat)
summary(model)
anova(m2,model,test = "Chisq")
# b
p <- function(z){
  return(exp(z)/(1+exp(z)))
}

z <- function(arsenic, educ, dist){
  return(coef(model)[1]*arsenic+coef(model)[2]*educ+coef(model)[3]*dist+coef(model)[4]*educ*dist)
}
# EDUC
range(train_dat$dist)
x <- seq(0, 340, length.out = 2520)
plot(x, as.numeric(as.character(train_dat$switch)),xlim=c(0,350), ylim = c(-0.1,1.1), xlab = "Distance",
     ylab="probability of switch", main = c("Stratifying by the number of years of education of the head of household."))
range(train_dat$educ)
educs <- c(0,4,8,12,16)
cols <- c("red", "blue", "green", "orange", "purple")
for (i in seq_along(educs)){
lines(x, p(z(mean(train_dat$arsenic),educ = educs[i], x)), col=cols[i])
}
abline(h=0.5)
# ARSENIC
plot(x, as.numeric(as.character(train_dat$switch)), ylim = c(-0.1,1.1), xlab = "Distance", ylab="probability of switch",
     main = c("Stratifying by the level of arsenic in the well in hundreds of micrograms per liter"))
range(train_dat$arsenic)
arsenics <- c(0.5, 2.5, 4.5, 6.5, 8.5)
for (i in seq_along(educs)){
  lines(x, p(z(arsenics[i],educ = mean(train_dat$arsenic), x)), col=cols[i])
}
abline(h=0.5)
# DIST
x1 <- seq(0, 9, length.out = 2520)
plot(x1, as.numeric(as.character(train_dat$switch)), ylim = c(-0.1,1.1), xlab = "Arsenic", ylab="probability of switch",
     main = c("Stratifying by the distance to the nearest safe well in meters"))

for (i in seq_along(educs)){
  lines(x, p(z(x1,educ = educs[i], mean(train_dat$dist))), col=cols[i])
}
abline(h=0.5)
#c
prob <- predict.glm(model, type = "response", newdata = test_dat)
pred <- ifelse(prob>0.5,1,0)
table(pred, test_dat$switch)
# Sensitivity
210/(24+210)
sensitivity(confusion.matrix(test_dat$switch, prob, threshold = 0.5))
# Specificity
58/(208+58)
specificity(confusion.matrix(test_dat$switch, prob, threshold = 0.5))
# 1- Accuracy
1-(210+58)/(58+24+208+210)

# d
library(pROC)
roc(test_dat$switch, prob,plot = T)

# e
library(SDMTools)
p_ts <- sum(dat$switch==1)/dim(dat)[1]
risk <- function(p){
  cm <- confusion.matrix(test_dat$switch, prob, threshold = p)
  sen <- sensitivity(cm)
  spe <- specificity(cm)
  return(p_ts*(1-sen)+(1-p_ts)*(1-spe))
}

x2 <- seq(0,1, length.out = 1000)
y <- mapply(risk, x2)
plot(x2, y, xlab = "Threshold", ylab = "Risk")

# Threshold with least risk
min(y)
threshold <-x2[which.min(y)]
threshold

cm0 <- confusion.matrix(test_dat$switch, pred, threshold = threshold)
sensitivity(cm0)
specificity(cm0)

# f
# LDA
library(MASS)  
library(ROCR)
mlda0 <- lda(switch ~ -1 + arsenic +  educ * dist, train_dat)
mlda0_fit <- predict(mlda0, test_dat)
prob_lda <- predict(mlda0, test_dat)$posterior[,2]
pred_lda <- prediction(prob_lda, test_dat$switch)
perf_lda <- performance(pred_lda, "tpr", "fpr")
plot(perf_lda,main="RUC Plot for LDA", colorize=TRUE)
table(mlda0_fit$class, test_dat$switch)
performance(pred_lda, "auc")@y.values

p_ts <- sum(dat$switch==1)/dim(dat)[1]
risk <- function(p){
  cm <- confusion.matrix(test_dat$switch, prob_lda, threshold = p)
  sen <- sensitivity(cm)
  spe <- specificity(cm)
  return(p_ts*(1-sen)+(1-p_ts)*(1-spe))
}

x3 <- seq(0,1, length.out = 1000)
y <- mapply(risk, x3)
plot(x3, y, xlab = "Threshold", ylab = "Risk")

# Threshold with least risk
min(y)
threshold <-x3[which.min(y)]
threshold

cm1 <- confusion.matrix(test_dat$switch, prob_lda, threshold = threshold)
sensitivity(cm1)
specificity(cm1)

#QDA
mqda0 <- qda(switch ~ -1 + arsenic +  educ * dist, train_dat)
mqda0_fit <- predict(mqda0, test_dat)
prob_qda <- predict(mqda0, test_dat)$posterior[,2]
pred_qda <- prediction(prob_qda, test_dat$switch)
perf_qda <- performance(pred_qda, "tpr", "fpr")
plot(perf_lda,main="RUC Plot for QDA", colorize=TRUE)
table(mqda0_fit$class, test_dat$switch)          
performance(pred_qda, "auc")@y.values

p_ts <- sum(dat$switch==1)/dim(dat)[1]
risk <- function(p){
  cm <- confusion.matrix(test_dat$switch, prob_qda, threshold = p)
  sen <- sensitivity(cm)
  spe <- specificity(cm)
  return(p_ts*(1-sen)+(1-p_ts)*(1-spe))
}

x4 <- seq(0,1, length.out = 1000)
y <- mapply(risk, x4)
plot(x4, y, xlab = "Threshold", ylab = "Risk")

# Threshold with least risk
min(y)
threshold <-x4[which.min(y)]
threshold

cm2 <- confusion.matrix(test_dat$switch, prob_qda, threshold = threshold)
sensitivity(cm2)
specificity(cm2)


# KNN
mknn0 <- knn(train = train_dat, test = test_dat,cl=train_dat$switch, k=1)
table(mknn0, test_dat$switch)
multiclass.roc(response=test_dat$switch, predictor = as.ordered(mknn0))

mknn1 <- knn(train = train_dat, test = test_dat,cl=train_dat$switch, k=5)
table(mknn1, test_dat$switch)
multiclass.roc(response=test_dat$switch, predictor = as.ordered(mknn1))
