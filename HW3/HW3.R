# Use Pandas to finish the preprocessing part
dat <- read.csv("dat.csv")
dat <- dat[,c(2:12)]
dat$ID <- as.factor(dat$ID)

# Deal with the Occupation Column
idx <- c(grep("tired", dat$Occupation), grep("TIRED", dat$Occupation))
dat$retired <- rep(0,length(dat$Occupation))
idx <- idx[!(idx %in% grep("never retired", dat$Occupation))]
dat$retired[idx] <- 1
dat$retired[which(dat$Occupation=="")] <- NA
dat$retired <- as.factor(dat$retired)

dat <- dat[!is.na(dat$temperature),]
# Model
library(lme4)
library(nlme)
m1 <- lmer(pain ~ 1|ID,REML=F, data=dat)

m2 <- lmer(pain ~ 1 + time + (1+time|ID), REML = F, data = dat)
# Model is nearly unidentifiable: very large eigenvalue - Rescale variables?
dat$time_scale <- scale(dat$time)
m2 <- lmer(pain ~ 1 + time_scale+ (1+time_scale|ID), REML = F, data = dat)

m3 <- lmer(pain ~ 1 + temperature+ (1+temperature|ID), REML = F, data = dat)
# Model is nearly unidentifiable: very large eigenvalue - Rescale variables?
dat$temp_scale <- scale(dat$temperature)
m3 <- lmer(pain ~ 1 + temp_scale+ (1+temp_scale|ID), REML = F, data = dat)

m4 <- lmer(pain ~ 1 + time_scale +temp_scale+ (1+time_scale+temp_scale|ID), REML = F, data = dat)

AIC(m1, m2, m3, m4)
anova(m2, m4)

# age
m5 <- lmer(pain ~ time_scale +(time_scale|ID), REML = F, data = dat, subset = !is.na(dat$age))
m6 <- lmer(pain ~ time_scale + age + (time_scale|ID), REML = F, data = dat, subset = !is.na(dat$age))
m7 <- lmer(pain ~ time_scale * age + (time_scale|ID), REML = F, data = dat, subset = !is.na(dat$age))

anova(m5, m6, m7)

# race2
dat$race2 <- as.factor(dat$race2)
m8 <- lmer(pain ~ time_scale +(time_scale|ID), REML = F, data = dat, subset = !is.na(dat$race2))
m9 <- lmer(pain ~ time_scale + race2 + (time_scale|ID), REML = F, data = dat, subset = !is.na(dat$race2))
m10 <- lmer(pain ~ time_scale * race2 + (time_scale|ID), REML = F, data = dat, subset = !is.na(dat$race2))
anova(m8, m9, m10)

# inccat
dat$inccat <- as.factor(dat$inccat)
m11 <- lmer(pain ~ time_scale +(time_scale|ID), REML = F, data = dat, subset = !is.na(dat$inccat))
m12 <- lmer(pain ~ time_scale + inccat + (time_scale|ID), REML = F, data = dat, subset = !is.na(dat$inccat))
m13 <- lmer(pain ~ time_scale * inccat + (time_scale|ID), REML = F, data = dat, subset = !is.na(dat$inccat))
anova(m11, m12, m13)

# treat
dat$treat <- as.factor(dat$treat)
m14 <- lmer(pain ~ time_scale +(time_scale|ID), REML = F, data = dat, subset = !is.na(dat$treat))
m15 <- lmer(pain ~ time_scale + treat + (time_scale|ID), REML = F, data = dat, subset = !is.na(dat$treat))
m16 <- lmer(pain ~ time_scale * treat + (time_scale|ID), REML = F, data = dat, subset = !is.na(dat$treat))
anova(m14, m15, m16)

# sex
dat$sex <- as.factor(dat$sex)
m17 <- lmer(pain ~ time_scale +(time_scale|ID), REML = F, data = dat, subset = !is.na(dat$sex))
m18 <- lmer(pain ~ time_scale + sex + (time_scale|ID), REML = F, data = dat, subset = !is.na(dat$sex))
m19 <- lmer(pain ~ time_scale * sex + (time_scale|ID), REML = F, data = dat, subset = !is.na(dat$sex))
anova(m17, m18, m19)

# nsaid  
dat$nsaid <- as.factor(dat$nsaid)
m20 <- lmer(pain ~ time_scale +(time_scale|ID), REML = F, data = dat, subset = !is.na(dat$nsaid))
m21 <- lmer(pain ~ time_scale + nsaid + (time_scale|ID), REML = F, data = dat, subset = !is.na(dat$nsaid))
m22 <- lmer(pain ~ time_scale * nsaid + (time_scale|ID), REML = F, data = dat, subset = !is.na(dat$nsaid))
anova(m20, m21, m22) # significant
# retired
m23 <- lmer(pain ~ time_scale +(time_scale|ID), REML = F, data = dat, subset = !is.na(dat$retired))
m24 <- lmer(pain ~ time_scale + retired + (time_scale|ID), REML = F, data = dat, subset = !is.na(dat$retired))
m25 <- lmer(pain ~ time_scale * retired + (time_scale|ID), REML = F, data = dat, subset = !is.na(dat$retired))
anova(m23, m24, m25)


model <- lmer(pain ~ 1 + nsaid*time_scale + (1+time_scale|ID), REML = F, data = dat, subset = !is.na(dat$nsaid))
summary(model)


#plot
x <- seq(0, 100, by=10)
x <- scale(x)
coefs_f <- fixed.effects(model)
coefs_r <- random.effects(model)
plot(x, rep(0, 11), type = "n", ylim = c(0,15), xlab = "Time", ylab = "Pain")
nsaid0 <- coefs_f[1]+x*coefs_f[3]
nsaid1 <- coefs_f[1] + coefs_f[2] + x*(coefs_f[3] + coefs_f[4])
points(x, nsaid0, type = "l", col='red', lwd=3)
points(x, nsaid1, type = "l", col='blue', lwd=3)
for (i in 1:7){
  yi0 = coefs_f[1] + x*coefs_f[3] +coefs_r$ID[i,1] + coefs_r$ID[i,2]*x
  yi1 = coefs_f[1] + coefs_f[2] + x*(coefs_f[3]+coefs_f[4]) +coefs_r$ID[i,1] + coefs_r$ID[i,2]*x
  points(x, yi0,type="l",col="pink",lwd=0.5)
  points(x, yi1,type="l",col="lightblue",lwd=0.5)
}


# Problem 2
srr <- read.table("srrs2.txt", sep = ",", header = T)
cty <- read.table("cty.txt", sep = ",", header = T)
col_s <- c("idnum","state2","stfips","typebldg","floor","basement","activity","cntyfips")
col_c <- cty_col <- c("stfips","ctfips","Uppm")
dat_s <- srr[col_s]
dat_c <- cty[col_c]
names(dat_c) <- c("stfips","cntyfips","uranium")

dat_s$uranium <- rep(0, dim(dat_s)[1])
for (i in 1:dim(dat_s)[1]){
  if (length(intersect(which(dat_c$cntyfips==dat_s$cntyfips[i]), which(dat_c$stfips==dat_s$stfips[i])))!=0){
  idx <- intersect(which(dat_c$cntyfips==dat_s$cntyfips[i]), which(dat_c$stfips==dat_s$stfips[i]))
  dat_s$uranium[i] <- dat_c$uranium[idx]
  }
}

dat_mn <- dat_s[which(dat_s$state2=="MN"),]
dat_mn$floor[which(dat_mn$floor==9)] <- NA

dat_mn$single <- rep(0, dim(dat_mn)[1])
dat_mn$single[which(dat_mn$typebldg == 1)] <- 1

dat_mn$basement[which(dat_mn$basement == 0)] <- NA
dat_mn$basement[which(dat_mn$basement == " ")] <- NA


dat_mn$activity_log <- log(dat_mn$activity)
dat_mn <- dat_mn[which(dat_mn$activity_log != -Inf), ]
dat_mn$uranium_log <- log(dat_mn$uranium)
dat_mn <- dat_mn[!is.na(dat_mn$floor) & !is.na(dat_mn$basement),]
dat_mn$single <- as.factor(dat_mn$single)
dat_mn$basement <- as.factor(dat_mn$basement)
dat_mn$floor <- as.factor(dat_mn$floor)

mod1 <- lmer(activity_log ~ 1|cntyfips, REML = F, data = dat_mn)
mod2 <- lmer(activity_log ~ 1 + floor + (1 + floor|cntyfips), REML = F, data = dat_mn)
mod3 <- lmer(activity_log ~ 1 + single + (1 + single|cntyfips), REML = F, data = dat_mn)
mod4 <- lmer(activity_log ~ 1 + basement + (1 + basement|cntyfips), REML = F, data = dat_mn)
anova(mod1, mod2) # significant
anova(mod1, mod3) # significant
anova(mod1, mod4) # significant
AIC(mod2, mod3, mod4)
mod5 <- lmer(activity_log ~ 1 + floor + basement + (1+floor+basement|cntyfips), REML = F, data = dat_mn)
mod6 <- lmer(activity_log ~ 1 + floor + single + (1+floor+single|cntyfips), REML = F, data = dat_mn)
mod7 <- lmer(activity_log ~ 1 + single + basement + (1+single+basement|cntyfips), REML = F, data = dat_mn)
mod8 <- lmer(activity_log ~ 1 + floor + single + basement + (1 + floor+single+basement|cntyfips), REML = F, data = dat_mn)

anova(mod5, mod8)
anova(mod6, mod8)
anova(mod7, mod8)

anova(mod2,mod6)
anova(mod3, mod6)

modf0 <- lmer(activity_log ~ 1 + floor + single + (1+floor+single|cntyfips), REML = F, data = dat_mn,
              subset = dat_mn$uranium_log != -Inf)
modf1 <- lmer(activity_log ~ 1 + floor + single + uranium_log + (1+floor+single|cntyfips), REML = F, data = dat_mn,
              subset = dat_mn$uranium_log != -Inf)
modf2 <- lmer(activity_log ~ 1 + (floor + single) * uranium_log + (1+floor+single|cntyfips), REML = F, data = dat_mn,
              subset = dat_mn$uranium_log != -Inf)
anova(modf0, modf1, modf2)

summary(modf1)


#plot
cnty <- unique(dat_mn$cntyfips)[1:10]
ur_cnty <- dat_mn$uranium_log[cnty]
x <- rep(0,10)
x1 <- c(0,1)
coefs_f <- fixed.effects(modf1)
coefs_r <- random.effects(modf1)
plot(x, rep(0, 10), type = "n", ylim = c(0,3), xlim=c(-0.01, 1.01), xlab = "Single Family House", ylab = "Activity (Radon Level)")
# floor == 0 
uranium0 <- coefs_f[1]+coefs_f[3]*x1
uranium1 <- coefs_f[1] +(coefs_f[3]+ coefs_f[4])*x1
points(x1, uranium0, type="l", col='red', lwd=3)
points(x1, uranium1, type="l", col='blue', lwd=3)
for (i in 1:10){
  yi0 = coefs_r$cntyfips[i,1] + coefs_r$cntyfips[i,3]*x1 + coefs_f[1]+coefs_f[3]*x1
  yi1 = coefs_r$cntyfips[i,1] + (coefs_r$cntyfips[i,3])*x1 + coefs_f[1] +(coefs_f[3]+ coefs_f[4])*x1
  points(x1, yi0,type="l",col="pink",lwd=0.5)
  points(x1, yi1,type="l",col="lightblue",lwd=0.5)
}
# floor == 1
plot(x, rep(0, 10), type = "n", ylim = c(0,3), xlim=c(-0.01, 1.01), xlab = "Single Family House", ylab = "Activity (Radon Level)")
uranium0 <- coefs_f[1]+coefs_f[2]+coefs_f[3]*x1
uranium1 <- coefs_f[1] + coefs_f[2]+(coefs_f[3]+ coefs_f[4])*x1
points(x1, uranium0, type="l", col='red', lwd=3)
points(x1, uranium1, type="l", col='blue', lwd=3)
for (i in 1:10){
  yi0 = coefs_r$cntyfips[i,1] +coefs_r$cntyfips[i,2]+ coefs_r$cntyfips[i,3]*x1 + coefs_f[1]+coefs_f[2]+coefs_f[3]*x1
  yi1 = coefs_r$cntyfips[i,1] +coefs_r$cntyfips[i,2]+ (coefs_r$cntyfips[i,3])*x1 + coefs_f[1] +coefs_f[2]+(coefs_f[3]+ coefs_f[4])*x1
  points(x1, yi0,type="l",col="pink",lwd=0.5)
  points(x1, yi1,type="l",col="lightblue",lwd=0.5)
}
# single == 0
plot(x, rep(0, 10), type = "n", ylim = c(0,3), xlim=c(-0.01, 1.01), xlab = "Floor", ylab = "Activity (Radon Level)")

uranium0 <- coefs_f[1]+coefs_f[2]*x1
uranium1 <- coefs_f[1] +(coefs_f[2]+ coefs_f[4])*x1
points(x1, uranium0, type="l", col='red', lwd=3)
points(x1, uranium1, type="l", col='blue', lwd=3)
for (i in 1:10){
  yi0 = coefs_r$cntyfips[i,1] + coefs_r$cntyfips[i,2]*x1 + coefs_f[1]+coefs_f[2]*x1
  yi1 = coefs_r$cntyfips[i,1] + (coefs_r$cntyfips[i,2])*x1 + coefs_f[1] +(coefs_f[2]+ coefs_f[4])*x1
  points(x1, yi0,type="l",col="pink",lwd=0.5)
  points(x1, yi1,type="l",col="lightblue",lwd=0.5)
}

# single == 1
plot(x, rep(0, 10), type = "n", ylim = c(0,3), xlim=c(-0.01, 1.01), xlab = "Floor", ylab = "Activity (Radon Level)")

uranium0 <- coefs_f[1]+coefs_f[3]+coefs_f[2]*x1
uranium1 <- coefs_f[1] +coefs_f[3]+(coefs_f[2]+ coefs_f[4])*x1
points(x1, uranium0, type="l", col='red', lwd=3)
points(x1, uranium1, type="l", col='blue', lwd=3)
for (i in 1:10){
  yi0 = coefs_r$cntyfips[i,1]+coefs_r$cntyfips[i,3] + coefs_r$cntyfips[i,2]*x1 + coefs_f[1]+coefs_f[2]*x1+coefs_f[3]
  yi1 = coefs_r$cntyfips[i,1] +coefs_r$cntyfips[i,1]+ (coefs_r$cntyfips[i,2])*x1 + coefs_f[1] +(coefs_f[2]+ coefs_f[4])*x1+coefs_f[3]
  points(x1, yi0,type="l",col="pink",lwd=0.5)
  points(x1, yi1,type="l",col="lightblue",lwd=0.5)
}
