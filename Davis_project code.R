# Lance Davis
# LDA Summer 2021
# data project code

# I imported the excel file, but made a CSV copy in case it is needed. 
library("readxl")
library(MASS)
library(nlme)
# update filepath to load
df<- read_excel("C:\\Users\\Lance\\Desktop\\ALZ data.xlsx", col_names = TRUE, trim_ws = TRUE)
df$male <- ifelse(df$`M/F` == "M", 1, 0)
df$demented <- ifelse(df$CDR > 0, 1, 0)
df$oldage <- ifelse(df$Age >= 77, 1, 0)
#df$ed <- df$EDUC-6
summary(df)
qqnorm(df$nWBV)
qqline(df$nWBV)

#subset data frame with 3 or more waves only
df2 <- subset(df, df$count > 2)

#subset data frame with "converted" diagnosis
df3 <- subset(df, df$Group == "Converted")

#interaction plot whole brain volume by wave
interaction.plot(df$Wave, df$ID, df$nWBV, xlab = "wave", ylab = "nWBV", legend = "F", 
                 pch=18, cex = 1, type = c("b"), lwd = 1, lty = 1)

#Normalize Whole Brain volume - Male/Female
par(mfrow = c(1,2))
interaction.plot(df$Wave[df$male==1], df$ID[df$male==1], df$nWBV[df$male==1], xlab = "wave", ylab = "nWBV", legend = "F", 
                 pch=18, cex = 1, type = c("b"), lwd = 1, lty = 1)
#grid(NULL, NULL, col = "lightgray", lty = "dotted")
title("Male")
interaction.plot(df$Wave[df$male==0], df$ID[df$male==0], df$nWBV[df$male==0], xlab = "wave", ylab = "nWBV", legend = "F",
                 pch=18, cex = 1, type = c("b"), lwd = 1, lty = 1)
title("Female")

#Normalize Whole Brain volume - years of education
par(mfrow = c(1,2))
interaction.plot(df$Wave[df$EDUC < 15], df$ID[df$EDUC < 15], df$nWBV[df$EDUC < 15], xlab = "wave", ylab = "nWBV", legend = "F")
title("Educ <15 years")
interaction.plot(df$Wave[df$EDUC >= 15], df$ID[df$EDUC >= 15], df$nWBV[df$EDUC >= 15], xlab = "wave", ylab = "nWBV", legend = "F")
title("Educ >=15 years")

#Normalize Whole Brain volume - by age (below or >= 77)
par(mfrow = c(1,2))
interaction.plot(df$Wave[df$Age < 77], df$ID[df$Age < 77], df$nWBV[df$Age < 77], xlab = "wave", ylab = "nWBV", legend = "F")
title("Age <77")
interaction.plot(df$Wave[df$Age >= 77], df$ID[df$Age >= 77], df$nWBV[df$Age >= 77], xlab = "wave", ylab = "nWBV", legend = "F")
title("Age >= 77")

#Normalize Whole Brain volume - Demented or Not Demented
par(mfrow = c(1,2))
interaction.plot(df$Wave[df$demented == 1], df$ID[df$demented == 1], df$nWBV[df$demented == 1], xlab = "wave", ylab = "nWBV", legend = "F")
title("Demented")
interaction.plot(df$Wave[df$demented == 0], df$ID[df$demented == 0], df$nWBV[df$demented == 0], xlab = "wave", ylab = "nWBV", legend = "F")
title("Not Demented")

mod0 <- lme(nWBV ~ 1, data = df, random = ~ 1|ID, method = "ML") 
summary(mod0)

mod1 <- lme(nWBV ~ Wave, data = df, random = ~ Wave | ID, method = "ML")
summary(mod1)

mod2 <- lme(nWBV ~ male*Wave, data = df, random = ~ Wave | ID, method = "ML")
summary(mod2)

mod3 <- lme(nWBV ~ male*Wave + demented*Wave + oldage*Wave, data = df, random = ~ Wave | ID, method = "ML")
summary(mod3)


AIC.result <- stepAIC(mod0, scope = list(lower=mod0, upper=mod3), direction = "forward")
AIC.result$anova

#final model
mod4 <- lme(nWBV ~ Wave + demented + oldage + Wave:demented + male, data = df, random = ~ Wave | ID, method = "ML")
summary(mod4)
plot(mod4)

fixef.4 <- fixef(mod4)
fixef.4
waveseq <- seq(1,4)
female <- fixef.4[[1]] + fixef.4[[2]]*waveseq 
male <- fixef.4[[1]] + fixef.4[[2]]*waveseq + fixef.4[[5]]*waveseq
oldfemale <- fixef.4[[1]] + fixef.4[[2]]*waveseq + fixef.4[[4]]*waveseq
oldmale <- fixef.4[[1]] + fixef.4[[2]]*waveseq + fixef.4[[4]]*waveseq + fixef.4[[5]]*waveseq
demoldfemal <- fixef.4[[1]] + fixef.4[[2]]*waveseq + fixef.4[[3]]*waveseq + fixef.4[[4]]*waveseq + fixef.4[[6]]*waveseq
demoldmale <- fixef.4[[1]] + fixef.4[[2]]*waveseq + fixef.4[[3]]*waveseq +fixef.4[[4]]*waveseq + fixef.4[[5]]*waveseq + fixef.4[[6]]*waveseq

par(mfrow = c(1,1))
plot(waveseq, female, type = "l", col = "darkorchid3", ylab = "nWBVhat", xlab = "wave", lwd=3, xlim = c(0.5,5), ylim = c(0.55,0.8))
lines(waveseq, male, lty = 1, lwd =3, col = "red")
lines(waveseq, oldfemale, lty = 2, lwd = 3, col = "darkorchid3")
lines(waveseq, oldmale, lty = 2, lwd = 3, col = "red")
lines(waveseq, demoldfemal, lty = 3, lwd=3, col = "darkorchid3")
lines(waveseq, demoldmale, lty = 3, lwd = 3, col = "red")
title("Normal whole brain volume over time")
legend(0.5,0.65, c("female", "male", "old female", "old male", "old ALZ fem", "old ALZ male"), lty = c(1,1,2,2,3,3), 
       col = c("darkorchid3","red","darkorchid3","red","darkorchid3","red"))

#polynomial 
df$wave2 <- df$Wave^2
df$wave3 <- df$Wave^3
df$nWBV2 <- df$nWBV^2

qqnorm(df$nWBV2)
qqline(df$nWBV2)

poly0 <- lme(nWBV2 ~ 1, data = df, random = ~ 1 | ID, method = "ML")
summary(poly0)

poly1 <- lme(nWBV ~ Wave + wave2 + wave3, data = df, random = ~ Wave + wave2 + wave3 | ID, method = "ML", control = lmeControl(maxIter = 100))
summary(poly1)

poly2 <- lme(nWBV2 ~ Wave, data = df, random = ~ Wave | ID, method = "ML")
summary(poly2)

poly3 <-lme(nWBV ~ Wave, data = df, random = ~ Wave | ID, method = "ML")
summary(poly3)

polyAICresult <- stepAIC(poly0, scope = list(lower=poly0, upper=poly1), direction = "forward")
polyAICresult$anova

poly4 <- lme(nWBV2 ~ male*Wave + demented*Wave + oldage*Wave, data = df, random = ~ Wave | ID, method = "ML")
summary(poly4)

polyAIC2 <- stepAIC(poly0, scope = list(lower=poly0, upper=poly4), direction = "forward")
polyAIC2$anova
