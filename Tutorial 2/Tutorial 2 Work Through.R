
library(survival)

cougars <- read.csv("JAE_00481_Cougar Survival Data.csv")
head(cougars)

mean(cougars$TotalMonth)
sd(cougars$TotalMonth)

survV <- Surv(time=cougars$TotalMonth, event=rep(1, nrow(cougars)))
fixV <- survfit(survV ~ 1)
plot(fixV)


esf <- data.frame(matrix(ncol=2, nrow=length(unique(cougars$TotalMonth))))
colnames(esf) <- c("y", "s")
esf$y <- sort(unique(cougars$TotalMonth))

for(i in 1:nrow(esf)){
        esf$s[i] <- sum(cougars$TotalMonth>esf$y[i])/nrow(cougars)
}

points(esf, type="s", col="red", lty=2)

print(fixV, print.rmean = TRUE)

fixlm <- lm(TotalMonth ~ Housing.Density + Sex, data = cougars)
summary(fixlm)

plot(TotalMonth~Housing.Density, data=cougars, pch=19, cex=0.5)
abline(fixlm$coef[1:2])

plot(TotalMonth~Sex, data=cougars, pch=19, cex=0.5)
abline(fixlm$coef[c(1,3)])

### Section 2.2

sum(cougars$Event == 0)/nrow(cougars)*100
cougarsD <- cougars[cougars$Event == 1,]

## Exercise 2

mean(cougarsD$TotalMonth)
mean(cougars$TotalMonth)

## original data graph:
survV <- Surv(time=cougars$TotalMonth, event=rep(1, nrow(cougars)))
fixV <- survfit(survV ~ 1)
plot(fixV)

## with new analysis in red:
survD <- Surv(time=cougarsD$TotalMonth)
fixD <- survfit(survVD ~ 1)
points(fixD$surv~ fixD$time, type= "s", col="red")

## linear regression on cougarsD
regcougarsD <- lm(TotalMonth ~ Housing.Density + Sex, data = cougarsD)
summary(regcougarsD)

## compare Housing Density between two methods
plot(TotalMonth~Housing.Density, data=cougars, pch=19, cex=0.5)
abline(fixlm$coef[1:2])
summary(fixlm)

points(TotalMonth~Housing.Density, data=cougarsD, pch=19, cex=0.5, col="red")
abline(regcougarsD$coef[1:2], col="red")
summary(regcougarsD)


## compare Sex between two methods
plot(TotalMonth~Sex, data=cougars, pch=19, cex=0.5)
abline(fixlm$coef[c(1,3)])

points(TotalMonth~Sex, data=cougarsD, pch=19, cex=0.5, col = "red")
abline(regcougarsD$coef[c(1,3)], col = "red")

summary(fixlm)
summary(regcougarsD)


## Section 3

survKM <- Surv(time=cougars$TotalMonth, event=cougars$Event)
kmV <- survfit(survKM~1)
plot(kmV)

# Exercise 3

### Plotting the Kaplan-Meier survival function (given in Tutorial)
survKM <- Surv(time=cougars$TotalMonth, event=cougars$Event)
kmV <- survfit(survKM~1)
plot(kmV)

### Plotting the empirical survival function from before (red lines):
points(esf, type="s", col="red")


## use K-M to estimate mean:
print(kmV, print.rmean =TRUE)

print(kmV, print.rmean=TRUE, rmean=132)

## Section 4

# Regression for censored data

couTobit <- survreg(survKM~Housing.Density+Sex, data = cougars, dist="gaussian")
summary(couTobit)

survInt <- Surv(time = cougars$EntryMonth, time2=cougars$ExitMonth, event= cougars$Event, type ="interval")
kmInt <- survfit(survInt~1)
print(kmInt, print.rmean=TRUE)

head(cougars)

## Section 4

# Exercise 4 - SKIP

## a. Estimate the mean survival time when accounting for the censorship entry date.

#```{r}
#survInt <- Surv(time = cougars$EntryMonth, time2=cougars$ExitMonth, event= cougars$Event, type ="interval")
#kmInt <- survfit(survInt~1)
#print(kmInt, print.rmean=TRUE)
#```

#Compared to empirical results:
#        ```{r}
#mean(cougars$TotalMonth)
#print(kmV, print.rmean =TRUE)
#```

#We can see here that the predicted mean is much higher when accounting for the interval censorship (38 months) than for the right-censored method (22 months) and for the Kaplan-Meier corrected right-censored method (31 months). This is to be expected, since many of the cougars marked in this study were tagged sometime after they entered adulthood.

# Regression for censored data

#couTobit <- survreg(survKM~Housing.Density+Sex, data = cougars, dist="gaussian")
#summary(couTobit)

#survInt <- Surv(time = cougars$EntryMonth, time2=cougars$ExitMonth, event= cougars$Event, type ="interval")
#kmInt <- survfit(survInt~1)
#print(kmInt, print.rmean=TRUE)

#head(cougars)



## Exercise 5

deers <- read.csv("Fawn_survival_70day.csv")
head(deers)
deers$total.days <- (deers$k - deers$i)
head(deers)

survDInt <- Surv(time = deers$total.days, event = deers$fate)
DInt <- survfit(survDInt~1)
plot(DInt)

print(DInt, print.rmean=TRUE)

#### OR is it . . .
survDInt2 <- Surv(time=deers$k, event=deers$fate)
DInt2 <- survfit(survDInt~1)
summary(DInt2)
points(DInt2$surv~DInt2$time, col = "red")
### Turns out they're the same

### 5c

deersreg <- survreg(survDInt~sex+timber, data=deers, dist = "gaussian")
summary(deersreg)

boxplot(k~sex, data = deers)
abline(deersreg$coef[1:2])

boxplot(k~timber, data=deers, ylim=c(0,90))
abline(deersreg$coef[c(1,3)])
