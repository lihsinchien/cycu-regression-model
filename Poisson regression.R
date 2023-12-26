## poisson distribution
barplot(dpois(0:5,0.5),xlab="y",ylab="Probability",names=0:5,main="mean = 0.5")
barplot(dpois(0:10,2),xlab="y",ylab="Probability",names=0:10,main="mean = 2")
barplot(dpois(0:15,5),xlab="y",ylab="Probability",names=0:15,main="mean = 5")

## gala data 
data(gala, package="faraway")
gala <- gala[,-2]

# fit linear model
modl <- lm(Species ~ . , gala)
plot(modl, 1) # unequal variance
modt <- lm(sqrt(Species) ~ . , gala)
plot(modt, 1) 
summary(modt)

# fit poisson regression model
modp <- glm(Species ~ ., family=poisson, gala)
summary(modp)

## dispersed  poisson model
plot(log(fitted(modp)),log((gala$Species-fitted(modp))^2), xlab=expression(hat(mu)),ylab=expression((y-hat(mu))^2))
abline(0,1)

# estimate dispersion parameter
(dp <- sum(residuals(modp,type="pearson")^2)/modp$df.res)

# fit model with dispersion 
summary(modp,dispersion=dp)
modd <- glm(Species ~ ., family=quasipoisson, gala)

# model selection with dispersion
drop1(modd,test="F")

## rate model
data(dicentric, package="faraway")

# fit linear model
lmod <- lm(ca/cells ~ log(doserate)*factor(doseamt), dicentric)
summary(lmod)$adj #adjust R square

# diagnosis of the linear model 
plot(residuals(lmod) ~ fitted(lmod),xlab="Fitted",ylab="Residuals") # unequal variance
abline(h=0)
dicentric$dosef <- factor(dicentric$doseamt)

# fit poisson regression model
pmod <- glm(ca ~ log(cells)+log(doserate)*dosef,  family=poisson,dicentric)
summary(pmod)

# fit a rate model (set the coefficient of log(cells) = 1)
rmod <- glm(ca ~ offset(log(cells))+log(doserate)*dosef,  family=poisson,dicentric)
summary(rmod)
