library("faraway")

### example: orings
data(orings)
plot(damage/6 ~ temp, orings, xlim=c(25,85), ylim = c(0,1), xlab="Temperature", ylab="Prob of damage")
lmod <- glm(cbind(damage,6-damage) ~ temp, family=binomial,orings)
summary(lmod)
x <- seq(25,85,1)
plot(orings$temp,orings$damage/6,xlab="temerature",ylab="Prob of damage",xlim=c(25,80),ylim=c(0,1))
lines(x,ilogit(11.6630-0.2162*x))

#predict when temp=31
ilogit(11.6630-0.2162*31)

#H0:model S, H1:saturated model
pchisq(deviance(lmod),df.residual(lmod),lower=FALSE)

#H0:Null,  H1:saturated model
pchisq(38.9,22,lower=FALSE)

#H0:Null,  H1:model S
pchisq(38.9-16.9,1,lower=FALSE)

#CI for beta
confint(lmod)


## overdispersion

data(troutegg)
?troutegg

ftable(xtabs(cbind(survive,total) ~ location+period, troutegg))
bmod <- glm(cbind(survive,total-survive) ~ location+period, family=binomial,troutegg)
summary(bmod)

halfnorm(residuals(bmod))

(sigma2 <- sum(residuals(bmod,type="pearson")^2)/12)
drop1(bmod,scale=sigma2,test="F")
summary(bmod,dispersion=sigma2)
