
library(faraway)
 
data(orings, package="faraway")
plot(damage/6 ~ temp, orings, xlim=c(25,85), ylim = c(0,1), xlab="Temperature", ylab="Prob of damage")
lmod <- glm(cbind(damage,6-damage) ~ temp, family=binomial,orings)
sumary(lmod)
x <- seq(25,85,1)
lines(x,ilogit(11.6630-0.2162*x))
ilogit(11.6630-0.2162*31)


# H0:current model, H1:Saturated model(full model)
pchisq(deviance(lmod),df.residual(lmod),lower=FALSE)
#not reject H0 => current model fit well

pchisq(38.9,22,lower=FALSE)
#Null model, p-value<0.05 => not fit well (not like saturated model)

pchisq(38.9-16.9,1,lower=FALSE)
#the effect of launch temperature is statistically significant

erings <- with(orings, data.frame(temp=rep(temp,each=6), damage=as.vector(sapply(orings$damage, function(x) rep(c(0,1), times=c(6-x,x))))))
head(erings)
emod <- glm(damage ~ temp, family=binomial, erings)
sumary(emod)
confint(lmod)
