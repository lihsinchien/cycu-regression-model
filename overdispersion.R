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


## other link function

data(bliss)
bliss
mlogit<-glm(cbind(dead,alive)~conc, family="binomial",data=bliss)
mprobit<-glm(cbind(dead,alive)~conc, family="binomial"(link=probit),data=bliss)
mcloglog<-glm(cbind(dead,alive)~conc, family="binomial"(link=cloglog),data=bliss)
mcauchit<-glm(cbind(dead,alive)~conc, family="binomial"(link=cauchit),data=bliss)

predval <- sapply(list(mlogit,mprobit,mcloglog,mcauchit),fitted)
dimnames(predval)<-list(0:4,c("logit","probit","cloglog","cauchit"))
round(predval,3)

dose<-seq(-4,8,0.2)
predval <-sapply(list(mlogit,mprobit,mcloglog,mcauchit),function(m) predict(m,data.frame(conc=dose),type="response"))
colnames(predval) <- c("logit","probit","cloglog","cauchit")
predval <- data.frame(dose,predval)


plot(dose,predval$logit,type="l")
lines(dose,predval$probit,col=2)
lines(dose,predval$cloglog,col=3)
lines(dose,predval$cauchit,col=4)
legend("topleft",c("logit","probit","cloglog","cauchit"),lty=1,col=1:4)
