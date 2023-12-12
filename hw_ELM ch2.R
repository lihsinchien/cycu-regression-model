
library(faraway)
head(pima)
?pima
summary(pima)


#參考 迴歸模型 ch1 講義

pima$diastolic[pima$diastolic==0] <- NA
pima$glucose[pima$glucose==0] <- NA
pima$triceps[pima$triceps==0] <- NA
pima$insulin[pima$insulin==0] <- NA
pima$bmi[pima$bmi==0] <- NA

r<-glm(test~age+diabetes+bmi+insulin+triceps+diastolic+glucose+pregnant,family="binomial",data=pima)
