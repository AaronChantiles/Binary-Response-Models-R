#1
mydata = read.csv("Loan.csv")
str(mydata)
mydata$Education = as.factor(mydata$Education) 
str(mydata)

#2
lpm.res = lm(Loan~., data=mydata)
summary(lpm.res)

#3
head(sort(predict(lpm.res)), 10)
tail(sort(predict(lpm.res)), 10)

#4
logit.res = glm(Loan~., family = binomial(link=logit), data=mydata)
summary(logit.res)
yhat = rep(1, nrow(mydata))
yhat[ predict(logit.res, type="response") < mean(mydata$Loan) ] = 0

#5
confusion = table(yhat, mydata$Loan)
confusion
sum(diag(confusion) / sum(confusion))
confusion[1:1] / sum(confusion[,1])
confusion[2:2] / sum(confusion[,2])

#6
coef(logit.res)
colMeans(mydata[1:4])
exp(-13.17783285+0.05979075*73.774200+0.58707882*2.396400+0.16267911*1.937938+3.91060897)
xvalues = data.frame(Income=73.774200, Family=2.396400, CCAvg=1.937938, Education="2")
predict(logit.res, newdata=xvalues, type="response")

#7
coef.lpm = coef(lpm.res)
coef.logit = coef(logit.res)
cbind(coef.lpm, coef.logit)

#8
xvalues = data.frame(Income=73.774200, Family=2.396400, CCAvg=1.937938, Education="2")
xb = predict(logit.res, newdata = xvalues)
PE.logit = dlogis(xb) * coef(logit.res)[-1]
PE.lpm = coef(lpm.res)[-1]
cbind(PE.lpm, PE.logit)







