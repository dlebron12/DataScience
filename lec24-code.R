########################### read in data 
data=read.table("surgical.txt")
##
names(data)=c("X1","X2","X3","X4", "X5", "X6","X7","X8","Y", "lnY")
data=data[,1:9]  ##delete the lnY column

##fit a first-order model with all X variables
fit1=lm(Y~., data=data) ##fit a first-order model with all X variables
plot(fit1, which=1)  ## residuals vs. fitted shows nonlinearity and nonconstant variance
plot(fit1, which=2) ##residuals Q-Q shows heavy right tail

library(MASS)
boxcox(fit1) ### boxcox procedure suggests logarithm transformation of the response variable.

fit2=lm(log(Y)~., data=data) ##fit a first-order model with all X variables and log Y
plot(fit2, which=1)  ## residuals vs. fitted shows nonlinearity and nonconstant variance
plot(fit2, which=2) ##residuals Q-Q shows heavy right tail



## call the necessary library for exhaustive model selection
library(leaps)

## nbest: number of top models for a fixed number of predictors
## nvmax: maximum number of predictors
nbest = 1
subset = regsubsets(log(Y)~., data=data, nbest=nbest, nvmax=8, method="exhaustive")

## return R^2, R_a^2, AIC and BIC for these ``best" models.
sumsub = summary(subset) 

## which variables are included
sumsub$which

## coefficient of determination R^2
sumsub$rsq

## adjusted coefficient of determination R_a^2
sumsub$adjr2

## BIC
## sample size 
n=nrow(data) 
## number of coefficients in each model: p
p.m=as.integer(as.numeric(rownames(sumsub$which))+1) 
bic = n*log(sumsub$rss/n) + (log(n))*p.m

## AIC 
aic = n*log(sumsub$rss/n) + 2*p.m

## table of all criteria values
sumtable = cbind(sumsub$which,sumsub$rsq,sumsub$adjr2,bic,aic)
colnames(sumtable)=c(colnames(sumsub$which), "R^2", "R^2_a", "BIC", "AIC")
round(sumtable,3) ## round the results to three decimals

## get the index for the best model within each size group (the first one in the group)
index.best=c(which.max(sumsub$rsq),which.max(sumsub$adjr2),which.min(bic),which.min(aic))

## draw plots and connect the best models across size groups 
par(mfrow=c(2,2))
plot(p.m,sumtable[,"R^2"], xlab="p", ylab="R^2", pch=19, cex=1.2, cex.axis=1, cex.lab=1.5, cex.main=1.5)
points(p.m[seq(1,length(p.m),nbest)],sumtable[seq(1,length(p.m),nbest),"R^2"], type='l', lwd=2)
points(p.m[which.max(sumtable[,"R^2"])],max(sumtable[,"R^2"]),col='red',pch = 16,cex = 2)

plot(p.m,sumtable[,"R^2_a"], xlab="p", ylab="R^2_a", pch=19, cex=1.2, cex.axis=1, cex.lab=1.5, cex.main=1.5)
points(p.m[seq(1,length(p.m),nbest)],sumtable[seq(1,length(p.m),nbest),"R^2_a"], type='l', lwd=2)
points(p.m[which.max(sumtable[,"R^2_a"])],max(sumtable[,"R^2_a"]),col='red',pch = 16,cex = 2)

plot(p.m,sumtable[,"BIC"], xlab="p", ylab="BIC", pch=19, cex=1.2, cex.axis=1, cex.lab=1.5, cex.main=1.5)
points(p.m[seq(1,length(p.m),nbest)],sumtable[seq(1,length(p.m),nbest),"BIC"], type='l', lwd=2)
points(p.m[which.min(sumtable[,"BIC"])],min(sumtable[,"BIC"]),col='red',pch = 16,cex = 2)

plot(p.m,sumtable[,"AIC"], xlab="p", ylab="AIC", pch=19, cex=1.2, cex.axis=1, cex.lab=1.5, cex.main=1.5)
points(p.m[seq(1,length(p.m),nbest)],sumtable[seq(1,length(p.m),nbest),"AIC"], type='l', lwd=2)
points(p.m[which.min(sumtable[,"AIC"])],min(sumtable[,"AIC"]),col='red',pch = 16,cex = 2)


##### Forward selection  ###################
fit = lm(log(Y)~1, data=data)  ## initial model: none-model with only intercept term
fita = lm(log(Y)~., data=data)
step = stepAIC(fit,scope=list(upper=formula(fita), lower=~1), direction="forward", k=2)

##### Backward elimination ####################
fit = lm(log(Y)~., data=data)
step = stepAIC(fit, scope = list(upper=~., lower=~1),direction="backward", k=2)

##### Forward selection with interactions ##########
fit.0 =lm(log(Y)~1, data=data) ## none-model without X variable
fit.2 = lm(log(Y)~.^2, data=data)  ## full model with all 2-way interactions
step = stepAIC(fit.0,scope=list(upper=formula(fit.2), lower=~1),direction="forward", k=2)












