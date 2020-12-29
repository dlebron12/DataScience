#### Let the true regression line be E(Y) = 1 + 2X
beta_0 = 1
beta_1 = 2

## Input X
X = c(-2, 9, 0, 8, -5, 7, 3, -3, 5, 4, 2.3, -1.5, 3.5, 4.5, 7.7, -3.8, 5.8, 6.7, 0.4, 1.3, -4.2, 1.7, 7.2, -1, 1)
n = length(X)

## Assume the normal error regression model with error terms iid N(0,10)
## generate the data several times and then plot to visualize the data
Y = beta_0 + beta_1*X + rnorm(n, sd=sqrt(10))
plot(X,Y)

## confidence intervals for beta_0 and beta_1
Xbar = mean(X)
Ybar = mean(Y)
SSX = sum((X-Xbar)^2)
SSY = sum((Y-Ybar)^2)
SSXY = sum((X-Xbar)*(Y-Ybar))

b1 = SSXY/SSX
b0 = Ybar - b1*Xbar

Yhat = b0+b1*X
residual = Y-Yhat
MSE = sum(residual^2)/(n-2)
sb1 = sqrt(MSE/SSX)
sb0 = sqrt(MSE*(1/n + Xbar^2/SSX))

alpha = 0.05

b0-qt(1-alpha/2,n-2)*sb0
b0+qt(1-alpha/2,n-2)*sb0

b1-qt(1-alpha/2,n-2)*sb1
b1+qt(1-alpha/2,n-2)*sb1

## check for the number of intervals containing the true values
B = 1000
alpha = 0.05
b0.lb = b0.ub = b1.lb = b1.ub = rep(0,B)
for (i in 1:B){
  Y = beta_0 + beta_1*X + rnorm(n, sd=sqrt(10))
  Ybar = mean(Y)
  SSY = sum((Y-Ybar)^2)
  SSXY = sum((X-Xbar)*(Y-Ybar))
  b1 = SSXY/SSX
  b0 = Ybar - b1*Xbar
  Yhat = b0+b1*X
  residual = Y-Yhat
  MSE = sum(residual^2)/(n-2)
  sb1 = sqrt(MSE/SSX)
  sb0 = sqrt(MSE*(1/n + Xbar^2/SSX))
  b0.lb[i] = b0-qt(1-alpha/2,n-2)*sb0
  b0.ub[i] = b0+qt(1-alpha/2,n-2)*sb0
  b1.lb[i] = b1-qt(1-alpha/2,n-2)*sb1
  b1.ub[i] = b1+qt(1-alpha/2,n-2)*sb1
}

# visualize the confidence intervals for beta_1
plot(c(b1.lb[1], b1.ub[1]), c(1,1), type="l", xlim=c(min(b1.lb), max(b1.ub)), ylim=c(0,B), ylab="experiments", xlab="")
for (i in 2:B){
  segments(b1.lb[i],i,b1.ub[i],i)
}
abline(v=beta_1, col="blue")
count1 = 0
for (i in 2:B){
  if (b1.lb[i]>beta_1 || b1.ub[i]<beta_1){
    segments(b1.lb[i],i,b1.ub[i],i, col="red")
    count1 = count1 + 1
  }
}
(B-count1)/B

# visualize the confidence intervals for beta_0
plot(c(b0.lb[1], b0.ub[1]), c(1,1), type="l", xlim=c(min(b0.lb), max(b0.ub)), ylim=c(0,B), ylab="experiments", xlab="")
for (i in 2:B){
  segments(b0.lb[i],i,b0.ub[i],i)
}
abline(v=beta_0, col="blue")
count0 = 0
for (i in 2:B){
  if (b0.lb[i]>beta_0 || b0.ub[i]<beta_0){
    segments(b0.lb[i],i,b0.ub[i],i, col="red")
    count0 = count0 + 1
  }
}
(B-count0)/B

### confidence interval of the mean response and prediction interval, e.g., for Xh=1
Xh = 1
# the true mean response
truemean = beta_0 + beta_1*Xh

# confidence intervals for the mean response, and the prediction intervals
B = 1000
alpha = 0.05
ci.lb = ci.ub = pi.lb = pi.ub = Yhs = rep(0,B)
for (i in 1:B){
  Y = beta_0 + beta_1*X + rnorm(n, sd=sqrt(10))
  Ybar = mean(Y)
  SSY = sum((Y-Ybar)^2)
  SSXY = sum((X-Xbar)*(Y-Ybar))
  b1 = SSXY/SSX
  b0 = Ybar - b1*Xbar
  Yhat = b0+b1*X
  residual = Y-Yhat
  MSE = sum(residual^2)/(n-2)
  Yhhat = b0 + b1*Xh
  sYhhat = sqrt(MSE*(1/n + (Xh-Xbar)^2/SSX))
  ci.lb[i] = Yhhat-qt(1-alpha/2,n-2)*sYhhat
  ci.ub[i] = Yhhat+qt(1-alpha/2,n-2)*sYhhat
  # observed Y for X=Xh in a new trial
  Yhs[i] = beta_0 + beta_1*Xh + rnorm(1,sd=sqrt(10))
  spred = sqrt(MSE*(1+ 1/n + (Xh-Xbar)^2/SSX))
  pi.lb[i] = Yhhat-qt(1-alpha/2,n-2)*spred
  pi.ub[i] = Yhhat+qt(1-alpha/2,n-2)*spred
}

# the number of C.I.'s contains the mean response
n1 = 0
for (i in 1:B){
  if (ci.lb[i]<truemean && ci.ub[i]>truemean){
    n1 = n1 + 1
  }
}
n1
n1/B

# the number of C.I.'s contains the new observation
n2 = 0
for (i in 1:B){
  if (ci.lb[i]<Yhs[i] && ci.ub[i]>Yhs[i]){
    n2 = n2 + 1
  }
}
n2
n2/B

# the number of P.I.'s contains the new observation
n3 = 0
for (i in 1:B){
  if (pi.lb[i]<Yhs[i] && pi.ub[i]>Yhs[i]){
    n3 = n3 + 1
  }
}
n3
n3/B

######## ANOVA ##########
#copier = read.table("/Users/lynnachu/Documents/STA 108/Winter 2017/CH01PR20.txt",header=F) 

# read the predictor response values from the data copier
#X = copier[,2]
#Y = copier[,1]

#model = lm(Y~X)

# the anova table is found using the anova function
#anova(model)

