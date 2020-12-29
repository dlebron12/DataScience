# We intentionally overfit a data set generated from a simple linear regression 
# by using a polynomial regression model.

# Sample size. Change to n = 5 to get an exact fit.
n = 6

# Predictor
X = rnorm(n,0,5)

# Variance of the noise
sig = 2

# Coefficient
beta = c(2,1)

# Response

Y = beta[1] + beta[2]*X + rnorm(n,0,sig)



# Fittting the data using 4th-order polynomial model with predictors X, X^2, X^3, X^4
X1 = X
X2 = X^2
X3 = X^3
X4 = X^4
polyfit = lm(Y~X1+X2+X3+X4)

polybeta = polyfit$coefficients

# Plot the graph of the 4-th order polynomial regression function 
# Y = betahat[1] + betahat[2]*X + betahat[3]*X^2 + betahat[4]*X^3 + betahat[5]*X^4
xvalue = seq(min(X)-1,max(X)+1,0.01) # the range of X for plotting
polyvalue = polybeta[1] + polybeta[2]*xvalue + polybeta[3]* xvalue^2 + polybeta[4]*xvalue^3 + polybeta[5]*xvalue^4
plot(xvalue,polyvalue,type = 'l') # graph of the 4-th order polynomial 
points(X,Y,col='red',pch = 16,cex = 2) # data points


# plot the fitted line of simple linear regression
regfit = lm(Y~X)
regbeta = regfit$coefficients
regvalue = regbeta[1] + regbeta[2]*xvalue
points(xvalue,regvalue,type = 'l',col= 'blue')
legend(min(X)-1,max(polyvalue),
       c('Polynomial fitted curve','Simple LR fitted line'),
       lty=c(1,1), lwd=c(2.5,2.5), col=c('black','blue') )

# Confidence interval of mean response at X = Xh. Here we consider the new value X = Xh between 
# minimum and maximum values of the predictor X in the data (to avoid extrapolation) where the 
# difference between the value of the polynomial fitted curve and  the simple LR line is largest      
diff = abs(polyvalue-regvalue)
mnid = which.min(abs(xvalue-min(X)))
mxid = which.min(abs(xvalue-max(X)))
Xh = xvalue[which.max(diff[mnid:mxid]) + mnid -1]
points(Xh,beta[1]+beta[2]*Xh, pch = 18,cex = 3, col = 'darkviolet')

# Confidence interval of mean response at X = Xh using simple linear regression
newX=data.frame(X=Xh)
regconf = predict.lm(regfit, newX, interval="confidence",level=0.95, se.fit=TRUE)
points(Xh,regconf$fit[1],pch=16,col='blue',cex = 1.5)

cat("\014") # clear the console
message('\nNew value of predictor: ', Xh)
message('\n\nSIMPLE LINEAR REGRESSION: \n','  True mean response: ', beta[1]+beta[2]*Xh, '\n',
        '  Estimated value: ', regconf$fit[1], '\n',
        '  Confidence interval: ','( ',regconf$fit[2],' , ',regconf$fit[3],' )\n',
        '  Multiple R squared: ',summary(regfit)$r.squared, '\n',
        '  Adjusted R Squared: ',summary(regfit)$adj.r.squared)



# Confidence interval of mean response at X = Xh using polynomial regression
newX=data.frame(X1=Xh,X2 = Xh^2,X3 = Xh^3,X4 = Xh^4)

polyconf = predict.lm(polyfit, newX, interval="confidence",level=0.95, se.fit=TRUE)
points(Xh,polyconf$fit[1],pch=16,col='black',cex = 1.5)
message('\n\nPOLYNOMIAL REGRESSION: \n','  True mean response: ', beta[1]+beta[2]*Xh, '\n',
        '  Estimated value: ', polyconf$fit[1], '\n',
        '  Confidence interval: ','( ',polyconf$fit[2],' , ',polyconf$fit[3],' )\n',
        '  Multiple R squared: ',summary(polyfit)$r.squared, '\n',
        '  Adjusted R Squared: ',summary(polyfit)$adj.r.squared,'\n\n')











