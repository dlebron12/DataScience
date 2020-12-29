#### Multiple Linear Regression ####
patient = read.table("patient.txt",header=FALSE)
names(patient) = c("satisf","age","severe","anxiety")

pairs(patient)
cor(patient)

# What is the response vector? 
Y = patient$satisf

# design matrix? 
X = cbind(rep(1,46),patient$age, patient$severe, patient$anxiety)
XtX = t(X)%*%X

# hat matrix
H = X%*%solve(XtX)%*%t(X)
install.packages("Matrix")
library(Matrix)
rankMatrix(H)

# least square estimators 
b = solve(XtX)%*%t(X)%*%Y

# calculate the fitted values and residuals
Yhat = X%*%b  
Yhat = H%*%Y
e = Y - Yhat

# perform multiple linear regression 
# first order-model

model = lm(satisf ~age + severe + anxiety, data=patient)
summary(model)
# MSE = (residual standard error)^2
 
# Find critical value for F-test
qf(0.99,3,42)

# Find pvalue for F*
1- pf(30.05,3,42)

# ANOVA decomposition 
anova(model) 
# Observe that SSR(age) = 8275.4, SSR(age| severe) = 480.9, SSR(anxiety| age, severe) = 364.2. 
# That means SSR = 9120.464

# using quadratic formulas to compute SSR and SSE (this will lead to same answers): 
n = dim(patient)[1]
SSE = t(Y)%*%(diag(n)-H)%*%Y
J = matrix(rep(1,n*n),nrow=n,ncol=n)
SSR = t(Y)%*%(H-1/n*J)%*%Y

# check diagnostic plots 
par(mfrow=c(2,2))
plot(model)

#confidence intervals for each regression coefficient 
confint(model, parm=c('age','severe','anxiety'),level=0.95)

## Models with two-way interaction (age*anxiety)
# what happens to our design matrix? 
X_v2 = cbind(X,patient$age,patient$severe, patient$anxiety, patient$age*patient$anxiety)
beta_v2.hat = solve(t(X_v2) %*% X_v2) %*% t(X_v2) %*% Y
H2 = X2 %*% solve(t(X2) %*% X2) %*% t(X2)
rankMatrix(H2)

# of course, we can do all of these using lm function. You can add interaction terms by using ":" in the formula:
model2 = lm(satisf ~ age + severe + anxiety + age:anxiety, data=patient)

summary(model2)
anova(model2)

#Suppose we want a confidence interval for the mean response at a given set of values for X. 
#For example, say we want the confidence interval for mean satisfaction (Y) when age is 10, severity is 50, and anxiety is 2.1
newX = data.frame(age=19,severe=50,anxiety=2.1, age.anxiety=39.9)
predict.lm(model2, newX, interval="confidence", level=0.99, se.fit=TRUE)
# The se.fit option is telling R that we also want the standard errors. 

# We can also swith the interval type to "prediction" if we want a prediction interval instead
predict.lm(model2, newX, interval="prediction", level=0.99, se.fit=TRUE)  

