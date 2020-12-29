#### Simple Linear Regression in Matrix Form ####
Y=c(16,15,17,12,22,20) # response vector
Y = matrix(c(16,15,17,12,22,20),6,1)

X0 = rep(1,6)
X1 = c(1,4,2,0,6,5)
X = cbind(X0,X1) # Design matrix

# calculate the least square estimator
XtX=t(X)%*%X 
XtY=t(X)%*%Y
b = solve(XtX)%*% XtY

# Checking using lm?
model = lm(Y~X1)

# calculate the hat matrix H. Is it symmetric? idempotent?
H = X%*%solve(XtX)%*%t(X)

# calculate the fitted values and residuals
Yhat = X%*%b  
Yhat = H%*%Y
e = Y - Yhat

#### Multiple Linear Regression ####
patient = read.table("/Users/lynnachu/Documents/STA 108/Winter 2017/patient.txt",header=FALSE)
names(patient) = c("satisf","age","severe","anxiety")
## scatter plot matrix 
pairs(~satisf+age+severe+anxiety, data=patient)
cor(patient)

# What is the response vector? 
Y = patient$satisf

# design matrix? 
X = cbind(rep(1,46),patient$age, patient$severe, patient$anxiety)

# hat matrix? Is it symmetric? idempotent? dimension? 
H = X%*%solve(XtX)%*%t(X)
#H is idempotent


# calculate the fitted values and residuals
Yhat = X%*%b  
Yhat = H%*%Y
e = Y - Yhat

# can use the lm function to do the same thing: 
model = lm(satisf ~ age+ severe + anxiety, data=patient)
plot(model) # see diagnostic plots 