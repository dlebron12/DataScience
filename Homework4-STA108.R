#Dayanara Lebron Aldea
#HW-4
#STA108

setwd("Downloads")
data<-read.csv("CH01PR27.txt", head=F)
y=data[,1]
x=data$V1
n=length(y)

#a Create the design matrix X and the response vector Y. What are their dimensions?
X=cbind(rep(1,length(x)),as.matrix(as.integer(x)))
Y=as.matrix(as.integer(y))
#Dimensions
dim(X)
dim(Y)

#b Calculate X'X, X'Y and (X'X)-1

xtx=t(X)%*%X
xty=t(X)%*%Y
ixtx=solve(xtx)

#c Calculate the least squares
B=ixtx%*%xty
H=X%*%ixtx%*%t(X)

#d Calculate the hat matrix, What is tits dimension, report the 6 diagonal values
dim(H) #60X60
diag(H)[1:6]
#0.04784477 0.04514863 0.02796811 0.05984837 0.02489204 0.06658160

#e Calculate the fitted values and residuals

####FITTED VALUES####
Yhat=H%*%Y

Yhat[1:6]
#52 51 43 56 41 58
####Residuals#####
residuals=Y-Yhat
residuals[1:6]
#-2.842171e-14 -2.842171e-14  0.000000e+00  1.421085e-14  2.131628e-14 -4.263256e-14

#f Calculate SSTO, SSE, SSR using the following R codes

SSTO=sum((Y-mean(Y))^2); SSTO
#16406.18
SSE=sum((Y-Yhat)^2); SSE
#1.460182e-26
SSR=sum((Yhat-mean(Y))^2);SSR
#16406.18

#f-2 Calculate SSTO,SSE and SSR using the quadratic formula used in class?
#Do f-1 and f-2 give you the same result? Does the decomposition of total variation hold?

#ANSWER: YES, RESULTS FROM THE QUESTIONS F1 AND F2 ARE THE SAME THEREFORE THE DECOMPOSITION OF VARIANCES HOLD. 
I=diag(n)
J=matrix(rep(1,n*n),ncol=n)


#### As stated in section 5.13 Sums of Squares as Quadratic Forms
SSTO=t(Y)%*%(I-J/n)%*%Y
#16406.18
SSE=t(Y)%*%(I-H)%*%Y
#1.460182e-26
SSR=t(Y)%*%(H-J/n)%*%Y
#16406.18

#Also get MSE
MSE=SSE/(n-2)
#4.867272e-27

#g Calculate variance of beta and get the standard errors s(b0) and s(b1) and cov(b0 and b1)

#CALCULATE VARIANCE-COVARIANCE MATRIX
vcm=MSE*(ixtx)

var_b0=vcm[1,1]
var_b1=vcm[2,2]
covb0_b1=vcm[1,2]; covb0_b1  #-8.717242e-30

#Check if this is the same as above matrix! then se_b0=sqrt(var_b0) and se_b1=sqrt(var_b1)
#Standard Errors
se_b0=sqrt((1/n+xbar^2/sum((x-xbar)^2))*MSE)
se_b1=sqrt(MSE/sum((x-xbar)^2))


#h What are the estimated variances for the first 6 residuals
var_e=MSE*(I-H); var_e
var_e[1:6,1]
# -2.388911e-13  1.165815e-14  8.891185e-15  1.338750e-14  8.199444e-15  1.407924e-14

####EXERCISE-4 MULTIPLE LINEAR REGRESSION BY MATRIX ALGEBRA IN R

#a Write down the model equations and the coefficient vector B
B=c(b0,b1,b2)


#b Specify the design matrix and the response vector. Obtain the hat matrix H.
y=c(-0.97, 2.51, -0.19, 6.53, 1.00)
x1=c(-0.63, 0.18, -0.84, 1.60, 0.33)
x2=c(-0.82, 0.49, 0.74, 0.58, -0.31)
X=cbind(rep(1,length(x1)),x1,x2) #Design matrix
        
H=X%*%solve(t(X)%*%X)%*%t(X)

#c Obtain the least squares estimators Beta-Hat
xtx=t(X)%*%X
xty=t(X)%*%y
ixtx=solve(xtx)
b=ixtx%*%xty

#d Obtain the fitted values and the residuals. 
Yhat=H%*%y
residuals=y-Yhat

