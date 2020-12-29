# standardized regression model
patient = read.table("patient.txt",header=FALSE)
names(patient) = c("Y","X1","X2","X3")

apply(patient,2,mean) # sample mean

apply(patient,2,sd) # sample standard deviation

# Correlation transformation 
n = dim(patient)[1]
Y = patient$Y
Y_s = (1/sqrt(n-1))*(Y-mean(Y))/sd(Y)

X1= patient$X1
X1_s=(1/sqrt(n-1))*(X1-mean(X1))/sd(X1)

X2=patient$X2
X2_s=(1/sqrt(n-1))*(X2-mean(X2))/sd(X2)

X3=patient$X3
X3_s=(1/sqrt(n-1))*(X3-mean(X3))/sd(X3)

apply(cbind(Y_s,X1_s,X2_s,X3_s),2,mean)

# Obtain standardized regression model
model = lm(Y_s ~ X1_s+X2_s+X3_s)

#transform back to original model
beta_s=coefficients(model)[2:4]

sd(Y)/sd(X1)*beta_s[1]
sd(Y)/sd(X2)*beta_s[2]
sd(Y)/sd(X3)*beta_s[3]

#check they are the same as the original model
summary(lm(Y~X1+X2+X3,data=patient))


