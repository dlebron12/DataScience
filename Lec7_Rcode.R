# read the data from the file CH01PR20.txt in the working directory
copier = read.table("CH01PR20.txt",header=F) 

# read the predictor response values from the data copier
X = copier[,2]
Y = copier[,1]

# plot the data points
plot(X,Y,main="Scatterplot",xlab="X",ylab="Y")


########## problem 3(a): estimate beta_1 #######################
# First, compute statistics
Xbar=mean(X)
Ybar=mean(Y)
SSx=sum((X-Xbar)^2)
SSxy=sum((X-Xbar)*(Y-Ybar))


### Calculate b0 and b1
b1=SSxy/SSx                     #use the formula to calculate b0 and b1
b0=Ybar-b1*Xbar


### Run simple linear regression model
model=lm(Y~X)
summary(model)
names(model)     #list out all the values given from the model
model$coefficients
model$fitted.values
model$residuals










