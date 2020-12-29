########################################################################################
########################### Linear Regression Example ##################################
########################################################################################

# Read data from a dataset: 

mydata = read.table("file directory where your txt file is saved",header=F) 

# example: mydata = read.table("/Users/lynnachu/CH01PR20.text",header=F) 

#### Input data
x=c(1,4,2,0,6,5)
y=c(16,15,17,12,22,20)
plot(x,y,main="Scatterplot",xlab="X",ylab="Y")

n=length(x)    #calculate the number of data points

xbar=mean(x)
ybar=mean(y)
SSx=sum((x-xbar)^2)
SSy=sum((y-ybar)^2)
SSxy=sum((x-xbar)*(y-ybar))

### Run simple linear regression model
model=lm(y~x)
summary(model)
names(model)     #list out all the values given from the model
model$coefficients
model$fitted.values
model$residuals

### Draw scatterplot 
plot(x,y,main="Scatterplot")
abline(model)        #add least square line


### Calculate b0 and b1
b1=SSxy/SSx                     #use the formula to calculate b0 and b1
b0=ybar-b1*xbar
b0=model$coefficients[1][[1]]   #take out the b0 and b1 value from fitted model result
b1=model$coefficients[2][[1]]


### Plot Scatterplot of data, least square line as well as fitted values, residuals
plot(x,y,main="scatterplot")        #scatter plot of x and y
abline(model)  				#add least square line
#add fitted value
points(x,model$fitted.values,pch=2,col="red")
#add residuals 
segments(x, y, x, model$fitted.values, col="blue")
#add legend to the plot
legend("topleft",c("y","y_hat","residual"),pch=c(1,2,3),col=c("black","red","blue"),text.col=c("black","red","blue"))


### calculate SSE, MSE
SSE=sum((y-model$fitted.values)^2)
MSE=SSE/(n-2)    #sigma_hat: sqrt(MSE)

### calculate se of b0 and b1
se_b0=sqrt((1/n+xbar^2/sum((x-xbar)^2))*MSE)
se_b1=sqrt(MSE/sum((x-xbar)^2))

### Hypothesis Testing on beta_1 at 5% significant level
t1=b1/se_b1   	      #test statistics for testing beta_1
qt(df=n-2,1-0.05/2)     #critical value of alpha=0.1
(1-pt(df=n-2,t1))*2     #P-VALUE of test statistics (two-sided test)


### Construct 95% confidence interval for beta1
b1-qt(df=n-2,1-0.05/2)*se_b1;b1+qt(df=n-2,1-0.05/2)*se_b1   
			##95% CI for beta_1