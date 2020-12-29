#### Multiple Linear Regression ####
patient = read.table("patient.txt",header=FALSE)

# adding a 4th variable to patient
set.seed(1234)
patient$X4 = rnorm(0,sd=5,n=46)

names(patient) = c("Y","X1","X2","X3","X4")

#fit a linear model
model1=lm(Y~X1+X2+X3+X4,data=patient)

# want to test whether both X2 and X3 can be dropped from the model 
model2=lm(Y~X4+X1+X2+X3,data=patient)
anova(model2)

#compare to F critical value
qf(0.99,2,41)

### Coefficient of Partial Determination ##
#A coefficient of partial determination measures the marginal contribution of one predictor variable when others   
# are already in the model. Let's use the patient dataset again. The response variable is patients's satisfaction. 
# We have 3 predictor variables: patient age (X1), severity of illness (X2), and anxiety level (X3). 
# We want to find the proportional reduction in SSE that is gained by including X2 into the model when X1 is already in the model. 

patient = read.table("patient.txt",header=FALSE)
names(patient)=c("Y","X1","X2","X3")

# how to get SSE(X1)?
fit1 = lm(Y~X1,data=patient)

# how to get SSE(X1,X2)?
fit2 = lm(Y~X1+X2,data=patient)

# the partial coefficient of determination for X2 when X1 is already in the model should be (5093.9 - 4613.0)/5093.9 = 0.0944
# then r2|1 = - sqrt(0.0944) = -0.3072458

# we can find r2|1 using a different approach: 
r1 = lm(Y~X1,data=patient) # regress Y on X1
r2 = lm(X2~X1,data=patient) # regress X2 on X1
e1 = residuals(r1)
e2 = residuals(r2)
cor(e1,e2) # this output is exactly the same as calculated above. 

# added variable plot: shows the marginal importance of X2 when X1 is already in the model
plot(e1,e2)

# Useful commands for variable exploration and standardization 
#histograms
par(mfrow=c(2,2))
hist(patient$X1,main="age",xlab="age")
hist(patient$X2,main="severity",xlab="severity")
hist(patient$X3,main="anxiety",xlab="anxiety")

#boxplots
boxplot(patient$X1,patient$X2,patient$X3)
