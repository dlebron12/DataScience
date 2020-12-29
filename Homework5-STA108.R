#Dayanara Lebron Aldea
#HW5-STA108
#24 Feb 2017
#Problem 2 comercial properties: 

#a) Read data into R and Draw the scatter-plot matrix
#Obtain correlation matrix
#what do you observe??? RESPOND
property=read.table("CH06PR18.txt", header=FALSE)
names(property)=c('Y',"X1","X2","X3","X4")
cor(property) #Correlation matrix
pairs(property)

attach(property)
p=4
n=length(property$Y)
#b) Perform regression of the rental rates Y on the four predictors
model1=lm(Y~X1+X2+X3+X4, data=property)

#least squares estimators
model1$coefficients
#(Intercept)            X1            X2            X3            X4 
#1.220059e+01 -1.420336e-01  2.820165e-01  6.193435e-01  7.924302e-06

#MSE 
SSE=sum(model1$residuals^2) #98.23059

#alternatively, 
SSE=anova(model1)[5,2] #98.23059
MSE=anova(model1)[5,3] #1.292508

mean_y=mean(Y)
SSR=sum(anova(model1)[1:4,2]) #138.3269

SSTO=SSE+SSR #236.5575

#R^2
R_2=SSR/SSTO #0.5847496

#alternatively:
R_2=summary(model1)$r.squared
#R^2-adjusted

R_2a=(1-((n-1)/(n-4))*(SSE/SSTO)) #0.568571
#alternatively:
R_2a=summary(model1)$adj.r.squared # 0.5628943


#Fitted regression function
#Y=11.5406-0.1191*X1+0.4461*X2+2.6204*X3+7.924302e-06*X4

#c) Draw residuals vs fitted values plot and residuals normal qq-plot
par(mfrow=c(2,1))
plot(model1)

INSERT PLOT HERE

#d) Draw residuals versus each predictor variable, 
#and residuals vs each two way interaction term plots.
#How many interaction terms are there?  ANS: 3*2=6 

#ANALYZE THE PLOTS AND SUMMARIZE THE FINDINGS.
res=model1$residuals
fig1=plot(X1,res, main ="residuals against X1")
fig2=plot(X2,res, main ="residuals against X2")
fig3=plot(X3,res, main ="residuals against X3")
fig4=plot(X4,res, main="residuals against X4")


#E) For each regression coefficient test weather is zero or not at alpha 0.01. 
#State the null and alternative hypothesis, the test statistic, its distribution and p-value?

#ANS: THE SUMMARY OF THE MODEL ALREADY PROVIDES THE TEST FOR H0: B_i=0; HA: B_i != 0
#we just have to compare that t-value to the qt(1-0.01, n-p), since the test follows a t-distribution
t=qt(1-0.01, n-p)

summary(model1)$coef[,"t value"] > t

#ANS: THIS SHOWS THAT FOR B0, B2, and B4 THE NULL HYPOTHESIS ARE REJECTED and p-values are more significant for these coefficients.
summary(model1)$coef[,"Pr(>|t|)", drop=F]

#F) test whether there is a relation at alpha 0.01. State null and alternative, the test statistic, and its null dist
#the decision rule and your conclusion.

#From before we have already retrieved SSTO,SSR, and SSE 
#H0: all B_i=0, HA: not all B_i's equal 0.
#TEST: F-STATISTIC WITH P-1, N-P DF
#DECISION RULE: REJECT HO IF F* > F(1-0.01,P-1,N-P)

F_star=(SSR/(p-1))/(MSE)
F_ST=qf(1-0.01,p-1,n-p)

F_star >F_ST #TRUE
#this means that there is a relation between X's and Y and not all X are 0. 

#G) FIT A DIFFERENT MODEL BY REGRESSING THE RENTAL RATES ON THREE PREDICTORS X1,X2,X4
#WHY WOULD YOU MAKE SUCH A DECISION? GET THE LEAST SQUARES ESTIMATORS AND WRITE DOWN THE FITTED
#REGRESSION FUNCTION. WHAT ARE THE MSE, R^2, R^2A? HOW DO THIS NUMBERS COMPARE WITH THOSE FROM MODEL1?

#WHY WOULD YOU MAKE SUCH A DECISION? Because B3 was not a significant variable and since it also failed the test it means that it has no relation with Y.
#it would be best to see what can we achieve in the model without that variable.

model2=lm(Y~X1+X2+X4, data=property)
COEFF=summary(model2)$coef[,"Estimate"] #least-squares estimators

#regression function is: Y=1.237058e+01+-1.441646e-01X1+2.671670X2+8.178210e-06X4

MSE2=anova(model2)[4,3] #1.2811
R2_2=summary(model2)$r.squared #0.5829752
R2_a2=summary(model2)$adj.r.squared #0.5667275

#how do these numbers compare to the first model?
#The MSE for model 1 was 1.2925 whereas for model 2 it was 1.28 which means we have lower residuals in this model but not by much
#Also the R^2 is  0.5847 and 0.5829 in model2. again numbers are lower but not by much since the X3 did not contribute much to the model.


#ANOVA
#Analysis of Variance Table

#Response: Y
#Df Sum Sq Mean Sq F value    Pr(>F)    
#X1         1 14.819  14.819  11.566  0.001067 ** 
#  X2         1 72.802  72.802  56.825 7.841e-11 ***
#  X4         1 50.287  50.287  39.251 1.973e-08 ***
#  Residuals 77 98.650   1.281                      
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#H) Compare standard errors of those under model 1 and model2. What do you find?
#ANS: STANDARD ERRORS FOR MODEL2 ARE SMALLER THAN THOSE FOR MODEL1

sd_mod2=as.numeric(summary(model2)$coef[,"Std. Error"])
#(Intercept)           X1           X2           X4 
#4.928469e-01 2.092012e-02 5.729487e-02 1.305377e-06

sd_mod1=summary(model1)$coef[,"Std. Error"]
# (Intercept)           X1           X2           X3           X4 
#5.779562e-01 2.134261e-02 6.317235e-02 1.086813e+00 1.384775e-06 


#Construct 95% Confidence intervals for the regression coefficients for X1,X2 and X4 under model2.
confint(model2,c("X1","X2","X4"), level=0.95)
#would they be narrower or wider than those in model1?
#they would be narrower than those in model 1, this is because the standard errors are smaller and the width of the CI depends on the sd.

#I)
Xnew=data.frame(X1=4, X2=10, X3=0.1, X4=80,000)

#construct 99% prediction intervals undpredict.lm(interfit, newX, interval="prediction",
predict.lm(model1, Xnew, interval="prediction", level=0.99, se.fit=TRUE)$fit
#fit      lwr      upr
#1 15.78771 12.18633 19.38909
predict.lm(model2, Xnew, interval="prediction", level=0.99, se.fit=TRUE)$fit
#      fit      lwr      upr
#1 14.46625 11.40128 17.53121

#Compare these two sets of intervals, what do you find?
#That the prediction interval for the model 2 is just a little bit narrower but about the same as the model 1

#J) I prefer model2, just because it got rid of a variable that serve nothing to predict values of Y and to ease interpretation of the model. 


