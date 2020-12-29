
#1) Read data 
diab<-read.table("diabetes.txt", header=T, na.strings="NA")
attach(diab)
diab<-droplevels(diab)

#2) Type of variables: glyhb, ratio, bp.1s, age, gender, frame
#glyhb, ratio, bp.1s, age are quantitative variables, and gender and frame are qualitative variables.

str(diab)

par(mfrow=c(1,2))
n=dim(diab)[1]
pct=round(100*table(diab$gender)/n)
labels=c("females:","males:")
lbls=paste(labels,pct)
lbls=paste(lbls,'%',sep=' ')
pie(table(diab$gender),labels=lbls, col=c("red","green"), main="Gender Distribution")
pie(table(droplevels(diab$frame)),col=c("light blue","pink","yellow"), main="Frame Levels")

###COMMENT ON DISTRIBUTION


par(mfrow=c(2,2))
hist(glyhb, breaks=20)
hist(ratio, breaks=20)
hist(bp.1s, breaks=20)
hist(age,breaks=20)

#COMMENT ON DISTRIBUTION

#3)Draw histograms for different transformations 
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
hist(log(glyhb), breaks=20)
hist(sqrt(glyhb), breaks=20)
hist((1/glyhb),breaks=20)

#1/glybh appears to have a normal distribution
glyhb_tr=(1/glyhb)

#4)Scatter plot matrix of predictor variables: ratio, bp.1s, age against glybh and glybh_tr
par(mfrow=c(3,2))
qvar=c(ratio,bp.1s,age)
plot(ratio, glyhb, main="Glyhb vs Ratio")
plot(ratio, glyhb_tr, main="1/Glybh vs Ratio")
plot(age, glyhb, main="Glyhb vs Ratio")
plot(age, glyhb_tr, main="1/Glyhb vs Ratio")
plot(bp.1s, glyhb, main="Glyhb vs Ratio")
plot(bp.1s, glyhb_tr, main="1/Glyhb vs Ratio")


#5) Distribution of glyhb for males and females
par(mfrow=c(1,2))
boxplot(glyhb~gender, col=c("yellow","orange"),main="Distribution of Glyhb vs. Gender")
boxplot(glyhb_tr~gender,col=c("yellow","orange"), main="Distribution of 1/Glyhb vs Gender")

#What do you observe in th boxplots?


#6) Regression of glyhb to ratio, bp.1s, gender and frame 
mod1<-lm(glyhb~bp.1s+age+ratio+gender+frame)
summary(mod1)

#Residuals qqplot and residuals vs fitted values plots
plot(mod1)

#Do model assumptions hold?
No.

#7 Using box-cox transformation to check what transformation is a good choice
library(MASS)
boxcox(glyhb~bp.1s+age+ratio+gender+frame)

#In this plot the lambda that maximixes the log-likelihood function is about -1.3 we can round this to -1
#our choice of glyhb is correct to be 1/glyhb.

#9 From model 2 dropped gender
mod2<-lm(glyhb_tr~bp.1s+age+ratio+frame+gender)
#Anova with this model kept SSE at 0.777 excludng gender
#Anova with model without frame has SSE at 0.785 so according to anova I will drop gender
anova(mod2)
summary(mod2)

plot(mod2)


#9) mod3<-lm(glyhb_tr~age+ratio+frame+bp.1s)
anova(mod3)
summary(mod3)

AIC(mod3)
BIC(mod3)
#Dropping frame I get a SSe of 0.785
#Dropping bp.1s I get an SSE of 0.7929, It would of been better to drop frame

mod4<-lm(glyhb_tr~age+ratio+frame)
summary(mod4)
AIC(mod4)
BIC(mod4)

mod5<-lm(glyhb_tr~age+ratio+frame+age:frame)
summary(mod5)
AIC(mod4)
BIC(mod4)

#Drop id, bp.2s, bp.2d from data
drops=c("id","bp.2s","bp.2d")
diab<-diab[,!(names(diab)%in%drops)]

#glyhb values are the inverse of the function
diab$glyhb=(1/glyhb)

#Drop cases having NA's
index.na=apply(is.na(diab),1,any)
diab_2=diab[index.na==FALSE,]
any(is.na(diab_2))
table(diab_2$frame)

#17. Draw scatterplot matrix and obtain the pairwise correlation matrix for all quantitative variables in the data.
#Comment on their relationships

#drop the factor variables
diabq=diab_2[,!(sapply(diab_2,class)%in%'factor')]

#compute correlation matrix
cor(diabq)


#Draw scatterplot matrix
plot(diab_2)

mod6<-lm(glyhb~., data=diab_2)
summary(mod6)

#install.packages(leaps)
library(leaps)
subset=regsubsets(glyhb~.,data=diab_2, nbest=1, nvmax=16)
sumsub=summary(subset)
sumsub$rsq
sumsub$adjr2

## BIC
## sample size 
n=nrow(diab_2) 
## number of coefficients in each model: p
p.m=as.integer(as.numeric(rownames(sumsub$which))+1) 
bic = n*log(sumsub$rss/n) + (log(n))*p.m

## AIC 
aic = n*log(sumsub$rss/n) + 2*p.m

## table of all criteria values
sumtable = cbind(sumsub$which,sumsub$rsq,sumsub$adjr2,bic,aic)
colnames(sumtable)=c(colnames(sumsub$which), "R^2", "R^2_a", "BIC", "AIC")
round(sumtable,3) ## round the results to three decimals

## get the index for the best model within each size group (the first one in the group)
index.best=c(which.max(sumsub$rsq),which.max(sumsub$adjr2),which.min(bic),which.min(aic))



#stepwise procedure-forward selection
##### Forward selection  ###################
fit = lm(glyhb~1, data=diab_2)  ## initial model: none-model with only intercept term
fita = lm(glyhb~., data=diab_2)
step = stepAIC(fit,scope=list(upper=formula(fita), lower=~1), direction="forward", k=2)

##### Forward selection with interactions ##########
fit.0 =lm(glyhb~1, data=diab_2) ## none-model without X variable
fit.2 = lm(glyhb~.^2, data=diab_2)  ## full model with all 2-way interactions
step = stepAIC(fit.0,scope=list(upper=formula(fit.2), lower=~1),direction="forward", k=2)



