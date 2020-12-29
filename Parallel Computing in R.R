####HOW TO DO PARALLEL COMPUTING ON R
library(parallel)

#Check parallel features
parallel::
  
#Linear Regression with Normal Eq
#Goal is to find the variabilty in estimates of B
  
b0=1
b1=5
siggma-10
n=10000L
set.seed(37)
x=runif(n)

#epsilon=sigma*rnorm(n)

#But if the errors are not normal then:
epsilon=sigma*rt(n,3)

y=b0+b1*x*epsilon
xy=data.frame(x=x,y=y)

#if we assume that the errors are normally distributed then we can use a t distribution to calculate exact
#analytic c.I for B0

fit=lm(y~x,data=xy)
#if variance is high then the points will be spread along the horizontal line
#plot(fit)


### R BOOTSTRAP#####
#IDEA IS TO TAKE A SAMPLE N WITH REPLACEMENT AND COMPUTE THE FITTED COEFFICIENTS THEN REPEAT 1000 TIMES.

bootbeta=function(...)
{ 
  n=nrow(xy)
  xy_boot=xy[sample.int(n, replace=T), ]
  fit_boot=lm(y~x,data=xy_boot)
  coef(fit_boot)
}

#Test it
bootbeta()

system.time(betas<-lapply(1:1000, bootbeta))
beta_todf=function(betas){
  betas=simplify2array()
  
}
