#Contrasts and CI's 

setwd("/Users/Irene/Documents/UCD/2016fall/STA106Gupta/discussion/")
data = read.csv("iris.data")
model = aov(data[,1]~as.factor(data[,5]))
anova(model)
mse = 0.2668
df = 146

#CI of m1

#point estimate of m1 
tapply(data[,1], data[,5], mean)
ybar1 = 5.004082
table(data[,5])
n1 = 49
alpha = 0.05
s_ybar1 = sqrt(mse/n1)

CIm1 = c( ybar1 - s_ybar1* qt(1-alpha/2, 146), ybar1 + s_ybar1* qt(1-alpha/2, 146) )


#CI of a contrast m1-m2
alpha = 0.05
ybar1 = 5.004082
ybar2 = 5.936000
n1 = 49
n2 = 50
c1 = 1
c2 = -1
s_ybar12 = sqrt(mse*((c1^2)/n1 + (c2^2)/n2))

CIm1m2 = c( (ybar1-ybar2) - s_ybar12*qt(1-alpha/2, df), (ybar1-ybar2) + s_ybar12*qt(1-alpha/2, df) ) 

#Bonferroni for two contrasts
#contrast1 m1-m2
#contrast2 m2-m3
alpha = 0.05
g = 2 # number of contrasts
#ybar1-ybar2 = 5.004082-5.936000
#ybar2-ybar3 = 5.936000-6.588000 
ybar3 = 6.588000
n3 = 50
s_ybar12 = sqrt(mse*((c1^2)/n1 + (c2^2)/n2))
s_ybar23 = sqrt(mse*((c1^2)/n2 + (c2^2)/n3))

CIm1m2B = c( (ybar1-ybar2) - s_ybar12*qt(1-alpha/(2*g), df), (ybar1-ybar2) + s_ybar12*qt(1-alpha/(2*g), df) ) 
CIm2m3B = c( (ybar2-ybar3) - s_ybar23*qt(1-alpha/(2*g), df), (ybar2-ybar3) + s_ybar23*qt(1-alpha/(2*g), df) ) 

#alternate testing method for all 3 pairwise comparison choosing bonferroni
pairwise.t.test(data[,1], data[,5], p.adj = "bonf")
#shown are p values


#Tukey for two pairwise difference
#pair diff1 m1-m2
#pair diff2 m2-m3
alpha = 0.05
#ybar1-ybar2
#ybar2-ybar3
r = 3
nT = n1+n2+n3
c3 = 1
s_ybar12 = sqrt(mse*((c1^2)/n1 + (c2^2)/n2))
s_ybar23 = sqrt(mse*((c1^2)/n2 + (c2^2)/n3))
s_ybar13 = sqrt(mse*((c1^2)/n1 + (c3^2)/n3))
df

CIm1m2T = c( (ybar1-ybar2) - s_ybar12*(1/sqrt(2))*qtukey(1-alpha, r, df), 
             (ybar1-ybar2) + s_ybar12*(1/sqrt(2))*qtukey(1-alpha, r, df) ) 
CIm2m1T = c( (ybar2-ybar1) - s_ybar12*(1/sqrt(2))*qtukey(1-alpha, r, df), 
             (ybar2-ybar1) + s_ybar12*(1/sqrt(2))*qtukey(1-alpha, r, df) ) 
CIm3m1T = c( (ybar3-ybar1) - s_ybar13*(1/sqrt(2))*qtukey(1-alpha, r, df), 
             (ybar3-ybar1) + s_ybar13*(1/sqrt(2))*qtukey(1-alpha, r, df) ) 

CIm3m2T = c( (ybar3-ybar2) - s_ybar23*(1/sqrt(2))*qtukey(1-alpha, r, df), 
             (ybar3-ybar2) + s_ybar23*(1/sqrt(2))*qtukey(1-alpha, r, df) ) 
#alternate method
TukeyHSD(model)
plot(TukeyHSD(model), las=1)

#Scheffe
#for any list of contrasts...
#m1-m2
#m2-m3
#m1/3 + m2/3 - 2*m3/3
...

#get a point estimate
r 
nT
alpha
c1 = 1/3
c2 = 1/3
c3 = -2/3
#ybar1/3 + ybar2/3 - 2*ybar3/3 = 5.004082/3+5.936000/3-2*6.588000/3
sd = sqrt(mse*((c1^2)/n1 + (c2^2)/n2 + (c3^2)/n3))

CIS = (ybar1/3 + ybar2/3 - 2*ybar3/3) -  sd*sqrt((r-1)*qf(1-alpha, r-1, nT-r))

