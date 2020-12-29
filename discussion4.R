#discussion 4 

#example #16.9 Rehabilitation Therapy

#(a) dot chart
data16.9y = c(29, 42, 38, 40, 43, 40, 30, 42, 30, 35, 39, 28, 31, 31, 29, 35, 29, 33, 26, 32, 21, 20, 23, 22)
data16.9x = rep(c(1,2,3), times = c(8, 10, 6))
data.color = 0
data.color[data16.9x==1] = "red"
data.color[data16.9x==2] = "blue"
data.color[data16.9x==3] = "darkgreen"
dotchart(data16.9y,labels = data16.9x ,cex=.7,groups= data16.9x,
         main="days for completion",
         xlab="days", gcolor="black", color=data.color)

#(b) fitted values

fit = 0
for (i in 1:3) fit[i] = mean(data16.9y[data16.9x == i])

#(c) residuals (sum to zero?)

meanvec = rep(fit, times = c(8,10,6))
res = data16.9y - meanvec
tapply(res, data16.9x, sum)

#(d) ANOVA table?

mod1 = lm(data16.9y~as.factor(data16.9x))
anova(mod1)

#(e)
#read anova table!
#alternative?
#decision rule?
#conclusion?

#(f)
#f statistic? 
fstat = (672/2)/(416/21)
#p-value? 
1-pf(fstat, 2, 21)
#critical value?
qf(0.95, 2, 21)

#power calculation?
r = 3
trueSSTR = 672
phi = (1/sig)*sqrt(SSTR/r)
1-pf(qf(0.95, 2, 21), 2, 21, r*phi^2)
