#pg 724 #16.9 Rehabilitation Therapy, ANOVA test

n1 = 8
n2 = 10
n3 = 6
n = c(n1, n2, n3)
r = length(n)
nT = sum(n)
physical_fitness = c(29, 42, 38, 40, 43, 40, 30, 42, 30, 35, 39, 28, 31, 31, 29, 35, 29, 33, 26, 32, 21, 20, 23, 22)
group = rep(c(1,2,3), times = c(n1, n2, n3))
data16.9 = as.data.frame(cbind(physical_fitness, group))
names(data16.9) = c("pf", "grp")
data16.9[,2] = as.factor(data16.9$grp)
model = aov(pf ~ grp, data = data16.9)
anova(model) #or summary(model)
 

group_mean = tapply(physical_fitness, as.factor(group), mean)
#wish to make a column of group means
agg_mean = aggregate(data16.9$pf, by = list(grp = data16.9$grp), FUN = mean)
data16.9 = merge(data16.9,agg_mean)

overal_mean = mean(physical_fitness)

#SSTR
SSTR=0
for (i in 1:3){
  SSTR = SSTR + n[i]*(group_mean[i]-overal_mean)^2
}
MSTR = SSTR/(r-1)

#SSE
SSE = sum((data16.9[,2]-data16.9[,3])^2)
MSE = SSE/(nT-r)

Fstat = MSTR/MSE

#critical value comparison
Fstat >  qf(0.95, r-1, nT-r) 
#if true, reject!!

#p value and alpha comparison
pvalue = 1-pf(Fstat, r-1, nT-r)
