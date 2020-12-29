setwd("~/Documents/UCD/2016fall/STA106Gupta/discussion/")
data16 = read.table("CH19PR16.txt")
data5 = read.table("CH19PR05.txt")
data12 = read.table("CH19PR12.txt")


#5
names(data5) = c("Mean", "A", "B")
#a. Factor B main effects?
factorB.mean = aggregate(Mean ~ B,data5, mean)
overall.mean = mean(data5$Mean)
factorB.mean$Mean - overall.mean

#b. Interaction Plot
dev.off()
par(mfrow = c(1,2))
interaction.plot(x.factor=data5$B, trace.factor = data5$A, response = data5$Mean, fun = mean, 
                 xlab = "Factor B", ylab = "level means", main = "Interaction Plot 1")
interaction.plot(x.factor=data5$A, trace.factor = data5$B, response = data5$Mean, fun = mean, 
                 xlab = "Factor B", ylab = "level means", main = "Interaction Plot 2")
#c. 
dev.off()
par(mfrow = c(1,2))
interaction.plot(x.factor=data5$B, trace.factor = data5$A, response = log(data5$Mean), fun = mean, 
                 xlab = "Factor B", ylab = "level means", main = "Interaction Plot 1")
interaction.plot(x.factor=data5$A, trace.factor = data5$B, response = log(data5$Mean), fun = mean, 
                 xlab = "Factor B", ylab = "level means", main = "Interaction Plot 2")

#8 Assume sigma = 4 n = 6
sig = 4
n = 6
#a. E{MSE} and E{MSAB}?
factorB.mean = aggregate(Mean ~ B,data5, mean)
data5 = merge(factorB.mean, data5, "B")
factorA.mean = aggregate(Mean.y ~ A, data5, mean)
data5 = merge(factorA.mean, data5, "A")
overall = rep(mean(data5$Mean.y.y), length(data5$Mean.y.y))
data5 = cbind(data5, overall)
abij = data5[,5]-data5[,2]-data5[,4] + data5[,6]
E_MSE = sig^2
E_MSAB = sig^2 + 6*sum((abij)^2)/((2-1)*(4-1))

#b. E_MSAB is greatly larger than E_MSE : F test might reject

#12
names(data12) = c("assessment", "A", "B", "index")
#a. fitted values for model 19.23
model19.23 = aov(assessment ~ A + B + A*B, data = data12)
model19.23$fitted.values
#b. residuals: Do they sum to zero for each treatment?
res = model19.23$residuals
data12 = cbind(data12, res)
aggregate(res ~ A + B,data12, sum)
#c. aligned residual dot plots
par(mfrow = c(1,2))
plot(res~A+B, data12, main = "residual dot plot")
#check if error variance is similar
#d. Normal probability plot of the residuals
dev.off()
qqs = qqnorm(res)
qqline(res)
cor(qqs$x, qqs$y)

#e. residual sequence plot
par(mfrow = c(2,2))
for (i in 1:2){
  for(j in 1:2){
    plot(model19.23$residuals[data12$A == i & data12$B == j], 
         type = "l", ylab = "res", xlab = "index", main = paste(as.character(i), as.character(j), "group"))
  }
}

#errors random??

#13
#a.
dev.off()
par(mfrow = c(1,2))
interaction.plot(x.factor = data12$A, trace.factor = data12$B, response = data12$assessment, xlab = "A", ylab = "")
interaction.plot(x.factor = data12$B, trace.factor = data12$A, response = data12$assessment, xlab = "B", ylab = "")

#b. ANOVA table
anova(model19.23)
#c. test interaction effect
#d. test main effects
#e. 