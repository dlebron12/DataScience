setwd("~/Documents/UCD/2016fall/STA106Gupta/discussion/")
data16 = read.table("CH19PR16.txt")
names(data16) = c("Y", "A", "B", "id")
two_way = aov(Y~as.factor(A)+as.factor(B), data = data16)
summary(two_way)

two_way_inter = aov(Y~as.factor(A)+as.factor(B) + as.factor(A)*as.factor(B), data = data16)
two_way_inter = aov(Y~as.factor(A)*as.factor(B), data = data16)#equivalent model
summary(two_way_inter)
tw_means = matrix(two_way_inter$fitted.values[(1:9)*5], nrow = 3, byrow = TRUE)
plot(tw_means[1,], type = "l", ylim = c(min(tw_means), max(tw_means)), main = "Interaction Plot", xlab = "factor B", ylab = "means", col = 1)
lines(tw_means[2,], col = 2)
lines(tw_means[3,], col = 3)
legend("topright", legend = c("A=1", "A=2", "A=3"), lty = 1,col = 1:3)
dev.off()

plot(tw_means[,1], type = "l", ylim = c(min(tw_means), max(tw_means)), main = "Interaction Plot", xlab = "factor A", ylab = "means", col = 1)
lines(tw_means[,2], col = 2)
lines(tw_means[,3], col = 3)
legend("topright", legend = c("B=1", "B=2", "B=3"), lty = 1,col = 1:3)
dev.off()


#factor effects, interaction effects
m = rep(mean(data16$Y), 3)
m_i = apply(tw_means, 1, mean)
m_j = apply(tw_means, 2, mean)
A_effects = m_i - m
B_effects = m_j - m
inter_effects = matrix(0, nrow = 3, ncol = 3)
for (i in 1:3){
  for (j in 1:3){
    inter_effects[i,j] = tw_means[i,j] - m[1] - A_effects[i] - B_effects[j]
  }
}
