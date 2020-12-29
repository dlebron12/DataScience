#### Insurance firm data ###########################################################################
data=read.table("insurance.txt",header=FALSE)
names(data)=c("Y","X1","X2")
fit1=lm(Y~ X1+factor(X2), data=data)
summary(fit1)

### bar chart
barplot(table(data$X2),col=rainbow(4),main='Barplot example')
### pie chart with percentages
lbls=c('Mutual','Stock')
n=dim(data)[1]
pct=round(100*table(data$X2)/n)
lab=paste(lbls,pct)
lab=paste(lab,'%',sep='')
pie(table(data$X2),labels=lab,col=c('blue','red','green','yellow'),
    main='Pie chart example')
## Boxplot
boxplot(data$Y~data$X2,main='Multiple boxplots',
        xlab='Firm type',ylab='Firm size',col=rainbow(4))

### model with interaction between quantitative and categorical variables
fit2 = lm(Y~ X1+factor(X2)+X1:factor(X2), data=data)

###################################### Real Estate model data ####################################### 
temp=read.table("realestate.txt")

### reformat data: take V2: sales price, V3: square feet, V10: quality and convert it into ### strings

data=temp[,c("V2", "V3", "V10")]
data[data[,3]=="1", 3]<-"high"
data[data[,3]=="2", 3]<-"medium"
data[data[,3]=="3", 3]<-"low"

data=data.frame(data)
names(data)=c("sales", "Sq", "quality")

### eploring the data set #############################
table(data[,"quality"]) ## frequency of different classes of "quality" variable
### bar chart
barplot(table(data$quality),col=rainbow(4),main='Barplot example')
### pie chart with percentages
lbls=c('High','Low','Medium')
n=dim(data)[1]
pct=round(100*table(data$quality)/n)
lab=paste(lbls,pct)
lab=paste(lab,'%',sep='')
pie(table(data$quality),labels=lab,col=c('blue','red','green','yellow'),
    main='Pie chart example')
## boxplot
boxplot(data$sales~data$quality,main='Multiple boxplots',
        xlab='Construction quality',ylab='Sales price',col=rainbow(4))

fit3=lm(sales~Sq+factor(quality), data=data)
summary(fit3)

