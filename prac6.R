# new 
house<-read.csv(file.choose(),sep=",",header = T)
summary (house)

# new
names(house)

# new
pairs(~death_rate+doctor_avail+hosp_avail+annual_income+density_per_capita,data = house)

# 
housemodel <- lm(density_per_capita~death_rate+doctor_avail+hosp_avail+annual_income,data=house)
housemodel
# 
index<-read.csv(file.choose(),sep=",",header = T)
names(index)

#
pairs(~index+written+language+tech+gk,data = index)

#
model1<-lm(index~.,data = index)
summary(model1)

#
index$pred<-fitted(model1)
head(index)

#
index$res<-residuals(model1)
head(index)

#
# install.packages("car")
library(car)
vif(model1)

#
plot(index$pred,index$res,col="red")

#
shapiro.test(index$res)

#
ncvTest(model1,~written+language+tech+ gk)

#
durbinWatsonTest(model1)

#
influencePlot(model1)

#
library(caret)
library(lattice)
library(ggplot2)
index<-read.csv(file.choose(),sep=",",header = T)
summary(index)

#
data<-createDataPartition(index$empid,p=0.8,list=F)
head(data)

#
dim(data)

#
traindata<-index[data,]
dim(traindata)

#
testdata<-index[-data,]
dim(testdata)

#
names(traindata)

#
modeltrain<-lm(index~written+language+tech+gk,data=traindata)
modeltrain$res<-residuals(modeltrain)
RMSEtrain<-sqrt(mean(modeltrain$res**2))
RMSEtrain

#
testdata$pred<-predict(modeltrain,testdata)
testdata$res<-testdata$index-testdata$pred
RMSEtest<-sqrt(mean(testdata$res**2))
RMSEtest

#
kfolds<-trainControl(method = "cv",number = 4)
modelkfold<-train(index~written+language+tech+gk,data = index,method="lm",trControl=kfolds)
modelkfold

#
kfoldsrp<-trainControl(method = "repeatedcv",number = 4,repeats = 5)
modelkfoldsrp<-train(index~written+language+tech+gk,data = index,method="lm",trControl=kfoldsrp)
modelkfoldsrp

#
kfoldsloocv<-trainControl(method = "LOOCV")
kfoldsloocvmodel<-train(index~written+language+tech+gk,data = index,method="lm",trControl=kfoldsloocv)
kfoldsloocvmodel

#
null<-lm(index~1,data=index)
full<-lm(index~.,data = index)
names(index)

#
step(null,scope = list(lower=null,upper=full),direction = "forward")

#
step(full,scope=list(lower=null,upper=full),direction = "backward")
