
install.packages('party')
install.packages('ROCR')




#q1
library(ISLR)
head(default)
tail(default)
def<-Default    
def.test<-def[-sample(10000,8000),]
def.train<-def[sample(10000,8000),]
def.lr<-glm(formula=default~student+balance+income, family=binomial(link="logit"), data=def.train)
summary(def.lr)
pred<-predict(def.lr,newdata = def.test,type="response")
pred<-ifelse(pred>.5,"Yes","No")
confusionMatrix(def.test$default,pred)

# And then a lift chart
perf <- performance(pred,"lift","rpp")
plot(perf, main="lift curve", colorize=T)



#naive
library(caret)
model<-naiveBayes(formula=default~.,data=def.train)
pred.nb<-predict(model,def.test)
head(pred.nb)
confusionMatrix(def.test$default,pred.nb)

#q2
wine<-read.csv(file.choose())
head(wine)
table(wine$Class)
wine$Class<-as.factor(wine$Class)
y<-wine$class
x<-wine[,-14]
library(e1071)
classifier <- naiveBayes(Class ~ ., data = wine)
summary(classifier)
pred = predict(classifier, x )
table(pred ,wine$Class)
pred
wine.train<-wine[sample(nrow(wine),nrow(wine)/2),]
wine.test<-wine[-sample(nrow(wine),nrow(wine)/2),] 
classifier1<-naiveBayes(Class~.,data=wine.train)
pred.classifier<-predict(classifier1,wine.test)
table(pred.classifier,wine.test$Class)

#q3
library("e1071")
library("caret")
library ("klaR")
digits<-read.csv(file.choose())
digits$OptDigit<-as.factor(digits$OptDigit)


#After 50-50% Split
for(i in 1:10){
  shuffled<- digits[sample(nrow(digits),5620),]
  train_shuffled<-shuffled[1:2180,]
  test_shuffled<-shuffled[2181:5620,]
  head(train_shuffled)
  digit_train_model<-naiveBayes(OptDigit~.,data=train_shuffled)
  predictor<-predict(digit_train_model,test_shuffled)
  varx <- confusionMatrix(test_shuffled$OptDigit,predictor)
  print(varx$overall["Accuracy"])
}

#After Split- 80% training and 20% test data

for(i in 1:10){
  shuffled<- digits[sample(nrow(digits),5620),]
  train_shuffled<-shuffled[1:4496,]
  test_shuffled<-shuffled[4497:5620,]
  head(train_shuffled)
  digit_train_model<-naiveBayes(OptDigit~.,data=train_digits)
  predictor<-predict(digit_train_model,test_shuffled)
  vary <-confusionMatrix(test_shuffled$OptDigit,predictor)
  print(vary$overall["Accuracy"])
}

#bootstrapping

' Read in SP500 weekly data'
SP500 = read.csv(file.choose())

'Create price change column'
SP500$Change = (SP500$Close - SP500$Open)/SP500$Open
PickRow = sample(1:nrow(SP500),1)
PickRow
PickRate = SP500$Change[PickRow]
PickRate

bAmt = 2550
iRate = .05/52
eAmt = bAmt * (1+iRate)
investment = data.frame(week=1, bAmt = bAmt, iRate = iRate, eAmt = eAmt )
investment

for (w in c(2:260)) {
  bAmt = eAmt
  iRate = 0.05/52
  eAmt = bAmt * (1+iRate)
  investment = rbind(investment,data.frame(week=w, bAmt = bAmt, iRate=iRate,eAmt = eAmt))
}
eAmt

getSampleReturn = function() {
  PickRow = sample(1:nrow(SP500),1)
  SP500$Change[PickRow]
}

bAmt = 1000
iRate = getSampleReturn()
eAmt = bAmt * (1+iRate)
investment = rbind(investment,data.frame(week=w, bAmt = bAmt, iRate=iRate,eAmt = eAmt))

for (w in c(2:260)) {
  bAmt = eAmt
  iRate = getSampleReturn()
  eAmt = bAmt * (1+iRate)
  investment = rbind(investment,data.frame(week=w, bAmt = bAmt, iRate=iRate,eAmt = eAmt))
}
eAmt

#mul investments
' Read in SP500 weekly data'
SP500 = read.csv(file.choose())
' Create price change column'
SP500$Change = (SP500$Close - SP500$Open)/SP500$Open
PickRow = sample(1:nrow(SP500),1)
PickRow
PickRate = SP500$Change[PickRow]
PickRate

getSampleReturn = function() {
  PickRow = sample(1:nrow(SP500),1)
  SP500$Change[PickRow]
}


bAmt = 1000
iRate = getSampleReturn()
eAmt = bAmt * (1+iRate)
investment = data.frame(week=1, bAmt = bAmt, iRate = iRate, eAmt = eAmt )

for (w in c(2:260)) {
  bAmt = eAmt
  iRate = getSampleReturn()
  eAmt = bAmt * (1+iRate)
  investment = rbind(investment,data.frame(week=w, bAmt = bAmt, iRate=iRate,eAmt = eAmt))
}
eAmt


library("e1071")
library("klaR")
library("caret")
library(ggplot2)
library(rpart)

churn = read.csv(file.choose())
set.seed(1234)
churn = churn[,-12]
head(churn)
#Randomly shuffle the data
churn<-churn[sample(nrow(churn)),]
churn$Retained = as.factor(churn$Retained)
head(churn)

#Create 10 equally size folds
folds <- cut(seq(1,nrow(churn)),breaks=10,labels=FALSE)
head(folds)
tail(folds)
BayesoutputData = 0
LogoutputData=0
rpartoutputData=0
gbmoutputData=0


#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- churn[testIndexes, ]
  trainData <- churn[-testIndexes, ]
  
  #test bayes
  classifier = NaiveBayes(Retained ~ ., data=trainData)
  pred = predict(classifier, testData)
  misClassifyError = mean(pred$class != testData$Retained)
  Accuracy = 1-misClassifyError
  Accuracy
  BayesoutputData[i] = Accuracy
  
  #test glm
  classifier=glm(Retained~., data=trainData, family="binomial"(link='logit'))
  pred=predict(classifier,newdata=testData,type="response")
  pred<-ifelse(pred > 0.5,1,0)
  misClassifyError = mean(pred != testData$Retained)
  Accuracy1 = 1-misClassifyError
  LogoutputData[i] = Accuracy1
  
  
  #test rpart
  train.ct<-rpart(trainData$Retained ~ ., data=trainData[,1:12], cp=0)
  Prediction <- predict(train.ct, newdata=testData, type='class')
  head(Prediction)
  
  misClassifyError <- mean(Prediction != testData$Retained)
  Accuracy2 = 1-misClassifyError
  #Accuracy
  rpartoutputData[i] = Accuracy2
  
  
  #test gbm
  library(gbm)
  classifier <- gbm(Retained ~ ., distribution="multinomial", data=trainData, n.trees=200, interaction.depth=6, shrinkage=0.01)
  pred = predict(classifier, newdata=testData, n.trees=200, type="response")
  pred[1,,]
  
  PredictionClass = ifelse(pred[,1,]>0.5,0,0)
  PredictionClass = ifelse(pred[,2,]>0.5,1,PredictionClass)
  misClassifyError = mean(PredictionClass != testData$Retained)
  Accuracy3 = 1-misClassifyError
  gbmoutputData[i] = Accuracy3
  
}
head(BayesoutputData,10)
head(LogoutputData,10)
head(rpartoutputData,10)
head(gbmoutputData,10)


summary(BayesoutputData)
summary(LogoutputData)
summary(rpartoutputData)
summary(gbmoutputData)

#Read in SP500 weekly data
SP500 = read.csv(file.choose())
#Create price change column
vector1<-c()

SP500$Change = (SP500$Close - SP500$Open)/SP500$Open
getSampleReturn = function() {
  PickRow = sample(1:nrow(SP500),1)
  SP500$Change[PickRow]
}

for( simulation in c(1:1000))
{
  bAmt = 1000
  iRate = getSampleReturn()
  eAmt = bAmt * (1+iRate)
  load_weekly=eAmt*(0.15/52)
  mybal=eAmt-load_weekly
  company_bal=load_weekly
  
  investment = data.frame(week=1, bAmt = bAmt, iRate = iRate, eAmt = eAmt,load_weekly=load_weekly,mybal=mybal,company_bal=company_bal )
  print(investment)
  
  
  for (w in c(2:1560)) {
    bAmt = eAmt
    iRate = getSampleReturn()
    eAmt = bAmt * (1+iRate)
    load_1=company_bal
    load_weekly=eAmt*(0.15/52)
    mybal=eAmt-load_weekly
    company_bal=load_weekly+(1+iRate)*load_1
    investment = data.frame(week=w, bAmt = bAmt, iRate = iRate, eAmt = eAmt,load_weekly=load_weekly,mybal=mybal,company_bal=company_bal )
    print(investment)
  }
  eAmt = rbind(eAmt, data.frame(mybalance=mybal,companybalance=company_bal))
}
# Create confidence intervals
c(mean(eAmt$mybalance) - 1.96*sd(eAmt$mybalance)/sqrt(100), mean(eAmt$mybalance), mean(eAmt$mybalance) + 1.96*sd(eAmt$mybalance)/sqrt(100))
c(mean(eAmt$companybalance) - 1.96*sd(eAmt$companybalance)/sqrt(100), mean(eAmt$companybalance), mean(eAmt$companybalance) + 1.96*sd(eAmt$companybalance)/sqrt(100))


#q2conway
#Read in SP500 weekly data
SP500 = read.csv(file.choose())
#Create price change column
vector1<-c()

SP500$Change = (SP500$Close - SP500$Open)/SP500$Open
getSampleReturn = function() {
  PickRow = sample(1:nrow(SP500),1)
  SP500$Change[PickRow]
}

for( simulation in c(1:100))
{
  bAmt = 100
  iRate = getSampleReturn()
  eAmt = bAmt * (1+iRate)
  load_weekly=eAmt*(0.15/52)
  mybal=eAmt-load_weekly
  company_bal=load_weekly
  
  investment = data.frame(week=1, bAmt = bAmt, iRate = iRate, eAmt = eAmt,load_weekly=load_weekly,mybal=mybal,company_bal=company_bal )
  print(investment)
  
  
  for (w in c(2:1560)) {
    bAmt = eAmt+1 00
    iRate = getSampleReturn()
    eAmt = bAmt * (1+iRate)
    load_1=company_bal
    load_weekly=eAmt*(0.15/52)
    mybal=eAmt-load_weekly
    company_bal=load_weekly+(1+iRate)*load_1
    investment = data.frame(week=w, bAmt = bAmt, iRate = iRate, eAmt = eAmt,load_weekly=load_weekly,mybal=mybal,company_bal=company_bal )
    print(investment)
  }
  eAmt = rbind(eAmt, data.frame(mybalance=mybal,companybalance=company_bal))
}
# Create confidence intervals
c(mean(eAmt$mybalance) - 1.96*sd(eAmt$mybalance)/sqrt(100), mean(eAmt$mybalance), mean(eAmt$mybalance) + 1.96*sd(eAmt$mybalance)/sqrt(100))
c(mean(eAmt$companybalance) - 1.96*sd(eAmt$companybalance)/sqrt(100), mean(eAmt$companybalance), mean(eAmt$companybalance) + 1.96*sd(eAmt$companybalance)/sqrt(100))

#q4
#Lasso Regression
library("glmnet")
library("lars")
library("PerformanceAnalytics")
library("MASS")

C_balance <-read.csv(file.choose())
C_balance <- C_balance[,c(-1)]
head(C_balance)

C_balance=scale(C_balance)
head(C_balance)
C_balance=data.frame(C_balance)
input <- as.matrix(C_balance[,-11])
output<-as.matrix(C_balance[,11])


head(input)
head(output)

#Lasso
fit_lasso<-lars(x=input,y=output,type="lasso")
coef.lars(fit_lasso,s=seq(from=0,to=100,by=1),mode='lambda')
coef.lars(fit_lasso,s=100,mode='lambda')
coef.lars(fit_lasso,s=500,mode='lambda')
coef.lars(fit_lasso,s=1000,mode='lambda')
coef.lars(fit_lasso)

#q6
library(e1071)

wine <- read.csv(file.choose())
wine$Qcat<-ifelse(wine$quality>5,1,0)
head(wine)
wine$Qcat <- as.factor(wine$Qcat)
str(wineData)
wineData<-wine[,-12]

wineTrain<-wineData[sample(nrow(wineData),nrow(wineData)/2),]
wineTest<-wineData[-sample(nrow(wineData),nrow(wineData)/2),]

#Do naive bayes classifier with train data
wineclassifier <- naiveBayes(Qcat ~ ., data = wineTrain)
summary(wineclassifier)
winepred = predict(wineclassifier, wineTest )
table(winepred, wineTest$Qcat)

#do logistic regression
wineclassifier.lrm <- glm(Qcat ~ ., data = wineTrain,family = binomial(link = "logit"))
summary(wineclassifier)
#Do prediction using logistic regression model
predictResult <- predict(wineclassifier.lrm, newdata = wineTest,type="response")

#Build a confusion matrix for the prediction using logistic regression
predictResult<-ifelse(predictResult>0,1,0)
LRconfMat <- confusionMatrix(wineTest$Qcat,predictResult)
LRconfMat




#q2
SP500 = read.csv(file.choose())
#Create price change column
vector1<-c()

SP500$Change = (SP500$Close - SP500$Open)/SP500$Open
getSampleReturn = function() {
  PickRow = sample(1:nrow(SP500),1)
  SP500$Change[PickRow]
}

finale<-data.frame(mine=0,companys=0)
for( sim in c(1:100))
 { bAmt = 200
  iRate = getSampleReturn()
  eAmt = bAmt * (1+iRate)
  load_weekly=eAmt*(0.015/52)
  mybal=eAmt-load_weekly
  company_bal=load_weekly
  
  inv = data.frame(week=1, bAmt = bAmt, iRate = iRate, eAmt = eAmt,load_weekly=load_weekly,mybal=mybal,company_bal=company_bal )
  print(inv)
  
  
  for (w in c(2:1560))
    {
    bAmt = eAmt+ 200
    iRate = getSampleReturn()
    eAmt = bAmt * (1+iRate)
    
    load_weekly=eAmt*(0.015/52)
    mybal=eAmt-load_weekly
    company_bal=load_weekly+company_bal
    investment = data.frame(week=w, bAmt = bAmt, iRate = iRate, eAmt = eAmt,load_weekly=load_weekly,mybal=mybal,company_bal=company_bal )
    print(investment)
  }
  
finale<-rbind(finale,data.frame(mine=mybal,companys=company_bal))
}




#Create 10 equally size folds
folds <- cut(seq(1,nrow(churn)),breaks=10,labels=FALSE)
head(folds)
tail(folds)
BayesoutputData = 0
LogoutputData=0
rpartoutputData=0
gbmoutputData=0


#q3Perform 10 fold cross validation
for(i in 1:7){
  #Segement your data by fold using the which() function
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- churn[testIndexes, ]
  trainData <- churn[-testIndexes, ]
  
  #test bayes
  classifier = NaiveBayes(Retained ~ ., data=trainData)
  pred = predict(classifier, testData)
  misClassifyError = mean(pred$class != testData$Retained)
  Accuracy = 1-misClassifyError
  Accuracy
  BayesoutputData[i] = Accuracy
  
  #test glm
  classifier=glm(Retained~., data=trainData, family="binomial"(link='logit'))
  pred=predict(classifier,newdata=testData,type="response")
  pred<-ifelse(pred > 0.5,1,0)
  misClassifyError = mean(pred != testData$Retained)
  Accuracy1 = 1-misClassifyError
  LogoutputData[i] = Accuracy1
  
  
  #test rpart
  train.ct<-rpart(trainData$Retained ~ ., data=trainData[,1:12], cp=0)
  Prediction <- predict(train.ct, newdata=testData, type='class')
  head(Prediction)
  
  misClassifyError <- mean(Prediction != testData$Retained)
  Accuracy2 = 1-misClassifyError
  #Accuracy
  rpartoutputData[i] = Accuracy2
  
  
  #test gbm
  library(gbm)
  classifier <- gbm(Retained ~ ., distribution="multinomial", data=trainData, n.trees=200, interaction.depth=6, shrinkage=0.01)
  pred = predict(classifier, newdata=testData, n.trees=200, type="response")
  pred[1,,]
  
  PredictionClass = ifelse(pred[,1,]>0.5,0,0)
  PredictionClass = ifelse(pred[,2,]>0.5,1,PredictionClass)
  misClassifyError = mean(PredictionClass != testData$Retained)
  Accuracy3 = 1-misClassifyError
  gbmoutputData[i] = Accuracy3
  
}


library(TSP)
library(tspmeta)

coords.df<- data.frame(long=runif(40,min=0,max=100),lat=runif(40,min=0,max=100))
coords.mx<- as.matrix(coords.df)

#compute distance matrix
dist.mx<-dist(coords.mx)

#construct a TSP object
tsp.ins<-tsp_instance(coords.mx,dist.mx)

tour<-run_solver(tsp.ins,method="2-opt")
as.integer(tour)

#Plot
autoplot(tsp.ins,tour)


library(zipcode)
data(zipcode)
head(zipcode)
MedicareData=read.csv(file.choose())
MedicareData$ZIP=clean.zipcodes(MedicareData$ZIP)
MedData=merge(MedicareData,zipccode,by.x='ZIP',by.y='zip')
head(MedData)

MedFla=subset(MedData,STATE=="FL")
head(MedFla)
MedFlaK=kmeans(data.frame(MedFla$latitude,MedFla$latitude),15)
MedFlaK$cluster
MedFla$cluster=MedFlaK$cluster

ResultSet=NULL
for(i in c(1:15)){
  #i=1
  route=subset(MedFla,cluster==1)
  nrow(route)
  coords.df<-data.frame(long=route$longitude,lat=route$latitude)
  coords.mx<-as.matrix(coords.df)
  dist.mx<-dist(coords.mx)
  tsp.ins<-tsp_instance(coords.mx,dist.mx)
  tour<-run_solver(tsp.ins,method="2-opt")
  route$tour=as.integer(tour)
  route
  ResultSet<-rbind(ResultSet,route)
  #autoplot(tsp.ins,tour)
}
#write.csv(ResultSet,MedicareFloridaResults.csv")


#DV-asssignment

#naive
library(caret)
model<-naiveBayes(formula=default~.,data=def.train)
pred.nb<-predict(model,def.test)
head(pred.nb)
confusionMatrix(def.test$default,pred.nb)

