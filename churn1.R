rm(list = ls())

# reading the data
path = 'D:/customer churn/'
train = read.csv('D:/customer churn/Train_data.csv',header = T)


test = read.csv('D:/customer churn/Test_data.csv',header = T)

# a look at the data

head(train)
head(test)

# structure of the data

dim(train)
dim(test)

str(train)
str(test)

# checking for missing values

sum(is.na(train))
sum(is.na(test))

#statistics 

summary(train)
summary(test)

# EDA
# lets see the distributions
# density plots 



library(ggplot2)

library(Rmisc)

#d <- density(train$account.length) # returns the density data 
p1 <- ggplot(train, aes(account.length)) +
  geom_density() + labs(title="distribution of account lenght") # plots the results
p2 <- ggplot(train, aes(area.code)) +
  geom_density() + labs(title="distribution of area code")
p3 <- ggplot(train, aes(number.vmail.messages)) +
  geom_density() + labs(title="distribution of number vmail ")
p4 <- ggplot(train, aes(total.day.minutes)) +
  geom_density() + labs(title="distribution of day minutes")
multiplot(p1, p2, p3, p4, cols=2)

#d <- density(train$account.length) # returns the density data 
p5 <- ggplot(train, aes(total.day.calls)) +
  geom_density()  + labs(title="distribution of total day calls")# plots the results
p6 <- ggplot(train, aes(total.day.charge)) +
  geom_density() + labs(title="distribution of total day charge")
p7 <- ggplot(train, aes(total.eve.minutes)) +
  geom_density() + labs(title="distribution of total eve minutes")
p8 <- ggplot(train, aes(total.eve.calls)) +
  geom_density() + labs(title="distribution of total eve calls")
multiplot(p5,p6,p7,p8,cols = 2)


p9 <- ggplot(train, aes(total.eve.charge)) +
  geom_density()  + labs(title="distribution of total eve charge")# plots the results
p10 <- ggplot(train, aes(total.night.minutes)) +
  geom_density() + labs(title="distribution of total night minutes")
p11 <- ggplot(train, aes(total.intl.minutes)) +
  geom_density() + labs(title="distribution of total intl minutes")
p12  <- ggplot(train, aes(total.intl.calls)) +
  geom_density() + labs(title="distribution of total intl calls")
multiplot(p9,p10,p11,p12,cols = 2)

# CDF plots


cdf1 <- ggplot(data=train, aes(x=total.intl.calls)) + stat_ecdf() + labs(title="distribution of total intl calls")
cdf2 <- ggplot(data=train, aes(x=number.vmail.messages)) + stat_ecdf() + labs(title="distribution of number vmail messages")
cdf3 <- ggplot(data=train, aes(x=total.eve.calls)) + stat_ecdf() + labs(title="distribution of total eve calls")
cdf4 <- ggplot(data=train, aes(x=number.customer.service.calls)) + stat_ecdf() + labs(title="distribution of number customer service calls")

multiplot(cdf1,cdf2,cdf3,cdf4,cols = 2)


# unique points in the variabels
print(length(unique(train$phone.number)))
print(length(unique(train$international.plan)))
print(length(unique(train$state)))
print(length(unique(train$voice.mail.plan)))
print(length(unique(train$account.length)))
print(length(unique(train$area.code)))
  
# count of dependent variable 
count(train$Churn) # Churn is imbalaced

# pie plots for international plan and voice mail plan

dev.new(width=5, height=4)
slices <- c(3010,323) 
lbls <- c('No', 'Yes')
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of International plan")

dev.new(width=5, height=4)
slices <- c(2411, 922) 
lbls <- c('No', 'Yes')
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Voice mail plans")
dev.off()
state = count(train$state)

# bar plot for state varaible
p<-ggplot(data=state, aes(x=state$x, y=state$freq)) +
  geom_bar(stat="identity")
# Horizontal bar plot
p + coord_flip()

# chi square test 

library(MASS) 


## Chi-squared Test of Independence
factor_index = sapply(train,is.factor)
factor_data = train[,factor_index]

for (i in 1:4)
{
  #print(names(factor_data)[i])
  #print(chisq.test(table(factor_data$Churn,factor_data[,i])))
}

# 

train$Churn = as.character(train$Churn)
#head(train$Churn,20)

train$Churn[train$Churn == " False."] = 0
train$Churn[train$Churn == " True."] = 1

train$Churn = as.numeric(train$Churn)

test$Churn = as.character(test$Churn)

test$Churn[test$Churn == " False."] = 0
test$Churn[test$Churn == " True."] = 1

test$Churn = as.numeric(test$Churn)

#----------------------------------------------------------------------------
# label encoding for categorical varaibles

train$ch = 1
test$ch = 0
# creating a new variable with rbind train and test
d = rbind(train,test)

train = rbind(train,test)

for (i in 1:ncol(train)){
  if(class(train[,i])=='factor'){
    train[,i] = factor(train[,i],labels = (1:length(levels(factor(train[,i])))))
  }
}


# correlation of the varilables

library(Hmisc)


numeric_index = sapply(train,is.numeric) #selecting only numeric
library(corrplot)
dev.new(width=5, height=4)
M = cor(train[,numeric_index])

corrplot(M, method = "circle")
dev.off()

p1 = ggplot(d, aes(x=Churn, y=total.day.minutes)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()

p2 = ggplot(d, aes(x=Churn, y=total.day.calls)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()



p3 = ggplot(d, aes(x=Churn, y=total.eve.minutes)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()


p4 = ggplot(d, aes(x=Churn, y=total.eve.calls)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()

multiplot(p1, p2, p3, p4, cols=2)

p5 = ggplot(d, aes(x=Churn, y=total.night.minutes)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()

p6 = ggplot(d, aes(x=Churn, y=total.night.calls)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()


p7 = ggplot(d, aes(x=Churn, y=total.eve.minutes)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()


p8 = ggplot(d, aes(x=Churn, y=total.intl.minutes)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()

p9 = ggplot(d, aes(x=Churn, y=number.customer.service.calls)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()
multiplot(p5, p6, p7, p8,p9, cols=3)

# feature enegineering 
x <- as.character(train$phone.number)



x2 = list()
for (i in 1:5000){
  x1 = strsplit(x, '-')[[i]][1]
  x2 = append(x2,x1)

}

train$phone.code = x2

train$total = train$total.day.charge + train$total.eve.charge + train$total.night.charge + train$total.intl.charge


library(dplyr)
train$number.customer.service.calls = as.factor(train$number.customer.service.calls)

#train$Churn = as.integer(train$Churn)
X_churn_mean = train  %>% group_by(number.customer.service.calls) %>% summarise(mean(Churn))
colnames(X_churn_mean) = c("number.customer.service.calls","churn.mean1")
train = merge(train,X_churn_mean,by = "number.customer.service.calls")


X_phone_mean = train  %>% group_by(account.length) %>% summarise(mean(Churn))
colnames(X_phone_mean) = c("account.length","account.length.mean")
train = merge(train,X_phone_mean,by = "account.length")


train$avg = train$total.day.minutes/train$total.day.calls

train$avg.e = train$total.eve.minutes/train$total.eve.calls
train$avg.n = train$total.night.minutes/train$total.night.calls
train$avg.i = train$total.intl.minutes/train$total.intl.calls

# correlation of feature importance variables

train[is.na(train)] <- 0

library(corrplot)
dev.new(width=5, height=4)

dub = select_(train,~avg, ~avg.e, ~avg.n,~avg.i,~churn.mean1,~Churn,~total,~account.length.mean)
M = cor(dub)

corrplot(M, method = "circle")
dev.off()

# violin plots 
p1 = ggplot(train, aes(x=Churn, y=avg.i)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()

p2 = ggplot(train, aes(x=Churn, y=avg)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()


p3 = ggplot(train, aes(x=Churn, y=avg.n)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()


p4 = ggplot(train, aes(x=Churn, y=avg.e)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()

p5 = ggplot(train, aes(x=Churn, y=total)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()

plot(p1)
multiplot(p1, p2, p3, p4,p5, cols=3)

# randomforest for feature engineering

train$phone.code[train$phone.code == 408] = 1
train$phone.code[train$phone.code == 415] = 0
train$phone.code[train$phone.code == 510] = 2

##Random forest for feature importance
library("randomForest")
library(caret)


#select(test,avg,total,account.length.mean,churn.mean1,phone.code.mean)
fit_classify = randomForest(Churn ~ avg+total+account.length.mean+churn.mean1+avg.i+avg.n+avg.e, train, importance = TRUE, ntree = 200)
#pred = predict(fit_classify, test[,-20])
#xtab = table(observed = test[,20], predicted = pred)
#confusionMatrix(xtab)

#extract the variable importance
importnace1  = data.frame(importance(fit_classify, type = 2))
importnace1$names = row.names.data.frame(importnace1)


ggplot(importnace1,aes(x = names,y= IncNodePurity))+geom_bar(stat = "identity")

# feature importance using Logistic regression

#Normalization

library(clusterSim)

data = select(train,avg,total,account.length.mean,churn.mean1,avg.i,avg.n,avg.e,Churn)
norm_data = data.Normalization(data[,-1], type = "n4", normalization = "column")
L = glm(Churn ~ avg+total+account.length.mean+churn.mean1+phone.code.mean+avg.i+avg.n+avg.e , data = norm_data, family = gaussian)
summary(L)

library(mlr)
train$area.code = as.factor(train$area.code)
train = createDummyFeatures(train, cols = "area.code")
train$area.code.408 = NULL

#Modeling --------------------------------------------------------------

test = train[which(train$ch == 0),]

train = train[which(train$ch == 1),]


# up sampling
#up_train <- upSample(x = train[, colnames(train) %ni% "Class"],
#                    y = train$Churn)

#table(up_train$Class)

#data$international.plan = as.numeric(data$international.plan)
#train$Churn = as.numeric(data$Churn)



data = select_(train,~international.plan,~total.day.minutes,~total.day.calls,~total.eve.minutes,
               ~total.intl.minutes,~total.night.minutes,~total,~account.length.mean,
               ~churn.mean1,~avg,~Churn)

#data$international.plan = as.numeric(data$international.plan)

library(dplyr)
data1 = select_(test,~international.plan,~total.day.minutes,~total.day.calls,~total.eve.minutes,
                    ~total.intl.minutes,~total.night.minutes,~total,~account.length.mean,
                    ~churn.mean1,~avg,~Churn)

#data1$international.plan = as.numeric(data1$international.plan)

    
    
library(randomForest)
library(InformationValue)

    
data2 = select_(train,~international.plan,~total.day.minutes,~total.day.calls,~total.eve.minutes,
                ~total.intl.minutes,~total.night.minutes,~total,~account.length.mean,
                ~churn.mean1,~avg,~total.intl.calls,~total.eve.calls,~avg.i,~total.night.calls,~number.vmail.messages~Churn)

data3 = select_(test,~international.plan,~total.day.minutes,~total.day.calls,~total.eve.minutes,
                ~total.intl.minutes,~total.night.minutes,~total,~account.length.mean,
                ~churn.mean1,~avg,~total.intl.calls,~total.eve.calls,~avg.i,~total.night.calls,~number.vmail.messages~Churn)
library(randomForest)
###Random Forest
RF_model = randomForest(Churn ~ ., data = data2,classwt=c(0.2,0,8), importance = TRUE, ntree = 500)

#Presdict test data using random forest model
RF_Predictions = predict(RF_model, data3[,-15])

#convert prob
RF_Predictions = ifelse(RF_Predictions > 0.5, 1, 0)
RF_Predictions[1:3] 
##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn, RF_Predictions)
#confusionMatrix(ConfMatrix_RF)
#Accuracy
sum(diag(ConfMatrix_RF))/nrow(test)

library(pROC)
roc_obj <- roc(test$Churn, RF_Predictions)
auc(roc_obj)

#Logistic Regression
logit_model = glm(Churn~ ., data = data, family = "binomial",control = list(maxit = 50))

#summary of the model
summary(logit_model)

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = data1[,-12], type = "response")

#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)


##Evaluate the performance of classification model
ConfMatrix_RF = table(data1$Churn, logit_Predictions)

#Accuracy
sum(diag(ConfMatrix_RF))/nrow(test)

#False Negative rate
#FNR = FN/FN+TP 
#FNR = 55/55+57

library(pROC)
fitted.results1 = data.frame(logit_Predictions)
auc(test$Churn, fitted.results1$logit_Predictions)


# svm 

library(e1071)

model_svm <- svm(Churn ~ . , data, kernel =
                   "linear",gamma = 0)

#Use the predictions on the data

pred <- predict(model_svm, data1[,-12])
pred = ifelse(pred > 0.5, 1, 0)

##Evaluate the performance of classification model
ConfMatrix_RF = table(data1$Churn, pred)

#Accuracy
sum(diag(ConfMatrix_RF))/nrow(test)

library(pROC)
roc_obj <- roc(test$Churn, Pred)
auc(roc_obj)

# gbm

library(gbm)

gbm1 <- gbm(Churn~ .,data = data2,n.trees = 300)

#Presdict test data using random forest model
RF_Predictions = predict(gbm1, data3[,-15])
#convert prob
RF_Predictions = ifelse(RF_Predictions > 0.5, 1, 0)
#RF_Predictions 
##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn, RF_Predictions)
#confusionMatrix(ConfMatrix_RF)
#Accuracy
sum(diag(ConfMatrix_RF))/nrow(test)

# xgboost
library(xgboost)
y = data2$Churn
xgb = xgboost(data = data.matrix(data2[,-15]),label = y,nrounds = 200)

Predictions = predict(xgb, data.matrix(data3[,-15]))
Predictions = ifelse(Predictions > 0.5, 1, 0)
##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn, Predictions)

#confusionMatrix(ConfMatrix_RF)
#Accuracy
sum(diag(ConfMatrix_RF))/nrow(test)

library(pROC)
roc_obj <- roc(test$Churn, Predictions)
auc(roc_obj)
