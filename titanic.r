#cleanup before start
rm(list=ls(all=T))

require("dplyr")
require('ggplot2')
require('e1071') #svm
setwd("D:/TitanicProject")
path = "D:/TitanicProject"

train = read.csv(paste(path,"/train.csv",sep=""))
test = read.csv(paste(path,"/test.csv",sep=""))

#combine the data
full = bind_rows(train,test)

#get titles
full$Title = gsub('(.*, )|(\\..*)', '', full$Name)
rare_title = c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
full$Title[full$Title == 'Mlle']= 'Miss' 
full$Title[full$Title == 'Ms']= 'Miss'
full$Title[full$Title == 'Mme']= 'Mrs' 
full$Title[full$Title %in% rare_title]='Rare'

#column for size of family
full$family = full$SibSp + full$Parch + 1

ggplot(full[1:891,], aes(x = family,fill= factor (Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') 

##missing values
#embarkment NA at 62 and 830
ggplot(full, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot()+ geom_hline(aes(yintercept=80), colour='red', linetype='dashed', lwd=2)
full$Embarked[c(62, 830)] <- 'C'

#missing fare at 1044
x= (full$Fare[full$Pclass == 3 & full$Embarked == 'S'])
summary(x)
full$Fare[1044] = 8.05

##fill in the missing age values using regression based imputation
#hist(full$Age, freq=F, main='Age: Original Data',col='darkgreen', ylim=c(0,0.04))
plot(density(full$Age[!is.na(full$Age)]))

with_age = subset(full, !is.na(Age))
without_age = subset(full, is.na(Age))

ageMLR = lm(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title, data = with_age)
temp = predict(ageMLR, full)

for(i in 1:1309)
{
  if(is.na(full$Age[i]))
  {
    full$Age[i] = ceiling(temp[i])
    if(full$Age[i] < 0)
      full$Age[i] = 0
  }  
}

#hist(full$Age, freq=F, main='Age: New Data', col='darkgreen', ylim=c(0,0.04))
plot(density(full$Age))
###
#########################################################################################
#scale all numeric variables to between 0 and 1
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
full$Pclass = range01(full$Pclass)
full$Age = range01(full$Age)
full$SibSp = range01(full$SibSp)
full$Parch = range01(full$Parch)
full$Fare = range01(full$Fare)
full$family = range01(full$family)

##now separate to predict
train <- full[1:891,]
test <- full[892:1309,]
train$Survived = (train$Survived ==1)
test$Survived = (test$Survived ==1)

model = svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data=train, type = 'C')#, cost = 100, gamma = 100)
#model = svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + family,family=binomial(link='logit'), data=train)
train_pred = predict(model, train)
#train_pred = train_pred >= 0.5

count =0
for(i in 1:891)
{
  if(train_pred[i] == train$Survived[i])
    count = count + 1
}
count/891

######
##  predict on test data
test_pred = predict(model, test)
#train_pred = train_pred >= 0.5
count =0
for(i in 1:418)
{
  if(test_pred[i] == test$Survived[i])
    count = count + 1
}
count/418
