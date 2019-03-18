library(rsample)
data("attrition")
typeof(attrition)
str(attrition)
mydata <- attrition
typeof(mydata)
table(mydata$Attrition)

# considering only the numeric variables in the dataset
numeric_mydata <- mydata[,c(1,4,6,11,13,17,19,21,24,25,26,28:31)]

#Data transformations Numeric
tra_num <- mydata[,c(7,10,14,15,20,22)]
num_Education <- as.numeric(tra_num$Education)
num_Gender <- as.numeric(tra_num$Gender)
num_JobRole <- as.numeric(tra_num$JobRole)
num_JobSatisfaction <- as.numeric(tra_num$JobSatisfaction)
num_OverTime <- as.numeric(tra_num$OverTime)
num_PerformanceRating <- as.numeric(tra_num$PerformanceRating)

# converting the target variable "yes" or "no" values into numeric
# it defaults to 1 and 2 however converting it into 0 and 1 to be consistent
numeric_Attrition = as.numeric(mydata$Attrition)- 1

# create a new data frame with numeric columns and numeric target
numeric_mydata = cbind(numeric_mydata,numeric_Attrition)
numeric_mydata = cbind(numeric_mydata,num_Education)
numeric_mydata = cbind(numeric_mydata,num_Gender)
numeric_mydata = cbind(numeric_mydata,num_JobRole)
numeric_mydata = cbind(numeric_mydata,num_JobSatisfaction)
numeric_mydata = cbind(numeric_mydata,num_OverTime)
numeric_mydata = cbind(numeric_mydata,num_PerformanceRating)

str(numeric_mydata)

# loading the required library
library(corrplot)

# creating correlation plot
M <- cor(numeric_mydata)
corrplot(M, method="circle")

### Overtime vs Attiriton
library(ggplot2)
l <- ggplot(mydata, aes(OverTime, fill = Attrition))
l <- l + geom_histogram(stat = "count")
plot(l)

tapply(as.numeric(mydata$Attrition)-1, mydata$OverTime, mean)

### MaritalStatus vs Attiriton
ma <- ggplot(mydata, aes(MaritalStatus,fill = Attrition))
ma <- ma + geom_histogram(stat="count")
plot(ma)

tapply(as.numeric(mydata$Attrition)-1, mydata$MaritalStatus, mean)

###JobRole vs Attrition
ja <- ggplot(mydata, aes(JobRole, fill = Attrition))
ja <- ja + geom_histogram(stat = "count")
plot(ja)

tapply(as.numeric(mydata$Attrition)-1, mydata$JobRole, mean)

mean(as.numeric(mydata$Attrition) - 1)

###Gender vs Attrition
ga <- ggplot(mydata, aes(Gender, fill = Attrition))
ga <- ga + geom_histogram(stat = "count")
plot(ga)

tapply(as.numeric(mydata$Attrition)-1, mydata$Gender, mean)

###EducationField vs Attrition
ea <- ggplot(mydata, aes(EducationField, fill = Attrition))
ea <- ea + geom_histogram(stat = "count")
plot(ea)

tapply(as.numeric(mydata$Attrition)-1, mydata$EducationField, mean)

###Department vs Attrition
da <- ggplot(mydata, aes(Department, fill = Attrition))
da <- da + geom_histogram(stat = "count")
plot(da)

tapply(as.numeric(mydata$Attrition)-1, mydata$Department, mean)

###BusinessTravel vs Attrition
ba <- ggplot(mydata, aes(BusinessTravel, fill = Attrition))
ba <- ba + geom_histogram(stat = "count")
plot(ba)

tapply(as.numeric(mydata$Attrition)-1, mydata$BusinessTravel, mean)


### OverTime vs.Age vs. MaritalStatus vs.Attrition
ggplot(mydata, aes(OverTime, Age)) + 
  facet_grid(.~MaritalStatus) + 
  geom_jitter(aes(color=Attrition), alpha = 0.4) +
  ggtitle("x=OverTime, y= Age, z = MaritalStatus , t = Attrition") +
  theme_light()

### MonthlyIncome vs. Age, by color = Attrition
ggplot(mydata, aes(MonthlyIncome, Age, color = Attrition)) +
  geom_jitter() +
  ggtitle("MonthlyIncome vs. Age, by color = Attrition") +
  theme_light()

### Histogram of Ages
ggplot(mydata, aes(Age))+ geom_histogram(fill='blue',color = 'black',bins=20,alpha=0.5) + scale_y_continuous(breaks = seq(min(0), max(200), by = 10)) + scale_x_continuous(breaks = seq(min(18), max(65), by = 2)) + theme_light()

### Population Pyramide
ggplot(mydata, aes(x= Age, fill = Gender)) +
  geom_bar(data = subset(mydata, Gender == "Female")) + 
  geom_bar(data = subset(mydata, Gender == "Male"), mapping = aes(y = -..count..), position = "identity") +
  scale_y_continuous(labels = abs) + coord_flip()



####  End of the Exploratory Data Analysis.  Now, starts the Logistic Regression to analyze Attrition
str(mydata)

###Let's select the relevant columns for training
###Assuming that the company doesn't know the personal opinions of worker about 
###Work-Life Balance, Relationship Satisfaction, Job Satisfaction, and Environment Satisfaction.:
library(dplyr)

df.mydata <- select(mydata, -WorkLifeBalance, -RelationshipSatisfaction, -JobSatisfaction, -EnvironmentSatisfaction)

str(df.mydata)

###Now let's set factor columns.
df.mydata$StockOptionLevel <- factor(df.mydata$StockOptionLevel)
df.mydata$JobLevel <- factor(df.mydata$JobLevel)

###Now let's set Number column of Attrition.
df.mydata$Attrition <- as.numeric(df.mydata$Attrition) -1


str(df.mydata)

### Train the Model###

###Separe datasets
library(caTools)

split = sample.split(df.mydata$Attrition, SplitRatio = 0.7)

train.mydata <- subset(df.mydata, split == TRUE)
test.mydata <- subset(df.mydata, split == FALSE)

###Run the  model on train.mydata set

train.mydt.model <- glm(formula = Attrition ~ ., family = binomial(link='logit'),data = train.mydata)

summary(train.mydt.model)

###check our prediction accuracy
fitted.prob <- predict(train.mydt.model, newdata = test.mydata, type = 'response')


###calculate from the predicted values
result.fit <- ifelse(fitted.prob > 0.5,1,0)

### Calc Accuracy of model
ClassError <- mean(result.fit!= test.mydata$Attrition)
ClassError
print(paste('Accuracy', 1-ClassError))

###Confusion Matrix
table(test.mydata$Attrition, fitted.prob > 0.5)