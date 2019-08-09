

#Working Directory
#Installing packages


# Calculations
# Whatever we do in console is lost hence we use script to store the information
# Using the help menu

2+2
3-4

mean(17,12,34,55)   # Not the correct way, it will only give the first content
mean(c(12,12,34,55)) # Checking the mean

log(122) # Checking the log

var(c(12,12,34,55)) # Checking the variance
sd(c(12,12,34,55))  # Checking the Standard Deviation

# Data Strctures in R

# Lists
# Matrix
# Arrays

# Data Types in R

# character
# numeric (real or decimal)
# integer
# logical
# complex


# Vector

x <- c(1, 5, 4, 9, 0)
typeof(x)
length(x)


x <- c(1, 5.4, TRUE, "hello")
x
typeof(x)

x[3] # accessing the vector


# Example
x <- "dataset"
typeof(x)


y <- 1:10
y

typeof(y)


length(y)

z <- as.numeric(y)
z

class(z)


# Matrix in R ########

A <- matrix( c(2, 4, 3, 1, 5, 7), # the data elements 
   nrow=2,              # number of rows 
   ncol=3,              # number of columns 
   byrow = TRUE)        # fill matrix by rows 

A                      # print the matrix 

A[2, 3]


A[2, ]


A[ ,c(1,3)]     # The first and third column


# Arrays


vector2 <- c(10,11,12,13,14,15)

# Take these vectors as input to the array.
result <- array(c(vector2),dim = c(3,3,2))
print(result)


#Lists

# Data Frames

weather<- data.frame(day=c('saturday','sunday','monday'),date=c('July-4','July-5','July-6'),temp=c(102,104,98))

weather

length(weather)

names(weather)

weather$temp

weather$tempc<-round((weather$temp-32)*5/9)

head(weather)

is.na(weather)

sum(is.na(weather))

summary(weather)

str(weather)

nrow(weather)

ncol(weather)

View(weather)

pairs(weather)



setwd('C:\\Users\\saravana.ayyappa\\Desktop\\Machine Learning')             # Changing the working directory
data<-read.csv('sales_data.csv')   #Loading the data
head(data)                   #head of the data
cor_data<-cor(data[,3:5])    #Correlation matrix of the dataset  

colnames(data)                #Checking the names of the columns in our data


# Data Exploration
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)


summary(data)   # Get the Description of the data

g1<-ggplot(data,aes(x=moisturizer,y=facewash)) + geom_point() # Creating various plots
plot(g1)
g2<-g1 + xlim(c(0, 3000)) + ylim(c(0, 5000)) 
plot(g2)
g3<-g2+geom_smooth(method="lm", col="blue")
g4<-g3+ ggtitle("TiTle", subtitle="From Stock dataset") + xlab("X-axis") + ylab("Y-Axis")
plot(g4)



boxplot(data$facecream)   #Box Plot

boxplot(data$toothpaste,               #Box plot with all variables
        main = "Tooth Paste Sales",
        xlab = "Parts Per Billion",
        ylab = "Ozone",
        col = "orange",
        border = "brown",
        horizontal = FALSE,
        notch = TRUE)




# For loops in R

# Create a vector filled with random normal values
u1 <- rnorm(30)

# Initialize `usq`
usq <- 0

for(i in 1:5) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  usq[i] <- u1[i]*u1[i]
  print(usq[i])
}

# Create a 30 x 30 matrix (of 30 rows and 30 columns)

mymat <- matrix(nrow=30, ncol=30)
print(mymat)


# For each row and for each column, assign values based on position: product of two indexes
for(i in 1:dim(mymat)[1]) {
  for(j in 1:dim(mymat)[2]) {
    mymat[i,j] = i*j
  }
}

# Just show the upper left 10x10 chunk
mymat[1:10, 1:10]



# Make a lower triangular matrix (zeroes in upper right corner)
m=10 
n=10

# A counter to count the assignment
ctr=0

# Create a 10 x 10 matrix with zeroes 
mymat = matrix(0,m,n)

for(i in 1:m) {
  for(j in 1:n) {   
    if(i==j) { 
      break;
    } else {
      # you assign the values only when i<>j
      mymat[i,j] = i*j
      ctr=ctr+1
    }
  }
  print(i*j) 
}


n<-4
if(n<=3){
  print('yes')
}else{
  print('No')
}


# Create vector quantity

quantity <-  10
# Create multiple condition statement
if (quantity <20) {
  print('Not enough for today')
} else if (quantity > 20  &quantity <= 30) {
  print('Average day')
} else {
  print('What a great day!')
}


# Print how many matrix cells were assigned

print(ctr)



# Print all uneven numbers

m=20

for (k in 1:m){
  if (!k %% 2)
    next
  print(k)
}

# while loops

i <- 1
while (i < 6) {
  print(i)
  i = i+1
}






# Machine Learning

#Linear Regression in R

library(tidyverse)
library(ggpubr)

data("marketing", package = "datarium")
head(marketing, 4)

ggplot(marketing, aes(x = youtube, y = sales)) +
  geom_point() +
  stat_smooth(method = lm)

cor(marketing$sales, marketing$youtube)

#Train Test Split

smp_size <- floor(0.75 * nrow(marketing))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(marketing)), size = smp_size)

train <- marketing[train_ind, ]
test <- marketing[-train_ind, ]

model <- lm(sales ~ youtube, data = train)
model

pred<-predict(model,test)
head(pred)

head(test$sales)

summary(model)

sigma(model)*100/mean(marketing$sales)

model$coefficients

RMSE(test$sales,pred)


# Logistic Regression in R


train <- read.csv('Social_Network_Ads.csv')
train[1:5,1:5]  #Check first five rows and first five colunns

train$Age      #Check Age COlumns
train[train$Age>50,"Gender"]   # COnditional Statements

head(train[order(train$Age),c('Age','Gender'),])   # Order the data with respect to Age

min(train$Age)

head(train[order(train$Age,decreasing = TRUE),c('Age','Gender'),])   # Order the data with respect to Age

max(train$Age)


#create training and validation data from given data

library(caTools)


set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(train,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train1 =subset(train,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test1=subset(train, sample==FALSE)

#logistic regression model
model <- glm (Purchased ~ ., data = train1, family = binomial)
summary(model)

predict <- predict(model,test1, type = 'response')


#confusion matrix
table(test1$Purchased, predict > 0.5)


#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, test1$Purchased)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))


# Decision Tree in R
library(rpart)

head(kyphosis)

# grow tree 
model2 <- rpart(Kyphosis ~ Age + Number + Start,
             method="class", data=kyphosis)


printcp(model2) # display the results 
plotcp(model2) # visualize cross-validation results 
summary(model2) # detailed summary of splits

# plot tree 
plot(model2, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(model2, use.n=TRUE, all=TRUE, cex=.8)



# Regression Tree Example
library(rpart)

# grow tree 
fit <- rpart(Mileage~Price + Country + Reliability + Type, 
             method="anova", data=cu.summary)


printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results   

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


# Random Forest

library(randomForest)
fit <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fit) # view results 
summary(fit) #summary of the model
importance(fit) # importance of each predictor


# Gradient Boosting Machine

require(gbm)
require(MASS)#package with the boston housing dataset

#separating training and test data
train=sample(1:506,size=374)

Bostongbm=gbm(medv ~ . ,data = Boston[train,],distribution = "gaussian",n.trees = 10000,
                 shrinkage = 0.01, interaction.depth = 4)
Bostongbm

summary(Bostongbm) #Summary gives a table of Variable Importance and a plot of Variable Importance


rm(list=ls())   # TO clear the Enviroment
