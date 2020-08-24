##HAVARDX FINAL PROJECT DATA SCIENCE
#VICTOR SOUOB


# DEMONSTRATION OF THE RMFT MARKETING MODEL BY STUDYING THE BEHAVIOR OF A BLOOD TRANSFUSION SERVICE CENTER DATA SET IN TAIWAN.

# Let's start by downloading all needed Libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(dplyr)
library(rpart)

# Download dataset from UCI respiratory website

data <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/blood-transfusion/transfusion.data",fill = TRUE,header = TRUE,sep = ",")
head(data)

# Rename columns

tranfusion_dat_names <- c( "last_M_after_don", "Frequency_of_don", "Amount_blood","M_after_first_don", 
                           "Are_theyComingBack")



names(data) <- tranfusion_dat_names

head(data)

#-looking for missing values NA
sum(is.na(data))

# adding period to the dataset  by soubstrating first month of donation to last month

data_period<- data %>% mutate(period = c( M_after_first_don - last_M_after_don))
head(data_period)



# DATA EXPLORATION

str(data_period)

# we are working with a data frame, has 748 rows and 6 variables
# The variable to predict "Are_theyComingBack"  is listed as numeric , and we will change it to a factor of 2 class(0,1)

# changing the variable to predict as a factor (0 = no, 1 = yes)
data_period$Are_theyComingBack <- as.factor(data_period$Are_theyComingBack)
# data summary
summary(data_period$period)
summary (data_period$Frequency_of_don)
summary(data_period$Amount_blood)


# DATA VIZUALISATION 

#  frequeny of donation during the giving period

data_period%>% select(Frequency_of_don,period) %>% 
  ggplot(aes(period,Frequency_of_don)) + geom_count()

## Blood vs frequency

data_period%>% select(Frequency_of_don,Amount_blood) %>% 
  ggplot(aes(Amount_blood ,Frequency_of_don)) + geom_count()
# when the frequency of donation increase , the amount of blood increase as well
# They are dependant.

# lets observe the tendency of people to comeback  or not for blood donation in the datasets

ggplot(data_period, aes(x=Are_theyComingBack)) + geom_bar()
table(data_period$Are_theyComingBack)
## we see that proportion of peaople that are not coming back (570) are higher vs 178

# DATA PARTITION 

set.seed(1)
data_period$Are_theyComingBack <- as.factor(data_period$Are_theyComingBack)
test_index <- createDataPartition(y = data_period$Are_theyComingBack, times = 1, p = 0.2, list = FALSE)
train_sample <- data_period [-test_index,]
test_sample <- data_period [test_index,]


# lets check if we have the same probabiblity after partition

prop.table(table(train_sample$Are_theyComingBack))
prop.table(table(test_sample$Are_theyComingBack))
# The probability for train and test set are almost the same. 


# RESULT
# KNN MODEL

# lets find the best k for the smallest RMSE

set.seed(1)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
knnFit <- train(Are_theyComingBack ~ ., data = train_sample, method = "knn", trControl = ctrl, preProcess = c("center","scale"),tuneLength = 20)
knnFit

#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)

plot(knnFit)

# lets predict with the test set
y_hat_knn <- predict(knnFit,test_sample, type = "raw")


# lets find the accuracy

confusionMatrix(y_hat_glm,test_sample$Are_theyComingBack)$overall[["Accuracy"]]
## we have an accuracy of 0.82 with KNN


## LOGISTIC REGRESSION

#### LOGISTIC REGRESSION
# lets fitt the model 

glmFit <- train(Are_theyComingBack ~ ., data = train_sample, method = "glm", family="binomial")
glmFit


# lets predict it using test set and  find the accuracy
y_hat_glmn<- predict(glmFit, test_sample, type = "raw")
y_hat_glmn
confusionMatrix(data = y_hat_glmn, reference = test_sample$Are_theyComingBack)$overall[1]
### final accuracy of our logisic regresion model is 0.76




#### DECISION TREES

# lets train the rpart and find the best cp
train_rpart <- train(Are_theyComingBack~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     
                     data = train_sample)
plot(train_rpart)

# lets look at the decision tree

plot(train_rpart$finalModel)
text(train_rpart$finalModel,  digits = 3)



## lets find the accuracy of the model
confusionMatrix(predict(train_rpart, test_sample), test_sample$Are_theyComingBack)$overall["Accuracy"]
### accuracy of 75 percent

# lets look at sthe summary of the model
train_rpart$finalModel





