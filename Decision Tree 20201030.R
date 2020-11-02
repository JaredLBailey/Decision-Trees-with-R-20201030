###
### 00 Overview ####
###

set.seed(85)

#
# 01 Read in csv file from Kaggle
# 02 Check data
# 03 Split data into train and test
# 04 Explore data
# 05 Edit data
# 06 Build silly gender model
# 07 Build math model
# 08 Predict on Test Data
# 09 Check the Results
#




###
### 01 Read in csv file from Kaggle ####
###

#https://www.kaggle.com/spscientist/students-performance-in-exams
#StudentsPerformance.csv

StudentsPerformance <- read.csv("C:/Users/jaredb/Desktop/Home/R/Decision Tree 20201030/Data/StudentsPerformance.csv", quote = '""')




###
### 02 Check data ####
###

#checks
str(StudentsPerformance)
head(StudentsPerformance)




###
### 03 Split data into train and test ####
###

#build train set
train_set_index <- sample(1:nrow(StudentsPerformance), .8 * nrow(StudentsPerformance))
str(train_set_index)

train_set <- StudentsPerformance[train_set_index, ]
str(train_set)




#build test set
test_set_index <- setdiff(1:nrow(StudentsPerformance), train_set_index)
str(test_set_index)

test_set <- StudentsPerformance[test_set_index, ]
str(test_set)



###
### 04 Explore data ####
###

barplot(prop.table(table(train_set$gender)))
barplot(prop.table(table(train_set$race.ethnicity)))
barplot(prop.table(table(train_set$parental.level.of.education)))
barplot(prop.table(table(train_set$lunch)))
barplot(prop.table(table(train_set$test.preparation.course)))
hist(train_set$math.score)
hist(train_set$reading.score)
hist(train_set$writing.score)

cor(train_set[6:8])




###
### 05 Edit data ####
###

#train grade edits
MathGrade <- ifelse(train_set$math.score >= 70, "Pass", "Fail")
ReadingGrade <- ifelse(train_set$reading.score >= 70, "Pass", "Fail")
WritingGrade <- ifelse(train_set$writing.score >= 70, "Pass", "Fail")

train_set <- data.frame(train_set[1:5], MathGrade, ReadingGrade, WritingGrade)
str(train_set)




#test grade edits
MathGrade <- ifelse(test_set$math.score >= 70, "Pass", "Fail")
ReadingGrade <- ifelse(test_set$reading.score >= 70, "Pass", "Fail")
WritingGrade <- ifelse(test_set$writing.score >= 70, "Pass", "Fail")

test_set <- data.frame(test_set[1:5], MathGrade, ReadingGrade, WritingGrade)
str(test_set)




#change data types to factors
train_set$gender = as.factor(train_set$gender)
train_set$race.ethnicity = as.factor(train_set$race.ethnicity)
train_set$parental.level.of.education = as.factor(train_set$parental.level.of.education)
train_set$lunch = as.factor(train_set$lunch)
train_set$test.preparation.course = as.factor(train_set$test.preparation.course)
train_set$MathGrade = as.factor(train_set$MathGrade)
train_set$ReadingGrade = as.factor(train_set$ReadingGrade)
train_set$WritingGrade = as.factor(train_set$WritingGrade)

test_set$gender = as.factor(test_set$gender)
test_set$race.ethnicity = as.factor(test_set$race.ethnicity)
test_set$parental.level.of.education = as.factor(test_set$parental.level.of.education)
test_set$lunch = as.factor(test_set$lunch)
test_set$test.preparation.course = as.factor(test_set$test.preparation.course)
test_set$MathGrade = as.factor(test_set$MathGrade)
test_set$ReadingGrade = as.factor(test_set$ReadingGrade)
test_set$WritingGrade = as.factor(test_set$WritingGrade)

str(train_set)
str(test_set)




###
### 06 Build silly gender model ####
###

#build the model

library(rpart)

model1 <- rpart(gender ~ race.ethnicity + 
                  parental.level.of.education +
                  lunch + 
                  test.preparation.course + 
                  MathGrade + 
                  ReadingGrade + 
                  WritingGrade,
                data = train_set
                )

summary(model1)

#plot the tree
library(rattle)
fancyRpartPlot(model1)

#get the tree details
model1




###
### 07 Build math model ####
###

#with reading and writing grades

model2 <- rpart(MathGrade ~ gender +
                  race.ethnicity + 
                  parental.level.of.education +
                  lunch + 
                  test.preparation.course + 
                  ReadingGrade + 
                  WritingGrade,
                data = train_set)

summary(model2)

#plot the tree
fancyRpartPlot(model2)




#get the tree details
model2


#without reading and writing grades

model3 <- rpart(MathGrade ~ gender +
                  race.ethnicity + 
                  parental.level.of.education +
                  lunch + 
                  test.preparation.course,
                data = train_set)

summary(model3)

#plot the tree
fancyRpartPlot(model3)

#get the tree details
model3




###
### 08 Predict on Test Data ####
###

model2_test_pred <- predict(model2, test_set, type = "class")

model3_test_pred <- predict(model3, test_set, type = "class")




###
### 09 Check the Results ####
###

#confusion matrix with reading and writing scores
table(test_set$MathGrade, model2_test_pred)
plot(table(model2_test_pred, test_set$MathGrade),
     main = "Confusion Matrix - Math Scores w/ R&W Scores",
     xlab = "Predicted Math Grades",
     ylab = "Actual Math Grades"
     )


#precision - TP / (TP + FP)
95 / (95 + 25)
#When our model claims a student failed, it was right 79.2% of the time


#recall - TP / (TP + FN)
95 / (95 + 17)
#Our model only found 84.8% of the failing math students




#confusion matrix without reading and writing scores
table(test_set$MathGrade, model3_test_pred)
plot(table(model3_test_pred, test_set$MathGrade),
     main = "Confusion Matrix - Math Scores w/o R&W Scores",
     xlab = "Predicted Math Grades",
     ylab = "Actual Math Grades"
)


#precision - TP / (TP + FP)
86 / (86 + 41)
#When our model claims a student failed, it was right 67.7% of the time


#recall - TP / (TP + FN)
86 / (86 + 26)
#Our model only found 76.8% of the failing math students
