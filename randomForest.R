h1b_transformed_na <- readRDS("h1b_transformed_without_na.rds")


# Dividing h1b_transformed_na into train and test data
train_na = h1b_transformed_na[1:1299875,]
test_na = h1b_transformed_na[1299876:2599750,]

# Replacing the CASE_STATUS of Test data with None in h1b_na data
test.case.na <- data.frame(CASE_STATUS=rep("None", nrow(test_na)), test_na[,])
test.case.na$CASE_STATUS.1 <- NULL

# Combine test and train datasets
data.combined.na <- rbind(train_na,test.case.na)

# Converting Employer name, Job Title into character
data.combined.na$JOB_TITLE <- as.character(data.combined.na$JOB_TITLE)
data.combined.na$EMPLOYER_NAME <- as.character(data.combined.na$EMPLOYER_NAME)

# Removing extra levels from CASE_STATUS
data.combined.na$CASE_STATUS <- factor(data.combined.na$CASE_STATUS)

#converting employer name, job title into numeric data
data.combined.na$JOB_TITLE <- as.numeric(data.combined.na$JOB_TITLE)
data.combined.na$EMPLOYER_NAME <- as.factor(data.combined.na$EMPLOYER_NAME)


#select the first 1000 case statuses
select <- train_na[1:1000,]

# Implementing randomForest on the data
install.packages("randomForest")
library(randomForest)

# Random forest training 1
rf.train.1 <- data.combined.na[1:1000 ,c("FULL_TIME_POSITION","PREVAILING_WAGE")]
rf.label <- as.factor(select$CASE_STATUS)
rf.label <- factor(rf.label)

set.seed(1234)
rf.1 <- randomForest(x= rf.train.1,y=rf.label, importance=TRUE,ntree = 1000)
rf.1
varImpPlot(rf.1)

# Random forest training 2
rf.train.2 <- data.combined.na[1:1000 ,c("FULL_TIME_POSITION","PREVAILING_WAGE","YEAR")]

set.seed(1234)
rf.2 <- randomForest(x= rf.train.2,y=rf.label, importance=TRUE,ntree = 1000)
rf.2
varImpPlot(rf.2)


# Random forest training 3
rf.train.3 <- data.combined.na[1:1000 ,c("FULL_TIME_POSITION","PREVAILING_WAGE","WORKSITE_STATE","YEAR")]
rf.train.3$WORKSITE_STATE <- factor(rf.train.3$WORKSITE_STATE)

set.seed(1234)
rf.3 <- randomForest(x= rf.train.3,y=rf.label, importance=TRUE,ntree = 1000)
rf.3
varImpPlot(rf.3)

# Random forest training 4
rf.train.4 <- data.combined.na[1:1000 ,c("WORKSITE_STATE","PREVAILING_WAGE")]
rf.train.4$WORKSITE_STATE <- factor(rf.train.4$WORKSITE_STATE)
set.seed(1234)
rf.4 <- randomForest(x= rf.train.4,y=rf.label, importance=TRUE,ntree = 1000)
rf.4
varImpPlot(rf.4)