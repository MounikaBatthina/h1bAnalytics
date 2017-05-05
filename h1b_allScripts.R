data <- read.csv(paste("C:/Users/maruthi/Dropbox/Masters/Spring17/239/Project/",'finaldata.csv',sep=""),header = TRUE)
svm_df <- data.frame(data$EMPLOYER_NAME,data$PREVAILING_WAGE,data$CASE_STATUS)
svm_df$data.CASE_STATUS <- as.character(svm_df$data.CASE_STATUS)
str(svm_df)

data$PREVAILING_WAGE <- as.numeric(data$PREVAILING_WAGE)
newdata <- subset(data, (CASE_STATUS == "CERTIFIED"  
                         |CASE_STATUS == "DENIED") & PW_UNIT_OF_PAY =="Year",
                  select = c(CASE_STATUS, PREVAILING_WAGE, EMPLOYER_NAME))

newdata$CASE_STATUS <- as.character(newdata$CASE_STATUS)
newdata$EMPLOYER_NAME <- as.character(newdata$EMPLOYER_NAME)
str(newdata)
newdata[newdata$CASE_STATUS == "CERTIFIED",]$CASE_STATUS = 1
newdata[newdata$CASE_STATUS == "DENIED",]$CASE_STATUS = -1
library(stringr)
newdata$UNIVERSITY <- as.factor(ifelse(str_detect(newdata$EMPLOYER_NAME, "UNIVERSITY"), 1, 0))


ifelse(grepl("UNIVERSITY",newdata$EMPLOYER_NAME),newdata$EMPLOYER_NAME <- "Y",newdata$EMPLOYER_NAME <- "N")

library(e1071)

# PREVAILING_WAGE 
fewdata <- newdata[1:100,]
fewdata <- subset(fewdata,select=c(PREVAILING_WAGE,CASE_STATUS))

fewdata$CASE_STATUS <- as.numeric(fewdata$CASE_STATUS)

plot(fewdata, pch=16)
model <- svm(CASE_STATUS ~ PREVAILING_WAGE, fewdata)

# make a prediction for each X
predictedY <- predict(model, fewdata)

# display the predictions
points(fewdata$PREVAILING_WAGE,predictedY , col = "blue", pch=4)


#UNIVERSITY
fewdata <- newdata[1:10000,]
fewdata <- subset(fewdata,select=c(UNIVERSITY,CASE_STATUS))

fewdata$CASE_STATUS <- as.numeric(fewdata$CASE_STATUS)

plot(fewdata, pch=16)
model <- svm(CASE_STATUS ~ UNIVERSITY, fewdata)

# make a prediction for each X
predictedY <- predict(model, fewdata)

# display the predictions
points(fewdata$UNIVERSITY,predictedY , col = "blue", pch=4)


 


