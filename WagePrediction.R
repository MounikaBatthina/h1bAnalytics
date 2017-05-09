# To Predict the prevailing wage for upcoming years for the respective employer and job title

library(ggplot2)
library(plyr)

# Subest the data set which are certified for the selected employer and job title to create the train data set
newdata.frame <- subset(mydata, (CASE_STATUS == "CERTIFIED") 
                        & PW_UNIT_OF_PAY == "Year" & PREVAILING_WAGE != "NA" & EMPLOYER_NAME == "INFOSYS LIMITED" 
                        & JOB_TITLE == "TECHNOLOGY LEAD - US", select = c(YEAR,PREVAILING_WAGE, WORKSITE_STATE))

# factor the case status to required levels
# newdata.frame$CASE_STATUS <- factor(newdata.frame$CASE_STATUS)

# Convert the prevailing wage to numeric values
newdata.frame$PREVAILING_WAGE <- as.numeric(sub(",","", newdata.frame$PREVAILING_WAGE))

#-------------------------PLOT THE DATA-----------------------------------------
# plot the graph for prevailing wage for each year for the selected state 
ggplot (subset(newdata.frame,WORKSITE_STATE == "CA"), aes(YEAR,PREVAILING_WAGE)) +geom_point()

# Create new data frame with summarized mean prevailing wage for selected state
nmatrix<-ddply(subset(newdata.frame,WORKSITE_STATE == "CA"),~YEAR,summarize, mean=mean(PREVAILING_WAGE))

# Create new data frame with summarized mean prevailing wage for all the states
#nmatrix<-ddply(newdata.frame,~YEAR,summarize, mean=mean(PREVAILING_WAGE))

# Plot the data 
ggplot(data=nmatrix,aes(YEAR,mean))+geom_smooth(method="lm")+ggtitle("H1-B Mean Salary\n")

#----------------------------------TRAINING MODEL----------------------------------
# Apply linear regression to the training data set
lm<-lm(mean~YEAR,data=nmatrix)
summary(lm)

# Verify the fitted value and residuals for the train data linear model
nmatrixfitting <- data.frame(nmatrix , fitted.value= fitted (lm), residual= resid (lm))

# Verify the model fit - lwr and upr values
predict(lm,interval="confidence")

#-------------------------------------TEST DATA-------------------------------------
# Create a data from with the year for which the prevailing wage is to be predicted
newyear <- data.frame(YEAR = 2016)

# Predict the prevailing wage by applying the lm and update in the data frame
newyear$mean <- predict(lm,newyear,type = "response")
