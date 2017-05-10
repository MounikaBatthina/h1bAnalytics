# Filtering dataset based on Jobtitle and Worksite_state
extractData <- function(df,job_title, worksite_state) {
  extracted.data <- subset(df, JOB_TITLE == job_title & WORKSITE_STATE_FULL == worksite_state & CASE_STATUS=="CERTIFIED")
  return (extracted.data)
}

# Function to calculate average wage based on input dataframe
getAverageWage <- function(df) {
  average_wage <- mean(df$PREVAILING_WAGE)
}

association <- function(data) {
  print("Association Rule Plot...")
  df16_subset <- data[c(2:14)]
  
  #str(df16_subset)
  
  df16_subset$WAGE_RATE_OF_PAY_FROM <-  NULL
  df16_subset$WORKSITE_STATE_FULL <- NULL
  df16_subset$WORKSITE_CITY <- NULL
  df16_subset$EMPLOYER_CITY <- NULL
  df16_subset$SOC_CODE <- NULL
  
  df16_subset$PREVAILING_WAGE <- as.factor(df16_subset$PREVAILING_WAGE)
  df16_subset$YEAR <- as.factor(df16_subset$YEAR)
  
  subset <- df16_subset[!duplicated(df16_subset[c(1:8)]),]
  
  
  Rules <- apriori(subset, parameter = list(minlen=2, supp=0.05, conf=0.6), 
                   appearance = list(rhs=c("CASE_STATUS=CERTIFIED"), 
                                     default="lhs"), control = list(verbose=F))
  saveRDS(Rules, "Rules.rds")
  return (Rules)
}

glmModel <- function(mydata) {
  # Subset the required data, Select CERTIFIED AND DENIED case status
  newdata <- subset(mydata, (CASE_STATUS == "CERTIFIED" | CASE_STATUS == "DENIED") & 
                      PREVAILING_WAGE != "NA",
                    select = c(CASE_STATUS, PREVAILING_WAGE, WORKSITE_STATE))
  
  # Factor the case status to two levels
  newdata$CASE_STATUS <- factor(newdata$CASE_STATUS)
  
  # Factor the WORKSITE STATE
  newdata$WORKSITE_STATE <- factor(newdata$WORKSITE_STATE)
  
  # Converting PREVAILING WAGE as numeric data
  newdata$PREVAILING_WAGE <- as.numeric(sub(",","", newdata$PREVAILING_WAGE))
  
  # Convert the STATUS as character to assign '1' to 'CERTIFIED and '0' to DENIED status
  newdata$CASE_STATUS <- as.character(newdata$CASE_STATUS)
  newdata[newdata$CASE_STATUS == "CERTIFIED",]$CASE_STATUS = 1
  newdata[newdata$CASE_STATUS == "DENIED",]$CASE_STATUS = 0
  
  # Convert the STATUS as character to assign '0' to 'CERTIFIED and '1' to DENIED status
  #newdata$CASE_STATUS <- as.character(newdata$CASE_STATUS)
  #newdata[newdata$CASE_STATUS == "CERTIFIED",]$CASE_STATUS = 0
  #newdata[newdata$CASE_STATUS == "DENIED",]$CASE_STATUS = 1
  
  # Convert the 0 & 1 values to numeric to apply standard deviation
  newdata$CASE_STATUS <- as.numeric(sub(",","", newdata$CASE_STATUS))
  
  # Apply standard deviation on the trasformed data
  #sapply(newdata,sd)
  #summary(newdata)
  
  #-------------------TRAIN DATA------------------------------------------------------------------
  # Apply logistic regression (generalized liner model function) on the training data set
  # CASE STATUS is set as a function of PREVAILING WAGE and WORKSITE STATE factors
  mylogit <- glm(CASE_STATUS ~ PREVAILING_WAGE + WORKSITE_STATE, 
                 data = subset(newdata, PREVAILING_WAGE < 200000), family = "binomial")
  summary(mylogit)
  # confint(mylogit)
  exp(coef(mylogit))
  
  #---------------------------------------TEST DATA 1----------------------------------------------------------------
  # Create test data with PREVAILING WAGE as mean wage value of the entire data set and
  # for each unique list of WORKSITE STATES
  newdata1 <- with(newdata, data.frame(PREVAILING_WAGE = mean(PREVAILING_WAGE), 
                                       WORKSITE_STATE = unique(WORKSITE_STATE)))
  
  # Predict the probability of being certified for the test data and store for each values in a new column
  newdata1$Prob <- predict(mylogit,newdata = newdata1, type = "response")
  
  # Order the States in data frame with respect to the probability predicted
  newdata1$WORKSITE_STATE <- factor(newdata1$WORKSITE_STATE, 
                                    levels = newdata1$WORKSITE_STATE[order(newdata1$Prob)])
  
  #----------------------------PLOT THE PROBABILITY FOR GIVEN WAGE IN ALL STATES---------------------------------------
  # Ploting probabilities vs state for a given prevailing wage (here mean wage of entire data is considered
  #ggplot (newdata1, aes(WORKSITE_STATE,Prob)) +geom_point()
  #ggplot (subset(newdata1,WORKSITE_STATE != "FM" & WORKSITE_STATE != ""), aes(WORKSITE_STATE,Prob)) +geom_point()
  
  #----------------------------------------------TEST DATA 2-----------------------------------
  # Create new dataframe with the main data 
  # Prevailing wage range from random wages between min - max wage and all the 57 unique state values
  newdata2 <- with(subset(newdata,PREVAILING_WAGE < 200000), 
                   data.frame(PREVAILING_WAGE = rep(seq(from = min(PREVAILING_WAGE), 
                                                        to = max(PREVAILING_WAGE),
                                                        length.out = 100), 60), 
                              WORKSITE_STATE = factor(rep(unique(WORKSITE_STATE), each = 100))))
  
  # Predict the probalilities 
  newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",se = TRUE))
  
  newdata3 <- within(newdata3, {
    PredictedProb <- plogis(fit)
    LL<- plogis(fit - (1.96 *se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
  
  head(newdata3)
  saveRDS(newdata3, "glmModel.rds")
  return (newdata3)
  
  #------------------------------PLOT THE PROBABILITY CURVE FOR WAGE RANGE FOR SPECIFIED STATES----------------------
  #ggplot(subset(newdata3,WORKSITE_STATE == "CA"), aes(x = PREVAILING_WAGE, y = PredictedProb)) + geom_line(aes(colour = WORKSITE_STATE),size = 1)
  #ggplot(subset(newdata3,WORKSITE_STATE != "FM" & WORKSITE_STATE != ""), aes(x = PREVAILING_WAGE, y = PredictedProb)) + geom_line(aes(colour = WORKSITE_STATE),size = 1)
  
  # ggplot(subset(newdata3,WORKSITE_STATE == "CA" | 
  #                 WORKSITE_STATE == "TX" | 
  #                 WORKSITE_STATE == "IL"), 
  #        aes(x = PREVAILING_WAGE, y = PredictedProb)) + 
  #   geom_ribbon(data = subset(newdata3, WORKSITE_STATE == "CA" | 
  #                               WORKSITE_STATE == "TX" | 
  #                               WORKSITE_STATE == "IL"), 
  #               aes(ymin = LL, ymax = UL, fill = WORKSITE_STATE), alpha = 0.2) + 
  #   geom_line(aes(colour = WORKSITE_STATE),size = 1)
}