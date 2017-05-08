h1b_transformed_na <- readRDS("h1b_transformed_without_na.rds")


# Dividing h1b_transformed_na into train and test data
train_na = h1b_transformed_na[1:1299875,]
test_na = h1b_transformed_na[1299876:2599750,]

# Replacing the CASE_STATUS of Test data with None in h1b_na data
test.case.na <- data.frame(CASE_STATUS=rep("None", nrow(test_na)), test_na[,])
test.case.na$CASE_STATUS.1 <- NULL

# Combine test and train datasets
data.combined.na <- rbind(train_na,test.case.na)



# Filtering dataset based on Jobtitle and Worksite_state
extractData <- function(df,job_title, worksite_state){
  extracted.data <- subset(df, JOB_TITLE == job_title & WORKSITE_STATE_FULL == worksite_state & CASE_STATUS=="CERTIFIED")
  return (extracted.data)
}

# Function to calculate average wage based on input dataframe
getAverageWage <- function(df){
  average_wage <- mean(df$PREVAILING_WAGE)
}

# Hard-coded values that has the user input for Job_title and State 
# extracting dataframes based on user inputs
ed <- extractData(data.combined.na,"WEB DEVELOPER","LOUISIANA")
ed1 <- extractData(data.combined.na,"WEB DEVELOPER","CALIFORNIA")

# Average wage for that job title and worksite state 
avg <- getAverageWage(ed)
avg1 <- getAverageWage(ed1)

# avg and avg1 are the respective average wages in the respective states.
