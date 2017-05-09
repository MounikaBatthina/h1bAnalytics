job_title_filter <- function(datadf,input_array){
  
  if(length(input_array) ==0){
    return(datadf)  
  }
  filtered_data <- data.frame()
  
  for(type in input_array){
    filtered_data <- rbind(filtered_data, datadf %>%
                             filter(regexpr(type,JOB_TITLE,ignore.case=TRUE) != -1
                             ))
  }
  return(unique(filtered_data))
  
}

employer_filter <- function(df, input_vec) {
  
  if(length(input_vec) == 0) {
    return(df)
  }
  
  new_df <- data.frame()
  
  for(value in input_vec){
    new_df <- rbind(new_df, df %>% 
                      filter(regexpr(value,EMPLOYER_NAME,ignore.case=TRUE) != -1))
  }
  return(unique(new_df))
}

# Filtering dataset based on Jobtitle and Worksite_state
extractData <- function(df,job_title, worksite_state) {
  extracted.data <- subset(df, JOB_TITLE == job_title & WORKSITE_STATE_FULL == worksite_state & CASE_STATUS=="CERTIFIED")
  return (extracted.data)
}

# Function to calculate average wage based on input dataframe
getAverageWage <- function(df) {
  average_wage <- mean(df$PREVAILING_WAGE)
}