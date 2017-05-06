
#datadf <- readRDS('h1b_transformed.rds')
#input_array <- c('data scientist','data engineer','machine learning')
#job_input <-data.frame()
#job_input <-data.frame(job_title_filter(datadf,input_array ))


  
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

plot_output <- function(df, x_feature,fill_feature,metric, xlabb,ylabb) {  
  options(scipen = 999)
  
  g <- ggplot(df, aes_string(x=x_feature,y=metric)) +
    geom_bar(stat = "identity", aes_string(fill = fill_feature), position = "dodge") + 
    coord_flip() + xlab(xlabb) + ylab(ylabb) + get_theme()
  
  return(g)
}

plot_input <- function(df, x_feature, fill_feature, metric,filter = FALSE, ...) {

  top_x <- unlist(find_top(df,x_feature,metric, ...))
  

  
  filter_criteria <- interp(~x %in% y, .values = list(x = as.name(x_feature), y = top_x))
  arrange_criteria <- interp(~ desc(x), x = as.name(metric))
  
  if(filter == TRUE) {
    df %>%
      filter_(filter_criteria) -> df
  }
  
  return(df %>% 
           group_by_(.dots=c(x_feature,fill_feature)) %>% 
           mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
           summarise(TotalApps = n(),CertiApps = sum(certified), Wage = median(PREVAILING_WAGE)))
}


find_top <- function(df,x_feature,metric, Ntop = 3) {
  arrange_criteria <- interp(~ desc(x), x = as.name(metric))
  
  df %>% 
    group_by_(x_feature) %>% 
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
    summarise(TotalApps = n(),
              Wage = median(PREVAILING_WAGE), 
              CertiApps = sum(certified)) %>%
    arrange_(arrange_criteria) -> top_df
  
  top_len <- min(dim(top_df)[1],Ntop)
  
  return(top_df[1:top_len,1])
}


get_theme <- function() {
  # Function for ggplot2 graphics parameters
  return(
    theme(axis.title = element_text(size = rel(1.5)),
          legend.position = "right",
          legend.text = element_text(size = rel(1.5)),
          legend.title = element_text(size=rel(1.5)),
          axis.text = element_text(size=rel(1.5))) 
  )
}