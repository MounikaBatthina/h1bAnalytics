#install.packages("ggrepel")
#install.packages("rdrop2")


library(shiny)
library(ggplot2)
library(plyr)
library(lazyeval)
library(hashmap)
library(ggrepel)
library(maps)
library(stats)
library(rdrop2)
library(mapproj)

data <- readRDS('h1b_transformed.rds')

h1b_df <- as.data.frame(data)
  
source("helper.R")

metric_lab_hash <- hashmap(c("TotalApps","CertiApps","Wage"),c("TOTAL H-1B VISA APPLICATIONS", "CERTIFIED H-1B VISA APPLICATIONS","MEDIAN PREVAILING WAGE"))
USA = map_data(map = "usa")

shinyServer(function(input, output) {
  print("Starting Shiny Server....")
  
  #Define Reactive Inputs
  reactive_inputs <- reactiveValues(
    job_list = c('data scientist','data engineer','machine learning'),
    employer_list = c('INFOSYS BPO LIMITED','COGNIZANT TECHNOLOGY SOLUTIONS ','TATA COMMUNICATIONS (AMERICA), INC.'), 
    
    year = as.character(seq(2011,2016)), 
    metric = "num_applications",
    location = "ALL STATES",
    slider_value = 15)
  
  #Event Observer for Reactive Values
  observeEvent(input$compute_result,{
    print("Event Observer...")
    reactive_inputs$year <- as.character(seq(input$year[1], input$year[2]))
    reactive_inputs$location <- input$location
    
    
    reactive_inputs$metric <- input$metric
    reactive_inputs$slider_value <- input$slider_value
  })
  
  # Filter year input
  year_input <- reactive({
    h1b_df %>%
      filter(YEAR %in% reactive_inputs$year)
  })
  
  # Filter location input
  location_input <- reactive({
    if(reactive_inputs$location == 'USA') 
      year_input() 
    else year_input() %>% filter(WORKSITE_STATE_FULL == reactive_inputs$location)
  })

  job_input <- reactive({
    job_title_filter(location_input(),reactive_inputs$job_list)
  })
  

  employer_input <- reactive({
   
    if(dim(job_input())[1] == 0) {
      employer_filter(location_input(),reactive_inputs$employer_list)
    } else {
      employer_filter(job_input(),reactive_inputs$employer_list)
    }
  })
  
  
  # Final input data frame for plotting
  data_input <- reactive({
    
    if(dim(employer_input())[1] == 0 & dim(job_input())[1] == 0) {
      location_input() 
    } else if (dim(employer_input())[1] == 0 & dim(job_input())[1] > 0){
      job_input()
    } else if (dim(employer_input())[1] > 0 & dim(job_input())[1] == 0){
      employer_input() 
    } else {
      employer_input()
    }
  })
  
  plot_input_job <- reactive({
    plot_input(data_input(),"JOB_TITLE", "YEAR",reactive_inputs$metric,filter = TRUE, Ntop = reactive_inputs$Ntop)
    
  })
  
  output$jobTitlePlot <- renderPlot({
    plot_output(plot_input_job(),"JOB_TITLE","YEAR", reactive_inputs$metric, "JOB TYPE",
                metric_lab_hash[[reactive_inputs$metric]])
  })
  
  # Analytics Tab - Plot between Employer & Number of applications
  output$analyticsPlot <- renderPlot({
    print("Analytics Plot...")
    if(reactive_inputs$metric == "num_applications")
      num_applications()
    else if(reactive_inputs$metric == "case_status")
      case_status()
    else if(reactive_inputs$metric == "case_certified")
      print("Case Certified...")
  })
  
  num_applications <- reactive({
    print("Number of Applications...")
    temp <- head(arrange(count(subset(data, YEAR %in% reactive_inputs$year),  
                               vars = "EMPLOYER_NAME"),desc(freq)), n = reactive_inputs$slider_value)
    
    ggplot(temp, aes(x=strtrim(EMPLOYER_NAME, 20), y=freq)) + 
      geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  case_status <- reactive({
    print("Case Status...")
    df <- subset(data, YEAR %in% reactive_inputs$year & JOB_TITLE == "PROGRAMMER ANALYST")
    temp <- head(arrange(count(df, vars = "EMPLOYER_NAME"),desc(freq)), n = reactive_inputs$slider_value)
    
    temp1 <- df[(df$EMPLOYER_NAME %in% temp$EMPLOYER_NAME), ]
    
    ggplot(temp1, aes(x = strtrim(EMPLOYER_NAME, 20) ,fill=factor(CASE_STATUS))) + 
      geom_bar(width = 0.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
  
})