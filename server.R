#install.packages("ggrepel")
#install.packages("rdrop2")


library(shiny)
library(ggplot2)
library(dplyr)
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
  
  reactive_inputs <- reactiveValues(job_list = c('data scientist','data engineer','machine learning'),
                                    employer_list = c('INFOSYS BPO LIMITED','COGNIZANT TECHNOLOGY SOLUTIONS ','TATA COMMUNICATIONS (AMERICA), INC.'), 
                                    year = as.character(seq(2011,2016)), 
                                    metric = "TotalApps",
                                    location = "USA",
                                    Ntop = 3)
  
  
  observeEvent(input$refresh,{
    
    #job_list <- reactive_inputs$job_list  
    
    #employer_list <- inputs
    
    reactive_inputs$year <- as.character(seq(input$year[1], input$year[2]))
    reactive_inputs$metric <- input$metric
    
    reactive_inputs$location <- input$location
    
    reactive_inputs$Ntop <- input$Ntop
  })
  
  # Filter year input
  year_input <- reactive({
    h1b_df %>%
      filter(YEAR %in% reactive_inputs$year)
  })
  
  # Filter location input
  location_input <- reactive({
    if(reactive_inputs$location == 'USA') year_input() else year_input() %>% filter(WORKSITE_STATE_FULL == reactive_inputs$location)
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
  
  
})
