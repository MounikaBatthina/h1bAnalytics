#install.packages("ggrepel")
#install.packages("rdrop2")


library(shiny)
library(ggplot2)
library(dplyr)
library(lazyeval)
library(hashmap)
library(ggrepel)
library(stats)
library(rdrop2)

#data <- readRDS('h1b_transformed.rds')
#h1b_transformed_na <- readRDS("h1b_transformed_without_na.rds")

#1b_df <- as.data.frame(data)
  
source("helper.R")

shinyServer(function(input, output, session) {
  
  print("Starting Shiny Server....")
  
  #shinyjs::hide(id ="wage_input", anim = TRUE)
  
  #Define Reactive Inputs
  reactive_inputs <- reactiveValues(
    year = as.character(seq(2011,2016)), 
    metric = "num_applications",
    location = "ALL STATES",
    wage_value = "300000",
    job_title = "WEB DEVELOPER",
    stateA = "LOUISIANA",
    stateB = "CALIFORNIA",
    slider_value = 15)
  
  #Event Observer for Reactive Values
  observeEvent(input$compute_result,{
    print("Event Observer...")
    
    reactive_inputs$year <- as.character(seq(input$year[1], input$year[2]))
    reactive_inputs$location <- input$location
    reactive_inputs$wage_value <- input$wage_value
    reactive_inputs$job_title <- input$job_title
    reactive_inputs$metric <- input$metric
    reactive_inputs$slider_value <- input$slider_value
    reactive_inputs$stateA <- input$stateA
    reactive_inputs$stateB <- input$stateB
    
    print(input$state_1)
    print(input$state_2)
  })
  
  observe({
    if(input$metric == "num_applications") {
      session$sendCustomMessage(type="jsCode",
                                list(code= "$('#ex').hide()"))
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
    else if(reactive_inputs$metric == "wage_rate")
      wage_rate()
    else if(reactive_inputs$metric == "case_denied")
      case_denied()
  })
  
  #Apriori
  output$wageCompare <-renderPlot({
    print("Compare Wage Plot...")
    
    wage_compare()
  })
  
  num_applications <- reactive({
    print("Number of Applications...")
    temp <- head(plyr::arrange(plyr::count(subset(data, YEAR %in% reactive_inputs$year),  
                               vars = "EMPLOYER_NAME"),plyr::desc(freq)), n = reactive_inputs$slider_value)
    
    ggplot(temp, aes(x=strtrim(EMPLOYER_NAME, 20), y=freq)) + 
      geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  case_status <- reactive({
    print("Case Status...")
    if(reactive_inputs$job_title == "") {
      df <- subset(data, YEAR %in% reactive_inputs$year)
    } else {
      df <- subset(data, YEAR %in% reactive_inputs$year & JOB_TITLE == "PROGRAMMER ANALYST")
    }
    
    temp <- head(plyr::arrange(plyr::count(df, vars = "EMPLOYER_NAME"),plyr::desc(freq)), n = reactive_inputs$slider_value)
    
    temp1 <- df[(df$EMPLOYER_NAME %in% temp$EMPLOYER_NAME), ]
    
    ggplot(temp1, aes(x = strtrim(EMPLOYER_NAME, 20) ,fill=factor(CASE_STATUS))) + 
      geom_bar(width = 0.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  wage_rate <- reactive({
    print("Wage Rate...")
    
    newdf <- data.frame(Wage = c("0-100k", "100k-200k", "200k-300k",">=300k"), 
                        Total = c(2423337, 277084, 6931, 2640), 
                        Denied = c(82717, 8599, 718, 2339))
    newdf$Wage <- factor(newdf$Wage, levels = newdf$Wage)
    
    ggplot(newdf, aes(x = Wage ,y = (newdf$Denied/newdf$Total)*100)) + 
      geom_bar(stat = "identity") + xlab("Wage Rate") + ylab("% Denied")
  })
  
  case_denied <- reactive({
    print("Case Denied...")
    #detach("package:plyr", unload=TRUE)
    library(dplyr)
    
    #f_df <- dplyr::filter(mydata, JOB_TITLE == "PROGRAMMER ANALYST" & CASE_STATUS == "CERTIFIED")
    
    if(reactive_inputs$job_title == "") {
      data_certified <- filter(data, YEAR %in% reactive_inputs$year & 
                                 CASE_STATUS == "CERTIFIED" )
      
      data_denied <- filter(data, YEAR %in% reactive_inputs$year & 
                              CASE_STATUS == "DENIED" )
      
      data_total <- filter(mydata)
    } else {
      data_certified <- filter(data, YEAR %in% reactive_inputs$year & JOB_TITLE == reactive_inputs$job_title & 
                                 CASE_STATUS == "CERTIFIED" )
      
      data_denied <- filter(data, YEAR %in% reactive_inputs$year & JOB_TITLE == reactive_inputs$job_title & 
                              CASE_STATUS == "DENIED" )
      
      data_total <- filter(mydata, JOB_TITLE == reactive_inputs$job_title) 
    }
    
    newtable <- merge(data_certified %>% dplyr::group_by(EMPLOYER_NAME) %>% summarise(count = n()) %>% arrange(desc(count)),
                      data_denied %>% dplyr::group_by(EMPLOYER_NAME) %>% summarise(count = n()) %>% arrange(desc(count)), 
                      by  = "EMPLOYER_NAME")
    
    new <- merge(newtable,
                 data_total %>% group_by(EMPLOYER_NAME) %>% summarise(count = n()) %>% arrange(desc(count)), 
                 by  = "EMPLOYER_NAME")
    
    colnames(new) <- c("EMPLOYER_NAME", "CERTIFIED", "DENIED", "TOTAL")
    
    df16 <- data.frame(PERCENTAGE = rep(new$DENIED/new$TOTAL), new[,])
    
    df16 <- arrange(df16, desc(TOTAL))
    
    ggplot(head(arrange(df16, desc(TOTAL)), n = reactive_inputs$slider_value), aes(x = strtrim(EMPLOYER_NAME, 20), 
                                                                                   y = PERCENTAGE)) + 
      geom_bar(stat = "identity") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  wage_compare <- reactive({
    print("Compare Wages...")
    
    print(reactive_inputs$stateA)
    print(reactive_inputs$stateB)
    print(reactive_inputs$job_title)
    
    # Dividing h1b_transformed_na into train and test data
    train_na = h1b_transformed_na[1:1299875,]
    test_na = h1b_transformed_na[1299876:2599750,]
    
    # Replacing the CASE_STATUS of Test data with None in h1b_na data
    test.case.na <- data.frame(CASE_STATUS=rep("None", nrow(test_na)), test_na[,])
    test.case.na$CASE_STATUS.1 <- NULL
    
    # Combine test and train datasets
    data.combined.na <- rbind(train_na,test.case.na)
    
    ed <- extractData(data.combined.na,reactive_inputs$job_title, reactive_inputs$stateA)
    ed1 <- extractData(data.combined.na,reactive_inputs$job_title, reactive_inputs$stateB)
    
    # Average wage for that job title and worksite state 
    avg <- getAverageWage(ed)
    avg1 <- getAverageWage(ed1)
    
    compdata <- data.frame(State = c(reactive_inputs$stateA,reactive_inputs$stateB),Average = c(avg,avg1))
    ggplot(compdata,aes(x= State,y = Average)) + geom_bar(stat = "identity",width = 0.5)
  })
  
  
  
})