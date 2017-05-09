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
library(arules)
library(arulesViz)
library(stringr)
library(e1071)

data <- readRDS('h1b_transformed.rds')
h1b_transformed_na <- readRDS("h1b_transformed_without_na.rds")

#1b_df <- as.data.frame(data)
  
source("helper.R")

#Rules <- association(data)
#newdata3 <- glmModel(data)
Rules <- readRDS("Rules.rds")
newdata3 <- readRDS("glmModel.rds")

shinyServer(function(input, output) {
  
  print("Starting Shiny Server....")
  
  #shinyjs::hide(id ="wage_input", anim = TRUE)
  
  #Define Reactive Inputs
  reactive_inputs <- reactiveValues(
    year = as.character(seq(2011,2016)), 
    metric = "num_applications",
    #algorithm_input = "choose_input",
    location = "ALL STATES",
    wage_value = "300000",
    job_title = "WEB DEVELOPER",
    stateA = "LOUISIANA",
    stateB = "CALIFORNIA",
    stateOne = "CA",
    stateTwo = "TX",
    stateThree = "IL",
    stateW = "MI",
    PYear = "2018",
    Empname = "IBM INDIA PRIVATE LIMITED",
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
    reactive_inputs$stateOne <- input$stateOne
    reactive_inputs$stateTwo <- input$stateTwo
    reactive_inputs$stateThree <- input$stateThree
    reactive_inputs$stateW <- input$stateW
    reactive_inputs$PYear <- input$PYear
    reactive_inputs$Empname <- input$Empname
    
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
  
  output$apprioriPlot <- renderPlot({
    inspect(Rules)
    plot(Rules, method="paracoord", control=list(reorder=TRUE))
  })
  
  output$logisticPlot <- renderPlot({
    
    ggplot(subset(newdata3, WORKSITE_STATE == reactive_inputs$stateOne |
                    WORKSITE_STATE == reactive_inputs$stateTwo |
                    WORKSITE_STATE == reactive_inputs$stateThree),
           aes(x = PREVAILING_WAGE, y = PredictedProb)) +
      geom_ribbon(data = subset(newdata3, WORKSITE_STATE == reactive_inputs$stateOne|
                                  WORKSITE_STATE == reactive_inputs$stateTwo |
                                  WORKSITE_STATE == reactive_inputs$stateThree),
                  aes(ymin = LL, ymax = UL, fill = WORKSITE_STATE), alpha = 0.2) +
      geom_line(aes(colour = WORKSITE_STATE),size = 1)
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
      
      data_total <- filter(data)
    } else {
      data_certified <- filter(data, YEAR %in% reactive_inputs$year & JOB_TITLE == reactive_inputs$job_title & 
                                 CASE_STATUS == "CERTIFIED" )
      
      data_denied <- filter(data, YEAR %in% reactive_inputs$year & JOB_TITLE == reactive_inputs$job_title & 
                              CASE_STATUS == "DENIED" )
      
      data_total <- filter(data, JOB_TITLE == reactive_inputs$job_title) 
    }
    
    newtable <- merge(data_certified %>% dplyr::group_by(EMPLOYER_NAME) %>% summarise(count = n()) %>% arrange(desc(count)),
                      data_denied %>% dplyr::group_by(EMPLOYER_NAME) %>% summarise(count = n()) %>% arrange(desc(count)), 
                      by  = "EMPLOYER_NAME")
    
    new <- merge(newtable,
                 data_total %>% group_by(EMPLOYER_NAME) %>% summarise(count = n()) %>% arrange(desc(count)), 
                 by  = "EMPLOYER_NAME")
    
    colnames(new) <- c("EMPLOYER_NAME", "CERTIFIED", "DENIED", "TOTAL")
    
    df16 <- data.frame(PERCENTAGE = rep((new$DENIED/new$TOTAL)*100), new[,])
    
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
  
  
  output$heatmap <- renderPlot({
    map_df <- data
    if(reactive_inputs$job_title != "") {
      map_df <-  filter(data, JOB_TITLE == reactive_inputs$job_title)
    }
    
    temp1 <- head(plyr::arrange(plyr::count(subset(map_df, YEAR %in% reactive_inputs$year),  
                                            vars = "WORKSITE_CITY"),plyr::desc(freq)), n = reactive_inputs$slider_value)
    
    usa_geocodes <- readRDS("geocodes.rds")
    USA = map_data(map = "state")
    
    temp1$lat <- usa_geocodes[match(temp1$WORKSITE_CITY,usa_geocodes$WORKSITE_CITY),2]
    temp1$lon <- usa_geocodes[match(temp1$WORKSITE_CITY,usa_geocodes$WORKSITE_CITY),1]
    
    
    usa <- map_data("usa") # we already did this, but we can do it again
    ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +geom_point(data=temp1, aes_string(x="lon", y="lat", label = "WORKSITE_CITY"), color="yellow", size=2)+
      coord_fixed(1.3)+  coord_map(ylim = c(23,50),xlim=c(-130,-65)) + geom_text(data = temp1, aes(label = WORKSITE_CITY, x = lon, y = lat), hjust = 0,color="red",size=3.5)
    
  })
  output$svm_algo  <- renderPlot({
    
    print("In svm algo function")
    
    svm_df <- h1b_df[1:100,]
    svm_df$PREVAILING_WAGE <- as.numeric(as.character(svm_df$PREVAILING_WAGE))
    svm_df$CASE_STATUS <- as.character(svm_df$CASE_STATUS)
    #svm_df$EMPLOYER_NAME <- as.character(svm_df$EMPLOYER_NAME)
    
    svm_df[svm_df$CASE_STATUS == "CERTIFIED",]$CASE_STATUS = 1
    svm_df[svm_df$CASE_STATUS == "DENIED",]$CASE_STATUS = -1
    
    svm_df$UNIVERSITY <- as.factor(ifelse(str_detect(svm_df$EMPLOYER_NAME, "UNIVERSITY"), 1, 0))
    
    
    # PREVAILING_WAGE 
    #fewdata <- svm_df[1:100,]
    svm_df <- subset(svm_df,select=c(PREVAILING_WAGE,CASE_STATUS))
    
    svm_df$CASE_STATUS <- as.numeric(svm_df$CASE_STATUS)
    
    
    model <- svm(CASE_STATUS ~ PREVAILING_WAGE, svm_df)
    
    print("After Model")
    
    
    # make a prediction for each X
    predictedY <- data.frame(predict(model, svm_df))
    colnames(predictedY) <- c("PREDICTED_VALUE")
    
    #predicted_vals <- data.frame(PREVAILING_WAGE = svm_df$PREVAILING_WAGE,CASE_STATUS = predictedY)
    #svm_plot_df <- rbind( svm_df, predicted_vals)
    svm_plot_df <- merge(svm_df,predictedY)
    
    # p <- ggplot(svm_plot_df, aes(PREVAILING_WAGE, CASE_STATUS))+points(svm_df$PREVAILING_WAGE,predictedY , col = "blue", pch=4)
    # p + geom_point()
    
    ggplot(svm_plot_df, aes(x=PREVAILING_WAGE)) +
      geom_point(aes(y=CASE_STATUS,colour ="blue"))+
      geom_point(aes(y=PREDICTED_VALUE,colour ="red")) 
    
  })  
  
  output$random_forest_algo <- renderPlot({
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
    
    #select the first 1000 case statuses
    select <- train_na[1:1000,]
    
    # Implementing randomForest on the data
    #install.packages("randomForest")
    #library(randomForest)
    
    # Random forest training 1
    rf.train.1 <- data.combined.na[1:1000 ,c("FULL_TIME_POSITION","PREVAILING_WAGE")]
    rf.label <- as.factor(select$CASE_STATUS)
    rf.label <- factor(rf.label)
    
    set.seed(1234)
    rf.1 <- randomForest(x= rf.train.1,y=rf.label, importance=TRUE,ntree = 1000)
    rf.1
    varImpPlot(rf.1)
  })
  
  
  output$wagePredict <- renderText({
    print("HI VAish")
    wage_Predict()
  })
  wage_Predict <- reactive({
    if (reactive_inputs$Empname != "" & reactive_inputs$job_title != "" &
        reactive_inputs$stateW != "" & reactive_inputs$PYear != ""){
      print(reactive_inputs$Empname)
      print(reactive_inputs$job_title)
      print(reactive_inputs$stateW)
      print(reactive_inputs$PYear)
      newdata.frame <- subset(data, CASE_STATUS == "CERTIFIED"
                              & PREVAILING_WAGE != "NA" & EMPLOYER_NAME == reactive_inputs$Empname 
                              & JOB_TITLE == reactive_inputs$job_title, 
                              select = c(YEAR,PREVAILING_WAGE, WORKSITE_STATE))
      newdata.frame$PREVAILING_WAGE <- as.numeric(sub(",","", newdata.frame$PREVAILING_WAGE))
      #-------------------------PLOT THE DATA-----------------------------------------
      #ggplot (subset(newdata.frame,WORKSITE_STATE == reactive_inputs$stateW), aes(YEAR,PREVAILING_WAGE)) +geom_point()
      
      nmatrix<-ddply(subset(newdata.frame,WORKSITE_STATE == reactive_inputs$stateW),~YEAR,summarize, mean=mean(PREVAILING_WAGE))
      print("HI VAish3")
      #ggplot(data=nmatrix,aes(YEAR,mean))+geom_smooth(method="lm")+ggtitle("H1-B Mean Salary\n")
      print(nmatrix)
      print(nrow(nmatrix))
      if (nrow(nmatrix) >1)
      {
        lm<-lm(mean~YEAR,data=nmatrix)
        summary(lm)
        nmatrixfitting <- data.frame(nmatrix , fitted.value= fitted (lm), residual= resid (lm))
        
        predict(lm,interval="confidence")
        #-------------------------------------TEST DATA-------------------------------------
        
        newyear <- data.frame(YEAR = reactive_inputs$PYear)
        newyear$YEAR <- as.numeric(sub(",","", newyear$YEAR))
        print("vi")
        newyear$mean <- predict(lm,newyear,type = "response")
      }
      else if(nrow(nmatrix) == 1){
        print("Employer applied H1B for only one year for this job title.")
      }
      else{
        print("No Previous data available.")
      }
    }
    else{
      print("")
    }
  })
  
})