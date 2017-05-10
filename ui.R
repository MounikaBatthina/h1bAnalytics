library(shiny)
library(shinythemes)
library(shinyjs)

#List of US states
statesList = toupper(c("all states","alaska","alabama","arkansas","arizona","california","colorado",
                   "connecticut","district of columbia","delaware","florida","georgia",
                   "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                   "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                   "missouri","mississippi","montana","north carolina","north dakota",
                   "nebraska","new hampshire","new jersey","new mexico","nevada",
                   "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                   "rhode island","south carolina","south dakota","tennessee","texas",
                   "utah","virginia","vermont","washington","wisconsin",
                   "west virginia","wyoming"))
state_list <- as.list(statesList)
names(state_list) <- statesList

shortStatesList = toupper(c("CA","TX","FL","DC","NC","RI","IL","OH","WA","MI","GA","NY",
                            "NJ","MA","VA","IN","MD","MO","NV","LA","MN","CO","TN","PA",
                            "MS","UT","DE","VI","WI","CT","GU","MP","AZ","OK","AR","KY",
                            "PR","SC","KS","HI","AK","WY","IA","ID","NE","NM","WV","SD",
                            "OR","ND","ME","AL","VT","NH","MT","FM","MH","PW","AS"))
short_state_list <- as.list(shortStatesList)
names(short_state_list) <- shortStatesList

shinyUI(fluidPage(
  id ="inputs",  
  theme = shinythemes::shinytheme("yeti"),
  # Page title
  titlePanel("H-1B Data Analysis"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Year:", min = 2011, max = 2016, value = c(2011,2016)),
      
      sliderInput("slider_value", "Number of Results", min = 5, max = 30, value = 15),
      
      selectInput("metric", "Metric", c("Number of Visa Applications" = "num_applications",
                    "Case Status" = "case_status",
                    "Case Denied" = "case_denied",
                    "Denied Wage Rate" = "wage_rate")),
      
      textInput("job_title", "Job Title","WEB DEVELOPER"),
      # actionButton("toggle", "Toggle the following text"),
      # conditionalPanel(
      #   condition = "input.toggle % 2 == 0",
      #   textInput("job_title", "Job Title",""),
      # ),
      
      # selectInput("location",
      #             h3("Location"),
      #             choices = state_list)
      
      actionButton("compute_result","Compute Results", icon = icon("refresh"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Random Forest", plotOutput("random_forest_algo")),
        
        tabPanel("AR Mining", plotOutput("apprioriPlot"), plotOutput("apprioriPlot1")),
        
        tabPanel("LogIt", div(style="display:inline-block",selectInput("stateOne", "State 1",choices = short_state_list)),
                 div(style="display:inline-block",selectInput("stateTwo", "State 2",choices = short_state_list)),
                 div(style="display:inline-block",selectInput("stateThree", "State 3",choices = short_state_list)),
                 plotOutput("logisticPlot")),
        
        tabPanel("SVM", plotOutput("svm_algo")),
        
        tabPanel("Analytics", plotOutput("analyticsPlot", height=500)),
        
        tabPanel("Wage Comparision", div(style="display:inline-block",selectInput("stateA", "State 1",choices = state_list)),
                 div(style="display:inline-block",selectInput("stateB", "State 2",choices = state_list)), plotOutput("wageCompare")),
        
        tabPanel("Wage Prediction", 
                 div(style="display:inline-block",textInput("stateW", "Work Site State","MI")),
                 div(style="display:inline-block",textInput("Empname", "Employer Name","IBM INDIA PRIVATE LIMITED")),
                 div(style="display:inline-block",textInput("PYear", "Prediction Year","2018")), 
                 br(),
                 br(),
                 tags$p("Predicted Wage in USD- "),
                 textOutput("wagePredict")),
        
        tabPanel("Map",plotOutput("heatmap"))
      )
    )
  )
))
