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
      
      textInput("job_title", "Job Title",""),
    
      selectInput("variable", "Choose Algoritm",
                  c("Apriori" = "apriori",
                    "Random Forest" = "random_forest",
                    "SVM" = "svm",
                    "Regression" = "regression")),
      
      actionButton("compute_result","Compute Results", icon = icon("refresh"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Analytics", plotOutput("analyticsPlot", height=500)),
        
        tabPanel("Wage Comparision", textInput("stateA", "State 1",""),
                 textInput("stateB", "State 2",""), plotOutput("wageCompare")),
        
        tabPanel("Job Title", plotOutput("jobTitlePlot")),
        tabPanel("Company", plotOutput("companyPlot")),
        tabPanel("Worksite", plotOutput("workSitePlot")),
        tabPanel("Insights", selectInput("location", "Location", choices = state_list),
                 textInput("job_title", "Job Title",""),
                 tags$p("Top Applications are from California"))
      )
    )
  )
  
))
