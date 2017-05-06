library(shiny)
library(shinythemes)
library(shinyjs)

statesList = toupper(c("usa","alaska","alabama","arkansas","arizona","california","colorado",
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
  
  # Application title
  titlePanel("H-1B Data Analysis"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("Ntop",
                  "Top Values",
                  min = 5,
                  max = 15,
                  value = 10),
      sliderInput("year",
                  "Year:",
                  min = 2011,
                  max = 2016,
                  value = c(2011,2016),
                  ticks= FALSE),
      selectInput("metric", "Choose Metric",
                  c("Number of Visa Applications" = "cyl",
                    "Wage Rate" = "wage",
                    "Denied" = "denied",
                    "Certified" = "certified")),
      selectInput("location",
                  "Location",
                  choices = state_list),
      textInput("employer_1", "Employer Name",""),
      textInput("employer_1", "Job Title",""),
      
      selectInput("variable", "Choose Algoritm",
                  c("Apriori" = "apr",
                    "Random Forest" = "rfo",
                    "SVM" = "svm",
                    "Regression" = "reg")),
      actionButton("compute","Compute", icon = icon("refresh"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #plotOutput("distPlot")
      tabsetPanel(
        tabPanel("Heat Map", plotOutput("distPlot")),
        tabPanel("Job Title", plotOutput("jobTitlePlot")),
        tabPanel("Company", plotOutput("companyPlot")),
        tabPanel("Worksite", plotOutput("workSitePlot")),
        tabPanel("Insights", tags$p("Top Applications are from California"))
      )
    )
  )
))
