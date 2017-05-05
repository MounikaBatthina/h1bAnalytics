library(shiny)

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
  
  # Application title
  titlePanel("H-1B Data Analysis"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Top Values",
                  min = 5,
                  max = 15,
                  value = 10),
      sliderInput("year",
                  "Year:",
                  min = 2011,
                  max = 2016,
                  value = c(2014,2016),
                  ticks= FALSE),
      selectInput("variable", "Choose Metric",
                  c("Number of Visa Applications" = "cyl",
                    "Wage Rate" = "wage",
                    "Denied" = "denied",
                    "Certified" = "certified")),
      selectInput("location",
                  "Location",
                  choices = state_list),
      textInput("employer_1", "Employer Name",""),
      textInput("employer_1", "Job Title",""),
      actionButton("compute","Compute", icon = icon("refresh"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #plotOutput("distPlot")
      tabsetPanel(
        tabPanel("Plot", plotOutput("distPlot")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
))