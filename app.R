#Begin to load up the core processes for R to use and install in necessary.

library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(readr)
library(tm)
library(wordcloud)
library(memoise)
library(DT)

Ethnic_Data <- read.csv("EthnicData.csv", stringsAsFactors = TRUE)
female_income <- read.csv("FemaleIncome.csv", stringsAsFactors = TRUE)
female_Regional <- read.csv("FemaleRegionalData.csv", stringsAsFactors = TRUE)
Total_Gender_Income <- read.csv("GenderIncomesTotal.csv", stringsAsFactors = TRUE)
male_income <- read.csv("MaleIncome.csv", stringsAsFactors = TRUE)
male_regional <- read.csv("MaleRegionalData.csv", stringsAsFactors = TRUE)
regional_income <- read.csv("RegionalIncome.csv", stringsAsFactors = TRUE)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "New Zealand Income Inequality Application"),
  
  dashboardSidebar(
    #Shows the selectable tabs on the Sidebar Navigation
    sidebarMenu(
      menuItem("Ethnic Income", tabName = "ethnic", icon = icon("bar-chart-o")),
      menuSubItem("Ethnic Raw Data", tabName = "rawethnic"),
      menuItem("Regional Income", tabName = "regional", icon = icon("th")),
      menuItem("Word Cloud", tabName = "words", icon = icon('book'))
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "ethnic",
              h2("Ethnic Income Visualisations"),
              fluidRow(box(titlePanel("Filters"), 
                           actionButton("malebutton", "Male"),
                           actionButton("femalebutton", "Female"),
                           br(),
                           sliderInput("Year", "financial year", 1998,2019,1998)),
                       box(plotlyOutput("bargraph"))
                       ),
              ),
      
      #Tab for the sub category - raw input data
      tabItem(tabName = "rawethnic",
              h2('Raw table Data for Ethnic Wage Data'),
             
              #create a new row in the UI for selected inputs
               fluidRow(
                 column(4,
                        selectInput("year",
                                    "Years:",
                                    c("All", unique(as.character(Ethnic_Data$Year))))
                        ),
                 column(4,
                        selectInput("ethnic",
                                    "Ethnicity:",
                                    c("All",
                                      unique(as.character(Ethnic_Data$Ethnicity))))
                        )
                
                   ),
              
              #Code for generating the table
              box(DT::dataTableOutput("table"))
              
              ),
      
#Content body for Regional data visualisations
      tabItem(tabName = "regional",
              h2("Regional Income Visualisation")
             ),
      
#Content body or container for the world cloud visualisations.
      tabItem(tabName = "words",
              h2("Word Cloud Visualisation"),
              box(titlePanel('Select report to analyse'),
                  selectInput("selection", "Select a book:",
                              choices = books),
                  actionButton("update", "change"),
                  hr(),
                  sliderInput("freq", "Minimum Frequency:",
                              min = 1, max = 50, value = 15),
                  
                  sliderInput("max", "Maximum number of words:",
                              min = 1, max = 200, value = 90)
                  ),
              
              #word cloud container
              box(plotOutput("wcplot"))
              )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$bargraph <- renderPlotly({
    plot_ly(data = Ethnic_Data, x = Ethnic_Data$Average.Weekly.Earnings, y = Ethnic_Data$Ethnicity)
  })
 
  terms <- reactive({
    #change book and update the code to reflect the changes
    input$update
    #not for others though
    isolate({
      withProgress({
        setProgress(message = "Processing work corpus")
        getTermMatrix(input$selection)
      })
    })
  }) 
  
  #Code for making the Word Cloud.
  wordcloud_rep <- repeatable(wordcloud)
  
  output$wcplot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8,"Dark2"))
  })
  
  #Code for server for Raw Table data for tab - gender data
  output$table <- DT::renderDataTable(DT::datatable({
    data <- Ethnic_Data
    if (input$year != "All") {
      data <-data[data$Year == input$year,]
    }
    
    if (input$ethnic != "All") {
      data <- data[data$Ethnicity == input$ethnic,]
    }
    
  }))
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)