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
library(leaflet)
library(rgdal)

Ethnic_Data <- read.csv("EthnicData.csv", stringsAsFactors = TRUE)
female_income <- read.csv("FemaleIncome.csv", stringsAsFactors = TRUE)
female_Regional <- read.csv("FemaleRegionalData.csv", stringsAsFactors = TRUE)
Total_Gender_Income <- read.csv("GenderIncomesTotal.csv", stringsAsFactors = TRUE)
male_income <- read.csv("MaleIncome.csv", stringsAsFactors = TRUE)
male_regional <- read.csv("MaleRegionalData.csv", stringsAsFactors = TRUE)
regional_income <- read.csv("RegionalIncome.csv", stringsAsFactors = TRUE)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "New Zealand Income Inequality Application", 
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Reegs",
                                 message = "Thank you for using this application."
                               ),
                               messageItem(
                                 from = "Reegs",
                                 message = "If you would like to know more about the features on the page, please visit the 'About' Tab.",
                                 icon = icon("bell")
                               )),
                  #---------------------------------dropdown for notifications--------------------------
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "Thank you for listening to my presentation",
                                 icon = icon("attention"),
                                 status = "success"
                               )
                               
                               ),
                  #----------------------------------menu for tasks needing to be complete---------------------------------
                  
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 80, color = "green",
                                        "Ethnic Income"
                               ),
                               taskItem(value = 90, color = "aqua",
                                        "Gender Income tab"
                               ),
                               taskItem(value = 10, color = "yellow",
                                        "Regional Income"
                               ),
                               taskItem(value = 70, color = "red",
                                        "Overall project"
                               )
                         )
                  ),
  
  dashboardSidebar(
    #-------------------------------------Shows the selectable tabs on the Sidebar Navigation---------------------------
    sidebarMenu(
      menuItem("Ethnic Income", tabName = "ethnic", icon = icon("bar-chart-o")),
      menuSubItem("Ethnic Raw Data", tabName = "rawethnic", icon = icon("globe")),
      menuItem("Gender Income", tabName = "gender", icon = icon("venus-mars")),
      menuItem("Regional Income", tabName = "regional", icon = icon("th")),
      menuItem("Word Cloud", tabName = "words", icon = icon('book')),
      menuItem("About", tabName = "about", icon = icon("info"))
    )
  ),

  dashboardBody(
    tabItems(
#--------------------------------------------------Tab for the Ethnic data page--------------------------------------------------------
      tabItem(tabName = "ethnic",
              h2("Ethnic Income Visualisations"),
              fluidRow(
                sidebarPanel(width = 6,
                titlePanel("Filters"), 
                           selectInput("ethnicyearselect", "Select Year:", c(unique(as.character(Ethnic_Data$Year)))),
                
                           
                h3("Choose type or graph to display"),
                selectInput("ethniccharts", "Change the type of graph would would like to use", choices = list("bar",
                                                                                                         "scatter",
                                                                                                         "histogram",
                                                                                                         "scatter3d"
                ),
                selected = "bar" ),
                

                
                ),
                fluidPage(column(9,
                                infoBoxOutput("selectedyearaverage1"), 
                                infoBoxOutput("selectedyearaverage2"),
                                infoBoxOutput("HighestAverage"),
                                infoBoxOutput("HighestPaidEthnic"),   
                                )

                ),
                
                       #display the bargraph
                       box(
                          plotlyOutput("ethnicgraph")
                                  
                         )
                       ),
              
              ),
#------------------------------------------------------Gender income page tab-----------------------------------------------------------
        tabItem(tabName = "gender",
                h2("Gender Income section"),
                fluidPage(
                  fluidRow(
                    column(12, box( width = 12,
                      titlePanel("Filters - select first"),
                      h3("Male Filters                                   Female Filters"),
                      actionButton("maleweekly", "Male Weekly Average", icon = icon("mars")),
                      actionButton("malehourly", "Male Hourly Average", icon = icon("mars")),
                      hr(),
                      h3("Type of graph to be drawn"),
                      
                      #!-----------------------------------give the user the option to select which kind of visualisation they would like-----------------------------
                      
                      selectInput("charts", "Change the type of graph would would like to use", choices = list("bar",
                                                                                                               "scatter",
                                                                                                               "histogram",
                                                                                                               "scatter3d"
                                                                                                               ),
                                  selected = "bar" ),
                      hr(),
                      h3("Female Filters"),
                      actionButton("femaleweekly", "Female Weekly Average", icon = icon("venus")),
                      actionButton("femalehourly", "Female Hourly Average", icon = icon("venus")),
                      hr(),
                      tags$h4("Please select an option before a plot will be produced"),
                      h3("Key Statistics", icon = icon("info-circle")),
                      hr(),
                      )
                           
                           
                    ),
                    
                    #!---------------------------------------------------Statistics module in the page--------------------------------
                    
                   column(12,
                          infoBoxOutput("HighestMaleIncome"),
                          infoBoxOutput("HighestFemaleIncome"),
                          infoBoxOutput("LowestMaleIncome"),
                          infoBoxOutput("LowestFemaleIncome"),
                          infoBoxOutput("Malehourly"),
                          infoBoxOutput("Femalehourly")
                          ),
                
                           br(), hr(),
                           box(title = "Male Visualisation", plotlyOutput("maleincome")),
                           br(),
                           box(title = "Female Visualisation", plotlyOutput("femaleincome"))
                    
                    
                  )
                ),
                
                ),
      
#---------------------------------------------Tab for the sub category - raw input data---------------------------------------------------
      tabItem(tabName = "rawethnic",
              h2('Raw table Data for Ethnic Wage Data'),
             
              #create a new row in the UI for selected inputs
               fluidPage(
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
                        ),
                 
                 DT::dataTableOutput("table")
                   
                 ),
              
              box(title = "Legend", width = 14,
                  p("This is the raw data that is being used to fuel the Ethnic data plots. Here you will also see some key facts about the data!"),
                  infoBox("Highest Hourly Earnings", max(Ethnic_Data$Average.Hourly.Earnings), icon = icon("arrow-alt-circle-up")),
                  infoBox("Higest Weekly Earnings", max(Ethnic_Data$Average.Weekly.Earnings), icon = icon("caret-square-up")),
                  infoBox("Lowest Hourly Earnings", min(Ethnic_Data$Average.Hourly.Earnings), icon = icon("arrow-alt-circle-down")),
                  infoBox("Lowest Weekly Earnings", min(Ethnic_Data$Average.Weekly.Earnings), icon = icon("caret-down")),
                  infoBox("Most Number of People Surveyed", max(Ethnic_Data$Number.of.People), icon = icon("user-friends"))
                  )
              
              ),
      
#-------------------------------------------Content body for Regional data visualisations----------------------------------------------------
      tabItem(tabName = "regional",
              h2("Regional Income Visualisation"),
              fluidRow(box(width = 4,
                           selectInput("regionalyear",
                                       "Income Year:",
                                       c("All", unique(as.character(regional_income$Year)))),
                           
                           )
                
              ),
              h3("New Zealand Leaflet Map"),
              hr(),
              fluidRow(
                fluidRow(box(width = 12, leafletOutput(outputId = "nzmap", width = '100%', height = 400))),
                fluidRow(box(width = 12, dataTableOutput(outputId = "summary_table")))
              )
             ),

#USE PLOTLY scattergeo 
      
#--------------------------------------Content body or container for the world cloud visualisations.-------------------------------------
      tabItem(tabName = "words",
              h2("Word Cloud Visualisation"),
              box(titlePanel('Select report to analyse'),
                  selectInput("selection", "Select a book:",
                              choices = books),
                  actionButton("update", "Change Selection"),
                  hr(),
                  h3("Cloud Filters:"),
                  sliderInput("freq", "Minimum Frequency:",
                              min = 1, max = 50, value = 13),
                  
                  sliderInput("max", "Maximum number of words:",
                              min = 1, max = 200, value = 90),
                  p("TIP: Adjust the Sliders based on the frequency of words stated or the quantity of words to be shown")
                  ),
              
              #word cloud container
              box(plotOutput("wcplot"))
              ),
#-------------------------------------------------ABOUT and SUBMISSION DETAILS-------------------------------------------
      tabItem(tabName = "about",
             h1("Project Information"),
                fluidRow(
                    box(
                        title = "About the Application",
                        hr(),
                        p("This Application was developed for the sole purpose of the INFO281 ST:Data Analytics Paper at Victoria University of Wellington"),
                        hr(),
                        h2("Author Information"),
                        h3("Name: Regan Vega"),
                        h3("Student ID: 300421287"),
                        h3("Contact: vegarega@myvuw.ac.nz"),
                        h4("All information that was used to formulate data will be referenced below"),
                        h4("NZ Stats: http://nzdotstat.stats.govt.nz/wbos/Index.aspx?_ga=2.103915008.1875428239.1573422165-1250021736.1573422165#", br(), br(),
                           "Ministry for Women: https://women.govt.nz/work-skills/income/gender-pay-gap", br(), br(),
                           "Employment NZ: https://www.employment.govt.nz/hours-and-wages/pay/pay-equity/gender-pay-gap/", br(), br(),
                           "The Treasury: https://treasury.govt.nz/sites/default/files/2018-08/ap18-03.pdf"
                           ),
                        hr(),
                        
                    ),
                    box(
                      title = "Application Help",
                      hr(),
                      h3("Ethnic Data Tab"),
                      p(""),
                      h3("Ethnic Raw Tab"),
                      p(""),
                      h3("Gender Income Tab"),
                      p(""),
                      h3("Regional Income Tab"),
                      p(""),
                      h3("Word Cloud Tab"),
                      p(""),
                    ),
              )
          )
    )
  )
)


#############################################################Server#######################################################################
# Define server logic required to draw a bargraph in plotly
server <- function(input, output) {
  
  observeEvent(input$ethnicyearselect, {
   
    ethnicdata <- filter(Ethnic_Data, Year == input$ethnicyearselect)
     
    output$ethnicgraph <- renderPlotly({
      ep <- plot_ly(
        x = ethnicdata$Ethnicity,
        y = ethnicdata$Average.Weekly.Earnings,
        type = input$ethniccharts,
        name = 'ethnics'
      ) %>%
        layout(
          title = "Comparisons of Ethnic Income 2008",
          xaxis = list(
            type = 'category',
            title = 'Ethnic incomes'
          ),
          yaxis = list(
            title = "Income Received"
          )
        )
      
    })
    
  })
  
observeEvent(input$ethnicyearselect, {
  ethnicdata <- filter(Ethnic_Data, Year == input$ethnicyearselect)
  
  output$selectedyearaverage1 <- renderInfoBox({
    infoBox("Current selected year high", max(ethnicdata$Average.Weekly.Earnings), icon = icon("money-bill-wave"))
  })
  
  output$selectedyearaverage2 <- renderInfoBox({
    infoBox("Current selected year high", max(ethnicdata$Average.Hourly.Earnings), icon = icon("money-bill-wave"))
  })
  
  
})
  
  
  output$HighestAverage <- renderInfoBox({
    infoBox("Highest Average", max(Ethnic_Data$Average.Weekly.Earnings), icon = icon("money-bill-wave"))
  })
  
  output$HighestPaidEthnic <- renderInfoBox({
    infoBox("Highest Paid Ethnic", "Asian", icon = icon("globe"))
  })
  
  #----------------------------------------------------------gender server stuff-----------------------------------------------
  output$HighestMaleIncome <- renderInfoBox({
    infoBox("Highest Average Male p/w", max(male_income$Average.Weekly.Earnings), icon = icon("money-bill-wave"))
  })
  
  output$HighestFemaleIncome <- renderInfoBox({
    infoBox("Highest Female Average P/W", max(female_income$Average.Weekly.Earnings), icon = icon("money-bill-wave"))
  })
  
  output$LowestMaleIncome <- renderInfoBox({
    infoBox("Lowest Male Average P/W", min(male_income$Average.Weekly.Earnings), icon = icon("money-bill-wave"))
  })
  
  output$LowestFemaleIncome <- renderInfoBox({
    infoBox("Lowest Female Average P/W", min(female_income$Average.Weekly.Earnings), icon = icon("money-bill-wave"))
  })
  
  output$Malehourly <- renderInfoBox({
    infoBox("Current Male Hourly Rate", max(male_income$Average.Hourly.Earnings), icon = icon("percent"))
  })
  
  output$Femalehourly <- renderInfoBox({
    infoBox("Current Female Hourly Rate", max(female_income$Average.Hourly.Earnings), icon = icon("percent"))
  })
  
  #!!!!!!!!!!!!!!!!!!!!!!!!!!Gender plots section!!!!!!!!!!!!!!!!!!!! 
  
  mgs <- observeEvent(input$maleweekly, {
    output$maleincome <- renderPlotly({
      p1m <- plot_ly(
        x = male_income$Year,
        y = male_income$Average.Weekly.Earnings,
        type = input$charts,
        name = "Male Income"
      ) %>%
        layout(
          title = "Comparison of Male weekly Income by Year",
          xaxis = list(
            type = 'category',
            title = 'Year'
          ),
          yaxis = list(
            title = 'Weekly Rate'
          )
        )
      
    })
  })
  
  observeEvent(input$malehourly, {
    output$maleincome <- renderPlotly({
      p1m <- plot_ly(
        x = male_income$Year,
        y = male_income$Average.Hourly.Earnings,
        type = input$charts,
        name = "Male Income"
      ) %>%
        layout(
          title = "Comparison of Male Hourly Incomes by Year",
          xaxis = list(
            type = 'category',
            title = 'Year'
          ),
          yaxis = list(
            title = 'Hourly Rate'
          )
        )
      
    })
    
  })
 
  fgs <- observeEvent(input$femaleweekly, {
    output$femaleincome <- renderPlotly({
      p1f <- plot_ly(
        x = female_income$Year,
        y = female_income$Average.Weekly.Earnings,
        type = input$charts,
        name = 'Female Income Weekly'
      ) %>%
      layout(
        title = 'Female Weekly Average income by Year',
        xaxis = list(
          type = 'category',
          title = 'Year'
        ),
        yaxis = list(
          title = 'Weekly Income'
        )
      )
    })
    
  }) 
  
  observeEvent(input$femalehourly, {
    output$femaleincome <- renderPlotly({
      p1f <- plot_ly(
        x = female_income$Year,
        y = female_income$Average.Hourly.Earnings,
        type = input$charts,
        name = 'Female Income Hourly Rate'
      ) %>%
        layout(
          title = 'Female Average Hourly income by Year',
          xaxis = list(
            type = 'category',
            title = 'year'
          ),
          yaxis = list(
            title = 'Hourly Rate'
          )
        )
    })
    
  }) 
  
  
  #---------------------------------------------------------Leaflet map creation code--------------------------------------
  
  output$nzmap <- renderLeaflet({
    m <- leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lng = 174.763336, lat = -36.848461, zoom = 5) %>%
      addPolygons(data = nzregions)
    m
    
  })
  
  nzregions <- readOGR("data/NewZealandPolygon70.shp")
  
  #Leaflet table
  output$summary_table <- DT::renderDataTable(DT::datatable({
    regiondata <- regional_income
    if (input$regionalyear != "All") {
      regiondata <- regiondata[regiondata$Year == input$regionalyear,]
    }
    regiondata
  }))
  
  #------------------------------------------------------Code for making the Word Cloud.------------------------------------
  #!-------------------for word clouds
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
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$wcplot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8,"Dark2"))
  })
  
  #---------------------------------------------------------------Code for server for Raw Table data for tab - ethnic data-----------------------------
  output$table <- DT::renderDataTable(DT::datatable({
    data <- Ethnic_Data
    if (input$year != "All") {
      data <-data[data$Year == input$year,]
    }
    
    if (input$ethnic != "All") {
      data <- data[data$Ethnicity == input$ethnic,]
    }
    data
  }))

  
}

# Run the application 
shinyApp(ui = ui, server = server)
