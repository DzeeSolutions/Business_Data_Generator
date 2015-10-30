# user interface for shiny application

library(shiny)
source("../initializeEcosystem.r")

shinyUI(fluidPage(
  titlePanel("Restaurant Simulator"),
  
  # sidebar has widgets to select parameters
  sidebarLayout(
    sidebarPanel(
      h3("Parameters"),
      br(),
      
      h4("Upload your own parameters:"),
      br(),
      fileInput("userStores", label = "Upload your stores here: ", multiple = TRUE,
                accept = c("text/csv", "text/comma-separated-values")),
      fileInput("userCustomers", label = "Upload your customers here: ", multiple = TRUE,
                accept = c("text/csv", "text/comma-separated-values")),
      fileInput("userItems", label = "Upload your generic items here: ", multiple = TRUE,
                accept = c("text/csv", "text/comma-separated-values")),
      
      h4("OR, use our randomly generated data:"),
      br(),
      numericInput("numStores", label = "Number of stores in the simulation: ",
                   value = "3", min = 1, max = 5),
      
      # select number of customers
      sliderInput("numCustomers", label = "# of customers in the simulation", min = 0,
                  max = 1000, value = 500),
      selectInput("time", label = "Simulation time(i.e. what is the virtual time
                   in the simulation.)", choices = generateTimeList(), 
                  selected = "10:00:00", multiple = FALSE ),
      
      # select number of days for simulation to run
      dateRangeInput("days", label = 'The days you wish to run the simulation for',
                     end = Sys.Date() + 5, min = Sys.Date()),
      selectizeInput("itemAddDest", label = "Which store should additional items be added to?",
                     choices = NULL, multiple = TRUE),
      fileInput("newItems", label = "Upload additional items here: ", multiple = TRUE,
                accept = c("text/csv", "text/comma-separated-values")),
      selectizeInput("promotionSelect", label = "Would you like to run a promotion on any of 
                the following items?", choices = NULL, selected = NULL, multiple = TRUE)
    ),
    mainPanel(
      # shows user number of customers being run through simulation
      textOutput("customerValue"),
      br(),
      # shows user the number of stores in the simulation
      textOutput("storesValue"),
      br(),
      textOutput('itemFileStatus'),
      br(),
      textOutput("promotionStatus"),
      br(),
      # shows the user the graphs of walk in and purchase history
      uiOutput('storesTabs')

    )
  )
))